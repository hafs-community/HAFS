#!/usr/bin/env python3

import os
import sys
import re
import numpy as np
import numpy.ma as ma
import yaml
from pathlib import Path

from datetime import datetime

import bufr
from bufr.obs_builder import ObsBuilder

def map_path(map_file_name):
    script_dir = os.path.dirname(os.path.abspath(__file__))
    return os.path.join(script_dir, map_file_name)


def check_include_tv(yaml_path):
    """Check if virtualTemperature should be included based on encoder variables in YAML."""
    with open(yaml_path, 'r') as f:
        config = yaml.safe_load(f) or {}
    encoder_vars = config.get('encoder', {}).get('variables', [])
    return any(v.get('name', '').startswith('ObsType/virtualTemperature') for v in encoder_vars)


class PrepbufrObsBuilder(ObsBuilder):
    def __init__(self, mapping_path, log_name=os.path.basename(__file__)):
        super().__init__(mapping_path, log_name=log_name)

    def _get_reference_time(self, ref_time_str) -> np.datetime64:
        """
        Convert a reference time string in YYYYMMDDHH format to np.datetime64.

        Parameters
        ----------
        ref_time_str : str
            Reference time in YYYYMMDDHH format.

        Returns
        -------
        np.datetime64
            Reference time as a numpy datetime64.

        Raises
        ------
        ValueError
            If ref_time_str is missing or is not a valid YYYYMMDDHH value.
        """
        if not ref_time_str:
            raise ValueError(
                "Reference time is required and must be in YYYYMMDDHH format"
            )

        try:
            ref_time = datetime.strptime(ref_time_str, "%Y%m%d%H")
        except ValueError as exc:
            raise ValueError(
                f"Invalid reference time '{ref_time_str}'."
                "Expected format YYYYMMDDHH."
            ) from exc
        return np.datetime64(ref_time)

    def _compute_datetime(self, cycleTimeSinceEpoch, dhr):
        """
        Compute dateTime using the cycleTimeSinceEpoch and Observation Time
            minus Cycle Time

        Parameters:
            cycleTimeSinceEpoch: Time of cycle in Epoch Time
            dhr: Observation Time Minus Cycle Time

        Returns:
            Masked array of dateTime values
        """

        int64_fill_value = np.int64(0)

        dateTime = np.zeros(dhr.shape, dtype=np.int64)
        for i in range(len(dateTime)):
            if ma.is_masked(dhr[i]):
                continue
            else:
                dateTime[i] = np.int64(dhr[i]*3600) + cycleTimeSinceEpoch

        dateTime = ma.array(dateTime)
        dateTime = ma.masked_values(dateTime, int64_fill_value)

        return dateTime

    def _replace_timestamp(self, container: bufr.DataContainer, reference_time: np.datetime64) -> np.array:
        int64_fill_value = np.int64(0)

        times = container.get('obsTimeMinusCycleTime')
        timestamps = np.zeros(times.shape, dtype=np.int64)

        reference_seconds = (
            reference_time.astype('datetime64[s]').astype(np.int64)
        )
    
        for i in range(len(timestamps)):
            if ma.is_masked(times[i]):
                continue
            else:
                
                timestamps[i] = np.int64(times[i]*3600) + reference_seconds
        timestamps = ma.array(timestamps)
        timestamps = ma.masked_values(timestamps, int64_fill_value)

        container.replace('timestamp', timestamps)    

    def _select_temperature_events(self, tpc_events, tob_events, tqm_events, toboe, use_tv, num_events):
        """
        Extract the reported air temperature(s) per observation from a
        stack of PREPBUFR temperature events. The sensible (Tdry) output is
        always taken from the first sensible event (1 <= temperatureEventCode
        < 8). When virtual temperature is desired (use_tv), the virtual
        (Tv) output is additionally taken from the virtual-temperature event
        (temperatureEventCode == 8), which sits above its underlying Tdry
        event in the same stack. Both outputs can therefore be populated for
        the same observation: airTemperature carries Tdry, virtualTemperature
        carries Tv. This mirrors the GSI's Tsensible option while keeping
        Tdry available (e.g. for QC) even when Tv is assimilated.

        Parameters
        ----------
        tpc_events, tob_events, tqm_events: list of masked arrays
            Event-stack values (temperatureEventCode, temperatureOb,
            temperatureQM), one array per event level, ordered from the top
            of the stack.
        toboe: masked array
            Per-observation temperature obs error (not stacked by event).
        use_tv: bool or (n_obs,) bool array
            Whether virtual temperature should be extracted. May be a
            per-observation array to enable/disable Tv on a per-obs basis.
        num_events: int
            Number of event-stack levels to search.

        Returns
        -------
        tsen, tsenqm, tsenoe, tvo, tvoqm, tvooe: np.ndarray
            Sensible/virtual temperature, QM, and error arrays, each
            fill_value where not selected.
        """

        n_obs = tob_events[0].shape[0]
        use_tv_arr = np.broadcast_to(np.asarray(use_tv), (n_obs,))

        tsen = np.full(n_obs, tob_events[0].fill_value)
        tsenqm = np.full(n_obs, tqm_events[0].fill_value)
        tsenoe = np.full(n_obs, toboe.fill_value)
        tvo = np.full(n_obs, tob_events[0].fill_value)
        tvoqm = np.full(n_obs, tqm_events[0].fill_value)
        tvooe = np.full(n_obs, toboe.fill_value)

        for idx in range(n_obs):
            use_tv_idx = bool(use_tv_arr[idx])
            tv_found = False
            tsen_found = False

            for ev in range(num_events):
                tpc_val = tpc_events[ev][idx]
                tob_val = tob_events[ev][idx]
                tqm_val = tqm_events[ev][idx]

                if ma.is_masked(tpc_val) or ma.is_masked(tob_val):
                    continue

                if tpc_val == 8:
                    # virtual-temperature (VIRTMP) event
                    if use_tv_idx and not tv_found:
                        # save Tv
                        tvo[idx] = tob_val
                        if not ma.is_masked(tqm_val):
                            tvoqm[idx] = tqm_val
                        if not ma.is_masked(toboe[idx]):
                            tvooe[idx] = toboe[idx]
                        tv_found = True
                    # keep scanning for the underlying Tdry event
                elif (tpc_val >= 1) and (tpc_val < 8):
                    # sensible (Tdry) event
                    if not tsen_found:
                        # save Tdry
                        tsen[idx] = tob_val
                        if not ma.is_masked(tqm_val):
                            tsenqm[idx] = tqm_val
                        if not ma.is_masked(toboe[idx]):
                            tsenoe[idx] = toboe[idx]
                        tsen_found = True

                # done once Tdry is captured and Tv is captured (or not wanted)
                if tsen_found and (tv_found or not use_tv_idx):
                    break

        return tsen, tsenqm, tsenoe, tvo, tvoqm, tvooe
