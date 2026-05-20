#!/usr/bin/env python3

import os
import numpy as np

import bufr
from bufr.obs_builder import add_main_functions, map_path
from gnssro_obs_builder import BaseGnssroBufrObsBuilder

MAPPING_PATH = map_path("./bufr_gnssro_mapping.yaml")

# ----------------------------------------------------------------------
# Concrete implementation
# ----------------------------------------------------------------------


class GnssroBufrObsBuilder(BaseGnssroBufrObsBuilder):
    """Supply the YAML files to the base class."""

    def __init__(self):
        super().__init__(MAPPING_PATH, log_name=os.path.basename(__file__))


add_main_functions(GnssroBufrObsBuilder)
