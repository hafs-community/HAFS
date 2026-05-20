#!/usr/bin/env python3

import os
import numpy as np
import numpy.ma as ma

import bufr
from bufr.obs_builder import ObsBuilder, add_main_functions, map_path
from bufr.bufr_python.encoders import *

MAPPING_PATH = map_path('bufr_gnssro_mapping.yaml')


class SatGroup:
    def __init__(self,
                 name: str,
                 categoryGroup: list[str],
                 sensor_name: str,
                 sensor_full_name: str,
                 sensor_id: int,
                 data_provider: str,
                 satellite_name: list[str],
                 satellite_full_name: list[str],
                 satellite_id: list[int]):
        self.name = name
        self.categoryGroup = categoryGroup
        self.sensor_name = sensor_name
        self.sensor_id = sensor_id
        self.sensor_full_name = sensor_full_name
        self.data_provider = data_provider
        self.satellite_name = satellite_name
        self.satellite_full_name = satellite_full_name
        self.satellite_id = satellite_id
        self.container = None

    def add_to_container(self, container: bufr.DataContainer, new_container: bufr.DataContainer) -> None:
        for var_name in container.list():
            data_sets = []
            for cat in self.categoryGroup:
                if [cat] not in container.all_sub_categories():
                    print('FAIL: ', [cat], container.all_sub_categories())
                    print()
                    continue

                data_sets.append(container.get(var_name, [cat]))
                data_paths = container.get_paths(var_name, [cat])
            new_array = np.concatenate(data_sets)
            new_container.add(var_name, new_array, data_paths, [self.name])


class BaseGnssroBufrObsBuilder(ObsBuilder):
    def __init__(self, mapping_path, log_name=os.path.basename(__file__)):
        super().__init__(mapping_path, log_name=log_name)

        self._init_sat_groups()

    def _init_sat_groups(self):
        self.sat_groups = [
            SatGroup(name='cosmic2',
                     categoryGroup=['cosmic2_750', 'cosmic2_751', 'cosmic2_752', 'cosmic2_753', 'cosmic2_754', 'cosmic2_755'],
                     sensor_name='Tri-G',
                     sensor_full_name='Triple-G',
                     sensor_id=104,
                     data_provider='UCAR',
                     satellite_name=['COSMIC-2 E1', 'COSMIC-2 E2', 'COSMIC-2 E3', 'COSMIC-2 E4', 'COSMIC-2 E5', 'COSMIC-2 E6'],
                     satellite_full_name=['Constellation Observing System for Meteorology, Ionosphere, and Climate-2 E1',
                                          'Constellation Observing System for Meteorology, Ionosphere, and Climate-2 E2',
                                          'Constellation Observing System for Meteorology, Ionosphere, and Climate-2 E3',
                                          'Constellation Observing System for Meteorology, Ionosphere, and Climate-2 E4',
                                          'Constellation Observing System for Meteorology, Ionosphere, and Climate-2 E5',
                                          'Constellation Observing System for Meteorology, Ionosphere, and Climate-2 E6'],
                     satellite_id=[750, 751, 752, 753, 754, 755]),
            SatGroup(name='geooptics',
                     categoryGroup=['geooptics_265', 'geooptics_266'],
                     sensor_name='CION',
                     sensor_full_name='CICERO Instrument for GNSS-RO',
                     sensor_id=526,
                     data_provider='UCAR',
                     satellite_name=['CICERO-1 OP1', 'CICERO-1 OP2'],
                     satellite_full_name=['Community Initiative for Cellular Earth Remote Observation OP1',
                                          'Community Initiative for Cellular Earth Remote Observation OP2'],
                     satellite_id=[265, 266]),
            SatGroup(name='grace',
                     categoryGroup=['grace_803', 'grace_804'],
                     sensor_name='Tri-G',
                     sensor_full_name='Triple-G',
                     sensor_id=104,
                     data_provider='GFZ',
                     satellite_name=['GRACE C', 'GRACE D'],
                     satellite_full_name=['Gravity Recovery and Climate Experiment Follow-On C',
                                          'Gravity Recovery and Climate Experiment Follow-On D'],
                     satellite_id=[803, 804]),
            SatGroup(name='kompsat5',
                     categoryGroup=['kompsat5_825'],
                     sensor_name='IGOR',
                     sensor_full_name='Integrated GPS and Occultation Receiver',
                     sensor_id=103,
                     data_provider='UCAR',
                     satellite_name=['KOMPSAT-5'],
                     satellite_full_name=['Korean Multi-Purpose Satellite'],
                     satellite_id=[825]),
            SatGroup(name='metop',
                     categoryGroup=['metop_3', 'metop_4', 'metop_5'],
                     sensor_name='GRAS',
                     sensor_full_name='GNSS Receiver for Atmospheric Sounding',
                     sensor_id=202,
                     data_provider='DMI',
                     satellite_name=['MetOp'],
                     satellite_full_name=['Meteorlogical Operational Satellite B',
                                          'Meteorlogical Operational Satellite A',
                                          'Meteorlogical Operational Satellite C'],
                     satellite_id=[3, 4, 5]),
            SatGroup(name='paz',
                     categoryGroup=['paz_44'],
                     sensor_name='IGOR',
                     sensor_full_name='Integrated GPS and Occultation Receiver',
                     sensor_id=103,
                     data_provider='UCAR',
                     satellite_name=['PAZ'],
                     satellite_full_name=['Paz Satellite'],
                     satellite_id=[44]),
            SatGroup(name='planetiq',
                     categoryGroup=['planetiq_267', 'planetiq_268'],
                     sensor_name='Pyxis-RO',
                     sensor_full_name='Pyxis-RO',
                     sensor_id=534,
                     data_provider='UCAR',
                     satellite_name=['PlanetIQ'],
                     satellite_full_name=['PLANETIQ GNOMES-A',
                                          'PLANETIQ GNOMES-B'],
                     satellite_id=[267, 268]),
            SatGroup(name='sentinel6',
                     categoryGroup=['sentinel6_66'],
                     sensor_name='Tri-G',
                     sensor_full_name='Triple-G',
                     sensor_id=104,
                     data_provider='JPL',
                     satellite_name=['Sentinel-6A'],
                     satellite_full_name=['Sentinel-6 Michael Freilich'],
                     satellite_id=[66]),
            SatGroup(name='spire',
                     categoryGroup=['spire_269'],
                     sensor_name='STRATOS',
                     sensor_full_name='STRATOS',
                     sensor_id=530,
                     data_provider='UCAR',
                     satellite_name=['Spire'],
                     satellite_full_name=['SPIRE LEMUR 3U CUBESAT'],
                     satellite_id=[269]),
            SatGroup(name='tandemx',
                     categoryGroup=['tandemx_43'],
                     sensor_name='IGOR',
                     sensor_full_name='Integrated GPS and Occultation Receiver',
                     sensor_id=103,
                     data_provider='GFZ',
                     satellite_name=['TanDEM-X'],
                     satellite_full_name=['TerraSAR-X add-on for Digital Elevation Measurement'],
                     satellite_id=[43]),
            SatGroup(name='terrasarx',
                     categoryGroup=['terrasarx_42'],
                     sensor_name='IGOR',
                     sensor_full_name='Integrated GPS and Occultation Receiver',
                     sensor_id=103,
                     data_provider='GFZ',
                     satellite_name=['TerraSAR-X'],
                     satellite_full_name=['X-band TerraSAR satellite'],
                     satellite_id=[42]),
        ]

    def make_obs(self, comm, input_path):
        container = super().make_obs(comm, input_path)
        new_container = bufr.DataContainer({'splits/satId': [sat_group.name for sat_group in self.sat_groups]})

        for sat_group in self.sat_groups:
            sat_group.add_to_container(container, new_container)

        # do Manipulations here
        self.log.info("   Replacing, Creating, and Deriving variables.")
        for sat_group in new_container.all_sub_categories():
            self._replace_gridcoordinates(new_container, sat_group)
            self._derive_stationidentification(new_container, sat_group)
            self._derive_imph(new_container, sat_group)
            self._update_depending_on_mefr(new_container, sat_group)
            self._update_satelliteascendingflag_and_qualityflags(new_container, sat_group)
            self._update_sequencenumbers(new_container, sat_group)

        return new_container

    def create_obs_file(self, input_path, output, type='netcdf', append=False):
        comm = bufr.mpi.Comm("world")
        self.log.comm = comm
        container = self.make_obs(comm, input_path)
        container.gather(comm)

        if comm.rank() == 0:
            for category in container.all_sub_categories():
                group_container = container.get_sub_container(category)

                encoder = netcdf.Encoder(self._make_cat_description(category))
                encoder.encode(group_container,
                               output,
                               append)

    def create_obs_group(self, input, env, category: str = None, cache_categories: list = None):
        """
        Create an observation file from the input data. Override this method if you want to
        customize the file creation process or if you need a different function signature (ex: you
        need to pass multiple input files).

        :param input: Input path to the BUFR file.
        :param env: The IODA environment. Dictionary with keys: start_time, end_time, comm_name
        :param category: The category to encode (comma-separated subcategories). This string is
                         parsed into a tuple of subcategories. (optional)
        :param cache_categories: The list of categories to cache. Each category is a string that
                                 is parsed into a tuple of subcategories. (optional)
        :return: IODA ObsGroup object.
        """

        # Guard Block
        if (cache_categories is not None) and (category is None):
            raise ValueError('Category must be provided if cache_categories are specified.')

        if category:
            if not isinstance(category, str):
                raise ValueError('Category must be a comma separated string of sub-categories '
                                 'ex: \'npp\'.')

        if cache_categories:
            if not isinstance(cache_categories, list) or \
               not len(cache_categories) > 0 or \
               not isinstance(cache_categories[0], str):
                raise ValueError('Cache categories must be a list of categories ex: [\'goes-17\''
                                 ', \'goes-18\'].')

            if category not in cache_categories:
                raise ValueError(f'Category {category} not found in cache categories.')

        # Parse category and cache_categories strings
        if category:
            category = tuple(category.replace(' ', '').split(','))

        if cache_categories:
            cache_categories = [tuple(cat.replace(' ', '').split(',')) for cat in cache_categories]

        if cache_categories:
            result = self._create_obs_group_w_cache(input, env, category, cache_categories, '')
        else:
            result = self._create_obs_group_no_cache(input, env, '')

        return result

    def _create_obs_group_w_cache(self, input, env, category: tuple, cache_categories: list, sat_group):
        from pyioda.ioda.Engines.Bufr import Encoder as iodaEncoder

        comm = bufr.mpi.Comm(env["comm_name"])
        self.log.comm = comm

        cache_input_path = input
        cache_mapping_path = list(self.map_dict.values())[0]

        # Check the cache for the data and return it if it exists
        self.log.debug(f'Check if bufr.DataCache exists? \
                        {bufr.DataCache.has(cache_input_path, cache_mapping_path)}')
        if bufr.DataCache.has(cache_input_path, cache_mapping_path):
            container = bufr.DataCache.get(cache_input_path, cache_mapping_path)
            self.log.info(f'Encode {category} from cache')
            group_container = container.get_sub_container(category)
            data = next(iter(iodaEncoder(self._make_cat_description(category[0])).encode(group_container).values()))
            self.log.info(f'Mark {category} as finished in the cache')
            bufr.DataCache.mark_finished(cache_input_path, cache_mapping_path, category)
            self.log.info(f'Return the encoded data for {category}')
            return data

        container = self.make_obs(comm, input)

        # Gather data from all tasks into all tasks. Each task will have the complete record
        self.log.info(f'Gather data from all tasks into all tasks')
        container.all_gather(comm)

        self.finalize_container(container)

        self.log.info(f'Add container to cache')
        # Add the container to the cache
        bufr.DataCache.add(cache_input_path,
                           cache_mapping_path,
                           cache_categories,
                           container)

        self.log.info(f'Encode {category} for {category[0]}')
        group_container = container.get_sub_container(category)

        # Add these debug statements before the KeyError line
        data = next(iter(iodaEncoder(self._make_cat_description(category)).encode(group_container).values()))

        self.log.info(f'Mark {category} as finished in the cache')
        bufr.DataCache.mark_finished(cache_input_path, cache_mapping_path, category)
        self.log.info(f'Return the encoded data for {category}')

        return data

    def _make_cat_description(self, category):
        description = super()._make_description()
        sat_groups = [group for group in self.sat_groups if group.name == category[0]]
        if sat_groups:
            sat_group = sat_groups[0]
            description.add_global('sensor_name', sat_group.sensor_name)
            description.add_global('sensor_full_name', sat_group.sensor_full_name)
            description.add_global('sensor_id', sat_group.sensor_id)
            description.add_global('data_provider', sat_group.data_provider)
            description.add_global('satellite_name', ', '.join([name for name in sat_group.satellite_name]))
            description.add_global('satellite_full_name', ', '.join([name for name in sat_group.satellite_full_name]))
            description.add_global('satellite_id', ', '.join([str(id) for id in sat_group.satellite_id]))

        description.remove_variable('MetaData/sequenceNumber')
        self._add_new_variable_descriptions(description)

        return description

    def _add_new_variable_descriptions(self, description):
        description.add_variables([
            {
                'name': 'MetaData/stationIdentification',
                'source': 'stationIdentification',
                'longName': 'Station Identification',
            },
            {
                'name': 'MetaData/impactHeightRO',
                'source': 'impactHeightRO1',
                'units': 'm',
                'longName': 'Impact Height Bending Angle',
            },
            {
                'name': 'MetaData/sequenceNumber',
                'source': 'sequenceNumber2',
                'longName': 'Sequence Number',
            }
        ])

    def _replace_gridcoordinates(self, new_container, sat_group):
        lat_deg = new_container.get('latitude', sat_group)
        lon_deg = new_container.get('longitude', sat_group)

        lat_rad = np.deg2rad(lat_deg)
        lon_rad = np.deg2rad(lon_deg)

        # Add to container
        new_container.replace('gridLatitude', lat_rad, sat_group)
        new_container.replace('gridLongitude', lon_rad, sat_group)

    def _derive_stationidentification(self, new_container, sat_group):
        said = new_container.get('satelliteId', sat_group)
        ptid = new_container.get('satelliteTransmitterId', sat_group)
        said_paths = new_container.get_paths('satelliteId', sat_group)

        stid = []
        for i in range(len(said)):
            newval = str(said[i]).zfill(4)+str(ptid[i]).zfill(4)
            stid.append(str(newval))
        stid = np.array(stid).astype(dtype='str')
        stid = ma.array(stid)
        ma.set_fill_value(stid, "")

        # Add to container
        new_container.add('stationIdentification', stid, said_paths, sat_group)

    def _derive_imph(self, new_container, sat_group):
        impp1 = new_container.get('impactParameterRO_roseq2repl1', sat_group).astype(np.float32)
        impp2 = new_container.get('impactParameterRO_roseq2repl2', sat_group).astype(np.float32)
        impp3 = new_container.get('impactParameterRO_roseq2repl3', sat_group).astype(np.float32)
        elrc = new_container.get('earthRadiusCurvature', sat_group)
        geodu = new_container.get('geoidUndulation', sat_group)
        impp1_paths = new_container.get_paths('impactParameterRO_roseq2repl1', sat_group)

        # Calculate imph
        imph1 = (impp1 - elrc - geodu).astype(np.float32)
        imph2 = (impp2 - elrc - geodu).astype(np.float32)
        imph3 = (impp3 - elrc - geodu).astype(np.float32)

        # Add to new_container
        new_container.add('impactHeightRO1', imph1, impp1_paths, sat_group)
        new_container.add('impactHeightRO2', imph2, impp1_paths, sat_group)
        new_container.add('impactHeightRO3', imph3, impp1_paths, sat_group)

    def _update_depending_on_mefr(self, new_container, sat_group):
        mefr1 = new_container.get('frequency__roseq2repl1', sat_group)
        mefr2 = new_container.get('frequency__roseq2repl2', sat_group)
        mefr3 = new_container.get('frequency__roseq2repl3', sat_group)
        impp1 = new_container.get('impactParameterRO_roseq2repl1', sat_group).astype(np.float32)
        impp2 = new_container.get('impactParameterRO_roseq2repl2', sat_group).astype(np.float32)
        impp3 = new_container.get('impactParameterRO_roseq2repl3', sat_group).astype(np.float32)
        bnda1 = new_container.get('bendingAngle_roseq2repl1', sat_group)
        bnda2 = new_container.get('bendingAngle_roseq2repl2', sat_group)
        bnda3 = new_container.get('bendingAngle_roseq2repl3', sat_group)
        bndaoe1 = new_container.get('obsErrorBendingAngle1', sat_group)
        bndaoe2 = new_container.get('obsErrorBendingAngle2', sat_group)
        bndaoe3 = new_container.get('obsErrorBendingAngle3', sat_group)
        imph1 = new_container.get('impactHeightRO1', sat_group)
        imph2 = new_container.get('impactHeightRO2', sat_group)
        imph3 = new_container.get('impactHeightRO3', sat_group)

        for i in range(len(impp1)):
            if (mefr2[i] == 0.0):
                mefr1[i] = mefr2[i]
                bnda1[i] = bnda2[i]
                impp1[i] = impp2[i]
                imph1[i] = imph2[i]
                bndaoe1[i] = bndaoe2[i]
            if (mefr3[i] == 0.0):
                mefr1[i] = mefr3[i]
                bnda1[i] = bnda3[i]
                impp1[i] = impp3[i]
                imph1[i] = imph3[i]
                bndaoe1[i] = bndaoe3[i]

        # Replace in new_container
        new_container.replace('bendingAngle_roseq2repl1', bnda1, sat_group)
        new_container.replace('frequency__roseq2repl1', mefr1, sat_group)
        new_container.replace('impactParameterRO_roseq2repl1', impp1, sat_group)
        new_container.replace('impactHeightRO1', imph1, sat_group)
        new_container.replace('obsErrorBendingAngle1', bndaoe1, sat_group)

    def _update_sequencenumbers(self, new_container, sat_group):
        seq = new_container.get('sequenceNumber', sat_group)
        seq_paths = new_container.get_paths('sequenceNumber', sat_group)
        seqnum2 = []
        current_seq = int(0)
        for i, s in enumerate(seq):
            if i == 0:
                seqnum2.append(int(current_seq))
            else:
                if s != seq[i-1]:
                    current_seq += 1
                seqnum2.append(int(current_seq))

        seqnum2 = np.array(seqnum2).astype(np.int32)  # <-- CONVERT TO NUMPY ARRAY
        new_container.add('sequenceNumber2', seqnum2, seq_paths, sat_group)

    def _update_satelliteascendingflag_and_qualityflags(self, new_container, sat_group):
        qfro = new_container.get('qualityFlags', sat_group)
        qfro2 = new_container.get('pccf', sat_group).astype(np.float32)
        satasc = new_container.get('satelliteAscendingFlag', sat_group)
        #   find ibit for qfro (16bit from left to right)
        #   bit5=1, reject the bending angle obs
        #   bit6=1, reject the refractivity obs
        bit3 = []
        bit5 = []
        bit6 = []
        for quality in qfro:
            if quality & 8192 > 0:
                bit3.append(1)
            else:
                bit3.append(0)

            if quality & 2048 > 0:
                bit5.append(1)
            else:
                bit5.append(0)

            # For refractivity data use only:
            if quality & 1024 > 0:
                bit6.append(1)
            else:
                bit6.append(0)

        bit3 = np.array(bit3)
        bit5 = np.array(bit5)
        bit6 = np.array(bit6)

        # overwrite satelliteAscendingFlag and QFRO
        for quality in range(len(bit3)):
            satasc[quality] = 0
            qfro2[quality] = 0.0
            if bit3[quality] == 1:
                satasc[quality] = 1
            # if (bit6[quality] == 1): refractivity data only
            #    qfro2[quality] = 1.0
            if (bit5[quality] == 1):
                qfro2[quality] = 1.0

        # Add to new_container
        new_container.replace('qualityFlags', qfro2, sat_group)
        new_container.replace('satelliteAscendingFlag', satasc, sat_group)


# Add main functions create_obs_file and create_obs_group
add_main_functions(BaseGnssroBufrObsBuilder)
