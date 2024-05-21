#! /usr/bin/env python3
################################################################################
# Script Name: hafs_opptcv_format.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script is the base-class object to collect geographical location and
#   intensity variable attributes for identical events within TC-vitals and
#   NCEP tracker formatted records.
# History:
#   10/16/2020: This script was adopted from a version developed by Henry R.
#               Winterbottom.
################################################################################
"""
SCRIPT:

   opptcv_format.py

AUTHOR:

   Henry R. Winterbottom; 27 November 2019

ABSTRACT:

   (1) ObsPreProcTCV: This is the base-class object to collect
       geographical location and intensity variable attributes for
       identical events within TC-vitals and NCEP tracker formatted
       records.

   (2) ObsPreProcTCVError: This is the base-class for all module
       raised exceptions; it is a sub-class of Exceptions.

   (3) ObsPreProcTCVOptions: This is the base-class object used to
       collect command line arguments provided by the user.

   * main; This is the driver-level method to invoke the tasks within
     this script.

USAGE:

   python opptcv_format.py

     --ncep_trkr_filename <a string specifying the path to the file
                           containing the NCEP TC tracker output
                           (fort.64)>

     --output_filename <a string specifying the path to the file to
                        contain the output record(s)>

     --tcv_filename <a string specifying the path to the file
                     containing the TC-vitals>

HISTORY:

   2019-11-27: Henry R. Winterbottom -- Initial implementation.

"""

# ----

import argparse
from math import sqrt

# ----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

# ----


class ObsPreProcTCV(object):
    """
    DESCRIPTION:

    This is the base-class object to collect geographical location and
    intensity variable attributes for identical events within
    TC-vitals and NCEP tracker formatted records.

    INPUT VARIABLES:

    * opts_obj; a Python object containing the user command line
      options.

    """

    def __init__(self, opts_obj):
        """
        DESCRIPTION:

        Creates a new ObsPreProcTCV object.

        """
        self.opts_obj = opts_obj
        opts_list = ['ncep_trkr_filename', 'output_filename', 'tcv_filename']
        for item in opts_list:
            value = getattr(self.opts_obj, item)
            setattr(self, item, value)

    def get_basinid(self, basin):
        """
        DESCRIPTION:

        This method returns a string which identifies the basin for
        the respective TC event; the returned string is compliant with
        the basin identifications provided in the TC-vitals formatted
        records; if one cannot be determined, NoneType is returned.

        INPUT VARIABLES:

        * basin; a Python string containing the basin identifier from
          the NCEP tracker record.

        OUTPUT VARIABLES:

        * basinid; a Python string denoting the basin identifier that
          is compliant with the basin identifications provided in the
          TC-vitals formatted records.

        """
        basinid = None
        if basin.upper() == 'AL':
            basinid = 'L'
        if basin.upper() == 'EP':
            basinid = 'E'
        if basin.upper() == 'CP':
            basinid = 'C'
        if basin.upper() == 'WP':
            basinid = 'W'
        if basin.upper() == 'SC':
            basinid = 'O'
        if basin.upper() == 'EC':
            basinid = 'T'
        if basin.upper() == 'AU':
            basinid = 'U'
        if basin.upper() == 'SP':
            basinid = 'P'
        if basin.upper() == 'SI':
            basinid = 'S'
        if basin.upper() == 'BB':
            basinid = 'B'
        if basin.upper() == 'NA':
            basinid = 'A'
        return basinid

    def get_tcvid(self, ncep_trkr_str):
        """
        DESCRIPTION:

        This method constructs the TC event, basin, and identifier
        strings that, using the a NCEP tracker record, that is
        compliant with the TC-vitals formatted records; if one cannot
        be determined, NoneType is returned.

        INPUT VARIABLES:

        * ncep_trkr_str; a Python string containing a NCEP tracker
          record.

        OUTPUT VARIABLES:

        * event; a Python string denoting the TC event identifier that
          is compliant with the TC-vitals formatted records.

        * basin; a Python string denoting the basin within which the
          TC event is occuring which is compliant with the TC-vitals
          formatted records.

        * tcid; a Python string denoting the TC event abbreviated
          indentifier that is compliant with the TC-vitals formatted
          records.

        """
        (event, basin, tcid) = (None, None, None)
        try:
            basin = ncep_trkr_str.split(',')[0].strip()
            tcid = ncep_trkr_str.split(',')[1].strip()
            kwargs = {'basin': basin}
            basinid = self.get_basinid(**kwargs)
            event = '%s%s' % (tcid, basinid)
        except IndexError:
            pass
        return (event, basin, tcid)

    def read_ncep_trkr(self):
        """
        DESCRIPTION:

        This method parses a NCEP tracker formatted file (fort.64) and
        returns a Python dictionary containing the geographical
        location and intensity (maximum wind speed intensity and
        minimum central sea-level pressure) for each unique event
        within the record.

        """
        ncep_trkr_vars_dict = {'clat': 6, 'clon': 7, 'vmax': 8, 'pcen': 9}
        self.ncep_trkr_dict = dict()
        with open(self.ncep_trkr_filename, 'r') as f:
            data = f.read()
        data = list(filter(None, data.split('\n')))
        event_opts = ['basin', 'tcid']
        for item in data:
            kwargs = {'ncep_trkr_str': item}
            (event, basin, tcid) = self.get_tcvid(**kwargs)
            if event is None:
                pass
            elif event.lower() == 'none':
                pass
            else:
                self.ncep_trkr_dict[event] = dict()
                for opt in event_opts:
                    self.ncep_trkr_dict[event][opt] = eval(opt)
        for key in self.ncep_trkr_dict.keys():
            for item in data:
                basin = self.ncep_trkr_dict[key]['basin']
                tcid = self.ncep_trkr_dict[key]['tcid']
                ncep_trkr_str = '%s, %s,' % (basin, tcid)
                if ncep_trkr_str in item:
                    for ncep_trkr_var in ncep_trkr_vars_dict.keys():
                        if (ncep_trkr_var.lower() == 'clat') or \
                           (ncep_trkr_var.lower() == 'clon'):
                            idx = ncep_trkr_vars_dict[ncep_trkr_var]
                            try:
                                var = item.split(',')[idx]
                                hemis = var[-1:]
                                if ncep_trkr_var.lower() == 'clon':
                                    clon = float(var[:-1])/10.0
                                    if hemis == 'W':
                                        lon_scale = -1.0
                                    else:
                                        lon_scale = 1.0
                                    clon = clon*lon_scale
                                if ncep_trkr_var.lower() == 'clat':
                                    clat = float(var[:-1])/10.0
                                    if hemis == 'S':
                                        lat_scale = -1.0
                                    else:
                                        lat_scale = 1.0
                                    clat = clat*lat_scale
                            except IndexError:
                                pass
                        if (ncep_trkr_var.lower() == 'vmax') or \
                           (ncep_trkr_var.lower() == 'pcen'):
                            idx = ncep_trkr_vars_dict[ncep_trkr_var]
                            try:
                                var = item.split(',')[idx]
                                if ncep_trkr_var.lower() == 'vmax':
                                    vmax = float(var)/1.94384
                                if ncep_trkr_var.lower() == 'pcen':
                                    pcen = float(var)
                            except IndexError:
                                pass
                    for ncep_trkr_var in ncep_trkr_vars_dict.keys():
                        if key.lower() != 'none':
                            self.ncep_trkr_dict[key][ncep_trkr_var] = eval(
                                ncep_trkr_var)
                    break

    def read_tcv(self):
        """
        DESCRIPTION:

        This method parses a TC-vitals formatted file and returns a
        Python dictionary containing the geographical location and
        intensity (maximum wind speed intensity and minimum central
        sea-level pressure) for each event within the record.

        """
        tcv_vars_dict = {'event': 1, 'clat': 5,
                         'clon': 6, 'pcen': 9, 'vmax': 12}
        self.tcv_dict = dict()
        with open(self.tcv_filename, 'r') as f:
            data = f.read()
        for item in data.split('\n'):
            for tcv_var in tcv_vars_dict.keys():
                if tcv_var.lower() == 'event':
                    idx = tcv_vars_dict[tcv_var]
                    try:
                        tcid = item.split()[idx]
                    except IndexError:
                        pass
                    self.tcv_dict[tcid] = dict()
                else:
                    pass
        for tcid in self.tcv_dict.keys():
            for item in data.split('\n'):
                try:
                    if tcid in item.split()[1]:
                        for tcv_var in tcv_vars_dict.keys():
                            if tcv_var.lower() != 'event':
                                if (tcv_var.lower() == 'clat') or \
                                   (tcv_var.lower() == 'clon'):
                                    idx = tcv_vars_dict[tcv_var]
                                    try:
                                        var = item.split()[idx]
                                        hemis = var[-1:]
                                        if tcv_var.lower() == 'clon':
                                            clon = float(var[:-1])/10.0
                                            if hemis == 'W':
                                                lon_scale = -1.0
                                            else:
                                                lon_scale = 1.0
                                            clon = clon*lon_scale
                                        if tcv_var.lower() == 'clat':
                                            clat = float(var[:-1])/10.0
                                            if hemis == 'S':
                                                lat_scale = -1.0
                                            else:
                                                lat_scale = 1.0
                                            clat = clat*lat_scale
                                    except IndexError:
                                        pass
                                if (tcv_var.lower() == 'vmax') or \
                                   (tcv_var.lower() == 'pcen'):
                                    idx = tcv_vars_dict[tcv_var]
                                    try:
                                        var = item.split()[idx]
                                        if tcv_var.lower() == 'vmax':
                                            vmax = float(var)
                                        if tcv_var.lower() == 'pcen':
                                            pcen = float(var)
                                    except IndexError:
                                        pass
                        self.tcv_dict[tcid]['clat'] = clat
                        self.tcv_dict[tcid]['clon'] = clon
                        self.tcv_dict[tcid]['pcen'] = pcen
                        self.tcv_dict[tcid]['vmax'] = vmax
                except IndexError:
                    pass

    def record_write(self):
        """
        DESCRIPTION:

        This method writes the geographical locations and intensity
        attributes for each event within the NCEP tracker record(s) to
        an external file; the respective NCEP tracker records are
        accompanied by their TC-vitals record(s) counterparts; the
        column-delimited output for the file is as follows:

        1. <TC event> <TCV latitude (degrees N)>
        2. <NCEP tracker latitude (degrees N)>
        3. <TCV longitude (degrees W)>
        4. <NCEP tracker longitude (degrees W)>
        5. <TCV minimum central pressure (hPa)>
        6. <NCEP tracker minimum central pressure (hPa)>
        7. <TCV maximum wind speed (meters per second)>
        8. <NCEP tracker maximum wind speed (meters per second)>

	Note: write out the records only if the forecasted and observed storm
	centers are 0.2 degree away from each other.

        """
        records_list = ['clat', 'clon', 'pcen', 'vmax']
        with open(self.output_filename, 'wt') as f:
            for event in self.ncep_trkr_dict.keys():
                # Only do relocation if the storm exists in both previous
                # forecast guess and tcvitals and the observed vmax > 17. m/s
                if event in self.tcv_dict.keys() and self.tcv_dict[event]['vmax'] > 17. :
                    info_str = str()
                    info_str = info_str+'%s' % event
                    for item in records_list:
                        info_str = info_str+' %s' % self.tcv_dict[event][item]
                        info_str = info_str + \
                            ' %s' % self.ncep_trkr_dict[event][item]
                    # Change to only write out the records if the forecasted and
                    # observed storm centers are 0.2 degree away from each other.
                    if sqrt( (self.ncep_trkr_dict[event]['clon']-self.tcv_dict[event]['clon'])**2.
                           + (self.ncep_trkr_dict[event]['clat']-self.tcv_dict[event]['clat'])**2. ) > 0.2 :
                        f.write('%s\n' % info_str)

    def run(self):
        """
        DESCRIPTION:

        This method performs the following tasks:

        (1) Parses the TC-vitals formatted record(s).

        (2) Parses the NCEP tracker formatted record(s).

        (3) Creates a formatted file containing the geographical
            locations and intensity attributes for the corresponding
            TC event record(s).

        """
        self.read_tcv()
        self.read_ncep_trkr()
        self.record_write()

# ----


class ObsPreProcTCVError(Exception):
    """
    DESCRIPTION:

    This is the base-class for all module raised exceptions.

    INPUT VARIABLES:

    * msg; a Python string to accompany the raised exception.

    """

    def __init__(self, msg):
        """
        DESCRIPTION:

        Creates a new ObsPreProcTCVError object.

        """
        super(ObsPreProcTCVError, self).__init__(msg)

# ----


class ObsPreProcTCVOptions(object):
    """
    DESCRIPTION:

    This is the base-class object used to collect command line
    arguments provided by the user.

    """

    def __init__(self):
        """
        DESCRIPTION:

        Creates a new ObsPreProcTCVOptions object.

        """
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument('-ncep', '--ncep_trkr_filename', help='The path to '
                                 'the file containing the NCEP TC tracker output (fort.64).', default=None)
        self.parser.add_argument('-tcv', '--tcv_filename', help='The path to the '
                                 'file containing the TC-vitals.', default=None)
        self.parser.add_argument('-out', '--output_filename', help='The path to the '
                                 'file to contain the output record(s).', default='obs-preproc.tcv.output')
        self.opts_obj = lambda: None

    def run(self):
        """
        DESCRIPTION:

        This method collects the user-specified command-line
        arguments; the available command line arguments are as
        follows:

        -ncep; The path to the file containing the NCEP TC tracker
               output (fort.64).

        -out; The path to the file to contain the output record(s).

        -tcv; The path to the file containing the TC-vitals.

        OUTPUT VARIABLES:

        * opts_obj; a Python object containing the user command line
          options.

        """
        opts_obj = self.opts_obj
        args_list = ['ncep_trkr_filename', 'tcv_filename']
        args = self.parser.parse_args()
        for item in args_list:
            value = getattr(args, item)
            if value is None:
                msg = ('The argument %s cannot be NoneType. Aborting!!!' % item)
                raise ObsPreProcTCVError(msg=msg)
            else:
                setattr(opts_obj, item, value)
        args_list = ['output_filename']
        args = self.parser.parse_args()
        for item in args_list:
            value = getattr(args, item)
            setattr(opts_obj, item, value)
        return opts_obj

# ----


def main():
    """
    DESCRIPTION:

    This is the driver-level method to invoke the tasks within this
    script.

    """
    options = ObsPreProcTCVOptions()
    opts_obj = options.run()
    formattcv = ObsPreProcTCV(opts_obj=opts_obj)
    formattcv.run()

# ----


if __name__ == '__main__':
    main()
