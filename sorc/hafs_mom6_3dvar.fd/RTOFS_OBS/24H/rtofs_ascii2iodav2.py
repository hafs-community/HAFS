#!/usr/bin/env python3

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string"),
]

obsvars = { '': '', }

AttrData = { 'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = { }

VarDims = {
	" ": ['nlocs']
}

class marine(object):
    def __init__(self, filename, varname):
        self.filename = filename
        self.varname = varname
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.metaDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        #self.VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.units = {}
        self._read()

    # Open input file and read relevant info
    def _read(self):
        print("input ",self.filename)
        print("variable ",self.varname)

        obs_line = open(self.filename, "r")
        lines = len(obs_line.readlines())
        #print(lines)

        lat=np.ndarray(shape=(lines), dtype=np.float32, order='F')
        lon=np.ndarray(shape=(lines), dtype=np.float32, order='F')
        val=np.ndarray(shape=(lines), dtype=np.float32, order='F')
        err=np.ndarray(shape=(lines), dtype=np.float32, order='F')
        qc =np.ndarray(shape=(lines), dtype=np.int32, order='F')
         
        if ( self.varname == 'sea_water_temperature' ):
           sws_val=np.ndarray(shape=(lines), dtype=np.float32, order='F')
           sws_err=np.ndarray(shape=(lines), dtype=np.float32, order='F')
           sws_qc =np.ndarray(shape=(lines), dtype=np.int32, order='F')
           depth  =np.ndarray(shape=(lines), dtype=np.float32, order='F')
       
        dates = []

        obs_txt = open(self.filename, "r")
        i=0
        for line in obs_txt:
            b = str(line.split()[0])
            ss = str(datetime.strptime(b, '%Y%m%d%H%M'))
            s2 = ss[0:10]+"T"+ss[11:19]+"Z"
            dates.append(s2)

            lat[i] = float(line.split()[1])
            lon[i] = float(line.split()[2])
            val[i] = float(line.split()[3])
            err[i] = float(line.split()[4])
            qc[i]  = float(line.split()[5])

            if ( self.varname == 'sea_water_temperature' ):
               sws_val[i] = float(line.split()[6])
               sws_err[i] = float(line.split()[7])
               sws_qc[i]  = float(line.split()[8])
               depth[i]   = float(line.split()[9])
            #print(i)
            i=i+1
        obs_txt.close()

        self.outdata[('datetime', 'MetaData')]=np.empty(len(dates), dtype=object)
        self.outdata[('datetime', 'MetaData')][:] = dates

        self.outdata[('latitude', 'MetaData')]  =lat
        self.outdata[('longitude', 'MetaData')] =lon
        self.outdata[(self.varname, 'ObsError')]=err
        self.outdata[(self.varname, 'ObsValue')]=val
        self.outdata[(self.varname, 'PreQC')]   =qc

        if ( self.varname == 'sea_water_temperature' ):
           print('sea_water_salinity')
           self.outdata[('sea_water_salinity', 'ObsError')]=sws_err
           self.outdata[('sea_water_salinity', 'ObsValue')]=sws_val
           self.outdata[('sea_water_salinity', 'PreQC')]   =sws_qc
           self.outdata[('depth', 'MetaData')]=depth

        # get global attributes
        DimDict['nlocs'] = len(val)
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])

def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'read RTOFS obs ascii file'
            'write IODA V2 netCDF file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="RTOFS obs ascii input file",
        type=str, required=True)
    required.add_argument(
        '-v', '--varname',
        help="IODA V2 variable name, e.g., sea_surface_temperature",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="IODA V2 output file",
        type=str, required=True)
    args = parser.parse_args()

    # Read in the marine data
    obs = marine(args.input,args.varname)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs.outdata, VarDims, obs.var_mdata, AttrData, obs.units)

if __name__ == '__main__':
    main()

