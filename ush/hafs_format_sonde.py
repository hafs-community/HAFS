#! /usr/bin/env python3
################################################################################
# Script Name: hafs_format_sonde.py
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script deals with and formats TEMP-DROP message (observations).
# History:
#   10/16/2020: This script was adopted from a version developed by Henry R.
#               Winterbottom for HWRF.
################################################################################
#
#    Author: Henry R. Winterbottom

#    Email: Henry.Winterbottom@noaa.gov

#    This file is part of obs-preproc.

#    obs-preproc is free software: you can redistribute it and/or
#    modify it under the terms of the GNU General Public License as
#    published by the Free Software Foundation, either version 3 of
#    the License, or (at your option) any later version.

#    obs-preproc is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#    General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with obs-preproc.  If not, see
#    <http://www.gnu.org/licenses/>.

#----

import argparse
import collections
import datetime
import logging
import numpy
import os
import string
import sys
import tarfile
import contextlib

from contextlib import closing

#----

__author__ = "Henry R. Winterbottom"
__copyright__ = "2019 Henry R. Winterbottom, NOAA/NCEP/EMC"
__version__ = "1.0.0"
__maintainer__ = "Henry R. Winterbottom"
__email__ = "henry.winterbottom@noaa.gov"
__status__ = "Development"

#----

class FormatSonde(object):
    """
    DESCRIPTION:

    This is the base-class object to format TEMP-DROP messages
    (observations) in accordance with the expectations of the
    tempdrop_sonde executable.

    INPUT VARIABLES:

    * opts_obj; a Python object containing the user command line
      options.

    """
    def __init__(self,opts_obj):
        """
        DESCRIPTION:

        Creates a new FormatSonde object.

        """
        self.opts_obj=opts_obj
        opts_list=['cycle','datapath']
        for item in opts_list:
            value=getattr(self.opts_obj,item)
            setattr(self,item,value)
        self.logger=FormatSondeLog()
        self.dateobj=datetime.datetime.strptime(self.cycle,'%Y-%m-%d_%H:%M:%S')
        self.srchstrs=['UZNT','UZPN','UZPA']
        self.flag_list=['CCA']
        self.max_offset_seconds=int(2*84600)
    def check_timestamp(self,file_timestamp,timestamps):
        """
        DESCRIPTION:

        This method checks the validity of the timestamp used to
        assign filenames; if the difference between the date in the
        timestamp and the date of the forecast cycle is greater than
        1-day, the filename timestamp is reset; in addition, if the
        timestamp assigned to the filename does not make sense, it is
        assigned to one that does.

        INPUT VARIABLES:

        * fts; a Python integer containing the timestamp (assuming the
          UNIX format %Y%m%d%H%M) to be assigned to the file name.

        * timestamps; a Python list of timestamps for which the
          current processing is occurring.

        OUTPUT VARIABLES:

        * fts; a Python integer containing the (updated) timestamp
          (assuming the UNIX format %Y%m%d%H%M) to be assigned to the
          file name.

        """
        fts=file_timestamp
        try:
            timestamp_obj=datetime.datetime.strptime(file_timestamp,'%Y%m%d%H%M')
            offset_seconds=numpy.abs((self.dateobj-timestamp_obj).seconds)
            if offset_seconds>self.max_offset_seconds:
                yyyymmdd=sorted(timestamps)[0][0:8]
                hhmm=file_timestamp[-4::]
                fts='%s%s'%(yyyymmdd,hhmm)
        except ValueError:
            hhmm=file_timestamp[-4::]
            for timestamp in timestamps:
                yyyymmdd=timestamp[0:8]
                fts='%s%s'%(yyyymmdd,hhmm)
                try:
                    timestamp_obj=datetime.datetime.strptime(fts,'%Y%m%d%H%M')
                    break
                except:
                    print('INFO: continue to the next timestamp.')
                    pass
        return fts
    def collect_sondes(self,data):
        """
        DESCRIPTION:

        This method loops through each key within an input Python
        dictionary containing all observations (data) and returns
        character strings denote the locations of observations to be
        formatted.

        INPUT VARIABLES:

        * data; a Python dictionary containing key and value pairs for
          all observations within the respective files to be
          processed.

        OUTPUT VARIABLES:

        * outinfostrs; a Python dictionary containing key (timestamp)
          and value (TEMP-DROP sonde observation headers) pairs.

        """
        infostrs=dict()
        for key in sorted(data.keys()):
            infostrs[key]=list()
            for srchstr in self.srchstrs:
                for item in data[key]:
                    if srchstr in item:
                        infostrs[key].append(item.strip())
            kwargs={'infostrs':infostrs[key],'data':data[key]}
            infostrs[key]=self.get_obsinfo(infostrs[key],data[key])
            flag_infostrs=list()
            for item in list(infostrs[key].keys()):
                for flag_item in self.flag_list:
                    if flag_item in item:
                        flag_infostrs.append(item)
            msg=('Found the following flagged message headers for %s:\n %s\n'%\
                (key,flag_infostrs))
            self.logger.info(msg=msg)
        rmvinfostrs=list()
        for key in sorted(data.keys()):
            infostr=infostrs[key]
            for item in flag_infostrs:
                for flag_item in self.flag_list:
                    if flag_item in item:
                        string=item.replace(flag_item,'').rstrip()
                        for ifs in list(infostr.keys()):
                            if string==ifs:
                                msn1=infostr[ifs]['mission']
                                obs1=infostr[ifs]['obid']
                                msn2=infostr[string]['mission']
                                obs2=infostr[string]['obid']
                                if (msn1==msn2) and (obs1==obs2):
                                    rmvinfostrs.append(string)
        msg=('Removing the following unique message header(s):\n %s\n'%\
            set(rmvinfostrs))
        self.logger.info(msg=msg)
        outinfostrs=list()
        for key in sorted(data.keys()):
            outstrs=list()
            for item in list(infostrs[key].keys()):
                if item not in rmvinfostrs:
                    outstrs.append(item)
                    outinfostrs.append(item)
            infostrs[key]=outstrs
        msg=('The following %d TEMP-DROP sonde message headers will be processed:\n%s\n'%\
            (len(outinfostrs),outinfostrs))
        self.logger.info(msg=msg)
        return outinfostrs
    def createfilelist(self):
        """
        DESCRIPTION:

        This method creates a formatted list of files in accordance
        with the expectations of the tempdrop_sonde executable.

        """
        workpath=os.getcwd()
        filenames=os.listdir(workpath)
        with open(os.path.join(workpath,'filelist.list'),'w') as f:
            for item in filenames:
                if '.mod' in item:
                    f.write('"%s"\n'%os.path.join(workpath,item))
    def find_sondefiles(self):
        """
        DESCRIPTION:

        This method collects relevant sonde files to be processed; the
        relevancy is determined relative to the forecast cycle
        timestamp.

        """
        offset_seconds=10800
        time_dict={'year':{'cstrt':0,'cstp':4},'month':{'cstrt':4,'cstp':6},\
            'day':{'cstrt':6,'cstp':8},'hour':{'cstrt':8,'cstp':10},\
            'minute':{'cstrt':10,'cstp':12},'second':{'cstrt':12,'cstp':14}}
        datetime_kwargs=dict()
        time_key_list=['year','month','day','hour','minute','second']
        cycle=datetime.datetime.strftime(self.dateobj,'%Y%m%d%H')
        for item in time_key_list:
            cstrt=time_dict[item]['cstrt']
            cstp=time_dict[item]['cstp']
            value=cycle[cstrt:cstp]
            if len(value)>0:
                datetime_kwargs[item]=int(value)
            else:
                datetime_kwargs[item]=0
        timestamp=datetime.datetime(**datetime_kwargs)
        dtime=datetime.timedelta(seconds=offset_seconds)
        in_list=[(timestamp-dtime).strftime('%Y%m%d'),(timestamp+dtime).strftime('%Y%m%d')]
        timestamps=sorted(set(in_list))
        filenames=os.listdir(self.datapath)
        filedict=dict()
        msg=('Cycle %s; searching for the following timestamps: %s'%(cycle,\
            timestamps))
        self.logger.info(msg=msg)
        self.tempdrop_list=list()
        for item in filenames:
            for timestamp in timestamps:
                if timestamp in item:
                    filename=os.path.join(self.datapath,item)
                    filedict[filename]=timestamp
                    msg=('Found file %s for processing.'%filename)
                    self.logger.info(msg=msg)
                    break
        filedict=collections.OrderedDict(sorted(filedict.items()))
        return filedict
    def formatsondes(self):
        """
        DESCRIPTION:

        This method formats TEMP-DROP messages (observations) in
        accordance with the expectations of the tempdrop_sonde
        executable.

        """
        srchstrs=['REL','SPG','SPL']
        excldstrs=['62626','REL','SPG','SPL']
        for infile in self.tempdrop_list:
            if os.path.exists(infile):
                with open(infile,'rb') as inf:
                    data=inf.read().decode("utf-8", "ignore")
                outfile=('%s.mod'%infile)
                datan=list()
                data.replace('\r','')
                data=data.split('\n')
                data=[_f for _f in data if _f]
                for item in data:
                    item=self.stripmeta(instr=item)
                    datan.append(item)
                data=datan
                with open(outfile,'w') as outf:
                    for item in data:
                        if any(s in item for s in excldstrs):
                            pass
                        else:
                            outf.write('%s\n'%item)
                    outdata=list()
                    for (i,item) in enumerate(data):
                        for srchstr in srchstrs:
                            if srchstr in item:
                                try:
                                    nstr=data[i]+data[i+1]
                                    nstr=self.stripmeta(instr=nstr)
                                    indx=nstr.index(srchstr)
                                    sstr=nstr[indx:indx+23]
                                    sstr=self.stripmeta(instr=sstr)
                                    outf.write('%s\n'%sstr)
                                except IndexError:
                                    print('INFO: continue next srchstr')
                                    pass
    def get_obsinfo(self,infostrs,data):
        """
        DESCRIPTION:

        This method collects the observation and aircraft flight
        identifications and returns a Python dictionary (obsdict)
        corresponding to the observation header string.

        INPUT VARIABLES:

        * infostrs; a Python list of observation header strings.

        * data; a Python list of all observations collected by driver
          level of script.

        OUTPUT VARIABLES:

        * obsdict; a Python dictionary containing the observation
          header string and the corresponding observation and aircraft
          flight identifications.

        """
        obsdict=dict()
        whitelist=['OB']
        for infostr in infostrs:
            obsdict[infostr]=dict()
            lnidx=0
            for (i,item) in enumerate(data):
                if infostr in item:
                    lnidx=i
                    break
            for (i,item) in enumerate(data[lnidx::]):
                if 'OB' in item:
                    obsitem=item.split().index('OB')
                    obsdict[infostr]['obid']=item.split()[obsitem+1]
                    obsdict[infostr]['mission']=item.split()[1]
                    break
        return obsdict
    def read_sondefiles(self,filedict):
        """
        DESCRIPTION:

        This method collects all observations contained within the
        files to be processed (filedict keys) and returns a Python
        dictionary containing key (timestamp) and value (observations)
        pairs.

        INPUT VARIABLES:

        * filedict; a Python dictionary containing key (timestamp) and
          values (file paths) pairs.

        OUTPUT VARIABLES:

        * data; a Python dictionary containing key (timestamp) and
          values (observations collected from the respective input
          file) pairs.

        """
        data=dict()
        for infile in list(filedict.keys()):
            msg=('Processing file %s.'%infile)
            self.logger.info(msg=msg)
            year=filedict[infile][0:4]
            month=filedict[infile][4:6]
            day=filedict[infile][6:8]
            lnidx=0
           #with open(infile,'rb') as f:
            with open(infile,'rb') as f:
                infdata=f.read().decode("utf-8", "ignore")
            data[filedict[infile]]=infdata.split('\n')
        return data
    def sondedump(self):
        """
        DESCRIPTION:

        This method collects all GPS dropsonde files and prepares them
        for further processing as follows:

        (1) Finds all relevant files containing observations; this
            script assumes that the observation files (in the user
            specified data path) are prefixed with a timestamp
            (assuming the UNIX format) as %Y%m%d.

        (2) Reads all relevant sonde files and compiles a Python list
            of observations within the respective files.'

        (3) Finds all TEMP-DROP sonde observations; this is done by
            identifying header strings throughout the respective
            file(s).

        (4) Loops though all collect TEMP-DROP sonde observation
            headers and prepares individual files for all identified
            TEMP-DROP observations.

        """
        # Collect sonde files relevant for the current cycle.
        filedict=self.find_sondefiles()

        # Collect sonde file observations.
        data=self.read_sondefiles(filedict=filedict)

        # Collect all relevant sonde observations.
        infostrs=self.collect_sondes(data=data)

        # Create concatenated list of sonde observations.
        infodata=dict()
        for timestamp in sorted(data.keys()):
            infodata[timestamp]=list()
            for item in data[timestamp]:
                item=self.stripmeta(instr=item)
                infodata[timestamp].append(item)

        # Loop through all timestamps and prepare individual files for
        # each observation.
        for infostr in set(infostrs):
            infostr=self.stripmeta(instr=infostr)
            mission_id=infostr.split()[1]
            timestr=infostr.split()[2]
            timestamps=sorted(data.keys())
            for timestamp in timestamps:
                year=timestamp[0:4]
                month=timestamp[4:6]
                timestrday=timestr[0:2]
                fts=('%s%s%s'%(year,month,timestr))
                kwargs={'file_timestamp':fts,'timestamps':timestamps}
                value=self.check_timestamp(**kwargs)
                if value is not None:
                    fts=value
                idxs=[idx for idx, e in enumerate(infodata[timestamp]) if infostr==e]
                for idx in idxs:
                    lnidx=idx
                    outfile=('%s.%s'%(fts,mission_id))
                    i=1
                    while os.path.isfile(outfile):
                        if os.path.isfile(outfile):
                            outfile=('%s.%s.%s'%(fts,mission_id,i))
                            i=i+1
                    tdkwargs={'outfile':outfile,'infostr':infostr,'data':\
                        infodata[timestamp],'lnidx':lnidx}
                    self.write_tempdrop(**tdkwargs)
                    self.tempdrop_list.append(outfile)
    def stripmeta(self,instr):
        """
        DESCRIPTION:

        This method stripts meta-characters and carriage returns from
        an input string.

        INPUT VARIABLES:

        * instr; a Python string possibly containing meta-characters.

        OUTPUT VARIABLES:

        * outstr; a Python string stripped of meta-characters and
          carriage returns.

        """
        for c in (string.ascii_lowercase+string.ascii_uppercase):
            chkstr='^%s'%c
            outstr=instr.replace(chkstr,'')
            instr=outstr
        outstr=outstr.replace('\r','')
        return outstr
    def write_tempdrop(self,outfile,infostr,data,lnidx):
        """
        DESCRIPTION:

        This method parses a list of strings and writes a given
        TEMP-DROP message to a user specified file.

        INPUT VARIABLES:

        * outfile; a Python string specifying the path to the output
          file to contain the TEMP-DROP message.

        * infostr; a Python string specifying the TEMP-DROP message
          header (e.g., UZNT10 KNHC 250600).

        * infdata; a Python list of data strings to be parsed using
          the infostr information.

        """
        strtmsg=lnidx
        for (i,item) in enumerate(data[strtmsg::],1):
            if ';' in item:
                lnidx=i
                break
            if not item.strip():
                lnidx=i
                break
        endmsg=strtmsg+lnidx
        msg=('Writing TEMP-DROP message to %s; data block: [%s,%s]' %\
            (outfile,strtmsg,endmsg))
        self.logger.info(msg=msg)
        with open(outfile,'wt+') as f:
            for item in data[strtmsg:endmsg]:
                item=self.stripmeta(instr=item)
                f.write('%s\n'%item)
    def build_tarball(self):
        """
        DESCRIPTION:

        This method performs the following tasks:

        (1) Write all newly created TEMP-DROP formatted files (if any) in a
            tarball called 'dropsonde.<cycle>.tar'; for example,
            'dropsonde.2017091106.tar'.

        """
        cycle=datetime.datetime.strftime(self.dateobj,'%Y%m%d%H')
        filename=('dropsonde.%s.tar'%cycle)
        tempdrop_list_mod=list()
        workpath=os.getcwd()
        filenames=os.listdir(workpath)
        for item in filenames:
            if '.mod' in item:
                tempdrop_list_mod.append(item)
        if tempdrop_list_mod:
            with closing(tarfile.open(filename,'w')) as tar:
                for item in tempdrop_list_mod:
                    if os.path.isfile(item):
                        tar.add(item,arcname=os.path.basename(item),\
                            recursive=False)
                        print(item)
                       #os.remove(item)
    def run(self):
        """
        DESCRIPTION:

        This method performs the following tasks:

        (1) Collects observations from external files (possibly)
            containing TEMP-DROP messages.

        (2) Formats (any) TEMP-DROP messages in accordance with the
            expectations of the tempdrop_sonde executable.

        (3) Creates a formatted list of TEMP-DROP message files (e.g.,
            observations) to be processed (and in accordance with the
            expectations of the) tempdrop_sonde executable.

        (4) Build a tarball for the newly created TEMP-DROP formatted files

        """
        self.sondedump()
        self.formatsondes()
        self.createfilelist()
        self.build_tarball()
#----

class FormatSondeError(Exception):
    """
    DESCRIPTION:

    This is the base-class for all module raised exceptions.

    INPUT VARIABLES:

    * msg; a Python string to accompany the raised exception.

    """
    def __init__(self,msg):
        """
        DESCRIPTION:

        Creates a new FormatSondeError object.

        """
        super(FormatSondeError,self).__init__(msg)

#----

class FormatSondeLog(object):
    """
    DESCRIPTION:

    This is the base-class object for all Log instances.

    """
    def __init__(self):
        """
        DESCRIPTION:

        Creates a new Log object.

        """
        self.exception=FormatSondeError
    def info(self,msg):
        """
        DESCRIPTION:

        This method writes a message to the base-class Python logger via
        the INFO level.

        INPUT VARIABLES:

        * msg; a Python string containing the user specified logger
          message.

        """
        self.log=self.setup(info=True)
        self.log.info(msg)
    def setup(self,info=False):
        """
        DESCRIPTION:

        This method defines the Python logging object.

        OUTPUT VARIABLES:

        * log; a Python object containing the user specifed/define
          Python logging object.

        """
        if info:
            format='%(levelname)s :: %(asctime)s : %(message)s'
        if not info:
            format='%(levelname)s :: %(asctime)s : %(pathname)s (%(lineno)s)'\
                '; %(message)s'
        datefmt='%Y-%m-%d %H:%M:%S'
        log=logging
        log.basicConfig(stream=sys.stdout,level=logging.INFO,format=format,\
            datefmt=datefmt)
        return log

#----

class FormatSondeOptions(object):
    """
    DESCRIPTION:

    This is the base-class object used to collect command line
    arguments provided by the user.

    """
    def __init__(self):
        """
        DESCRIPTION:

        Creates a new FormatSondeOptions object.

        """
        self.parser=argparse.ArgumentParser()
        self.parser.add_argument('-c','--cycle',help='The forecast cycle timestamp; '\
            'formatted as (assuming UNIX convention) %Y-%m-%d_%H:%M:%S.',default=None)
        self.parser.add_argument('-d','--datapath',help='The path to the sonde files '\
            'containing TEMP-DROP observations.',default=None)
        self.opts_obj=lambda:None
    def run(self):
        """
        DESCRIPTION:

        This method collects the user-specified command-line
        arguments; the available command line arguments are as
        follows:

        -c; The forecast cycle timestamp; formatted as (assuming UNIX
            convention) %Y-%m-%d_%H:%M:%S.

        -d; The path to the sonde files containing TEMP-DROP
            observations.

        OUTPUT VARIABLES:

        * opts_obj; a Python object containing the user command line
          options.

        """
        opts_obj=self.opts_obj
        args_list=['cycle','datapath']
        args=self.parser.parse_args()
        for item in args_list:
            value=getattr(args,item)
            if value is None:
                msg=('The argument %s cannot be NoneType. Aborting!!!'%item)
                raise FormatSondeError(msg=msg)
            else:
                setattr(opts_obj,item,value)
        return opts_obj

#----

def main():
    """
    DESCRIPTION:

    This is the driver-level method to invoke the tasks within this
    script.

    """
    options=FormatSondeOptions()
    opts_obj=options.run()
    formatsonde=FormatSonde(opts_obj=opts_obj)
    formatsonde.run()

#----

if __name__=='__main__':
    main()

