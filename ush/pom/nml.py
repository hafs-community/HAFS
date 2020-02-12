#!/usr/bin/python

#from collections import OrderedDict
from util import makenwrap

##@namespace pom.nml
#   Generates the POM namelist.
#
#   @note Please report bugs/questions/comments to bijuthomas(at)mail(dot)uri(dot)edu.
#   @author Biju Thomas, GSO, University of Rhode Island.
#   @date June 9, 2014
#
#   Arguments:
#     * @c title --- run's title
#
#     * @c netcdf_file --- netCDF output file (rund ID)
#
#     * @c mode --- calculation mode.  Description:
#       *    @c 2 --- 2-D calculation (bottom stress calculated in advave)
#       *    @c 3 --- 3-D calculation (bottom stress calculated in profu,v)
#       *    @c 4 --- 3-D calculation with t and s held fixed
#
#     * @c nadv --- advection scheme.  Options:
#       *    @c 1 --- Centred scheme, as originally provide in POM
#       *    @c 2 --- Smolarkiewicz iterative upstream scheme, based on
#              subroutines provided by Gianmaia Sannino and Vincenzo
#              Artale
#
#   Constants for Smolarkiewicz iterative upstream scheme:
#
#     * @c nitera --- number of iterations. This should be in the range 1 - 4. 1 is
#               standard upstream differencing; 3 adds 50% CPU time to POM
#
#     * @c sw ---  smoothing parameter. This should preferably be 1, but 0 < sw < 1
#           gives smoother solutions with less overshoot when nitera > 1
#
#     * @c npg ---  pressure gradient scheme.  Options:
#       *  @c 1 --- Second order scheme, as originally provide in POM
#       *  @c 2 --- Fourth order scheme using the McCalpin method (Berntsen and
#                Oey, Ocean Dynamics, 2010)
#
#     * @c dte --- external (2-D) time step (secs.) according to CFL
#
#     * @c isplit --- Ratio: (Internal (3-D) time step)/(External (2-D) time step)
#               (dti/dte; dimensionless)
#
#     * @c time_start --- date and time of start of initial run of model in format
#                 (i.e. UDUNITS convention)
#                 "YYYY-MM-DD HH:MM:SS <+/->HH:MM"
#                 where "<+/->HH:MM" is the time zone (positive eastwards
#                 from Coordinated Universal Time). NOTE that the
#                 climatological time axis (i.e. beginning of year zero,
#                 which does not exist in the real-world calendar) is used
#
#    Restart options:
#
#      * @c nread_rst --- index to indicate whether run to start from restart file
#                  (nread_rst=0: no restart input file; nread_rst=1: restart
#                  input file)
#      * @c read_rst_file --- restart input file name
#      * @c write_rst ---  interval (days) to write a restart file
#      * @c write_rst_file --- restart output file name
#
#    Additional options:
#
#      * @c days --- run duration (days)
#
#      * @c prtd1 --- initial print interval (days)
#
#      * @c prtd2 --- final print interval (days)
#
#      * @c swtch --- time to switch from prtd1 to prtd2
#
#      * @c nbct --- surface temperature boundary condition (see initialize.f)
#
#      * @c ifplane --- f-plane option.  Valid values:
#        *  @c 0 ---   Grid is not an f-plane, as originally provided in POM
#        *  @c 1 ---   Grid is an f-plane, calculated at latitude cnorth_e
#
#      * @c cnorth_e --- Constant for f-plane option: latitude for Coriolis
#        calculation when using an f-plane
#
#      * @c ismoth --- smoothing/desmoothing option
#        *  @c 0 --- do not use 3D U V T S smoothing
#        *  @c 1 --- use 3D U V T S smoothing
#
#      * @c smh --- Constant for smoothing/desmoothing option: smoothing time
#           step frequency. This has traditionally been 3.
#
#      * @c tnowindd --- no prescribed wind after tnowindd (days)
#
#      * @c igeovel --- initial geostrophic velocity option.  Valid values:
#        * 0 --- No calculation of initial geostrophic velocity
#        * 1 --- Calculate initial geostrophic velocity
#
#      * @c nl --- number of z-levels in initial condition file(s)
#
#      * @c ionedim --- 3D (full) vs. 1D (vertical-only) physics option.  Valid values:
#        * @c 0 --- use full 3D physics
#        * @c 1 --- use vertical-only 1D phsyics
#
#      * @c ipwave --- wave-induced mixing option
#        * @c 0 --- do not use wave-induced mixing
#        * @c 1 --- use wave-induced mixing
#      


class nml(object):
  """!Class that generates the namelist for POM."""
  def __init__(self):
    """!nml constructor.  Generates the namelist."""
    self.namelist = {'title'         :  "'MPIPOM-TC: stormenv'"        ,          
                     'netcdf_file'   :  "'stormenv'"                   ,           
                     'mode'          :  3                              ,          
                     'nadv'          :  1                              ,          
                     'nitera'        :  1                              ,           
                     'sw'            :  0.5                            ,           
                     'npg'           :  1                              ,           
                     'dte'           :  6                              ,           
                     'isplit'        :  45                             ,           
                     'time_start'    :  "'yyyy-mm-dd-hh:00:00 +00:00'" ,          
                     'nread_rst'     :  0                              ,               
                     'read_rst_file' :  "'restart.phaseN.nc'"          ,         
                     'write_rst'     :  2.0                            ,            
                     'write_rst_file':  "'restart'"                    ,           
                     'days'          :  2.0                            ,            
                     'prtd1'         :  1.0                            ,         
                     'prtd2'         :  1.0                            ,             
                     'swtch'         :  9999.                          ,             
                     'nbct'          :  3                              ,             
                     'ifplane'       :  0                              ,             
                     'cnorth_e'      :  22.4                           ,             
                     'ismoth'        :  1                              ,             
                     'smh'           :  3.                             ,             
                     'tnowindd'      :  0.                             ,             
                     'igeovel'       :  1                              ,             
                     'nl'            :  33                             ,           
                     'ionedim'       :  0                              ,           
                     'ipwave'        :  0                              ,
                     'kppflag'       :  1}                            
    self.keys = ['title','netcdf_file','mode','nadv','nitera','sw','npg',  \
               'dte','isplit','time_start','nread_rst','read_rst_file',    \
               'write_rst','write_rst_file','days','prtd1','prtd2','swtch',\
               'nbct','ifplane','cnorth_e','ismoth','smh','tnowindd',      \
               'igeovel','nl','ionedim','ipwave','kppflag'] 

  ##@var namelist
  # a dict mapping from  namelist variable name to value.

  ## @var keys
  # A list of namelist variables. 

  def __call__(self,title,netcdf_file,dte, isplit,time_start,nread_rst,read_rst_file,  \
                    write_rst,days,prtd1,nbct,ifplane, cnorth_e,tnowindd,igeovel,
                    nl,ionedim, ipwave,kppflag):
     """!Updates the namelist values

     @param title Runs' title.
     @param netcdf_file POM NetCDF output file (run ID)
     @param time_start Model analysis time in format "YYYY-MM-DD HH:MM:SS <+/->HH:MM"
        where the "<+/->HH:MM" is the timezone.
     @param nread_rst index index to indicate whether run to start from restart file
                 (nread_rst=0: no restart input file; nread_rst=1: restart
                 input file)
     @param read_rst_file restart input filename
     @param write_rst Restart interval.
     @param days Run duration (days)
     @param prtd1 Initial print interval (days)
     @param nbct Surface temperature boundary condition (see initialize.f)
     @param tnowindd No prescribed wind after tnowindd (days)
     @param igeovel Initial geostrophic velocity option.  0=no, 1=yes
     @param nl Number of Z levels in initial condition files."""
     self.namelist['title']         =   title
     self.namelist['netcdf_file']   =   '"'+netcdf_file+'"'
     self.namelist['dte']           =   dte
     self.namelist['isplit']        =   isplit
     self.namelist['time_start']    =   time_start
     self.namelist['nread_rst']     =   nread_rst
     self.namelist['read_rst_file'] =   read_rst_file
     self.namelist['write_rst']     =   write_rst 
     self.namelist['days']          =   days
     self.namelist['prtd1']         =   prtd1
     self.namelist['nbct']          =   nbct
     self.namelist['ifplane']       =   ifplane
     self.namelist['cnorth_e']      =   cnorth_e
     self.namelist['tnowindd']      =   tnowindd
     self.namelist['igeovel']       =   igeovel
     self.namelist['nl' ]           =   nl
     self.namelist['ionedim']       =  ionedim
     self.namelist['ipwave']        =  ipwave
     self.namelist['kppflag']       =  kppflag
   
  @makenwrap
  def make(self,DEST):
    """!Writes the POM namelist pom.nml in the destination directory.
    @param DEST destination directory"""
    pass

class kppnml(object):
  """!Class that generates the namelist for POM KPP."""
  def __init__(self):
    """!kppnml constructor.  Generates the kpp namelist."""
    self.namelist = {'kpp_lt_log'            : '.true.'                   ,          
                     'kpp_lagrangian_log'    : '.false.'                  ,           
                     'kpp_sig_match_sbl_log' : '.false.'                  ,          
                     'kpp_sig_match_bbl_log' : '.false.'                  ,          
                     'kpp_sbl_log'           : '.true.'                   ,           
                     'kpp_int_log'           : '.true.'                   ,           
                     'kpp_bbl_log'           : '.true.'                   ,           
                     'kpp_std_out_log'       : '.true.'                   ,           
                     'kpp_heatflux_log'      : '.true.'                   ,           
                     'use_kpp_k_log'         : '.true.'                   ,          
                     'kpp_sbl_ek_lim_log'    : '.false.'                  ,               
                     'min_kpp_kh'            :  1.e-7                     ,         
                     'min_kpp_km'            :  1.e-7                     ,            
                     'max_kpp_kh'            :  10.0                      ,           
                     'max_kpp_km'            :  10.0                      ,            
                     'kpp_ric'               :  0.235 }                            
    self.keys = ['kpp_lt_log','kpp_lagrangian_log','kpp_sig_match_sbl_log','kpp_sig_match_bbl_log', \
               'kpp_sbl_log','kpp_int_log','kpp_bbl_log','kpp_std_out_log','kpp_heatflux_log','use_kpp_k_log','kpp_sbl_ek_lim_log', \
               'min_kpp_kh','min_kpp_km','max_kpp_kh','max_kpp_km','kpp_ric'] 

  ##@var namelist
  # a dict mapping from  namelist variable name to value.

  ## @var keys
  # A list of namelist variables. 

  def __call__(self,kpp_lt_log,kpp_ric):
     """!Updates the namelist values

     @param kpp_lt_log    True to enable Langmuir turbulence enhancement
     @param kpp_ric       Critical Richardson number in KPP """
     self.namelist['kpp_lt_log']    =  kpp_lt_log
     self.namelist['kpp_ric']       =  kpp_ric
   
  def make(self,DEST):
    """!Writes the POM KPP namelist kpp.nml in the destination directory.
    @param DEST destination directory"""
    filename = DEST+"/kpp.nml"
    asgn = " = "
    with open(filename, "w+") as file:
        arg = "&kpp_nml"
        file.write("%s\n" % (arg))
        for k in range(len(self.keys)):
            file.write("\t %s %s %s\n" % (self.keys[k], asgn, \
                self.namelist[self.keys[k]]))
        file.write("%s\n" %('/'))
