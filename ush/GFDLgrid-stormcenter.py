# JungHoon Shin 
# Before run this load the following python anaconda modules, because we need xarray & numpy
#---------------- For Hera ---------------------
# module load intel
# module use -a /contrib/anaconda/modulefiles
# module load anaconda/latest
#---------------- For Jet ---------------------
# module use -a /contrib/anaconda/modulefiles
# module load anaconda

# Usage example:  python3 GFDLgrid-stormcenter.py 13L 2020082512 C512_grid.tile7.halo0.nc    
# We MUST enter file in the command line, because this script caluculates grid size from C number

# ---------------------------------- Description ----------------------------------------------------------

# This python script reads ESG or GFDL grid file and the tcvital that only includes the storm information for the model initialization time.
# Based on "mean" grid size in degree (dx) from grid file, latitude and longitude (tclat & tclon) values from TCvitals, 
# this script estimates the first guess center (marked as C) with respect to the domain center (domlat & domlon) 
# as shown in the following figure.

# cenx & ceny: Grid indices of domain center which are correspond to domlat & domlon 
# disx & disy: The number of grid distance/space from domain center (domlat & domlon) and latitude and longitude (tclat & tclon) values from TCvitals
# cenx-disx & ceny-disy: Grid indices of the first guess center

#
#                             X: Parent domain center (cenx & ceny) 
#                             | 
#                             | disy=(domlat-tclat)/dx   
#                             |
#   First center(icen&jcen)   |
#              C:------------- 
#                   disx=(domlon-tclon)/dx
#

# dx (grid size) varies within the domain slightly, but in the above calculation, we use mean grid size. 
# So the observed storm center does not exactly match with this first guess center, 
# instead the real storm center is located at the vicinity of this first guess center
# Then, this script file searches 2000 by 1000 grid points with respect to this first guess center to find the storm center indices
# If a portion of 2000 by 1000 domain is located at the outside of parent domain,
# it only searches the sub-domain region inside of parent domain 

# IMPORTANT NOTE1: Grid indices are numbered with respect to the upper-right corner of the domain. That is, the grid point(i=1,j=1) is the upper-right corner of the domain.  
# IMPORTANT NOTE2: To save the computational cost, this script reads every 2 grid point to find the storm center
#################################################################################################################

import os, sys
import numpy as np
import xarray as xr
global grd_x,grd_y,npx,npy

#-------------------------------------- open the grid file ---------------------------------------------
try:
  fname = sys.argv[3]         # entered from command line
except IndexError:
  fname = 'C512_grid.tile7.halo0.nc'
try: grd=xr.open_mfdataset(fname)	#, engine='netcdf4')
except: raise Exception('Could NOT find the file',fname)

#============================================== Step 1 ===================================================
#========= Extract longitudes, latitudes, and grid array size from the grid file =========================
grd_x=np.ma.masked_invalid(grd['x'].data)
grd_y=np.ma.masked_invalid(grd['y'].data)

# array size
(nyp,nxp)=grd_x.shape
# Doamin center  
cenx=int(nxp/2)  
ceny=int(nyp/2)  

print('Domain center Y and X grid=',ceny,cenx, 'domain center lon & lat=',grd_x[ceny,cenx]-360.0,grd_y[ceny,cenx])
domlon=grd_x[ceny,cenx]-360.0
domlat=grd_y[ceny,cenx]

#============================================== Step 2 ===================================================
#================ Get the delx (grid size in degree) from the grid file name =============================    

data1=fname.split("/")[-1].split("_")
print(data1)
data2=data1[0].split("C")
print(int(data2[1]))
cnum=int(data2[1])  #Global Equivalent Resolution
dx=2*3.14*6371/(4*cnum*111.11*2) # From Global Equivalent Resolution, caculates the resolution of super grid

print('Mean grid size in degree: dx=',dx)

#============================================== Step 3 ===================================================
#================================== Read TC center from the "tcvitals" ===================================

# Read input arguments
try:
  stormid = sys.argv[1]
except IndexError:
  raise Exception('ERROR! Storm ID (1st arg) must be provided.')
try:
  hafscycle = sys.argv[2]
except:
  raise Exception('ERROR! Forecast cycle (2nd arg) must be provided.')
try:
  tmpvit = sys.argv[4]
except IndexError:
  print('WARNING! Vitals not detected. Defaulting to \'tmpvit\'.')
  tmpvit = 'tmpvit'

# Open vitals
with open(tmpvit,"r") as file:
 for line in file:
  name=line.strip('\n')
  nam=name.split()

  storm=nam[1]
  date=nam[3]
  utc=nam[4]
  time=utc[0:2]
  cycle=date+time  # Cycle format-e.g.; 2020082500

  lat=nam[5]
  lon=nam[6]
# Handle the lat and lon more accurately!
  if lat[3] == 'N':
   latc=0.1*int(lat[0:3])  # Northern hemisphere
  if lat[3] == 'S':
   latc=-0.1*int(lat[0:3]) # Southern hemisphere

  if lon[4] == 'W':
   lonc=-0.1*int(lon[0:4]) # Western hemisphere
  if lon[4] == 'E':
   lonc=0.1*int(lon[0:4]) # Eastern hemisphere

# TC vital check': If we find the storm information, goto Step 4 & 5  
  if storm == stormid and cycle == hafscycle:
   tclat=latc
   tclon=lonc
   print('Storm center informatin',storm,cycle,tclat,tclon)
   print('=========================================================================')
  
#============================================== Step 4 ===================================================
#=============== Find the the guess center indices to search real TC center ============================== 
   disx=int((domlon-tclon)/dx)              # The number of grid distance from domain center and TC center (TCvital)
   disy=int((domlat-tclat)/dx)              # The number of grid distance from domain center and TC center (TCvital)
# First guess center indices which are ROUGH storm center  
   icen=cenx+disx
   jcen=ceny+disy   

#   if icen < 0 or jcen < 0 or icen > nxp or jcen > nyp:  # If the First guess center is not in the domain, stop  
#    print('WARNING: First guess center is not in the domain-stop')
#    quit()  

   print('First guess center is','y=',jcen,'x=',icen,' & ',grd_y[jcen,icen],grd_x[jcen,icen]-360.0)

#============================================== Step 5 ===================================================
# Find the REAL TC center by reading lat/lon values from each 2000 by 1000 grid with respect to the first guess center
# If a portion of 2000 by 1000 domain is located at the outside of parent domain, 
# it only searches the sub-domain region inside of parent domain

   # Read input arguments
   try:
     ofile = sys.argv[5]
   except IndexError:
     print('WARNING! Output file not detected. Defaulting to \'./gfdlcentergr.txt\'.')
     ofile = 'gfdlcentergr.txt'
   f=open(ofile, mode='w+')
   print('Finding the storm center grid within 2000 by 1000 grids region with respect to the above first guess center')

   iflag=0    # Flag for checking: it will be updated to 1 if we find the TC center    

   # Set the starting (ista) and ending point (iend) of search area in X-direction 
   if icen-1000 < 0:     # The right side of 2000 by 1000 domain gets out of parent domain  
    ista=0 
   else:
    ista=icen-1000
   if icen+1000 > nxp-1: # The left side of 2000 by 1000 domain gets out of parent domain
    iend=nxp-1
   else:
    iend=icen+1000
   # Set the starting (jsta) and ending point (jend) of search area in Y-direction
   if jcen-500 < 0:     # The northern side of 2000 by 1000 domain gets out of parent domain
    jsta=0
   else:
    jsta=jcen-500
   if jcen+500 > nyp-1: # The southern side of 2000 by 1000 domain gets out of parent domain
    jend=nyp-1
   else:
    jend=jcen+500

   print('Search area is',ista,'<i<',iend,jsta,'<j<',jend)
 # To save time, we read every two grid points  
   for ii in range(ista,iend,2):
    for jj in range(jsta,jend,2):
     longrd=grd_x[jj,ii]
     latgrd=grd_y[jj,ii]
     if longrd > 180.0: longrd=longrd-360.0
     if abs(longrd-tclon) < 0.1 and abs(latgrd-tclat) < 0.1:   # Find the TC center within domain  
      print('I find the storm center and storm center indices!')
      print( "%6d %6d" %(ii, jj),'  ', "%.20f" %latgrd,'  ',"%.20f" %longrd )
      line= "%6d %6d"%(ii, jj)+'\n'   
      f.writelines(line)  
      f.close   
      iflag += 1
      quit()  

   if iflag == 0:
    print('WARNING!-I cant find the storm center within the domain! Please check this script file or other files')
    quit()

#=================================================================

  else:    # TC vital check': in case TC vital is wrong  
   print('storm center ID or cycle is not correct. STOP')
   quit()

