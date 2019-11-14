      program trakmain
c
c$$$  MAIN PROGRAM DOCUMENTATION BLOCK
c
c Main Program: GETTRK       Track model vortices   
C   PRGMMR: MARCHOK          ORG: NP22        DATE: 2002-05-20
c
c ABSTRACT: This program tracks the average of the max or min
c   of several parameters in the vicinity of an input
c   first guess (lat,lon) position of a vortex in order to give  
c   forecast position estimates for that vortex for a given numerical
c   model.  For the levels 700 & 850 mb, the tracked parameters are:
c   Relative vorticity (max), wind magnitude (min), and geopotential
c   height (min).  Also tracked is the min in the MSLP.  So many
c   parameters are tracked in order to provide more accurate position
c   estimates for weaker storms, which often have poorly defined
c   structures/centers.  Currently, the system is set up to be able
c   to process GRIB input data files from the GFS, MRF, UKMET, GDAS,
c   ECMWF, NGM, NAM and FNMOC/NAVGEM models.  Two 1-line files
c   are  output from this program, both containing the forecast fix
c   positions that the  tracker has obtained.  One of these  output 
c   files contains the positions at every 12 hours from forecast 
c   hour 0 to the end of the forecast. The other file is in ATCF 
c   format, which is the particular format needed by the Tropical
c   Prediction Center, and provides the positions at forecast hours
c   12, 24, 36, 48 and 72, plus the maximum wind near the storm center
c   at each of those forecast hours.
c
c Program history log:
c   98-03-16  Marchok - Original operational version.
c   98-07-15  Marchok - Added code to calculate radii of gale-, storm-,
c                       and hurricane-force winds in each quadrant.
c   99-04-01  Marchok - Added code to be able to read in 4-digit years
c                       off of the TC Vitals records.
c                       Added code, including subroutine  is_it_a_storm,
c                       to make a better determination of whether or 
c                       not the center that was found at each time is
c                       the center of a storm, and not just a passing
c                       vort max, etc.
c   99-06-15  Marchok - Fixed a bug in calcdist that was triggered by a
c                       rounding error sending a number just above 1 
c                       into ACOS to get the distance between 2 
c                       identical points (which, obviously, is 0).
c   00-06-20  Marchok - Added GDAS option for vortex relocation work.
c                       Changed nhalf from 3 to 5.  Relaxed the 
c                       requirements for pthresh and vthresh.
c   00-11-30  Marchok - Added ability to handle GFDL and NCEP Ensemble
c                       model data.  Extended time range to be able to
c                       handle 5-day capability.  Forecast hours are 
c                       now input via a namelist (easiest way to account
c                       for NAM, GFS and GFDL having different forecast
c                       lengths at 00/12z and 06/18z).  Model ID's are 
c                       now input via a namelist (makes it easier, for
c                       example, to run for many different ensemble 
c                       members).  Added new output, the atcfunix 
c                       format, needed for 5-day forecasts.
c   01-08-24  Marchok   Fixed a bug in rvcal and getgridinfo.  When a 
c                       grid that was south-->north is flipped in 
c                       conv1d2d_real to be north-->south, the scanning 
c                       mode flag remains 64 and what we would consider
c                       the max and min latitudes are reversed, so I 
c                       added code to correct this in both routines.
c   02-05-20  Marchok   Weakened the mslp gradient threshold and v850
c                       threshold in is_it_a_storm to cut down on the
c                       number of dropped storms.
c   03-03-18  Marchok   Fixed a bug in get_ij_bounds that was allowing
c                       a cos(90) and cos(-90), which then led to a
c                       divide by zero.
c   05-08-01  Marchok   Updated to allow tracking of ECMWF hi-res, ECMWF
c                       ensemble, CMC hi-res, CMC ensemble, NCEP
c                       ensemble.
c   06-11-07  Marchok   Updated to locate, and report to the atcfunix
c                       file, the value of the gridpoint minimum value
c                       of mslp.  Previously, the  barnes-averaged
c                       value had been used.
c   08-01-10  Marchok   Changed the storm ID for genesis tracking so
c                       that the ID includes info
c                       on storm detection location & time.  Added
c                       algorithms for Hart's cyclone phase space.
c                       Added new output fields to the atcfunix
c                       records, actually creating a modified atcfunix
c                       record, to include things such as the mean &
c                       max values of zeta850 & zeta700 centered on
c                       the storm, the speed & direction of storm
c                       translation, and the Hart CPS parameters.
c   10-01-07  Marchok   - input grib lead time can be hrs or minutes
c                       - added code for warm core check
c                       - added code to detect genesis
c                       - added code to report on sfc wind structure
c                       - added buffer ("grid_buffer") to avoid fixing
c                         center to boundaries on regional grids
c                       - modified rvcal to report missing zeta values
c                         as background coriolis instead of -999, since
c                         the -999 was messing up center-fixing
c                       - added 10-m wind and sfc zeta as center-fixing
c                         parms.
c
c   10-05-25  Slocum    Add verbose feature to code
c                       0 = Not terminal output, 1 = error messages only
c                       2 = all output
c
c   10-05-26  Marchok   - added flags and code to check the temporal 
c                         consistency of the mslp closed contour and 
c                         Vt850 checks for tcgen and midlat cases.
c
c   13-04-01  Marchok   Added code to upgrade the wind radii diagnosid.
c                       Hurricane Sandy exposed an issue with the
c                       tracker for large storms.  The code was modified
c                       to use an iterative technique that can 
c                       diagnose radii for large storms but still 
c                       accurately diagnost radii for small storms.  See
c                       subroutine  getradii for more details.
c
c   15-11-01  Marchok   Replaced the routine which tracks the wind 
c                       minimum at the center of a storm, as that 
c                       routine proved troublesome with very hi-res 
c                       grids (0.02-deg) from HWRF for very small 
c                       storms.  This has been replaced with a routine
c                       that looks for "wind circulation difference", 
c                       whereby the center for this parm is located at
c                       the spot where the tangential wind circulation
c                       minus the wind magnitude at the candidate 
c                       center position is maximized.  ALSO: Added in 
c                       tracking of thickness as an additional 
c                       tracked parm. ALSO: Added a separate verbose
c                       flag for only the GRIB2 read diagnostics, which
c                       can be voluminous.
c
c   16-09-01  Marchok   Added in the ability to read in NetCDF files.
c                       As with GRIB data, the NetCDF data must be on 
c                       a lat/lon grid.
c
c   17-08-31  Marchok   Added a logical bitmap capability for NetCDF 
c                       files to prevent the accessing of missing data.
c                       Also modified the code to permit more accurate
c                       reporting of the grid point value of the 
c                       minimum SLP for reporting to the atcfunix file.
c                       Finally, fixed a bug (reported by JTWC) whereby
c                       radii were being reported for thresholds that
c                       were in exceedance of the tracker-diagnosed
c                       Vmax (e.g., 34-kt radii for a storm with 
c                       Vmax = 25 kts).
c
c   19-05-29  Marchok   Only do the check for 50- and 64-kt radii the
c                       first time through the getradii routine.  The
c                       34-kt radii may be adjusted on subsequent calls.
c
c                       Adjusted code to allow for forward tracking for
c                       NetCDF data files that do not have hour0 data 
c                       in them.
c
c                       Code updated to include cyclone phase space
c                       analysis for 00h.  Previously this was not done
c                       since storm motion direction is needed for 
c                       Parameter B.  This is now included for forward
c                       tracking cases (i.e., cases in which we have TC
c                       Vitals), and we use the storm motion as reported
c                       from the TC Vitals.  Of course, the observed 
c                       motion from TC Vitals may not exactly match the
c                       model forecast motion, but for 00h it will be
c                       close enough that it will allow us to get the
c                       CPS diagnostics.  This was a request from Chris
c                       Velden for use with the latest version of AODT.
c                       For genesis cases (trkrtype = midlat or tcgen),
c                       CPS diagnostics are still not performed at the
c                       first detection lead time.
c
c                       In subroutine get_sfc_center, replaced double-
c                       weighting for surface wind circulation fix with
c                       single weighting, as results of testing did not
c                       support the use of double weighting.
c
c                       Modified code in subroutine getvrvt to properly
c                       handle the computation of radial and tangential
c                       winds when a storm is near the Greenwich
c                       Meridian.
c
c                       Made significant changes to subroutine
c                       get_next_ges to fix issues for cases that are
c                       near or crossing the Greenwich meridian.
c
c                       Made changes to subroutine get_max_wind to allow
c                       for checking of the max wind at points closer to
c                       the border of a regional grid.  Previously, the
c                       last few points were not sampled in order to
c                       avoid numerical issues with sampling points at
c                       the boundary.  This change was added in response
c                       to a request from HRD for use with a regional
c                       version of fv3 (hfvgfs).
c
c                       Made a substantial change to the algorithm that
c                       returns the value of the wind circulation in
c                       subroutine get_wind_circulation.  Previously,
c                       a difference was computed between the radially-
c                       integrated wind circulation and the wind
c                       magnitude at the center of the storm.  This was
c                       flawed.  Now, just the value of the radially-
c                       integrated wind circulation is used, and the
c                       results are robust and comparable to those of
c                       using relative vorticity. 
c
c                       Bug fix in subroutine getcorr related to the
c                       correlation residuals.
c
c   19-09-03  Marchok   Made a change to subroutine getgridinfo to 
c                       allow a particular longitude specification to
c                       be okay.  Previously, if the grid max west lon
c                       was > 0 and the grid max east long was also > 0,
c                       but if glonmin > glonmax (for example, a grid
c                       that spans across the GM going from 265E to 5E),
c                       the code was written to assume the user had 
c                       coded the grid specification incorrectly.  This
c                       condition is now allowed.
c
c Input files:
c   unit   11    Unblocked GRIB1 file containing model data
c   unit   12    Text file containing TC Vitals card for current time
c   unit   31    Unblocked GRIB index file
c
c Output files:
c   unit   61    Output file with forecast positions every 12h from 
c                vt=00h to the end of the forecast
c   unit   62    Output file in ATCF format, with forecast positions
c                at vt = 12, 24, 36, 48 and 72h, plus wind speeds.
c   unit   63    Output file with forecast wind radii for 34, 50 and
c                64 knot thresholds in each quadrant of each storm.
c
c Subprograms called:
c   read_nlists  Read input namelists for input date & storm number info
c   read_tcv_card Read TC vitals file to get initial storm position
c   getgridinfo  Read GRIB file to get basic grid information
c   tracker      Begin main part of tracking algorithm
c
c Attributes:
c   Language: Standard Fortran_90
c
c$$$
c
c-------
c
c     LOCAL:
c
c     ifhours:   Integer array holding numerical forecast times for
c                the input model (99 = no more times available).
c                These values are read in via a namelist.
c     Model numbers used: (1) GFS, (2) MRF, (3) UKMET, (4) ECMWF,
c                (5) NGM, (6) NAM, (7) NAVGEM, (8) GDAS,
c                (10) NCEP Ensemble, (11) ECMWF Ensemble (13) SREF
c                Ensemble, (14) NCEP Ensemble (from ensstat mean
c                fields), (15) CMC, (16) CMC Ensemble, (17) HWRF,
c                (18) HWRF Ensemble, (19) HWRF-DAS (HDAS),
c                (20) Ensemble RELOCATION (21) UKMET hi-res (NHC)
c                (23) FNMOC Ensemble
c     stormswitch:  This switch tells how to handle each storm in 
c                the TCV file:
c                1 = process this storm for this forecast hour.
c                2 = Storm was requested to be tracked, but either
c                    the storm went off the grid (regional models),
c                    the storm dissipated, or the program was
c                    unable to track it.
c                3 = Storm was NOT requested to be tracked at all.
c     storm:     An array of type tcvcard.  Each member of storm 
c                contains a separate TC Vitals card.
c     maxstorm:  Maximum number of storms the system is set up to 
c                handle at any 1 time.
c     slonfg,slatfg:  Holds first guess positions for storms.  The 
c                very first, first guess position is read from the
c                TC vitals card. (maxstorm,maxtime)
c     clon,clat: Holds the coordinates for the center positions for
c                all storms at all times for all parameters.
c                (max_#_storms, max_fcst_times, max_#_parms)
c
      USE def_vitals; USE inparms; USE set_max_parms; USE level_parms
      USE trig_vals; USE atcf; USE trkrparms; USE verbose_output
      USE netcdf_parms
c
      implicit none
c
      logical(1) file_open
      integer date_time(8)
      character (len=10) big_ben(3)
      character :: ncfile*180,ncfile_has_hour0*1
      integer itret,iggret,iicret,igcret,iret,ifhmax,maxstorm,numtcv
      integer iocret,enable_timing,ncfile_id,ncfile_tmax,irnhret
      integer, parameter :: lugb=11,lugi=31,lucard=12,lgvcard=14,lout=51
c
      type (datecard) inp
      type (trackstuff) trkrinfo
      type (netcdfstuff) netcdfinfo

c     --------------------------------------------------------

      call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
      write (6,31) date_time(5),date_time(6),date_time(7)
  31  format (1x,'TIMING: beginning ...  ',i2.2,':',i2.2,':',i2.2)

      call w3tagb('GETTRK  ',1999,0104,0058,'NP22   ')

      pi = 4. * atan(1.)   ! Both pi and dtr were declared in module 
      dtr = pi/180.0       ! trig_vals, but were not yet defined.
      ncfile_has_hour0 = 'n'  ! Default value; set in read_netcdf_hours
c
      call read_nlists (inp,trkrinfo,netcdfinfo)
      enable_timing=trkrinfo%enable_timing

      call read_fhours (ifhmax)

      call read_tcv_card (lucard,maxstorm,trkrinfo,numtcv,iret)

      if (iret == 0) then
        if ( verb .ge. 3 ) then
          print *,'After read_tcv_card, num vitals = ',numtcv
        endif
      else
        if ( verb .ge. 1 ) then
         print '(/,a50,i4,/)','!!! ERROR: in read_tcv_card, rc= ',iret
        endif
        goto 890
      endif

      call read_gen_vitals (lgvcard,maxstorm,trkrinfo,numtcv,iret)

      if (iret == 0) then
        if ( verb .ge. 3 ) then
          print *,'After read_gen_vitals, total number of vitals (both'
     &         ,' TC and non-TC) now = ',numtcv
        endif
      else
        if ( verb .ge. 1 ) then
          print '(/,a50,i4,/)','!!! ERROR: in read_gen_vitals, rc= '
     &         ,iret
        endif
        goto 890
      endif

      if (inp%file_seq == 'onebig') then
        if (trkrinfo%inp_data_type == 'netcdf') then
          ncfile = netcdfinfo%netcdf_filename
          print *,' '
          print *,'before open_ncfile call, ncfile= ',ncfile
          call open_ncfile (ncfile,ncfile_id)
          print *,'after open_ncfile call, ncfile_id= ',ncfile_id
          call read_netcdf_hours (ncfile,ncfile_id,ncfile_tmax,ifhmax
     &                           ,ncfile_has_hour0,netcdfinfo,irnhret)
          if (irnhret /= 0) then
            print *,'(/,a32,a5,i4,/)','!!! ERROR: in read_netcdf_hours,'
     &             ,' rc= ',irnhret
            goto 890
          endif
        else
          call open_grib_files (inp,lugb,lugi,'dummy','dummy',lout,iret)
          if (iret /= 0) then
            print '(/,a50,i4,/)','!!! ERROR: in open_grib_files, rc= '
     &             ,iret
            goto 890
          endif
        endif
      endif

      call tracker (inp,maxstorm,numtcv,ifhmax,trkrinfo,ncfile
     &             ,ncfile_id,netcdfinfo,ncfile_has_hour0
     &             ,ncfile_tmax,itret)
c
890   continue

      igcret=0
      iicret=0
      iocret=0

      inquire (unit=lugb, opened=file_open)
      if (file_open) call baclose(lugb,igcret)
      inquire (unit=lugi, opened=file_open)
      if (file_open) call baclose(lugi,iicret)
      inquire (unit=lout, opened=file_open)
      if (file_open) call baclose(lout,iocret)
      if ( verb .ge. 3 ) then
        print *,'baclose: igcret= ',igcret,' iicret= ',iicret
        print *,'baclose: iocret= ',iocret
      endif
      call w3tage('GETTRK  ')
c
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine tracker (inp,maxstorm,numtcv,ifhmax,trkrinfo,ncfile
     &                   ,ncfile_id,netcdfinfo,ncfile_has_hour0
     &                   ,ncfile_tmax,itret)
c
c     ABSTRACT: This subroutine is the core of the program.  It contains
c     the main loop for looping through all the forecast hours and all
c     the storms.  Basically, the way it works is that it has an outer 
c     loop that loops on the forecast hour.  At the beginning of this 
c     loop, the data are read in for all parameters and levels needed
c     for tracking.  The full regional or global grid is read in. 
c     If vorticity was not read in (some of the centers do not send us
c     vorticity), then vorticity calculations are done on the whole 
c     grid at both 850 and 700 mb.  Then the program goes into the inner
c     loop, which loops on storm number (program originally set up to 
c     handle a max of 15 storms).  For each storm, subroutine 
c     find_maxmin is called for the following parameters: Rel Vort and  
c     geopotential hgt at 700 & 850 mb, and MSLP.  Within find_maxmin,
c     a barnes analysis is performed over the guess position of the 
c     storm to find the max or min value, and then iteratively, the 
c     grid size is cut in half several times and the  barnes analysis
c     rerun to refine the positioning of the max or min location.  After
c     the center positions for these parameters have been obtained, 
c     subroutine  get_uv_center is called to get a center fix for the 
c     minimum in the wind field, specifically, a minimum in the
c     magnitude of the wind speed (vmag).  The calculation of the vmag
c     minimum is done differently than the calculation for the other
c     parameters;  for vmag, the grid near the storm center guess 
c     position is interpolated down to a very fine grid, and then 
c     find_maxmin is called and a barnes analysis is done on that 
c     smaller grid.  For vmag, there are no further calls made to barnes
c     with a smaller grid, since the grid has already been interpolated 
c     down to a smaller grid.  Once all of the parameter center fixes 
c     have been made, subroutine  fixcenter is called to average these 
c     positions together to get a best guess fix position.  Then a check
c     is done with a call to subroutine  is_it_a_storm to make sure that
c     the center that we have found does indeed resemble a tropical 
c     cyclone.  Finally, subroutine  get_next_ges is called to make a 
c     guess position for the next forecast time for this storm.
c
c     INPUT:
c     inp        contains input date and model number information
c     maxstorm   maximum # of storms to be handled
c     numtcv     number of storms read off of the tcvitals file
c     ifhmax     max number of analysis & forecast times to be handled
c     trkrinfo   derived type that holds/describes various tracker parms
c     ncfile     if the input data type is netcdf, then this ncfile 
c                variable contains the name of the netcdf file
c     ncfile_id  if the input data type is netcdf, then this ncfile_id
c                variable contains an integer id assigned to the netcdf
c                file from the open_ncfile subroutine
c     ncfile_has_hour0  character flag (y|n) that, if the tracker is 
c                running on NetCDF data, tells if the NetCDF file 
c                actually contains hour0 data or not (some, like the 
c                2016 version of FV3, do not).
c     ncfile_tmax integer with max number of time levels in the input
c                NetCDF file, as read in from the NetCDF file itself in
c                subroutine  read_netcdf_fhours.
c
c     OUTPUT:
c     itret      return code from this subroutine
c 
c     LOCAL PARAMETERS:
c     storm      contains the tcvitals for the storms
c     stormswitch 1,2 or 3 (see more description under Main pgm section)
c     slonfg     first guess array for longitude
c     slatfg     first guess array for latitude
c     maxtime    Max number of forecast times program can track
c     maxtp      Max number of tracked parameters program will track.
c                Currently (7/2015), this maxtp is 11, and these 11 are
c                listed just a few lines below.
c     readflag   L  Indicates status of read for each of 16 parms:
c                1: 850 mb absolute vorticity
c                2: 700 mb absolute vorticity
c                3: 850 mb u-comp
c                4: 850 mb v-comp
c                5: 700 mb u-comp
c                6: 700 mb v-comp
c                7: 850 mb gp hgt
c                8: 700 mb gp hgt
c                9: MSLP
c                10: near-surface u-comp
c                11: near-surface v-comp
c                12: 500 mb u-comp
c                13: 500 mb v-comp
c                14: Mean temperature, centered at 400 mb
c                15: 500 mb gp hgt
c                16: 200 mb gp hgt
c                17: Land-Sea Mask (for use in tcgen applications, and 
c                                   even there, it's optional)
c
c     calcparm   L  indicates which parms to track and which not to.
c                Array positions are defined exactly as for clon
c                and clat, listed next, except that, in general, when
c                flag 3 is set to a value, flag 4 is set to the same 
c                value as 3, and when flag 5 is set to a value, flag
c                6 is set to the same value as 5.  This is because 
c                3 & 4 are for the 850 mb winds, and if either u or
c                v is missing, we obviously can't calculate the 
c                magnitude of the wind.  The same applies for 5 & 6,
c                which are for the 700 mb winds. And also for reference,
c                here is a list of all the variables & levels for the 
c                tracked parameters (i.e., the "calcparm" elements):
c
c                 1: 850 mb relative vorticity
c                 2: 700 mb relative vorticity
c                 3: 850 mb wind circulation difference
c                 4: NOT USED
c                 5: 700 mb wind circulation differenc
c                 6: NOT USED
c                 7: 850 mb geopotential height
c                 8: 700 mb geopotential height
c                 9: MSLP
c                10: 10-m wind circulation difference
c                11: 10-m relative vorticity
c                12: 500-850 mb thickness (lower level)
c                13: 200-500 mb thickness (upper level)
c                14: 200-850 mb thickness (deep-layer)
c 
c     clon,clat: Holds the coordinates for the center positions for
c                all storms at all times for all parameters.
c                (max_#_storms, max_fcst_times, max_#_parms).
c                For the third position (max_#_parms), here they are:
c                 1: Relative vorticity at 850 mb
c                 2: Relative vorticity at 700 mb
c                 3: Wind circulation difference at 850 mb
c                 4: NOT CURRENTLY USED
c                 5: Wind circulation difference at 700 mb
c                 6: NOT CURRENTLY USED
c                 7: Geopotential height at 850 mb
c                 8: Geopotential height at 700 mb
c                 9: Mean Sea Level Pressure
c                10: Wind circulation difference at 10 m
c                11: Relative vorticity at 10 m
c                12: Lower-level thickness (500-850)
c                13: Upper-level thickness (200-500)
c                14: Deep-Layer thickness (200-850)
c
c     xmaxwind   Contains maximum near-surface wind near the storm
c                center for each storm at each forecast hour.
c     stderr     Standard deviation of the position "errors" of the 
c                different parameters for each storm at each time.
c     fixlat,fixlon: Contain the final coordinates for each storm at
c                each forecast hour.  These coordinates are a 
c                weighted average of all the individual parameter
c                positions (hgt, zeta, mslp, vmag).
c     cvort_maxmin: Contains the characters 'max' or 'min', and is 
c                used when calling the  find_maxmin routine for the
c                relative vorticity (Look for max in NH, min in SH).
c     vradius    Contains the distance from the storm fix position to
c                each of the various near-surface wind threshhold 
c                distances in each quadrant. 
c                (3,4) ==> (# of threshholds, # of quadrants)
c                See subroutine  getradii for further details.
c     wfract_cov Fractional coverage (areal coverage) of winds
c                exceeding a certain threshold (34, 50, 64 kts) in
c                each quadrant.
c                (5,5,3) ==> (# of quadrants + 1, # of distance bins,
c                             # of thresholds).
c                The "extra" array size for quadrants (5, instead of 4)
c                is there to hold the total (i.e., "whole disc")
c                statistics.
c                See subroutine  get_fract_wind_cov for further details
c
c     er_wind    Quadrant winds in earth-relative framework
c     sr_wind    Quadrant winds in storm-relative framework
c     er_vr      Quadrant radial winds in earth-relative framework
c     sr_vr      Quadrant radial winds in storm-relative framework
c     er_vt      Quadrant tangential winds in earth-relative framework
c     sr_vt      Quadrant tangential winds in storm-relative framework
c
c     isastorm   Character array used in the call to is_it_a_storm,
c                tells whether the minimum requirement for an MSLP
c                gradient was met (isastorm(1)), whether for the midlat
c                and tcgen cases if a closed mslp contour was found
c                (isastorm(2)), and if a circulation exists at 850 mb
c                (isastorm(3)).  Can have a value of 'Y' (requirement
c                met), 'N' (requirement not met) or 'U' (requirement
c                undetermined, due to the fact that no center location
c                was found for this parameter).
c     maxmini    These 2 arrays contain the i and j indeces for the
c     maxminj    max/min centers that are found using the rough check
c                in first_ges_ctr and subsequent routines.  Only needed
c                for a midlatitude or a genesis run, NOT needed for a
c                TC tracker run.
c     stormct    Integer: keeps and increments a running tab of the
c                number of storms that have been tracked at any time
c                across all forecast hours.  Used only for midlat or
c                tcgen runs.
c     gridprs    This contains the actual value of the minimum pressure
c                at a gridpoint.  The  barnes analysis will return an
c                area-averaged value of pressure; this variable will
c                contain the actual minimum value at a gridpoint near
c                the lat/lon found by the  barnes analysis.
c     closed_mslp_ctr_flag  This flag keeps track of the value of the 
c                closed contour flag returned from subroutine
c                check_closed_contour.
c     vt850_flag This flag keeps track of the value of the flag for 
c                the 850 mb Vt check.
c-----
c
      USE def_vitals; USE inparms; USE tracked_parms; USE error_parms
      USE set_max_parms; USE level_parms; USE grid_bounds; USE trkrparms
      USE contours; USE atcf; USE radii; USE trig_vals; USE phase
      USE gen_vitals; USE structure; USE verbose_output 
      USE waitfor_parms; USE module_waitfor; USE netcdf_parms
      USE tracking_parm_prefs
c         
      implicit none
c
      type (datecard) inp
      type (trackstuff) trkrinfo
      type (netcdfstuff) netcdfinfo
      type (cint_stuff) contour_info
c
      character, allocatable :: closed_mslp_ctr_flag(:,:)*1
      character, allocatable :: vt850_flag(:,:)*1
      character :: r34_check_okay*1,had_to_try_backup_850_vt_check*1
      character :: need_to_expand_r34(4)*1,ncfile_has_hour0*1
      character*(*), intent(in) :: ncfile
      integer :: ncfile_id
      integer, parameter :: nreadparms=17
      real, allocatable :: prstemp(:),iwork(:)
      integer, parameter :: numdist=14,numquad=4,lout=51
      integer, allocatable :: prsindex(:)
      integer   imax,jmax,ifh,ist,irf,jj,istmp,ifhtemp,itret,ivpa
      integer   isiret1,isiret2,isiret3,idum,m,iix,jjx,imode,numtcv
      integer   iha,isa,iua,iva,iza,maxstorm,ivort,ifix,jfix,issret
      integer   imoa,imoca,iksa,isda,ileadtime,leadtime_check
      integer   ioaret,ioaxret,ifgcret,ifmret,igugret,isoiret,icccret
      integer   igrret,igmwret,iorret,ignret,iovret,icbret,igucret,ita
      integer   ifilret,ifret,iaret,isret,iotmret,iwa,iisa,sl_counter
      integer   iicret,igcret,pfcret,igwcret,imbowret,iatret
      logical(1), allocatable :: valid_pt(:,:)
      logical(1), allocatable :: masked_outc(:,:),masked_out(:,:)
      logical(1) readflag(nreadparms),calcparm(maxtp,maxstorm)
      logical(1) tracking_previously_known_storms
      logical(1) need_to_flip_lats,need_to_flip_lons
      logical(1) file_open,first_time_thru_getradii
      character cvort_maxmin*3,isastorm(3)*1,ccflag*1,gotten_avg_value*1
      character cmaxmin*3,get_last_isobar_flag*1,wcore_flag*1
      character gfilename*120,ifilename*120,gridmove_status*7
      integer   vradius(3,4),igridzeta(nlevgrzeta),imeanzeta(nlevgrzeta)
      integer   maxmini(maxstorm),maxminj(maxstorm),pdf_ct_bin(16)
      integer   ifcsthour,stormct,prevstormct,kf,istmspd,istmdir,iggret
      integer   igiret,iuret,jdum,icount,ilonfix,jlatfix,igpret,ifhmax
      integer   ibeg,jbeg,iend,jend,ix1,ix2,n,ilev,npts,icpsa,igzvret
      integer   igfwret,ioiret,igisret,iofwret,iowsret,igwsret,igscret
      integer   pdf_ct_tot,lugb,lugi,iret,icmcf,iccfh,ivt8f
      integer   waitfor_gfile_status,waitfor_ifile_status,ncfile_tmax
      integer   wait_max_ifile_wait,ivr,r34_good_ct,itha,ilma,inctcv
      integer   date_time(8)
      character (len=10) big_ben(3)
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      gridprs(maxstorm,maxtime)
      real      wfract_cov(5,5,3)
      real      er_wind(numquad,numdist)
      real      sr_wind(numquad,numdist)
      real      er_vr(numquad,numdist)
      real      er_vt(numquad,numdist)
      real      sr_vr(numquad,numdist)
      real      sr_vt(numquad,numdist)
      real      ike(max_ike_cats)
      real      clon(maxstorm,maxtime,maxtp)
      real      clat(maxstorm,maxtime,maxtp)
      real      xmaxwind(maxstorm,maxtime),xmeanzeta
      real      stderr(maxstorm,maxtime),xval(maxtp),cps_vals(3)
      real      gridpoint_maxmin,dist,distnm,xknots,xmaxspeed
      real      uvgeslon,uvgeslat,xavg,stdv,search_cutoff,re,ri,dx,dy
      real      xinp_fixlat,xinp_fixlon,degrees,plastbar,rlastbar
      real      xinterval_fhr,cc_time_sum_tot,cc_time_sum_yes
      real      rmax,sdp,wdp,paramb,vtl_slope,vtu_slope
      real      xsfclon,xsfclat,cc_time_pct,radmax,r34_dist_thresh
      real      prev_latmax,prev_latmin,prev_lonmax,prev_lonmin
      real      vradius_km,hold_old_contint,tcv_max_wind_ms
      real      tcv_mslp_pa,r34_from_tcv,roci_from_tcv
      real      proci_from_tcv,prs_contint_thresh
      integer   enable_timing,igrct
      character(pfc_cmd_len) :: pfc_final
c
      prev_latmax = -999.0
      prev_latmin = -999.0
      prev_lonmax = -999.0
      prev_lonmin = -999.0
      enable_timing=trkrinfo%enable_timing
      icmcf = 0
      ivt8f = 0
      if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') then
        allocate (closed_mslp_ctr_flag(maxstorm,ifhmax),stat=icmcf)
        allocate (vt850_flag(maxstorm,ifhmax),stat=ivt8f)
        ! Initialize flags to 'u', not 'n'.  That way, 
        ! when we are evaluating its value back over recent past hours,
        ! we can distinguish a "no" value from an initialized value of
        ! 'u' for which a storm hadn't yet been detected.
        closed_mslp_ctr_flag = 'u'
        vt850_flag = 'u'
      endif
   
      allocate (prsindex(maxstorm),stat=iisa)
      allocate (prstemp(maxstorm),stat=iva)
      allocate (iwork(maxstorm),stat=iwa)
      if (iisa /= 0 .or. iva /= 0 .or. iwa /= 0 .or. icmcf /= 0 .or.
     &     ivt8f /= 0) then
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in sub tracker allocating prsindex,'
          print *,'!!! prstemp or iwork array for storms: iisa = ',iisa
          print *,'!!! iva= ',iva,' iwa= ',iwa,' icmcf= ',icmcf
          print *,'!!! ivt8f= ',ivt8f
        endif
        itret = 94
        return    
      endif

      ike = 0.0
      sdp = 0.0
      wdp = 0.0

      clon = 0.0
      clat = 0.0
      stderr = stermn    ! initialize stderr to 0.1 (error_parms)
      itret = 0
      xmaxwind = 0.0
      stormct = 0

      ! It is critical to initialize the gridprs array to something
      ! greater than normal atmospheric pressures (I've chosen 9999.99 
      ! mb).  This is so that in the  sort on pressure before stormloop,
      ! the top of the  sorting index array will be filled with pressure
      ! values from active storms, while those inactive 9999 storms 
      ! will fill the bottom of the  sorting index array (prsindex).

      gridprs =  999999.0
      fixlon  =    -999.0
      fixlat  =    -999.0

      if (inp%file_seq == 'multi') then
        ! Each tau will have a separate file, starting with unit 
        ! number 200 (GRIB data) and 5200 (GRIB index file) and 
        ! incrementing upwards from there for each tau.
        if (trkrinfo%gribver == 1) then
          lugb =  200
c          lugi = 5200
          lugi = 600 ! 3/2017: w3lib on Jet cannot handle unit #'s >999
        else
          lugb =  200
c          lugi = 5200
          lugi = 600 ! 3/2017: w3lib on Jet cannot handle unit #'s >999
        endif
      else
        ! All lead times are included in one big file.  These values
        ! for lugb and lugi will remain static for all taus.
        lugb = 11
        lugi = 31
      endif

      ifh = 1

      if ( verb .ge. 3 ) then
        print *,'top of tracker, ifh= ',ifh,' ifhmax= ',ifhmax
      endif

      ifhloop: do while (ifh <= ifhmax)

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'*-------------------------------------------*'
          write (6,402) ifhours(ifh),ifclockmins(ifh)
  402     format (1x,'*   New forecast hour: ',i4,':',i2.2)
          print *,'*-------------------------------------------*'
        endif

        if (inp%file_seq == 'multi') then

          lugb = lugb + 1
          lugi = lugi + 1
        
          call get_grib_file_name (ifh,gfilename,ifilename)

          if (use_waitfor == 'y') then

            ! First check for existence of grib file....

            call waitfor(trim(gfilename),waitfor_gfile_status
     &                  ,wait_min_age,wait_min_size,wait_max_wait
     &                  ,wait_sleeptime)
            if (waitfor_gfile_status /= 0) then
              print *,' '
              write(6,405) 
              write(6,406) wait_max_wait,trim(gfilename)
  405         format('ERROR: TIMEOUT from waitfor for GRIB file.') 
  406         format('Waited longer than ',I0,' seconds for "',A,'"')
              stop 91
            endif

            ! Now check for existence of index file.  Use a separate 
            ! max_wait time -- a much shorter one -- since once the 
            ! grib file is there, the index file should appear within
            ! a matter of seconds.  Also, the index file is much 
            ! smaller, so set the wait_min_size accordingly.

            wait_max_ifile_wait = 180
            wait_min_size = 500
            call waitfor(trim(ifilename),waitfor_ifile_status
     &                  ,wait_min_age,wait_min_size,wait_max_ifile_wait
     &                  ,wait_sleeptime)
            if (waitfor_ifile_status /= 0) then
              print *,' '
              write(6,415)
              write(6,416) wait_max_ifile_wait,trim(ifilename)
  415         format('ERROR: TIMEOUT from waitfor for INDEX file.')
  416         format('Waited longer than ',I0,' seconds for "',A,'"')
              stop 91
            endif

          endif

          call open_grib_files (inp,lugb,lugi,gfilename,ifilename
     &         ,lout,iret)
        
          if (iret /= 0) then
            print '(/,a50,i4,/)','!!! ERROR: from open_grib_files, rc= '
     &           ,iret
            print *,'!!! Files after hour0 are missing, '
     &             ,'exiting normally'
            stop 0
          endif
        endif

        if (trkrinfo%inp_data_type == 'grib') then
          inquire (unit=lugb, opened=file_open)
          if (file_open) then
            print *,'TEST b4 getgridinfo, unit lugb= ',lugb,' is OPEN'
          else
            print *,'TEST b4 getgridinfo, unit lugb= ',lugb,' is CLOSED'
          endif
  
          inquire (unit=lugi, opened=file_open)
          if (file_open) then
            print *,'TEST b4 getgridinfo, unit lugi= ',lugi,' is OPEN'
          else
            print *,'TEST b4 getgridinfo, unit lugi= ',lugi,' is CLOSED'
          endif
        endif

        !--------------------------------------------------------------
        ! Within this next IF statement, we deal with writing out atcf
        ! records for storms for the case in which we have netcdf data,
        ! but that netcdf data does not have hour0 data (as of Nov 2016,
        ! this is the case for FV3 data).  In this case, we write out 
        ! missing values for the hour0 time, and then we update the 
        ! guess for next lead time by extrapolating data from TC Vitals.
        ! Note in the IF statement itself, "iftotalmins" is the array 
        ! of *user-requested* lead times, meaning that the user has 
        ! requested to look at hour0, but the ncfile_has_hour0 flag
        ! indicates the hour0 time is not in the NetCDF data.
        !--------------------------------------------------------------

        if (ifh == 1 .and. iftotalmins(ifh) == 0 .and.
     &      trkrinfo%inp_data_type == 'netcdf'   .and.
     &      ncfile_has_hour0 == 'n') then

          null_netcdf_hour0_storm_loop: do inctcv = 1,numtcv

            call output_atcfunix (-999.0
     &                      ,-999.0,inp,inctcv
     &                      ,0,0.0
     &                      ,0.0,vradius,maxstorm,trkrinfo
     &                      ,-99.0,-99.0,-99.0
     &                      ,cps_vals,wcore_flag,ioaxret)
            imeanzeta = -99
            igridzeta = -99
            if (trkrinfo%type == 'midlat' .or.
     &          trkrinfo%type == 'tcgen') then
              call output_atcf_gen (-999.0   
     &           ,-999.0,inp,inctcv
     &           ,0,0.0
     &           ,0.0,vradius,maxstorm,trkrinfo
     &           ,-99,-99,-999.0,-999.0,-99.0
     &           ,cps_vals,'u',imeanzeta,igridzeta,ioaxret)
            endif 
            call output_atcf_sink (-999.0
     &                ,-999.0,inp,inctcv
     &                ,0,0.0
     &                ,0.0,vradius,maxstorm,trkrinfo
     &                ,-99,-99,imeanzeta,igridzeta
     &                ,cps_vals,-999.0,-999.0,ioaxret)
            call output_hfip (-999.0
     &                ,-999.0,inp,inctcv
     &                ,ifh,0.0
     &                ,0.0,vradius,-99.0,ioaxret)

            if (verb .ge. 3) then
              print *,' ' 
              print *,'++ NOTE: Even though a fix could not be'
              print *,'   made for this storm at 00h, we will '
              print *,'   use the storm heading info from tc'
              print *,'   vitals to create a guess for the next'
              print *,'   lead time and attempt to track again'
              print *,'   at that time.'
              print *,'   ifh= ',ifh,' ist= ',inctcv
              write (6,431) storm(inctcv)%tcv_storm_id
     &                     ,storm(inctcv)%tcv_storm_name
 431          format (1x,'  storm_id = ',a4,' storm_name = ',a9)
            endif 

            call advect_tcvitals_from_hour0 (slonfg,slatfg,maxstorm
     &                             ,inctcv,ifh,trkrinfo,iatret)

            if (iatret /= 0) then
              fixlon (inctcv,ifh) = -999.0
              fixlat (inctcv,ifh) = -999.0
              stormswitch(inctcv) = 2
              cycle null_netcdf_hour0_storm_loop
            endif 

            stormswitch(inctcv) = 1

          enddo null_netcdf_hour0_storm_loop

          ifh = ifh + 1
          cycle ifhloop

        endif

        !--------------------------------------------------------------
        ! Make call to getgridinfo in order to get info on the imax,
        ! jmax, as well as the x- and y-increments, and also to see if
        ! the grid is correctly oriented for the tracker so that the 
        ! data go north to south and west to east or if we need to flip
        ! either the lats or the lons.
        !--------------------------------------------------------------

        if (trkrinfo%inp_data_type == 'grib') then
          call getgridinfo_grib (imax,jmax,ifh,dx,dy,lugb,lugi
     &                 ,trkrinfo,need_to_flip_lats,need_to_flip_lons
     &                 ,inp,iggret)
        elseif (trkrinfo%inp_data_type == 'netcdf') then
          call getgridinfo_netcdf (ncfile_id,imax,jmax,dx,dy
     &                 ,trkrinfo,need_to_flip_lats,need_to_flip_lons
     &                 ,inp,netcdfinfo,iggret)
        else
          print *,' '
          print *,'!!! ERROR: trkrinfo%inp_data_type NOT VALID '
          print *,'!!! trkrinfo%inp_data_type= ',trkrinfo%inp_data_type
          print *,'!!! Should have value of grib or netcdf.'
          print *,'!!! EXITING....'
          print *,' '
          stop 93
        endif

        if (iggret == 0) then
          if ( verb .ge. 1 ) then
            print *,'TEST after getgridinfo in sub tracker, '
     &             ,'iggret= ',iggret
          endif
        else
          if ( verb .ge. 1 ) then
            print '(/,a50,i4,/)','!!! ERROR: in getgridinfo, rc= '
     &            ,iggret
          endif
          stop 95
        endif

        if (inp%modtyp == 'regional' .and. inp%nesttyp == 'moveable') 
     &  then 
          if (glatmax == prev_latmax .and. glatmin == prev_latmin .and.
     &        glonmax == prev_lonmax .and. glonmin == prev_lonmin) then
            ! The moveable, nested regional grid has not moved since 
            ! the last lead time.  This could be an indication that the
            ! model lost the storm and so the grid has not moved to 
            ! stay with the cyclone center. Set a flag to indicate this.
            gridmove_status = 'stopped'
          else
            gridmove_status = 'moving'
          endif
        else
          gridmove_status = 'notappl'
        endif

        prev_latmax = glatmax
        prev_latmin = glatmin
        prev_lonmax = glonmax
        prev_lonmin = glonmin

        gotten_avg_value = 'n'

c       First, allocate the working data arrays....
      
        if (allocated(valid_pt)) deallocate (valid_pt)
        if (allocated(zeta))     deallocate (zeta) 
        if (allocated(u))        deallocate (u)
        if (allocated(v))        deallocate (v)
        if (allocated(hgt))      deallocate (hgt) 
        if (allocated(slp))      deallocate (slp)
        if (allocated(tmean))    deallocate (tmean)
        if (allocated(cpshgt))   deallocate (cpshgt)
        if (allocated(thick))    deallocate (thick)
        if (allocated(lsmask))   deallocate (lsmask)
        if (allocated(masked_out))  deallocate (masked_out)
        if (allocated(masked_outc)) deallocate (masked_outc)
      
        ! Allocate all of the allocatable arrays....
      
        allocate (valid_pt(imax,jmax),stat=ivpa)
        allocate (zeta(imax,jmax,nlevzeta),stat=iza)
        allocate (u(imax,jmax,nlevs),stat=iua)
        allocate (v(imax,jmax,nlevs),stat=iva)
        allocate (hgt(imax,jmax,nlevhgt),stat=iha)
        allocate (slp(imax,jmax),stat=isa)
        allocate (tmean(imax,jmax),stat=ita)
        allocate (thick(imax,jmax,nlevthick),stat=itha)
        allocate (lsmask(imax,jmax),stat=ilma)
        allocate (masked_out(imax,jmax),stat=imoa)
        allocate (masked_outc(imax,jmax),stat=imoca)
      
        ita=0
        icpsa=0 
        if (phaseflag == 'y') then
          if (phasescheme == 'cps' .or. phasescheme == 'both') then
            if (allocated(cpshgt)) deallocate (cpshgt)
            allocate (cpshgt(imax,jmax,nlevs_cps),stat=icpsa)
          endif
        endif   

        if (iza /= 0 .or. iua /= 0 .or. iha /= 0 .or. ivpa /= 0 .or.
     &      iva /= 0 .or. isa /= 0 .or. icpsa /= 0 .or. ita /= 0 .or.
     &      itha /= 0 .or. imoa /= 0 .or. imoca /= 0 .or. ilma /= 0) 
     &     then
          
          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in sub tracker allocating arrays.'
            print *,'!!! iza = ',iza,' iua= ',iua,' iha= ',iha
            print *,'!!! iva = ',iva,' isa= ',isa,' icpsa= ',icpsa
            print *,'!!! iksa = ',iksa,' isda= ',isda,' ivpa= ',ivpa
            print *,'!!! ita = ',ita,' imoa= ',imoa,' imoca= ',imoca
            print *,'!!! itha = ',itha,' ilma= ',ilma
          endif
          itret = 94
          return
        endif

        masked_out  = .false.   ! Initialize all pts to false at each hr
        masked_outc = .false.   ! Initialize all pts to false at each hr

        if ( verb .ge. 3 ) then 
          print *,'in beginning of tracker, imax= ',imax,' jmax= ',jmax
        endif

c       Initialize all readflags to NOT FOUND for this forecast time,
c       then call subroutine to read data for this forecast time.

        zeta  = -9999.0 
        u     = -9999.0
        hgt   = -9999.0 
        v     = -9999.0
        slp   = -9999.0 
        tmean = -9999.0

        readflag = .FALSE.


        if(enable_timing/=0) then
        call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
        write (6,31) date_time(5),date_time(6),date_time(7)
 31     format (1x,'TIMING: b4 getdata ... ',i2.2,':',i2.2,':',i2.2)
        endif

        if (trkrinfo%inp_data_type == 'grib') then
          call getdata_grib (readflag,valid_pt,imax,jmax
     &               ,ifh,need_to_flip_lats,need_to_flip_lons,inp
     &               ,lugb,lugi,trkrinfo)
        elseif (trkrinfo%inp_data_type == 'netcdf') then
          call getdata_netcdf (ncfile_id,readflag,valid_pt,imax,jmax
     &               ,ifh,need_to_flip_lats,need_to_flip_lons
     &               ,ncfile_tmax,netcdfinfo)
        endif

        if(enable_timing/=0) then
        call date_and_time (big_ben(1),big_ben(2),big_ben(3),date_time)
        write (6,32) date_time(5),date_time(6),date_time(7)
 32     format (1x,'TIMING: after getdata ... ',i2.2,':',i2.2,':',i2.2)
        endif
 
c       Count how many parms were successfully read for this fcst time.
c       Also, for right now, put the value of readflag into all of the
c       calcparms for parameters 3 through 9.  Note that in getdata we
c       read in 17 parms, but in this next loop we only check the 
c       readflags up to maxtp (= 14 as of 7/2015).  That's because
c       parms 12 & 13 are for 500 mb u & v, which are not used for 
c       tracking, only for calculating the deep layer mean wind for
c       the next guess, and parm 14 is the 300-500 mb mean temperature, 
c       which is used for determining storm phase.  Parms 10 & 11 are 
c       for the near-surface winds, which are used in estimating surface
c       winds near the storm, and will now also be used as a
c       parameter for position estimates.  Finally, parm 17 is the 
c       land-sea mask, which is not used as a tracking parm.

        idum = 0
        do irf = 1,nreadparms
          if (readflag(irf)) idum = idum + 1
          if (irf > 2 .and. irf < 10) then 
            ! calcparm for parms > 9 is done further below.
            do jj=1,maxstorm
              calcparm(irf,jj) = readflag(irf)
            enddo
          endif
        enddo          

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'Of ',nreadparms,' readable parms, you read in ',idum
          print *,'parms for this fcst hour from the input grib file.'
        endif

c       If not enough tracked parms were read in, exit the program....

        if (idum == 0) then
          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in subroutine  tracker'
            print *,'!!! Not enough tracked parms read in from getdata.'
            print *,'!!! Check for a problem with the input GRIB file.'
            print *,'!!! Model identifier = ',inp%model
            print *,'!!! STOPPING EXECUTION FOR THIS MODEL'
          endif
          itret = 99
          ifhtemp = ifh
          do while (ifhtemp <= ifhmax)
            do istmp=1,maxstorm
              fixlon (istmp,ifhtemp) = -999.0
              fixlat (istmp,ifhtemp) = -999.0
            enddo
            ifhtemp = ifhtemp + 1
          enddo
          call output_all (fixlon,fixlat,inp,maxstorm,ifhmax,ioaret)
          call output_atcf (fixlon,fixlat,inp,xmaxwind,maxstorm,ifhmax
     &                     ,ioaret)
          if (ifh == 1) then
            ! Per Jim Gross (1/01), if the  tracker ran but was unable
            ! to get an initial fix (or, in this case, unable to get 
            ! the data needed to run), write out zeroes for the 00h 
            ! fixes to indicate that the  tracker ran unsuccessfully, 
            ! but don't write out any subsequent forecast times
            ! with zeroes....
            vradius = 0
            cps_vals(1) =  -9999.0
            cps_vals(2) =  -9999.0
            cps_vals(3) =  -9999.0
            wcore_flag  = 'u'   ! 'u' = initial value of 'undetermined'
            do istmp = 1,maxstorm
              if (stormswitch(istmp) /= 3) then
               ileadtime = nint(fhreal(ifh) * 100.0)
               ifcsthour = ileadtime / 100
               call output_atcfunix (-999.0,-999.0,inp,istmp
     &                   ,ifcsthour,0.0,0.0,vradius,maxstorm
     &                   ,trkrinfo,-99.0,-99.0,-99.0,cps_vals
     &                   ,wcore_flag,ioaxret)
               call output_hfip (-999.0,-999.0,inp,istmp
     &                   ,ifh,0.0,0.0,vradius,-99.0,ioaxret)
              endif
            enddo
          endif
          return
        endif

c          1: 850 mb relative vorticity
c          2: 700 mb relative vorticity
c          3: 850 mb wind circulation difference
c          4: NOT USED
c          5: 700 mb wind circulation differenc
c          6: NOT USED
c          7: 850 mb geopotential height
c          8: 700 mb geopotential height
c          9: MSLP
c         10: 10-m wind circulation difference
c         11: 10-m relative vorticity
c         12: 500-850 mb thickness (lower level)
c         13: 200-500 mb thickness (upper level)
c         14: 200-850 mb thickness (deep-layer)

c       Check the flags that were read in from the namelist for 
c       determining which parameters the user wants to track.
c       Here, check for z850, z700 and mslp....

        if (user_wants_to_track_gph850 == 'n' .or.
     &      user_wants_to_track_gph850 == 'N') then
          do jj=1,maxstorm
            calcparm(7,jj) = .FALSE.
          enddo
        endif

        if (user_wants_to_track_gph700 == 'n' .or.
     &      user_wants_to_track_gph700 == 'N') then
          do jj=1,maxstorm
            calcparm(8,jj) = .FALSE.
          enddo
        endif
          
        if (user_wants_to_track_mslp == 'n' .or.
     &      user_wants_to_track_mslp == 'N') then
          do jj=1,maxstorm
            calcparm(9,jj) = .FALSE.
          enddo
        endif
          

c       Parameters 1 & 2 are abs vorticity at 850 & 700.  If the data 
c       files had this parm at 850 & 700 (ECMWF & UKMET do NOT), then 
c       we don't need to re-calculate relative vorticity, we just need 
c       to subtract out the Coriolis component.  If the files did not
c       have vorticity, then we need to calculate relative vorticity.
c       If we're able to read vorticity or calculate it, then set the
c       vorticity calcparms to TRUE for all storms for now.

        vortloop: do ivort=1,2

          if (ivort == 1) then
            if (user_wants_to_track_zeta850 == 'n' .or.
     &          user_wants_to_track_zeta850 == 'N') then
              do jj=1,maxstorm
                calcparm(1,jj) = .FALSE.
              enddo
              cycle vortloop
            endif
          endif

          if (ivort == 2) then
            if (user_wants_to_track_zeta700 == 'n' .or.
     &          user_wants_to_track_zeta700 == 'N') then
              do jj=1,maxstorm
                calcparm(2,jj) = .FALSE.
              enddo
              cycle vortloop
            endif
          endif

          if (readflag(ivort)) then
          
            call subtract_cor (imax,jmax,dy,ivort)
            
            do jj=1,maxstorm
              calcparm(ivort,jj) = .TRUE.
            enddo
          else
            if (ivort == 1) then
              if (readflag(3) .and. readflag(4)) then
                call rvcal (imax,jmax,dx,dy,ivort,valid_pt)
                do jj=1,maxstorm
                  calcparm(1,jj) = .TRUE.
                enddo
              else
                do jj=1,maxstorm
                  calcparm(1,jj) = .FALSE.
                enddo
              endif
            else
              if (readflag(5) .and. readflag(6)) then
                call rvcal (imax,jmax,dx,dy,ivort,valid_pt)
                do jj=1,maxstorm
                  calcparm(2,jj) = .TRUE.
                enddo
              else
                do jj=1,maxstorm
                  calcparm(2,jj) = .FALSE.
                enddo
              endif
            endif
          endif

        enddo vortloop


c       Check the flags that were read in from the namelist for 
c       determining which parameters the user wants to track.
c       Here, check for user preferences for the wind circulation 
c       difference at 850 & 700...

        if (readflag(3) .and. readflag(4)) then
          if (user_wants_to_track_wcirc850 == 'n' .or.
     &        user_wants_to_track_wcirc850 == 'N') then
            do jj=1,maxstorm
              calcparm(3,jj) = .FALSE.
            enddo
          else
            do jj=1,maxstorm
              calcparm(3,jj) = .TRUE.
            enddo
          endif
        else
          do jj=1,maxstorm
            calcparm(3,jj) = .FALSE.
          enddo
        endif

        if (readflag(5) .and. readflag(6)) then
          if (user_wants_to_track_wcirc700 == 'n' .or.
     &        user_wants_to_track_wcirc700 == 'N') then
            do jj=1,maxstorm
              calcparm(5,jj) = .FALSE.
            enddo
          else
            do jj=1,maxstorm
              calcparm(5,jj) = .TRUE.
            enddo
          endif
        else
          do jj=1,maxstorm
            calcparm(5,jj) = .FALSE.
          enddo
        endif


c       Compute the sfc vorticity if sfc_u and sfc_v have been read in.

        if (readflag(10) .and. readflag(11)) then

          if (user_wants_to_track_wcircsfc == 'n' .or.
     &        user_wants_to_track_wcircsfc == 'N') then
            do jj=1,maxstorm
              calcparm(10,jj) = .FALSE.
            enddo
          else
            do jj=1,maxstorm
              calcparm(10,jj) = .TRUE.
            enddo
          endif

          if (user_wants_to_track_zetasfc == 'n' .or.
     &        user_wants_to_track_zetasfc == 'N') then
            do jj=1,maxstorm
              calcparm(11,jj) = .FALSE.
            enddo
          else
            ! The 3 in the next call to rvcal is to indicate the 3rd 
            ! level for the zeta array, which is for the surface (or 
            ! 10m) data.
            call rvcal (imax,jmax,dx,dy,3,valid_pt)
            do jj=1,maxstorm
              calcparm(11,jj) = .TRUE.
            enddo
          endif

        else
          do jj=1,maxstorm
            calcparm(10,jj) = .FALSE.
            calcparm(11,jj) = .FALSE.
          enddo
        endif


c       Compute the  thicknesses for 200-850, 200-500 and 500-850 mb
c       if the gp hgt fields have been read in for 200, 500 and 850.

        if (readflag(7) .and. readflag(15) .and. readflag(16)) then

          call thickness_calc (imax,jmax,valid_pt)

          do jj=1,maxstorm

            if (user_wants_to_track_thick500850 == 'n' .or.
     &          user_wants_to_track_thick500850 == 'N') then
              calcparm(12,jj) = .FALSE.
            else
              calcparm(12,jj) = .TRUE.
            endif

            if (user_wants_to_track_thick200500 == 'n' .or.
     &          user_wants_to_track_thick200500 == 'N') then
              calcparm(13,jj) = .FALSE.
            else
              calcparm(13,jj) = .TRUE.
            endif

            if (user_wants_to_track_thick200850 == 'n' .or.
     &          user_wants_to_track_thick200850 == 'N') then
              calcparm(14,jj) = .FALSE.
            else
              calcparm(14,jj) = .TRUE.
            endif

          enddo
        else
          if (verb .ge. 3) then
            print *,' '
            print *,'NOTE: Thickness will not be tracked since at least'
            print *,'one of the gp height fields was not read in.'
            print *,'  readflag(7)  -- 850 mb ---> ',readflag(7)
            print *,'  readflag(15) -- 500 mb ---> ',readflag(15)
            print *,'  readflag(16) -- 200 mb ---> ',readflag(16)
            print *,' '
          endif
          do jj=1,maxstorm
            calcparm(12,jj) = .FALSE.
            calcparm(13,jj) = .FALSE.
            calcparm(14,jj) = .FALSE.
          enddo
        endif
 
c       ---------------------------------------------------------------
c       Now call  find_maxmin for the variables zeta, hgt and slp. Only
c       process those storms for which stormswitch is set to 1.  If a
c       storm is selected to be processed, we still have to check the
c       calcparm for each parameter, to make sure that the particular
c       parm exists at that level and is able to be processed.
c
c       The following commented-out data statements are just included 
c       as a reference so you can see the array positioning of the 
c       different parameters and levels that are read in:
c
c       data igparm   /41,41,33,34,33,34,7,7,2,33,34,33,34,11,7,7/
c       data iglevtyp /100,100,100,100,100,100,100,100,102,sfc,sfc
c                     ,100,100,100,100,100/
c       data iglev    /850,700,850,850,700,700,850,700,0,sfc,sfc
c                     ,500,500,400,500,200/
c  
c       And also for reference, here are the variables / levels for
c       the *tracked* parameters (i.e., the "calcparm" elements):
c  
c          1: 850 mb relative vorticity
c          2: 700 mb relative vorticity
c          3: 850 mb wind circulation difference
c          4: NOT USED
c          5: 700 mb wind circulation differenc
c          6: NOT USED
c          7: 850 mb geopotential height
c          8: 700 mb geopotential height
c          9: MSLP
c         10: 10-m wind circulation difference
c         11: 10-m relative vorticity
c         12: 500-850 mb thickness (lower level)
c         13: 200-500 mb thickness (upper level)
c         14: 200-850 mb thickness (deep-layer)
c
c       NOTE: For mid-latitude cases, we will track ONLY mslp, which
c       is why we set all the other calcparms to 'false' just below.

        if (trkrinfo%type == 'midlat') then
          do m = 1,maxstorm
            calcparm(1,m) = .false.
            calcparm(2,m) = .false.
            calcparm(3,m) = .false.
            calcparm(4,m) = .false.
            calcparm(5,m) = .false.
            calcparm(6,m) = .false.
            calcparm(7,m) = .false.
            calcparm(8,m) = .false.
            calcparm(10,m) = .false.
            calcparm(11,m) = .false.
            calcparm(12,m) = .false.
            calcparm(13,m) = .false.
            calcparm(14,m) = .false.
          enddo
        endif

        if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen')
     &  then
          call sort_storms_by_pressure (gridprs,ifh,maxstorm,prsindex
     &                                 ,issret)
          if ( (ifh == 1) .or.
     &         (ifh == 2 .and. trkrinfo%inp_data_type == 'netcdf' .and.
     &          ncfile_has_hour0 == 'n') ) then
            stormct = numtcv
          endif
        endif

        prevstormct = stormct
        tracking_previously_known_storms = .true.

        stormloop: do sl_counter = 1,maxstorm

         cps_vals(1) =  -9999.0
         cps_vals(2) =  -9999.0
         cps_vals(3) =  -9999.0
         wcore_flag  = 'u'   ! 'u' = initialized value of 'undetermined'

         if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') 
     &   then
           ist = prsindex(sl_counter)
         else
           ist = sl_counter
         endif

         if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') 
     &   then

           if (ist == (prevstormct + 1)) then

             ! For the mid-latitude and tropical cyclogenesis cases, we
             ! need to scan the mslp field to find new storms.  If we 
             ! are at this point inside the if statement in stormloop,
             ! then that means we have looped through and attempted to 
             ! track all storms that have already been found up to this 
             ! point in the forecast, and we need to scan the field for
             ! any new storms at this forecast hour.  If this is for 
             ! forecast hour = 0, then right off the bat we may be 
             ! scanning the field (if there were no tcvitals records
             ! read in for this forecast), since ist = 1 and 
             ! (prevstormct + 1) = 0 + 1 = 1.  All that the call just 
             ! below to first_ges_center does is return a rough idea 
             ! of the location of new lows; more specific locations are
             ! obtained through the  barnes analysis tracking algorithm 
             ! further below.

             if (readflag(9)) then
               if (ifh > 1) then
                 ! We need the use of 2 different masks.  One 
                 ! (masked_out) is to be used when looking for new lows,
                 ! so that after we find a new low, we mask out the 
                 ! surrounding area so we don't find it on a subsequent 
                 ! search for this forecast hour.  The other 
                 ! (masked_outc) is used in the routine to check for a 
                 ! closed contour.  If checking for a closed contour
                 ! at, say 70W/25N, this and surrounding points may have
                 ! already been masked out in first_ges_center, so "N"
                 ! would misleadingly/incorrectly be returned from 
                 ! check_closed_contour, so that is why we need 2 masks.
                 ! But now after the first forecast hour (t=0), the way 
                 ! we have this set up is that we track previously known
                 ! storms first, and once we're done with them, we 
                 ! search for new storms at that same forecast hour.  
                 ! But when looking for new storms, we need to know the 
                 ! positions of the previously tracked storms at this 
                 ! current forecast hour, so we copy the masked_outc 
                 ! array to masked_out in this case....

                 masked_out = masked_outc

               endif
               call first_ges_center (imax,jmax,dx,dy,'mslp',slp
     &                ,'min',trkrinfo,ifh,valid_pt,maxstorm,masked_out
     &                ,stormct,contour_info,maxmini,maxminj,ifgcret)
               tracking_previously_known_storms = .false.
             else
               if ( verb .ge. 1 ) then
                 print *,' '
                 print *,'!!! ERROR: In subroutine  tracker, readflag'
                 print *,'!!!    for mslp indicates that the mslp data'
                 print *,'!!!    is not available for this forecast '
                 print *,'!!!    hour, and it is needed for a "midlat"'
                 print *,'!!!    or "tcgen" run of the  tracker.  '
                 print *,'!!!    We will exit....'
                 print *,'!!!    readflag(9) = ',readflag(9)
                 print *,'!!!    ifh= ',ifh
                 print *,' '
               endif
               itret = 98
               return
             endif
           endif
         endif

         xval = 0.0     ! initialize entire xval array to 0
         isastorm = 'U' ! re-initialize flag for each time, each storm
 
         select case (stormswitch(ist))

          case (1)

            vradius = 0

            if ( verb .ge. 2 ) then
              print *,'   ---------------------------------------------'
              print *,'   |      *** TOP OF STORM LOOP ***             '
              print *,'   | Beginning of storm loop in tracker for'
              print *,'   | Storm number ',ist
              write (6,418) ifhours(ifh),ifclockmins(ifh)
 418          format (1x,'   | Forecast hour: ',i4,':',i2.2)
              print *,'   | Storm name = ',storm(ist)%tcv_storm_name
              print *,'   | Storm ID   = ',storm(ist)%tcv_storm_id
              write (6,420) gstorm(ist)%gv_gen_date
     &             ,gstorm(ist)%gv_gen_fhr
     &             ,gstorm(ist)%gv_gen_lat
     &             ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &             ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
              print *,'   ---------------------------------------------'
              print *,' '
              
 420          format ('    | Gen ID (if available): ',i10.10,'_F',i3.3
     &             ,'_',i3.3,a1,'_',i4.4,a1,'_',a3)
              
            endif
c           First, make sure storm is within the grid boundaries...
 
            call check_bounds (slonfg(ist,ifh),slatfg(ist,ifh),ist,ifh
     &                        ,trkrinfo,icbret)
            if (icbret == 95) then   ! Out of regional grid bounds
              fixlon (ist,ifh) = -999.0
              fixlat (ist,ifh) = -999.0
              stormswitch(ist) = 2
              cycle stormloop
            endif

            if (slatfg(ist,ifh) > 0.0) then
              cvort_maxmin = 'max'
            else
              cvort_maxmin = 'min'
            endif

            if (calcparm(1,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for zeta at 850 mb'
              endif

              call find_maxmin (imax,jmax,dx,dy,'zeta'
     &           ,zeta(1,1,1),cvort_maxmin,ist,slonfg(ist,ifh)
     &           ,slatfg(ist,ifh),glon,glat,valid_pt,trkrinfo
     &           ,calcparm(1,ist),clon(ist,ifh,1),clat(ist,ifh,1)
     &           ,xval(1),glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif 
            endif

            if (calcparm(2,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for zeta at 700 mb'
              endif

              call find_maxmin (imax,jmax,dx,dy,'zeta'
     &           ,zeta(1,1,2),cvort_maxmin,ist,slonfg(ist,ifh)
     &           ,slatfg(ist,ifh),glon,glat,valid_pt,trkrinfo
     &           ,calcparm(2,ist),clon(ist,ifh,2),clat(ist,ifh,2)
     &           ,xval(2),glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(7,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for hgt at 850 mb'
              endif

              call find_maxmin (imax,jmax,dx,dy,'hgt'
     &           ,hgt(1,1,1),'min',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,trkrinfo,calcparm(7,ist)
     &           ,clon(ist,ifh,7),clat(ist,ifh,7),xval(7)
     &           ,glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(8,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for hgt at 700 mb'
              endif

              call find_maxmin (imax,jmax,dx,dy,'hgt'
     &           ,hgt(1,1,2),'min',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,trkrinfo,calcparm(8,ist)
     &           ,clon(ist,ifh,8),clat(ist,ifh,8),xval(8)
     &           ,glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(9,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for mslp'
              endif

              call find_maxmin (imax,jmax,dx,dy,'slp'
     &           ,slp,'min',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,trkrinfo,calcparm(9,ist)
     &           ,clon(ist,ifh,9),clat(ist,ifh,9),xval(9)
     &           ,glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(11,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for sfc zeta'
              endif

              call find_maxmin (imax,jmax,dx,dy,'zeta'
     &           ,zeta(1,1,3),cvort_maxmin,ist,slonfg(ist,ifh)
     &           ,slatfg(ist,ifh),glon,glat,valid_pt,trkrinfo
     &           ,calcparm(11,ist),clon(ist,ifh,11),clat(ist,ifh,11)
     &           ,xval(11),glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

c           The array indices for the 3 different thickness layers are
c           as follows:
c             1: 500-850
c             2: 200-500
c             3: 200-850

            if (calcparm(12,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for thickness in'
                print *,'the 500-850 mb layer.'
              endif

              call find_maxmin (imax,jmax,dx,dy,'thick'
     &           ,thick(1,1,1),'max',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,trkrinfo,calcparm(12,ist)
     &           ,clon(ist,ifh,12),clat(ist,ifh,12),xval(12)
     &           ,glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(13,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for thickness in'
                print *,'the 200-500 mb layer.'
              endif

              call find_maxmin (imax,jmax,dx,dy,'thick'
     &           ,thick(1,1,2),'max',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,trkrinfo,calcparm(13,ist)
     &           ,clon(ist,ifh,13),clat(ist,ifh,13),xval(13)
     &           ,glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

            if (calcparm(14,ist)) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'         ---    ---    ---'
                print *,'Now calling find_maxmin for thickness in'
                print *,'the 200-850 mb layer.'
              endif

              call find_maxmin (imax,jmax,dx,dy,'thick'
     &           ,thick(1,1,3),'max',ist,slonfg(ist,ifh),slatfg(ist,ifh)
     &           ,glon,glat,valid_pt,trkrinfo,calcparm(14,ist)
     &           ,clon(ist,ifh,14),clat(ist,ifh,14),xval(14)
     &           ,glatmax,glatmin,glonmax,glonmin
     &           ,inp%modtyp,ifmret)
              if (ifmret /= 0) then   ! Out of regional grid bounds
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop
              endif
            endif

c           Now get centers for wind circulation at 700 & 850 mb and
c           at 10m.  First, get a modified guess lat/lon position for
c           wind circulation.  Do this because we will be searching 
c           for this wind circulation center over a smaller area and
c           so it's more crucial to have a better first guess position.
c           This modified guess position will be an average of the first
c           guess position for this time and the  fix positions for this
c           time from some of the other parameters.

            if (slatfg(ist,ifh) >= 0.0) then
              cmaxmin = 'max'
            else
              cmaxmin = 'min'
            endif

            if (calcparm(3,ist) .and. calcparm(4,ist)) then
              call get_uv_guess (slonfg(ist,ifh),slatfg(ist,ifh)
     &                          ,clon,clat,calcparm,ist,ifh,maxstorm
     &                          ,uvgeslon,uvgeslat,igugret)
              if (igugret == 0) then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'          ---    ---    ---'
                  print *,'Now calling get_wind_circulation for 850 mb '
                endif

                print *,' '
                print *,'Before first call to get_wind_circulation, '
                print *,' glatmax= ',glatmax
                print *,' glatmin= ',glatmin
                print *,' glonmax= ',glonmax
                print *,' glonmin= ',glonmin
                print *,' trkrinfo%gridtype= ',trkrinfo%gridtype
                print *,' inp%modtyp= ',inp%modtyp
                print *,' cmaxmin= ',cmaxmin
                print *,' nlev850= ',nlev850
                print *,' u(1,1,nlev850)= ',u(1,1,nlev850)
                print *,' u(imax,jmax,nlev850)= ',u(imax,jmax,nlev850)
                print *,' imax= ',imax,'  jmax= ',jmax
                print *,' uvgeslon= ',uvgeslon,'  uvgeslat= ',uvgeslat
                print *,' dx= ',dx,' dy= ',dy,' ist= ',ist
                print *,' calcparm(3,ist)= ',calcparm(3,ist)
                print *,' clon(ist,ifh,3)= ',clon(ist,ifh,3)
                print *,' clat(ist,ifh,3)= ',clat(ist,ifh,3)
                print *,' xval(3)= ',xval(3)

                if (enable_timing/=0) then
                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  write (6,141) date_time(5),date_time(6),date_time(7)
 141              format (1x,'TIMING: Before GWC 850 ... ',i2.2,':',i2.2
     &                   ,':',i2.2)
                endif

                call get_wind_circulation (uvgeslon,uvgeslat,imax,jmax
     &               ,dx,dy,ist,850,valid_pt,calcparm(3,ist)
     &               ,clon(ist,ifh,3),clat(ist,ifh,3),xval(3),trkrinfo
     &               ,inp%modtyp,cmaxmin,igwcret)

                if (enable_timing/=0) then
                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  write (6,142) date_time(5),date_time(6),date_time(7)
 142              format (1x,'TIMING: After GWC 850 ... ',i2.2,':',i2.2
     &                   ,':',i2.2)
                endif

c                call get_uv_center (uvgeslon,uvgeslat,imax,jmax,dx,dy
c     &               ,ist,850,valid_pt,calcparm(3,ist)
c     &               ,clon(ist,ifh,3),clat(ist,ifh,3),xval(3),trkrinfo
c     &               ,igucret)

                if (igwcret /= 0) then
                  calcparm(3,ist) = .FALSE.
                  calcparm(4,ist) = .FALSE.
                endif
              else
                calcparm(3,ist) = .FALSE.
                calcparm(4,ist) = .FALSE.
                clon(ist,ifh,3) = 0.0
                clat(ist,ifh,3) = 0.0
              endif 
            endif
  
            if (calcparm(5,ist).and. calcparm(6,ist)) then
              call get_uv_guess (slonfg(ist,ifh),slatfg(ist,ifh)
     &                          ,clon,clat,calcparm,ist,ifh,maxstorm
     &                          ,uvgeslon,uvgeslat,igugret)
              if (igugret == 0) then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'          ---    ---    ---'
                  print *,'Now calling get_wind_circulation for 700 mb '
                endif

                if (enable_timing/=0) then
                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  write (6,143) date_time(5),date_time(6),date_time(7)
 143              format (1x,'TIMING: Before GWC 700 ... ',i2.2,':',i2.2
     &                   ,':',i2.2)
                endif

                call get_wind_circulation (uvgeslon,uvgeslat,imax,jmax
     &               ,dx,dy,ist,700,valid_pt,calcparm(5,ist)
     &               ,clon(ist,ifh,5),clat(ist,ifh,5),xval(5),trkrinfo
     &               ,inp%modtyp,cmaxmin,igwcret)

                if (enable_timing/=0) then
                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  write (6,144) date_time(5),date_time(6),date_time(7)
 144              format (1x,'TIMING: After GWC 700 ... ',i2.2,':',i2.2
     &                   ,':',i2.2)
                endif

c                call get_uv_center (uvgeslon,uvgeslat,imax,jmax,dx,dy
c     &               ,ist,700,valid_pt,calcparm(5,ist)
c     &               ,clon(ist,ifh,5),clat(ist,ifh,5),xval(5),trkrinfo
c     &               ,igucret)

                if (igwcret /= 0) then
                  calcparm(5,ist) = .FALSE.
                  calcparm(6,ist) = .FALSE.
                endif
              else 
                calcparm(5,ist) = .FALSE.
                calcparm(6,ist) = .FALSE.
                clon(ist,ifh,5) = 0.0
                clat(ist,ifh,5) = 0.0
              endif
            endif

            if (calcparm(10,ist)) then
              call get_uv_guess (slonfg(ist,ifh),slatfg(ist,ifh)
     &                          ,clon,clat,calcparm,ist,ifh,maxstorm
     &                          ,uvgeslon,uvgeslat,igugret)
              if (igugret == 0) then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'          ---    ---    ---'
                  print *,'Now calling get_wind_circulation for the'
                  print *,'surface (10m) level'
                endif

                ! NOTE: The 1020 in the call here is just a number/code
                ! to indicate to the subroutine to process sfc winds.

                if (enable_timing/=0) then
                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  write (6,145) date_time(5),date_time(6),date_time(7)
 145              format (1x,'TIMING: Before GWC Sfc ... ',i2.2,':',i2.2
     &                   ,':',i2.2)
                endif

                call get_wind_circulation (uvgeslon,uvgeslat,imax,jmax
     &                ,dx,dy,ist,1020,valid_pt,calcparm(10,ist)
     &                ,clon(ist,ifh,10),clat(ist,ifh,10),xval(10)
     &                ,trkrinfo,inp%modtyp,cmaxmin,igwcret)

                if (enable_timing/=0) then
                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  write (6,146) date_time(5),date_time(6),date_time(7)
 146              format (1x,'TIMING: After GWC Sfc ... ',i2.2,':',i2.2
     &                   ,':',i2.2)
                endif

c                call get_uv_center (uvgeslon,uvgeslat,imax,jmax,dx,dy
c     &               ,ist,1020,valid_pt,calcparm(10,ist)
c     &               ,clon(ist,ifh,10),clat(ist,ifh,10),xval(10)
c     &               ,trkrinfo,igucret)

                if (igwcret /= 0) then
                  calcparm(10,ist) = .FALSE.
                endif
              else
                calcparm(10,ist) = .FALSE.
                clon(ist,ifh,10) = 0.0
                clat(ist,ifh,10) = 0.0
              endif
            endif
  
c           ------------------------------------------------------
c           All of the parameter center fixes have been done.  Now 
c           average those positions together to get the best guess
c           fix position.  If a center fix is able to be made, then
c           call subroutine  get_max_wind to get the maximum near-
c           surface wind near the center, and then call  get_next_ges
c           to get a guess position for the next forecast hour.

            if (stormswitch(ist) == 1) then

              call fixcenter (clon,clat,ist,ifh,calcparm
     &             ,slonfg(ist,ifh),slatfg(ist,ifh),inp
     &             ,stderr,fixlon,fixlat,xval,maxstorm,ifret)

              if (ifret == 0) then
                if ((trkrinfo%type == 'midlat' .or.
     &               trkrinfo%type == 'tcgen') .and.
     &               trkrinfo%gridtype == 'regional')then
                  if (fixlon(ist,ifh) > (trkrinfo%eastbd + 7.0) .or.
     &                fixlon(ist,ifh) < (trkrinfo%westbd - 7.0) .or.
     &                fixlat(ist,ifh) > (trkrinfo%northbd + 7.0) .or.
     &                fixlat(ist,ifh) < (trkrinfo%southbd - 7.0)) then

                    if ( verb .ge. 3 ) then
                      print *,' '      
                      print *,'!!! For a midlat or tcgen case, a fix '
                      print *,'!!! will NOT be made for this time due'
                      print *,'!!! the storm being more than 7 degrees'
                      print *,'!!! outside the user-specified lat/lon'
                      print *,'!!! bounds for this run.  We will stop'
                      print *,'!!! tracking this storm.'
                      print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                      print *,'!!! Storm    = '
     &                       ,storm(ist)%tcv_storm_name
                      write (6,432) ifhours(ifh),ifclockmins(ifh)
 432                  format (1x,'!!! Fcst hr  = ',i4,':',i2.2)
                      print *,'!!! fixlat= ',fixlat(ist,ifh)
                      print *,'!!! fixlon= ',fixlon(ist,ifh)
                      print *,'!!! User East  Bound = ',trkrinfo%eastbd
                      print *,'!!! User West  Bound = ',trkrinfo%westbd
                      print *,'!!! User North Bound = ',trkrinfo%northbd
                      print *,'!!! User South Bound = ',trkrinfo%southbd
                    endif

                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                    if (ifh == 1) then
                      vradius = 0     
                      ileadtime = nint(fhreal(ifh) * 100.0)
                      ifcsthour = ileadtime / 100
                      call output_atcfunix (-999.0
     &                    ,-999.0,inp,ist
     &                    ,ifcsthour,0.0
     &                    ,0.0,vradius,maxstorm,trkrinfo
     &                    ,-99.0,-99.0,-99.0
     &                    ,cps_vals,wcore_flag,ioaxret)
                      imeanzeta = -99
                      igridzeta = -99
                      call output_atcf_gen (-999.0
     &                      ,-999.0,inp,ist
     &                      ,ifcsthour,0.0
     &                      ,0.0,vradius,maxstorm,trkrinfo
     &                      ,-99,-99,-999.0,-999.0,-99.0
     &                      ,cps_vals,'u',imeanzeta,igridzeta,ioaxret)
                      call output_atcf_sink (-999.0
     &                      ,-999.0,inp,ist
     &                      ,ifcsthour,0.0
     &                      ,0.0,vradius,maxstorm,trkrinfo
     &                      ,-99,-99,imeanzeta,igridzeta
     &                      ,cps_vals,-999.0,-999.0,ioaxret)
                    endif
                    cycle stormloop     
                  endif
                endif
              else
                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
              endif

c             Just because we've found a center doesn't mean there is
c             actually a storm there.  I noticed in the first year that
c             for some decaying or just weak storms, the  tracker would
c             identify a center to follow, but it may have only been
c             a weak trough passing by, or something else that's not
c             our storm.  This next subroutine checks to see that the 
c             surface pressure gradient and/or tangential winds at 
c             850 mb resemble a storm.  It is called twice; the first
c             time for MSLP, the 2nd time for 850 mb winds.  We will
c             apply these storm-checking criteria if either the mslp
c             or v850 check come back negative.  Remember, there
c             is the possibility that centers could not be found for 
c             1 or both of these parameters, in which case the isastorm
c             flag will have a value of 'U', for "undetermined".

              isiret1 = 0; isiret2 = 0; isiret3 = 0

              print *,' ttest, ifret= ',ifret

              if (ifret == 0) then

                print *,' ttest, calcparm(9,ist)= ',calcparm(9,ist)

                if (calcparm(9,ist)) then

                  ! Do a check of the mslp gradient....

                  print *,' ttest, in IF part: ' 
                  print *,'     clon(ist,ifh,9)= ',clon(ist,ifh,9)
                  print *,'     clat(ist,ifh,9)= ',clat(ist,ifh,9)
                  print *,'     xval(9)= ',xval(9)

                  call is_it_a_storm (imax,jmax,dx,dy,'slp',ist
     &                 ,valid_pt,clon(ist,ifh,9),clat(ist,ifh,9)
     &                 ,xval(9),trkrinfo,isastorm(1),isiret1)

                else

                  ! Sept 2016: There has been a hole in this logic for
                  ! a while.  If a fix can't be made for mslp (e.g., 
                  ! maybe the mslp fix was too far away from the 
                  ! guess?), then this check isn't performed.  We are 
                  ! changing this so that the mslp gradient check will
                  ! still be performed, but using the mean fixlat and
                  ! fixlon positions as the center.  Still, we first
                  ! need to check to see if mslp was even read in.  If
                  ! it wasn't, then we are just out of luck.

                  print *,' ttest, in ELSE part: '

                  if (trkrinfo%use_backup_mslp_grad_check == 'y' .or.
     &                trkrinfo%use_backup_mslp_grad_check == 'Y') then

                    print *,' ttest ELSE, readflag(9)= ',readflag(9)

                    if (readflag(9)) then

                      print *,'ttest ELSE A, ist= ',ist,' ifh= ',ifh
                      print *,'ttest ELSE A, fixlon(ist,ifh)= '
     &                       ,fixlon(ist,ifh)
                      print *,'ttest ELSE A, fixlat(ist,ifh)= '
     &                       ,fixlat(ist,ifh)

                      call fix_latlon_to_ij (imax,jmax,dx,dy,slp,'min'
     &                   ,valid_pt,fixlon(ist,ifh),fixlat(ist,ifh)
     &                   ,9999.0,ifix,jfix,gridpoint_maxmin,'tracker'
     &                   ,glatmax,glatmin,glonmax,glonmin
     &                   ,trkrinfo,ifilret)

                      print *,'ttest ELSE B, ifilret= ',ifilret

                      if (ifilret == 0) then

                        print *,'ttest ELSE B, ifilret= ',ifilret
                        print *,'ttest ELSE B, fixlon(ist,ifh)= '
     &                         ,fixlon(ist,ifh)
                        print *,'ttest ELSE B, fixlat(ist,ifh)= '
     &                         ,fixlat(ist,ifh)

                        call is_it_a_storm (imax,jmax,dx,dy,'slp',ist
     &                     ,valid_pt,fixlon(ist,ifh),fixlat(ist,ifh)
     &                     ,gridpoint_maxmin,trkrinfo,isastorm(1)
     &                     ,isiret1)

                        if (isiret1 == 0) then
                          ! Even though calcparm(9) is FALSE and mslp 
                          ! will not be used for center-fixing 
                          ! purposes, we need to fill the clat and clon
                          ! arrays just a few lines below so that 
                          ! calls to fix_latlon_to_ij below do not 
                          ! get screwed up.  So, into the clat and clon
                          ! arrays we put the mean fixlat and fixlon
                          ! positions for this lead time. 
                          clat(ist,ifh,9) = fixlat(ist,ifh)
                          clon(ist,ifh,9) = fixlon(ist,ifh)
                          xval(9) = gridpoint_maxmin
                        endif

                      endif

                    endif

                  endif

                endif

                ! If we have found a valid mslp gradient, then make
                ! a call to fix_latlon_to_ij to (1) get the actual
                ! gridpoint value of the mslp (the value previously
                ! stored in xval(9) is an area-averaged value coming
                ! from the  barnes analysis), and (2) to get the 
                ! (i,j) indices for this gridpoint to be used in the
                ! call to check_closed_contour below.
                !
                ! NOTE: If a mslp fix was not made, or if the mslp
                ! "isastorm" flag comes back as no, we make the same
                ! call to fix_latlon_to_ij, but we use the mean fix
                ! position as our input to search around, and then
                ! basically we just find the lowest mslp near that
                ! mean fix position.  There is a check on the value
                ! of xinp_fixlat and xinp_fixlon to make sure that 
                ! they contain valid values and not just the 
                ! initialized -999 values.

                if (isiret1 == 0 .and. isastorm(1) == 'Y') then
                  xinp_fixlat = clat(ist,ifh,9)
                  xinp_fixlon = clon(ist,ifh,9)
                  if (verb >= 3) then
                    print *,' ttest at location C IF....'
                    print *,'     xinp_fixlat= ',xinp_fixlat
                    print *,'     xinp_fixlon= ',xinp_fixlon
                  endif
                else
                  xinp_fixlat = fixlat(ist,ifh)
                  xinp_fixlon = fixlon(ist,ifh)
                  if (verb >= 3) then
                    print *,' ttest at location C ELSE....'
                    print *,'     xinp_fixlat= ',xinp_fixlat
                    print *,'     xinp_fixlon= ',xinp_fixlon
                  endif
                endif

                if (xinp_fixlat > -99.0 .and. xinp_fixlon > -990.0) 
     &          then
                  if (verb >= 3) then
                    print *,' ttest at location D'
                  endif
                  call fix_latlon_to_ij (imax,jmax,dx,dy,slp,'min'
     &               ,valid_pt,xinp_fixlon,xinp_fixlat
     &               ,xval(9),ifix,jfix,gridpoint_maxmin,'tracker'
     &               ,glatmax,glatmin,glonmax,glonmin
     &               ,trkrinfo,ifilret)
                  if (verb >= 3) then
                    print *,' ttest at location E, ifilret= ',ifilret
                  endif
                  if (ifilret == 0) then  
                    gridprs(ist,ifh) = gridpoint_maxmin
                  else          
                    ! Search went out of regional grid bounds....
                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                    cycle stormloop     
                  endif
                endif

                print *,' ttest at location F'

                ! For a "tracker" case, check to see if the user has
                ! requested to compute and write out the ROCI.  If 
                ! so, then we make a call to check_closed_contour,
                ! being sure to specify 999 as the number of levels
                ! to check....

                if (isiret1 == 0 .and. isastorm(1) == 'Y' .and.
     &              trkrinfo%type == 'tracker') then

                  if (trkrinfo%want_oci) then

                    if ( verb .ge. 3 ) then
                      print *,' '
                      print *,'Before call to check_closed_contour, '
                      print *,'ifix= ',ifix,' jfix= ',jfix
                      print *,'longitude= ',xinp_fixlon,'E   ('
     &                     ,360-xinp_fixlon,'W)'
                      print *,'latitude= ',xinp_fixlat
                      print *,'mean mslp value (xval(9))= ',xval(9)
                    endif

                    if (contour_info%numcont == 0) then
                      contour_info%numcont = maxconts
                    endif

                    if (xval(9) < 1100.0) then
                      ! Pressure units are in mb...
                      prs_contint_thresh = 4.0
                    elseif (xval(9) >80000.0) then
                      ! Pressure units are in Pa...
                      prs_contint_thresh = 400.0
                    else
                      if (verb .ge. 3) then
                        print *,' '
                        print *,'ERROR: Something wrong in subroutine'
                        print *,'       tracker.  The mslp value'
                        print *,'       (xval(9)) is not in range.'
                        print *,'       before call to'
                        print *,'       check_closed_contour.'
                        print *,'       xval(9) = ',xval(9) 
                        print *,'       EXITING....' 
                        print *,' ' 
                        stop 95
                      endif
                    endif
 
                    if (trkrinfo%contint < prs_contint_thresh) then
                      hold_old_contint = trkrinfo%contint
                      trkrinfo%contint = prs_contint_thresh
                      if ( verb .ge. 3 ) then
                        print *,' '
                        print *,'Before going into routine to diagnose'
                        print *,'the ROCI for a tracker run, the '
                        print *,'requested contour interval is being '
                        print *,'adjusted up (coarser) to avoid having'
                        print *,'the contour check routine break and '
                        print *,'return an invalid value.'
                        print *,'User-requested contint value (Pa) = '
     &                         ,hold_old_contint
                        print *,'Modified contint value (Pa) = '
     &                         ,trkrinfo%contint
                      endif
                    endif

                    masked_outc = .false.
                    get_last_isobar_flag = 'y'
                    call check_closed_contour (imax,jmax,ifix,jfix,slp
     &                  ,valid_pt,masked_outc,ccflag,'min',trkrinfo
     &                  ,999,contour_info,get_last_isobar_flag,plastbar
     &                  ,rlastbar,icccret)

                    if ( verb .ge. 3 ) then
                      print *,' '
                      print *,'After call to check_closed_contour, '
                      print *,'ifix= ',ifix,' jfix= ',jfix
                      print *,'longitude= ',xinp_fixlon,'E   ('
     &                     ,360-xinp_fixlon,'W)'
                      print *,'latitude= ',xinp_fixlat
                      print *,'mean mslp value (xval(9))= ',xval(9)
                      print *,'gridpoint mslp value= ',slp(ifix,jfix)
                      print *,'ccflag= ',ccflag
                      print *,'prs of last closed isobar = ',plastbar
                      print *,'radius of last closed isobar = '
     &                     ,rlastbar,' nm'
                      print *,' '
                    endif

                  endif

                endif

                ! For the midlat & tcgen cases, do a check to see if
                ! there is a closed mslp contour.  The ifix and jfix
                ! values passed into check_closed_contour are the 
                ! values for the (i,j) at the gridpoint minimum, 
                ! which was obtained just above from the call to
                ! fix_latlon_to_ij.
                ! UPDATE 7/12/2016 tpm: A change was made to fix a 
                ! hole in the logic.  Previously, for a genesis run
                ! (type = midlat or tcgen), if a fix was not made 
                ! for mslp, then the isastorm(1) flag would not be 
                ! 'Y', and so the call to check_closed_contour in
                ! the following IF statement would not be made, and
                ! that would prevent the mask from getting updated
                ! for this particular storm, allowing the same storm
                ! to be detected when the scan for new storms takes
                ! place at this lead time (i.e., after all previously-
                ! known storms from the last lead time have been 
                ! tracked).  As a fix, if that isastorm(1) flag is not
                ! 'Y', then we call a new subroutine which updates the
                ! mask based on the circulation at 850 mb.

                if (isastorm(1) == 'Y' .and. isiret1 == 0 .and.
     &              (trkrinfo%type == 'midlat' .or.
     &               trkrinfo%type == 'tcgen')) then

                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'Before call to check_closed_contour, '
                    print *,'ifix= ',ifix,' jfix= ',jfix
                    print *,'longitude= ',xinp_fixlon,'E   ('
     &                   ,360-xinp_fixlon,'W)'
                    print *,'latitude= ',xinp_fixlat
                    print *,'mean mslp value (xval(9))= ',xval(9)
                  endif

                  if (contour_info%numcont == 0) then
                    contour_info%numcont = maxconts
                  endif

                  get_last_isobar_flag = 'y'
                  call check_closed_contour (imax,jmax,ifix,jfix,slp
     &                ,valid_pt,masked_outc,ccflag,'min',trkrinfo
     &                ,999,contour_info,get_last_isobar_flag,plastbar
     &                ,rlastbar,icccret)

                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'After call to check_closed_contour, '
                    print *,'ifix= ',ifix,' jfix= ',jfix
                    print *,'longitude= ',xinp_fixlon,'E   ('
     &                   ,360-xinp_fixlon,'W)'
                    print *,'latitude= ',xinp_fixlat
                    print *,'mean mslp value (xval(9))= ',xval(9)
                    print *,'gridpoint mslp value= ',slp(ifix,jfix)
                    print *,'ccflag= ',ccflag
                    print *,'prs of last closed isobar = ',plastbar
                    print *,'radius of last closed isobar = ',rlastbar
     &                   ,' nm'
                    print *,' '
                  endif

                  ! This next bit of code adds a second layer of closed
                  ! contour checking.  This is to decrease the 
                  ! occurrence of interrupted midlat and tcgen tracks,
                  ! which usually happens when the closed contour 
                  ! criterion is not met for one time period.  So in 
                  ! this next code, we check to see if the ccflag was 
                  ! 'y' for at least half the time over the last 24h.  
                  ! For time periods shorter than 24h (e.g., the storm 
                  ! was just detected at 144h and we are now at 156h),
                  ! the threshold is still that for at least half of 
                  ! the time the system has been detected as a storm,
                  ! it must have a ccflag value of 'y'.

                  if (ccflag == 'y') then
                    closed_mslp_ctr_flag(ist,ifh) = 'y'
                  else
                    closed_mslp_ctr_flag(ist,ifh) = 'n'
                    if (ifh > 1) then
                      iccfh = ifh
                      cc_time_sum_tot = 0.0
                      cc_time_sum_yes = 0.0
                      do while (iccfh > 1 .and. 
     &                     closed_mslp_ctr_flag(ist,iccfh) /= 'u' .and.
     &                     cc_time_sum_tot < 24.0)
                        xinterval_fhr = fhreal(iccfh) - fhreal(iccfh-1)
                        cc_time_sum_tot = cc_time_sum_tot 
     &                                  + xinterval_fhr
                        if (closed_mslp_ctr_flag(ist,iccfh) == 'y') then
                          cc_time_sum_yes = cc_time_sum_yes 
     &                                    + xinterval_fhr
                        endif
                        iccfh = iccfh - 1
                      enddo
                      if (cc_time_sum_tot > 0.0) then
                        cc_time_pct = cc_time_sum_yes / cc_time_sum_tot
                      else
                        cc_time_pct = 0.0
                      endif
                      if (cc_time_pct >= 0.50) then
                        ccflag = 'y'

                        if ( verb .ge. 3 ) then
                          print *,' '
                          print *,'++ NOTE ON CLOSED CONTOUR CHECK: The'
                          print *,'   ccflag returned for this hour was'
                          print *,'   NO, but a check of recent ccflags'
                          print *,'   indicates that more than 50% of '
                          print *,'   the ccflags over the last 24h are'
                          print *,'   YES, so we will continue.'
                          print *,'   cc_time_pct= ',cc_time_pct
                          print *,' '
                        endif

                      else
                        ccflag = 'n'

                        if ( verb .ge. 3 ) then
                          print *,' '
                          print *,'!! NOTE ON CLOSED CONTOUR CHECK: The'
                          print *,'!! ccflag returned for this hour was'
                          print *,'   NO, and a check of recent ccflags'
                          print *,'   indicates that less than 50% of '
                          print *,'   the ccflags over the last 24h are'
                          print *,'   YES, so we will stop tracking.'
                          print *,'   cc_time_pct= ',cc_time_pct
                        endif

                      endif
                    endif
                  endif

                  if (ccflag == 'y') then
                    isastorm(2) = 'Y'
                  else if (ccflag == 'n') then
                    isastorm(2) = 'N'
                  endif

                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'*---------------------------------------*'
                    print *,'* After check_closed_contour...         *'
                    print *,'*---------------------------------------*'
                    print *,' '
                  endif


                else if (isastorm(1) /= 'Y' .and.
     &               calcparm(3,ist) .and.
     &              (trkrinfo%type == 'midlat' .or.
     &               trkrinfo%type == 'tcgen')) then

                  ! The isastorm(1) flag indicates that a mslp gradient
                  ! could not be found at this lead time, so the mask
                  ! cannot be updated using mslp.  Instead,
                  ! do a check of the 850 mb wind circulation 
                  ! surrounding the 850 wind circulation fix, and then
                  ! set the mask to be TRUE for all points within the
                  ! area where mean cyclonic Vt exceed +1 m/s....

c                  call check_closed_contour (imax,jmax,ifix,jfix,slp
c     &                ,valid_pt,masked_outc,ccflag,'min',trkrinfo
c     &                ,999,contour_info,get_last_isobar_flag,plastbar
c     &                ,rlastbar,icccret)

                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'Calling mask_based_on_wind_circ at '
     &                     ,ifcsthour
                  endif

                  call mask_based_on_wind_circ (imax,jmax,dx,dy,850
     &                     ,valid_pt,masked_outc,trkrinfo
     &                     ,clon(ist,ifh,3),clat(ist,ifh,3),inp%modtyp
     &                     ,imbowret)

                endif

                ! For tropical cyclones, check the avg 850 mb tangential
                ! windspeed close to the storm center....

                if (trkrinfo%type == 'tcgen' .or.
     &              trkrinfo%type == 'tracker') then

                  had_to_try_backup_850_vt_check = 'n'

                  if (calcparm(3,ist)) then

                    if (verb .ge. 3) then
                      print *,' '
                      print *,'Checking 850 mb Vt speed using 850 mb '
                      print *,'wind circulation fix: '
                      print *,' 850 mb wcirc fix lon= ',clon(ist,ifh,3)
                      print *,' 850 mb wcirc fix lat= ',clat(ist,ifh,3)
                      print *,' Multi-parm fix lon=   ',fixlon(ist,ifh)
                      print *,' Multi-parm fix lat=   ',fixlat(ist,ifh)
                      print *,' '
                    endif
 

                    call is_it_a_storm (imax,jmax,dx,dy,'v850',ist
     &                   ,valid_pt,clon(ist,ifh,3),clat(ist,ifh,3)
     &                   ,xval(3),trkrinfo,isastorm(3),isiret3)

                  else

                    ! Sept 2016: There has been a hole in this logic for
                    ! a while.  If a fix can't be made for 850 mb wind 
                    ! circulation (maybe the 850 mb wind circulation fix
                    ! was too far away from the guess?), then this check
                    ! isn't performed.  We are changing this so that the
                    ! 850 mb Vt wind speed check will still be
                    ! performed, but using the mean fixlat and fixlon 
                    ! positions as the center.  Still, we first need to
                    ! check to see if 850 mb u-comp and v-comp were even
                    ! read in. If they weren't, then we are just out 
                    ! of luck.

                    had_to_try_backup_850_vt_check = 'y'
                    isiret3 = -99

                    if (trkrinfo%use_backup_850_vt_check == 'y' .or.
     &                  trkrinfo%use_backup_850_vt_check == 'Y') then

                      if (readflag(3) .and. readflag(4)) then

                        if (verb .ge. 3) then
                          print *,' '
                          print *,'!!! NOTE: 850 mb wcirc fix not '
                          print *,'available.  We are instead '
                          print *,'checking 850 mb Vt speed using '
                          print *,'multi-parm fix position: '
                          print *,' Multi-parm fix lon=   '
     &                                      ,fixlon(ist,ifh)
                          print *,' Multi-parm fix lat=   '
     &                                      ,fixlat(ist,ifh)
                          print *,' '
                        endif

                        call is_it_a_storm (imax,jmax,dx,dy,'v850',ist
     &                     ,valid_pt,fixlon(ist,ifh),fixlat(ist,ifh)
     &                     ,0.00,trkrinfo,isastorm(3),isiret3)

                      endif
  
                    endif

                  endif
                    
                  if (calcparm(3,ist) .or. 
     &                (had_to_try_backup_850_vt_check == 'y' .and.
     &                 isiret3 == 0) ) then
                    
                    if (trkrinfo%type == 'tcgen') then
                      ! This next bit of code adds a second layer of 850
                      ! mb Vt magnitude checking.  This is to decrease 
                      ! the occurrence of interrupted tcgen tracks, 
                      ! which occasionally happens for weak storms when
                      ! this criterion is not met for one time period. 
                      ! So in this next code, we check to see if the 
                      ! vt850_flag was 'y' for at least 75% of the time
                      ! over the last 24h.  For time periods shorter 
                      ! than 24h (e.g., the storm was just detected at 
                      ! 144h and we are now at 156h), the threshold is 
                      ! still that for at least 75% of the time the 
                      ! system has been detected as a storm, it must 
                      ! have a vt850_flag value of 'y'.

                      if (isastorm(3) == 'Y') then
                        vt850_flag(ist,ifh) = 'y'
                      else
                        vt850_flag(ist,ifh) = 'n'
                        if (ifh > 1) then
                          iccfh = ifh
                          cc_time_sum_tot = 0.0
                          cc_time_sum_yes = 0.0
                          do while (iccfh > 1 .and. 
     &                         vt850_flag(ist,iccfh) /= 'u' .and.
     &                         cc_time_sum_tot < 24.0)
                            xinterval_fhr = fhreal(iccfh) - 
     &                                      fhreal(iccfh-1)
                            cc_time_sum_tot = cc_time_sum_tot 
     &                                      + xinterval_fhr
                            if (vt850_flag(ist,iccfh) == 'y') then
                              cc_time_sum_yes = cc_time_sum_yes 
     &                                        + xinterval_fhr
                            endif
                            iccfh = iccfh - 1
                          enddo
                          if (cc_time_sum_tot > 0.0) then
                            cc_time_pct = cc_time_sum_yes / 
     &                                    cc_time_sum_tot
                          else
                            cc_time_pct = 0.0
                          endif
                          if (cc_time_pct >= 0.75) then
                            isastorm(3) = 'Y'

                            if ( verb .ge. 3 ) then
                              print *,' '
                              print *,'+++ NOTE ON Vt_850 CHECK: The '
                              print *,'    isastorm flag returned for '
                              print *,'    this hour was NO, but a'
                              print *,'    check of recent vt850_flags'
                              print *,'    indicates that more than 75%'
                              print *,'    of the vt850_flags over the'
                              print *,'    last 24h are YES, so we will'
                              print *,'    continue.'
                              print *,'    cc_time_pct= ',cc_time_pct
                              print *,' '
                            endif

                          else
                            isastorm(3) = 'N'

                            if ( verb .ge. 3 ) then
                              print *,' '
                              print *,'!!! NOTE ON Vt_850 CHECK: The '
                              print *,'!!! isastorm flag returned for '
                              print *,'    this hour was NO, and a'
                              print *,'    check of recent vt850_flags '
                              print *,'    indicates that less than 75%'
                              print *,'    of the vt850_flags over the'
                              print *,'    last 24h are YES, so we will'
                              print *,'    stop tracking.'
                              print *,'    cc_time_pct= ',cc_time_pct
                            endif

                          endif
                        endif
                      endif

                    endif

                  endif   
                endif

              else

                if (trkrinfo%type == 'midlat' .or.
     &              trkrinfo%type == 'tcgen') then
                  isastorm(1) = 'N'

                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'!!! For a midlat or tcgen case, a fix '
                    print *,'!!! could not be made for mslp, '
                    print *,'!!! therefore we will stop tracking '
                    print *,'!!! for this storm.'
                  endif

                else
                  isastorm(1) = 'N'
                  isastorm(3) = 'N'

                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'!!! For a TC tracker case, a fix could'
                    print *,'!!! not be made using any tracked parms,'
                    print *,'!!! therefore we will stop tracking for'
                    print *,'!!! this storm.'
                  endif

                endif

                if ( verb .ge. 3 ) then
                  print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                  print *,'!!! Storm    = ',storm(ist)%tcv_storm_name
                  write (6,432) ifhours(ifh),ifclockmins(ifh)
                endif

                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
                cycle stormloop 

              endif

              if (isiret1 /= 0 .or. isiret2 /= 0 .or. isiret3 /= 0) 
     &             then 

                if ( verb .ge. 1 ) then
                  print *,' '
                  print *,'!!! ERROR: One of the calls to '
                  print *,'!!! is_it_a_storm produced an error.'
                  print *,'!!! Chances are this is from a call to '
                  print *,'!!! get_ij_bounds, meaning we are too close'
                  print *,'!!! to a regional grid boundary to do this '
                  print *,'!!! analysis.  Processing will continue....'
                  print *,'!!! isiret1= ',isiret1,' isiret2= ',isiret2
                  print *,'!!! isiret3= ',isiret3
                endif

              endif

              if (isastorm(1) == 'N' .or. isastorm(2) == 'N' .or.
     &            isastorm(3) == 'N') then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! At least one of the isastorm flags from'
                  print *,'!!! subroutine  is_it_a_storm is "N", so '
                  print *,'!!! either we were unable to find a good '
                  print *,'!!! mslp gradient and/or a valid 850 mb '
                  print *,'!!! circulation for the storm at this time,'
                  print *,'!!! or, for the cases of midlat or tcgen '
                  print *,'!!! tracking, a closed mslp contour could '
                  print *,'!!! not be found, thus we will stop tracking'
                  print *,'!!! this storm.'
                  print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                  print *,'!!! Storm    = ',storm(ist)%tcv_storm_name
                  write (6,432) ifhours(ifh),ifclockmins(ifh) 
                  print *,'!!! mslp gradient flag = ',isastorm(1)
                  print *,'!!! closed contour flag = ',isastorm(2)
                  print *,'!!! 850 mb winds flag = ',isastorm(3)
                  print *,' '
                endif

                fixlon (ist,ifh) = -999.0
                fixlat (ist,ifh) = -999.0
                stormswitch(ist) = 2
              endif

              ! Now do another check for the  tracker and tcgen cases.
              ! If the isastorm flags for mslp gradient and v850 BOTH
              ! came back positive AND you have been able to locate an
              ! 850 mb vort center, just do a check to make sure that
              ! the distance between the 850 vort center and the mslp
              ! center is not too great.

              if (trkrinfo%type == 'tracker' .or. 
     &            trkrinfo%type == 'tcgen') then
                if (isastorm(1) == 'Y' .and. isastorm(3) == 'Y' .and.
     &            calcparm(1,ist) .and. stormswitch(ist) == 1) then

c                  if (atcfname == 'GFSO' .and.
c     &                abs(slatfg(ist,ifh)) >= 25.0) then
c                    trkrinfo%max_mslp_850 = 405.0
c                  else if (atcfname == 'GFSO' .and.
c     &                abs(slatfg(ist,ifh)) < 25.0) then
c                    trkrinfo%max_mslp_850 = 405.0
c                  else
c                    trkrinfo%max_mslp_850 = 323.0
c                  endif

                  call calcdist (clon(ist,ifh,9),clat(ist,ifh,9)
     &                          ,clon(ist,ifh,1),clat(ist,ifh,1),dist
     &                          ,degrees)

                  if (dist > trkrinfo%max_mslp_850) then
                    if ( verb .ge. 3 ) then
                      print *,' '
                      print *,'!!! In routine  tracker, the dist betw'
                      print *,'!!! the mslp center & the 850 zeta '
                      print *,'!!! center is too great, thus we will'
                      print *,'!!! stop tracking this storm.'
                      print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                      print *,'!!! Storm    = '
     &                     ,storm(ist)%tcv_storm_name
                      write (6,432) ifhours(ifh),ifclockmins(ifh)
                      print *,'!!! Max dist allowed (km) = '
     &                     ,trkrinfo%max_mslp_850
                      print *,'!!! Actual distance  (km) = ',dist
                      print *,' '
                    endif
 
                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                  else
                    
                    if ( verb .ge. 3 ) then
                      print *,' '
                      print *,'Actual distance between the parm centers'
                      print *,'for 850 zeta and mslp is ',dist,' (km)'
                      print *,'Max dist allowed (km) = '
     &                     ,trkrinfo%max_mslp_850
                    endif

                  endif
                endif
              endif

              ! Do one final check.  Check the new fix position and 
              ! the old fix position and calculate the speed that the
              ! storm would have had to travel to get to this point.
              ! If that speed exceeds a certain threshold (~60 kt), 
              ! assume you're tracking the wrong thing and quit.
              ! Obviously, only do this for times > 00h.  The check
              ! in the if statement to see if the previous hour's 
              ! lats and lons were > -999 is for the midlat and 
              ! tcgen cases -- remember, they can have genesis at
              ! any hour of the forecast, in which case the previous
              ! forecast hour's lat & lon would be -999.

              if (ifh > 1 .and. stormswitch(ist) == 1) then
                if (fixlon(ist,ifh-1) > -999.0 .and.
     &              fixlat(ist,ifh-1) > -999.0 ) then

                  if (trkrinfo%type == 'midlat') then
                    xmaxspeed = maxspeed_ml
                  else
                    xmaxspeed = maxspeed_tc
                  endif

                  call calcdist (fixlon(ist,ifh-1),fixlat(ist,ifh-1)
     &                          ,fixlon(ist,ifh),fixlat(ist,ifh),dist
     &                          ,degrees)

                  ! convert distance from km to nm and get speed.

                  distnm = dist * 0.539638
                  xinterval_fhr = fhreal(ifh) - fhreal(ifh-1)
                  xknots = distnm / xinterval_fhr

                  if (xknots > xmaxspeed) then

                    if ( verb .ge. 3 ) then
                      print *,' '
                      print *,'!!! In routine  tracker, calculated spd'
                      print *,'!!! of the storm from the last position'
                      print *,'!!! to the current position is too high,'
                      print *,'!!! so we will stop tracking this storm'
                      print *,'!!! (For fear that we are not actually '
                      print *,'!!! tracking our storm, but have instead'
                      print *,'!!! locked onto some other feature....)'
                      print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                      print *,'!!! Storm    = '
     &                     ,storm(ist)%tcv_storm_name
                      write (6,432) ifhours(ifh),ifclockmins(ifh)  
                      print *,'!!! Max speed allowed (kt) = ',xmaxspeed
                      print *,'!!! Actual speed      (kt) = ',xknots
                      print *,' '
                    endif

                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                  else

                    if ( verb .ge. 3 ) then
                      print *,' '
                      print *,'The average speed that the storm moved'
                      print *,'at since the previous forecast time is'
     &                     ,xknots,' knots.'
                    endif

                  endif

                endif

              endif
 
            endif
 
c           Now get the maximum near-surface wind speed near the storm
c           center (get_max_wind).  Also, call  getradii to get the 
c           radii in each storm quadrant of gale-force, storm-force 
c           and hurricane force winds.

            if (readflag(10) .and. readflag(11) .and. ifret == 0
     &          .and. stormswitch(ist) == 1) then
              call get_max_wind (fixlon(ist,ifh),fixlat(ist,ifh)
     &                       ,imax,jmax,dx,dy,valid_pt,levsfc
     &                       ,xmaxwind(ist,ifh),trkrinfo,rmax,igmwret)
c              if (igmwret /= 0 .and. gridmove_status == 'stopped') then
              if (igmwret /= 0) then
                
                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! Return code from get_max_wind is /= 0. '
                  print *,'!!! rcc= igmwret= ',igmwret
                  print *,'!!! Also, this is a moveable, regional grid'
                  print *,'!!! and the grid did not change from last' 
                  print *,'!!! lead time to current one, so what has'
                  print *,'!!! likely happened is that the storm has '
                  print *,'!!! moved close to the edge of the nested '
                  print *,'!!! grid domain, but the nested grid itself'
                  print *,'!!! had stopped moving, probably because it'
                  print *,'!!! dropped or lost the storm.'
                  print *,'!!! '
                  print *,'!!! TRACKING WILL STOP FOR THIS STORM'
                  print *,'!!! '
                endif

                stormswitch(ist) = 2
                cycle stormloop
              endif

              ileadtime = nint(fhreal(ifh) * 100.0)
              ifcsthour = ileadtime / 100

              ! For the radii, we encountered a problem with radmax
              ! being too small.  It was set at 650 km.  Hurricane
              ! Sandy exceeded this in the models, so the values
              ! returned from getradii were close to the default
              ! radmax value of 650 km (350 nm), instead of higher.
              ! To fix it, we now use an iterative technique, where
              ! we start with radmax as a small value (500 km).  If
              ! getradii returns a value for R34 in a quadrant that
              ! does not exceed 0.97*radmax, then that value is ok.
              ! If it does exceed 0.97*radmax, then we bump up radmax
              ! by 50 km and call  getradii again, looking to diagnose
              ! radii only in those quadrants where the
              ! need_to_expand_r34 flag = 'n'.  BTW... note the 
              ! initial IF statement... we will only go into this 
              ! routine if the max wind just diagnosed for this lead
              ! time is at least 34 kts (17.5 m/s).

              if (xmaxwind(ist,ifh) >= 17.5) then

                vradius = 0
                first_time_thru_getradii = .true.
                r34_check_okay = 'n'
                do ivr = 1,4
                  need_to_expand_r34(ivr) = 'y'
                enddo
                radmax = 500.0  ! Initial radmax, in km

                igrct = 1

                if ( verb .ge. 3 ) then
                  write (6,242) ifcsthour,igrct,xmaxwind(ist,ifh)
     &                        ,date_time(5)
     &                        ,date_time(6),date_time(7)
 242              format (1x,'TIMING: b4 getrad_iter_loop, fhr= ',i5
     &                   ,' igrct= ',i2,' Vmax (m/s)= ',f8.3
     &                   ,'   ',i2.2,':',i2.2,':',i2.2)
                endif

                getrad_iter_loop: do while
     &          (r34_check_okay == 'n' .and. radmax <= 1050.)

                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  if ( verb .ge. 3 ) then
                    write (6,244) ifcsthour,igrct,date_time(5)
     &                          ,date_time(6),date_time(7)
 244                format (1x,'TIMING: after call getradii, fhr= ',i5
     &                     ,' igrct= ',i2,'   ',i2.2,':',i2.2,':',i2.2)
                  endif

                  call getradii (fixlon(ist,ifh),fixlat(ist,ifh),imax
     &                      ,jmax,dx,dy,valid_pt,storm(ist)%tcv_storm_id
     &                      ,ifcsthour,xmaxwind(ist,ifh),vradius
     &                      ,trkrinfo,need_to_expand_r34,radmax
     &                      ,first_time_thru_getradii,igrct,igrret)

                  if (igrret /= 0) then
                    if (verb >= 3) then
                      print *,' '
                      print *,'!!! ERROR: Return code from getradii = '
     &                       ,igrret
                      print *,'!!! Searching for radii will not be '
                      print *,'!!! completed for this lead time and'
                      print *,'!!! all radii values will be set to '
                      print *,'!!! missing.'
                      print *,' '
                      exit getrad_iter_loop
                    endif
                  endif

                  call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                               ,date_time)
                  if ( verb .ge. 3 ) then
                    write (6,245) ifcsthour,igrct,date_time(5)
     &                          ,date_time(6),date_time(7)
 245                format (1x,'TIMING: after call getradii, fhr= ',i5
     &                     ,' igrct= ',i2,'   ',i2.2,':',i2.2,':',i2.2)
                  endif

                  first_time_thru_getradii = .false.
                  igrct = igrct + 1
                  r34_dist_thresh = 0.97 * radmax
                  r34_good_ct = 0
                  do ivr = 1,4
                    vradius_km = float(vradius(1,ivr)) / 0.5396
                    if (vradius_km < r34_dist_thresh) then
                      r34_good_ct = r34_good_ct + 1
                      need_to_expand_r34(ivr) = 'n'
                    endif
                  enddo
                  if (r34_good_ct == 4) then
                    r34_check_okay = 'y'
                  endif
                  radmax = radmax + 50.0
                enddo getrad_iter_loop

                if ( verb .ge. 3 ) then
                  write (6,246) ifcsthour,igrct,xmaxwind(ist,ifh)
     &                        ,date_time(5)
     &                        ,date_time(6),date_time(7)
 246              format (1x,'TIMING: after getrad_iter_loop, fhr= ',i5
     &                   ,' igrct= ',i2,' Vmax (m/s)= ',f8.3
     &                   ,'   ',i2.2,':',i2.2,':',i2.2)
                endif

              endif

            endif

c           If the user has requested so, then call a routine to 
c           determine the type of cyclone, using Bob Hart's 
c           cyclone phase space (CPS) algorithms.  It is only used
c           for times after t=0, since for the first check (of the
c           "parameter B" thickness asymmetry), we need to know 
c           in which direction the storm is moving.  Pulling that 
c           storm movement data off of the tcvitals is not reliable
c           since the model storm may not be moving in the same 
c           direction as the observed storm.  However, we could do
c           an upgrade later where this storm movement data is 
c           pulled from the "genesis vitals", which are derived 
c           from the model forecast data itself, not the obs.

            if (phaseflag == 'y' .and. stormswitch(ist) == 1) then
              wcore_flag = 'u'   ! 'u' = undetermined
              call get_phase (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &                       ,fixlon,fixlat,valid_pt,maxstorm
     &                       ,cps_vals,wcore_flag,igpret)
            endif

            if (structflag == 'y' .or. ikeflag == 'y') then
              call get_sfc_center (fixlon(ist,ifh),fixlat(ist,ifh)
     &                      ,clon,clat,ist,ifh,calcparm,xsfclon
     &                      ,xsfclat,maxstorm,igscret)
            endif

            if (structflag == 'y' .and. stormswitch(ist) == 1) then
              call get_wind_structure (imax,jmax,inp,dx,dy
     &                     ,ist,ifh,fixlon,fixlat,xsfclon,xsfclat
     &                     ,valid_pt,er_wind,sr_wind,er_vr,sr_vr
     &                     ,er_vt,sr_vt,maxstorm,trkrinfo,igwsret)
              if (igwsret == 0) then
                call output_wind_structure (fixlon(ist,ifh)
     &                      ,fixlat(ist,ifh),xsfclon,xsfclat,inp,ist
     &                      ,ifcsthour,xmaxwind(ist,ifh)
     &                      ,gridprs(ist,ifh),er_wind,sr_wind
     &                      ,er_vr,sr_vr,er_vt,sr_vt,maxstorm,iowsret)
              endif
            endif

            if (structflag == 'y' .and. stormswitch(ist) == 1) then
              call get_fract_wind_cov (imax,jmax,inp,dx,dy
     &                     ,ist,ifh,fixlon,fixlat,xsfclon,xsfclat
     &                     ,valid_pt,calcparm,wfract_cov,pdf_ct_bin
     &                     ,pdf_ct_tot,maxstorm,trkrinfo,igfwret)
              if (igfwret == 0) then 
                call output_fract_wind (fixlon(ist,ifh)
     &                      ,fixlat(ist,ifh),xsfclon,xsfclat,inp,ist
     &                      ,ifcsthour,xmaxwind(ist,ifh)
     &                      ,gridprs(ist,ifh),wfract_cov,'earth'
     &                      ,pdf_ct_bin,pdf_ct_tot,maxstorm,iofwret)
              endif
            endif

            if (ikeflag == 'y' .and. stormswitch(ist) == 1) then
              call get_ike_stats (imax,jmax,inp,dx,dy
     &                     ,ist,ifh,fixlon,fixlat,xsfclon,xsfclat
     &                     ,valid_pt,calcparm,ike,sdp,wdp,maxstorm
     &                     ,trkrinfo,igisret)
              if (igisret == 0) then
                call output_ike (fixlon(ist,ifh)
     &                      ,fixlat(ist,ifh),xsfclon,xsfclat,inp,ist
     &                      ,ifcsthour,xmaxwind(ist,ifh)
     &                      ,gridprs(ist,ifh),ike,sdp,wdp,maxstorm
     &                      ,ioiret)
              endif
            endif

c           Now print out the current fix position and intensity
c           (in knots) to standard output.  Conversion for m/s to
c           knots (1.9427) is explained in output_atcf.

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'After call to fixcenter, fix positions at '
              write (6,442) ifhours(ifh),ifclockmins(ifh)
 442          format (1x,'forecast hour= ',i4,':',i2.2,' follow:')
              print *,' '
            endif

            if (ifret == 0 .and. stormswitch(ist) == 1) then

              if ( verb .ge. 3 ) then
                write (6,73) storm(ist)%tcv_storm_id,ifhours(ifh)
     &               ,ifclockmins(ifh),fixlon(ist,ifh)
     &               ,360.-fixlon(ist,ifh),fixlat(ist,ifh)
     &               ,int((xmaxwind(ist,ifh)*1.9427) + 0.5)
                print *,' '
              endif

              ! Only call output routines every atcffreq/100 hours....

              ileadtime = nint(fhreal(ifh) * 100.0)
              leadtime_check = mod(ileadtime,atcffreq)

              if (leadtime_check == 0) then

                ifcsthour = ileadtime / 100

                call output_atcfunix (fixlon(ist,ifh)
     &                    ,fixlat(ist,ifh),inp,ist
     &                    ,ifcsthour,xmaxwind(ist,ifh)
     &                    ,gridprs(ist,ifh),vradius,maxstorm
     &                    ,trkrinfo,plastbar,rlastbar,rmax,cps_vals
     &                    ,wcore_flag,ioaxret)

                ! Get the storm motion vector and the speed of 
                ! motion so that we can output this in the 
                ! "atcf_sink" forecast text file.

                if (ifh < ifhmax) then
                  call get_next_ges (fixlon,fixlat,ist,ifh
     &              ,imax,jmax,dx,dy,inp%model,valid_pt,readflag
     &              ,maxstorm,istmspd,istmdir,'vitals',trkrinfo
     &              ,ignret)
                else
                  istmdir = -999
                  istmspd = -999
                  ignret  = 0
                endif

                if ( verb .ge. 3 ) then
                  write (6,617) istmspd,istmdir,ignret
 617              format (1x,'+++ RPT_STORM_MOTION: istmspd= ',i5
     &                 ,' istmdir= ',i5,' rcc= ',i3)
                endif

                ! Call a routine to find the mean & max relative
                ! vorticity near the storm at 850 & 700.  These will
                ! be written out to the "atcf_sink" fcst text file.

                imeanzeta = -99
                igridzeta = -99
                call get_zeta_values (fixlon,fixlat,imax,jmax,dx,dy
     &                 ,trkrinfo,imeanzeta,igridzeta,readflag
     &                 ,valid_pt,ist,ifh,maxstorm,inp,igzvret)

                if (trkrinfo%type == 'midlat' .or.
     &              trkrinfo%type == 'tcgen') then
                  call output_atcf_gen (fixlon(ist,ifh)
     &               ,fixlat(ist,ifh),inp,ist
     &               ,ifcsthour,xmaxwind(ist,ifh)
     &               ,gridprs(ist,ifh),vradius,maxstorm,trkrinfo
     &               ,istmspd,istmdir,plastbar,rlastbar,rmax
     &               ,cps_vals,wcore_flag,imeanzeta,igridzeta,ioaxret)
                endif

                call output_atcf_sink (fixlon(ist,ifh)
     &                    ,fixlat(ist,ifh),inp,ist
     &                    ,ifcsthour,xmaxwind(ist,ifh)
     &                    ,gridprs(ist,ifh),vradius,maxstorm
     &                    ,trkrinfo,istmspd,istmdir,imeanzeta
     &                    ,igridzeta,cps_vals,plastbar,rlastbar
     &                    ,ioaxret)

                if (inp%model == 12 .and. ifcsthour == 0) then
                  ! Write vitals for GFS ens control analysis
                  call output_tcvitals (fixlon(ist,ifh)
     &                    ,fixlat(ist,ifh),inp,ist,iovret)

                endif
              endif

              ! The exception here is for the call to the  output_hfip
              ! routine, which will be called for every lead time
              ! that is processed....

              call output_hfip (fixlon(ist,ifh),fixlat(ist,ifh),inp,ist
     &                        ,ifh,xmaxwind(ist,ifh)
     &                        ,gridprs(ist,ifh),vradius,rmax,ioaxret)
            else

              if ( verb .ge. 3 ) then
                write (6,452) 'fixpos ',storm(ist)%tcv_storm_id
     &               ,' fhr= ',ifhours(ifh),ifclockmins(ifh)
     &               ,' Fix not made for this forecast hour'
 452            format (1x,a7,1x,a4,a6,i4,':',i2.2,a36)
                
                print *,' '
                print *,'!!! RETURN CODE from fixcenter not equal to 0,'
                print *,'!!! or output from is_it_a_storm indicated the'
                print *,'!!! system found was not our storm, or the '
                print *,'!!! speed calculated indicated we may have '
                print *,'!!! locked onto a different center, thus a fix'
                print *,'!!! was not made for this storm at this '
                print *,'!!! forecast hour.'
                print *,'!!! mslp gradient check       = ',isastorm(1)
                print *,'!!! mslp closed contour check = ',isastorm(2)
                print *,'!!! 850 mb winds check      = ',isastorm(3)
                print *,'!!! fixcenter return code = ifret = ',ifret
                print *,' '
              endif

              if (ifh == 1) then
                vradius = 0
                ileadtime = nint(fhreal(ifh) * 100.0)
                ifcsthour = ileadtime / 100

                if (inp%model == 1 .or. inp%model == 8 .or. 
     &              inp%model == 22) then                

                  ! For the vt=00h lead time, if the  tracker failed to 
                  ! locate a position, we are going to write out an
                  ! atcfunix record that contains the position, 
                  ! intensity, mslp and 34-kt wind radii from TC Vitals
                  ! for this storm and initial time.  Only do this for 
                  ! the GFS or GDAS runs of the  tracker.

                  tcv_max_wind_ms = float(storm(ist)%tcv_vmax)
                  tcv_mslp_pa = float(storm(ist)%tcv_pcen) * 100.0

                  ! Convert tcvitals NE 34-kt wind radius from km to nm
                  r34_from_tcv = float(storm(ist)%tcv_r15ne)
                  if (r34_from_tcv > 0.0) then
                    vradius(1,1) = int( ((r34_from_tcv*0.5396) 
     &                             / 5.0) + 0.5) * 5
                  else
                    vradius(1,1) = 0
                  endif

                  ! Convert tcvitals SE 34-kt wind radius from km to nm
                  r34_from_tcv = float(storm(ist)%tcv_r15se)
                  if (r34_from_tcv > 0.0) then
                    vradius(1,2) = int( ((r34_from_tcv*0.5396)
     &                             / 5.0) + 0.5) * 5
                  else    
                    vradius(1,2) = 0
                  endif

                  ! Convert tcvitals SW 34-kt wind radius from km to nm
                  r34_from_tcv = float(storm(ist)%tcv_r15sw)
                  if (r34_from_tcv > 0.0) then
                    vradius(1,3) = int( ((r34_from_tcv*0.5396)
     &                             / 5.0) + 0.5) * 5
                  else    
                    vradius(1,3) = 0
                  endif

                  ! Convert tcvitals NW 34-kt wind radius from km to nm
                  r34_from_tcv = float(storm(ist)%tcv_r15nw)
                  if (r34_from_tcv > 0.0) then
                    vradius(1,4) = int( ((r34_from_tcv*0.5396)
     &                             / 5.0) + 0.5) * 5
                  else    
                    vradius(1,4) = 0
                  endif

                  ! Convert tcvitals roci from km to nm
  
                  if (storm(ist)%tcv_penvrad > 0) then
                    roci_from_tcv = float(storm(ist)%tcv_penvrad)
                    rlastbar = roci_from_tcv * 0.5396
                  else
                    rlastbar = -99.0
                  endif
                  
                  ! Convert tcvitals pressure at roci from km to nm
  
                  if (storm(ist)%tcv_penv > 0) then
                    proci_from_tcv = float(storm(ist)%tcv_penv)
                    plastbar = proci_from_tcv * 100.0
                  else
                    plastbar = -99.0
                  endif

                  write (6,291) storm(ist)%tcv_storm_id
     &                         ,storm(ist)%tcv_storm_name
     &                         ,atcfymdh
  291             format (1x,'NOTE: TCVITALS_USED_FOR_ATCF_F00 '
     &                   ,' Storm ID: ',a4,' Storm name: ',a9
     &                   ,' YMDH: ',i10)

                  call output_atcfunix (slonfg(ist,ifh)
     &                      ,slatfg(ist,ifh),inp,ist
     &                      ,ifcsthour,tcv_max_wind_ms
     &                      ,tcv_mslp_pa,vradius,maxstorm,trkrinfo
     &                      ,plastbar,rlastbar,-99.0
     &                      ,cps_vals,wcore_flag,ioaxret)
                else

                  ! For all other models, we print out missing 
                  ! data values at tau=00h if the  tracker was 
                  ! unable to find the storm....

                  call output_atcfunix (-999.0
     &                      ,-999.0,inp,ist
     &                      ,ifcsthour,0.0
     &                      ,0.0,vradius,maxstorm,trkrinfo
     &                      ,-99.0,-99.0,-99.0
     &                      ,cps_vals,wcore_flag,ioaxret)

                endif

                imeanzeta = -99
                igridzeta = -99
                if (trkrinfo%type == 'midlat' .or.
     &              trkrinfo%type == 'tcgen') then
                  call output_atcf_gen (-999.0   
     &               ,-999.0,inp,ist
     &               ,ifcsthour,0.0
     &               ,0.0,vradius,maxstorm,trkrinfo
     &               ,-99,-99,-999.0,-999.0,-99.0
     &               ,cps_vals,'u',imeanzeta,igridzeta,ioaxret)
                endif
                call output_atcf_sink (-999.0
     &                    ,-999.0,inp,ist
     &                    ,ifcsthour,0.0
     &                    ,0.0,vradius,maxstorm,trkrinfo
     &                    ,-99,-99,imeanzeta,igridzeta
     &                    ,cps_vals,-999.0,-999.0,ioaxret)
                call output_hfip (-999.0
     &                    ,-999.0,inp,ist
     &                    ,ifh,0.0
     &                    ,0.0,vradius,-99.0,ioaxret)

                if (trkrinfo%type == 'tracker') then
                  ! Update 11/11: For a 'tracker' run, i.e., one in 
                  ! which we know that there is an observed storm in
                  ! the area, we will assume that there was some type
                  ! of problem in the initialization that prevented 
                  ! the storm from being found.  In this case, even 
                  ! though we have written out zeroes for the 00h
                  ! time, we want to at least try tracking again at
                  ! the next lead time.  Requested by HWRF folks....
                  if (verb .ge. 3) then
                    print *,' '
                    print *,'++ NOTE: Even though a fix could not be'
                    print *,'   made for this storm at 00h, we will '
                    print *,'   use the storm heading info from tc'
                    print *,'   vitals to create a guess for the next'
                    print *,'   lead time and attempt to track again'
                    print *,'   at that time.'
                    print *,'   ifh= ',ifh,' ist= ',ist
                    write (6,301) storm(ist)%tcv_storm_id
     &                           ,storm(ist)%tcv_storm_name
 301                format (1x,'  storm_id = ',a4,' storm_name = ',a9)
                  endif
                  call get_next_ges (slonfg,slatfg,ist,ifh
     &              ,imax,jmax,dx,dy,inp%model,valid_pt,readflag
     &              ,maxstorm,istmspd,istmdir,'tracker',trkrinfo
     &              ,ignret)
                  if (ignret /= 0) then
                    fixlon (ist,ifh) = -999.0
                    fixlat (ist,ifh) = -999.0
                    stormswitch(ist) = 2
                    cycle stormloop
                  endif
                  stormswitch(ist) = 1
                endif

              endif
              cycle stormloop
            endif


c           Now get first guess for next forecast time's position.
c           But first, if this is the first time level (ifh=1) and 
c           the user has requested that storm vitals be  output (this
c           is usually only done for model analyses in order to get 
c           an analysis position from one time to the next), we will
c           write out a storm vitals record for this time level.  
c           Note that we have already gotten the next guess position
c           info just above for the case of the repeated analysis 
c           data, so we'll just output the genesis vitals record.

            if (ifh <= ifhmax) then
              if (ifh == 1 .and. trkrinfo%out_vit == 'y') then
                call output_gen_vitals (fixlon(ist,ifh)
     &             ,fixlat(ist,ifh),inp,ist,istmspd,istmdir,iovret)
              endif
              if (ifh < ifhmax) then
                call get_next_ges (fixlon,fixlat,ist,ifh
     &            ,imax,jmax,dx,dy,inp%model,valid_pt,readflag
     &            ,maxstorm,istmspd,istmdir,'tracker',trkrinfo
     &            ,ignret)
                if (ignret /= 0) then
                  if ( verb .ge. 3 ) then
                    print *,' '
                    print *,'!!! ERROR: Problem getting first guess '
                    print *,'!!! position for next lead time.  Return'
                    print *,'!!! code from call to get_next_ges = '
                    print *,'!!! ignret = ',ignret
                    print *,'!!! Storm name = '
     &                     ,storm(ist)%tcv_storm_name
                    print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
                    print *,'!!! TRACKING WILL STOP FOR THIS STORM.'
                  endif
                  fixlon (ist,ifh) = -999.0
                  fixlat (ist,ifh) = -999.0
                  stormswitch(ist) = 2
                  cycle stormloop
                endif
              else
                istmdir = -999
                istmspd = -999
              endif
            endif
 
          case (2)
            fixlon (ist,ifh) = -999.0
            fixlat (ist,ifh) = -999.0

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'!!! Case 2 in tracker for stormswitch'
              print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
              print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
            endif

            if (ifh == 1) then
              vradius = 0
              ileadtime = nint(fhreal(ifh) * 100.0)
              ifcsthour = ileadtime / 100
              call output_atcfunix (-999.0
     &                  ,-999.0,inp,ist
     &                  ,ifcsthour,0.0
     &                  ,0.0,vradius,maxstorm,trkrinfo
     &                  ,-99.0,-99.0,-99.0
     &                  ,cps_vals,wcore_flag,ioaxret)
              imeanzeta = -99
              igridzeta = -99
              if (trkrinfo%type == 'midlat' .or.
     &            trkrinfo%type == 'tcgen') then
                call output_atcf_gen (-999.0
     &             ,-999.0,inp,ist
     &             ,ifcsthour,0.0
     &             ,0.0,vradius,maxstorm,trkrinfo
     &             ,-99,-99,-999.0,-999.0,-99.0
     &             ,cps_vals,'u',imeanzeta,igridzeta,ioaxret)
              endif
              call output_atcf_sink (-999.0
     &                  ,-999.0,inp,ist
     &                  ,ifcsthour,0.0
     &                  ,0.0,vradius,maxstorm,trkrinfo
     &                  ,-99,-99,imeanzeta,igridzeta
     &                  ,cps_vals,-999.0,-999.0,ioaxret)
              call output_hfip (-999.0
     &                  ,-999.0,inp,ist
     &                  ,ifh,0.0
     &                  ,0.0,vradius,-99.0,ioaxret)
            endif

          case (3)
          continue

c            print *,' '
c            print *,'!!! Case 3 in tracker for stormswitch'
c            print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
c            print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id

          end select
 
        enddo stormloop

        if (trkrinfo%type == 'midlat' .or.
     &      trkrinfo%type == 'tcgen') then
          ileadtime = nint(fhreal(ifh) * 100.0)
          leadtime_check = mod(ileadtime,atcffreq)
          if (leadtime_check == 0) then
            ifcsthour = ileadtime / 100
          endif
          if (trkrinfo%inp_data_type == 'grib') then
            call output_tracker_mask (masked_outc,valid_pt,ifh
     &                  ,ifcsthour,imax,jmax,iotmret)
          endif
        endif

        if(use_per_fcst_command=='y') then
c          User wants us to run a command per forecast time

!     Replace %[FHOUR] with forecast hour, %[FMIN] with forecast minute.

!     The %[] format is chosen to avoid shell syntax errors if someone
!     includes unknown %[] constructs.  A stray <FYEAR>, for example,
!     would generate syntax errors or unexpected results in some
!     shells.  

!     If an unrecognized %[xxx] sequence is used, it will be retained in
!     the final command.  This allows the underlying command to detect
!     the unreplaced %[] and use suitable default values or abort, as
!     appropriate.

           pfc_final=per_fcst_command
           call argreplace(pfc_final,pfc_cmd_len,'%[FHOUR]',            &
     &                     ifhours(ifh))
           call argreplace(pfc_final,pfc_cmd_len,'%[FMIN]',             &
     &                     iftotalmins(ifh))

           if(verb.ge.2) then
              print *,' '
              print *,'!!! Running per-fcst command'
              print *,'!!! Unparsed = ',trim(per_fcst_command)
              print *,'!!! Parsed = ',trim(pfc_final)
           endif
           call run_command(trim(pfc_final),pfcret)
           if(pfcret/=0 .and. verb.ge.1) then
              print *,' '
              print *,'!!! Non-zero exit status from per-fcst command'
              print *,'!!! Command = ',trim(pfc_final)
              print *,'!!! Exit status = ',pfcret
              print *,'!!! Continuing anyway...'
           elseif(pfcret==0 .and. verb.ge.2) then
              print *,' '
              print *,'!!! Per-fcst command returned success status (0)'
           endif
        endif

        ifh = ifh + 1
        if (ifh > ifhmax) exit ifhloop

        if (inp%file_seq == 'multi') then
          call baclose(lugb,igcret)
          call baclose(lugi,iicret)
          if ( verb .ge. 3 ) then
            print *,'baclose return code for unit ',lugb,' = igcret = '
     &           ,igcret
            print *,'baclose return code for unit ',lugi,' = iicret = '
     &           ,iicret
          endif
        endif

      enddo ifhloop
c
      call output_all (fixlon,fixlat,inp,maxstorm,ifhmax,ioaret)
      call output_atcf (fixlon,fixlat,inp,xmaxwind,maxstorm,ifhmax
     &                 ,ioaret)
c
  73  format ('fixpos  ',a4,'  fhr= ',i4,':',i2.2,'   Fix position=  '
     &       ,f7.2,'E  (',f6.2,'W)',2x,f7.2,'   Max Wind= ',i3,' kts')

      if (allocated(prstemp)) deallocate (prstemp) 
      if (allocated(prsindex)) deallocate (prsindex) 
      if (allocated(iwork)) deallocate(iwork)
      if (allocated(zeta)) deallocate (zeta) 
      if (allocated(u)) deallocate (u) 
      if (allocated(v)) deallocate (v)
      if (allocated(hgt)) deallocate (hgt) 
      if (allocated(slp)) deallocate (slp) 
      if (allocated(tmean)) deallocate (tmean)
      if (allocated(thick))    deallocate (thick)
      if (allocated(lsmask))   deallocate (lsmask)
      if (allocated(masked_out)) deallocate (masked_out) 
      if (allocated(masked_outc)) deallocate (masked_outc)
      if (allocated(cpshgt)) deallocate (cpshgt)
      if (allocated(vt850_flag)) deallocate (vt850_flag)
      if (allocated(closed_mslp_ctr_flag)) 
     &  deallocate (closed_mslp_ctr_flag)
      if (allocated(netcdf_file_time_values)) 
     &  deallocate (netcdf_file_time_values)
      if (allocated(nctotalmins)) 
     &  deallocate (nctotalmins)
c
      return 
      end   
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine argreplace(arg,n,name,val)
      ! This subroutine is used to generate the pre-forecast-command
      ! It will edit the command (argument "arg") and replace string
      ! name with value val.  That is how the per-forecast-command
      ! has these modifications:

      ! %[FHOUR]  -> replace with ->  last forecast hour
      ! %[FMIN]   -> replace with ->  last forecast minute

      implicit none

      integer, intent(in) :: n
      character(n), intent(inout) :: arg
      character(*), intent(in) :: name
      integer, intent(in) :: val

      integer found,namelen,i1,i2
      character(n) :: out

      found=index(arg,name)
      namelen=len(name)
      i1=found-1  ! last char that is before name
      i2=found+namelen  ! index of last char in name

      if(found==0) return

      out=' '

      if(found>1 .and. i2<n) then
         write(out,'(A,I0,A)') arg(1:i1),val,arg(i2:n)
      elseif(found>1) then
!        special case: name is at end of string
!        hope the value fits...
         write(out,'(A,I0)') arg(1:i1),val
      elseif(i2<n) then
!        special case: name is at beginning of string
         write(out,'(I0,A)') val,arg(i2:n)
      else
!        special case: name is the entirety of the string
!        hope the value fits...
         write(out,'(I0)') val
      endif

      arg=out

      end subroutine argreplace

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine open_grib_files (inp,lugb,lugi,gfilename,ifilename
     &                           ,lout,iret)

C     ABSTRACT: This subroutine must be called before any attempt is
C     made to read from the input GRIB files.  The GRIB and index files
C     are  opened with a call to baopenr.  This call to baopenr was not
C     needed in the cray version of this program (the files could be
C     opened with a simple Cray assign statement), but the GRIB-reading
C     utilities on the SP do require calls to this subroutine (it has
C     something to do with the GRIB I/O being done in C on the SP, and
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
c     inp      Contains user-input info on the date & data
C     lugb     The Fortran unit number for the GRIB data file
C     lugi     The Fortran unit number for the GRIB index file
c     ifh      integer index for lead time level
c     gfilename If using individual files for each tau, gfilename will 
c              contain the grib data filename for this tau.  Otherwise, 
c              if using one big file for all taus, this contains dummy 
c              character data.
c     ifilename If using individual files for each tau, gfilename will 
c              contain the grib index filename for this tau.  Otherwise,
c              if using one big file for all taus, this contains dummy 
c              character data.
C     lout     The Fortran unit number for the  output grib file
C
C     OUTPUT:
C     iret     The return code from this subroutine

      USE inparms
      USE verbose_output

      implicit none
c
      type (datecard) inp

      logical(1)  output_file_open
      logical(1)  file_open
      logical(4)  file_open4,file_open5
      character fnameg*7,fnamei*7,fnameo*7
      character(*) gfilename,ifilename
      character(120) gopen_g_file,gopen_i_file
      integer  igoret,iioret,iooret,lugb,lugi,lout,iret,nlen1,nlen2

      iret=0

      if (inp%file_seq == 'onebig') then
        fnameg(1:5) = "fort."
        fnamei(1:5) = "fort."
        fnameo(1:5) = "fort."
        write(fnameg(6:7),'(I2)') lugb
        write(fnamei(6:7),'(I2)') lugi
        write(fnameo(6:7),'(I2)') lout
        call baopenr (lugb,fnameg,igoret)
        call baopenr (lugi,fnamei,iioret)
        call baopenw (lout,fnameo,iooret)
      else

        print *,'in open_grib_files in multi else part....'

        nlen1        = len_trim(gfilename)
        gopen_g_file = trim(gfilename(1:nlen1))
        nlen2        = len_trim(ifilename)
        gopen_i_file = trim(ifilename(1:nlen2))

        print *,'  lugb= ',lugb,'  lugi= ',lugi
        print *,'nlen1= ',nlen1,' nlen2= ',nlen2
        print *,'gopen_g_file= ',gopen_g_file
        print *,'gopen_i_file= ',gopen_i_file

        write (6,81) gopen_g_file,gopen_i_file
   81   format (1x,'tpm gopen_g_file= ...',a<nlen1>
     &         ,'...  gopen_i_file= ...',a<nlen2>,'...')

        print *,'gopen_g_file= ',gopen_g_file,'....'
        print *,'gopen_i_file= ',gopen_i_file,'....'

        call baopenr (lugb,gopen_g_file,igoret)
        call baopenr (lugi,gopen_i_file,iioret)
        inquire (unit=lout, opened=output_file_open)
        if (output_file_open) then
          iooret = 0
        else
          fnameo(1:5) = "fort."
          write(fnameo(6:7),'(I2)') lout
          call baopenw (lout,fnameo,iooret)
        endif
      endif

      inquire (unit=lugb, opened=file_open)
      if (file_open) then
        print *,'TEST open_grib_files, unit lugb= ',lugb
     &        ,' is OPEN'
      else
        print *,'TEST open_grib_files, unit lugb= ',lugb
     &         ,' is CLOSED'
      endif

      inquire (unit=lugi, opened=file_open)
      if (file_open) then
        print *,'TEST open_grib_files, unit lugi= ',lugi
     &         ,' is OPEN'
      else
        print *,'TEST open_grib_files, unit lugi= ',lugi
     &         ,' is CLOSED'
      endif

      inquire (file=gopen_g_file, opened=file_open4)
      if (file_open4) then
        print *,'TEST gname  open_grib_files, gfile= '
     &        ,gopen_g_file,' is OPEN'
      else
        print *,'TEST gname  open_grib_files, gfile= '
     &        ,gopen_g_file,' is CLOSED'
      endif

      inquire (file=gopen_i_file, opened=file_open5)
      if (file_open5) then
        print *,'TEST iname  open_grib_files, ifile= '
     &         ,gopen_i_file,' is OPEN'
      else
        print *,'TEST iname  open_grib_files, ifile= '
     &         ,gopen_i_file,' is CLOSED'
      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'gettrk baopen: igoret= ',igoret,' iioret= ',iioret
     &       ,' iooret= ',iooret
      endif

      if (igoret /= 0 .or. iioret /= 0 .or. iooret /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in sub open_grib_files opening grib file'
          print *,'!!! or grib index file.  baopen return codes:'
          print *,'!!! grib  file return code = igoret = ',igoret
          print *,'!!! index file return code = iioret = ',iioret
          print *,'!!! output file return code = iooret = ',iooret
        endif

        iret = 113
        return
      endif

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine open_ncfile (filename,ncid)

c     ABSTRACT: This subroutine opens a netcdf file specified by the
c     input file "ncfile" and returns the netcdf file id that will be
c     associated with that file.
c
c     INPUT:
c     ncfile   character full-path file netcdf name
c
c     OUTPUT:
c     ncfile_id integer, netcdf id assigned to the netcdf file

      implicit none

      include "netcdf.inc"

      character*(*), intent(in) :: filename
      integer, intent(out)      :: ncid
      integer :: status

      status = nf_open (filename, NF_NOWRITE, ncid)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      end subroutine open_ncfile
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine is_it_a_storm (imax,jmax,dx,dy,cparm,ist
     &                          ,defined_pt,parmlon,parmlat
     &                          ,parmval,trkrinfo,stormcheck,isiret)

c     ABSTRACT: This subroutine is called after the center of the storm
c     has been fixed.  Its purpose is to determine whether or not 
c     the center that was found is actually a storm, and not just some
c     passing trough (this has happened in the case of decaying or weak
c     storms).  It's called twice -- once to check for a minimum MSLP
c     gradient, and once to check for a circulation at 850 mb.  The 
c     subroutine input parameter "cparm" determines which parameter to
c     check for.
c
c     INPUT:
c     imax     Num pts in i direction on input grid
c     jmax     Num pts in j direction on input grid
c     dx       Grid spacing in i-direction on input grid
c     dy       Grid spacing in j-direction on input grid
c     cparm    Char string indicating what parm is to be checked:
c              slp  = mslp, for a check of mslp gradient
c              v850 = tangential winds at 850 mb
c     ist      integer storm number (internal to the  tracker)
c     defined_pt Logical; bitmap indicating if valid data at that pt.
c     parmlon  Longitude of the max/min value for the input parameter
c     parmlat  Latitude  of the max/min value for the input parameter
c     parmval  Data value at parm's max/min point (used for mslp call)
c     trkrinfo derived type containing grid info on user boundaries
c
c     OUTPUT:
c     stormcheck Character; set to 'Y' if mslp gradient or 850 mb 
c                tangential winds check okay.
c     isiret   Return code for this subroutine.
c
      USE radii; USE grid_bounds; USE set_max_parms; USE level_parms
      USE trig_vals; USE tracked_parms; USE atcf; USE trkrparms
      USE verbose_output

      implicit none
c
      type (trackstuff) trkrinfo

      real         vt,vtavg,vr,parmlat,parmlon,parmval,dist
      real         pthresh,vthresh,degrees,dx,dy,dell,ri,radinf
      real         pgradient,xmaxpgrad
      character(*) cparm
      logical(1)   defined_pt(imax,jmax)
      character*1  stormcheck
      integer      isiret,imax,jmax,ist,npts,ilonfix,jlatfix,igvtret
      integer      ibeg,iend,jbeg,jend,ivt,i,j,iix,jix,bskip,igiret

      isiret = 0
      stormcheck = 'N'

      dell = (dx+dy)/2.

c     First define the radius of influence, which depends on the
c     grid spacing of the model data being used.  The ceiling statement
c     for npts in the first if statement is needed in case the
c     resolution of the grib files eventually goes very low, down to
c     say a half degree or less, in order to cover enough points in
c     the search.

      if (dell < 1.24) then      ! GFS, MRF, NAM, NGM, NAVGEM, GDAS,
                                 ! GFDL, NCEP Ensemble & Ensemble
                                 ! Relocation, SREF Ensemble
        ri     = ritrk_most
        if (cparm == 'slp') then
          radinf = 300.0
        else
          radinf = 225.0
        endif
        npts   = ceiling(radinf/(dtk*(dx+dy)/2.))
      else if (dell >= 1.24 .and. dell < 2.49) then     ! UKMET
        ri     = ritrk_most     
        radinf = 275.0
        npts   = 2
      else                       ! ECMWF
        ri     = ritrk_coarse
        radinf = 350.0
        npts   = 1
      endif

      pthresh = trkrinfo%mslpthresh    ! These are read in in 
      vthresh = trkrinfo%v850thresh    ! subroutine  read_nlists....

      call get_ij_bounds (npts,0,ri,imax,jmax,dx,dy
     &           ,glatmax,glatmin,glonmax,glonmin,parmlon,parmlat
     &           ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' After get_ij B, ibeg jbeg = ',ibeg,jbeg
        print *,' After get_ij B, iend jend = ',iend,jend
      endif

      if (igiret /= 0) then

        if ( verb .ge. 1 ) then
          print*,' '
          print*,'!!! ERROR in is_it_a_storm from call to'
          print*,'!!! get_ij_bounds, stopping processing for '
          print*,'!!! storm number ',ist
        endif

        isiret = 92
        return
      endif

c     If the input cparm is slp, then check to see that the MSLP 
c     gradient in any direction from the MSLP center is at least 
c     1mb / 200km, or 0.005mb/km.  This is based on discussions with 
c     Morris & Bob, who have had good results using a 2mb/200km 
c     requirement.  Since their model has a much finer resolution than
c     all of the models we run the  tracker on AND a much better 
c     depiction of the hurricane vortex, we do not use a requirement
c     as strict as theirs, and so make the requirement only half as
c     strong as theirs.
c
c     If the input cparm is v850, then check to see that there is
c     a circulation at 850 mb.  We will do this by calculating the
c     tangential wind of all points within a specified radius of 
c     the 850 minimum wind center, and seeing if there is a net
c     average tangential wind speed of at least 5 m/s.
c
c     UPDATE APRIL 2000: I've relaxed the thresholds slightly from
c     0.005 mb/km to 0.003 mb/km, and the wind threshold from 
c     5 m/s to 3 m/s.  Also, note that a special case for GDAS has
c     been hardwired in that is weaker (0.002 mb/km and 2 m/s).
c     That weaker GDAS requirement is for Qingfu's relocation stuff.
c
c     UPDATE JULY 2001: The relaxed requirement put in place in
c     April 2000 for the GDAS relocation has also been put in place
c     for the GFS ensemble relocation.

      ! We will want to speed things up for finer resolution grids.  
      ! We can do this by skipping some of the points in the loop.

      if ((dx+dy)/2. > 0.20) then
        bskip = 1
      else if ((dx+dy)/2. > 0.10 .and. (dx+dy)/2. <= 0.20) then
        bskip = 2
      else if ((dx+dy)/2. > 0.05 .and. (dx+dy)/2. <= 0.10) then
        bskip = 3
      else if ((dx+dy)/2. > 0.03 .and. (dx+dy)/2. <= 0.05) then
        bskip = 5
      else if ((dx+dy)/2. <= 0.03) then
        bskip = 10
      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'In is_it_a_storm, ilonfix= ',ilonfix
     &       ,' jlatfix= ',jlatfix
        print *,'ibeg jbeg iend jend = ',ibeg,jbeg,iend,jend
        print *,'cparm= ',cparm,'  parmlon parmlat = ',parmlon,parmlat
        print *,'parmval= ',parmval
        print *,' '
      endif

      vtavg = 0.0
      ivt   = 0

      xmaxpgrad = -999.0

      jloop: do jix = jbeg,jend,bskip
        iloop: do iix = ibeg,iend,bskip

          i = iix
          j = jix

          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              i = iix + imax
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i < 1 in subroutine  is_it_a_storm'
                print *,'!!! for a non-global grid.  STOPPING....'
                print *,'!!! i= ',i
                print *,' '
              endif

              stop 97    
            endif    
          endif  

          if (i > imax) then
            if (trkrinfo%gridtype == 'global') then
              i = iix - imax
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i > imax in subroutine '
                print *,'!!! is_it_a_storm for a non-global grid.'
                print *,'!!! STOPPING....'
                print *,'!!! i= ',i,' imax= ',imax
                print *,' '
              endif

              stop 97    
            endif    
          endif
  
          call calcdist(parmlon,parmlat,glon(i),glat(j),dist,degrees)

          if (dist > radinf .or. dist == 0.0) cycle

          if (defined_pt(i,j)) then

            if (cparm == 'slp') then
              pgradient = (slp(i,j) - parmval) / dist
              if (pgradient > xmaxpgrad) xmaxpgrad = pgradient

              if ( verb .ge. 3 ) then
                write (6,93) i,j,glon(i),glat(j),dist,slp(i,j),pgradient
              endif

              if (pgradient > pthresh) then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'In is_it_a_storm, valid pgradient found.'
                  print '(a23,f8.5)',' pgradient threshold = ',pthresh
                  print '(a23,f8.5)',' pgradient found     = ',pgradient
                  print *,'mslp center = ',parmlon,parmlat,parmval
                  print *,'pgrad loc   = ',glon(i),glat(j),slp(i,j)
                endif

                stormcheck = 'Y'
                exit jloop
              endif
            endif

            if (cparm == 'v850') then
              call getvrvt (parmlon,parmlat,glon(i),glat(j)
     &             ,u(i,j,nlev850),v(i,j,nlev850),vr,vt,igvtret)
              if ( verb .ge. 3 ) then
                write (6,91) i,j,glon(i),glat(j),u(i,j,nlev850)
     &               ,v(i,j,nlev850),vr,vt
              endif

              vtavg = vtavg + vt
              ivt   = ivt + 1
            endif

          endif
              
        enddo iloop
      enddo jloop

  91  format (1x,'i= ',i4,' j= ',i4,' glon= ',f7.2,' glat= ',f6.2
     &       ,' u= ',f8.4,' v= ',f8.4,' vr= ',f9.5,' vt= ',f9.5)

  93  format (1x,'i= ',i4,' j= ',i4,' glon= ',f7.2,' glat= ',f6.2
     &       ,' dist= ',f8.2,' slp= ',f10.2,' pgradient= ',f8.5)

      if (stormcheck /= 'Y' .and. cparm == 'slp') then

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'!!! In is_it_a_storm, valid pgradient NOT FOUND.'
          write (6,94) '!!! (Max pgradient less than ',pthresh,' mb/km)'
 94       format (1x,a29,5x,f8.5,a7)
          write (6,95) '!!! Max pgradient (mb/km) found = ',xmaxpgrad
 95       format (1x,a34,f8.5)
          print *,' '
        endif

      endif

      if (cparm == 'v850') then

        if (ivt > 0) then
          vtavg = vtavg / float(ivt)
        else
          vtavg = 0.0
        endif

        if (parmlat > 0) then
          if (vtavg >= vthresh) then
            stormcheck = 'Y'

            if ( verb .ge. 3 ) then
              print *,' '
              print *,' In is_it_a_storm, average 850 tangential'
     &          ,' winds are OKAY (>= +',vthresh,' m/s for a NH storm).'
              print *,' Avg 850 tangential winds = ',vtavg,' m/s'
              print *,' '
            endif

          else

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'!!! In is_it_a_storm, average 850 tangential'
              print *,'!!! winds did NOT exceed +',vthresh
     &             ,' m/s (NH storm).'
              print *,'!!! Avg 850 tangential winds = ',vtavg,' m/s'
              print *,' '
            endif

          endif
        else
          if (vtavg <= -vthresh) then
            stormcheck = 'Y'

            if ( verb .ge. 3 ) then
              print *,' '
              print *,' In is_it_a_storm, average 850 tangential'
     &          ,' winds are OKAY (<= -',vthresh,' m/s for a SH storm).'
              print *,' Avg 850 tangential winds = ',vtavg,' m/s'
              print *,' '
            endif

          else

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'!!! In is_it_a_storm, average 850 tangential'
              print *,'!!! winds did NOT exceed -',vthresh
     &             ,' m/s (SH storm).'
              print *,'!!! Avg 850 tangential winds = ',vtavg,' m/s'
              print *,' '
            endif

          endif
        endif

      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_phase (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &                    ,fixlon,fixlat,valid_pt,maxstorm
     &                    ,cps_vals,wcore_flag,igpret)
c
c     ABSTRACT: This subroutine is a driver subroutine for
c     determining the structure or phase of a cyclone.  Initially, we
c     will just have it use the Hart cyclone phase space (CPS) scheme.

      USE inparms; USE phase; USE set_max_parms; USE tracked_parms
      USE def_vitals; USE trkrparms; USE grid_bounds
      USE verbose_output

      implicit none

      type (datecard) inp
      type (trackstuff) trkrinfo

      character wcore_flag*1,okay_to_call_cps_routines*1
      real     fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real     cps_vals(3)
      real     dx,dy,paramb,vtl_slope,vtu_slope
      integer  imax,jmax,igpret,igcpret,ist,ifh,maxstorm
      integer  igvpret,igcv1ret,igcv2ret
      logical(1) valid_pt(imax,jmax)
c         

      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,611)
        write (6,613)
        write (6,615)
        write (6,*) ' '
        
 611    format(1x,'#-----------------------------------------------#')
 613    format(1x,'# start of routine to determine cyclone phase...#')
 615    format(1x,'#-----------------------------------------------#')
      endif

      if (phasescheme == 'cps' .or. phasescheme == 'both') then

        if (ifh > 1 .or. (ifh == 1 .and. trkrinfo%type == 'tracker'))
     &  then

          ! This condition that ifh > 1 is so that we *not* do the cps
          ! stuff for fhour=0 if it's a tcgen or midlat case, since we
          ! don't know the model storm motion direction for the
          ! analysis.  For a regular case where type = 'tracker', we
          ! have the observed storm's heading direction from tc vitals,
          ! so we can use that (even though the model's storm direction
          ! may differ slightly from the observed storm).  This current
          ! if statement and the ones below carefully check for these
          ! various instances.

          okay_to_call_cps_routines = 'n'

          if (ifh > 1) then
            if (fixlon(ist,ifh-1) > -990.0 .and.
     &          fixlat(ist,ifh-1) > -990.0) then
              okay_to_call_cps_routines = 'y'
            else
              if (verb >= 3) then
                print *,' '
                print *,' ><  CPS diagnostics were requested but will'
                print *,' ><  NOT be performed for this time level '
                print *,' ><  since the fixlon and fixlat at the '
                print *,' ><  previous lead time are undefined.'
                print *,' ><  This is likely the first found position'
                print *,' ><  for a genesis (tcgen or midlat) case.'
                print *,' ><  ifh= ',ifh
              endif
            endif
          elseif (ifh == 1 .and. trkrinfo%type == 'tracker') then
            okay_to_call_cps_routines = 'y'
          else
            if (verb >= 3) then
              print *,' '
              print *,' ><  CPS diagnostics were requested but will'
              print *,' ><  NOT be performed for this time level.'
              print *,' ><  The likely reason is that ifh=0 and'
              print *,' ><  this is a genesis case, so we do not '
              print *,' ><  know the storm motion direction.'
              print *,' ><  for a genesis (tcgen or midlat) case.'
              print *,' ><  ifh= ',ifh
              print *,' ><  trkrinfo%type ',trkrinfo%type
            endif
          endif

          if (okay_to_call_cps_routines == 'y') then

            ! Similarly, these next two conditions (previous lat and
            ! previous lon > -999) are in there in case we're doing a
            ! tcgen or midlat case and this is the *first* time level
            ! within a forecast that the storm has been detected (again,
            ! we don't yet know the storm heading).

            call get_cps_paramb (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &                  ,fixlon,fixlat,valid_pt,paramb,maxstorm,igcpret)

            call get_cps_vth (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &                       ,fixlon,fixlat,valid_pt,'lower',vtl_slope
     &                       ,maxstorm,igcv1ret)

            call get_cps_vth (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &                       ,fixlon,fixlat,valid_pt,'upper',vtu_slope
     &                       ,maxstorm,igcv2ret)

            if ( verb .ge. 3 ) then
              write (6,*) ' '
              write (6,73) storm(ist)%tcv_storm_id,ifhours(ifh)
     &             ,ifclockmins(ifh)
     &             ,paramb,vtl_slope,vtu_slope
            endif

            cps_vals(1) = paramb
            cps_vals(2) = vtl_slope
            cps_vals(3) = vtu_slope

          else

            if ( verb .ge. 3 ) then
              print *,' '
              print *,' ><  CPS diagnostics were requested but will NOT'
              print *,' ><  be performed for this time level since we '
              print *,' ><  are either at the first time level for a '
              print *,' ><  genesis case (type = midlat or tcgen), or' 
              print *,' ><  we are at any time level in which for some' 
              print *,' ><  reason the fixlon and fixlat at the'
              print *,' ><  previous time level are not defined.'
              print *,' ><  ifh= ',ifh
            endif

          endif

        else

          if ( verb .ge. 3 ) then
            print *,' '
            print *,' ><  CPS diags were requested but will NOT be'
            print *,' ><  performed for this time level since we are at'
            print *,' ><  time level 1 for a genesis case '
            print *,' ><  (type = midlat or tcgen) and we cannot'
            print *,' ><  diagnose the model direction of storm'
            print *,' ><  movement.  ifh= ',ifh
          endif

        endif

      endif

  73  format ('cps_stats: ',a4,'  lead time= ',i3,':',i2,'   paramb= '
     &       ,f8.2,'  vtl= ',f9.2,'  vtu= ',f9.2)


      if (phasescheme == 'vtt' .or. phasescheme == 'both') then
        call get_vtt_phase (inp,imax,jmax,dx,dy,ist,ifh,trkrinfo
     &        ,fixlon,fixlat,valid_pt,maxstorm,wcore_flag,igvpret)
      endif


      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,631)
        write (6,633)
        write (6,635)
        write (6,*) ' '
        
 631    format(1x,'#-------------------------------------------------#')
 633    format(1x,'# End of routine to determine cyclone phase...    #')
 635    format(1x,'#-------------------------------------------------#')
      endif

c         
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_cps_paramb (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &                 ,fixlon,fixlat,valid_pt,paramb,maxstorm,igcpret)
c
c     ABSTRACT: This subroutine is part of the algorithm for determining
c     the structure, or phase, of a cyclone.  For Hart's cyclone phase
c     space, this subroutine determines "Parameter B", which determines
c     the degree of thermal symmetry between the "left" and "right" 
c     hemispheres of a storm, in the layer between 900 and 600 mb.
c     We evaluate only those points that are within 500 km of the 
c     storm center.

      USE inparms; USE phase; USE set_max_parms; USE trig_vals 
      USE grid_bounds; USE tracked_parms; USE def_vitals; USE trkrparms
      USE verbose_output

      implicit none

      type (datecard) inp
      type (trackstuff) trkrinfo

      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      zthicksum(2)
      real      rlonc,rlatc,rlonb,rlatb,xdist,degrees,d,cosarg
      real      st_heading,st_heading_rad,ricps,dx,dy
      real      pt_dir,pt_dir_rad,zthick,hemval,paramb
      real      zthick_right_mean,zthick_left_mean
      integer   imax,jmax,igpret,igcpret,ist,ifh,npts,bskip,i,j
      integer   ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret
      integer   left_ct,right_ct,hemis,icount,maxstorm,ip
      logical(1) valid_pt(imax,jmax)
c
      ricps = 500.0

c     -----------------------------------------------------------------
c     First, determine the angle that the storm took getting from the
c     last position to the current one.  If this is for ifh=1 for a
c     regular type=tracker case, we will just use the storm direction
c     as read from the tcvitals card.
c     -----------------------------------------------------------------

      if (ifh == 1) then

        st_heading = float(storm(ist)%tcv_stdir)

      else

        call calcdist(fixlon(ist,ifh),fixlat(ist,ifh)
     &             ,fixlon(ist,ifh-1),fixlat(ist,ifh-1),xdist,degrees)

        rlonc = (360.-fixlon(ist,ifh)) * dtr
        rlatc = fixlat(ist,ifh) * dtr
        rlonb = (360.-fixlon(ist,ifh-1)) * dtr
        rlatb = fixlat(ist,ifh-1) * dtr
        d     = degrees * dtr

        if (d == 0.0) then
  
          ! Storm is stationary...
          st_heading = 0.0

        else
  
          cosarg = (sin(rlatc)-sin(rlatb)*cos(d))/(sin(d)*cos(rlatb))
          if (cosarg > 1.0)  cosarg = 1
          if (cosarg < -1.0) cosarg = -1
  
          if (sin(rlonc-rlonb) < 0.0) then
            st_heading_rad = acos(cosarg)
          else
            st_heading_rad = 2*pi - acos(cosarg)
          endif

          st_heading = st_heading_rad / dtr

        endif

      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' In get_cps_paramb, lead time= ',ifhours(ifh),':'
     &         ,ifclockmins(ifh)
     &         ,'  ',storm(ist)%tcv_storm_id,' '
     &              ,storm(ist)%tcv_storm_name
        print '(a43,f9.3)'
     &       ,'  In get_cps_paramb, model storm heading = '
     &       ,st_heading
        print *,' '
      endif

c     -----------------------------------------------------------------
c     Now call  get_ij_bounds to get the boundaries for a smaller 
c     subdomain, or subset of gridpoints, in which to evaluate the 
c     parameter B statistic.  We will only include points within 
c     500 km of the storm center for evaluation.
c     -----------------------------------------------------------------

      npts = ceiling(ricps/(dtk*(dx+dy)/2.))

      call get_ij_bounds (npts,0,ricps,imax,jmax,dx,dy
     & ,glatmax,glatmin,glonmax,glonmin,fixlon(ist,ifh),fixlat(ist,ifh)
     & ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_cps_paramb from call to'
          print *,'!!! get_ij_bounds, stopping processing for'
          print *,'!!! storm number ',ist
        endif
        igcpret = 92
        return
      endif

c     -----------------------------------------------------------------
c     Now loop through all of the points of the subdomain.  If the 
c     point is further than 500 km from the storm center, discard it.
c     Otherwise, evaluate the angle from the storm center to this point
c     to determine the hemisphere of the point, that is, if the point 
c     is to the left or the right of the storm track.
c     -----------------------------------------------------------------

      ! We will want to speed things up for finer resolution grids.  
      ! We can do this by skipping some of the points in the  
      ! loop for the evaluation of parameter B.

      if ((dx+dy)/2. > 0.20) then
        bskip = 1
      else if ((dx+dy)/2. > 0.10 .and. (dx+dy)/2. <= 0.20) then
        bskip = 2
      else if ((dx+dy)/2. > 0.05 .and. (dx+dy)/2. <= 0.10) then
        bskip = 3
      else if ((dx+dy)/2. > 0.03 .and. (dx+dy)/2. <= 0.05) then
        bskip = 5
      else if ((dx+dy)/2. <= 0.03) then
        bskip = 10
      endif

      left_ct = 0
      right_ct = 0
      zthicksum = 0
      icount = 0

c      print *,'CPS CORE: ibeg= ',ibeg,' iend= ',iend
c      print *,'CPS CORE: jbeg= ',jbeg,' jend= ',jend

      jloop: do j=jbeg,jend,bskip
        iloop: do i=ibeg,iend,bskip

          icount = icount + 1

c          print *,'CPS CORE: ist= ',ist,' ifh= ',ifh,' j= ',j,' i= ',i

          if (i > imax) then
            if (trkrinfo%gridtype == 'global') then
              ip = i - imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: In get_cps_paramb, the '
                print *,'!!!    user-requested eastern search boundary'
                print *,'!!!    is beyond the eastern bounds of '
                print *,'!!!    this regional grid.  '
                print *,'!!!    Parameter B will not be computed.'
                print *,'!!!    Subroutine location A....'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   User-requested eastern i = ',i
                print *,' '
              endif

              paramb  = -9999.99
              igcpret = 95
              return
            endif    
          else   
            ip = i
          endif   

          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              ip = i + imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i < 1 in subroutine  get_cps_paramb'
                print *,'!!! for a non-global grid.'
                print *,'!!! Parameter B will not be computed.'
                print *,'!!! i= ',i
                print *,' '
              endif

              paramb  = -9999.99
              igcpret = 95
              return
            endif
          endif

          call calcdist (fixlon(ist,ifh),fixlat(ist,ifh),glon(ip)
     &                  ,glat(j),xdist,degrees)

          if (xdist > ricps) cycle iloop

          if (valid_pt(ip,j)) then
            continue
          else

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'!!! UNDEFINED PT OUTSIDE OF GRID '
              print *,'!!! IN GET_CPS_PARAMB....'
              print *,'!!! i= ',i,' ip= ',ip,' j= ',j
              print *,'!!! fixlon= ',fixlon(ist,ifh),' fixlat= '
     &             ,fixlat(ist,ifh)
              print *,'!!! glon= ',glon(ip),' glat= ',glat(j)
              print *,'!!! Parameter B will not be computed.'
              print *,'!!! EXITING GET_CPS_PARAMB....'
              print *,' '
            endif

            paramb  = -9999.99
            igcpret = 95
            return
          endif   

          !----------------------------------------------------------
          ! Calculate angle from storm center to point, in a 0-360
          ! framework, clockwise positive.
          !----------------------------------------------------------

          rlonc = (360.-glon(ip)) * dtr
          rlatc = glat(j) * dtr
          rlonb = (360.-fixlon(ist,ifh)) * dtr
          rlatb = fixlat(ist,ifh) * dtr
          d     = degrees * dtr

          if (d > 0.) then
            cosarg = (sin(rlatc)-sin(rlatb)*cos(d))/(sin(d)*cos(rlatb))
            if (cosarg > 1.0)  cosarg = 1
            if (cosarg < -1.0) cosarg = -1
  
            if (sin(rlonc-rlonb) < 0.0) then
              pt_dir_rad = acos(cosarg)
            else
              pt_dir_rad = 2*pi - acos(cosarg)
            endif
          else
            pt_dir_rad = 0.0
          endif

          pt_dir = pt_dir_rad / dtr

          !------------------------------------------------------------
          ! Based on the angle that the point is from the storm center,
          ! determine if the point is to the left or the right of the
          ! storm track.
          !------------------------------------------------------------

          if (st_heading >= 180.0) then
            if ((st_heading - pt_dir) > 0.0 .and. 
     &          (st_heading - pt_dir) <= 180) then
              hemis = 2
              left_ct = left_ct + 1
            else
              hemis = 1
              right_ct = right_ct + 1
            endif
          else
            if ((pt_dir - st_heading) > 0.0 .and. 
     &          (pt_dir - st_heading) <= 180) then
              hemis = 1
              right_ct = right_ct + 1
            else
              hemis = 2
              left_ct = left_ct + 1
            endif
          endif

          !------------------------------------------------------------
          ! Calculate the 600-900 mb thickness at this point and add 
          ! the  thickness value to the array for the correct "storm
          ! hemisphere".
          !------------------------------------------------------------

          zthick = cpshgt(ip,j,7) - cpshgt(ip,j,1)
          zthicksum(hemis) = zthicksum(hemis) + zthick

          if ( verb .ge. 3 ) then
            write (6,51) rlonb/dtr,rlatb/dtr,rlonc/dtr,rlatc/dtr
     &           ,st_heading,pt_dir,hemis,zthick
          endif
               
        enddo iloop
      enddo jloop

 51   format (1x,'stlon stlat = ',2(f6.2,2x),'  ptlon ptlat = '
     &       ,2(f6.2,2x),'  sthead= ',f6.2,'  ptdir= ',f6.2,'  hemis= '
     &       ,i1,'  zthick= ',f7.2)

c     ------------------------------------------------------------------
c     Now calculate parameter B.  The hemval parameter = +1 for storms
c     in the Northern Hemisphere and -1 for Southern Hemisphere storms.
c     ------------------------------------------------------------------

      zthick_right_mean = zthicksum(1) / float(right_ct)
      zthick_left_mean  = zthicksum(2) / float(left_ct)

      if (fixlat(ist,ifh) < 0.0) then
        hemval = -1.0
      else
        hemval =  1.0
      endif

      paramb = hemval * (zthick_right_mean - zthick_left_mean)

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' In get_cps_paramb, lead time= ',ifhours(ifh),':'
     &       ,ifclockmins(ifh)
     &       ,'  ',storm(ist)%tcv_storm_id,' ',storm(ist)%tcv_storm_name
        print *,'  right_ct= ',right_ct,'  left_ct= ',left_ct
        print *,'  zthicksum(1)= ',zthicksum(1)
        print *,'  zthicksum(2)= ',zthicksum(2)
        print *,'  zthick_right_mean= ',zthick_right_mean
        print *,'  zthick_left_mean=  ',zthick_left_mean
        print *,'  hemval= ',hemval
        print *,'  END of get_cps_paramb, paramb= ',paramb
      endif

c         
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_cps_vth (imax,jmax,inp,dx,dy,ist,ifh,trkrinfo
     &        ,fixlon,fixlat,valid_pt,clayer,vth_slope,maxstorm,igcvret)
c
c     ABSTRACT: This subroutine is part of the algorithm for determining
c     the structure, or phase, of a cyclone.  For Hart's cyclone phase
c     space, this subroutine determines the thermal wind profile for 
c     either the lower troposphere (i.e., between 600 and 900 mb) or the
c     upper troposphere (i.e., between 300 and 600 mb).  We evaluate 
c     only those points that are within 500 km of the storm center.

      USE inparms; USE phase; USE set_max_parms; USE trig_vals
      USE grid_bounds; USE tracked_parms; USE def_vitals; USE trkrparms
      USE verbose_output

      implicit none

      type (datecard) inp
      type (trackstuff) trkrinfo

      character clayer*5
      real      tmp1,tmp2,tmp3
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      zmax(7),zmin(7),zdiff(7),xlolevs(7),xhilevs(7),plev(7)
      real      dlnp(7),dzdlnp(7),dz(7),lnp(7)
      real      vth_slope,xdist,degrees,d,cosarg
      real      ricps,dx,dy,R2
      integer   imax,jmax,igpret,igcpret,ist,ifh,npts,bskip,i,j,k,kix
      integer   ilonfix,jlatfix,ibeg,jbeg,iend,jend,igcvret,igiret
      integer   kbeg,kend,maxstorm,ip
      logical(1) valid_pt(imax,jmax)

      data xlolevs /900.,850.,800.,750.,700.,650.,600./
      data xhilevs /600.,550.,500.,450.,400.,350.,300./
c      data xlolevs /90000.,85000.,80000.,75000.,70000.,65000.,60000./
c      data xhilevs /60000.,55000.,50000.,45000.,40000.,35000.,30000./
c
      ricps = 500.0
      plev = 0.0

      if (clayer == 'lower') then
        kbeg = 1
        kend = 7
        plev = xlolevs
      else
        kbeg = 7
        kend = 13
        plev = xhilevs
      endif

c     -----------------------------------------------------------------
c     First, call  get_ij_bounds to get the boundaries for a smaller
c     subdomain, or subset of gridpoints, in which to evaluate the
c     parameter B statistic.  We will only include points within
c     500 km of the storm center for evaluation.
c     -----------------------------------------------------------------

      npts = ceiling(ricps/(dtk*(dx+dy)/2.))

      call get_ij_bounds (npts,0,ricps,imax,jmax,dx,dy
     & ,glatmax,glatmin,glonmax,glonmin,fixlon(ist,ifh),fixlat(ist,ifh)
     & ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_cps_vtl from call to'
          print *,'!!! get_ij_bounds, stopping processing for'
          print *,'!!! storm number ',ist
        endif

        igcvret = 92
        return
      endif

c     ------------------------------------------------------------------
c     Now loop through all of the points of the subdomain at each level.
c     If a point is further than 500 km from the storm center, discard 
c     it. Otherwise, evaluate the gp height at the point to determine 
c     if it is a max or a min for the given level.  Store the max and
c     min height at each level in an array.
c     ------------------------------------------------------------------
        
c      ! We will want to speed things up for finer resolution grids.
c      ! We can do this by skipping some of the points in the
c      ! loop for the evaluation of parameter B.
c        
c      if ((dx+dy)/2. > 0.20) then
c        bskip = 1
c      else if ((dx+dy)/2. > 0.10 .and. (dx+dy)/2. <= 0.20) then
c        bskip = 2
c      else if ((dx+dy)/2. <= 0.10) then
c        bskip = 3
c      endif

      bskip = 1    ! Don't do any skipping for now....

      zmax  = -9999999.0
      zmin  = 9999999.0
      zdiff = 0.0
      lnp   = 0.0

      levloop: do k = kbeg,kend

        if (kbeg == 7) then   
          ! processing upper layers (600-300 mb)
          kix = k - 6
        else
          ! processing lower layers (900-600 mb)
          kix = k
        endif

        lnp(kix) = log(plev(kix))
        
        jloop: do j=jbeg,jend,bskip
          iloop: do i=ibeg,iend,bskip

            if (i > imax) then
              if (trkrinfo%gridtype == 'global') then
                ip = i - imax   ! If wrapping past GM
              else

                if ( verb .ge. 1 ) then
                  print *,' '
                  print *,'!!! ERROR: In get_cps_vth, the '
                  print *,'!!!  user-requested eastern search boundary'
                  print *,'!!!  is beyond the eastern bounds of '
                  print *,'!!!  this regional grid.  '
                  print *,'!!!  Thermal wind parm will not be computed.'
                  print *,'!!!  Subroutine location A....'
                  print *,'!!!         '
                  print *,'!!!   imax of regional grid    = ',imax
                  print *,'!!!   User-requested eastern i = ',i
                  print *,' '
                endif

                vth_slope = -9999.99
                igcvret   = 95
                return
              endif    
            else   
              ip = i
            endif   

            if (i < 1) then
              if (trkrinfo%gridtype == 'global') then
                ip = i + imax
              else

                if ( verb .ge. 1 ) then
                  print *,' '
                  print *,'!!! ERROR: i < 1 in subroutine  get_cps_vth'
                  print *,'!!! for a non-global grid.'
                  print *,'!!! Thermal wind parm will not be computed.'
                  print *,'!!! i= ',i
                  print *,' '
                endif

                vth_slope = -9999.99
                igcvret   = 95
                return
              endif
            endif
        
            call calcdist (fixlon(ist,ifh),fixlat(ist,ifh),glon(ip)
     &                    ,glat(j),xdist,degrees)
        
            if (xdist > ricps) cycle iloop
        
            if (valid_pt(ip,j)) then
              continue
            else

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'!!! UNDEFINED PT OUTSIDE OF GRID '
                print *,'!!! IN GET_CPS_VTH....'
                print *,'!!! i= ',i,' ip= ',ip,' j= ',j,' k= ',k
     &               ,' clayer= ',clayer
                print *,'!!! fixlon= ',fixlon(ist,ifh),' fixlat= '
     &               ,fixlat(ist,ifh)
                print *,'!!! glon(ip)= ',glon(ip),' glat= ',glat(j)
                print *,'!!! Thermal wind parm will not be computed.'
                print *,'!!! EXITING GET_CPS_VTH....'
                print *,' '
              endif

              vth_slope = -9999.99
              igcvret   = 95
              return
            endif

            tmp1 = zmax(kix)
            tmp2 = cpshgt(ip,j,k)
            tmp3 = zmin(kix)

            zmax(kix) = max(tmp1,tmp2)
            zmin(kix) = min(tmp3,tmp2)

c            zmax(kix) = max(zmax(kix),cpshgt(ip,j,k))
c            zmin(kix) = min(zmin(kix),cpshgt(ip,j,k))

          enddo iloop
        enddo jloop

        zdiff(kix) = zmax(kix) - zmin(kix)

      enddo levloop

c     ------------------------------------------------------------------
c     Now calculate the vertical derivative of the gp height, that is,
c     d(dz)/d(ln(p)).  Here, zdiff is the gp height perturbation at a 
c     given level, calculated in the loop above; dz is the vertical 
c     change in that perturbation from one level to the next.
c     ------------------------------------------------------------------
      
      dz = 0.0
      dlnp = 0.0
      dzdlnp = 0.0

      do k = 2,7
        dz(k) = zdiff(k) - zdiff(k-1)
        dlnp(k) = log(plev(k)) - log(plev(k-1))
        dzdlnp(k) = dz(k) / dlnp(k)
      enddo

c     ------------------------------------------------------------------
c     Now call a correlation routine to get the slope of a regression 
c     line.  The independent variable that we input is dlnp, the change
c     in log of pressure with height.  The dependent variable is 
c     dzdlnp, the vertical change in the height perturbation with 
c     respect to the change in pressure.  The slope that is returned 
c     defines whether we've got a cold core or warm core system.
c     See Hart (MWR, April 2003, Vol 131, pp. 585-616) for more 
c     details, specifically his Fig. 3 and the discussion surrounding.
c     Note that in the call to calccorr, we are sending only 6 of the 
c     7 elements of the dlnp and dzdlnp arrays, beginning with the 
c     2nd element of each.  That's because the first array value for
c     each of those arrays is empty, since in the loop just above, we
c     start with kbeg+1, not kbeg.
c     ------------------------------------------------------------------

      call calccorr(lnp(2),zdiff(2),6,R2,vth_slope)

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'++ In get_cps_vth, values for vth follow for '
     &       ,'lead time= ',ifhours(ifh),':',ifclockmins(ifh),'  '
     &       ,storm(ist)%tcv_storm_id,' ',storm(ist)%tcv_storm_name
        print *,'  ... clayer = ',clayer 
        print *,' '
      endif

      do k = kbeg,kend

        if (kbeg == 7) then
          kix = k - 6
        else
          kix = k
        endif

        if ( verb .ge. 3 ) then
          print *,' '
          write (6,31) k,plev(kix),zmax(kix),zmin(kix),zdiff(kix)
          if (kix > 1) then
            write (6,32) plev(kix),log(plev(kix))
     &           ,plev(kix-1),log(plev(kix-1))
            write (6,33) dz(kix),dlnp(kix),dzdlnp(kix)
          else
            write (6,34)
          endif
        endif

      enddo

  31  format (1x,'  +++ k= ',i2,' press= ',f8.1,' zmax= ',f7.2
     &       ,' zmin= ',f7.2,' zdiff= ',f7.2)
  32  format (1x,'      ln(',f7.1,')= ',f9.6,'  ln(',f7.1,')= ',f9.6)
  33  format (1x,'      dz= ',f10.2,' dlnp= ',f13.6,'  dzdlnp= ',f12.3)
  34  format (1x,'      --- First level... no derivatives done...')
c
      return
      end
c
C----------------------------------------------------
C
C----------------------------------------------------
      subroutine calccorr(xdat,ydat,numpts,R2,slope)
c
c     This subroutine is the main driver for a series of
c     other subroutines below this that will calculate the
c     correlation between two input arrays, xdat and ydat.
c
c     INPUT:
c      xdat     array of x (independent) data points
c      ydat     array of y (dependent)   data points
c      numpts   number of elements in each of xdat and ydat
c
c     OUTPUT:
c      R2    R-squared, the coefficient of determination
c      slope Slope of regression line
c
c     xdiff   array of points for xdat - xmean
c     ydiff   array of points for ydat - ymean
c     yestim  array of regression-estimated points
c     yresid  array of residuals (ydat(i) - yestim(i))

      USE verbose_output

      implicit none

      real    xdat(numpts),ydat(numpts)
      real    xdiff(numpts),ydiff(numpts)
      real    yestim(numpts),yresid(numpts)
      real    xmean,ymean,slope,yint,R2
      integer numpts,i

c
      call getmean(xdat,numpts,xmean)
      call getmean(ydat,numpts,ymean)
c
      call getdiff(xdat,numpts,xmean,xdiff)
      call getdiff(ydat,numpts,ymean,ydiff)
c
      call getslope(xdiff,ydiff,numpts,slope)
      yint = ymean - slope * xmean
c
      call getyestim(xdat,slope,yint,numpts,yestim)
      call getresid(ydat,yestim,numpts,yresid)
c

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' *--------------------------------------------------* '
        print *,' * CPS Thermal wind regression details              * '
        print *,' *--------------------------------------------------* '
      endif

      call getcorr(yresid,ydiff,numpts,R2)

      if ( verb .ge. 3 ) then
        print *,'   i     ydat     xdat    ydiff    xdiff        e'
     &       ,'       e2   ydiff2'
        print *,' ----   -----    -----    -----    -----    -----   '
     &       ,' -----    -----'
        do i = 1,numpts
          write(6,'(2x,i3,2x,f7.2,2x,f7.4,2x,f7.2,2x,f7.4,3(2x,f7.2))')
     &         i,ydat(i),xdat(i),ydiff(i)
     &         ,xdiff(i),yresid(i),yresid(i)*yresid(i)
     &         ,ydiff(i)*ydiff(i)
        enddo
        
        print *,' ----   -----    -----    -----    -----    -----   '
     &       ,' -----    -----'
        print *,' '
        write (6,'(1x,a13,f9.3,3x,a5,f7.2)') ' means:   y: ',ymean
     &       ,'  x: ',xmean
        
        write (6,*) ' '
        write (6,30) 'slope= ',slope,'         y-intercept = ',yint
 30     format (2x,a7,f10.3,a23,f10.3)
        if (slope .gt. 0.0) then
          write(6,40) 'Regression equation:   Y = ',yint,' + ',slope
        else
          write(6,40) 'Regression equation:   Y = ',yint,' - '
     &              ,abs(slope)
        endif
 40     format (2x,a27,f8.2,a3,f8.2,'X')
c         
        print *,' '
        write (6,'(1x,a17,f7.4,5x,a7,f7.4)') ' R2(r_squared) = ',R2
     &       ,'   r = ',sqrt(R2)
        print *,' '
        print *,' *--------------------------------------------------* '
        print *,' *  End of regression details                       * '
        print *,' *--------------------------------------------------* '
      endif

      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getmean(xarr,inum,zmean)
c     
c     This subroutine is part of the correlation calculation,
c     and it simply returns the mean of the input array, xarr.
c     
c     INPUT:
c      xarr   input array of data points
c      inum   number of data points in xarr
c     
c     OUTPUT:
c      zmean  mean of data values in xarr

      implicit none
      
      real   xarr(inum)
      real   xsum,zmean
      integer i,inum
c     
      xsum = 0.0
      do i = 1,inum
        xsum = xsum + xarr(i)
      enddo
c     
      zmean = xsum / float(MAX(inum,1))
c     
      return
      end
      
c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getdiff(xarr,inum,zmean,zdiff)
c     
c     This subroutine is part of the correlation calculation,
c     and it returns in the array zdiff the difference values
c     between each member of the input array xarr and the
c     mean value, zmean.
c     
c     INPUT:
c      xarr   input array of data points
c      inum   number of data points in xarr
c      zmean  mean of input array (xarr)
c     
c     OUTPUT:
c      zdiff  array containing xarr(i) - zmean

      implicit none
      
      real xarr(inum),zdiff(inum)
      real zmean
      integer i,inum
c     
      do i = 1,inum
        zdiff(i) = xarr(i) - zmean
      enddo
c     
      return
      end
      
c-------------------------------------------c
c                                           c
c-------------------------------------------c

      subroutine getslope(xarr,yarr,inum,slope)
c
c     This subroutine is part of the correlation calculation,
c     and it returns the slope of the regression line.
c
c     INPUT:
c      xarr   input array of xdiffs (x - xmean)
c      yarr   input array of ydiffs (y - ymean)
c      inum   number of points in x & y arrays
c
c     OUTPUT:
c      slope  slope of regression line

      real xarr(inum),yarr(inum)
      real slope,sumxy,sumx2
      integer i,inum

c     First sum up the xarr*yarr products....

      sumxy = 0.0
      do i = 1,inum
        sumxy = sumxy + xarr(i) * yarr(i)
      enddo

c     Now sum up the x-squared terms....

      sumx2 = 0.0
      do i = 1,inum
        sumx2 = sumx2 + xarr(i) * xarr(i)
      enddo

c     Now get the slope....

      slope = sumxy / sumx2

      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getyestim(xarr,slope,yint,inum,yestim)
c
c     This subroutine is part of the correlation calculation,
c     and it calculates all the predicted y-values using the
c     regression equation that has been calculated.
c
c     INPUT:
c      xarr   array of x data points
c      slope  slope of the calculated regression line
c      yint   y-intercept of the calculated regression line
c      inum   number of input points
c
c     OUTPUT:
c      yestim array of y pts estimated from regression eqn.

      implicit none

      real xarr(inum),yestim(inum)
      real slope,yint
      integer i,inum
c
      do i = 1,inum
        yestim(i) = yint + xarr(i) * slope
      enddo
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getresid(yarr,yestim,inum,yresid)
c
c     This subroutine is part of the correlation calculation,
c     and it calculates all the residual values between the
c     input y data points and the y-estim predicted y values.
c
c     INPUT:
c      yarr   array of y data points
c      yestim array of y pts estimated from regression eqn.
c      inum   number of input points
c
c     OUTPUT:
c      yresid array of residuals (ydat(i) - yestim(i))

      implicit none

      real yarr(inum),yestim(inum),yresid(inum)
      integer i,inum
c
      do i = 1,inum
        yresid(i) = yarr(i) - yestim(i)
      enddo
c
      return
      end

c-------------------------------------------c
c                                           c
c-------------------------------------------c
      subroutine getcorr(yresid,ydiff,inum,R2)
c
c     This subroutine is part of the correlation calculation,
c     and it does the actual correlation calculation.
c
c     INPUT:
c      yresid array of residuals (ydat(i) - yestim(i))
c      ydiff  array of points for ydat - ymean
c      inum   number of points in the arrays
c
c     OUTPUT:
c      R2     R-squared, the coefficient of determination

      USE verbose_output

      implicit none

      real yresid(inum),ydiff(inum)
      real R2,sumyresid,sumydiff
      integer i,inum
c
      sumyresid = 0.0
      sumydiff  = 0.0

      do i = 1,inum
        sumyresid = sumyresid + yresid(i) * yresid(i)
        sumydiff  = sumydiff  + ydiff(i) * ydiff(i)
      enddo

      if ( verb .ge. 3 ) then
        write (6,*)  ' '
        write (6,30) 'Sum of y-residuals squared (e2) = ',sumyresid
        write (6,30) 'Sum of y-diffs squared (ydiff2) = ',sumydiff
        write (6,*)  ' '
 30     format (1x,a35,f15.2)
      endif

      if (sumyresid .lt. sumydiff) then
        if (sumydiff .le. 0.000001) then
          R2 = 1.0
        else
          R2 = 1 - sumyresid / sumydiff
        endif 
      else
        R2=0.0
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_vtt_phase (inp,imax,jmax,dx,dy,ist,ifh,trkrinfo
     &          ,fixlon,fixlat,valid_pt,maxstorm,wcore_flag,igvpret)
c
c     ABSTRACT: This subroutine is part of the algorithm for determining
c     the structure, or phase, of a cyclone.  Here, we are only looking
c     at the mid-to-upper tropospheric warm anomaly at the center of
c     the storm.  The temperature data that we are searching through in
c     the tmean array should be the 300-500 mb mean temperature data.
c     The criteria in this algorithm are based loosely on Vitart's
c     criteria for warm core checking, but the nuts & bolts of the
c     subroutine use algorithms from this tracker, including the  barnes
c     analysis.  First, we locate the warm core with the  find_maxmin
c     routine.  Then we use the  check_closed_contour routine to see if
c     there is a closed temperature contour surrounding the warm core.
c
c     INPUT:
c     inp
c     imax     Num pts in i direction on input grid
c     jmax     Num pts in j direction on input grid
c     inp      contains input date and model number information
c     dx       Grid spacing in i-direction on input grid
c     dy       Grid spacing in j-direction on input grid
c     ist      integer storm number (internal to the  tracker)
c     ifh      integer index for lead time
c     trkrinfo derived type containing grid info on user boundaries
c     fixlon   array containing found fix longitudes
c     fixlat   array containing found fix latitudes
c     valid_pt Logical; bitmap indicating if valid data at that pt.
c     maxstorm maximum # of storms to be handled
c
c     OUTPUT:
c     wcore_flag 'u'=undetermined, 'y'=yes, 'n'=no
c     igvpret  Return code for this subroutine.
c
c     LOCAL:
c     wcore_mean_val barnes-averaged value of the temperature at the
c              location where the  tracker found the warm core.
c     wcore_point_max max temperature found at a gridpoint near the
c              location where the  tracker found the warm core using
c              barnes analysis.

      USE set_max_parms; USE grid_bounds; USE trkrparms; USE contours
      USE tracked_parms; USE gen_vitals; USE def_vitals; USE inparms
      USE phase
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo,wcore_trkrinfo
      type (cint_stuff) wcore_contour_info
      type (datecard) inp

      character*1 get_last_contour_flag,wcore_flag
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      dx,dy,wcore_mean_val,wcore_mean_lon,wcore_mean_lat
      real      wcore_point_max,tlastcont,rlastcont,tlastout,rlastout
      integer   imax,jmax,igvpret,ist,ifh,npts,bskip,i,j
      integer   ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret
      integer   icount,maxstorm,ip,ifmret,ifilret,ifix,jfix,icccret
      integer   num_check_conts
      logical(1) valid_pt(imax,jmax),compflag,wcore_mask(imax,jmax)
      logical(1) output_file_open
c

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'*-------------------------------------------------*'
        print *,'* At top of get_vtt_phase                         *'
        write (6,102) ifhours(ifh),ifclockmins(ifh)
 102    format (1x,'* Searching for warm core at hour ',i4,':',i2.2)
        write (6,103) wcore_depth
 103    format (1x,'* Warm core depth threshold (wcore_depth) = ',f7.2)
        print *,'*-------------------------------------------------*'
      endif

c     ------------------------------------------------------------
      wcore_mask = .false.
      wcore_mean_lon = -999.0
      wcore_mean_lat = -999.0
      wcore_trkrinfo = trkrinfo ! set equal to values from trkrinfo...
      wcore_trkrinfo%contint = wcore_depth  ! ...except use the warm
                                ! core contour interval specified by
                                ! the user in the extrkr.sh script.

c     ------------------------------------------------------------
c     First, call  find_maxmin to locate the warm core

      call find_maxmin (imax,jmax,dx,dy,'tmp'
     &      ,tmean,'max',ist,fixlon(ist,ifh),fixlat(ist,ifh)
     &      ,glon,glat,valid_pt,trkrinfo,compflag
     &      ,wcore_mean_lon,wcore_mean_lat,wcore_mean_val
     &      ,glatmax,glatmin,glonmax,glonmin,inp%modtyp,ifmret)

      if (verb .ge. 3) then
        print *,' '
        print *,'After call to find_maxmin for wcore, ifmret= ',ifmret
        print *,'      wcore_mean_val= ',wcore_mean_val
      endif

c     ------------------------------------------------------------
c     Once  find_maxmin returns a value and a location for the
c     barnes-averaged value of a warm core, then make a call to
c     fix_latlon_to_ij to (1) get the actual gridpoint value of the
c     temperature (the value stored in wcore_mean_val is an
c     area-averaged value coming from the  barnes analysis), and
c     (2) to get the (i,j) indeces for this gridpoint to be used in
c     the call to check_closed_contour below.

      if (wcore_mean_lat > -99.0 .and. wcore_mean_lon > -990.0) then
        call fix_latlon_to_ij (imax,jmax,dx,dy,tmean,'max'
     &     ,valid_pt,wcore_mean_lon,wcore_mean_lat
     &     ,wcore_mean_val,ifix,jfix,wcore_point_max,'tracker'
     &     ,glatmax,glatmin,glonmax,glonmin
     &     ,trkrinfo,ifilret)
        if (ifilret == 0) then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'+++ Warm core stats: '
            write (6,105) storm(ist)%tcv_storm_id
     &           ,gstorm(ist)%gv_gen_date
     &           ,gstorm(ist)%gv_gen_fhr,gstorm(ist)%gv_gen_lat
     &           ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &           ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
     &           ,ifhours(ifh),ifclockmins(ifh)
     &           ,wcore_mean_lon,360.-wcore_mean_lon
     &           ,wcore_mean_lat,wcore_mean_val
            write (6,106) storm(ist)%tcv_storm_id
     &           ,gstorm(ist)%gv_gen_date
     &           ,gstorm(ist)%gv_gen_fhr,gstorm(ist)%gv_gen_lat
     &           ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &           ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
     &           ,ifhours(ifh),ifclockmins(ifh)
     &           ,ifix,jfix,wcore_point_max
          endif

        else
          ! Search went out of regional grid bounds....
          
          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR IN get_vtt_phase.  The call to  '
            print *,'!!! fix_latlon_to_ij returned a non-zero return '
            print *,'!!! code, which means that the search for the fix'
            print *,'!!! i and j went out of bounds for a regional '
            print *,'!!! grid. This should have been caught in a '
            print *,'!!! previous call to find_maxmin for one of the '
            print *,'!!! various fix parms. In any event, we will not'
            print *,'!!! search for a warm core for this storm and '
            print *,'!!! lead time.'
            print *,' '
            write (6,115) storm(ist)%tcv_storm_id
     &           ,gstorm(ist)%gv_gen_date
     &           ,gstorm(ist)%gv_gen_fhr,gstorm(ist)%gv_gen_lat
     &           ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &           ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
     &           ,ifhours(ifh),ifclockmins(ifh)
     &           ,'U',-999.99,-9999.99
          endif

          igvpret = 95
          wcore_flag = 'u'
          return
        endif
      endif

  105 format (1x,'    wcore: ',a4,1x,i10.10,'_F',i3.3,'_',i3.3,a1
     &       ,'_',i4.4,a1,'_',a3,2x,i4,':',i2.2,'  mean_lon: ',f7.2,'E'
     &       ,1x,'(',f7.2,'W)',2x,'mean_lat: ',f7.2,2x
     &       ,'wcore_mean_val(K): ',f12.3)
  106 format (1x,'    wcore: ',a4,1x,i10.10,'_F',i3.3,'_',i3.3,a1
     &       ,'_',i4.4,a1,'_',a3,2x,i4,':',i2.2,'  ifix: ',i5,2x
     &       ,' jfix: ',i5,2x,'wcore_point_max(K): ',f12.3)


c     ------------------------------------------------------------
c     The Vitart scheme specifies that the temperature must decrease
c     by at least 1.0C in all directions from the warm core center
c     within a distance of 8 deg.  A rigorous check of this criterion
c     is performed here by utilizing the  check_closed_contour routine.
c     If we have a closed contour in the temperature field
c     surrounding the warm core (using a 1 deg K interval), that
c     criterion is satisfied.  For diagnostic purposes, we set the
c     value of num_check_conts to 999 in order to keep searching for
c     all contours surrounding the warm core, and this allows us to
c     get an idea of the "depth" or magnitude of the warm core when
c     the tlastcont and rlastcont values are returned.

      wcore_contour_info%numcont = maxconts
      num_check_conts = 999

      get_last_contour_flag = 'y'
      call check_closed_contour (imax,jmax,ifix,jfix,tmean
     &    ,valid_pt,wcore_mask,wcore_flag,'max',wcore_trkrinfo
     &    ,num_check_conts,wcore_contour_info,get_last_contour_flag
     &    ,tlastcont,rlastcont,icccret)

      if (wcore_flag == 'y') then
        tlastout = tlastcont
        rlastout = rlastcont/0.539638
      else
        tlastout = -999.0
        rlastout = -9999.0
      endif

      if ( verb .ge. 3 ) then
        write (6,115) storm(ist)%tcv_storm_id,gstorm(ist)%gv_gen_date
     &       ,gstorm(ist)%gv_gen_fhr,gstorm(ist)%gv_gen_lat
     &       ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &       ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
     &       ,ifhours(ifh),ifclockmins(ifh)
     &       ,wcore_flag,tlastout,rlastout
        
 115    format (1x,'    wcore: ',a4,1x,i10.10,'_F',i3.3,'_',i3.3,a1
     &       ,'_',i4.4,a1,'_',a3,2x,i4,':',i2.2
     &       ,'  wcore_flag= ',a1,2x,' Temp of last contour(K) = '
     &       ,f7.2,2x,'Radius of last contour(km) = ',f8.2)
      endif

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_sfc_center (xmeanlon,xmeanlat,clon
     &                  ,clat,ist,ifh,calcparm,xsfclon,xsfclat
     &                  ,maxstorm,igscret)
c
c     ABSTRACT: This subroutine computes a modified lat/lon fix position
c     to use as the input center position for the subroutines that
c     follow which calculate surface-wind related values.  The reason
c     for this is that since we are concerned with the positioning of
c     low-level wind features (e.g., rmax), we want the center position
c     to be based solely on low-level features. We'll use mslp and the
c     min in the sfc wind speed.  If a center fix was unable to be made
c     at this forecast hour for mslp and low-level winds, then we will
c     stick with just using the mean position we got using all the other
c     parameters.
c
c     INPUT:
c     xmeanlon The mean center longitude computed from all the various
c              parameter fixes found in array clon
c     xmeanlat The mean center latitude computed from all the various
c              parameter fixes found in array clat
c     clon     Center longitudes of tracked parms for this storm & ifh
c     clat     Center latitudes of tracked parms for this storm & ifh
c     ist      Index for storm number
c     ifh      Index for forecast hour
c     calcparm Logical; Use this parm's location for this storm or not
c              (if a parameter fix could not be made at this forecast
c              hour, then calcparm is set to false for this time for
c              that parameter).
c     maxstorm Maximum number of storms that can be tracked
c
c     OUTPUT:
c     xsfclon  low-level longitude estimate for this storm & time,
c              computed ideally from mean of mslp & low-level winds.
c     xsfclat  low-level latitude estimate for this storm & time,
c              computed ideally from mean of mslp & low-level winds.
c     igscret  Return code from this subroutine

      USE set_max_parms
      USE verbose_output

      implicit none

      integer  ist,ifh,ipct,igscret,maxstorm
      real     clon(maxstorm,maxtime,maxtp)
      real     clat(maxstorm,maxtime,maxtp)
      real     xmeanlon,xmeanlat
      real     xsfclon,xsfclat,xlonsum,xlatsum
      logical(1) calcparm(maxtp,maxstorm)

      ipct = 0
      xlonsum = 0.0
      xlatsum = 0.0

      ! Do NOT include MSLP for the surface center at this time.
c      if (calcparm(9,ist)) then
c        ipct = ipct + 1
c        xlonsum = xlonsum + clon(ist,ifh,9)
c        xlatsum = xlatsum + clat(ist,ifh,9)
c      endif

      if (calcparm(10,ist)) then
c        ! NOTE: Put double weighting on surface wind center if
c        ! the  tracker was able to find a center for it....
c        ipct = ipct + 2
c        xlonsum = xlonsum + 2.*clon(ist,ifh,10)
c        xlatsum = xlatsum + 2.*clat(ist,ifh,10)
        ! Just use single weighting for the sfc wcirc fix
        ipct = ipct + 1
        xlonsum = xlonsum + clon(ist,ifh,10)
        xlatsum = xlatsum + clat(ist,ifh,10)
      endif

      if (calcparm(11,ist)) then
        ! This is for the sfc vorticity center....
        ipct = ipct + 1
        xlonsum = xlonsum + clon(ist,ifh,11)
        xlatsum = xlatsum + clat(ist,ifh,11)
      endif

      if (ipct > 0) then
        xsfclon = xlonsum / float(ipct)
        xsfclat = xlatsum / float(ipct)
      else

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'!!! In get_fract_wind_cov, CANNOT get modified fix '
          print *,'!!! position because the parameter fixes for mslp'
          print *,'!!! and the sfc winds could not be obtained at this'
          print *,'!!! forecast hour.   ist= ',ist,' ifh= ',ifh
          print *,'!!! We will use the  fixlon and fixlat values for'
          print *,'!!! this forecast hour.'
        endif

        xsfclon = xmeanlon
        xsfclat = xmeanlat
      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'+++ In get_sfc_center, modified fix (mslp + sfc_winds)'
        print *,'+++ position follows: '
        print *,'+++ '
        print *,'+++ mslp:            lon: ',clon(ist,ifh,9),'   lat: '
     &       ,clat(ist,ifh,9)
        print *,'+++ sfc_winds:       lon: ',clon(ist,ifh,10),'   lat: '
     &       ,clat(ist,ifh,10)
        print *,'+++ sfc_vorticity:   lon: ',clon(ist,ifh,11),'   lat: '
     &       ,clat(ist,ifh,11)
        print *,'+++ multi-parm mean: lon: ',xmeanlon,'   lat: '
     &       ,xmeanlat
        print *,'+++ sfc-only mean:   lon: ',xsfclon,'   lat: ',xsfclat
      endif

      return
      end
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_wind_structure (imax,jmax,inp,dx,dy
     &             ,ist,ifh,fixlon,fixlat,xsfclon,xsfclat,valid_pt
     &             ,er_wind,sr_wind,er_vr,sr_vr,er_vt,sr_vt,maxstorm
     &             ,trkrinfo,igwsret)
c       
c     ABSTRACT: This subroutine is a driver subroutine for
c     determining the structure of the low level winds of a cyclone.
c     The algorithm will search out at specified distances from the
c     storm center along arcs in each quadrant of the storm, 
c     evaluating the winds every 15 degrees along the arc.  In each
c     arc, start 7.5 degrees in, then make stops at 22.5, 37.5, 
c     52.5, 67.5, and 82.5 degrees.  At each of those points, we 
c     will bilinearly interpolate the winds to the points along those 
c     arcs.  Then we compute a quadrant average of the wing magnitude,
c     as well as the mean Vt and Vr values.  This will be done
c     twice -- First, for an earth-relative coordinate system, and
c     second, for a storm-relative coordinate system.  For the
c     earth-relative estimates, we will always have 4 earth-relative
c     quadrants: NE, SE, SW and NW.  For the storm-relative estimates,
c     these mean values of the wind will be computed for the same 
c     relative quadrants (front-right, back-right, back-left, front-
c     left, but with respect (positive clockwise) to the
c     direction of storm motion.  
c       
c     LOCAL:
c       numdist   Number of discrete radii at which the winds will
c                 be evaluated
c       rdist     The radii (km) at which winds will be evaluated
c     
c     Arrays:
c       rdist     Radii (km) at which the winds will be evaluated
c       er_wind:  Quadrant winds in earth-relative framework
c       sr_wind:  Quadrant winds in storm-relative framework
c       er_vr:    Quadrant radial winds in earth-relative framework
c       sr_vr:    Quadrant radial winds in storm-relative framework
c       er_vt:    Quadrant tangential winds in earth-relative framework
c       sr_vt:    Quadrant tangential winds in storm-relative framework

      USE inparms; USE phase; USE set_max_parms; USE tracked_parms
      USE def_vitals; USE trig_vals; USE trkrparms
      USE verbose_output

      implicit none

      type (datecard) inp
      type (trackstuff) trkrinfo

      integer, parameter :: numdist=14,numquad=4,num_qtr_azim=6
      integer  imax,jmax,igwsret,ist,ifh,iquad,idist,ibiret1,ibiret2
      integer  igvtret,ipct,maxstorm,iazim,azimuth_ct,bimct
      real     fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real     rdist(numdist)
      real     er_wind(numquad,numdist)
      real     sr_wind(numquad,numdist)
      real     er_vr(numquad,numdist)
      real     er_vt(numquad,numdist)
      real     sr_vr(numquad,numdist)
      real     sr_vt(numquad,numdist)
      real     dx,dy,bear,targlat,targlon,xintrp_u,xintrp_v,st_heading
      real     d,cosarg,rlonc,rlatc,rlonb,rlatb,st_heading_rad,degrees
      real     temp_bear,xdist,xsfclon,xsfclat,wmag,wmag_sum
      real     vr,vt,vr_sum,vt_sum
      logical(1) valid_pt(imax,jmax)
c
      data rdist/10.,25.,50.,75.,100.,125.,150.,200.,250.,300.,350.
     &          ,400.,450.,500./

      igwsret = 0

      er_wind = 0.0
      sr_wind = 0.0
      er_vr   = 0.0
      er_vt   = 0.0
      sr_vr   = 0.0
      sr_vt   = 0.0

c     -----------------------------------------------------------------
c     Now determine the angle that the storm took getting from the
c     last position to the current one.  If this is the initial time,
c     use the observed direction of motion from the TC Vitals.  This
c     may not match up with the model storm's initial direction of
c     motion, but it is all we have available to us in order to get
c     a heading estimate for the initial time.  This storm heading
c     information will be used for the storm-relative profiles.
c     -----------------------------------------------------------------

      if (ifh == 1) then

        st_heading = float(storm(ist)%tcv_stdir)

      else

        call calcdist(fixlon(ist,ifh),fixlat(ist,ifh)
     &               ,fixlon(ist,ifh-1),fixlat(ist,ifh-1),xdist,degrees)

        rlonc = (360.-fixlon(ist,ifh)) * dtr
        rlatc = fixlat(ist,ifh) * dtr
        rlonb = (360.-fixlon(ist,ifh-1)) * dtr
        rlatb = fixlat(ist,ifh-1) * dtr
        d     = degrees * dtr

        cosarg = (sin(rlatc)-sin(rlatb)*cos(d))/(sin(d)*cos(rlatb))
        if (cosarg > 1.0)  cosarg = 1
        if (cosarg < -1.0) cosarg = -1

        if (sin(rlonc-rlonb) < 0.0) then
          st_heading_rad = acos(cosarg)
        else
          st_heading_rad = 2*pi - acos(cosarg)
        endif

        st_heading = st_heading_rad / dtr

        if ( verb .ge. 3 ) then
          print *,' '
          print *,' In get_wind_structure, fhr= ',fhreal(ifh)
     &         ,'  ',storm(ist)%tcv_storm_id
     &         ,'  ',storm(ist)%tcv_storm_name
          print '(a25,a23,f9.3)','  In get_wind_structure, '
     &         ,' model storm heading = ',st_heading
          print *,' '
        endif

      endif

c     -----------------------------------------------------------------
c     Get the profiles for the earth-relative coordinate system.
c     Start with NE, then SE, SW, and NW.  First go through 
c     radiusloop, which goes from one radial distance to the next,
c     then do the quadloop, which goes through each quadrant, and 
c     then within each quadrant, the qtr_azimloop goes through for 
c     six points along an arc, spaced 15 degrees apart, starting at
c     7.5 degrees clockwise from the north.
c     -----------------------------------------------------------------

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' *****************************************************'
        print *,'   Wind Structure: distbear bilin interp starts here.'
        print *,' *****************************************************'
        print *,' '
      endif

      bimct = 0

      radiusloop1: do idist = 1,numdist

        if ( verb .ge. 3 ) then
          print *,'-- ER structure idist= ',idist,' rdist= '
     &           ,rdist(idist)
        endif

        quadloop1: do iquad = 1,4

          if ( verb .ge. 3 ) then
            print *,'     structure iquad= ',iquad
          endif

          wmag_sum = 0.0
          vr_sum   = 0.0
          vt_sum   = 0.0
          azimuth_ct = 0

          ! In each quadrant, run through six points along an
          ! arc and evaluate the winds.

          qtr_azimloop1: do iazim = 1,num_qtr_azim

            bear = ((iquad-1) * 90.) + ((iazim-1) * 15.) + 7.5

            if ( verb .ge. 3 ) then
              print *,'       structure iazim= ',iazim
     &               ,' earth-relative bear= ',bear
            endif

            call distbear (xsfclat,xsfclon,rdist(idist)
     &                    ,bear,targlat,targlon)

            if ( verb .ge. 3 ) then
              print *,' '
              print '(5(a10,f7.2))','  sfclat= ',xsfclat
     &             ,'  sfclon= ',xsfclon
     &             ,'   rdist= ',rdist(idist),' targlat= ',targlat
     &             ,' targlon= ',targlon
              print '(19x,a8,f7.2,35x,a9,f7.2)','sfclon= ',360.-xsfclon
     &             ,'targlon= ',360.-targlon
            endif

            ! NOTE: The 1020 in the call here is just a number/code to
            ! indicate to the subroutine to process sfc winds....

            call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,1020,'u',xintrp_u
     &            ,valid_pt,bimct,ibiret1)

            call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,1020,'v',xintrp_v
     &            ,valid_pt,bimct,ibiret2)

            if (ibiret1 == 0 .and. ibiret2 == 0) then
              wmag = sqrt (xintrp_u**2 + xintrp_v**2)
              wmag_sum = wmag_sum + wmag
              call getvrvt (xsfclon,xsfclat,targlon,targlat
     &                     ,xintrp_u,xintrp_v,vr
     &                     ,vt,igvtret)
              vr_sum = vr_sum + vr
              vt_sum = vt_sum + vt
              azimuth_ct = azimuth_ct + 1

              if ( verb .ge. 3 ) then
                print '(2x,a21,f8.2,a14,f8.2,2(a11,f8.2))'
     &               ,'   intrp wind speed= '
     &               ,wmag,'    (in kts)= ',wmag*1.9427
     &               ,'  vr(m/s)= ',vr,'  vt(m/s)= ',vt
              endif

            endif

          enddo qtr_azimloop1

          if (azimuth_ct > 0) then
            ! Compute quadrant-azimuthally-averaged winds at 
            ! this distance
            er_wind(iquad,idist) = wmag_sum / float(azimuth_ct)
            er_vr(iquad,idist) =   vr_sum / float(azimuth_ct)
            er_vt(iquad,idist) =   vt_sum / float(azimuth_ct)
          else
            er_wind(iquad,idist) = -999.0
            er_vr(iquad,idist) = -999.0
            er_vt(iquad,idist) = -999.0
          endif

        enddo quadloop1

      enddo radiusloop1

      if (bimct > 0) then
        if (verb .ge. 3) then
          print *,' '
          print *,'Warning summary: From sub get_wind_structure,'
          print *,'calls to sub bilin_int_uneven resulted in'
          print *,'(blocked) attempts to access points outside the'
          print *,'bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in radiusloop1= ',bimct
          print *,' '
        endif
      endif

c     -----------------------------------------------------------------
c     Get the profiles for the storm-relative coordinate system.
c     Start with the front-right quadrant and go clockwise through
c     back-right, back-left and front-left.
c     -----------------------------------------------------------------

      bimct = 0

      radiusloop2: do idist = 1,numdist

        if ( verb .ge. 3 ) then
          print *,'-- SR structure idist= ',idist,' rdist= '
     &           ,rdist(idist)
        endif

        quadloop2: do iquad = 1,4

          if ( verb .ge. 3 ) then
            print *,'     structure iquad= ',iquad
          endif

          wmag_sum = 0.0
          vr_sum   = 0.0
          vt_sum   = 0.0
          azimuth_ct = 0

          qtr_azimloop2: do iazim = 1,num_qtr_azim

c            temp_bear = st_heading + ((iquad-1) * 90.) + 45.

            temp_bear = st_heading + ((iquad-1) * 90.)
     &                + ((iazim-1) * 15.) + 7.5
            bear      = mod(temp_bear,360.)

            if ( verb .ge. 3 ) then
              print *,'       structure iazim= ',iazim
     &               ,' storm-relative bear= ',bear
            endif

            call distbear (xsfclat,xsfclon,rdist(idist)
     &                    ,bear,targlat,targlon)

            ! NOTE: The 1020 in the call here is just a number/code to
            ! indicate to the subroutine to process sfc winds....

            call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,1020,'u',xintrp_u
     &            ,valid_pt,bimct,ibiret1)

            call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,1020,'v',xintrp_v
     &            ,valid_pt,bimct,ibiret2)

            if (ibiret1 == 0 .and. ibiret2 == 0) then
              wmag = sqrt (xintrp_u**2 + xintrp_v**2)
              wmag_sum = wmag_sum + wmag
              call getvrvt (xsfclon,xsfclat,targlon,targlat
     &                     ,xintrp_u,xintrp_v,vr
     &                     ,vt,igvtret)
              vr_sum = vr_sum + vr
              vt_sum = vt_sum + vt
              azimuth_ct = azimuth_ct + 1

              if ( verb .ge. 3 ) then
                print '(2x,a21,f8.2,a14,f8.2,2(a11,f8.2))'
     &               ,'   intrp wind speed= '
     &               ,wmag,'    (in kts)= ',wmag*1.9427
     &               ,'  vr(m/s)= ',vr,'  vt(m/s)= ',vt
              endif

            endif

          enddo qtr_azimloop2

          if (azimuth_ct > 0) then
            ! Compute quadrant-azimuthally-averaged winds at
            ! this distance
            sr_wind(iquad,idist) = wmag_sum / float(azimuth_ct)
            sr_vr(iquad,idist) =   vr_sum / float(azimuth_ct)
            sr_vt(iquad,idist) =   vt_sum / float(azimuth_ct)
          else
            sr_wind(iquad,idist) = -999.0
            sr_vr(iquad,idist) = -999.0
            sr_vt(iquad,idist) = -999.0
          endif

        enddo quadloop2

      enddo radiusloop2

      if (bimct > 0) then
        if (verb .ge. 3) then
          print *,' ' 
          print *,'Warning summary: From sub get_wind_structure,'
          print *,'calls to sub bilin_int_uneven resulted in'
          print *,'(blocked) attempts to access points outside the'
          print *,'bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in radiusloop2= ',bimct
          print *,' ' 
        endif 
      endif
c
      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_fract_wind_cov (imax,jmax,inp,dx,dy
     &             ,ist,ifh,fixlon,fixlat,xsfclon,xsfclat,valid_pt
     &             ,calcparm,wfract_cov,pdf_ct_bin,pdf_ct_tot,maxstorm
     &             ,trkrinfo,igfwret)
c
c     ABSTRACT: This subroutine determines the fractional areal coverage
c     of winds exceeding various thresholds within specified arcs
c     (e.g., 200 km, 400 km, etc) in each quadrant of a storm.  The bins
c     that are used go as follows: (1) 0-100; (2) 0-200; (3) 0-300;
c     (4) 0-400; (5) 0-500.
c
c     LOCAL:
c       numdist   Number of discrete radii at which the winds will
c                 be evaluated
c       rdist     The radii (km) at which winds will be evaluated
c
c     Arrays:
c       rdist     Radii (km) at which the winds will be evaluated

      USE inparms; USE phase; USE set_max_parms; USE tracked_parms
      USE def_vitals; USE trig_vals; USE grid_bounds; USE level_parms
      USE trkrparms
      USE verbose_output

      implicit none

      type (datecard) inp
      type (trackstuff) trkrinfo

      integer, parameter :: numdist=14,numquad=4,numbin=5,numthresh=3
      real     fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real     rdist(numdist)
      real     wfract_cov(numquad+1,numbin,numthresh)
      real     area_total_quad_bin(numquad,numbin)
      real     area_exceed_quad_bin(numquad,numbin,numthresh)
      real     xintlon,xintlat
      real ::  windthresh(numthresh) = (/17.5,25.74,32.94/)
      real     dx,dy,bear,targlat,targlon,xintrp_u,xintrp_v,st_heading
      real     d,cosarg,rlonc,rlatc,rlonb,rlatb,st_heading_rad,degrees
      real     temp_bear,xdist,conv_ms_knots,vmagkts
      real     rads,ri,dell,vmag,xarea,grdintincr,xsfclon,xsfclat
      real     sum_exceed_area(numbin,numthresh)
      real     sum_total_area(numbin,numthresh)
      integer  pdf_ct_bin(16)
      integer  imax,jmax,igwsret,ist,ifh,iquad,idist,ibiret1,ibiret2
      integer  igfwret,ipct,i,j,numinterp,ixoa,ixaa,iq,ib,it,ii
      integer  jlatfix,ilonfix,npts,ibeg,iend,jbeg,jend,ngridint,ni,nj
      integer  itret,igiret,idistbin,ipdfbin,pdf_ct_tot,maxstorm
      logical(1) calcparm(maxtp,maxstorm)
      logical(1) valid_pt(imax,jmax)
      character  got_pdf*6
      character*2 :: cquad(4) = (/'NE','SE','SW','NW'/)
      character*5 :: cbin(5) = 
     &               (/'0-100','0-200','0-300','0-400','0-500'/)
      character*2 :: cthresh(3) = (/'34','50','64'/)
c
      igfwret = 0
      conv_ms_knots = 1.9427
      rads = 500.0
      ri   = 300.0
      dell = (dx+dy)/2.
      npts = rads/(dtk*dell)

      wfract_cov            = 0.0
      area_total_quad_bin   = 0.0
      area_exceed_quad_bin  = 0.0
      sum_exceed_area       = 0.0
      sum_total_area        = 0.0

c     Call  get_ij_bounds in order to get the dimensions for a smaller
c     subdomain of grid points to search over.

      call get_ij_bounds (npts,0,ri,imax,jmax,dx,dy
     &     ,glatmax,glatmin,glonmax,glonmin,xsfclon,xsfclat
     &     ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_fract_wind_cov from call to '
          print *,'!!! get_ij_bounds, stopping processing for storm'
          print *,'!!! number ',ist
        endif

        igfwret = 92
        return
      endif

      if (ibeg < 1) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_fract_wind_cov, the ibeg returned'
            print *,'!!! from get_ij_bounds is < 1, but our gridtype is'
            print *,'!!! global, so we are going to leave it as is and '
            print *,'!!! account for the grid wrapping as we go.'
            print *,' '
          endif

        else

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_fract_wind_cov, the ibeg returned'
            print *,'!!! from get_ij_bounds is < 1, and our gridtype is'
            print *,'!!! NOT global, so we are going to abort the '
            print *,'!!! fractional wind coverage processing for'
            print *,'!!! this time.'
            print *,' '
          endif

          igfwret = 94
          return
        endif
      endif

      if (ibeg > imax .or. jbeg > jmax .or. jbeg < 1 .or.
     &    iend < 1 .or. jend < 1) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'ERROR in get_fract_wind_cov calculating ibeg, iend,'
          print *,'jbeg or jend.  ibeg= ',ibeg,' iend= ',iend
          print *,'               jbeg= ',jbeg,' jend= ',jend
          print *,'               imax= ',imax,' jmax= ',jmax
          print *,'fractional wind coverage processing will not be '
          print *,'performed for this time.'
        endif

        igfwret = 94
        return
      endif

      if (iend > imax) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_fract_wind_cov, the iend returned'
            print *,'!!! from get_ij_bounds is > imax, but our gridtype'
            print *,'!!! is global, so we are going to leave it as is '
            print *,'!!! and account for the grid wrapping.'
            print *,' '
          endif

        else

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_fract_wind_cov, the iend returned'
            print *,'!!! from get_ij_bounds is > imax, and our gridtype'
            print *,'!!! is NOT global, so we will abort the '
            print *,'!!! fractional wind coverage processing for'
            print *,'!!! this time.'
            print *,' '
          endif

          igfwret = 94
          return
        endif
      endif


c     When evaluating the winds at a gridpoint, keep in mind that each
c     gridpoint represents area around it.  There are 2 special cases
c     we need to watch out for.  The first is for cases in which the
c     area of a gridpoint straddles across a distance threshold, so
c     that some of the gridpoint's area is in the "<200" bin, while
c     some is in the "<100" bin.  The other is for the case in which
c     the area of a gridpoint straddles between 2 adjacent quadrants
c     (e.g., a gridpoint exactly to the north of the center would have
c     half its area in the NW quadrant and half in the NE quadrant).
c
c     To properly "partition" and assign gridpoint areas, we need to
c     interpolate the current grid down to a fine resolution.
c
c     This next if statement determines how many times to interpolate
c     the input grid to a smaller grid.  Here are the guidelines that
c     will be used, keeping in mind that we want the final grid spacing
c     to be on the order of between 0.05 and 0.10 degree (finer than
c     0.05 deg is superfluous, and coarser than 0.10 deg is too coarse).
c
c      Original grid size (deg)     # of interps
c     -------------------------    ------------
c           0.8 <= g                    4
c         0.4 <= g < 0.8                3
c         0.2 <= g < 0.4                2
c         0.1 <= g < 0.2                1
c                g < 0.1                0


      if ((dx+dy)/2. >= 0.8) then
        numinterp = 4
      else if ((dx+dy)/2. < 0.8 .and. (dx+dy)/2. >= 0.4) then
        numinterp = 3
      else if ((dx+dy)/2. < 0.4 .and. (dx+dy)/2. >= 0.2) then
        numinterp = 2
      else if ((dx+dy)/2. < 0.2 .and. (dx+dy)/2. >= 0.1) then
        numinterp = 1
      else
        numinterp = 0
      endif

      grdintincr = (dx+dy)/2.
      do i = 1,numinterp
        grdintincr = 0.5 * grdintincr
      enddo

c     Now loop through the points in this subdomain, determine if any
c     are within 500 km of the center, and then determine what quadrant
c     the point is in relative to the center, and then calculate the
c     fractional area coverage for winds.

      pdf_ct_tot = 0
      pdf_ct_bin = 0

      jloop: do j = jbeg,jend
        iloop: do i = ibeg,iend

          if (i > imax) then
            if (trkrinfo%gridtype == 'global') then
              ii = i - imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: In get_fract_wind_cov, the '
                print *,'!!!    user-requested eastern search boundary'
                print *,'!!!    is beyond the eastern bounds of '
                print *,'!!!    this regional grid.  We will not '
                print *,'!!!    perform the fractional wind coverage' 
                print *,'!!!    processing for this storm & time.'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   User-requested eastern i = ',i
                print *,' '
              endif

              igfwret = 94
              return
            endif    
          else   
            ii = i
          endif   

          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              ii = i + imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i < 1 in get_fract_wind_cov' 
                print *,'!!! for a non-global grid.  We will not '
                print *,'!!! perform the fractional wind coverage'
                print *,'!!! processing for this storm & time.'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   User-requested eastern i = ',i
                print *,' '
              endif

              igfwret = 94
              return
            endif    
          endif

          if (.not. valid_pt(ii,j)) then
            cycle iloop   ! Only an issue for regional grids
          endif

          call calcdist (glon(ii),glat(j),xsfclon,xsfclat,xdist,degrees)

          if (xdist > (rads+(0.75*((dx+dy)/2.)*dtk*cos(glat(j)*dtr)))) 
     &    then

            ! If the distance is greater than "rads" (500 km at initial
            ! writing) plus another 3/4 of a gridpoint, then cycle.
            ! The extra 3/4 of a gridpoint is to allow for the case of
            ! some portion of the area around a gridpoint (whose
            ! center point > 500 km) being within the 500 km arc...
            ! although that is only factored in for grids with spacing
            ! >= 0.1 deg.  For smaller grids, where no interpolation is
            ! done in this subroutine, then the distance to that point
            ! is considered representative and the point is ignored if
            ! it is not less than 500 km from the center.

            cycle iloop

          else

            ! First interpolate the area surrounding each grid point to
            ! get fine resolution of lats & lons for determining how to
            ! partition the area of a gridpoint among quadrants as well
            ! as among distance thresholds.

            vmag = sqrt (u(ii,j,levsfc)**2 + v(ii,j,levsfc)**2)
            vmagkts = vmag * conv_ms_knots

            if (numinterp > 0) then

              grdintincr = ((dx+dy)/2.) / 2**numinterp  ! "grid spacing"
                                                  ! of interpolated grid
              ngridint = (2**numinterp) / 2

              got_pdf = 'notyet'

              njloop: do nj= ngridint,-ngridint,-1

                xintlat = glat(j) + float(nj) * grdintincr

                niloop: do ni= -ngridint,ngridint

                  xintlon = glon(ii) + float(ni) * grdintincr

                  call calcdist (xintlon,xintlat,xsfclon
     &                          ,xsfclat,xdist,degrees)

                  if (xdist <= 350. .and. got_pdf == 'notyet') then
                    ! The got_pdf flag is needed because in these loops
                    ! for niloop & njloop, we are actually looking at
                    ! tiny areas around the same grid point.  So we
                    ! want to make sure we only count each gridpoint
                    ! once.
                    ipdfbin = min((int(vmagkts / 10.) + 1),16)
                    pdf_ct_bin(ipdfbin) = pdf_ct_bin(ipdfbin) + 1
                    pdf_ct_tot = pdf_ct_tot + 1
                    got_pdf = 'got_it'
                  endif

                  if (xdist < 500.) then

                    ! Compute area of this fraction of a grid box
                    xarea = (grdintincr * 111195) *
     &                      (grdintincr * 111195
     &                                  * cos(xintlat * dtr))

                    idistbin = int(xdist / 100.) + 1

                    ! Go through a loop of the bins.  The purpose of
                    ! this is that these "bins" all go from the
                    ! the center out to a specified radius, they are
                    ! NOT 100-km wide bins.  So if we are dealing with
                    ! a point at r = 250 km, then that falls in the
                    ! 0-300 km bin, but it also falls in the 0-400 and
                    ! 0-500 km bins as well.  So we need to run through
                    ! this binloop multiple times to get the area data
                    ! into multiple bins.  Here are the bins & indices:
                    !   1: 0-100 km
                    !   2: 0-200 km
                    !   3: 0-300 km
                    !   4: 0-400 km
                    !   5: 0-500 km

                    binloop: do ib = idistbin,numbin

                      if (xintlon >= xsfclon .and.
     &                    xintlat >= xsfclat) then

                        ! NE quadrant

                        area_total_quad_bin(1,ib) =
     &                            area_total_quad_bin(1,ib) + xarea
                        if (vmag > windthresh(1)) then
                          area_exceed_quad_bin(1,ib,1) =
     &                          area_exceed_quad_bin(1,ib,1) + xarea
                        endif
                        if (vmag > windthresh(2)) then
                          area_exceed_quad_bin(1,ib,2) =
     &                          area_exceed_quad_bin(1,ib,2) + xarea
                        endif
                        if (vmag > windthresh(3)) then
                          area_exceed_quad_bin(1,ib,3) =
     &                          area_exceed_quad_bin(1,ib,3) + xarea
                        endif

                      else if (xintlon >= xsfclon .and.
     &                         xintlat < xsfclat) then

                        ! SE quadrant

                        area_total_quad_bin(2,ib) =
     &                             area_total_quad_bin(2,ib) + xarea
                        if (vmag > windthresh(1)) then
                          area_exceed_quad_bin(2,ib,1) =
     &                          area_exceed_quad_bin(2,ib,1) + xarea
                        endif
                        if (vmag > windthresh(2)) then
                          area_exceed_quad_bin(2,ib,2) =
     &                          area_exceed_quad_bin(2,ib,2) + xarea
                        endif
                        if (vmag > windthresh(3)) then
                          area_exceed_quad_bin(2,ib,3) =
     &                          area_exceed_quad_bin(2,ib,3) + xarea
                        endif

                      else if (xintlon < xsfclon .and.
     &                         xintlat < xsfclat) then

                        ! SW quadrant

                        area_total_quad_bin(3,ib) =
     &                             area_total_quad_bin(3,ib) + xarea
                        if (vmag > windthresh(1)) then
                          area_exceed_quad_bin(3,ib,1) =
     &                          area_exceed_quad_bin(3,ib,1) + xarea
                        endif
                        if (vmag > windthresh(2)) then
                          area_exceed_quad_bin(3,ib,2) =
     &                          area_exceed_quad_bin(3,ib,2) + xarea
                        endif
                        if (vmag > windthresh(3)) then
                          area_exceed_quad_bin(3,ib,3) =
     &                          area_exceed_quad_bin(3,ib,3) + xarea
                        endif

                      else if (xintlon < xsfclon .and.
     &                         xintlat >= xsfclat) then

                        ! NW quadrant

                        area_total_quad_bin(4,ib) =
     &                             area_total_quad_bin(4,ib) + xarea
                        if (vmag > windthresh(1)) then
                          area_exceed_quad_bin(4,ib,1) =
     &                          area_exceed_quad_bin(4,ib,1) + xarea
                        endif
                        if (vmag > windthresh(2)) then
                          area_exceed_quad_bin(4,ib,2) =
     &                          area_exceed_quad_bin(4,ib,2) + xarea
                        endif
                        if (vmag > windthresh(3)) then
                          area_exceed_quad_bin(4,ib,3) =
     &                          area_exceed_quad_bin(4,ib,3) + xarea
                        endif

                      endif

                    enddo binloop

                  endif

                enddo niloop

              enddo njloop

            else

              ! In this else statement is the case for a grid whose
              ! resolution is already fine enough that we don't need
              ! to interpolate any further.  For example, we will have
              ! the H*Wind data on a 0.05 degree grid, so that's already
              ! fine enough.

              call calcdist (glon(ii),glat(j),xsfclon,xsfclat
     &                      ,xdist,degrees)

              if (xdist <= 350.) then
                ipdfbin = min((int(vmagkts / 10.) + 1),16)
                pdf_ct_bin(ipdfbin) = pdf_ct_bin(ipdfbin) + 1
                pdf_ct_tot = pdf_ct_tot + 1
              endif

              if (xdist < 500.) then

                ! Compute area of this grid box
                xarea = (dy * 111195) *
     &                  (dx * 111195 * cos(glat(j) * dtr))

                idistbin = int(xdist / 100.) + 1

                ! Why the binloop2?  See explanation above in the "if"
                ! part of this if-then block, where binloop is.

                binloop2: do ib = idistbin,numbin

                  if (glon(ii) >= xsfclon .and.
     &                glat(j) >= xsfclat) then

                    ! NE quadrant

                    area_total_quad_bin(1,ib) =
     &                         area_total_quad_bin(1,ib) + xarea
                    if (vmag > windthresh(1)) then
                      area_exceed_quad_bin(1,ib,1) =
     &                      area_exceed_quad_bin(1,ib,1) + xarea
                    endif
                    if (vmag > windthresh(2)) then
                      area_exceed_quad_bin(1,ib,2) =
     &                      area_exceed_quad_bin(1,ib,2) + xarea
                    endif
                    if (vmag > windthresh(3)) then
                      area_exceed_quad_bin(1,ib,3) =
     &                      area_exceed_quad_bin(1,ib,3) + xarea
                    endif

                  else if (glon(ii) >= xsfclon .and.
     &                     glat(j) <  xsfclat) then

                    ! SE quadrant

                    area_total_quad_bin(2,ib) =
     &                         area_total_quad_bin(2,ib) + xarea
                    if (vmag > windthresh(1)) then
                      area_exceed_quad_bin(2,ib,1) =
     &                      area_exceed_quad_bin(2,ib,1) + xarea
                    endif
                    if (vmag > windthresh(2)) then
                      area_exceed_quad_bin(2,ib,2) =
     &                      area_exceed_quad_bin(2,ib,2) + xarea
                    endif
                    if (vmag > windthresh(3)) then
                      area_exceed_quad_bin(2,ib,3) =
     &                      area_exceed_quad_bin(2,ib,3) + xarea
                    endif

                  else if (glon(ii) < xsfclon .and.
     &                     glat(j) < xsfclat) then

                    ! SW quadrant

                    area_total_quad_bin(3,ib) =
     &                         area_total_quad_bin(3,ib) + xarea
                    if (vmag > windthresh(1)) then
                      area_exceed_quad_bin(3,ib,1) =
     &                      area_exceed_quad_bin(3,ib,1) + xarea
                    endif
                    if (vmag > windthresh(2)) then
                      area_exceed_quad_bin(3,ib,2) =
     &                      area_exceed_quad_bin(3,ib,2) + xarea
                    endif
                    if (vmag > windthresh(3)) then
                      area_exceed_quad_bin(3,ib,3) =
     &                      area_exceed_quad_bin(3,ib,3) + xarea
                    endif

                  else if (glon(ii) < xsfclon .and.
     &                     glat(j) >= xsfclat) then

                    ! NW quadrant

                    area_total_quad_bin(4,ib) =
     &                         area_total_quad_bin(4,ib) + xarea
                    if (vmag > windthresh(1)) then
                      area_exceed_quad_bin(4,ib,1) =
     &                      area_exceed_quad_bin(4,ib,1) + xarea
                    endif
                    if (vmag > windthresh(2)) then
                      area_exceed_quad_bin(4,ib,2) =
     &                      area_exceed_quad_bin(4,ib,2) + xarea
                    endif
                    if (vmag > windthresh(3)) then
                      area_exceed_quad_bin(4,ib,3) =
     &                      area_exceed_quad_bin(4,ib,3) + xarea
                    endif

                  endif

                enddo binloop2

              endif

            endif

          endif

        enddo iloop

      enddo jloop

c     -------------------------------------------------
c     Now compute the fractional wind coverage for all
c     the different quadrants, bins and thresholds...
c     -------------------------------------------------

      if ( verb .ge. 3 ) then
        write (6,109) '                                 '
     &       ,'                                     '
     &       ,'                '
        write (6,109) ' Quadrant   Bin    Wind_Thresh   '
     &       ,'Fract_coverage (%)      Area_exceeded'
     &       ,'      Area_total'
        write (6,109) ' --------   ---    -----------   '
     &       ,'------------------      -------------'
     &       ,'      ----------'
        write (6,109) '                                 '
     &       ,'                                     '
     &       ,'                '
        
        do iq = 1,numquad
          do ib = 1,numbin
            do it = 1,numthresh
              wfract_cov(iq,ib,it) = area_exceed_quad_bin(iq,ib,it) /
     &             area_total_quad_bin(iq,ib)
              write (6,117) cquad(iq),cbin(ib),cthresh(it)
     &             ,wfract_cov(iq,ib,it)*100.0
     &             ,area_exceed_quad_bin(iq,ib,it)
     &             ,area_total_quad_bin(iq,ib)
            enddo
          enddo
        enddo
      endif


  109 format (1x,a33,a37,a16)
  117 format (5x,a2,5x,a5,7x,a2,13x,f6.2,10x,f16.1,2x,f16.1)

c     -------------------------------------------------
c     Now compute the fractional wind coverage for all
c     the different bins and thresholds, but for the
c     entire "disc" of the storm, that is, summing all
c     quadrants together.
c     -------------------------------------------------

      do it = 1,numthresh
        do ib = 1,numbin
          do iq = 1,numquad
            sum_total_area(ib,it) = sum_total_area(ib,it)
     &                            + area_total_quad_bin(iq,ib)
            sum_exceed_area(ib,it) = sum_exceed_area(ib,it)
     &                             + area_exceed_quad_bin(iq,ib,it)
          enddo
          wfract_cov(5,ib,it) = sum_exceed_area(ib,it)
     &                        / sum_total_area(ib,it)
        enddo
      enddo

      if ( verb .ge. 3 ) then
        do ib = 1,numbin
          do it = 1,numthresh
            write (6,117) 'TT',cbin(ib),cthresh(it)
     &           ,wfract_cov(5,ib,it)*100.0
     &           ,sum_exceed_area(ib,it)
     &           ,sum_total_area(ib,it)
          enddo
        enddo
      endif

      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_ike_stats (imax,jmax,inp,dx,dy,ist,ifh
     &               ,fixlon,fixlat,xsfclon,xsfclat,valid_pt,calcparm
     &               ,ike,sdp,wdp,maxstorm,trkrinfo,igisret)
c
c     ABSTRACT: This subroutine computes the Integrated Kinetic Energy
c     (IKE) and Storm Surge Damage Potential (SDP) values, based on
c     Powell (BAMS, 2007).  At this time, we are only computing the IKE
c     values for TS threshold (17.5 m/s) and above.  We are not yet
c     computing wind damage potential (WDP) since, per Mark Powell
c     (4/2008), he is currently re-formulating an algorithm for it.
c
c     LOCAL:
c
c     Arrays:
c
c     ike   Integrated kinetic energy:
c           ike(1) = IKE_10m/s  (storm energy)
c           ike(2) = IKE_18m/s  (IKE_ts, tropical storm)
c           ike(3) = IKE_33m/s  (IKE_h,  hurricane)
c           ike(4) = IKE_25_40 m/s  (Not currently computed)
c           ike(5) = IKE_41_54 m/s  (Not currently computed)
c           ike(6) = IKE_55 m/s     (Not currently computed)
c
c     sdp   Storm surge damage potential

      USE inparms; USE phase; USE set_max_parms; USE tracked_parms
      USE def_vitals; USE trig_vals; USE grid_bounds; USE level_parms
      USE trkrparms
      USE verbose_output

      implicit none

      type (datecard) inp
      type (trackstuff) trkrinfo

      integer, parameter :: numdist=14,numquad=4
      integer  npts,ipct,igisret,imax,jmax,ist,ifh,ilonfix,jlatfix
      integer  ibeg,jbeg,iend,jend,igiret,i,j,maxstorm,ii
      real     fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real     ike(max_ike_cats)
      real     dx,dy,degrees,rads,ri,dell,xdist,vmag,xarea
      real     xsfclon,xsfclat,sdp,wdp
      logical(1) calcparm(maxtp,maxstorm)
      logical(1) valid_pt(imax,jmax)
c
      igisret = 0
      ike = 0.0
      sdp = 0.0
      wdp = 0.0

      rads = 400.0
      ri   = 300.0
      dell = (dx+dy)/2.
      npts = rads/(dtk*dell)

c     Call get_ij_bounds in order to get the dimensions for a smaller
c     subdomain of grid points to search over.

      call get_ij_bounds (npts,0,ri,imax,jmax,dx,dy
     &     ,glatmax,glatmin,glonmax,glonmin,xsfclon,xsfclat
     &     ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_ike_stats from call to '
          print *,'!!! get_ij_bounds, STOPPING processing for storm '
          print *,'!!! number ',ist
        endif
        
        igisret = 92
        return
      endif

      if (ibeg < 1) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_ike_stats, the ibeg returned'
            print *,'!!! from get_ij_bounds is < 1, but our gridtype is'
            print *,'!!! global, so we are going to leave it as is and '
            print *,'!!! account for the grid wrapping as we go.'
            print *,' '
          endif

        else

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_ike_stats, the ibeg returned'
            print *,'!!! from get_ij_bounds is < 1, and our gridtype is'
            print *,'!!! NOT global, so we are going to abort the '
            print *,'!!! fractional wind coverage processing for'
            print *,'!!! this time.'
            print *,' '
          endif

          igisret = 94
          return
        endif
      endif
          
      if (ibeg > imax .or. jbeg > jmax .or. jbeg < 1 .or.
     &     iend < 1 .or. jend < 1) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'ERROR in get_ike_stats calculating ibeg, iend,'
          print *,'jbeg or jend.  ibeg= ',ibeg,' iend= ',iend
          print *,'               jbeg= ',jbeg,' jend= ',jend
          print *,'               imax= ',imax,' jmax= ',jmax
          print *,'fractional wind coverage processing will not be '
          print *,'performed for this time.'
        endif

        igisret = 94
        return
      endif
          
      if (iend > imax) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_ike_stats, the iend returned'
            print *,'!!! from get_ij_bounds is > imax, but our gridtype'
            print *,'!!! is global, so we are going to leave it as is '
            print *,'!!! and account for the grid wrapping.'
            print *,' '
          endif

        else

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In get_ike_stats, the iend returned'
            print *,'!!! from get_ij_bounds is > imax, and our gridtype'
            print *,'!!! is NOT global, so we will abort the '
            print *,'!!! fractional wind coverage processing for'
            print *,'!!! this time.'
            print *,' '
          endif

          igisret = 94
          return
        endif
      endif

c     Search a grid of points near the storm center, evaluate if the
c     storm is within the "rads" distance threshold.  If so, compute
c     the IKE values for all applicable thresholds (10, 18, 33 m/s).

      do j = jbeg,jend
        do i = ibeg,iend

          if (i > imax) then
            if (trkrinfo%gridtype == 'global') then
              ii = i - imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: In get_ike_stats, the '
                print *,'!!!    user-requested eastern search boundary'
                print *,'!!!    is beyond the eastern bounds of '
                print *,'!!!    this regional grid.  We will not '
                print *,'!!!    perform the ike stats'
                print *,'!!!    processing for this storm & time.'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   User-requested eastern i = ',i
                print *,' '
              endif

              igisret = 94
              return
            endif
          else
            ii = i
          endif
              
          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              ii = i + imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i < 1 in get_ike_stats'
                print *,'!!! for a non-global grid.  We will not '
                print *,'!!! perform the ike stats'
                print *,'!!! processing for this storm & time.'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   User-requested eastern i = ',i
                print *,' '
              endif

              igisret = 94
              return
            endif
          endif

          if (.not. valid_pt(ii,j)) then
            cycle    ! Only an issue for regional grids
          endif

          call calcdist (glon(ii),glat(j),xsfclon,xsfclat,xdist,degrees)

          if (xdist > rads) then
            cycle
          else

            vmag = sqrt(u(ii,j,levsfc)**2 + v(ii,j,levsfc)**2)

            if (vmag > 10.0) then
              ! Add gridpoint to IKE_10.  Compute area first...
              xarea = (dy * 111195) *
     &                (dx * 111195 * cos(glat(j) * dtr))
              ike(1) = ike(1) + (0.5 * (vmag**2) * xarea)
            endif

            if (vmag > 18.0) then
              ! Add gridpoint to IKE_ts. Area already computed for 10
              ike(2) = ike(2) + (0.5 * (vmag**2) * xarea)
            endif

            if (vmag > 33.0) then
              ! Add gridpoint to IKE_h. Area already computed for 10
              ike(3) = ike(3) + (0.5 * (vmag**2) * xarea)
            endif

          endif

        enddo
      enddo

      ike(1) = ike(1) * 1.e-12      ! Convert from J to TJ
      ike(2) = ike(2) * 1.e-12      ! Convert from J to TJ
      ike(3) = ike(3) * 1.e-12      ! Convert from J to TJ

c     Compute the storm surge damage potential (sdp)

      if (ike(2) >= 0.0) then
        sdp = 0.676 + (0.43 * sqrt(ike(2)))
     &      - (0.0176 * ((sqrt(ike(2)) - 6.5)**2) )
      else
        sdp = -99.0
      endif

c     Print out the IKE and SDP statistics...

      if ( verb .ge. 3 ) then
        print *,' IKE_10 (storm energy)   = ',ike(1)
        print *,' IKE_TS (tropical storm) = ',ike(2)
        print *,' IKE_H  (hurricane)      = ',ike(3)
        print *,' SDP                     = ',sdp
      endif

      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine distbear (xlatin,xlonin,dist,bear,xlatt,xlont)
c
c     ABSTRACT: Given an origin at latitude, longitude=xlato,xlono,
c     this subroutine will locate a target point at a distance dist in
c     km or nautical miles (depends on what you use for "rad_earth..."
c     below), at bearing bear (degrees clockwise from north).
c     Returns latitude xlatt and longitude xlont of target point.
c
c     *** NOTE ***
c     This subroutine was written to handle input lats & lons as this:
c     All latitudes are in degrees, north positive and south negative.
c     All longitudes are in degrees, west positive and east negative.
c     *** **** ***
c
c     However, for the longitudes, the rest of the  tracker uses all
c     0-360 longitudes.  Therefore, we need to convert the input lons
c     and then once again convert the lons that are returned back to
c     the calling routine.
c
c     NOTE-- When origin is at north or south pole, bearing is no
c     longer measured from north.  Instead, bearing is measured
c     clockwise from the longitude opposite that specified in xlono.
c     Example-- if xlato=90., xlono=80., the opposite longitude is
c     -100 (100 East), and a target at bearing 30. will lie on the
c     -70. (70 East) meridian.
c
c     AUTHOR: The core of this subroutine was written by Albion
c             Taylor, another NOAA employee, in 1981.
c
      USE trig_vals

      implicit none
c
      real, parameter :: rad_earth_nm = 3440.170  ! radius of earth
      real, parameter :: rad_earth_km = 6372.797  ! radius of earth
      real     xlato,xlono,dist,bear,xlatt,xlont,xlatin,xlonin
      real     cdist,sdist,clato,slato,clono,slono,cbear,sbear
      real     z,y,x,r,xlattz,xlontz,ddist,dbear,dxlato,dxlono
c
      xlato = xlatin
      xlono = xlonin

cstr   print *,' '
cstr   print *,'+++ At top of distbear....'
cstr   print '(a6,f7.2,a3,f7.2,a9,f7.2)','xlon= ',xlono,'E  ',360.-xlono
cstr  &                                 ,'W   xlat=',xlato
cstr   print '(a6,f7.2,a8,f7.2)','dist= ',dist,'  bear= ',bear

      if (xlono > 180.) then
        ! Longitude input for this subroutine must be positive west
        xlono = 360. - xlono
      else
        ! Longitude input for this subroutine must be negative east
        xlono = -1. * xlono
      endif

cstr      print '(a31,a8,f8.2)','After conversion for distbear, '
cstr     &                     ,' xlono= ',xlono

      ddist  = dist
      dbear  = bear
      dxlato = xlato
      dxlono = xlono

      cdist = cos(ddist/rad_earth_km)
      sdist = sin(ddist/rad_earth_km)
      clato = cos(dtr*dxlato)
      slato = sin(dtr*dxlato)

cstr      print *,'cdist= ',cdist,' sdist= ',sdist,' clato= ',clato
cstr     &       ,' slato= ',slato

      clono = cos(dtr*dxlono)
      slono = sin(dtr*dxlono)

cstr      print *,'dxlono= ',dxlono,' clono= ',clono
cstr     &       ,' slono= ',slono

      cbear = cos(dtr*dbear)
      sbear = sin(dtr*dbear)

cstr      print *,'cbear= ',cbear,' sbear= ',sbear

      z=cdist*slato + clato*sdist*cbear
      y=clato*clono*cdist + sdist*(slono*sbear - slato*clono*cbear)
      x=clato*slono*cdist - sdist*(clono*sbear + slato*slono*cbear)

cstr      print *,'z= ',z,' y= ',y,' x= ',x

      r = sqrt(x**2 + y**2)

cstr      print *,'r = sqrt(x**2 + y**2) = ',r

      xlattz = atan2(z,r)/dtr

cstr      print *,'xlattz = datan2(z,r)/dtr = ',xlattz

      xlatt = xlattz

      if (r <= 0.) go to 20

      xlontz = atan2(x,y)/dtr

cstr      print *,'xlontz = atan2(x,y)/dtr = ',xlontz

c      xlont = xlontz

      ! Return the target longitude back to the calling routine
      ! as a 0-360 positive east longitude....

      xlont = mod(360.-xlontz,360.)

c      xlont = mod(360.+xlontz,360.)

cstr      print *,' '
cstr      print *,'At end of distbear....'
cstr      print '(a6,f7.2,a3,f7.2,a9,f7.2)','xlont= ',xlont,'E  '
cstr                            ,360.-xlont,'W   xlatt=',xlatt

      return
   20 xlont=0.
c
      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine bilin_int_uneven (targlat,targlon,dx,dy
     &              ,imax,jmax,trkrinfo,level,cparm,xintrp_val
     &              ,valid_pt,bimct,ibiret)
c
c     ABSTRACT: This subroutine performs a bilinear interpolation to get
c     a data value at a given lat/lon that may be anywhere within a box
c     defined by the four surrouding grid points.  In the diagram below,
c     remember that for our grids we are using in the  tracker, the
c     latitude index starts at the north pole and increases southward.
c     The point "X" indicates the target lat/lon location of the value
c     for which we are  bilinearly interpolating.  The values to and ta
c     below are ratios that determine how geographically close the
c     target location is to the point of origin (pt.1 (i,j)) in terms
c     of both longitude (to) and latitude (ta).
c
c
c        pt.1                pt.2
c        (i,j)              (i+1,j)
c
c
c
c                       X
c
c        pt.4                pt.3
c        (i,j+1)            (i+1,j+1)
c

      USE grid_bounds; USE tracked_parms; USE level_parms
      USE trkrparms
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo

      character  cparm*1
      logical(1) valid_pt(imax,jmax)
      real       targlat,targlon,xintrp_val,dx,dy
      real       to,ta,d1,d2,d3,d4,z,eastlon
      integer    ie,iw,jn,js,ibiret,imax,jmax,level,nlev,bimct

      ibiret = 0

c     --------------------------------------------------------------
c     For the latitudes and longitudes surrounding our target
c     lat/lon location, convert the lat/lon values into i- and
c     j-indices.
c     --------------------------------------------------------------

c     Find the j-indices for the points just to the north and the
c     south of targlat....

      if (targlat >= 0.0) then
        ! For a northern hemisphere storm, jn is the j-index for the
        ! point just to the *NORTH* (poleward) of targlat.
        jn = int((glatmax - targlat)/dy + 1.)
        js = jn + 1
      else
        ! For a southern hemisphere storm, js is the j-index for the
        ! point just to the *SOUTH* (poleward) of targlat.
        js = ceiling((glatmax - targlat)/dy + 1.)
        jn = js - 1
      endif

      ! Check to make sure that points are not being requested beyond
      ! the northern or southern boundaries of the grid.  This is most
      ! likely to happen for a smaller, regional grid.

      if (jn > jmax .or. js > jmax) then
        if ( verb .ge. 1 .and. bimct == 0) then
          print *,' '
          print *,'!!! ERROR: jmax exceeded in subroutine  '
          print *,'!!! bilin_int_uneven.  Returning to calling '
          print *,'!!! routine after assigning wind value of -99.'
          print *,'!!! jn= ',jn,' js= ',js,' jmax= ',jmax
          print *,' '
        endif
        xintrp_val = -999.0 
        ibiret = 85
        bimct = bimct + 1
        return
      endif

      if (jn < 1 .or. js < 1) then
        if ( verb .ge. 1 .and. bimct == 0) then
          print *,' '
          print *,'!!! ERROR: jn < 0 or js < 0 in subroutine  '
          print *,'!!! bilin_int_uneven.  Returning to calling '
          print *,'!!! routine after assigning wind value of -99.'
          print *,'!!! jn= ',jn,' js= ',js,' jmax= ',jmax
          print *,' '
        endif
        xintrp_val = -999.0
        ibiret = 85
        bimct = bimct + 1
        return
      endif

c     Find the i-indices for the points just to the east and the
c     west of targlon....

      ie = int((targlon - glonmin)/dx + 2.)
      iw = ie - 1

      ! Check for GM wrapping.  Check ie to see if it is between the 
      ! most eastward gridpoint and the GM (i.e., on a 1-deg global
      ! grid (360x181), it would be if targlon was between 359.0 (i=360)
      ! and the GM (i=1, not i=361)).  Similarly then, if we adjust ie 
      ! to then be 1, then we have a problem with iw, 
      ! since iw = 1 - 1 = 0.

      if (ie > imax) then
        if (trkrinfo%gridtype == 'global') then 
          ie = ie - imax
        else

          if ( verb .ge. 1 .and. bimct == 0) then
            print *,' '
            print *,'!!! ERROR: ie > imax in subroutine  '
            print *,'!!! bilin_int_uneven for a non-global grid.  '
            print *,'!!! Returning to calling routine after '
            print *,'!!! assigning missing wind value of -99.'
            print *,'!!! ie= ',ie,' imax= ',imax
            print *,' '
          endif
          xintrp_val = -999.0
          ibiret = 85
          bimct = bimct + 1
          return
        endif    
      endif

      if (iw < 1) then
        if (trkrinfo%gridtype == 'global') then
          iw = iw + imax
        else
          if ( verb .ge. 1 .and. bimct == 0) then
            print *,' '
            print *,'!!! ERROR: iw < 1 in subroutine  bilin_int_uneven'
            print *,'!!! for a non-global grid.  Returning to calling '
            print *,'!!! routine after assigning missing wind value '
            print *,'!!! of -99.   iw= ',iw
            print *,' '
          endif
          xintrp_val = -999.0
          ibiret = 85
          bimct = bimct + 1
          return
        endif    
      endif

ctmwc      if ( verb .ge. 3 ) then
ctmwc        print *,'  +++ Interpolating winds for cparm= ',cparm
ctmwc        print '(6x,4(a4,i3))','jn= ',jn,' js= ',js,' iw= ',iw,' ie= ',ie
ctmwc      endif

c     ----------------------------------------------------------------
c     Calculate the longitude (to) and latitude (ta) location ratios.
c     Check for GM wrapping, as we can run into a problem here if 
c     interpolating for points that are just west of the GM, since we
c     would be interpolating using values of longitude just west of
c     GM (say, glon(iw)=359.5) and the GM (glon(ie) = 0.0).  This 
c     makes for an incorrect "to" ratio below, with 0-359.5 in the 
c     denominator.  We have to account for this....
c     ----------------------------------------------------------------

      if (glon(iw) > 300.0 .and. 
     &   (glon(ie) < 10. .and. glon(ie) >= 0.)) then
        eastlon = 360. - glon(ie)
      else
        eastlon = glon(ie)
      endif

ctmwc      if ( verb .ge. 3 ) then
ctmwc        print *,'glat(js)= ',glat(js),' glat(jn)= ',glat(jn)
ctmwc      endif

      to = (targlon - glon(iw)) / (eastlon - glon(iw))
      ta = (targlat - glat(jn)) / (glat(js) - glat(jn))

c     --------------------------------------------------------------
c     Copy the data values at the 4 known points into simple scalar
c     variables
c     --------------------------------------------------------------

      if (valid_pt(iw,jn) .and. valid_pt(iw,js) .and.
     &    valid_pt(ie,jn) .and. valid_pt(ie,js)) then
        continue
      else
        if (verb .ge. 3) then
c          print *,' '
c          print *,'+++ NOTE: In bilin_int_uneven, pts with invalid '
c          print *,'          data were accessed.  We are likely at'
c          print *,'          the edge of a regional grid.'
c          print *,'          iw= ',iw,' ie= ',ie,' jn= ',jn,' js= ',js
          ibiret = 85          
          return
        endif
      endif 

      select case (level)
        case (850);  nlev = nlev850  ! check module level_parms for
        case (700);  nlev = nlev700  ! the values of these....
        case (500);  nlev = nlev500
        case (1020); nlev = levsfc
      end select

      if (cparm == 'u') then
        d1 = u(iw,jn,nlev)
        d2 = u(ie,jn,nlev)
        d3 = u(ie,js,nlev)
        d4 = u(iw,js,nlev)
      else if (cparm == 'v') then
        d1 = v(iw,jn,nlev)
        d2 = v(ie,jn,nlev)
        d3 = v(ie,js,nlev)
        d4 = v(iw,js,nlev)
      else if (cparm == 'm') then
        d1 = lsmask(iw,jn)
        d2 = lsmask(ie,jn)
        d3 = lsmask(ie,js)
        d4 = lsmask(iw,js)
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in bilin_int_uneven.'
          print *,'!!! Input cparm not recognized.'
          print *,'!!! cparm= ',cparm
          print *,'!!! EXITING....'
        endif

        stop 95
      endif

      z = 1.9427

cstr      print '(2x,4(a4,f8.2))','  d1= ',d1*z,'  d2= ',d2*z
cstr     &                       ,'  d3= ',d3*z,' d4= ',d4*z

c     -------------------------------------------------------------
c     Compute the interpolated value
c     -------------------------------------------------------------

      xintrp_val = (1.-to) * (1.-ta) * d1
     &           + to * (1.-ta) * d2
     &           + to * ta * d3
     &           + (1.-to) * ta * d4

cstr     print '(2x,2(a11,f8.2))','   xintrp= ',xintrp_val,' (in kts)= '
cstr    &                     ,xintrp_val*z
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine sort_storms_by_pressure (gridprs,ifh,maxstorm,sortindex
     &                                   ,issret)
c
c     ABSTRACT: This subroutine  sorts storms by mslp.  It is called by
c     subroutine  tracker just before the loop for "stormloop" is done
c     for all the storms at a particular forecast hour.  It is only
c     called for the "midlat" and "tcgen" cases.  The end result of
c     this sort is an array (prsindex) that contains the indeces of
c     the storms, arranged from lowest pressure to highest (and note
c     that the "undefined" storms have a pressure of 9999.99 mb and
c     thus get sorted to the bottom of the array).  The purpose of
c     doing this is so that we track the most intense storms first.
c     Why go to the trouble?  Imagine a scenario in which we are
c     tracking a complex system in which there are 2 low pressure
c     centers.  Let's say that one is becoming dominant and
c     intensifying, while the other is weakening.  Now, let's assume
c     that the weakening one eventually gets absorbed into the
c     stronger, more dominant low.  Now we only have 1 low, but if in
c     the  tracker stormloop, we first process the data for the
c     weakening low, we will attribute the track to that storm, and
c     then when we get to the point in the loop where we are trying
c     to get the track for the stronger storm, we will (erroneously)
c     stop the tracking for that storm since the storm center has
c     already been attributed to the weaker storm.  But by using this
c     subroutine, we will track the stronger storm first, and thus
c     avoid this problem.
c
c     NOTE: The pressures used in the  sort are those obtained at the
c     previous forecast hour.  At forecast hour = 0, just use the
c     values as they were input to this routine, since they were
c     found in first_ges_center from strongest to weakest already.
c
c     INPUT:
c     gridprs  real array of storm mslp values
c     ifh      integer index for the current forecast hour
c     maxstorm max num of storms that can be handled in this run
c
c     OUTPUT:
c     sortindex contains a sorted array of indeces.  The  orders
c               sort routine does NOT rearrange the data.  Rather, it
c               returns this array of sorted indeces which point to
c               the correct order of data values in the data array.
c     issret    return code from this subroutine
c
      USE set_max_parms
      USE verbose_output

      real, allocatable :: iwork(:)
      real      gridprs(maxstorm,maxtime)
      integer   ifh,maxstorm
      integer   sortindex(maxstorm)
      integer, parameter  :: dp = selected_real_kind(12, 60)
      real (dp), allocatable ::  prstemp(:)
c
      allocate (prstemp(maxstorm),stat=iva)
      allocate (iwork(maxstorm),stat=iwa)
      if (iva /= 0 .or. iwa /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in sub sort_storms_by_pressure allocating'
          print *,'!!! prstemp or iwork arrays: '
          print *,'!!! iva= ',iva,' iwa= ',iwa
        endif

        STOP 94
        return
      endif

      if (ifh > 1) then

c        print *,' '
c        print *,'--- Before  sort, original prs values follow:'
c        print *,' '

        do ist = 1,maxstorm
          prstemp(ist) = gridprs(ist,ifh-1)
c           write (6,81) ist,prstemp(ist)/100.0
        enddo

        imode = 2
        sortindex = 0
        call qsort (prstemp,sortindex,maxstorm)

ccccc        call orders (imode,iwork,prstemp,sortindex,maxstorm,1,8,1)
ccccc        call orders_4byte (imode,iwork,prstemp,sortindex,maxstorm,1,8,1)

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ Pressure-sorted storm list:'
          print *,' '

          do ist = 1,maxstorm
            if (prstemp(sortindex(ist))/100.0 < 9999.0) then
              write (6,82) ist,sortindex(ist)
     &             ,prstemp(sortindex(ist))/100.0
            endif
          enddo

 81       format (1x,'ist= ',i5,' Original (unsorted) prstemp= ',f7.2)
 82       format (1x,'ist= ',i5,'  sortindex(ist)= ',i5
     &           ,' prstemp= ',f7.2)
        endif

      else
        do ist = 1,maxstorm
          sortindex(ist) = ist
        enddo
      endif

      deallocate (prstemp); deallocate (iwork)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getvrvt (centlon,centlat,xlon,xlat
     &                   ,udat,vdat,vr,vt,igvtret)
c
c     ABSTRACT: This subroutine takes as input a u-wind and v-wind value
c     at an input (xlon,xlat) location and returns the tangential and
c     radial wind components relative to the input center lat/lon 
c     position (centlon,centlat).  The only trick to this whole 
c     subroutine is figuring out the angle from the center point to the
c     data point, and we do this by creating a triangle with the leg 
c     from the center point to the data point being the hypotenuse.
c
c     NOTE: All longitudes must be in positive degrees east (0-360) !!!
c
c     INPUT:
c     centlon  Longitude of center point
c     centlat  Latitude  of center point
c     xlon     Longitude of pt at which vr & vt will be computed
c     xlat     Latitude  of pt at which vr & vt will be computed
c     udat     u-value of wind at the point (xlon,xlat) 
c     vdat     v-value of wind at the point (xlon,xlat) 
c
c     OUTPUT:
c     vr      Radial wind component at (xlon,xlat) wrt (centlon,centlat)
c     vt      Tang   wind component at (xlon,xlat) wrt (centlon,centlat)
c     igvtret Return code from this subroutine
c
      USE trig_vals
      USE verbose_output

      implicit none

      real centlon,centlat,xlon,xlat,udat,vdat,vr,vt,degrees,tmpxlon
      real angle,xlondiff,xlatdiff,opp_dist,hyp_dist,sin_value
      real cos_value,adj_dist,tmpangle,sin_angle,cos_angle
      real uvrcomp,vvrcomp,uvtcomp,vvtcomp
      integer igvtret
c
      call calcdist(centlon,centlat,xlon,xlat,hyp_dist,degrees)

c      xxxx

      tmpxlon = xlon

      if (centlon > 330.0) then

        if (xlon > 360.0) then
         
          tmpxlon = xlon  ! All lons will be in the 300+ range, so for
                          ! consistency, we're ok.

        elseif (xlon < 30.0) then

          tmpxlon = xlon + 360.  ! In this case, the fix center is just
                          ! to the west of the GM with a lon (centlon) 
                          ! > 330, while the point being evaluated
                          ! (xlon) is just east of the GM, but with a 
                          ! lon (centlon) < 30.  Need to adjust here to
                          ! to get the xlon in the 330+ frame of 
                          ! reference.

        endif

      elseif (centlon >= 0 .and. centlon < 30.0) then

        if (xlon >= 360.0) then

          tmpxlon = xlon - 360.

        elseif (xlon > 330. .and. xlon < 360.) then

          tmpxlon = 360. - xlon

        endif

      elseif (centlon < 0.0) then

        if (xlon >= 360.0) then
 
          tmpxlon = xlon - 360.

        elseif (xlon > 330. .and. xlon < 360.) then

          tmpxlon = -1 * (360. - xlon)

        endif

      endif

      xlatdiff = abs(centlat - xlat)
      xlondiff = abs(centlon - tmpxlon)

      if (centlon > 355.0) then
        if (xlondiff > 40.0) then
          if (verb .ge. 3) then
            write (6,91) centlon,tmpxlon,hyp_dist,degrees,xlondiff
   91       format (1x,'WARNING - XLONDIFF > 40 in getvrvt: centlon= '
     &                ,f8.3,' tmpxlon= ',f8.3,' hyp_dist= '
     &                ,f10.2,' degrees= ',f10.2,' xlondiff= ',f12.2)
          endif
        endif
      endif

      if (xlondiff == 0 .and. xlatdiff > 0) then

        if (centlat > xlat) angle = 180   ! pt directly south of ctr
        if (centlat < xlat) angle = 0     ! pt directly north of ctr

      else if (xlondiff > 0 .and. xlatdiff == 0) then

        if (centlon > tmpxlon) angle = 270   ! pt directly west of ctr
        if (centlon < tmpxlon) angle = 90    ! pt directly east of ctr

      else

        ! This next part figures out the angle from the center point
        ! (centlon,centlat) to the data point (tmpxlon,xlat).  It does
        ! this by setting up a triangle and then using inverse trig
        ! functions to get the angle.  Since this is a kludgy way to
        ! do it that doesn't account for the curvature of the earth,
        ! we'll do it 2 ways, using asin and then acos, then take the
        ! average of those 2 for the angle.  hyp_dist, calculated just
        ! above, is the distance from the center pt to the data pt.

        opp_dist  = xlatdiff/360. * ecircum
        sin_value = opp_dist / hyp_dist
        if (sin_value > 1.0) then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! In getvrvt, sin_value > 1, setting to 1.'
            print *,'!!! opp_dist= ',opp_dist,' hyp_dist= ',hyp_dist
            print *,'!!! sin_value = ',sin_value
            print *,'!!! centlon= ',centlon,' centlat= ',centlat
            print *,'!!! tmpxlon=    ',tmpxlon,' xlat=    ',xlat
            print *,' '
          endif

          sin_value = 0.99999
        endif
        sin_angle = asin(sin_value) / dtr

        call calcdist(centlon,centlat,tmpxlon,centlat,adj_dist,degrees)
        cos_value = adj_dist / hyp_dist
        if (cos_value > 1.0) then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! In getvrvt, cos_value > 1, setting to 1.'
            print *,'!!! adj_dist= ',adj_dist,' hyp_dist= ',hyp_dist
            print *,'!!! cos_value = ',cos_value
            print *,'!!! centlon= ',centlon,' centlat= ',centlat
            print *,'!!! tmpxlon=    ',tmpxlon,' xlat=    ',xlat
            print *,' '
          endif

          cos_value = 0.99999
        endif
        cos_angle = acos(cos_value) / dtr

        tmpangle = 0.5 * (sin_angle + cos_angle)

        ! The previous lines of code just calculated an angle between
        ! 0 and 90.  This next if structure adjusts that angle to 
        ! instead be between 0 and 360.

        if      (centlat <= xlat .and. centlon <= tmpxlon) then
          angle = 90 - tmpangle
        else if (centlat >  xlat .and. centlon <= tmpxlon) then
          angle = 90 + tmpangle
        else if (centlat >= xlat .and. centlon >= tmpxlon) then
          angle = 270 - tmpangle
        else if (centlat <  xlat .and. centlon >= tmpxlon) then
          angle = 270 + tmpangle
        endif

      endif

      uvrcomp = udat * sin(angle * dtr)
      vvrcomp = vdat * cos(angle * dtr)
      vr      = uvrcomp + vvrcomp

      uvtcomp = (-udat) * cos(angle * dtr)
      vvtcomp = vdat    * sin(angle * dtr)
      vt      = uvtcomp + vvtcomp

      return 
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_atcfunix (outlon,outlat,inp,ist
     &         ,ifcsthour,vmaxwind,xminmslp,vradius,maxstorm
     &         ,trkrinfo,plastbar,rlastbar,rmax,cps_vals
     &         ,wcore_flag,ioaxret)

c     ABSTRACT: This subroutine  outputs a 1-line message for a given 
c     storm at an input forecast hour in the new ATCF UNIX format.  
c     Unlike the old atcf DOS format in which you waited until the 
c     whole tracking was over to write the  output for all forecast 
c     hours, with this atcfunix format, each time we are calling this
c     subroutine, it is to only write out 1 record, which will be the
c     fix info for a particular storm at a given time.  Also, even 
c     though we have some data (GFS, NAM) at 6-hour intervals, Jim 
c     Gross informed me that TPC does not need the positions at such
c     frequency, and keeping the reporting at 12 hour intervals is fine.
c
c     While this new atcfunix format contains much more information than
c     the old 1-line atcf dos message, for our purposes we will use the
c     slots for mslp and wind radii.  An example set of output records
c     will look like the following:
c
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  34,
c             NEQ,  242,  163,  124,  208
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  50,
c             NEQ,  155,  000,  000,  000
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  64,
c             NEQ,  000,  000,  000,  000
c
c     (NOTE: Each of the above lines beginning with "AL" is output as 
c            a single line of text.)
c
c     Note that in this example, for this 36h forecast hour, there are 
c     3 entries.  This is so that we can include the radii for the 
c     3 different wind thresholds (34kt, 50kt and 64kt).  So the only
c     thing different in each entry is the wind radii info;  all the
c     other info is identical for each entry.
c
c     This message also contains the intensity estimates (in knots) 
c     for every forecast hours  The  conversion for m/s to knots is 
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft, 
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the 
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and 
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     outlon    longitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c     maxstorm  max # of storms that can be handled
c     plastbar  pressure of the outermost closed isobar
c     rlastbar  radius (nm) of the outermost closed isobar
c     rmax      radius of max winds (n mi).... it was already converted
c               from km to n mi in subroutine  get_max_wind
c     cps_vals  real array with the values for the 3 cyclone phase 
c               space parameters: (1) is for Parameter B (thermal
c               asymmetry); (2) is for lower level (600-900 mb) thermal
c               wind; (3) is for upper level (300-600 mb) thermal wind.
c     wcore_flag character for value of 300-500 mb warm core: y, n, or 
c               'u' for undetermined.
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c     
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c     storm     An array of type tcvcard.  Use this for the storm ID
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE trkrparms; USE phase
      USE verbose_output

      type (datecard) inp
      type (trackstuff) trkrinfo

      real    cps_vals(3)
      real    outlon,outlat,rmax,mslp_outp_adj
      real    vmaxwind,conv_ms_knots,xminmslp,plastbar,rlastbar
      integer intlon,intlat,irmax,output_fhr,ic,iplastbar,irlastbar
      integer vradius(3,4),icps_vals(3)
      character  basinid*2,clatns*1,clonew*1,wcore_flag*1
      character comma_fill1*48,comma_fill2*31,comma_filler*79

      if ( verb .ge. 3 ) then
        print *,'TTT top of atcfunix, ist= ',ist,' ifh= ',ifcsthour
      endif

      if (xminmslp == 999999.0) xminmslp = 0.0

      if (xminmslp < 1100.0) then
        ! Pressure units are in mb...
        mslp_outp_adj = 1.0
      elseif (xminmslp >80000.0) then
        ! Pressure units are in Pa...
        mslp_outp_adj = 100.0
      else
        if (verb .ge. 3) then
          print *,' '
          print *,'ERROR: Something wrong in subroutine'
          print *,'       output_atcfunix.  The mslp value'
          print *,'       (xminmslp) is not in range.'
          print *,'       xminmslp = ',xminmslp
          print *,'       EXITING....'
          print *,' '
          stop 95
        endif
      endif

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'in output_atcfunix, tcv_storm_id= '
     &       ,storm(ist)%tcv_storm_id
        print *,'in output_atcfunix, tcv_storm_id(3:3)= '
     &       ,storm(ist)%tcv_storm_id(3:3)
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
!zhang        case ('A','a');  basinid = 'NA'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = 'HC'
      end select

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

      if (rmax == -99.0) then
        irmax = -99
      else    
        irmax = int(rmax + 0.5)
      endif

      if (trkrinfo%want_oci) then
        if (plastbar > 0.0) then
          iplastbar = int(plastbar/mslp_outp_adj + 0.5)
        else
          iplastbar = -99
        endif
        if (rlastbar > 0.0) then
          irlastbar = int(rlastbar + 0.5)
        else
          irlastbar = -99
        endif
      else
        iplastbar = -99
        irlastbar = -99
      endif

      if ( verb .ge. 3 ) then
        print *, 'output: rlastbar=',rlastbar,' irlastbar=',irlastbar
        print *, 'output: plastbar=',plastbar,' iplastbar=',iplastbar
      endif

c     Now convert all of the cyclone phase space parameter values from
c     real to integer.

      do ic = 1,3
        if (cps_vals(ic) > -9999.0) then
          if (cps_vals(ic) >= 0.0) then
            icps_vals(ic) = int(cps_vals(ic)*10. + 0.5)
          else
            icps_vals(ic) = int(cps_vals(ic)*10. - 0.5)
          endif
        else
          icps_vals(ic) = -9999
        endif
      enddo

      if (wcore_flag == 'y'.or. wcore_flag == 'Y') then
        wcore_flag = 'Y'
      elseif (wcore_flag == 'n' .or. wcore_flag == 'N') then
        wcore_flag = 'N'
      elseif (wcore_flag == 'u' .or. wcore_flag == 'U') then
        wcore_flag = 'U'
      else
        wcore_flag = 'U'
      endif

      comma_fill1 = ',   0,   0,    ,   0,    ,   0,   0,           ,'
      comma_fill2 = '  ,   ,    ,   0,   0,   0,   0'
      comma_filler = comma_fill1//comma_fill2

      if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') then

        if (stcvtype(ist) == 'FOF') then
          ! If this is a TC vitals-described storm (i.e., one that is
          ! numbered by JTWC or NHC), then leave the basinid as is.
          ! Otherwise, we want to use the "basinid" location as a 
          ! label to identify what type of run this is.
          if (trkrinfo%type == 'midlat') basinid = 'ML'
          if (trkrinfo%type == 'tcgen')  basinid = 'TG'
        endif

        write (64,91) basinid,adjustr(storm(ist)%tcv_storm_id)
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  34, NEQ'
     &        ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &        ,iplastbar,irlastbar,irmax,0,0,stcvtype(ist)

        if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &      vradius(2,3) > 0 .or. vradius(2,4) > 0) then
          write (64,91) basinid,adjustr(storm(ist)%tcv_storm_id)
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  50, NEQ'
     &        ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &        ,iplastbar,irlastbar,irmax,0,0,stcvtype(ist)
        endif

        if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &      vradius(3,3) > 0 .or. vradius(3,4) > 0) then
          write (64,91) basinid,adjustr(storm(ist)%tcv_storm_id)
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  64, NEQ'
     &        ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &        ,iplastbar,irlastbar,irmax,0,0,stcvtype(ist)
        endif

      else

        write (64,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  34, NEQ'
     &        ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &        ,iplastbar,irlastbar,irmax,comma_filler,icps_vals(1)
     &        ,icps_vals(2),icps_vals(3),wcore_flag,int(wcore_depth*10)

        if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &      vradius(2,3) > 0 .or. vradius(2,4) > 0) then
          write (64,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)                   
     &        ,int(xminmslp/mslp_outp_adj + 0.5)         
     &        ,'XX,  50, NEQ'          
     &        ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &        ,iplastbar,irlastbar,irmax,comma_filler,icps_vals(1)
     &        ,icps_vals(2),icps_vals(3),wcore_flag,int(wcore_depth*10)
        endif

        if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &      vradius(3,3) > 0 .or. vradius(3,4) > 0) then
          write (64,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  64, NEQ'
     &        ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &        ,iplastbar,irlastbar,irmax,comma_filler,icps_vals(1)
     &        ,icps_vals(2),icps_vals(3),wcore_flag,int(wcore_depth*10)
        endif

      endif

      if ( verb .ge. 3 ) then
        print *,'rmax= ',rmax,'  irmax= ',irmax
      endif

   81 format (a2,', ',a2,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
     &       ,2(', ',i4),', ',i3,a79,',       THERMO PARAMS'
     &       ,3(', ',i7),', ',a1,', ',i2,', DT, -999')
   91 format (a2,', ',a4,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
     &       ,2(', ',i4),', ',i3,2(', ',i3),', ',a3)

c     bug fix for IBM: flush the output stream so it actually writes
      flush(64)

      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_all (fixlon,fixlat,inp,maxstorm,ifhmax,ioaret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for each
c     storm.  This message contains the model identifier, the forecast
c     initial date, and the positions for 0, 12, 24, 36, 48, 60 and 72
c     hours.  In the case of the regional models (NGM, Eta), which
c     only go out to 48h, zeroes are included for forecast hours
c     60 and 72.
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The  output of this
c     subroutine is used by Steve Lord for plotting purposes, and his
c     plotting routines need the longitudes in 0 - 360, increasing
c     westward.  Thus, a necessary adjustment is made.
c
      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE tracked_parms
c
      type (datecard) inp
c
      real    fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      integer modelnum(maxmodel)
      integer intlon(maxtime),intlat(maxtime)
      character  modelchar(maxmodel)*4

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0 - 360, increasing westward.

      print *,'top of output_all'

      stormloop: do ist = 1,maxstorm

        if (stormswitch(ist) == 3) cycle stormloop
        intlon = 0; intlat = 0

        ifhloop: do ifh = 1,maxtime

          if (ifh <= ifhmax) then
            if (ifhours(ifh) == 99) then
              intlon(ifh) = 0
              intlat(ifh) = 0
              cycle ifhloop
            endif
          else
            intlon(ifh) = 0
            intlat(ifh) = 0
            cycle ifhloop
          endif

          if (fixlon(ist,ifh) < -998.0 .or. fixlat(ist,ifh) < -998.0)
     &    then
            intlon(ifh) = 0
            intlat(ifh) = 0
          else
            intlon(ifh) = 3600 - int(fixlon(ist,ifh) * 10. + 0.5)
            intlat(ifh) = int(abs(fixlat(ist,ifh)) * 10. + 0.5)
            if (fixlat(ist,ifh) < 0.0) then
              intlat(ifh) = intlat(ifh) * (-1)
            endif
          endif

        enddo ifhloop

        print *,'before select case, atcfname= '

        select case (atcfname(1:3))

          case ('SEC','SEN','SEP','SKC','SKN','SKP','SRC','SRN','SRP')
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(5),intlon(5),intlat(9),intlon(9),intlat(13)
     &           ,intlon(13),intlat(17),intlon(17),intlat(21),intlon(21)
     &           ,0,0,storm(ist)%tcv_storm_id

          case ('AVN','NGM','ETA','GFD','AP0','AN0','AP1','AN1','AC0'
     &         ,'AMM','CMC','HWR')
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(3),intlon(3),intlat(5),intlon(5),intlat(7)
     &           ,intlon(7),intlat(9),intlon(9),intlat(11),intlon(11)
     &           ,intlat(13),intlon(13),storm(ist)%tcv_storm_id

          case ('MRF','UKX','NGX','EP0','EP1','EP2','EN0','EN1','EN2'
     &         ,'CP0','CN0','CC0','EC0','EMX')
            ! MRF, UKMET, NAVGEM, ECMWF Ensemble
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),intlat(3),intlon(3),intlat(4)
     &           ,intlon(4),intlat(5),intlon(5),intlat(6),intlon(6)
     &           ,intlat(7),intlon(7),storm(ist)%tcv_storm_id

          case ('GDA','HDA')       ! GDAS, HDAS
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),intlat(3),intlon(3)
     &           ,intlat(4),intlon(4),0,0,0,0,0,0
     &           ,storm(ist)%tcv_storm_id

          case ('WP0','WP1','WN0','WN1','XP0','XP1','XN0','XN1'
     &         ,'YP0','YP1','YN0','YN1','ZP0','ZP1','ZN0','ZN1')
            ! Ensemble RELOCATION ONLY
            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),0,0,0,0,0,0,0,0,0,0
     &           ,storm(ist)%tcv_storm_id

          case default
c            print *,'!!! ERROR in subroutine  output_all. '
c            print *,'!!! Model name is not identified.'
c            print *,'!!! Model name = ',atcfname
c            print *,'!!! ist = ',ist,' Model number = ',atcfnum
            print *,' '
            print *,'!!! Model name is not identified: ',atcfname

            write (61,81) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(1),intlon(1)
     &           ,intlat(2),intlon(2),intlat(3),intlon(3)
     &           ,intlat(4),intlon(4),0,0,0,0,0,0
     &           ,storm(ist)%tcv_storm_id

        end select

      enddo stormloop

  81  format (i2,a4,4i2.2,14i4,1x,a3)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_atcf (fixlon,fixlat,inp,xmaxwind,maxstorm
     &                       ,ifhmax,ioaret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for each storm
c     in ATCF format.  This message contains the model identifier, the
c     forecast initial date, and the positions for 12, 24, 36, 48
c     and 72 hours.  This message also contains the intensity
c     estimates (in knots) for those same hours.  The  conversion for
c     m/s to knots is to multiply m/s by 1.9427 (3.281 ft/m,
c     1 naut mile/6080 ft, 3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The  output of this
c     subroutine is used by the atcf system at TPC for plotting
c     purposes, and the atcf plotting routines need the longitudes in
c     0 - 360, increasing westward.  Thus, a necessary adjustment is
c     made.
c
      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE tracked_parms
c
      type (datecard) inp
c
      real    fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real    xmaxwind(maxstorm,maxtime)
      real    conv_ms_knots
      integer modelnum(maxmodel)
      integer intlon(maxtime),intlat(maxtime)
      character  modelchar(maxmodel)*4,basinid*4

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0 - 360, increasing westward.

      conv_ms_knots = 1.9427

      stormloop: do ist = 1,maxstorm

        if (stormswitch(ist) == 3) cycle stormloop
        intlon = 0; intlat = 0

        ifhloop: do ifh = 1,maxtime

          if (ifh <= ifhmax) then
            if (ifhours(ifh) == 99) then
              intlon(ifh) = 0
              intlat(ifh) = 0
              cycle ifhloop
            endif
          else
            intlon(ifh) = 0
            intlat(ifh) = 0
            cycle ifhloop
          endif

          if (fixlon(ist,ifh) < -998.0 .or. fixlat(ist,ifh) < -998.0)
     &    then

            intlon(ifh) = 0
            intlat(ifh) = 0

          else
            intlon(ifh) = 3600 - int(fixlon(ist,ifh) * 10. + 0.5)
            intlat(ifh) = int(abs(fixlat(ist,ifh)) * 10. + 0.5)
            if (fixlat(ist,ifh) < 0.0) then
              intlat(ifh) = intlat(ifh) * (-1)
            endif

          endif

        enddo ifhloop

        basinid = '    '
        select case (storm(ist)%tcv_storm_id(3:3))
          case ('L','l');  basinid(1:2) = 'AL'
          case ('E','e');  basinid(1:2) = 'EP'
          case ('C','c');  basinid(1:2) = 'CP'
          case ('W','w');  basinid(1:2) = 'WP'
          case ('O','o');  basinid(1:2) = 'SC'
          case ('T','t');  basinid(1:2) = 'EC'
          case ('U','u');  basinid(1:2) = 'AU'
          case ('P','p');  basinid(1:2) = 'SP'
          case ('S','s');  basinid(1:2) = 'SI'
          case ('B','b');  basinid(1:2) = 'BB'
          case ('A','a');  basinid(1:2) = 'NA'
          case default;    basinid(1:2) = '**'
        end select
        basinid(3:4) = storm(ist)%tcv_storm_id(1:2)


        select case (atcfname(1:3))

          case ('SEC','SEN','SEP','SKC','SKN','SKP','SRC','SRN','SRP')
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(5),intlon(5)
     &           ,intlat(9),intlon(9),intlat(13),intlon(13),intlat(17)
     &           ,intlon(17),0,0
     &           ,int((xmaxwind(ist,5)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,9)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,13)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,17)*conv_ms_knots) + 0.5)
     &           ,0
     &           ,basinid,inp%byy

          case ('AVN','NGM','ETA','GFD','AP0','AN0','AP1','AN1','AC0'
     &         ,'AMM','CMC','HWR')
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(3),intlon(3)
     &           ,intlat(5),intlon(5),intlat(7),intlon(7),intlat(9)
     &           ,intlon(9),intlat(13),intlon(13)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,5)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,7)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,9)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,13)*conv_ms_knots) + 0.5)
     &           ,basinid,inp%byy

          case ('MRF','UKX','NGX','EP0','EP1','EP2','EN0','EN1','EN2'
     &         ,'CP0','CN0','CC0','EC0','EMX')
            ! MRF, UKMET, NAVGEM, ECMWF Ensemble, ECMWF hi-res
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(2),intlon(2)
     &           ,intlat(3),intlon(3),intlat(4),intlon(4),intlat(5)
     &           ,intlon(5),intlat(7),intlon(7)
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,5)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,7)*conv_ms_knots) + 0.5)
     &           ,basinid,inp%byy

          case ('GDA','HDA')        ! GDAS, HDAS
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &           ,intlon(1),intlat(1),intlat(2),intlon(2)
     &           ,intlat(3),intlon(3),intlat(4),intlon(4)
     &           ,0,0
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,0,0,basinid,inp%byy

          case ('WP0','WP1','WN0','WN1','XP0','XP1','XN0','XN1'
     &         ,'YP0','YP1','YN0','YN1','ZP0','ZP1','ZN0','ZN1')
            ! Ensemble RELOCATION ONLY
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh
     &           ,intlon(1),intlat(1),intlat(2),intlon(2)
     &           ,0,0,0,0
     &           ,0,0
     &           ,int((xmaxwind(ist,2)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,4)*conv_ms_knots) + 0.5)
     &           ,0,0,basinid,inp%byy

          case default
c            print *,'!!! ERROR in subroutine  output_atcf. '
c            print *,'!!! Model name is not identified.'
c            print *,'!!! Model name = ',atcfname
c            print *,'!!! ist = ',ist,' Model number = ',atcfnum
            print *,' '
            write (62,82) atcfnum,atcfname
     &           ,inp%byy,inp%bmm,inp%bdd,inp%bhh,intlat(3),intlon(3)
     &           ,intlat(5),intlon(5),intlat(7),intlon(7),intlat(9)
     &           ,intlon(9),intlat(13),intlon(13)
     &           ,int((xmaxwind(ist,3)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,5)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,7)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,9)*conv_ms_knots) + 0.5)
     &           ,int((xmaxwind(ist,13)*conv_ms_knots) + 0.5)
     &           ,basinid,inp%byy

        end select

      enddo stormloop

  82  format (i2,a4,4i2.2,10i4,5i3,1x,a4,i2.2)
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_hfip (outlon,outlat,inp,ist
     &          ,ifh,vmaxwind,xminmslp,vradius,rmax,ioaxret)

c     ABSTRACT: This subroutine  outputs a 1-line message for a given
c     storm at an input forecast hour in a modified ATCF UNIX format.
c     The modification is to allow for sub-hourly output.  That is,
c     instead of just integer output hours, we can have output at 
c     10, 15 or 20 past an hour.  This necessitates a change in the 
c     "forecast hour" placeholder in the ATCF format.  Instead of it
c     being an I3, we'll make it an I5, with something like a lead time
c     of 36.25h being rounded and truncated to 03625 for output.
c     
c     An example set of output records using the standard atcf format
c     looks like the following:
c     
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  34,
c             NEQ,  242,  163,  124,  208
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  50,
c             NEQ,  155,  000,  000,  000
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  64,
c             NEQ,  000,  000,  000,  000
c     
c     An example set of modified output records will look like the
c     following, for the case of a lead time of 36:15 (36.25):
c     
c     AL, 13, 2000092500, 03, AVNO, 03625, 243N, 675W, 42, 995, XX,  34,
c             NEQ,  242,  163,  124,  208
c     AL, 13, 2000092500, 03, AVNO, 03625, 243N, 675W, 42, 995, XX,  50,
c             NEQ,  155,  000,  000,  000
c     AL, 13, 2000092500, 03, AVNO, 03625, 243N, 675W, 42, 995, XX,  64,
c             NEQ,  000,  000,  000,  000
c     
c     (NOTE: Each of the above lines beginning with "AL" is output as
c            a single line of text.)
c     
c     Note that in this example, for this 36h forecast hour, there are
c     3 entries.  This is so that we can include the radii for the
c     3 different wind thresholds (34kt, 50kt and 64kt).  So the only
c     thing different in each entry is the wind radii info;  all the
c     other info is identical for each entry.
c     
c     This message also contains the intensity estimates (in knots)
c     for every forecast hours  The  conversion for m/s to knots is
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft,
c     3600s/h).
c     
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and
c     'S' to distinguish Northern/Southern Hemispheres).
c     
c     INPUT:    
c     storm     An array of type tcvcard.  Use this for the storm ID
c     outlon    longitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifh       index for the lead time array
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c     rmax      Radius of max winds (n mi).... it was already converted
c               from km to n mi in subroutine  get_max_wind
c
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE tracked_parms
      USE verbose_output

      type (datecard) inp

      real    outlon,outlat,mslp_outp_adj
      real    vmaxwind,conv_ms_knots,xminmslp,rmax
      integer intlon,intlat,output_fhr,irmax,ileadtime
      integer vradius(3,4)
      character  basinid*2,clatns*1,clonew*1

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      if (xminmslp == 999999.0) xminmslp = 0.0

      if (xminmslp < 1100.0) then
        ! Pressure units are in mb...
        mslp_outp_adj = 1.0
      elseif (xminmslp >80000.0) then
        ! Pressure units are in Pa...
        mslp_outp_adj = 100.0
      else
        if (verb .ge. 3) then
          print *,' '
          print *,'ERROR: Something wrong in subroutine'
          print *,'       output_hfip.  The mslp value'
          print *,'       (xminmslp) is not in range.'
          print *,'       xminmslp = ',xminmslp
          print *,'       EXITING....'
          print *,' '
          stop 95
        endif
      endif

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = '**'
      end select

      ! ST: ifcsthour does not exist, so output_fhr is always
      ! filled with invalid data here.  However, output_fhr is
      ! never used, so it is safe to remove.
      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        ! output_fhr = ifcsthour + 3
        ileadtime  = nint((fhreal(ifh) + 3.0) * 100.0)
      else
        ! output_fhr = ifcsthour
        ileadtime  = nint(fhreal(ifh) * 100.0)
      endif

      if (rmax == -99.0) then
        irmax = -99
      else
        irmax = int(rmax + 0.5)
      endif

      write (69,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &      ,atcfymdh
     &      ,adjustr(atcfname),ileadtime,intlat,clatns,intlon,clonew
     &      ,int((vmaxwind*conv_ms_knots) + 0.5)
     &      ,int(xminmslp/mslp_outp_adj + 0.5)
     &      ,'XX,  34, NEQ'
     &      ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4),irmax

      if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &    vradius(2,3) > 0 .or. vradius(2,4) > 0) then
        write (69,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &        ,atcfymdh
     &        ,adjustr(atcfname),ileadtime,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  50, NEQ'
     &        ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4),irmax
      endif

      if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &    vradius(3,3) > 0 .or. vradius(3,4) > 0) then
        write (69,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &        ,atcfymdh
     &        ,adjustr(atcfname),ileadtime,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  64, NEQ'
     &        ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4),irmax
      endif

   81 format (a2,', ',a2,', ',i10.10,', 03, ',a4,', ',i5.5,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
     &       ,',    0,    0, ',i3)
c
c     bug fix for IBM: flush the output stream so it actually writes
      flush(69)

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_fract_wind (outlon,outlat,xsfclon,xsfclat
     &          ,inp,ist,ifcsthour,vmaxwind,xminmslp,wfract_cov
     &          ,wfract_type,pdf_ct_bin,pdf_ct_tot,maxstorm,iofwret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for a given
c     storm at an input forecast hour.  This message contains the
c     values for the fractional areal coverage of various wind
c     thresholds.  In addition, this subroutine also writes out
c     records to a file containing data on the PDF of wind magnitudes
c     within r=350 km.
c
c     This format will mimic the current atcfunix format with the
c     difference coming late in the record, where the various wind radii
c     will be replaced with areal coverage thresholds.
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             000, 100,  34, NEE,  981,  857,  629,  810
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             000, 100,  50, NEE,  874,  732,  319,  610
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             000, 100,  64, NEE,  454,  327,   99,  270
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             000, 100,  34, AAE,  721,  721,  721,  721
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             000, 100,  50, AAE,  465,  465,  465,  465
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             000, 100,  64, AAE,  298,  298,  298,  298
c
c     (NOTE: Each of the above lines beginning with "AL" is output as
c            a single line of text.)
c
c     Note that in this example, for this 36h forecast hour, there are
c     3 entries.  This is so that we can include the pctgs for the
c     3 different wind thresholds (34kt, 50kt and 64kt).  So the only
c     thing different in each entry is the wind pctg info;  all the
c     other info is identical for each entry.
c
c     Listed after the "XX" in each record is the radius from which
c     the coverage is valid (000 km in this case); Next is the radius
c     at which the coverage stops (100 km in this case).  Next is the
c     wind threshold (34, 50, 64).  Next is an identifier for which
c     quadrant the coverage starts in (first 2 characters are NE, SE,
c     SW, NW); the last character indicates if the coverages are
c     computed in the quadrants as earth-relative ("E") or
c     storm-motion relative ("R").  The ones listed there as "AAE"
c     are for the full disc (i.e., 4-quadrant average), earth-relative.
c     Next are the wind coverage percentages, listed as percentage * 10
c     (e.g., 981 = 98.1%).
c
c     This message also contains the intensity estimates (in knots)
c     for every forecast hours  The  conversion for m/s to knots is
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft,
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     outlon    longitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     xsfclon   low-level longitude estimate for this storm & time,
c               computed ideally from mean of mslp & low-level winds.
c     xsfclat   low-level latitude estimate for this storm & time,
c               computed ideally from mean of mslp & low-level winds.
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     wfract_cov percent areal coverage for various wind thresholds
c     wfract_type 'earth' or 'storm' relative analysis
c     pdf_ct_bin array for pdf of wind magnitudes within r=350 km
c     pdf_ct_tot total count of pdf points for r < 350 km
c
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE verbose_output

      type (datecard) inp
c
      integer, parameter :: numdist=14,numquad=4,numbin=5,numthresh=3
      real    outlon,outlat,pdfval
      real    wfract_cov(numquad+1,numbin,numthresh)
      real    vmaxwind,conv_ms_knots,xminmslp,xsfclon,xsfclat
      integer ::  windthresh(numthresh) = (/34,50,64/)
      integer pdf_ct_bin(16)
      integer intlon,intlat,output_fhr,intlon100,intlat100,pdf_ct_tot
      integer maxstorm
      character  basinid*2,clatns*1,clonew*1,wfract_type*5,wt*1,cquad*2

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        intlon100 = 0
        intlat100 = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          intlon100 = 36000 - int(outlon * 100. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          intlon100 = int(outlon * 100. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        intlat100 = int(abs(outlat) * 100. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = '**'
      end select

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

      if (wfract_type == 'earth') then
        wt = 'E'
      else if (wfract_type == 'storm') then
        wt = 'R'
      else
        wt = 'X'
      endif

      do ib = 1,numbin
        do it = 1,numthresh

          write (73,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &      ,atcfymdh
     &      ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &      ,int((vmaxwind*conv_ms_knots) + 0.5)
     &      ,int(xminmslp/100.0 + 0.5)
     &      ,', XX, ',0,ib*100,windthresh(it),'NE',wt
     &      ,int((1000.*wfract_cov(1,ib,it))+0.5)
     &      ,int((1000.*wfract_cov(2,ib,it))+0.5)
     &      ,int((1000.*wfract_cov(3,ib,it))+0.5)
     &      ,int((1000.*wfract_cov(4,ib,it))+0.5)
     &      ,intlat100,clatns,intlon100,clonew

        enddo
      enddo

      do ib = 1,numbin
        do it = 1,numthresh

          write (73,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &      ,atcfymdh
     &      ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &      ,int((vmaxwind*conv_ms_knots) + 0.5)
     &      ,int(xminmslp/100.0 + 0.5)
     &      ,', XX, ',0,ib*100,windthresh(it),'AA',wt
     &      ,int((1000.*wfract_cov(5,ib,it))+0.5)
     &      ,int((1000.*wfract_cov(5,ib,it))+0.5)
     &      ,int((1000.*wfract_cov(5,ib,it))+0.5)
     &      ,int((1000.*wfract_cov(5,ib,it))+0.5)
     &      ,intlat100,clatns,intlon100,clonew

        enddo
      enddo

   81 format (a2,', ',a2,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a6,i3.3,', ',i3.3,', '
     &       ,i3,', ',a2,a1,4(', ',i4),', ',i4,a1,', ',i5,a1)

c     --------------------------------------------------
c     Now compute and write out the pdf values for the
c     wind magnitude....
c     --------------------------------------------------

      do ip = 1,16
        pdfval = float(pdf_ct_bin(ip)) / float(pdf_ct_tot)
        write (76,85) atcfymdh,basinid,storm(ist)%tcv_storm_id(1:2)
     &               ,output_fhr,10*(ip-1),10*ip,pdf_ct_bin(ip)
     &               ,pdf_ct_tot,pdfval
      enddo

   85 format (1x,i10.10,3x,a2,a2,3x,i3,3x,i3.3,'_',i3.3,3x,i7,2x,i7
     &       ,2x,f6.3)
c
c     bug fix for IBM: flush the output stream so it actually writes
      flush(73)

      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_wind_structure (outlon,outlat,xsfclon
     &          ,xsfclat,inp,ist,ifcsthour,vmaxwind,xminmslp,er_wind
     &          ,sr_wind,er_vr,sr_vr,er_vt,sr_vt,maxstorm,iofwret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for a given
c     storm at an input forecast hour.  This message contains the
c     values of the winds at specified distances along 45-degree
c     radials in each storm quadrant.  These are  output
c     twice -- First, for an earth-relative coordinate system, and
c     second, for a storm-relative coordinate system.  For the
c     earth-relative estimates, we will always have 4 radials: NE, SE,
c     SW and NW (45,135,225,315).  For the storm-relative estimates,
c     these radials will be computed at the same relative angles (i.e.,
c     45,135,225,315), but with respect (positive clockwise) to the
c     direction of storm motion.  For example, for a storm moving with
c     a heading of 280, the wind structure is evaluated at these
c     radials: 325 (front-right; 45 deg CW from heading), 55 (back-
c     right; 135 deg CW from heading), 145 (back-left; 225 deg CW from
c     heading), 235 (front-left; 315 deg CW from heading).
c
c     LOCAL:
c       numdist   Number of discrete radii at which the winds will
c                 be evaluated
c
c
c     This format will mimic the current atcfunix format with the
c     difference coming late in the record, where the various wind radii
c     will be replaced with wind values at the 13 specified distances
c     (10, 25, 50, 75, 100, 150, 200, 250, 300, 350, 400, 450, 500 km)
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             71, NEE, 1137, 1221,  854,  655, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             71, SEE,  947,  982,  474,  396, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             71, SWE,  645,  683,  328,  277, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             71, NWE,  725,  753,  619,  429, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             72, FRR, 1134, 1224,  852,  654, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             72, BRR,  944,  984,  472,  393, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             72, BLR,  649,  686,  321,  272, etc., ... out to 500 km
c     AL, 13, 2000092500, 03, AVNO, 036, 243N,  675W,  42,  995, XX,
c             72, FLR,  729,  756,  613,  421, etc., ... out to 500 km
c
c     NOTE: Each of the above lines beginning with "AL" is output as
c           a single line of text.
c     NOTE: These winds are in m/s coming into this routine and will
c           be converted to knots*10 for output (e.g., 1221 = 122.1 kts)
c
c     The "71" ID indicates earth-relative winds, the "72" ID indicates
c     storm-relative winds.  Here are the other IDs that will be used:
c       81: Tangential winds, earth-relative (m/s)
c       82: Tangential winds, storm-relative (m/s)
c       91: Radial winds, earth-relative (m/s)
c       92: Radial winds, storm-relative (m/s)
c
c     Note that in this example, for this 36h forecast hour, there are
c     8 entries.  This is so that we can include the wind values for
c     the 4 different quadrants, for both the earth relative analyses
c     (NEE, SEE, SWE, NWE) and the storm-relative analyses (FRR, BRR,
c     BLR, FLR).
c
c     This message also contains the intensity estimates (in knots)
c     for every forecast hours  The  conversion for m/s to knots is
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft,
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     outlon    longitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     er_wind   Quadrant winds in earth-relative framework
c     sr_wind   Quadrant winds in storm-relative framework
c     er_vr     Quadrant radial winds in earth-relative framework
c     sr_vr     Quadrant radial winds in storm-relative framework
c     er_vt     Quadrant tangential winds in earth-relative framework
c     sr_vt     Quadrant tangential winds in storm-relative framework
c
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE verbose_output

      type (datecard) inp

      integer, parameter :: numdist=14,numquad=4,numbin=5,numthresh=3
      integer  ioutwind(numdist)
      real     fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real     er_wind(numquad,numdist)
      real     sr_wind(numquad,numdist)
      real     er_vr(numquad,numdist)
      real     er_vt(numquad,numdist)
      real     sr_vr(numquad,numdist)
      real     sr_vt(numquad,numdist)
      real     outlon,outlat
      real     vmaxwind,conv_ms_knots,xminmslp,xsfclon,xsfclat
      integer intlon,intlat,output_fhr,id,intlon100,intlat100,ir
      character  basinid*2,clatns*1,clonew*1,wfract_type*5,wt*1
      character*2 :: cquad(4) = (/'NE','SE','SW','NW'/)
      character*2 :: crel(4) = (/'FR','BR','BL','FL'/)


c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        intlon100 = 0
        intlat100 = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          intlon100 = 36000 - int(outlon * 100. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          intlon100 = int(outlon * 100. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        intlat100 = int(abs(outlat) * 100. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = '**'
      end select

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

c     Total wind (converted to knots*10), earth relative....

      do iq = 1,numquad
        do ir = 1,numdist
          if (er_wind(iq,ir) < -998.0) then
            ioutwind(ir) = -999
          else
            ioutwind(ir) = int((er_wind(iq,ir)*conv_ms_knots*10)+0.5)
          endif
        enddo
        write (72,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &    ,atcfymdh
     &    ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &    ,int((vmaxwind*conv_ms_knots) + 0.5)
     &    ,int(xminmslp/100.0 + 0.5)
     &    ,', XX, 71, ',cquad(iq),'E'
     &    ,(ioutwind(it),it=1,numdist)
     &    ,intlat100,clatns,intlon100,clonew
      enddo

c     Total wind (converted to knots*10), storm relative....

      do iq = 1,numquad
        do ir = 1,numdist
          if (sr_wind(iq,ir) < -998.0) then
            ioutwind(ir) = -999
          else
            ioutwind(ir) = int((sr_wind(iq,ir)*conv_ms_knots*10)+0.5)
          endif
        enddo
        write (72,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &    ,atcfymdh
     &    ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &    ,int((vmaxwind*conv_ms_knots) + 0.5)
     &    ,int(xminmslp/100.0 + 0.5)
     &    ,', XX, 72, ',crel(iq),'R'
     &    ,(ioutwind(it),it=1,numdist)
     &    ,intlat100,clatns,intlon100,clonew
      enddo

c     Tangential wind (m/s * 10), earth relative....

      do iq = 1,numquad
        do ir = 1,numdist
          if (er_vt(iq,ir) < -998.0) then
            ioutwind(ir) = -999
          else
            ioutwind(ir) = int((er_vt(iq,ir)*conv_ms_knots*10)+0.5)
          endif
        enddo
        write (72,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &    ,atcfymdh
     &    ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &    ,int((vmaxwind*conv_ms_knots) + 0.5)
     &    ,int(xminmslp/100.0 + 0.5)
     &    ,', XX, 81, ',cquad(iq),'E'
     &    ,(ioutwind(it),it=1,numdist)
     &    ,intlat100,clatns,intlon100,clonew
      enddo

c     Tangential wind (m/s * 10), storm relative....

      do iq = 1,numquad
        do ir = 1,numdist
          if (sr_vt(iq,ir) < -998.0) then
            ioutwind(ir) = -999
          else
            ioutwind(ir) = int((sr_vt(iq,ir)*conv_ms_knots*10)+0.5)
          endif
        enddo
        write (72,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &    ,atcfymdh
     &    ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &    ,int((vmaxwind*conv_ms_knots) + 0.5)
     &    ,int(xminmslp/100.0 + 0.5)
     &    ,', XX, 82, ',crel(iq),'R'
     &    ,(ioutwind(it),it=1,numdist)
     &    ,intlat100,clatns,intlon100,clonew
      enddo

c     Radial wind (m/s * 10), earth relative....

      do iq = 1,numquad
        do ir = 1,numdist
          if (er_vr(iq,ir) < -998.0) then
            ioutwind(ir) = -999
          else
            ioutwind(ir) = int((er_vr(iq,ir)*conv_ms_knots*10)+0.5)
          endif
        enddo
        write (72,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &    ,atcfymdh
     &    ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &    ,int((vmaxwind*conv_ms_knots) + 0.5)
     &    ,int(xminmslp/100.0 + 0.5)
     &    ,', XX, 91, ',cquad(iq),'E'
     &    ,(ioutwind(it),it=1,numdist)
     &    ,intlat100,clatns,intlon100,clonew
      enddo

c     Radial wind (m/s * 10), storm relative....

      do iq = 1,numquad
        do ir = 1,numdist
          if (sr_vr(iq,ir) < -998.0) then
            ioutwind(ir) = -999
          else
            ioutwind(ir) = int((sr_vr(iq,ir)*conv_ms_knots*10)+0.5)
          endif
        enddo
        write (72,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &    ,atcfymdh
     &    ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &    ,int((vmaxwind*conv_ms_knots) + 0.5)
     &    ,int(xminmslp/100.0 + 0.5)
     &    ,', XX, 92, ',crel(iq),'R'
     &    ,(ioutwind(it),it=1,numdist)
     &    ,intlat100,clatns,intlon100,clonew
      enddo
c
   81 format (a2,', ',a2,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,a10,a2,a1,14(', ',i4)
     &       ,', ',i4,a1,', ',i5,a1)

c     bug fix for IBM: flush the output stream so it actually writes
      flush(72)

      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_ike (outlon,outlat,xsfclon,xsfclat,inp,ist
     &          ,ifcsthour,vmaxwind,xminmslp,ike,sdp,wdp,maxstorm
     &          ,ioiret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for a given
c     storm at an input forecast hour.  This message contains the values
c     for the Integrated Kinetic Energy (IKE) and Storm Surge Damage
c     Potential (SDP), based on Powell (BAMS, 2007).  At this time, we
c     are only computing the IKE values for TS threshold (17.5 m/s) and
c     above.  We are not yet computing wind damage potential (WDP)
c     since, per Mark Powell (4/2008), he is currently re-formulating
c     an algorithm for it.
c
c     LOCAL:
c
c     Arrays:
c
c     ike   Integrated kinetic energy:
c           ike(1) = IKE_10m/s  (storm energy)
c           ike(2) = IKE_18m/s  (IKE_ts, tropical storm)
c           ike(3) = IKE_33m/s  (IKE_h,  hurricane)
c           ike(4) = IKE_25_40 m/s  (Not currently computed)
c           ike(5) = IKE_41_54 m/s  (Not currently computed)
c           ike(6) = IKE_55 m/s     (Not currently computed)
c
c
c     The format used will mimic the current atcfunix format with the
c     difference coming late in the record, where the various wind radii
c     will be replaced with WDP, SDP and IKE values:
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  91,
c             IKE,  340,  560,  212,  174,   42,   93,   12,    0
c
c     Where the places are identified as follows:
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  91,
c             IKE,  WDP,  SDP,  I10,  ITS,  IH ,I2540,I4154,  I55
c
c     (NOTE: Each of the above lines beginning with "AL" is output as
c            a single line of text.)
c
c     Values for WDP and SDP are multiplied by 10 in this routine
c     before being written out.
c
c     This message also contains the intensity estimates (in knots)
c     for every forecast hour.  The  conversion for m/s to knots is
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft,
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     storm     An array of type tcvcard.  Use this for the storm ID
c     outlon    longitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     xsfclon   low-level longitude estimate for this storm & time,
c               computed ideally from mean of mslp & low-level winds.
c     xsfclat   low-level latitude estimate for this storm & time,
c               computed ideally from mean of mslp & low-level winds.
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     ike       integrated kinetic energy, in units of TJ
c     sdp       storm surge damage potential
c     wdp       wind damage potential
c
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE verbose_output

      type (datecard) inp
c
      integer, parameter :: numdist=14,numquad=4,numbin=5,numthresh=3
      real    outlon,outlat,sdp,wdp
      real    ike(max_ike_cats)
      real    vmaxwind,conv_ms_knots,xminmslp,xsfclon,xsfclat
      integer intlon,intlat,output_fhr,intlon100,intlat100,maxstorm
      character  basinid*2,clatns*1,clonew*1,wfract_type*5,wt*1,cquad*2

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        intlon100 = 0
        intlat100 = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          intlon100 = 36000 - int(outlon * 100. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          intlon100 = int(outlon * 100. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        intlat100 = int(abs(outlat) * 100. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = '**'
      end select

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

      write (74,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &  ,atcfymdh
     &  ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &  ,int((vmaxwind*conv_ms_knots) + 0.5)
     &  ,int(xminmslp/100.0 + 0.5)
     &  ,', XX,  91, IKE',int((wdp*10)+0.5),int((sdp*10)+0.5)
     &  ,int(ike(1)+0.5),int(ike(2)+0.5),int(ike(3)+0.5)
     &  ,int(ike(4)+0.5),int(ike(5)+0.5),int(ike(6)+0.5)
     &  ,intlat100,clatns,intlon100,clonew
c
   81 format (a2,', ',a2,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,a14,8(',',i5)
     &       ,', ',i4,a1,', ',i5,a1)

c     bug fix for IBM: flush the output stream so it actually writes
      flush(74)

      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_phase (outlon,outlat,inp,ist
     &          ,ifcsthour,vmaxwind,xminmslp,paramb,vtl_slope
     &          ,vtu_slope,ioiret)
c
c     ABSTRACT: This subroutine  outputs a 1-line message for a given
c     storm at an input forecast hour.  This message contains the values
c     for the three parameters that comprise Bob Hart's cyclone phase
c     space (CPS).  These parameters are his "parameter B", which
c     assesses the left-right thermal asymmetry, and the upper
c     troposphere (300-600 mb) and lower troposphere (900-600 mb)
c     thermal wind values.
c
c     LOCAL:
c
c     Arrays:
c
c     The format used will mimic the current atcfunix format with the
c     difference coming late in the record, where the various wind radii
c     will be replaced with paramb, vtl_slope and vtu_slope values:
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  95,
c             CPS,   340,   560,   212
c
c     Where the places are identified as follows:
c
c     AL, 13, 2000092500, 03, AVNO, 036, 243N, 675W, 42, 995, XX,  95,
c             CPS,     B,   VTL,   VTU
c
c     (NOTE: Each of the above lines beginning with "AL" is output as
c            a single line of text.)
c
c     This message also contains the intensity estimates (in knots)
c     for every forecast hour.  The  conversion for m/s to knots is
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft,
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     storm     An array of type tcvcard.  Use this for the storm ID
c     outlon    longitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     paramb    thermal asymmetry
c     vtl_slope thermal wind value for lower troposphere (900-600 mb)
c     vtu_slope thermal wind value for upper troposphere (600-300 mb)
c
c     OUTPUT:
c     ioiret    integer return code from this subroutine
c
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE verbose_output

      type (datecard) inp

      real    outlon,outlat,paramb,vtl_slope,vtu_slope
      real    vmaxwind,conv_ms_knots,xminmslp
      integer intlon,intlat,output_fhr
      character  basinid*2,clatns*1,clonew*1

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = '**'
      end select

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

      write (71,81) basinid,storm(ist)%tcv_storm_id(1:2)
     &  ,atcfymdh
     &  ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &  ,int((vmaxwind*conv_ms_knots) + 0.5)
     &  ,int(xminmslp/100.0 + 0.5)
     &  ,', XX,  95, CPS',int(paramb+0.5),int(vtl_slope+0.5)
     &  ,int(vtu_slope+0.5)
c
   81 format (a2,', ',a2,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a14,3(',',i6))

c     bug fix for IBM: flush the output stream so it actually writes
      flush(71)

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_atcf_gen (outlon,outlat,inp,ist
     &         ,ifcsthour,vmaxwind,xminmslp,vradius,maxstorm
     &         ,trkrinfo,istmspd,istmdir,plastbar,rlastbar,rmax
     &         ,cps_vals,wcore_flag,imeanzeta,igridzeta,ioaxret)

c     ABSTRACT: This subroutine  outputs a 1-line message for a given 
c     storm at an input forecast hour in a modified atcfunix format.  
c     The reason that it's called "modified" is that the format is 
c     slightly different from the standard TPC-accepted atcfunix 
c     format that they use for TCs.  Specifically, the first part that
c     identifies the storm is different.  Here's an example of the 
c     TPC standard atcfunix format:
c
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  34,
c             NEQ,  242,  163,  124,  208
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  50,
c             NEQ,  155,  000,  000,  000
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  64,
c             NEQ,  000,  000,  000,  000
c
c     (NOTE: Each of the above lines beginning with "AL" is output as 
c            a single line of text.... they're just broken up into 2 
c            lines here for readability.)
c
c     Here's an example of the modified output format for the same 
c     storm.  Note that the lat/lon identifier in the new storm id at
c     the beginning of the record is different from that shown later
c     in the record.  The reason is that the lat/lon identifier will
c     be the one that is pulled from the tcvitals or gen_vitals 
c     record:
c
c     2000092500_230N_0658W_13L, 2000092500, 03, GFSO, 036, 243N, 675W,
c             42, 995, XX,  34, NEQ,  242,  163,  124,  208
c     2000092500_230N_0658W_13L, 2000092500, 03, GFSO, 036, 243N, 675W,
c             42, 995, XX,  50, NEQ,  155,  000,  000,  000
c     2000092500_230N_0658W_13L, 2000092500, 03, GFSO, 036, 243N, 675W,
c             42, 995, XX,  64, NEQ,  000,  000,  000,  000
c
c
c     Note that in this example, for this 36h forecast hour, there are 
c     3 entries.  This is so that we can include the radii for the 
c     3 different wind thresholds (34kt, 50kt and 64kt).  So the only
c     thing different in each entry is the wind radii info;  all the
c     other info is identical for each entry.
c
c     This message also contains the intensity estimates (in knots) 
c     for every forecast hours  The  conversion for m/s to knots is 
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft, 
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the 
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and 
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     outlon    longitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c     maxstorm  max # of storms that can be handled
c     istmspd   storm translation speed
c     istmdir   direction of storm movement
c     plastbar  pressure of last closed isobar
c     rlastbar  radius of last closed isobar
c     rmax      radius of max winds
c     cps_vals  Hart's cyclone phase space values: (1) is for parameter
c               B (thickness asymmetry), (2) and (3) are for thermal
c               wind values.
c     wcore_flag 'u'=undetermined, 'y'=yes, 'n'=no
c     imeanzeta array with values of mean 850 & 700 zeta
c     igridzeta array with values of max (gridpoint) 850 & 700 zeta
c 
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c     
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c     storm     An array of type tcvcard.  Use this for the storm ID
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE trkrparms; USE gen_vitals; USE level_parms
      USE verbose_output

      type (gencard) gstm
      type (datecard) inp
      type (trackstuff) trkrinfo
c
      real    outlon,outlat,plastbar,rlastbar,rmax
      real    vmaxwind,conv_ms_knots,xminmslp,mslp_outp_adj
      real    cps_vals(3)
      integer intlon,intlat,istmspd,istmdir,iplastbar,irlastbar,irmax
      integer ivtl,ivtu,iparamb,output_fhr
      integer vradius(3,4)
      integer imeanzeta(nlevgrzeta),igridzeta(nlevgrzeta)
      character  basinid*2,clatns*1,clonew*1,wcore_flag*1

      if ( verb .ge. 3) then
        print *,'+++ Top of output_atcf_gen, ist= ',ist,' ifh= '
     &         ,ifcsthour
      endif


      if (xminmslp == 999999.0) xminmslp = 0.0

      if (xminmslp < 1100.0) then
        ! Pressure units are in mb...
        mslp_outp_adj = 1.0
      elseif (xminmslp >80000.0) then
        ! Pressure units are in Pa...
        mslp_outp_adj = 100.0
      else
        if (verb .ge. 3) then
          print *,' '
          print *,'ERROR: Something wrong in subroutine'
          print *,'       output_atcf_gen.  The mslp value'
          print *,'       (xminmslp) is not in range.'
          print *,'       xminmslp = ',xminmslp
          print *,'       EXITING....'
          print *,' '
          stop 95
        endif
      endif

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

c     Unlike the regular atcfunix output, in which we  output a record
c     at forecast time = 00h even if the storm cannot be found, here
c     we don't want to do that.  So check the lat & lon positions and
c     exit this subroutine now if they're both zero.

      if (intlat == 0 .and. intlon == 0) then
        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ Currently inside  output_atcf_gen.  The reported'
          print *,'+++ longitude and latitude are both zero, so that '
          print *,'+++ means that the  tracker could not get a fix '
          print *,'+++ for this storm at this hour.  Therefore, we will'
          print *,'+++ NOT write out an atcf_gen record for this'
          print *,'+++ storm & forecast hour.'
          print *,'+++ '
          print *,'+++ ist= ',ist
          print *,'+++ gstorm= ',gstorm(ist)
          print *,' '
        endif

        return
      endif

c     Initially, set all "gstm" components equal to the input "gstorm"
c     components for this storm, then we will change the specific
c     components that we need to.

      gstm = gstorm(ist)

c     If the "gv_gen_date" for this storm does not equal 99999,
c     then that means that a vitals was read in for this storm in
c     subroutine  read_gen_vitals, so be sure to use the genesis
c     date, genesis latitude and genesis longitude for the storm
c     identifier at the beginning of the modified atcfunix record.

      if (gstm%gv_gen_date /= 99999) then

        continue    ! Just use the info off the genesis vitals record

      else
          
        ! This storm was found on the fly during
        ! this run and there was no previous vitals record for
        ! this system.  The information that will be used to 
        ! identify the genesis location is the same exact info
        ! as the  tracker-found position for this time.

        gstm%gv_gen_date = inp%bcc * 100000000
     &                   + inp%byy * 1000000
     &                   + inp%bmm * 10000
     &                   + inp%bdd * 100
     &                   + inp%bhh

        gstm%gv_gen_fhr   = ifcsthour
        gstm%gv_gen_lat   = intlat
        gstm%gv_gen_latns = clatns
        gstm%gv_gen_lon   = intlon
        gstm%gv_gen_lonew = clonew
        gstm%gv_gen_type  = 'FOF'

      endif

      if (plastbar > -990.0) then
        iplastbar = int(plastbar/mslp_outp_adj + 0.5)
      else
        iplastbar = -999
      endif

      if (rlastbar > -990.0) then
        irlastbar = int(rlastbar + 0.5)
      else
        irlastbar = -999
      endif

      if (rmax > -90.0) then
        irmax = int(rmax + 0.5)
      else
        irmax = -99
      endif

      if (cps_vals(1) > -9999.0) then
        if (cps_vals(1) >= 0.0) then
          iparamb = int(cps_vals(1)*10 + 0.5)
        else
          iparamb = int(cps_vals(1)*10 - 0.5)
        endif
      else
        iparamb = -999
      endif

      if (cps_vals(2) > -9999.0) then
        if (cps_vals(2) >= 0.0) then
          ivtl = int(cps_vals(2)*10 + 0.5)
        else
          ivtl = int(cps_vals(2)*10 - 0.5)
        endif
      else
        ivtl = -9999
      endif

      if (cps_vals(3) > -9999.0) then
        if (cps_vals(3) >= 0.0) then
          ivtu = int(cps_vals(3)*10 + 0.5)
        else
          ivtu = int(cps_vals(3)*10 - 0.5)
        endif
      else
        ivtu = -9999
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = 'HC'
      end select

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

      if (stcvtype(ist) == 'FOF') then
        ! If this is a TC vitals-described storm (i.e., one that is
        ! numbered by JTWC or NHC), then leave the basinid as is.
        ! Otherwise, we want to use the "basinid" location as a
        ! label to identify what type of run this is.
        if (trkrinfo%type == 'midlat') basinid = 'ML'
        if (trkrinfo%type == 'tcgen')  basinid = 'TG'
      endif

      write (66,87) basinid,adjustr(storm(ist)%tcv_storm_id)
     &      ,gstm%gv_gen_date,gstm%gv_gen_fhr,gstm%gv_gen_lat
     &      ,gstm%gv_gen_latns,gstm%gv_gen_lon
     &      ,gstm%gv_gen_lonew,gstm%gv_gen_type
     &      ,atcfymdh
     &      ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &      ,int((vmaxwind*conv_ms_knots) + 0.5)
     &      ,int(xminmslp/mslp_outp_adj + 0.5)
     &      ,'XX,  34, NEQ'
     &      ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &      ,iplastbar,irlastbar,irmax,iparamb,ivtl,ivtu,wcore_flag
     &      ,istmdir,istmspd
     &      ,imeanzeta(1),igridzeta(1),imeanzeta(2),igridzeta(2)

      if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &    vradius(2,3) > 0 .or. vradius(2,4) > 0) then
      write (66,87) basinid,adjustr(storm(ist)%tcv_storm_id)
     &        ,gstm%gv_gen_date,gstm%gv_gen_fhr,gstm%gv_gen_lat
     &        ,gstm%gv_gen_latns,gstm%gv_gen_lon
     &        ,gstm%gv_gen_lonew,gstm%gv_gen_type
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)                   
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  50, NEQ'
     &        ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &        ,iplastbar,irlastbar,irmax,iparamb,ivtl,ivtu,wcore_flag
     &        ,istmdir,istmspd
     &        ,imeanzeta(1),igridzeta(1),imeanzeta(2),igridzeta(2)
      endif

      if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &    vradius(3,3) > 0 .or. vradius(3,4) > 0) then
      write (66,87) basinid,adjustr(storm(ist)%tcv_storm_id)
     &        ,gstm%gv_gen_date,gstm%gv_gen_fhr,gstm%gv_gen_lat
     &        ,gstm%gv_gen_latns,gstm%gv_gen_lon
     &        ,gstm%gv_gen_lonew,gstm%gv_gen_type
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)                   
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  64, NEQ'
     &        ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &        ,iplastbar,irlastbar,irmax,iparamb,ivtl,ivtu,wcore_flag
     &        ,istmdir,istmspd
     &        ,imeanzeta(1),igridzeta(1),imeanzeta(2),igridzeta(2)
      endif

   87 format (a2,', ',a4,', ',i10.10,'_F',i3.3,'_',i3.3,a1,'_',i4.4,a1
     &       ,'_',a3,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
     &       ,', ',3(i4,', '),3(i6,', '),a1,2(', ',i4),4(', ',i4))

c     bug fix for IBM: flush the output stream so it actually writes
      flush(66)

      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_atcf_sink (outlon,outlat,inp,ist
     &         ,ifcsthour,vmaxwind,xminmslp,vradius,maxstorm
     &         ,trkrinfo,istmspd,istmdir,imeanzeta,igridzeta
     &         ,cps_vals,plastbar,rlastbar,ioaxret)

c     ABSTRACT: This subroutine  outputs a 1-line message for a given 
c     storm at an input forecast hour in a modified atcfunix format.  
c     The "sink" in the subroutine name indicates that this output
c     contains the whole kitchen sink of forecast storm info.
c     The reason that it's called "modified" is that the format is 
c     slightly different from the standard TPC-accepted atcfunix 
c     format that they use for TCs.  Specifically, the first part that
c     identifies the storm is different, and the part after the radii
c     data is different.  Here's an example of the TPC standard 
c     atcfunix format:
c
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  34,
c             NEQ,  242,  163,  124,  208
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  50,
c             NEQ,  155,  000,  000,  000
c     AL, 13, 2000092500, 03, GFSO, 036, 243N, 675W, 42, 995, XX,  64,
c             NEQ,  000,  000,  000,  000
c
c     (NOTE: Each of the above lines beginning with "AL" is output as 
c            a single line of text.... they're just broken up into 2 
c            lines here for readability.)
c
c     Here's an example of the modified output format for the same 
c     storm.  Note that the lat/lon identifier in the new storm id at
c     the beginning of the record is different from that shown later
c     in the record.  The reason is that the lat/lon identifier will
c     indicate the lat/lon at which the storm was *first* found in
c     the model.  The position may be either found within this run 
c     of the  tracker, or that position may have been pulled from the
c     tcvitals or gen_vitals record:
c
c     2000092500_F000_206N_0623W_13L, 2000092500, 03, GFSO, 036
c       , 243N, 675W,  42, 995, XX,  34, NEQ,  242,  163,  124,  208
c       , PLAS, RLAS, RMX, DIR, SPD,   B,   VTU,   VTL
c       , Z8MN, Z8MX, Z7MN, Z7MX
c
c     As noted above, there is extra info at the end, after the 
c     "34, NEQ,  242,  163,  124,  208" radii info.  Here is a key 
c     to indicate what these items are:
c
c     PLAS:  Pressure (mb) of last closed isobar
c     RLAS:  Radius of the last closed isobar in nm, 0 - 9999 nm.
c     RMX:   Radius of max winds, 0 - 999 nm.
c     DIR:   Direction of storm motion.
c     SPD:   Speed of storm motion (m/s * 10).
c     B:     Hart's CPS "Parameter B" thickness asymmetry value (m).
c     VTL:   Hart's CPS thermal wind (Lower, 900-600) value.
c     VTU:   Hart's CPS thermal wind (Upper, 600-300) value.
c     Z8MN:  Mean value of 850 mb zeta surrounding storm.
c     Z8MX:  Max value of 850 mb zeta near storm.
c     Z7MN:  Mean value of 700 mb zeta surrounding storm.
c     Z7MX:  Max value of 700 mb zeta near storm.
c
c     This message also contains the intensity estimates (in knots) 
c     for every forecast hour.  The  conversion for m/s to knots is 
c     to multiply m/s by 1.9427 (3.281 ft/m, 1 naut mile/6080 ft, 
c     3600s/h).
c
c     NOTE: The longitudes that are passed into this subroutine are
c     given in 0 - 360, increasing eastward.  The format for the 
c     atcfunix system requires that the  output be 0-180E or
c     0-180W, so we must adjust the values, if needed.  Also, the
c     values for southern latitudes must be positive (use 'N' and 
c     'S' to distinguish Northern/Southern Hemispheres).
c
c     INPUT:
c     outlon    longitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     outlat    latitude  fix position for this storm at this time 
c               which is to be written out to the  output file
c     inp       contains input date and model number information
c     ist       the number storm that we're processing (can be 1-15)
c     ifcsthr   the current forecast hour being output
c     vmaxwind  the max surface wind for this storm at this fcst hour
c     xminmslp  the min mslp for this storm at this fcst hour
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c     maxstorm  max # of storms that can be handled
c     istmspd   speed of storm translation
c     istmdir   direction of storm motion
c     cps_vals  Hart's cyclone phase space values: (1) is for parameter
c               B (thickness asymmetry), (2) and (3) are for thermal
c               wind values.
c     imeanzeta array with values of mean 850 & 700 zeta
c     igridzeta array with values of max (gridpoint) 850 & 700 zeta
c     plastbar  pressure of last closed isobar (pa)
c     rlastbar  radius of last closed isobar (nm)
c 
c     OUTPUT:
c     ioaxret   integer return code from this subroutine
c     
c     LOCAL:
c     intlon    integer that holds the value of outlon*10
c     intlat    integer that holds the value of outlat*10
c     storm     An array of type tcvcard.  Use this for the storm ID
c

      USE def_vitals; USE inparms; USE set_max_parms; USE atcf
      USE trkrparms; USE gen_vitals
      USE verbose_output

      type (gencard) gstm
      type (datecard) inp
      type (trackstuff) trkrinfo
c
      real    cps_vals(3)
      real    outlon,outlat,mslp_outp_adj
      real    vmaxwind,conv_ms_knots,xminmslp,plastbar,rlastbar
      integer intlon,intlat,istmspd,istmdir,iplastbar,irlastbar
      integer iparamb,ivtl,ivtu,output_fhr
      integer vradius(3,4)
      integer imeanzeta(2),igridzeta(2)
      character  basinid*2,clatns*1,clonew*1

      if ( verb .ge. 3 ) then
        print *,'+++ Top of output_atcf_sink, ist= ',ist,' ifh= '
     &       ,ifcsthour
      endif

      if (xminmslp == 999999.0) xminmslp = 0.0

      if (xminmslp < 1100.0) then
        ! Pressure units are in mb...
        mslp_outp_adj = 1.0
      elseif (xminmslp >80000.0) then
        ! Pressure units are in Pa...
        mslp_outp_adj = 100.0
      else
        if (verb .ge. 3) then
          print *,' '
          print *,'ERROR: Something wrong in subroutine'
          print *,'       output_atcf_gen.  The mslp value'
          print *,'       (xminmslp) is not in range.'
          print *,'       xminmslp = ',xminmslp
          print *,'       EXITING....'
          print *,' '
          stop 95
        endif
      endif

c     First convert all of the lat/lon values from reals into integers.
c     These integer values must be 10x their real value (eg. 125.4 will
c     be written out as 1254).  Convert the lon values so that they go
c     from 0-180E or 0-180W, and convert the lat values so that they are
c     positive and use 'N' or 'S' to differentiate hemispheres.

      conv_ms_knots = 1.9427

      if (outlon < -998.0 .or. outlat < -998.0) then
        intlon = 0
        intlat = 0
        clonew = ' '
        clatns = ' '
      else
        if (outlon >= 180.0) then
          intlon = 3600 - int(outlon * 10. + 0.5)
          clonew = 'W'
        else
          intlon = int(outlon * 10. + 0.5)
          clonew = 'E'
        endif
        intlat = int(abs(outlat) * 10. + 0.5)
        if (outlat < 0.0) then
          clatns = 'S'
        else
          clatns = 'N'
        endif
      endif

      select case (storm(ist)%tcv_storm_id(3:3))
        case ('L','l');  basinid = 'AL'
        case ('E','e');  basinid = 'EP'
        case ('C','c');  basinid = 'CP'
        case ('W','w');  basinid = 'WP'
        case ('O','o');  basinid = 'SC'
        case ('T','t');  basinid = 'EC'
        case ('U','u');  basinid = 'AU'
        case ('P','p');  basinid = 'SP'
        case ('S','s');  basinid = 'SI'
        case ('B','b');  basinid = 'BB'
        case ('A','a');  basinid = 'AA'
        case ('Q','q');  basinid = 'SL'
        case default;    basinid = 'HC'
      end select

      if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') then

        if (stcvtype(ist) == 'FOF') then
          ! If this is a TC vitals-described storm (i.e., one that is
          ! numbered by JTWC or NHC), then leave the basinid as is.
          ! Otherwise, we want to use the "basinid" location as a 
          ! label to identify what type of run this is.
          if (trkrinfo%type == 'midlat') basinid = 'ML'
          if (trkrinfo%type == 'tcgen')  basinid = 'TG'
        endif
      endif

c     Unlike the regular atcfunix output, in which we  output a record
c     at forecast time = 00h even if the storm cannot be found, here
c     we don't want to do that.  So check the lat & lon positions and
c     exit this subroutine now if they're both zero.

      if (intlat == 0 .and. intlon == 0) then
        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ Currently inside  output_atcf_gen.  The reported'
          print *,'+++ longitude and latitude are both zero, so that '
          print *,'+++ means that the  tracker could not get a fix '
          print *,'+++ for this storm at this hour.  Therefore, we will'
          print *,'+++ NOT write out an atcf_gen record for this'
          print *,'+++ storm & forecast hour.'
          print *,'+++ '
          print *,'+++ ist= ',ist
          print *,'+++ gstorm= ',gstorm(ist)
          print *,' '
        endif

        return
      endif

c     Initially, set all "gstm" components equal to the input "gstorm"
c     components for this storm, then we will change the specific
c     components that we need to.

      gstm = gstorm(ist)

c     If the "gv_gen_date" for this storm does not equal 99999,
c     then that means that a vitals was read in for this storm in
c     subroutine  read_gen_vitals, so be sure to use the genesis
c     date, genesis latitude and genesis longitude for the storm
c     identifier at the beginning of the modified atcfunix record.

      if (gstm%gv_gen_date /= 99999) then

        continue    ! Just use the info off the genesis vitals record

      else
          
        ! This storm was found on the fly during
        ! this run and there was no previous vitals record for
        ! this system.  The information that will be used to 
        ! identify the genesis location is the same exact info
        ! as the  tracker-found position for this time.

        gstm%gv_gen_date = inp%bcc * 100000000
     &                   + inp%byy * 1000000
     &                   + inp%bmm * 10000
     &                   + inp%bdd * 100
     &                   + inp%bhh

        gstm%gv_gen_fhr   = ifcsthour
        gstm%gv_gen_lat   = intlat
        gstm%gv_gen_latns = clatns
        gstm%gv_gen_lon   = intlon
        gstm%gv_gen_lonew = clonew
        gstm%gv_gen_type  = 'FOF'

        ! Transfer all this local "gstm" data back into the 
        ! saved "gstorm" array for use in subsequent fcst hrs...

        gstorm(ist) = gstm

      endif

      if (plastbar > -990.0) then
        iplastbar = int(plastbar/mslp_outp_adj + 0.5)
      else
        iplastbar = -999
      endif

      if (rlastbar > -990.0) then
        irlastbar = int(rlastbar + 0.5)
      else
        irlastbar = -999
      endif

      if (cps_vals(1) > -9999.0) then
        if (cps_vals(1) >= 0.0) then
          iparamb = int(cps_vals(1)*10 + 0.5)
        else
          iparamb = int(cps_vals(1)*10 - 0.5)
        endif
      else
        iparamb = -999
      endif

      if (cps_vals(2) > -9999.0) then
        if (cps_vals(2) >= 0.0) then
          ivtl = int(cps_vals(2)*10 + 0.5)
        else
          ivtl = int(cps_vals(2)*10 - 0.5)
        endif
      else
        ivtl = -9999
      endif

      if (cps_vals(3) > -9999.0) then
        if (cps_vals(3) >= 0.0) then
          ivtu = int(cps_vals(3)*10 + 0.5)
        else
          ivtu = int(cps_vals(3)*10 - 0.5)
        endif
      else
        ivtu = -9999
      endif

      if (atcfname(1:2) == 'SP') then
        ! Add 3 for SREF to account for the 3-hour off-synoptic
        ! time offset....
        output_fhr = ifcsthour + 3
      else
        output_fhr = ifcsthour
      endif

      write (68,87) basinid,storm(ist)%tcv_storm_id
     &      ,gstm%gv_gen_date,gstm%gv_gen_fhr,gstm%gv_gen_lat
     &      ,gstm%gv_gen_latns,gstm%gv_gen_lon
     &      ,gstm%gv_gen_lonew,gstm%gv_gen_type
     &      ,atcfymdh
     &      ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &      ,int((vmaxwind*conv_ms_knots) + 0.5)
     &      ,int(xminmslp/mslp_outp_adj + 0.5)
     &      ,'XX,  34, NEQ'
     &      ,vradius(1,1),vradius(1,2),vradius(1,3),vradius(1,4)
     &      ,iplastbar,irlastbar,-99,istmdir,istmspd
     &      ,iparamb,ivtl,ivtu
     &      ,imeanzeta(1),igridzeta(1),imeanzeta(2),igridzeta(2)
     &      ,storm(ist)%tcv_storm_name

      if (vradius(2,1) > 0 .or. vradius(2,2) > 0 .or.
     &    vradius(2,3) > 0 .or. vradius(2,4) > 0) then
        write (68,87) basinid,storm(ist)%tcv_storm_id
     &        ,gstm%gv_gen_date,gstm%gv_gen_fhr,gstm%gv_gen_lat
     &        ,gstm%gv_gen_latns,gstm%gv_gen_lon
     &        ,gstm%gv_gen_lonew,gstm%gv_gen_type
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  50, NEQ'
     &        ,vradius(2,1),vradius(2,2),vradius(2,3),vradius(2,4)
     &        ,iplastbar,irlastbar,-99,istmdir,istmspd
     &        ,iparamb,ivtl,ivtu
     &        ,imeanzeta(1),igridzeta(1),imeanzeta(2),igridzeta(2)
     &        ,storm(ist)%tcv_storm_name
      endif

      if (vradius(3,1) > 0 .or. vradius(3,2) > 0 .or.
     &    vradius(3,3) > 0 .or. vradius(3,4) > 0) then
        write (68,87) basinid,storm(ist)%tcv_storm_id
     &        ,gstm%gv_gen_date,gstm%gv_gen_fhr,gstm%gv_gen_lat
     &        ,gstm%gv_gen_latns,gstm%gv_gen_lon
     &        ,gstm%gv_gen_lonew,gstm%gv_gen_type
     &        ,atcfymdh
     &        ,adjustr(atcfname),output_fhr,intlat,clatns,intlon,clonew
     &        ,int((vmaxwind*conv_ms_knots) + 0.5)
     &        ,int(xminmslp/mslp_outp_adj + 0.5)
     &        ,'XX,  64, NEQ'
     &        ,vradius(3,1),vradius(3,2),vradius(3,3),vradius(3,4)
     &        ,iplastbar,irlastbar,-99,istmdir,istmspd
     &        ,iparamb,ivtl,ivtu
     &        ,imeanzeta(1),igridzeta(1),imeanzeta(2),igridzeta(2)
     &        ,storm(ist)%tcv_storm_name
      endif

c   87 format (i10.10,'_',i3.3,a1,'_',i4.4,a1,'_',a3,', ',5i2.2
c     &       ,', 03, ',a4,', ',i3.3,', ',i3,a1
c     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4))

c   87 format (a2,', ',a4,', ',i10.10,'_F',i3.3,'_',i3.3,a1,'_',i4.4,a1
c     &       ,'_',a3,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
c     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
c     &       ,', ',2(i4,', '),4(i3,', '),2(i5,', '),4(i4,', '),a9)

   87 format (a2,', ',a4,', ',i10.10,'_F',i3.3,'_',i3.3,a1,'_',i4.4,a1
     &       ,'_',a3,', ',i10.10,', 03, ',a4,', ',i3.3,', ',i3,a1
     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,4(', ',i4.4)
     &       ,', ',2(i4,', '),i3,', ',2(i4,', '),3(i6,', '),4(i6,', ')
     &       ,a9)

c      write (68,87) gstm%gv_gen_date,gstm%gv_gen_lat
c     &      ,gstm%gv_gen_latns,gstm%gv_gen_lon
c     &      ,gstm%gv_gen_lonew,gstm%gv_gen_type
c     &      ,inp%bcc,inp%byy,inp%bmm,inp%bdd,inp%bhh
c     &      ,adjustr(atcfname),ifcsthour,intlat,clatns,intlon,clonew
c     &      ,int((vmaxwind*conv_ms_knots) + 0.5)
c     &      ,int(xminmslp/100.0 + 0.5)
c     &      ,'XX,  34, NEQ'
c     &      ,istmspd,istmdir,imeanzeta(1),igridzeta(1)
c     &      ,imeanzeta(2),igridzeta(2)
c
c   87 format (i10.10,'_',i3.3,a1,'_',i4.4,a1,'_',a3,', ',5i2.2
c     &       ,', 03, ',a4,', ',i3.3,', ',i3,a1
c     &       ,', ',i4,a1,', ',i3,', ',i4,', ',a12,6(', ',i4))

c     bug fix for IBM: flush the output stream so it actually writes
      flush(68)

      return
      end

c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine output_tcvitals (xlon,xlat,inp,ist,iovret)
c
c     ABSTRACT: This subroutine  outputs a tcvitals record.  The
c     lat/lon location is given by the xlon and xlat that are
c     input to this subroutine.
c
c     INPUT:
c     xlon   longitude of storm position to be  output
c     xlat   latitude of storm position to be  output
c     inp    contains input date and model number information
c     ist    the number storm that we're processing (can be 1-15)
c
c     OUTPUT:
c     iovret return code from this subroutine
c
c     OTHER:
c     storm  contains the tcvitals info (from module def_vitals)
c
      USE def_vitals; USE inparms; USE set_max_parms
      USE verbose_output

      type (tcvcard) stm
      type (datecard) inp
      real       xlon,xlat
c
      iovret = 0

c     Initially, set all "stm" components equal to the input "storm"
c     components for this storm, then we will change the specific
c     components that we need to.

      stm = storm(ist)

      stm%tcv_center = 'AEAR'

      stm%tcv_lat = int(abs(xlat) * 10. + 0.5)
      if (xlat < 0.0) then
        stm%tcv_latns = 'S'
      else
        stm%tcv_latns = 'N'
      endif

      if (xlon >= 180.) then
        stm%tcv_lon = 3600 - int(xlon * 10. + 0.5)
        stm%tcv_lonew = 'W'
      else
        stm%tcv_lon = int(xlon * 10. + 0.5)
        stm%tcv_lonew = 'E'
      endif
      
      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,21) stm
      endif

      write (65,21) stm
      
   21 format (a4,1x,a3,1x,a9,1x,i8.8,1x,i4.4,1x,i3,a1,1x,i4,a1,1x
     &       ,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)
      
c     
c     bug fix for IBM: flush the output stream so it actually writes
      flush(65)

      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine output_gen_vitals (xlon,xlat,inp,ist,istmspd,istmdir
     &                             ,iovret)
c
c     ABSTRACT: This subroutine  outputs a modified vitals record.  
c     The lat/lon location is given by the xlon and xlat that are
c     input to this subroutine.
c
c     The reason that these are referred to as modified tcvitals is
c     that the format is different from standard TC vitals format.
c     The storm identifier is different than that for a standard 
c     tcvitals.  The storm identifier contains the date/time that 
c     the storm was first identified, and the lat/lon position at
c     which it was first identified.
c
c     EXAMPLE:  The following is a standard TC Vitals record, split
c               up over 3 lines:           
c
c       NHC  01L ALBERTO   20060614 1200 343N 0807W 035 093 1004 1012
c            0278 15 222 -999 -999 -999 -999 M -999 -999 -999 -999 72
c            520N  410W  -999 -999 -999 -999
c
c     EXAMPLE:  The following is the format for the "genesis" vitals,
c               split over 3 lines, for the same system:
c
c       2006061000_F000_210N_0853W_01L 20060614 1200 343N 0807W 035 093
c            1004 1012 0278 15 222 -999 -999 -999 -999 M -999 -999
c            -999 -999 72 520N  410W  -999 -999 -999 -999
c
c     EXAMPLE:  If the vitals record is for a non-officially numbered
c               system (i.e., any system that's not a TC being tracked
c               by NHC or JTWC), then the storm number is replaced
c               by the characters "FOF", for "Found On the Fly" by
c               the  tracker.               
c                                          
c       2006071500_F000_150N_0681W_FOF 20060718 1200 185N 0792W 035 093
c            1004 1012 0278 15 222 -999 -999 -999 -999 M -999 -999
c            -999 -999 72 520N  410W  -999 -999 -999 -999
c
c
c     INPUT:
c     xlon   longitude of storm position to be  output
c     xlat   latitude of storm position to be  output
c     inp    contains input date and model number information
c     ist    the number storm that we're processing (can be 1-15)
c
c     OUTPUT:
c     iovret return code from this subroutine
c
c     OTHER:
c     storm  contains the tcvitals info (from module def_vitals)
c
      USE def_vitals; USE gen_vitals; USE inparms; USE set_max_parms
      USE verbose_output

      implicit none

      type (gencard) gstm
      type (datecard) inp
      real       xlon,xlat
      integer    ist,iovret,istmspd,istmdir
c
      iovret = 0

c     Initially, set all "stm" components equal to the input "gstorm"
c     components for this storm, then we will change the specific
c     components that we need to.

      gstm = gstorm(ist)

c     If the "gv_gen_date" for this storm does not equal 99999, 
c     then that means that a vitals was read in for this storm in 
c     subroutine  read_gen_vitals, so be sure to use the genesis 
c     date, genesis latitude and genesis longitude for the storm
c     identifier at the beginning of the vitals record.

      if (gstm%gv_gen_date /= 99999) then

        if (gstm%gv_gen_type /= 'FOF') then
          ! If this is not a 'FOF' storm (found on the fly storm), then
          ! it must be a TC vitals storm, or a tropical cyclone, and we
          ! don't want to create a vitals record for a tropical cyclone,
          ! since we will rely on reading them from the TC Vitals 
          ! database instead.
          return
        endif

      else

        ! This storm is new in this forecast/analysis and was found on
        ! the fly in the first time level for this run and there was no
        ! previous vitals record for this system

        gstm%gv_gen_date = inp%bcc * 100000000
     &                   + inp%byy * 1000000
     &                   + inp%bmm * 10000
     &                   + inp%bdd * 100
     &                   + inp%bhh

        gstm%gv_gen_fhr = 0

        gstm%gv_gen_lat = int(abs(xlat) * 10. + 0.5)
        if (xlat < 0.0) then                    
          gstm%gv_gen_latns = 'S'
        else                 
          gstm%gv_gen_latns = 'N'
        endif                
                           
        if (xlon >= 180.) then
          gstm%gv_gen_lon = 3600 - int(xlon * 10. + 0.5)
          gstm%gv_gen_lonew = 'W'                      
        else                                        
          gstm%gv_gen_lon = int(xlon * 10. + 0.5)
          gstm%gv_gen_lonew = 'E'               
        endif

        gstm%gv_gen_type = 'FOF'

        ! Transfer all this local "gstm" data back into the 
        ! saved "gstorm" array for use in subsequent fcst hrs...

        gstorm(ist) = gstm

      endif

      gstm%gv_obs_ymd = inp%bcc * 1000000
     &                + inp%byy * 10000
     &                + inp%bmm * 100
     &                + inp%bdd

      gstm%gv_obs_hhmm = inp%bhh * 100

      gstm%gv_obs_lat = int(abs(xlat) * 10. + 0.5)
      if (xlat < 0.0) then                       
        gstm%gv_obs_latns = 'S'              
      else                    
        gstm%gv_obs_latns = 'N'
      endif                   
                          
      if (xlon >= 180.) then
        gstm%gv_obs_lon = 3600 - int(xlon * 10. + 0.5)
        gstm%gv_obs_lonew = 'W'                      
      else                                          
        gstm%gv_obs_lon = int(xlon * 10. + 0.5)  
        gstm%gv_obs_lonew = 'E'               
      endif

      gstm%gv_stdir = istmdir
      gstm%gv_stspd = istmspd

      gstm%gv_depth = 'U'

      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,21) gstm
      endif

      write (67,21) gstm

   21 format (i10,'_F',i3.3,'_',i3.3,a1,'_',i4.4,a1,'_',a3,1x,i8,1x
     &       ,i4.4,1x,i3.3,a1,1x,i4.4,a1,1x,i3,1x,i3,3(1x,i4),1x,i2,1x
     &       ,i3,4(1x,i4),1x,a1)
c
c     bug fix for IBM: flush the output stream so it actually writes
      flush(67)

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine output_tracker_mask (masked_outc,lb,ifh,ifcsthour
     &                ,imax,jmax,iotmret)
c
c     ABSTRACT: This subroutine  outputs a GRIB record that contains the
c     "mask" used to mask out areas surrounding low pressure centers 
c     that have been found during the search at each forecast hour. This
c     mask is written out purely for diagnostic purposes.  The GRIB 
c     identifier given to the mask in the pds is 850 mb height (you can 
c     make it anything you want).  This is only done for the "midlat"
c     and "tcgen" cases, since the runs for those cases use a mask while
c     the regular "tracker" run (that is, the run which strictly tracks
c     only those storms in the TC vitals file) does not.
c
c     INPUT:
c     masked_outc logical array containing mask
c     ifh         integer counter for current forecast hour
c     ifcsthour   integer current forecast hour
c     imax     num points is i-direction of input grid
c     jmax     num points is j-direction of input grid
c
c     OUTPUT:
c     iotmret  return code from this subroutine

      implicit none
c
      integer   ifh,imax,jmax,iotmret,kf,igoret,iix,jjx,ipret
      integer   ifcsthour
      integer   kpds(200),kgds(200)
      logical(1) masked_outc(imax,jmax),lb(imax,jmax)
      real      xmask(imax,jmax)
c
      if (ifh == 1) then
        call baopenw (77,"fort.77",igoret)
        print *,'baopenw: igoret= ',igoret

        if (igoret /= 0) then
          print *,' '
          print *,'!!! ERROR in sub output_tracker_mask opening'
          print *,'!!! **OUTPUT** grib files.  baopenw return codes:'
          print *,'!!! grib file 1 return code = igoret = ',igoret
          STOP 95
          return    
        endif   
      endif  

      xmask = 0.0
      do jjx = 1,jmax
        do iix = 1,imax
          if (masked_outc(iix,jjx)) then
            xmask(iix,jjx) = 1.0
          else
            xmask(iix,jjx) = 0.0
          endif
        enddo  
      enddo  

      kf = imax * jmax

c      kpds(5)  =       7  
c      kpds(6)  =     100
c      kpds(7)  =     850
c      kpds(22) =       0

      kpds(1)  =       7  ;    kpds(2)  =      80
      kpds(3)  =     255  ;    kpds(4)  =     192
      kpds(5)  =       7  ;    kpds(6)  =     100
      kpds(7)  =     850  ;    kpds(8)  =      99
      kpds(9)  =       7  ;    kpds(10) =      20
      kpds(11) =      12  ;    kpds(12) =       0
      kpds(13) =       1  ;    kpds(14) =  ifcsthour
      kpds(15) =       0  ;    kpds(16) =      10
      kpds(17) =       0  ;    kpds(18) =       1
      kpds(19) =       2  ;    kpds(20) =       0
      kpds(21) =      20  ;    kpds(22) =       0
      kpds(23) =       0  ;    kpds(24) =       0
      kpds(25) =       0
      kgds(1)  =       0  ;    kgds(2)  =    imax
      kgds(3)  =    jmax  ;    kgds(4)  =  -90000
      kgds(5)  =       0  ;    kgds(6)  =     128
      kgds(7)  =   90000  ;    kgds(8)  =  359750
      kgds(9)  =     250  ;    kgds(10) =     250
      kgds(11) =      64  ;    kgds(12) =       0
      kgds(13) =       0  ;    kgds(14) =       0
      kgds(15) =       0  ;    kgds(16) =       0
      kgds(17) =       0  ;    kgds(18) =       0
      kgds(19) =       0  ;    kgds(20) =     255

      write(*,980) kpds(1),kpds(2)
      write(*,981) kpds(3),kpds(4)
      write(*,982) kpds(5),kpds(6)
      write(*,983) kpds(7),kpds(8)
      write(*,984) kpds(9),kpds(10)
      write(*,985) kpds(11),kpds(12)
      write(*,986) kpds(13),kpds(14)
      write(*,987) kpds(15),kpds(16)
      write(*,988) kpds(17),kpds(18)
      write(*,989) kpds(19),kpds(20)
      write(*,990) kpds(21),kpds(22)
      write(*,991) kpds(23),kpds(24)
      write(*,992) kpds(25)
      write(*,880) kgds(1),kgds(2)
      write(*,881) kgds(3),kgds(4)
      write(*,882) kgds(5),kgds(6)
      write(*,883) kgds(7),kgds(8)
      write(*,884) kgds(9),kgds(10)
      write(*,885) kgds(11),kgds(12)
      write(*,886) kgds(13),kgds(14)
      write(*,887) kgds(15),kgds(16)
      write(*,888) kgds(17),kgds(18)
      write(*,889) kgds(19),kgds(20)
      write(*,890) kgds(21),kgds(22)
c
  980 format('tmow    kpds(1)  = ',i7,'  kpds(2)  = ',i7)
  981 format('tmow    kpds(3)  = ',i7,'  kpds(4)  = ',i7)
  982 format('tmow    kpds(5)  = ',i7,'  kpds(6)  = ',i7)
  983 format('tmow    kpds(7)  = ',i7,'  kpds(8)  = ',i7)
  984 format('tmow    kpds(9)  = ',i7,'  kpds(10) = ',i7)
  985 format('tmow    kpds(11) = ',i7,'  kpds(12) = ',i7)
  986 format('tmow    kpds(13) = ',i7,'  kpds(14) = ',i7)
  987 format('tmow    kpds(15) = ',i7,'  kpds(16) = ',i7)
  988 format('tmow    kpds(17) = ',i7,'  kpds(18) = ',i7)
  989 format('tmow    kpds(19) = ',i7,'  kpds(20) = ',i7)
  990 format('tmow    kpds(21) = ',i7,'  kpds(22) = ',i7)
  991 format('tmow    kpds(23) = ',i7,'  kpds(24) = ',i7)
  992 format('tmow    kpds(25) = ',i7)
  880 format('tmow    kgds(1)  = ',i7,'  kgds(2)  = ',i7)
  881 format('tmow    kgds(3)  = ',i7,'  kgds(4)  = ',i7)
  882 format('tmow    kgds(5)  = ',i7,'  kgds(6)  = ',i7)
  883 format('tmow    kgds(7)  = ',i7,'  kgds(8)  = ',i7)
  884 format('tmow    kgds(9)  = ',i7,'  kgds(10) = ',i7)
  885 format('tmow    kgds(11) = ',i7,'  kgds(12) = ',i7)
  886 format('tmow    kgds(13) = ',i7,'  kgds(14) = ',i7)
  887 format('tmow    kgds(15) = ',i7,'  kgds(16) = ',i7)
  888 format('tmow    kgds(17) = ',i7,'  kgds(18) = ',i7)
  889 format('tmow    kgds(19) = ',i7,'  kgds(20) = ',i7)
  890 format('tmow    kgds(20) = ',i7,'  kgds(22) = ',i7)
c
      print *,'just before call to putgb, kf= ',kf
      call putgb (77,kf,kpds,kgds,lb,xmask,ipret)
      print *,'just after call to putgb, kf= ',kf
      if (ipret == 0) then
        print *,' '
        print *,'+++ IPRET = 0 after call to putgb'
        print *,' '
      else
        print *,' '
        print *,'!!!!!! ERROR: IPRET NE 0 AFTER CALL TO PUTGB !!!'
        print *,' '
      endif
c
c     bug fix for IBM: flush the output stream so it actually writes
      flush(6)

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_next_ges (fixlon,fixlat,ist,ifh,imax,jmax
     &          ,dx,dy,modelid,valid_pt,readflag,maxstorm,istmspd
     &          ,istmdir,ctype,trkrinfo,ignret)
c
c     ABSTRACT: This subroutine calculates a guess position for the next
c               forecast time.  It does this by using two different 
c               methods and averaging the results from those two.  The
c               first method is a simple linear extrapolation made by
c               basically drawing a line from the previous position 
c               through the current fix position and assuming straight
c               line motion.  The second method is to do a barnes 
c               smoothing of u & v in the vicinity of the storm at 850, 
c               700 & 500 mb to get an average environmental wind 
c               vector at each level, and then move the storm according 
c               to the vector at each level.  Then a weighted average is
c               taken of all these positions from methods 1 & 2 to get 
c               the consensus for the guess position.  NOTE: For a 
c               regional model and a storm that is relatively close to
c               the model boundary, there is a strong possibility that
c               the  barnes analysis subroutine will fail due to trying
c               to access grid points beyond the model's lateral  
c               boundary.  In this case, the redlm & ridlm are halved
c               and barnes is called again.  If it still fails, then 
c               just use the result from method 1 as a default.
c
c     INPUT:
c     fixlon  Array with longitudes of fix positions
c     fixlat  Array with latitudes of fix positions
c     ist     Storm number currently being processed
c     ifh     Forecast hour currently being processed
c     imax    Max number of pts in x-direction for this grid
c     jmax    Max number of pts in y-direction for this grid
c     dx      grid-spacing of the model in the i-direction
c     dy      grid-spacing of the model in the j-direction
c     modelid Integer indicating what model's data is being processed
c     valid_pt Logical; bitmap indicating if valid data at that pt.
c     readflag Logical; Tells whether or not a variable was read in
c              for this model
c     maxstorm Max # of storms that can be handled in this run
c     ctype   character that lets subroutine know if this is a search 
c             for the next position for the purposes of tc vitals or
c             for general tracking.  In the case of vitals, eventually
c             in the  barnes subroutine we are more lax and allow the 
c             routine to keep searching even if we are close to the 
c             grid boundary.  In a general tracking search, if we hit
c             the grid boundary even just once, we exit.
c     trkrinfo derived type detailing user-specified grid info
c
c     OUTPUT:
c     istmspd The speed that the storm would have to move to get from
c             the current position to the next guess position
c     istmdir The direction in which the storm would have to move to 
c             get from the current position to the next guess position
c
c     LOCAL:
c     dt      Number of seconds between successive forecast times
c             for this particular model.
c     dtkm    Distance in meters of 1 degree latitude
c     icutmax Max number of times to cut the ridlm and redlm in half,
c             for use in calling barnes.  If you're using a regional
c             model and on the first call to barnes you try to access
c             a point that's outside the model grid boundary, we'll
c             call  barnes again, but not before cutting the redlm and
c             ridlm in half.  icutmax says how many times to allow 
c             this cutting in half before giving up and just going
c             with the extrapolation method.  At first writing, we'll
c             set icutmax to 2, so that it will allow the ridlm to 
c             get down to 500 km (originally 2000 km) and the redlm 
c             to 125 km (originally 500 km).
c         *** NOTE: After testing the system, it was found that if
c             we cut these radii, the u and v values that are 
c             calculated from barnes are too strongly influenced by
c             the near-storm environment and, especially for asymmetric
c             systems, resulted in u and v values being much too strong.
c             As such, we will not allow these values to be cut, and if
c             we hit the boundaries in barnes, we'll just use the 
c             extrapolation method, which has seemed to work just fine.
c
c     OTHER:  (slonfg, slatfg & storm defined in module def_vitals)
c     slonfg  Array containing first guess longitude positions
c     slatfg  Array containing first guess latitude positions
c     storm   Contains tcvitals information
c
      USE radii; USE def_vitals; USE set_max_parms; USE grid_bounds
      USE tracked_parms; USE level_parms; USE trig_vals; USE trkrparms
      USE gen_vitals
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo
      integer   icutmax,istmspd,istmdir,bskip,ileadtime,ifcsthour
      integer   ifh,ist,npts,ilonfix,jlatfix,ibeg,jbeg,iend,jend
      integer   igiret,ignret,icut,iuret,ivret,ibarnct,n,ix1,ix2
      integer   icount,imax,jmax,modelid,maxstorm
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      dist,distm,xincr,yincr,stmspd,stmdir,atan,arct,degrees
      real      barneswt,extrapwt,dtkm,dt,ucomp,vcomp,xdist,ydist,ydeg
      real      extraplat,avglat,cosfac,xdeg,extraplon,ylatdegmove_last
      real      xlondegmove_last,xnumh_last,ylatdegmove_last_perhour
      real      xlondegmove_last_perhour,xnumh_next,yoldavglat
      real      yoldcosfac,xdistmove_last,xdistmove_last_perhour
      real      ynewavglat,ynewcosfac,xdegnew,dx,dy,re,ri,ubar,vbar
      real      wgttot,uavg,vavg,reold,riold,barnlat,barnlon,wt_total
      real      tmp_fix_lon_curr,tmp_fix_lon_prev
      character*1 :: in_grid, extrap_flag, barnes_flag
      character(*)  ctype
      logical(1) valid_pt(imax,jmax),readflag(14)
c
      in_grid = 'n'
      extrap_flag = 'y'

      ileadtime = nint(fhreal(ifh) * 100.0)
      ifcsthour = ileadtime / 100
c
c     For updating the first guess, if Method 1 and Method 2 are both 
c     able to be done, give the following weights to the 2 methods.
c      
      data barneswt /0.50/, extrapwt /0.50/
c
c     -------------------------------
c     METHOD 1: LINEAR EXTRAPOLATION
c     -------------------------------
c     First, just do a simple linear extrapolation from the previous
c     fix position through the current fix position.  If it's the 
c     first time (vt=0), then use the storm motion vector and storm 
c     speed information from the TC Vitals card.
c
      dtkm = dtk * 1000.
      dt   = (fhreal(ifh+1) - fhreal(ifh)) * 3600.0
c
      if (ifh == 1) then
        if (storm(ist)%tcv_stdir == -99 .or.
     &      storm(ist)%tcv_stspd == -99) then
          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! IN GET_NEXT_GES, at fcst hour = 0, either '
            print *,'!!! storm motion or storm speed = -99 on TCV card.'
            print *,'!!! ist= ',ist,' ifh= ',ifh
            print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
            print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
            print *,'!!! storm motion vector= ',storm(ist)%tcv_stdir
            print *,'!!! storm motion speed= ',storm(ist)%tcv_stspd
            print *,'!!! CANNOT USE LINEAR EXTRAP TO GET NEXT GUESS !!!'
          endif

          extrap_flag = 'n'
        else
          ucomp = sin(float(storm(ist)%tcv_stdir) * dtr) *
     &                float(storm(ist)%tcv_stspd)/10.0
          vcomp = cos(float(storm(ist)%tcv_stdir) * dtr) *
     &                float(storm(ist)%tcv_stspd)/10.0
          xdist = ucomp * dt
          ydist = vcomp * dt
          ydeg = ydist / dtkm
          extraplat = fixlat(ist,ifh) + ydeg
          avglat = 0.5 * (extraplat + fixlat(ist,ifh))
          if (avglat > 89.5)  avglat =  89.0
          if (avglat < -89.5) avglat = -89.0
          cosfac = cos(avglat * dtr)
          xdeg = xdist / (dtkm*cosfac)
          extraplon = fixlon(ist,ifh) + xdeg
        endif
      else

c       Do a simple linear extrapolation of the current motion of the
c       storm.  Follow a line from the  fix position from the last fix
c       through the current fix and extrapolate out.  To figure out the
c       new latitude, just see how many deg lat the storm moved since
c       last time and add it to the current fix latitude.  To calculate
c       the new fix longitude, though, we need to see how many deg lon
c       the storm moved since the last time, convert that to the 
c       distance (km) the storm travelled in the x-direction (at an
c       average latitude between the current and previous latitudes),
c       and then add that distance on to the current longitude and 
c       convert that distance to the num of degrees the storm has 
c       travelled in the x-direction (at an average latitude between
c       the current and next(extrap) latitudes).
c
c       UPDATE Feb 2009: To account for the possibility of using
c       irregularly spaced forecast hours (e.g., 6,10,10.5,...etc),
c       I had to modify this linear extrapolation.

        print *,' '
        print *,'xxxx get_next_ges, prev fix lon= ',fixlon(ist,ifh-1)
        print *,'xxxx get_next_ges, curr fix lon= ',fixlon(ist,ifh)
        print *,' '

        if (fixlat(ist,ifh-1) > -900.0 .and.
     &      fixlon(ist,ifh-1) > -900.0) then

          ylatdegmove_last = fixlat(ist,ifh) - fixlat(ist,ifh-1)

          tmp_fix_lon_curr = fixlon(ist,ifh)
          tmp_fix_lon_prev = fixlon(ist,ifh-1)

          if (tmp_fix_lon_prev < 0.0 .and. tmp_fix_lon_prev > -25.0)
     &    then
            ! previous lon position is within 25 deg west of the GM
            ! and is listed in negative degrees.
            if (tmp_fix_lon_curr < 0.0 .and. tmp_fix_lon_curr > -25.0)
     &      then
              if (verb .ge. 3) then
                print *,' '
                print *,'+++ GM WRAP ALERT 1 in get_next_ges.'
                print *,'+++ In get_next_ges, lon for previous and  '
                print *,'    current time are both negative.  All ok!'
                print *,'    tmp_fix_lon_prev= ',tmp_fix_lon_prev
                print *,'    tmp_fix_lon_curr= ',tmp_fix_lon_curr
              endif 
            elseif (tmp_fix_lon_curr > 0.0 .and.
     &              tmp_fix_lon_curr < 25.0) then
              if (verb .ge. 3) then
                print *,' '
                print *,'+++ GM WRAP ALERT 2 in get_next_ges.'
                print *,'+++ In get_next_ges, lon for previous time'
                print *,'    is negative, while lon for current time'
                print *,'    is positive, but within 0-25 deg East.'
                print *,'    All ok!'
              endif
            endif

          elseif (tmp_fix_lon_prev > 335.0 .and. 
     &            tmp_fix_lon_prev <= 360.0) then
            ! previous lon position is within 25 deg west of the GM
            ! and is listed in positive degrees.
            if (tmp_fix_lon_curr > 335.0 .and.
     &          tmp_fix_lon_curr <= 360.0) then
              if (verb .ge. 3) then
                print *,' '
                print *,'+++ GM WRAP ALERT 3 in get_next_ges.'
                print *,'+++ In get_next_ges, lon for previous and  '
                print *,'    current time are both positive.  All ok!'
                print *,'    tmp_fix_lon_prev= ',tmp_fix_lon_prev
                print *,'    tmp_fix_lon_curr= ',tmp_fix_lon_curr
              endif 
            elseif (tmp_fix_lon_curr > 0.0 .and.
     &              tmp_fix_lon_curr <= 25.0) then
              tmp_fix_lon_curr = tmp_fix_lon_curr + 360.0
              if (verb .ge. 3) then
                print *,' '
                print *,'+++ GM WRAP ALERT 4 in get_next_ges.'
                print *,'+++ In get_next_ges, lon for previous '
                print *,'    time is between 335 & 360, while lon'
                print *,'    for current time is east of the GM and'
                print *,'    is between 0 & 25.  Current tmp_lon'
                print *,'    has been adjusted to be > 360 for the'
                print *,'    purpose of computation.'
                print *,'    tmp_fix_lon_prev= ',tmp_fix_lon_prev
                print *,'    tmp_fix_lon_curr= ',tmp_fix_lon_curr
              endif
            elseif (tmp_fix_lon_curr < 0.0 .and.
     &              tmp_fix_lon_curr >= -25.0) then
              tmp_fix_lon_curr = tmp_fix_lon_curr + 360.0
              if (verb .ge. 3) then
                print *,' '
                print *,'+++ GM WRAP ALERT 5 in get_next_ges.'
                print *,'+++ In get_next_ges, lon for previous '
                print *,'    time is between 335 & 360, while lon'
                print *,'    for current time is west of the GM and'
                print *,'    is between 0 & -25.  Current tmp_lon'
                print *,'    has been adjusted to be positive'
                print *,'    for the purpose of computation.'
                print *,'    tmp_fix_lon_prev= ',tmp_fix_lon_prev
                print *,'    tmp_fix_lon_curr= ',tmp_fix_lon_curr
              endif
            elseif (tmp_fix_lon_curr < -335.0 .and.
     &              tmp_fix_lon_curr >= -360.0) then
              tmp_fix_lon_curr = 720.0 - tmp_fix_lon_curr
              if (verb .ge. 3) then
                print *,' '
                print *,'+++ GM WRAP ALERT 6 in get_next_ges.'
                print *,'+++ In get_next_ges, lon for previous '
                print *,'    time is between 335 & 360, while lon'
                print *,'    for current time is east of the GM and'
                print *,'    is between -335 & -360.  Current tmp_lon'
                print *,'    has been adjusted to be positive and '
                print *,'    > 360 for the purpose of computation.'
                print *,'    tmp_fix_lon_prev= ',tmp_fix_lon_prev
                print *,'    tmp_fix_lon_curr= ',tmp_fix_lon_curr
              endif

            endif

          endif

          xlondegmove_last = tmp_fix_lon_curr - tmp_fix_lon_prev

          xnumh_last = fhreal(ifh) - fhreal(ifh-1)

          ylatdegmove_last_perhour = ylatdegmove_last / xnumh_last
          xlondegmove_last_perhour = xlondegmove_last / xnumh_last

          xnumh_next = fhreal(ifh+1) - fhreal(ifh)

          extraplat = fixlat(ist,ifh)  
     &              + (ylatdegmove_last_perhour * xnumh_next)

          yoldavglat = 0.5 * (fixlat(ist,ifh) + fixlat(ist,ifh-1))
          yoldcosfac = cos (dtr * yoldavglat)
          xdistmove_last  = xlondegmove_last * dtk * yoldcosfac
      
          xdistmove_last_perhour = xdistmove_last / xnumh_last
      
          ynewavglat = 0.5 * (extraplat + fixlat(ist,ifh))
          ynewcosfac = cos(dtr * ynewavglat)
          xdegnew    = (xdistmove_last_perhour * xnumh_next)
     &               / (dtk * ynewcosfac) 
          extraplon  = tmp_fix_lon_curr + xdegnew

        else 

          if ( verb .ge. 3 ) then
            print *,' '
            write(6,92) '!!! IN GET_NEXT_GES, at fcst hour = '
     &             ,ifhours(ifh),ifclockmins(ifh)
            print *,'!!! the lon and lat positions for the previous'
            print *,'!!! forecast hour are -999, meaning that this is a'
            print *,'!!! new storm, so we cannot use the extrap method.'
            print *,'!!! Storm name = ',storm(ist)%tcv_storm_name
            print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
            print *,'!!! CANNOT USE LINEAR EXTRAP TO GET NEXT GUESS !!!'
          endif

  92      format (1x,a36,i4,':',i2.2)

          extrap_flag = 'n'

        endif

      endif

c     -------------------------------
c     METHOD 2: Barnes analysis
c     -------------------------------
c     Do a barnes analysis on the u & v components of the wind near the
c     storm to get an average u & v, then advect the storm according to
c     the average wind vector obtained.  The call to get_ij_bounds is 
c     needed in order to restrict the number of grid points that are 
c     searched in the  barnes subroutine.  See Abstract from this 
c     subroutine for further details.
 
      npts = ceiling(ridlm/(dtk*((dx+dy)/2)))
 
      call get_ij_bounds (npts,0,ridlm,imax,jmax,dx,dy
     & ,glatmax,glatmin,glonmax,glonmin,fixlon(ist,ifh),fixlat(ist,ifh)
     & ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_next_ges from call to '
          print *,'!!! get_ij_bounds, STOPPING processing for '
          print *,'!!! storm number ',ist
        endif

        ignret = 92
        return
      endif

      if (verb >= 3) then
        print *,' '
        print *,' +++ In get_next_ges after call to get_ij_bounds,'
        print *,'     getting bounds for the barnes analysis...'
        print *,'     glatmax= ',glatmax,'  glatmin= ',glatmin
        print *,'     glonmax= ',glonmax,'  glonmin= ',glonmin
        print *,'     fixlon= ',fixlon(ist,ifh),'  fixlat= '
     &                ,fixlat(ist,ifh)
        print *,'     ilonfix= ',ilonfix,'  jlatfix= ',jlatfix
        print *,'     ibeg= ',ibeg,'  iend= ',iend
        print *,'     jbeg= ',jbeg,'  jend= ',jend
      endif

c     For the  barnes analysis, we will want to speed things up for
c     finer resolution grids.  We can do this by skipping some of
c     the points in the  barnes analysis.

      if ((dx+dy)/2 > 0.20) then
        bskip = 1
      else if ((dx+dy)/2 > 0.10 .and. (dx+dy)/2 <= 0.20) then
        bskip = 2
      else if ((dx+dy)/2 > 0.05 .and. (dx+dy)/2 <= 0.10) then
        bskip = 3
      else if ((dx+dy)/2 > 0.03 .and. (dx+dy)/2 <= 0.05) then
        bskip = 5
      else if ((dx+dy)/2 <= 0.03) then
        bskip = 10
      endif

c     Calculate average wind at each level (currently: 850, 700 & 500)

      re = redlm
      ri = ridlm
      icut = 0

      if (trkrinfo%type == 'midlat') then
        icutmax = 2
      else
        icutmax = 1
      endif

      radmaxloop:  do while (icut <= icutmax .and. in_grid == 'n')

        ubar  = 0.0; vbar  = 0.0
        iuret = 0; ivret = 0
        wgttot = 0.0
        ibarnct = 0
        barnes_flag = 'n'

        levelloop: do n=1,nlevg

          select case (n)
           case (1); ix1=3; ix2=4    ! For 850 mb readflags
           case (2); ix1=5; ix2=6    ! For 700 mb readflags
           case (3); ix1=12; ix2=13  ! For 500 mb readflags
          end select

          if (readflag(ix1) .and. readflag(ix2)) then

            call barnes (fixlon(ist,ifh),fixlat(ist,ifh),glon,glat
     &           ,imax,jmax,ibeg,jbeg,iend,jend,u(1,1,n),valid_pt
     &           ,bskip,re,ri,uavg,icount,ctype,trkrinfo,iuret)

            call barnes (fixlon(ist,ifh),fixlat(ist,ifh),glon,glat
     &           ,imax,jmax,ibeg,jbeg,iend,jend,v(1,1,n),valid_pt
     &           ,bskip,re,ri,vavg,icount,ctype,trkrinfo,ivret)
      
            if (iuret /= 0 .or. ivret /= 0) then
 
c             ...barnes probably tried to access a pt outside the grid
c             domain.  So, reduce by half the distance from the center
c             of the farthest pt that barnes tries to access, exit this
c             loop, and try it again with the smaller re and ri.
 
              iuret = 96; ivret = 96
              reold = re
              riold = ri
              re = 0.5 * re
              ri = 0.5 * ri
              if ( verb .ge. 3 ) then
                print *,' ' 
                print *,'NOTE: While attempting to use the  barnes '
                print *,'method to update the first guess, the '
                print *,'algorithm tried to access a grid point that '
                print *,'does not have valid data, meaning that too '
                print *,'large a radius is being searched.  So, the 2 '
                print *,'radii, re and ri, are being halved and, if the'
                print *,'value of icutmax > 0,  the algorithm will be '
                print *,'run again.  Otherwise, if icutmax = 0, only '
                print *,'the extrapolation method will be used.'
                print *,'iuret= ',iuret,' ivret= ',ivret,' icut= ',icut
                print *,'Old re = ',reold,' New re = ',re
                print *,'Old ri = ',riold,' New ri = ',ri
              endif
                
              exit levelloop

            else
              ubar = ubar + wgts(n) * uavg
              vbar = vbar + wgts(n) * vavg
              wgttot = wgttot + wgts(n)
              ibarnct = ibarnct + 1
              if (verb >= 3) then
                print *,' '
                print *,' --- In get_next_ges, ix1= ',ix1,' ix2= ',ix2
                print *,'     uavg= ',uavg,' vavg= ',vavg
                print *,'     ubar= ',ubar,' vbar= ',vbar
                print *,'     n= ',n,' wgts(n)= ',wgts(n),' wgttot= '
     &                       ,wgttot
                print *,'     ibarnct= ',ibarnct
                print *,' '
                print *,' '
              endif
            endif

          endif
              
        enddo levelloop

        if (ibarnct > 0 .and. wgttot > 0.0) then
          barnes_flag = 'y'
          in_grid = 'y'    
          ubar = ubar / wgttot
          vbar = vbar / wgttot
          barnlat = fixlat(ist,ifh) + (vbar * dt)/dtkm
          cosfac = cos (dtr * 0.5 * (fixlat(ist,ifh) + barnlat))
          barnlon = fixlon(ist,ifh) + (ubar * dt)/(dtkm * cosfac)

          if (verb >= 3) then
            print *,' '
            print *,' --- In get_next_ges, mean stats follow: '
            print *,'     ubar= ',ubar,' vbar= ',vbar
            print *,'     wgttot= ',wgttot
            print *,'     fixlon= ',fixlon(ist,ifh),'  fixlat= '
     &                   ,fixlat(ist,ifh)
            print *,'     barnlon= ',barnlon,' barnlat= ',barnlat
            print *,'     dt= ',dt,' dtkm= ',dtkm,' cosfac= ',cosfac
          endif


c         This next if statement says that if we've had to reduce the
c         size of the  barnes analysis domain twice already, then we've
c         only done the analysis on a much smaller area, and this 
c         doesn't give us as good a picture of the average winds in the
c         area of the storm, so reduce the emphasis we place on the 
c         barnes method.

          if (icut >= 2) barneswt = barneswt / 2.

        else
          barnes_flag = 'n'
        endif

        icut = icut + 1

      enddo radmaxloop

c     ---------------------
c     Average the results
c     ---------------------
c     Now do a weighted average of the positions obtained from the 
c     linear extrapolation and the  barnes analysis methods.

      if (extrap_flag == 'y' .and. barnes_flag == 'y') then
        wt_total = barneswt + extrapwt
        slatfg(ist,ifh+1) = (barneswt * barnlat + extrapwt * extraplat)
     &                      / wt_total

        ! Note that in any of these statements just below, in order for
        ! any of these to be > 360, the original fixlon must be close
        ! to 360, i.e., in the far eastern part of the grid, as opposed
        ! to being in the far western part (e.g., 0-2 deg East or so).
        ! Conversely, for any of these to be < 0, the original fixlon 
        ! must be close to 0, i.e., in the far *western* part of the 
        ! grid.

c         yyyy

        if (fixlon(ist,ifh) > 330.0) then

          ! In this part of the IF, we will make sure that the two 
          ! guess lons (barnlon and extraplon) are consistent as 
          ! both being 330+, to be consistent with the fixlon for
          ! this time.

          if (extraplon > 330. .and. barnlon > 330.) then

            continue  ! All lons will be in the 300+ range, so for 
                      ! consistency, we're ok.

          elseif (extraplon > 330. .and. 
     &            (barnlon >= 0.0 .and. barnlon < 30.)) then

            ! extraplon > 330, but barnlon is in the 0-30 range, so
            ! we need to convert the barnlon value to be 360+

            barnlon = barnlon + 360.

          elseif (extraplon > 330. .and. barnlon < 0.) then

            ! extraplon > 330, but barnlon is < 0, so
            ! we need to convert the barnlon value to be positive...

            barnlon = barnlon + 360.

          elseif (barnlon > 330. .and.
     &            (extraplon >= 0.0 .and. extraplon < 30.)) then

            ! barnlon > 330, but extraplon is in the 0-30 range, so
            ! we need to convert the extraplon value to be 360+

            extraplon = extraplon + 360.

          elseif (barnlon > 330. .and. extraplon < 0.) then

            ! barnlon > 330, but extraplon is < 0, so
            ! we need to convert the extraplon value to be positive...

            extraplon = extraplon + 360.

          endif

        elseif (fixlon(ist,ifh) >= 0. and. fixlon(ist,ifh) < 30.0) then

          ! In this part of the ELSEIF, we will make sure that the two 
          ! guess lons (barnlon and extraplon) are consistent as 
          ! both being in the reference of >360 since that is what the 
          ! code below this is expecting with the computation of 
          ! slonfg for the next lead time.

          if ((extraplon >= 0. .and. extraplon < 60.) .and. 
     &        (barnlon   >= 0. .and. barnlon < 60.)) then

              extraplon = extraplon + 360.
              barnlon   = barnlon + 360.

          elseif ((extraplon < 0. .and. extraplon > -60.) .and. 
     &            (barnlon   < 0. .and. barnlon > -60.)) then

            ! convert extraplon and barnlon to be positive

            extraplon = extraplon + 360.
            barnlon   = barnlon + 360.

          elseif ((extraplon >= 0. .and. extraplon < 60.) .and.
     &            barnlon < 0.) then

            extraplon = extraplon + 360.
            barnlon   = barnlon + 360.

          elseif ((barnlon >= 0. .and. barnlon < 60.) .and.
     &            extraplon < 0.) then

            extraplon = extraplon + 360.

          elseif ((barnlon >= 0. .and. barnlon < 60.) .and.
     &            extraplon > 330.) then

            barnlon = barnlon + 360.

          elseif (barnlon >= 330. .and. extraplon < 60.) then

            extraplon = extraplon + 360.

          elseif (extraplon >= 330. .and. barnlon < 60.) then

            barnlon = barnlon + 360.

          elseif ((extraplon >= 0. .and. extraplon < 60.) .and.
     &            barnlon > 330.) then

            extraplon = extraplon + 360.

          endif

        else

          continue   ! extraplon and barnlon do not need to be modified
                     ! since there should be no way that a storm 
                     ! currently east of 30E and west of 30W could make
                     ! it to the Greenwich Mer in one forecast interval

        endif

        print *,' '
        print *,'+++ In get_next_ges, before averaging the 2 methods, '
        print *,'    Raw (no conversion for GM wrap) barnlon=   '
     &         ,barnlon
        print *,'    Raw (no conversion for GM wrap) extraplon= '
     &         ,extraplon

        slonfg(ist,ifh+1) = (barneswt * barnlon + extrapwt * extraplon)
     &                      / wt_total

        if (slonfg(ist,ifh+1) > 360.) then
          ! If we've GM-wrapped past 360, adjust it to be 0-360...
          slonfg(ist,ifh+1) = slonfg(ist,ifh+1) - 360.
        endif

        if ( verb .ge. 3 ) then
          write (6,*) ' '
          if (barnlon >= 360.) then
            write (6,41) barnlon-360.,720.-barnlon,barnlat        
          elseif (barnlon >= 0. .and. barnlon < 360.) then
            write (6,41) barnlon,360.-barnlon,barnlat        
          elseif (barnlon < 0.) then
            write (6,41) barnlon+360.,-1.*barnlon,barnlat        
          endif
          if (extraplon >= 360.) then
            write (6,43) extraplon-360.,720.-extraplon,extraplat        
          elseif (extraplon >= 0. .and. extraplon < 360.) then
            write (6,43) extraplon,360.-extraplon,extraplat        
          elseif (extraplon < 0.) then
            write (6,43) extraplon+360.,-1.*extraplon,extraplat        
          endif
        endif

        ignret = 0
      else if (extrap_flag == 'y' .and. barnes_flag == 'n') then

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'!!! NOTE: In get_next_ges, barnes method was not '
          print *,'!!! done for updating the first guess for this '
          print *,'!!! storm. Only the linear extrapolation method '
          print *,'!!! was used.'
          print *,'!!! ist= ',ist,' ifh= ',ifh
          print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
          print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
        endif

        slatfg(ist,ifh+1) = extraplat
        if (extraplon > 360.) then
          if (trkrinfo%gridtype == 'global') then
            slonfg(ist,ifh+1) = extraplon - 360.
          else

            if ( verb .ge. 1 ) then
              print *,'!!! ERROR in get_next_ges, extraplon >360'
              print *,'!!! for non-global grid.  We only'
              print *,'!!! do GM wrapping for global grids.'
            endif

            ignret = 95
            return
          endif
        elseif (extraplon < 0.) then
          if (trkrinfo%gridtype == 'global') then
            slonfg(ist,ifh+1) = extraplon + 360.
          else

            if ( verb .ge. 1 ) then
              print *,'!!! ERROR in get_next_ges, extraplon < 0'
              print *,'!!! for non-global grid.  We only'
              print *,'!!! do GM wrapping for global grids.'
            endif

            ignret = 95
            return
          endif
        else
          slonfg(ist,ifh+1) = extraplon
        endif

        if ( verb .ge. 3 ) then
          write (6,*) ' '
          write (6,41) 0.0,0.0,0.0
          if (extraplon >= 360.) then
            write (6,43) extraplon-360.,720.-extraplon,extraplat
          elseif (extraplon >= 0. .and. extraplon < 360.) then
            write (6,43) extraplon,360.-extraplon,extraplat
          elseif (extraplon < 0.) then
            write (6,43) extraplon+360.,-1.*extraplon,extraplat
          endif
        endif

        ignret = 0
      else if (extrap_flag == 'n' .and. barnes_flag == 'y') then
        slatfg(ist,ifh+1) = barnlat
        if (barnlon > 360.) then
          if (trkrinfo%gridtype == 'global') then
            slonfg(ist,ifh+1) = barnlon - 360.
          else

            if ( verb .ge. 1 ) then
              print *,'!!! ERROR in get_next_ges, barnlon >360'
              print *,'!!! for non-global grid.  We only'       
              print *,'!!! do GM wrapping for global grids.'
            endif

            ignret = 95  
            return    
          endif  
        elseif (barnlon < 0.) then
          if (trkrinfo%gridtype == 'global') then
            slonfg(ist,ifh+1) = barnlon + 360.
          else 
          
            if ( verb .ge. 1 ) then
              print *,'!!! ERROR in get_next_ges, barnlon < 0'
              print *,'!!! for non-global grid.  We only'
              print *,'!!! do GM wrapping for global grids.'
            endif

            ignret = 95  
            return    
          endif  
        else
          slonfg(ist,ifh+1) = barnlon
        endif


        if ( verb .ge. 3 ) then
          write (6,*) ' '
          write (6,41) 360.-barnlon,barnlat
          if (barnlon >= 360.) then
            write (6,41) barnlon-360.,720.-barnlon,barnlat
          elseif (barnlon >= 0. .and. barnlon < 360.) then
            write (6,41) barnlon,360.-barnlon,barnlat
          elseif (barnlon < 0.) then
            write (6,41) barnlon+360.,-1.*barnlon,barnlat
          endif
          write (6,43) 0.0,0.0,0.0
        endif

        ignret = 0
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_next_ges, new position guess not'
          print *,'!!! made.  Could not get guess using either barnes'
          print *,'!!! method or extrapolation method.'
          print *,'!!! extrap_flag = ',extrap_flag
          print *,'!!! barnes_flag = ',barnes_flag
          print *,'!!! Storm number = ',ist,' ifh = ',ifh
          print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
          print *,'!!! Storm ID = ',storm(ist)%tcv_storm_id
          write (6,41) 0.0,0.0,0.0
          write (6,43) 0.0,0.0,0.0
        endif

        ignret = 95 
      endif


      if ( verb .ge. 3 ) then
        print *,' '
        print *,'-------------------------------------------------- '
        print *,'|      Current fix & updated fix positions       |'
        print *,'-------------------------------------------------- '
        print *,'| In get_next_ges, current fcst hour    = ',fhreal(ifh)
        print *,'|                 current storm number  = ',ist
        print *,'| Return code from get_next_ges = ',ignret
        print *,'| Storm Name = ',storm(ist)%tcv_storm_name
        print *,'| Storm ID = ',storm(ist)%tcv_storm_id
        write (6,420) gstorm(ist)%gv_gen_date,gstorm(ist)%gv_gen_fhr
     &       ,gstorm(ist)%gv_gen_lat
     &       ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &       ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
        write (6,21) fixlat(ist,ifh)
        write (6,23) 360.-fixlon(ist,ifh),fixlon(ist,ifh)
        write (6,25) slatfg(ist,ifh+1)
        write (6,27) 360.-slonfg(ist,ifh+1),slonfg(ist,ifh+1)
        print *,'-------------------------------------------------'
        print *,' '
      endif

 420  format (' | Gen ID (if available): ',i10.10,'_F',i3.3,'_'
     &             ,i3.3,a1,'_',i4.4,a1,'_',a3)
 21   format (' | Current fix lat is ',f7.2)
 23   format (' | Current fix lon is ',f7.2,'W   (',f7.2,'E)')
 25   format (' | Updated guess lat for next fcst hour is ',f7.2)
 27   format (' | Updated guess lon for next fcst hour is ',f7.2
     &       ,'W   (',f7.2,'E)')
c 41   format (' --- barnlon=   ',f7.2,'W    barnlat=   ',f7.2)
c 43   format (' --- extraplon= ',f7.2,'W    extraplat= ',f7.2)

 41   format (' --- barnlon=   ',f7.2,'E  (',f7.2
     &       ,'W)    barnlat=   ',f7.2)
 43   format (' --- extraplon= ',f7.2,'E  (',f7.2
     &       ,'W)    extraplat= ',f7.2)
 
c     Now calculate the speed that the storm would have to move at in
c     order to make it to the next forecast position.  We will use
c     this information in writing out the "gen_vitals" record, if this
c     is requested.

      call calcdist (fixlon(ist,ifh),fixlat(ist,ifh)
     &              ,slonfg(ist,ifh+1),slatfg(ist,ifh+1),dist,degrees)

      ! convert distance from km to meters, then get speed in m/s.

      distm   = dist * 1000.
      stmspd  = distm / dt
      istmspd = int ((stmspd * 10) + 0.5)

      xincr = slonfg(ist,ifh+1) - fixlon(ist,ifh)
      yincr = slatfg(ist,ifh+1) - fixlat(ist,ifh)

      if ( verb .ge. 3 ) then
        print *,'iocheck, dist= ',dist,'  distm= ',distm
        print *,'iocheck, stmspd= ',stmspd,'  istmspd= ',istmspd
        print *,'iocheck, xincr= ',xincr,'  yincr= ',yincr
      endif
 
      if (xincr < 0.0 .and. slonfg(ist,ifh+1) < 30.0 .and.
     &                      fixlon(ist,ifh) > 300.0) then
        ! This means we have a storm moving east across the GM, and
        ! so we are  subtracting, for example, something like
        ! 0.5 - 359.5, so redo xincr, but add 360 to slonfg first...
        xincr = (slonfg(ist,ifh+1) + 360.0) - fixlon(ist,ifh)
      else if (xincr > 300.0) then
        ! This means we have a storm moving west across the GM, and
        ! so we are  subtracting, for example, something like
        ! 359.5 - 0.5, so redo xincr, but add 360 to fixlon first...
        xincr = slonfg(ist,ifh+1) - (fixlon(ist,ifh) + 360.0)
      endif

      if (xincr == 0.0) then
        if (yincr == 0.0) then 
          stmdir = 0.0
        else if (yincr > 0) then
          stmdir = 360.0
        else if (yincr < 0) then
          stmdir = 180.0
        endif
      else if (xincr > 0.0) then
        if (yincr == 0.0) then 
          stmdir = 90.0
        else 
          arct = atan(yincr/xincr) 
          stmdir = 90. - arct / dtr
        endif
      else if (xincr < 0.0) then
        if (yincr == 0.0) then 
          stmdir = 270.0
        else
          arct = atan(yincr/xincr)
          stmdir = 270. - arct / dtr
        endif
      endif

      istmdir = int (stmdir + 0.5)
      if (istmdir > 360) then
        istmdir = 360
      else if (istmdir < 0) then
        istmdir = 0
      endif

      if ( verb .ge. 3 ) then
        print *,'iocheck, stmdir= ',stmdir,'  istmdir= ',istmdir
      endif

      return
      end       

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine advect_tcvitals_from_hour0 (fixlon,fixlat,maxstorm
     &                                 ,inctcv,ifh,trkrinfo,iatret)
c
c     ABSTRACT: This subroutine calculates a guess position for the next
c     forecast time.  As of 11/2016, it is called only for the case in
c     which we've got NetCDF data and no hour0 data, and so we want to 
c     simply take the TC Vitals data and advect the current position to 
c     a position at the next lead time.  We can't use the code in 
c     subroutine  get_next_ges because there are certain allocatable 
c     arrays in that subroutine that need to have been allocated first, 
c     and at this point prior to the first lead time in hour0, they 
c     haven't been allocated.
c
c     INPUT:
c     inctcv  Index for storm number currently being processed
c     ifh     Forecast hour currently being processed
c     trkrinfo derived type detailing user-specified grid info
c
c     OUTPUT:
c     iatret  Return code from this subroutine
c
      USE def_vitals; USE trkrparms; USE tracked_parms
      USE verbose_output; USE trig_vals; USE set_max_parms
      USE gen_vitals

      type (trackstuff) trkrinfo
      integer   iatret,inctcv
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      dist,distm,xincr,yincr,stmspd,stmdir,atan,arct,degrees
      real      ucomp,vcomp,xdist,ydist,ydeg,dt,extraplat
      real      cosfac
      real      dtkm
c
      in_grid = 'n'
      extrap_flag = 'y'

      ileadtime = nint(fhreal(ifh) * 100.0)
      ifcsthour = ileadtime / 100
c
c     ------------------------------------------------------------------
c     Using the storm motion vector and storm translation speed as read
c     from the TC Vitals card, do a simple linear extrapolation from the
c     current observed (TC Vitals) position and advect the storm to a 
c     position at the next lead time.
c     ------------------------------------------------------------------

      iatret = 0

      dtkm = dtk * 1000.
      dt   = (fhreal(ifh+1) - fhreal(ifh)) * 3600.0
c
      if (ifh == 1) then
        if (storm(inctcv)%tcv_stdir == -99 .or.
     &      storm(inctcv)%tcv_stspd == -99) then
          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! In advect_tcvitals_from_hour0, at fcst hour= 0'
            print *,'!!! either storm motion or storm speed = -99 on '
            print *,'!!! TCV card,  ist= inctcv= ',inctcv,' ifh= ',ifh
            print *,'!!! Storm name = ',storm(inctcv)%tcv_storm_name
            print *,'!!! Storm ID = ',storm(inctcv)%tcv_storm_id
            print *,'!!! storm motion vector= ',storm(inctcv)%tcv_stdir
            print *,'!!! storm motion speed= ',storm(inctcv)%tcv_stspd
            print *,'... CANNOT USE LINEAR EXTRAP TO GET NEXT GUESS ...'
            print *,' '
            print *,'... Instead, we will simply use the current '
            print *,'... observed position from TC Vitals and hope that'
            print *,'... it is close enough at the next lead time for '
            print *,'... the tracker to be able to still track it.'
            print *,' '
          endif
          extraplat = slatfg(inctcv,ifh)
          extraplon = slonfg(inctcv,ifh)
        else
          ucomp = sin(float(storm(inctcv)%tcv_stdir) * dtr) *
     &                float(storm(inctcv)%tcv_stspd)/10.0
          vcomp = cos(float(storm(inctcv)%tcv_stdir) * dtr) *
     &                float(storm(inctcv)%tcv_stspd)/10.0
          xdist = ucomp * dt
          ydist = vcomp * dt
          ydeg = ydist / dtkm
          extraplat = fixlat(inctcv,ifh) + ydeg
          cosfac = cos(extraplat * dtr)
          xdeg = xdist / (dtkm*cosfac)
          extraplon = fixlon(inctcv,ifh) + xdeg
        endif
      else
        print *,' '
        print *,'!!! ERROR: In advect_tcvitals_from_hour0, the value of'
        print *,'    ifh is > 1, and this routine should only be called'
        print *,'    if ifh=1 (i.e., for hour0).  STOPPING....'
        print *,' '
        stop 95
      endif

      slatfg(inctcv,ifh+1) = extraplat

      if (extraplon > 360.) then
        if (trkrinfo%gridtype == 'global') then
          slonfg(inctcv,ifh+1) = extraplon - 360.
        else  

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in advect_tcvitals_from_hour0,' 
            print *,'!!! extraplon >360 for non-global grid.  We only'
            print *,'!!! do GM wrapping for global grids.'
          endif 

          iatret = 95
          return
        endif 
      elseif (extraplon < 0.) then
        if (trkrinfo%gridtype == 'global') then
          slonfg(inctcv,ifh+1) = extraplon + 360.
        else  

          if ( verb .ge. 1 ) then
            print *,'!!! ERROR in advect_tcvitals_from_hour0,'
            print *,'!!! extraplon < 0 for non-global grid.  We only'
            print *,'!!! do GM wrapping for global grids.'
          endif 

          iatret = 95
          return
        endif 
      else  
        slonfg(inctcv,ifh+1) = extraplon
      endif

      if ( verb .ge. 3 ) then
        print *,' ' 
        print *,'-------------------------------------------------- '
        print *,'| In advect_tcvitals_from_hour0, info on the '
        print *,'| positions for the current and next lead times '
        print *,'| follow: '
        print *,'-------------------------------------------------- '
        print *,'| current fcst hour     = ',fhreal(ifh)
        print *,'| current storm number  = ',inctcv
        print *,'| Return code from advect_tcvitals_from_hour0= ',iatret
        print *,'| Storm Name = ',storm(inctcv)%tcv_storm_name
        print *,'| Storm ID = ',storm(inctcv)%tcv_storm_id
        write (6,420) gstorm(inctcv)%gv_gen_date
     &       ,gstorm(inctcv)%gv_gen_fhr
     &       ,gstorm(inctcv)%gv_gen_lat
     &       ,gstorm(inctcv)%gv_gen_latns,gstorm(inctcv)%gv_gen_lon
     &       ,gstorm(inctcv)%gv_gen_lonew,gstorm(inctcv)%gv_gen_type
        write (6,21) fixlat(inctcv,ifh)
        write (6,23) 360.-fixlon(inctcv,ifh),fixlon(inctcv,ifh)
        write (6,25) slatfg(inctcv,ifh+1)
        write (6,27) 360.-slonfg(inctcv,ifh+1),slonfg(inctcv,ifh+1)
        print *,'-------------------------------------------------'
        print *,' ' 
      endif 

 420  format (' | Gen ID (if available): ',i10.10,'_F',i3.3,'_'
     &             ,i3.3,a1,'_',i4.4,a1,'_',a3)
 21   format (' | Current TC Vitals lat is ',f7.2)
 23   format (' | Current TC Vitals lon is ',f7.2,'W   (',f7.2,'E)')
 25   format (' | Updated guess lat for next fcst hour is ',f7.2)
 27   format (' | Updated guess lon for next fcst hour is ',f7.2
     &       ,'W   (',f7.2,'E)')


      return
      end       
c
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getradii (xcenlon,xcenlat,imax,jmax,dx,dy,valid_pt
     &                    ,cstormid,ifcsthr,vmaxwind,vradius,trkrinfo
     &                    ,need_to_expand_r34,radmax
     &                    ,first_time_thru_getradii,igrct,igrret)
c
c     ABSTRACT: This subroutine looks through the wind data near an
c     input storm center (fixlon,fixlat) and gets the radii of various
c     surface winds in each of the 4 storm quadrants (NE,NW,SE,SW).  
c     The wind thresholds that are sought are gale force (34kt|17.5m/s),
c     storm force (50kt|25.7m/s), and hurricane force (64kt|32.9m/s). 
c     This subroutine calls the Cray subroutine  orders, which is a 
c     Cray-optimized sort routine.
c
c     UPDATE (AUG 2001): The Cray subroutine  orders was ported to the
c     SP by NCEP personnel.  On the SP version, some changes were
c     apparently made so that the size of the arrays for calling
c     arguments 2, 3 and 4 (iwork, dtemp and isortix in my calling
c     routine) must be the same.  This was not the case on the Crays,
c     and this was causing the  tracker to crash for cases far north
c     on fine grids (GFDL 1/3 grid).
c
c     UPDATE (AUG 2012): The call to the Cray subroutine orders was
c     replaced with a call to qsort, which uses a quicksort sorting
c     algorithm.  While this is not the fastest sorting routine out
c     there, we don't do a lot of sorting here, and qsort is simple
c     and it is portable.
c
c     UPDATE (April 2013):  For the radii, we encountered a problem with
c     radmax being too small.  It was set at 650 km.  Hurricane Sandy
c     exceeded this in the models, so the values returned from getradii
c     were close to the default radmax value of 650 km (350 nm), instead
c     of much higher as they should have been.  To fix it, we now use an
c     iterative technique, where we start with radmax as a small value
c     (450 km).  If getradii returns a value for R34 in a quadrant that
c     does not exceed 0.97*radmax, then that value is ok.  If it does
c     exceed 0.97*radmax, then we bump up radmax by 50 km and call
c     getradii again, looking to diagnose radii only in those quadrants
c     where the need_to_expand_r34 flag = 'n'.
c
c     INPUT:
c
c     xcenlon   fix longitude of storm center for current forecast hour
c     xcenlat   fix latitude of storm center for current forecast hour
c     imax      max i dimension of model grid
c     jmax      max j dimension of model grid
c     dx        grid spacing in i-direction of model grid
c     dy        grid spacing in j-direction of model grid
c     valid_pt  logical bitmap for valid data at a grid point
c     cstormid  3-character storm ATCF ID (e.g., 03L, 11E, etc)
c     ifcsthr   integer value for current forecast hour
c     trkrinfo  derived type containing various info on the storm
c     need_to_expand_r34 1-character array that specifies which of the
c               4 quadrants still need to be expanded on this time
c               through getradii in order to get an R34 value that is
c               not right at the outermost boundary.
c     vmaxwind  max wind (in m/s) that was reported from the 
c               get_max_wind subroutine
c     radmax    input max radius (km) that will be used for this
c               iteration of getradii.
c     first_time_thru_getradii  logical flag.  It is used so that any
c               checking for 50- or 64-kt radii is only done on the 
c               first time through getradii.  Only the checking for 
c               34-kt radii is done on multiple iterations.
c     igrct     integer that indicates what iteration of getradii this
c               call is.
c
c     OUTPUT:
c   
c     igrret    return code from this subroutine
c     vradius   Contains the distance from the storm fix position to
c               each of the various wind threshhold distances in each
c               quadrant. (3,4) ==> (# of threshholds, # of quadrants)
c
c     LOCAL:
c
c     radmax    the maximum radius to look for winds for the various
c               thresholds.
c     quadinfo  This array contains the magnitude of the near-surface 
c               winds and the distance from the gridpoint to the fix 
c               position for each point in each quadrant that is within
c               the maximum allowed radius, radmax.  quadinfo is 
c               allocated within this subroutine, and is allocated as
c               (quadrant, num_pts_in_quadrant, data_type), where 
c               data_type is either windspeed(1) or distance(2) from 
c               storm center to grid point.
c     quadmax   This array contains the max surface wind in each 
c               quadrant, plus the location of it and the distance from
c               the storm center.  This information is critical to 
c               identifying when this subroutine is malfunctioning.

      USE grid_bounds; USE tracked_parms; USE trig_vals; USE level_parms
      USE trkrparms
      USE verbose_output

c
      type (trackstuff) trkrinfo
c
      logical(1) valid_pt(imax,jmax)     
      logical(1) first_time_thru_getradii
c      dimension iwork(257)
      real, allocatable :: quadinfo(:,:,:),iwork(:)
      real      quadmax(4,4)
      real      exactdistnm,exactdistkm,radmax,degrees,cosarg
      real      rlonb,rlonc,rlatb,rlatc,vmaxwind
      real      pt_heading_rad,pt_heading,d
      integer, allocatable :: isortix(:)
      integer   iwindix,ipoint,ifcsthr,igrct
      integer   quadct(4),vradius(3,4)
      integer, parameter  :: dp = selected_real_kind(12, 60)
      real (dp), allocatable :: dtemp(:)
      real ::   windthresh(3) = (/17.5,25.7,32.9/)
      character cstormid*3
      character :: need_to_expand_r34(4)*1

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' *************************************************** '
        print *,' AT BEGINNING OF GETRADII, input radmax= ',radmax
        print *,' *************************************************** '
        print *,' '
        print *,'xcenlon= ',xcenlon,' xcenlat= ',xcenlat
        print *,'imax= ',imax,' jmax= ',jmax,' dx= ',dx,' dy= ',dy
      endif

      igrret  = 0
      
c     -----------------------------------------------------------
c     PART 1: Define the maximum radius for which you'll search
c     for the wind values, and then get the beginning and ending 
c     i and j points for that sub-region to search.  Define this
c     maximum radius (radmax) in terms of km.
c     -----------------------------------------------------------
 
c      radmax = 650.0  ! This value is in units of km. With April 2013
c                      ! update, this is now defined in calling routine

c     Roughly fix xcenlat to the grid point just poleward of xcenlat,
c     and fix xcenlon to the grid point just EASTward of xcenlon.

      if (xcenlat >= 0.0) then
        jlatfix = int((glatmax - xcenlat)/dy + 1.)
      else
        jlatfix = ceiling((glatmax - xcenlat)/dy + 1.)
      endif

      ilonfix = int((xcenlon - glonmin)/dx + 2.)

      if (ilonfix > imax) then
        if (trkrinfo%gridtype == 'global') then
          ilonfix = ilonfix - imax   ! If wrapping past GM
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: In getradii, the '
            print *,'!!!    user-requested eastern boundary'
            print *,'!!!    is beyond the eastern bounds of '
            print *,'!!!    this regional grid.  '
            print *,'!!!         '
            print *,'!!!   imax of regional grid    = ',imax
            print *,'!!!   eastern ilonfix = ',ilonfix
            print *,'!!!         '
            print *,'!!! Radii will not be computed for this time.'
            print *,' '
          endif

          idta=0; iisa=0; iwa=0; iqa=0
          if (allocated(dtemp))    deallocate (dtemp,stat=idta)
          if (allocated(isortix))  deallocate (isortix,stat=iisa)
          if (allocated(iwork))    deallocate (iwork,stat=iwa)
          if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

          if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) then
            print *,' '
            print *,'!!! ERROR in getradii deallocating arrays.'
            print *,'!!! iqa= ',iqa,' idta= ',idta
            print *,'!!! iisa= ',iisa,' iwa= ',iwa
            print *,'!!! EXITING at GR-A....'
            stop 98
          endif

          igrret = 99
          return
        endif   
      endif

      if (ilonfix < 1) then
        if (trkrinfo%gridtype == 'global') then
          ilonfix = ilonfix + imax
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: ilonfix < 1 in subroutine  getradii'
            print *,'!!! for a non-global grid.'
            print *,'!!! ilonfix= ',ilonfix
            print *,'!!!         '
            print *,'!!! Radii will not be computed for this time.'
            print *,' '
          endif

          idta=0; iisa=0; iwa=0; iqa=0
          if (allocated(dtemp))    deallocate (dtemp,stat=idta)
          if (allocated(isortix))  deallocate (isortix,stat=iisa)
          if (allocated(iwork))    deallocate (iwork,stat=iwa)
          if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

          if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) then
            print *,' '
            print *,'!!! ERROR in getradii deallocating arrays.'
            print *,'!!! iqa= ',iqa,' idta= ',idta
            print *,'!!! iisa= ',iisa,' iwa= ',iwa
            print *,'!!! EXITING at GR-B....'
            stop 98
          endif

          igrret = 99
          return
        endif   
      endif

c     Calculate number of grid points to have surrounding the storm so
c     that we are sure radmax is within those points.

      cosfac  = cos (xcenlat * dtr)
      numipts = ceiling((radmax/(dtk*dx))/cosfac)
      numjpts = ceiling(radmax/(dtk*dy))

      jbeg = jlatfix - numjpts
      jend = jlatfix + numjpts + 1
      ibeg = ilonfix - (numipts + 1)
      iend = ilonfix + numipts

      if (ibeg < 1) then

        if (trkrinfo%gridtype == 'global') then
          continue   ! If wrapping past GM, there is code below in this
                     ! getradii routine that can modify the indices
                     ! appropriately.  So... do nothing here.
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: In getradii, the '
            print *,'!!!    user-requested western boundary'
            print *,'!!!    is beyond the western bounds of '
            print *,'!!!    this regional grid.  '
            print *,'!!!         '
            print *,'!!!   imax of regional grid    = ',imax
            print *,'!!!   ilonfix = ',ilonfix,' ibeg= ',ibeg
            print *,'!!!         '
            print *,'!!! Radii will not be computed for this time.'
            print *,' '
          endif

          idta=0; iisa=0; iwa=0; iqa=0
          if (allocated(dtemp))    deallocate (dtemp,stat=idta)
          if (allocated(isortix))  deallocate (isortix,stat=iisa)
          if (allocated(iwork))    deallocate (iwork,stat=iwa)
          if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

          if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) then
            print *,' '
            print *,'!!! ERROR in getradii deallocating arrays.'
            print *,'!!! iqa= ',iqa,' idta= ',idta
            print *,'!!! iisa= ',iisa,' iwa= ',iwa
            print *,'!!! EXITING at GR-C...'
            stop 98
          endif

          igrret = 99
          return
        endif

      endif

      if (jbeg < 1) jbeg = 1

      if (jbeg > jmax .or. jbeg < 1 .or. jend < 1) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'ERROR in getradii calculating jbeg or jend.'
          print *,'jbeg= ',jbeg,' jend= ',jend
          print *,'Wind radii will not be calculated for this time.'
        endif

        idta=0; iisa=0; iwa=0; iqa=0
        if (allocated(dtemp))    deallocate (dtemp,stat=idta)
        if (allocated(isortix))  deallocate (isortix,stat=iisa)
        if (allocated(iwork))    deallocate (iwork,stat=iwa)
        if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

        if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) then
          print *,' '
          print *,'!!! ERROR in getradii deallocating arrays.'
          print *,'!!! iqa= ',iqa,' idta= ',idta
          print *,'!!! iisa= ',iisa,' iwa= ',iwa
          print *,'!!! EXITING at GR-D....'
          stop 98
        endif

        igrret = 99
        return
      endif

      if (iend > imax) then

        if (trkrinfo%gridtype == 'global') then
          continue   ! If wrapping past GM, there is code below in this
                     ! getradii routine that can modify the indices
                     ! appropriately.  So... do nothing here.
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: In getradii, the '
            print *,'!!!    user-requested eastern boundary'
            print *,'!!!    is beyond the eastern bounds of '
            print *,'!!!    this regional grid.  '
            print *,'!!!         '
            print *,'!!!   imax of regional grid    = ',imax
            print *,'!!!   ilonfix = ',ilonfix,' iend= ',iend
            print *,'!!!         '
            print *,'!!! Radii will not be computed for this time.'
            print *,' '
          endif

          idta=0; iisa=0; iwa=0; iqa=0
          if (allocated(dtemp))    deallocate (dtemp,stat=idta)
          if (allocated(isortix))  deallocate (isortix,stat=iisa)
          if (allocated(iwork))    deallocate (iwork,stat=iwa)
          if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

          if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) then
            print *,' '
            print *,'!!! ERROR in getradii deallocating arrays.'
            print *,'!!! iqa= ',iqa,' idta= ',idta
            print *,'!!! iisa= ',iisa,' iwa= ',iwa
            print *,'!!! EXITING at GR-E....'
            stop 98
          endif

          igrret = 99
          return
        endif

      endif

      if (jend > jmax) jend = jmax
 
      if ( verb .ge. 3 ) then
        print *,' '
        print *,'In getradii, ibeg= ',ibeg,' iend= ',iend
        print *,'             jbeg= ',jbeg,' jend= ',jend
        print *,'  ilonfix= ',ilonfix,' jlatfix= ',jlatfix
      endif
 
c     -----------------------------------------------------------
c     PART 2: Within the area of grid points defined by jbeg, 
c     jend, ibeg and iend, (1) calculate all the wind speeds at 
c     each grid point, (2) calculate all of the distances from 
c     each grid point to the storm center, (3) assign each grid 
c     point to one of the 4 quadrants (NE,NW,SE,SW), (4) in each 
c     quadrant, sort the points, based on windspeed.
c     -----------------------------------------------------------

      jnum = jend - jbeg + 1
      inum = iend - ibeg + 1
c      numalloc = ((jnum * inum) / 2) + inum/2 + jnum/2
      numalloc = jnum * inum + inum/2 + jnum/2

      if ( verb .ge. 3 ) then
        print *,'in getradii, numalloc= ',numalloc,' radmax= ',radmax
      endif

      if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

      allocate (quadinfo(4,numalloc,2),stat=iqa)

      if (iqa /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in sub getradii allocating quadinfo array.'
          print *,'!!! iqa = ',iqa
        endif

        idta=0; iisa=0; iwa=0; iqa=0
        if (allocated(dtemp))    deallocate (dtemp,stat=idta)
        if (allocated(isortix))  deallocate (isortix,stat=iisa)
        if (allocated(iwork))    deallocate (iwork,stat=iwa)
        if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

        if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) then
          print *,' '
          print *,'!!! ERROR in getradii deallocating arrays.'
          print *,'!!! iqa= ',iqa,' idta= ',idta
          print *,'!!! iisa= ',iisa,' iwa= ',iwa
          print *,'!!! EXITING at GR-F....'
          stop 98
        endif

        igrret = 94
        return
      endif

      quadct = 0

c     Calculate the distances and wind speeds at each grid point.  If
c     the distance is < radmax, include that wind info in the 
c     appropriate quadinfo array location for that quadrant.

      quadmax = 0.0

      jloop: do j=jbeg,jend
        iloop: do i=ibeg,iend

          ip = i
      
          if (i > imax) then
            if (trkrinfo%gridtype == 'global') then
              ip = i - imax   ! If wrapping past GM
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: In getradii, the '
                print *,'!!!    user-requested point '
                print *,'!!!    is beyond the eastern bounds of '
                print *,'!!!    this regional grid.  '
                print *,'!!!    At location B in subroutine.'
                print *,'!!!         '
                print *,'!!!   imax of regional grid    = ',imax
                print *,'!!!   eastern point in question = ',i
                print *,'!!!         '
                print *,'!!! Radii will not be computed for this time'
                print *,' '
              endif

              idta=0; iisa=0; iwa=0; iqa=0
              if (allocated(dtemp))    deallocate (dtemp,stat=idta)
              if (allocated(isortix))  deallocate (isortix,stat=iisa)
              if (allocated(iwork))    deallocate (iwork,stat=iwa)
              if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

              if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0) 
     &        then
                print *,' '
                print *,'!!! ERROR in getradii deallocating arrays.'
                print *,'!!! iqa= ',iqa,' idta= ',idta
                print *,'!!! iisa= ',iisa,' iwa= ',iwa
                print *,'!!! EXITING at GR-G...'
                stop 98
              endif

              igrret = 99
              return
            endif
          endif
      
          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              ip = i + imax
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i < 1 in sub getradii'
                print *,'!!! for a non-global grid.  i= ',i
                print *,'!!! At location C in subroutine.'
                print *,'!!!         '
                print *,'!!! Radii will not be computed for this time'
                print *,' '
              endif

              idta=0; iisa=0; iwa=0; iqa=0
              if (allocated(dtemp))    deallocate (dtemp,stat=idta)
              if (allocated(isortix))  deallocate (isortix,stat=iisa)
              if (allocated(iwork))    deallocate (iwork,stat=iwa)
              if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

              if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0)
     &        then
                print *,' '
                print *,'!!! ERROR in getradii deallocating arrays.'
                print *,'!!! iqa= ',iqa,' idta= ',idta
                print *,'!!! iisa= ',iisa,' iwa= ',iwa
                print *,'!!! EXITING at GR-H....'
                stop 98
              endif
              
              igrret = 99
              return
            endif
          endif

          call calcdist (xcenlon,xcenlat,glon(ip),glat(j),dist,degrees)
          if (dist > radmax) cycle iloop

          if (valid_pt(ip,j)) then

            vmag = sqrt (u(ip,j,levsfc)**2  + v(ip,j,levsfc)**2)

cc            print *,'i= ',i,' j= ',j,' dist= ',dist,' vmag= ',vmag

            ! Calculate the angle from the center point to this point
            ! and then assign this point to the appropriate quadrant bin

            rlonc = (360.-glon(ip)) * dtr
            rlatc = glat(j) * dtr
            rlonb = (360.-xcenlon) * dtr
            rlatb = xcenlat * dtr
            d     = degrees * dtr

c            write (6,59) 360.-xcenlon,xcenlat,360.-glon(ip),glat
c
c            write (6,61) d/dtr,rlatc/dtr,360.-(rlonc/dtr),rlatb/dtr
c     &                  ,360.-(rlonb/dtr),sin(rlatc),sin(rlatb),cos(d)
c     &                  ,sin(d),cos(rlatb)
c
c
c   59       format (1x,'+++ gr, xcenlon= ',f8.3,'W   xcenlat= '
c     &            ,f8.3,'  glon= ',f8.3,'W   glat= ',f8.3)
c
c   61       format (1x,'+++ gr, d rlatc rlonc rlatb rlonb= ',5f9.4
c     &            ,' sin(rlatc)= ',f8.6,' sin(rlatb)= ',f8.6
c     &            ,' cos(d)= ',f8.6,' sin(d)= ',f8.6
c     &            ,' cos(rlatb)= ',f8.6)

            if (d == 0.0) then

              pt_heading = 0.0

            else

              cosarg = (sin(rlatc)-sin(rlatb)*cos(d)) /
     &                 (sin(d)*cos(rlatb))
              if (cosarg > 1.0)  cosarg = 1
              if (cosarg < -1.0) cosarg = -1
  
              if (sin(rlonc-rlonb) < 0.0) then
                pt_heading_rad = acos(cosarg)
              else
                pt_heading_rad = 2*pi - acos(cosarg)
              endif

              pt_heading = pt_heading_rad / dtr
 
            endif

            if (pt_heading >= 0.0 .and. pt_heading < 90.) then
              ! NE quadrant
              iq = 1
            else if (pt_heading >= 90.0 .and. pt_heading < 180.) then
              ! SE quadrant
              iq = 2
            else if (pt_heading >= 180.0 .and. pt_heading < 270.) then
              ! SW quadrant
              iq = 3
            else if (pt_heading >= 270.0 .and. pt_heading <= 360.) then
              ! NW quadrant
              iq = 4
            endif

c            write (6,73) xcenlat,360.-xcenlon,j,i,ip,glat(j)
c     &                  ,360.-glon(ip),pt_heading,iq

   73       format (1x,'+++ getradii  clat clon: ',f6.2,' ',f7.2,'W',3i4
     &            ,'  plat plon: ',f6.2,' ',f7.2,'W   Dir: ',f7.2
     &            ,'  Quad: ',i2)

            quadct(iq) = quadct(iq) + 1
            quadinfo(iq,quadct(iq),1) = vmag
            quadinfo(iq,quadct(iq),2) = dist
            if (vmag > quadmax(iq,4)) then
              quadmax(iq,1) = glon(ip)
              quadmax(iq,2) = glat(j)
              quadmax(iq,3) = dist
              quadmax(iq,4) = vmag
            endif

          endif

        enddo iloop
      enddo jloop

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'After loop, quadct(1)= ',quadct(1),' quadct(2)= '
     &       ,quadct(2)
        print *,'            quadct(3)= ',quadct(3),' quadct(4)= '
     &       ,quadct(4)
        print *,' '
        
        write (6,110) cstormid,ifcsthr,'NE',quadmax(1,1),quadmax(1,2)
     &       ,quadmax(1,3)*0.539638,quadmax(1,4)*1.9427
        write (6,110) cstormid,ifcsthr,'SE',quadmax(2,1),quadmax(2,2)
     &       ,quadmax(2,3)*0.539638,quadmax(2,4)*1.9427
        write (6,110) cstormid,ifcsthr,'SW',quadmax(3,1),quadmax(3,2)
     &       ,quadmax(3,3)*0.539638,quadmax(3,4)*1.9427
        write (6,110) cstormid,ifcsthr,'NW',quadmax(4,1),quadmax(4,2)
     &       ,quadmax(4,3)*0.539638,quadmax(4,4)*1.9427
        print *,' '
        
 110    format (' quadmax: ',a3,1x,i3.3,1x,a2,1x,' lon: ',f6.2,'E',1x
     &       ,' lat: ',f6.2,' radius: ',f7.2,' nm',2x,' vmag: '
     &       ,f6.2,' kts')
      endif
        
c     Now go through each quadrant and put the wind speed distance info
c     into a temporary array (dtemp), sort that array, and then scan 
c     through that array to find the various thresholds.  

      quadrantloop: do k=1,4

        if (need_to_expand_r34(k) == 'y') then
          print *,'---> R34 search underway for quadrant ',k
     &           ,' radmax= ',radmax
          continue
        else
          print *,'+ R34 okay for quadrant ',k,'... skipping...'
          cycle quadrantloop
        endif

        if (allocated(isortix)) deallocate (isortix)
        if (allocated(dtemp)) deallocate (dtemp)
        if (allocated(iwork)) deallocate (iwork)
        allocate (isortix(quadct(k)),stat=iisa)
        allocate (dtemp(quadct(k)),stat=idta)
        allocate (iwork(quadct(k)),stat=iwa)
        if (iisa /= 0 .or. idta /= 0 .or. iwa /= 0) then
          
          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in getradii allocating isortix, dtemp'
            print *,'!!! or iwork array for quadrant= ',k
            print *,'!!! iisa = ',iisa,' idta= ',idta,' iwa= ',iwa
          endif

          idta=0; iisa=0; iwa=0; iqa=0
          if (allocated(dtemp))    deallocate (dtemp,stat=idta)
          if (allocated(isortix))  deallocate (isortix,stat=iisa)
          if (allocated(iwork))    deallocate (iwork,stat=iwa)
          if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

          if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0)
     &    then
            print *,' '
            print *,'!!! ERROR in getradii deallocating arrays.'
            print *,'!!! iqa= ',iqa,' idta= ',idta
            print *,'!!! iisa= ',iisa,' iwa= ',iwa
            print *,'!!! EXITING at GR-I....'
            stop 98
          endif

          itret = 94
          return
        endif

c       -------------------

        do m=1,quadct(k)
          dtemp(m) = quadinfo(k,m,2)
        enddo

        imode = 2
        isortix = 0

        call qsort (dtemp,isortix,quadct(k))

ccccc        call orders (imode,iwork,dtemp,isortix,quadct(k),1,8,1)
cccc        call orders_4byte (imode,iwork,dtemp,isortix
cccc     &                   ,quadct(k),1,8,1)

        if ( verb .ge. 3 ) then
          print *,' '
c         **************************************************************
c---      mf 20100609
c         CAUSE OF SEG FAULT!!!!!!!! -- not sure still an issue if dtemp
c         properly allocated
c
          !print *,' dtemp(isortix(1)) = ',dtemp(isortix(1))
          print *,' dtemp(isortix(quadct(k)))= '
     &           ,dtemp(isortix(quadct(k)))
          print *,' isortix(1) = ',isortix(1)
          print *,' isortix(quadct(k)) = ',isortix(quadct(k))
        endif

c        ! Uncomment these next lines to see a listing in the  output of
c        ! all wind values & distances in this quadrant less than radmax
c        do iqq = 1,quadct(k)
c          print *,' iqq= ',iqq,'  vmag= ',quadinfo(k,isortix(iqq),1)
c     &           ,' dist= ',quadinfo(k,isortix(iqq),2)
c        enddo

c       -------------------

        if (quadct(k) < 2) then   ! not enough members in array

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! IN GETRADII, NOT ENOUGH MEMBERS IN ARRAY FOR'
            print *,'!!! QUADRANT #',k,' .... # members = quadct(k)= '
     &           ,quadct(k)
            print *,'!!! SETTING ALL VRADII = 0 for quadrant = ',k
          endif

          vradius(1,k) = 0
          vradius(2,k) = 0
          vradius(3,k) = 0
          cycle quadrantloop
        endif

c       Within this quadrant, go through the sorted array of wind
c       magnitudes and compare those wind values against the set
c       wind thresholds to get the wind radii.   The array has 
c       been sorted by distance from the storm center in order of
c       closest (ipoint=1) to farthest (ipoint=quadct(k)).  We 
c       analyze these wind values by starting at the farthest 
c       point and moving inward until we hit a point that has a
c       wind value of at least 34-knot winds (17.5 m/s).  When
c       we find that point, we interpolate between that point and
c       the next farthest out point to get the distance that would
c       be for the exact 17.5 m/s value.  We then continue searching
c       through the wind values down closer to the storm center to
c       see if we can find values for the 50- and 64-knot winds.

        iwindix = 1
        ipoint = quadct(k) + 1

c        print *,'drp: quad= ',k,' quadct= ',quadct(k)

        threshloop: do while (iwindix <= 3 .and. ipoint > 1)

          if (iwindix > 1) then
            if (first_time_thru_getradii) then

              ! We are only doing the wind radii for 50 and 64 kts on
              ! the first time through subroutine getradii (we only
              ! need to do the multiple call iterations for 34 kts).
              !
              ! Make sure vmax for this lead time exceeds the radii
              ! threshold being diagnosed.  The check below avoids,
              ! for example, reporting 50-kt wind radii when the max
              ! wind diagnosed was only 44 kts.  This can happen since
              ! the radius for searching for radii is larger than the 
              ! radius for searching for the max wind.
              if (vmaxwind >= windthresh(iwindix)) then
                if (verb >= 3) then
c                  print *,' '
c                  print *,' +++ vmaxwind of ',vmaxwind,' m/s exceeds'
c                  print *,' +++ threshold of ',windthresh(iwindix)
c                  print *,' +++ (m/s), so radii checking will continue'
c                  print *,' +++ for this threshold.'
c                  print *,' +++ igrct= ',igrct,' ipoint= ',ipoint
c     &                   ,' iwindix= ',iwindix
                  continue
                endif
                continue
              else
                if (verb >= 3) then
                  print *,' '
                  print *,' --- vmaxwind of ',vmaxwind,' m/s does NOT'
                  print *,' - - exceed threshold of '
     &                   ,windthresh(iwindix)
                  print *,' - - (m/s), so radii checking will NOT be '
                  print *,' - - performed for this threshold.'
                endif
                iwindix = iwindix + 1
                cycle threshloop
              endif
            else
              iwindix = iwindix + 1
              cycle threshloop
            endif
          endif

          ipoint = ipoint - 1

          if (quadinfo(k,isortix(ipoint),1) < windthresh(iwindix)) then
            cycle threshloop
          else
            if (ipoint == quadct(k)) then

              if ( verb .ge. 3 ) then
                print *,' ' 
                print *,'!!! NOTE: In getradii, a max wind radius was' 
                print *,'!!! found at the maximum radius checked, so ' 
                print *,'!!! you may want to make sure that you are'
                print *,'!!! checking at a far enough distance from  ' 
                print *,'!!! the fix position, that is, you may want to'
                print *,'!!! increase the value of radmax in subroutine'
                print *,'!!! getradii. Currently, radmax (km) = ',radmax
                print *,'!!! iwindix = ',iwindix,' quadrant= ',k
              endif

              vradius(iwindix,k) = int( ((quadinfo(k,isortix(ipoint),2) 
     &                                  * 0.5396) / 5.0) + 0.5) * 5
            else 

c             Interpolate between the 2 closest distances to each wind
c             threshold to get "exact" distance to that wind threshold
c             radius, convert from km to nm, and then round to the 
c             nearest 5 nm (since TPC uses this precision). 
c             7/23/98 UPDATE: Jim Gross has asked that values not be
c             rounded to the nearest 5 nm, but rather only to the 
c             nearest 1 nm.

              exactdistkm = quadinfo(k,isortix(ipoint),2) +
     &        ( (quadinfo(k,isortix(ipoint),1) - windthresh(iwindix)) /
     &          (quadinfo(k,isortix(ipoint),1) -
     &           quadinfo(k,isortix(ipoint+1),1)) *
     &          ( (quadinfo(k,isortix(ipoint+1),2) -
     &             quadinfo(k,isortix(ipoint),2)) ) )

              exactdistnm = exactdistkm * 0.5396   ! Convert km to nm
              vradius(iwindix,k) = int(exactdistnm + 0.5)

cc             vradius(iwindix,k) = int( (exactdistnm / 5.0) + 0.5) * 5


              if ( verb .ge. 3 ) then
                print *,'iwindix= ',iwindix,' exactdistnm = '
     &                 ,exactdistnm
                print *,'vradius(iwindix,k) =',vradius(iwindix,k)
              endif

            endif

c           The possibility exists, especially for coarse  output 
c           grids, that there could be a jump over more than 1 wind-
c           thresh category when going from 1 grid point to the next, so
c           we need to account for this.  For example, if 1 point has
c           vmag = 15 m/s and the next point closer in has vmag = 28 
c           m/s, then between those 2 points you have the thresholds
c           for gale force AND storm force winds, so to be safe, we
c           actually need to add 1 to ipoint and re-check the current 
c           point, if the wind value at that point is found to be 
c           greater than a wind threshold value (which it has if you've
c           gotten to this point in threshloop).

            ipoint = ipoint + 1

            iwindix = iwindix + 1

          endif

        enddo threshloop

        if (allocated(dtemp))    deallocate (dtemp,stat=idta)
        if (allocated(isortix))  deallocate (isortix,stat=iisa)
        if (allocated(iwork))    deallocate (iwork,stat=iwa)
        if (idta /= 0 .or. iisa /= 0 .or. iwa /= 0) then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in getradii deallocating isortix or'
            print *,'!!! dtemp or work for quadrant= ',k
            print *,'!!! idta= ',idta,' iisa= ',iisa,' iwa= ',iwa
          endif

          idta=0; iisa=0; iwa=0; iqa=0
          if (allocated(dtemp))    deallocate (dtemp,stat=idta)
          if (allocated(isortix))  deallocate (isortix,stat=iisa)
          if (allocated(iwork))    deallocate (iwork,stat=iwa)
          if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

          if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0)
     &    then
            print *,' '
            print *,'!!! ERROR in getradii deallocating arrays.'
            print *,'!!! iqa= ',iqa,' idta= ',idta
            print *,'!!! iisa= ',iisa,' iwa= ',iwa
            print *,'!!! EXITING at GR-J....'
            stop 98
          endif

          itret = 94
          return
        endif

      enddo quadrantloop

      if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

      if (iqa /= 0) then
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in getradii deallocating quadinfo array.'
          print *,'!!! iqa= ',iqa
        endif

        idta=0; iisa=0; iwa=0; iqa=0
        if (allocated(dtemp))    deallocate (dtemp,stat=idta)
        if (allocated(isortix))  deallocate (isortix,stat=iisa)
        if (allocated(iwork))    deallocate (iwork,stat=iwa)
        if (allocated(quadinfo)) deallocate (quadinfo,stat=iqa)

        if (iqa /= 0 .or. iwa /= 0 .or. idta /= 0 .or. iisa /= 0)
     &  then
          print *,' '
          print *,'!!! ERROR in getradii deallocating arrays.'
          print *,'!!! iqa= ',iqa,' idta= ',idta
          print *,'!!! iisa= ',iisa,' iwa= ',iwa
          print *,'!!! EXITING at GR-K....'
          stop 98
        endif

        itret = 94
        return
      endif
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_max_wind (xcenlon,xcenlat,imax,jmax,dx,dy
     &                    ,valid_pt,levsfc,vmax,trkrinfo,rmax,igmwret)
c
c     ABSTRACT: This subroutine looks for the maximum near-surface wind
c     near the storm center.  This subroutine is only concerned with the 
c     value of the max wind, NOT where it's located radially with 
c     respect to the center.  The value that's returned in vmax is the 
c     max wind speed in m/s, which are the units the data are stored in.
c     However, when the max wind values are  output in output_atcf, they
c     will be converted from m/s to knots.
c
c     INPUT:
c
c     xcenlon   fix longitude of storm center for current forecast hour
c     xcenlat   fix latitude of storm center for current forecast hour
c     imax      max i dimension of model grid
c     jmax      max j dimension of model grid
c     dx        grid spacing in i-direction of model grid
c     dy        grid spacing in j-direction of model grid
c     valid_pt  logical bitmap for valid data at a grid point
c     levsfc    integer holding the value of the array member that holds
c               the near-surface winds in the u and v arrays (at orig
c               writing, it's = 4).
c
c     OUTPUT:
c    
c     vmax      value of maximum near-surface wind near the storm ctr
c     rmax      radius of max winds
c     igmwret   return code from this subroutine
c
c     LOCAL:
c
c     radmaxwind the maximum radius to look for a max wind near the 
c                storm center.  You have to allow this to be bigger for
c                model grids with coarse resolution (ECMWF 2.5 degree).

      USE grid_bounds; USE tracked_parms; USE trig_vals; USE trkrparms
      USE verbose_output

      type (trackstuff) trkrinfo

      real      radmaxwind,degrees,dx,dy,rmax
      logical(1) valid_pt(imax,jmax)
      integer   jbeg_hold,jend_hold,bimect,bimwct
c
      igmwret = 0
      rmax    = -99.0

      if ((dx+dy)/2. <= 1.25) then
        if ((dx+dy)/2. <= 0.25) then
          radmaxwind = 300.0
        else
          radmaxwind = 300.0
        endif
      else
        radmaxwind = 500.0
      endif

c     Roughly fix xcenlat to the grid point just poleward of xcenlat,
c     and fix xcenlon to the grid point just EASTward of xcenlon.

      if (xcenlat >= 0.0) then
        jlatfix = int((glatmax - xcenlat)/dy + 1.)
      else
        jlatfix = ceiling((glatmax - xcenlat)/dy + 1.)
      endif

      ilonfix = int((xcenlon - glonmin)/dx + 2.)

      if (ilonfix > imax) then
        if (trkrinfo%gridtype == 'global') then
          ilonfix = ilonfix - imax   ! If wrapping past GM
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: In get_max_wind, ilonfix > imax for a'
            print *,'!!!    non-global grid.  This would mean the '
            print *,'!!!    storm fix position is off the grid.'
            print *,'!!!         '
            print *,'!!!   imax of regional grid    = ',imax
            print *,'!!!   eastern ilonfix = ',ilonfix
            print *,'!!!         '
            print *,'!!! Value of vmax will be set to 0 for this time.'
            print *,' '
          endif

          igmwret = 99
          return
        endif    
      endif   

      if (ilonfix < 1) then
        if (trkrinfo%gridtype == 'global') then
          ilonfix = ilonfix + imax
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: ilonfix < 1 in subroutine  get_max_wind'
            print *,'!!! for a non-global grid.  This would mean the '
            print *,'!!! storm fix position is off the grid.'
            print *,'!!! ilonfix= ',ilonfix
            print *,'!!!         '
            print *,'!!! Value of vmax will be set to 0 for this time.'
            print *,' '
          endif

          igmwret = 99
          return
        endif    
      endif

c     Calculate number of grid points to have surrounding the storm so
c     that we are sure radmaxwind is within those points.

      cosfac  = cos (xcenlat * dtr)
      numipts = ceiling((radmaxwind/(dtk*dx))/cosfac)
      numjpts = ceiling(radmaxwind/(dtk*dy))
 
      jbeg = jlatfix - numjpts
      jend = jlatfix + numjpts + 1
      ibeg = ilonfix - (numipts + 1)
      iend = ilonfix + numipts

      if (jbeg > jmax .or. jend < 1) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'ERROR in get_max_wind calculating jbeg or jend.'
          print *,'Either jbeg > jmax, or jend < 1'
          print *,'jbeg= ',jbeg,' jend= ',jend,' jmax= ',jmax
          print *,'Value of vmax will be set to 0 for this time.'
        endif

        vmax = 0.0
        igmwret = 99
        return
      endif

      if (jbeg < 1) then
        ! Storm is close to southern boundary of grid. Set jbeg=1,
        ! but check to make sure that this point is still south of 
        ! jlatfix.  If it's not, then this makes no sense, as it would
        ! mean that the center of the storm is off the grid....
        jbeg_hold = jbeg
        jbeg = 1
        if (jbeg >= jlatfix) then
          if (verb .ge. 3) then
            print *,' '
            print *,'WARNING: Trouble in get_max_wind calculating jbeg.'
            print *,'Value that was calculated for jbeg was < 1 and was'
            print *,'adjusted to 1, however even with that, the j-index'
            print *,'for the center of the storm is not north of there.'
            print *,'The value of jlatfix is likely < 0 (unrealistic).'
            print *,'In short, the storm is too close to the southern '
            print *,'boundary of the grid.'
            print *,'original jbeg = jbeg_hold = ',jbeg_hold
            print *,'modified jbeg = jbeg= ',jbeg
            print *,'jlatfix = ',jlatfix
            print *,'Value of vmax will be set to 0 for this time.'
            vmax = 0.0
            igmwret = 99
            return
          endif
        else
          if (verb .ge. 3) then
            print *,' '
            print *,'WARNING: Found vortex must be close to southern '
            print *,'boundary of the grid, as the value of jbeg has'
            print *,'been adjusted in get_max_wind. '
            print *,'original jbeg = jbeg_hold = ',jbeg_hold
            print *,'modified jbeg = jbeg= ',jbeg
            print *,'jlatfix = ',jlatfix
          endif 
        endif
      endif

      if (jend > jmax) then
        ! Storm is close to northern boundary of grid. Set jend=jmax,
        ! but check to make sure that this point is still north of 
        ! jlatfix.  If it's not, then this makes no sense, as it would
        ! mean that the center of the storm is off the grid....
        jend_hold = jend
        jend = jmax
        if (jend <= jlatfix) then
          if (verb .ge. 3) then
            print *,' '
            print *,'WARNING: Trouble in get_max_wind calculating jend.'
            print *,'Value that was calculated for jend was > jmax and'
            print *,'was set to jmax, but even with that, the j-index'
            print *,'for the center of the storm is not south of there.'
            print *,'The value of jlatfix is likely > jmax, whic is'
            print *,'unrealistic. In short, the storm is too close to'
            print *,'the northern boundary of the grid.'
            print *,'original jend = jend_hold = ',jend_hold
            print *,'modified jend = jend= ',jend
            print *,'jlatfix = ',jlatfix
            print *,'Value of vmax will be set to 0 for this time.'
            vmax = 0.0
            igmwret = 99
            return
          endif
        else
          if (verb .ge. 3) then
            print *,' '
            print *,'WARNING: Found vortex must be close to northern '
            print *,'boundary of the grid, as the value of jend has'
            print *,'been adjusted in get_max_wind. '
            print *,'original jend = jend_hold = ',jend_hold
            print *,'modified jend = jend= ',jend
            print *,'jlatfix = ',jlatfix
          endif
        endif
      endif

      if ( verb .ge. 3 ) then

        print *,' '
        print *,'In get_max_wind, ibeg= ',ibeg,' iend= ',iend
        print *,'                 jbeg= ',jbeg,' jend= ',jend
        print *,'        ilonfix= ',ilonfix,' jlatfix= ',jlatfix
      endif

      vmax = 0.0
      bimect = 0
      bimwct = 0

      jloop: do j=jbeg,jend
        iloop: do i=ibeg,iend

          ip = i

          if (i > imax) then
            if (trkrinfo%gridtype == 'global') then
              ip = i - imax   ! If wrapping past GM
            else
              if ( verb .ge. 3 .and. bimect == 0) then
                print *,' '
                print *,'!!! WARNING: In get_max_wind, the user-'
                print *,'!!!   -requested point (i) is beyond the'
                print *,'!!!   EASTERN bounds of this regional grid.'
                print *,'!!!   imax of regional grid = ',imax
                print *,'!!!   user-requested point = ',i
                print *,'!!!   ilonfix = ',ilonfix
                print *,'!!!   Skipping to next i in loop....'
                print *,' '
              endif
              bimect = bimect + 1
              cycle iloop
            endif
          endif

          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              ip = i + imax
            else
              if ( verb .ge. 3 .and. bimwct == 0) then
                print *,' '
                print *,'!!! WARNING: i < 1 in sub get_max_wind for a'
                print *,'!!! regional grid.  ilonfix = ',ilonfix
                print *,'!!! i = ',i,' imax = ',imax
                print *,'!!! Skipping to next i in loop....'
                print *,' '
              endif
              bimwct = bimwct + 1
              cycle iloop
            endif   
          endif

          call calcdist (xcenlon,xcenlat,glon(ip),glat(j),dist,degrees)

          if (dist > radmaxwind) cycle

          if (valid_pt(ip,j)) then
            vmag = sqrt (u(ip,j,levsfc)**2  + v(ip,j,levsfc)**2)
            if (vmag > vmax) then
              vmax = vmag
              rmax = dist * 0.539638  ! convert from km to nm
            endif
          endif

        enddo iloop
      enddo jloop

      if (bimwct > 0 .or. bimect > 0) then
        if (verb .ge. 3) then
          print *,' '
          print *,'Warning summary: From sub get_max_wind, there were'
          print *,'numerous (blocked) attempts to access points outside'
          print *,'the bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in jloop:'
          print *,'  # of beyond-WESTERN boundary calls: ',bimwct
          print *,'  # of beyond-EASTERN boundary calls: ',bimect
          print *,'  *** YOU SHOULD CHECK YOUR RESULTS TO ENSURE THAT '
          print *,'  *** THE FIX CENTER FOR THIS STORM AT THIS LEAD'
          print *,'  *** TIME WAS NOT SO CLOSE TO THE REGIONAL GRID'
          print *,'  *** BOUNDARY THAT THE TRUE VMAX VALUE MAY'
          print *,'  *** ACTUALLY BE OFF THE GRID.'
          print *,' '
        endif
      endif

      if ( verb .ge. 3 ) then
        print *,'At end of get_max_wind, vmax= ',vmax,' rmax= ',rmax
      endif

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine fixcenter (clon,clat,ist,ifh,calcparm,geslon,geslat
     &               ,inp,stderr,fixlon,fixlat,xvalues,maxstorm,ifret)
c
c     ABSTRACT: This subroutine loops through the different parameters
c               for the input storm number (ist) and calculates the 
c               center position of the storm by taking an average of
c               the center positions obtained for those parameters.
c               First we check to see which parameters are within a 
c               max error range (errmax), and we discard those that are
c               not within that range.  Of the remaining parms, we get 
c               a mean position, and then we re-calculate the position
c               by giving more weight to those estimates that are closer
c               to this mean first-guess position estimate.
c
c     INPUT:
c     clon     Center longitudes of tracked parms for this storm & ifh
c     clat     Center latitudes of tracked parms for this storm & ifh
c     ist      Storm number
c     ifh      Index for forecast hour
c     calcparm Logical; Use this parm's location for this storm or not
c     geslon   Initial guess longitude for this storm at this fcst hour
c     geslat   Initial guess latitude for this storm at this fcst hour
c     inp      contains the input date and model number information
c     xvalues  The actual max or min data values for each parameter
c     maxstorm max # of storms to be handled in this run
c
c     INPUT/OUTPUT:
c     stderr   Standard deviation of the position "error" of the parms
c              relative to the guess storm position.  As long as the 
c              distance of a parm center to the guess center is <=
c              errpmax, it is included in the std dev calculation.
c
c     OUTPUT:
c     fixlon   Best approximation of storm center's longitude
c     fixlat   Best approximation of storm center's latitude
c     ifret    Return code from this subroutine
c
c     LOCAL:
c     storm       Contains tcvitals info for the storms (def_vitals)
c     trkerr_avg  Sum/avg of the track errors for all parms for this
c                 fcst hour, regardless of whether or not the error was
c                 > errmax.  It's used for getting the std deviation of
c                 the position error for this forecast time, to be used
c                 as part of the errmax calculation for the next fcst 
c                 time.
c     iclose      Number of parameters whose position estimates are 
c                 found to be within a distance errmax of the guess pos
c     wtpos       The weight given to each position estimate.  It's 
c                 based on the distance from the average position.
c     errdist     The "error" of the parameter center position relative
c                 to the storm's guess position.
c     avgerr      Average "error" of the parameter center positions
c                 relative to the storm's guess position.
c     use4next    Logical; If a parm center has been calculated but its
c                 distance from the guess position is > errmax, we don't
c                 use this center in calculating the new guess position,
c                 however we will use this position in calculating the 
c                 standard deviation of the current time's guess 
c                 positions, to be used in calculating the new errmax 
c                 for the next forecast time.  So in this subroutine,
c                 calcparm may be set to FALSE if errdist > errmax, but
c                 use4next will not be set to FALSE (Actually, it is 
c                 only set to FALSE if errdist > errpmax, which is 
c                 defined in error_parms and is roughly 600km).
c     stderr_close  Standard deviation of position errors for parms that
c                 have center estimates that are within a distance 
c                 errmax of the guess position.
c     clon_fguess These are the first-guess mean position estimates, 
c     clat_fguess which are the means of the position estimates that
c                 are within a distance errmax.  These first-guess mean
c                 positions will be refined by giving more weight to
c                 individual parameter estimates that are closer to 
c                 this first-guess mean position.
c     dist_from_mean Contains the "error" distance of each parameter
c                 from the first-guess mean position (clon_fguess,
c                 clat_fguess).  NOTE: If a parameter is not within
c                 a distance errmax of the guess position for this
c                 time (geslon,geslat), then there will be NO 
c                 dist_from_mean calculated for that parm.
c
      USE error_parms; USE set_max_parms; USE inparms; USE def_vitals
      USE atcf; USE gen_vitals; USE tracked_parms
      USE verbose_output

      type (datecard) inp

      real      clon(maxstorm,maxtime,maxtp),temp_clon(maxtp)
      real      clat(maxstorm,maxtime,maxtp),temp_clat(maxtp)
      real      fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real      trkerr(maxtp),errdist(maxtp),xvalues(maxtp)
      real      stderr(maxstorm,maxtime),devia(maxtp),wtpos(maxtp)
      real      dist_from_mean(maxtp)
      real      degrees,errtmp
      integer   gt345_ct,lt15_ct
      logical(1) calcparm(maxtp,maxstorm),use4next(maxtp)
      character charparm(maxtp)*8,charmaxmin(maxtp)*8
c
      data charparm /'zeta 850','zeta 700','circ 850','NOT USED'
     &   ,'circ 700','NOT USED',' gph 850',' gph 700','    MSLP'
     &   ,'circ sfc','zeta sfc',' thk 5-8',' thk 2-5',' thk 2-8'/
      data charmaxmin /'  Max   ','  Max   ','  Min   ','NOT USED'
     &   ,'  Min   ','NOT USED','  Min   ','  Min   ','  Min   '
     &   ,'  Min   ','  Max   ','  Max   ','  Max   ','  Max   '/
c
      ifret=0
c
c     We need to judge whether each parameter position is reasonable,
c     so we'll check to make sure that the dist from each parameter's 
c     estimate to the guess position is less than a maximum allowable 
c     error. If it's the first forecast time, use the initial error max 
c     (defined as errinit in error_parms) as errmax.  Otherwise, the 
c     max error criterion is that the distance error must not exceed 3 
c     times the previous forecast time's standard deviation (after a 
c     small growth factor has been applied).
c     UPDATE 3/5/98: During testing, it was found that just using the
c     previous time's stdev made errmax too "jumpy" (i.e., at vt=48h,
c     errmax could = 380, and then at vt=54h, errmax could jump down
c     to 190, so we've changed it so that it uses an average of the 
c     stdev's from the 3 previous forecast times to maintain some
c     continuity between successive forecast times).
c
      if (ifh == 1) then
        if (atcfname == 'GFSO' .or. atcfname == 'MRFO' .or. 
     &      atcfname == 'GDAS' .or. atcfname == 'GFDT' .or.
     &      atcfname(1:3) == 'AP0' .or. atcfname(1:3) == 'AN0' .or.
     &      atcfname(1:3) == 'AP1' .or. atcfname(1:3) == 'AN1' .or.
     &      atcfname(1:3) == 'AC0' .or. atcfname == 'AEAR' ) then
          errmax  = err_gfs_init
          errinit = err_gfs_init
        else if (atcfname == 'EMX ' .or. atcfname == 'FV3 ') then
          errmax  = err_ecm_max
          errinit = err_ecm_max
        else
          errmax  = err_reg_init
          errinit = err_reg_init
        endif
      else
        if (atcfname == 'GFSO' .or. atcfname == 'MRFO' .or.
     &      atcfname == 'GDAS' .or. atcfname == 'GFDT' .or.
     &      atcfname(1:3) == 'AP0' .or. atcfname(1:3) == 'AN0' .or.
     &      atcfname(1:3) == 'AP1' .or. atcfname(1:3) == 'AN1' .or.
     &      atcfname(1:3) == 'AC0' .or. atcfname == 'AEAR') then
          errinit = err_gfs_init
        else if (atcfname == 'EMX ' .or. atcfname == 'FV3 ') then
          errinit = err_ecm_max
        else
          errinit = err_reg_max
        endif

        if (ifh >= 4) then
          xavg_stderr = (stderr(ist,ifh-3) + stderr(ist,ifh-2)
     &                +  stderr(ist,ifh-1)) / 3.0
        else if (ifh == 3) then
          xavg_stderr = (stderr(ist,ifh-2) + stderr(ist,ifh-1)) / 2.0
        else if (ifh == 2) then
          xavg_stderr = stderr(ist,ifh-1)
        endif

c       The following errmax statement was replaced by the ensuing 4 
c       lines due to a compiler bug on some other platforms:
c        errmax = amin1(amax1(3.0*xavg_stderr*errpgro,errinit)
c     &                ,errpmax)

        errtmp = 3.0*xavg_stderr*errpgro
        errmax = max(errtmp,errinit)
        errtmp = errpmax
        errmax = min(errmax,errtmp)

      endif

      if ( verb .ge. 3 ) then
        print *,' '
        if (ifh > 1) then
          print '(a42,f8.2,a15,f8.2)'
     &         ,' At beg of fixcenter, stderr(ist,ifh-1) = '
     &         ,stderr(ist,ifh-1),'  xavg_stderr= ',xavg_stderr 
        else
          print '(a45,a18)'
     &         ,' At beg of fixcenter, stderr(ist,ifh-1) = N/A'
     &         ,'  xavg_stderr= N/A'
        endif
        print *,'At beg of fixcenter, errpgro = ',errpgro
        print *,'At beg of fixcenter, errinit = ',errinit
        print *,'At beg of fixcenter, errpmax = ',errpmax
        print *,'At beg of fixcenter, ifh= ',ifh,' errmax= ',errmax
      endif

      trkerr_avg = 0.0
      iclose = 0; itot4next = 0 
      clonsum = 0.0; clatsum = 0.0
      errdist = 0.0
      use4next = .FALSE.
      gt345_ct = 0
      lt15_ct  = 0
 
c     For each parm, check to see if the estimated center is within
c     distance errmax of the guess center.  If it's within errmax,
c     then use that parm for locating the center.  If it's NOT
c     within errmax, but IS within errpmax, then we still use this
c     in calculating the standard deviation of the parameters for
c     helping to determine the errmax for the next forecast hour.

c OLD NOTE: For calculating the std dev to be used for the next
c OLD forecast hour, do NOT use vmag 850, vmag 700 or vmag sfc, since
c OLD those parms are always guaranteed to be within a short range of 
c OLD the guess, due to the nature of the algorithm (see subroutine
c OLD get_uv_center for further details on that).

      do ip=1,maxtp

        if (ip == 4 .or. ip == 6) then   ! Parms 4 & 6 not defined.
          calcparm(ip,ist) = .FALSE.   
          cycle  
        endif
        if (calcparm(ip,ist)) then
          call calcdist (geslon,geslat,clon(ist,ifh,ip)
     &                    ,clat(ist,ifh,ip),dist,degrees)
          errdist(ip) = dist
          if (dist <= errpmax) then
            use4next(ip) = .TRUE.
            trkerr_avg = trkerr_avg + dist
            itot4next = itot4next + 1
          endif
          if (dist <= errmax) then
            iclose = iclose + 1
            if (clon(ist,ifh,ip) > 345.) then
              gt345_ct = gt345_ct + 1
            endif
            if (clon(ist,ifh,ip) < 15.) then
              lt15_ct  = lt15_ct + 1
            endif
            clonsum = clonsum + clon(ist,ifh,ip)
            clatsum = clatsum + clat(ist,ifh,ip)
          else 
            calcparm(ip,ist) = .FALSE.
          endif
        endif

      enddo

      if (iclose > 0) then
        if (gt345_ct > 0 .and. lt15_ct > 0) then
          ! We have some parms left of the GM and some to the right,
          ! so we will add (360*lt15_ct) to the sum of the lons (clonsum)
          clon_fguess = (clonsum + (360.*float(lt15_ct)))/ iclose
        else
          clon_fguess = clonsum / float(iclose)
        endif
        if (clon_fguess >= 360.0) then
          clon_fguess = clon_fguess - 360.
        endif
        clat_fguess = clatsum / float(iclose)
      endif

c     Print out a table listing of the locations of the  fixes for 
c     the individual parameters.

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'--------------------------------------------------'
        write (6,95) 'Individual fixes follow..., fhr= ',ifhours(ifh)
     &       ,ifclockmins(ifh),'  ',storm(ist)%tcv_storm_id,' '
     &       ,storm(ist)%tcv_storm_name
        write (6,97) gstorm(ist)%gv_gen_date,gstorm(ist)%gv_gen_fhr
     &       ,gstorm(ist)%gv_gen_lat
     &       ,gstorm(ist)%gv_gen_latns,gstorm(ist)%gv_gen_lon
     &       ,gstorm(ist)%gv_gen_lonew,gstorm(ist)%gv_gen_type
        print *,'Model name = ',atcfname
        print *,'Values of -99.99 indicate that a fix was unable to be'
        print *,'made for that paramater.  Parameters 4 & 6 are not'
        print *,'used.  Vorticity data values are scaled by 1e5.'
        print *,'Circulation data values are scaled by 1e-6.'
        print *,'errdist is the distance that the position estimate is'
        print *,'from the guess position for this time.  MSLP value '
        print *,'here may differ from that in the atcfunix file since '
        print *,'the one here is that derived from the area-averaged '
        print *,'barnes analysis, while that in the atcfunix file is '
        print *,'from a specific gridpoint.'
        write (6,21) geslon,360.-geslon,geslat
        write (6,*)  ' '
        write (6,23) 
        write (6,25)
      endif

      if (geslat > 0.0) then
        charmaxmin(1)  = '  Max   '
        charmaxmin(2)  = '  Max   '
        charmaxmin(3)  = '  Max   '
        charmaxmin(5)  = '  Max   '
        charmaxmin(10) = '  Max   '
        charmaxmin(11) = '  Max   '
      else 
        charmaxmin(1)  = '  Min   '
        charmaxmin(2)  = '  Min   '
        charmaxmin(3)  = '  Min   '
        charmaxmin(5)  = '  Min   '
        charmaxmin(10) = '  Min   '
        charmaxmin(11) = '  Min   '
      endif

      do ip=1,maxtp
        if (ip == 1 .or. ip == 2 .or. ip == 11) then 
          ! This IF block allows vorticity values to be 
          ! written out and scaled up by 1e5 ...
          if (clon(ist,ifh,ip) < 0.001 .and. 
     &        clon(ist,ifh,ip) > -0.001) then

            if ( verb .ge. 3 ) then
              write (6,27) ip,charparm(ip),charmaxmin(ip),0.0
     &             ,0.0,clat(ist,ifh,ip),xvalues(ip)*1e5
     &             ,calcparm(ip,ist),errdist(ip)
            endif

          else

            if ( verb .ge. 3 ) then
              write (6,27) ip,charparm(ip),charmaxmin(ip)
     &             ,clon(ist,ifh,ip),360.-clon(ist,ifh,ip)
     &             ,clat(ist,ifh,ip),xvalues(ip)*1e5
     &             ,calcparm(ip,ist),errdist(ip)
            endif

          endif

        elseif (ip == 3 .or. ip == 5 .or. ip == 10) then
          ! This IF block allows circulation values to be
          ! written out and scaled down by 1e-6 ...

          if (clon(ist,ifh,ip) < 0.001 .and.
     &        clon(ist,ifh,ip) > -0.001) then

            if ( verb .ge. 3 ) then
              write (6,27) ip,charparm(ip),charmaxmin(ip),0.0
     &             ,0.0,clat(ist,ifh,ip),xvalues(ip)*1e-6
     &             ,calcparm(ip,ist),errdist(ip)
            endif

          else

            if ( verb .ge. 3 ) then
              write (6,27) ip,charparm(ip),charmaxmin(ip)
     &             ,clon(ist,ifh,ip),360.-clon(ist,ifh,ip)
     &             ,clat(ist,ifh,ip),xvalues(ip)*1e-6
     &             ,calcparm(ip,ist),errdist(ip)
            endif

          endif

        else
          if (clon(ist,ifh,ip) < 0.001 .and.
     &        clon(ist,ifh,ip) > -0.001) then

            if ( verb .ge. 3 ) then
              write (6,27) ip,charparm(ip),charmaxmin(ip),0.0
     &             ,0.0,clat(ist,ifh,ip),xvalues(ip)
     &             ,calcparm(ip,ist),errdist(ip)
            endif

          else

            if ( verb .ge. 3 ) then
              write (6,27) ip,charparm(ip),charmaxmin(ip)
     &             ,clon(ist,ifh,ip),360.-clon(ist,ifh,ip)
     &             ,clat(ist,ifh,ip),xvalues(ip)
     &             ,calcparm(ip,ist),errdist(ip)
            endif

          endif
        endif
      enddo

 21   format (' Guess location for this time: ',f7.2,'E  (',f6.2,'W)'
     &       ,2x,f7.2)
 23   format (' parm#    parm    Max/Min   Lon_fix(E)  Lon_fix(W)'
     &       ,'   Lat_fix   Max/Min_value   calcparm   errdist(km)')
 25   format (' -----    ----    -------   ----------  ----------'
     &       ,'   -------   -------------   --------   ----------')
 27   format (2x,i2,4x,a8,2x,a8,3x,f7.2,5x,f7.2,4x,f7.2,7x,f9.2
     &       ,6x,L2,7x,f7.2)
 95   format (1x,a33,1x,i4,':',i2.2,a2,a4,a1,a9)
 97   format (' Gen ID (if available): ',i10.10,'_F',i3.3,'_',i3.3,a1
     &       ,'_',i4.4,a1,'_',a3)


c     If number of parameter centers close enough (iclose) > 0, then
c     calculate the center by taking an average of all the parameter
c     center positions that are within distance errmax from the guess
c     position (geslon,geslat).  Get a first-guess mean position, and
c     then re-calculate the position estimate by giving more weight
c     to those positions that are closer to the first-guess mean
c     position.

      dist_from_mean = 0.0

      if (iclose > 0) then

c       Get distances from first-guess mean position....

        do ip=1,maxtp
          if (calcparm(ip,ist)) then
            call calcdist (clon_fguess,clat_fguess,clon(ist,ifh,ip)
     &                    ,clat(ist,ifh,ip),dist,degrees)
            dist_from_mean(ip) = dist
          endif
        enddo
           
c       Get the mean distance of each parameter estimate from 
c       the first-guess mean position

        call avgcalc (dist_from_mean,maxtp,calcparm(1,ist)
     &               ,xmn_dist_from_mean,iaret)

        if (iaret == 0) then

          call stdevcalc (dist_from_mean,maxtp,calcparm(1,ist)
     &                   ,xmn_dist_from_mean,stderr_close,isret)

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'After stdevcalc, xmn_dist_from_mean= '
     &           ,xmn_dist_from_mean,' stderr_close= '
     &           ,stderr_close,' isret= ',isret
          endif

        endif
        if (iaret /= 0 .or. isret /= 0) then
          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR IN FIXCENTER -- Error occurred in either'
            print *,'!!! avgcalc or stdevcalc.  Storm number = ',ist
            print *,'!!! RCC from avgcalc = ',iaret
            print *,'!!! RCC from stdevcalc = ',isret
            print *,'!!! Center fix will NOT be made, and processing'
            print *,'!!! for this storm is ending.  The probable cause'
            print *,'!!! is that no calcparms were valid for this storm'
            print *,'!!! at this forecast hour.'
          endif

          fixlon(ist,ifh) = -999.0
          fixlat(ist,ifh) = -999.0
          ifret = 95
          return
        endif

        if (calcparm(1,ist) .or. calcparm(2,ist) .or. calcparm(7,ist)
     &      .or. calcparm(8,ist) .or. calcparm(9,ist) 
     &      .or. calcparm(11,ist) .or. calcparm(3,ist)
     &      .or. calcparm(10,ist) .or. calcparm(5,ist)
     &      .or. calcparm(12,ist) .or. calcparm(13,ist)
     &      .or. calcparm(14,ist)) then
          continue
        else

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! In fixcenter, STOPPING PROCESSING for this'
            print *,'!!! storm.  The reason is that none of the  fix'
            print *,'!!! locations for parms z850, z700, zeta 850,'
            print *,'!!! zeta 700, MSLP, wcirc_850, wcirc_700, '
            print *,'!!! wcirc_sfc, sfc zeta or the various levels '
            print *,'!!! of thicknesses were within a '
            print *,'!!! reasonable distance of the guess location.'
            print *,'!!! ist= ',ist,' ifh= ',ifh
            write (6,102) ifhours(ifh),ifclockmins(ifh)
 102        format (1x,'!!! Forecast hour: ',i4,':',i2.2)
          endif

          fixlon(ist,ifh) = -999.0
          fixlat(ist,ifh) = -999.0
          ifret = 95
          return
        endif

c       Now re-calculate the mean position by giving more weight 
c       to those position estimates that are closer to the first
c       guess mean position.  Note that if stderr_close < 5.0, we
c       force it to be 5.0; we do this to avoid getting very 
c       large numbers for devia values, which could make the 
c       weights (wtpos) equal to 0.  This occurred during testing
c       when only 2 parameters were valid, and so, of course, the
c       standard deviation from the mean of those 2 parameters 
c       was close to 0, which gave devia values around 6000, and
c       then wtpos values of 0, leading to a divide by 0 crash
c       later on in subroutine  wtavrg.

        kprm=0

        if (stderr_close > 0.0) then
          if (stderr_close < 5.0) then

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'NOTE: Since stderr_close had a value less than'
              print *,'5, stderr_close has been forced to be equal'
              print *,'to 5 in order to avoid dividing by zero later'
              print *,'on in subroutine  wtavrg.'
            endif

            stderr_close = 5.0
          endif
          do ip=1,maxtp
            if (calcparm(ip,ist)) then
              kprm = kprm + 1
              devia(kprm) = dist_from_mean(ip) / stderr_close
              wtpos(kprm) = exp(-devia(kprm)/3.)
              temp_clon(kprm) = clon(ist,ifh,ip)
              temp_clat(kprm) = clat(ist,ifh,ip)

              if ( verb .ge. 3 ) then
                write (6,113) ip,kprm,dist_from_mean(ip),devia(kprm)
     &               ,wtpos(kprm),temp_clon(kprm)
     &               ,360.-temp_clon(kprm),temp_clat(kprm)
              endif

            endif
          enddo
  113     format (1x,'ip= ',i2,' kprm= ',i2,' dist_from_mean= ',f7.3
     &           ,' devia= ',f7.3,' wtpos= ',f8.5,2x,3(2x,f7.2))
        else
c     
c         This next if statement is for the case in which only 1
c         parameter is valid, for which the stderr_close will = 0
c         (obviously), but as long as we have 1 valid parameter,
c         continue processing, and set the weight for that parm = 1.
c         The else portion is for the case in which stderr_close
c         = 0 with NO parms being close.
c
          if (iclose == 1) then
            do ip=1,maxtp
              if (calcparm(ip,ist)) then
                kprm = kprm + 1
                wtpos(kprm) = 1
                temp_clon(kprm) = clon(ist,ifh,ip)
                temp_clat(kprm) = clat(ist,ifh,ip)
              endif
            enddo
          else
            
            if ( verb .ge. 1 ) then
              print *,' '
              print *,'!!! ERROR IN FIXCENTER, stderr_close not > 0'
              print *,'!!! stderr_close = ',stderr_close
              print *,'!!! The probable cause is that no calcparms were'
              print *,'!!! valid for this storm at this forecast hour.'
            endif

            fixlon(ist,ifh) = -999.0
            fixlat(ist,ifh) = -999.0
            ifret = 95
            return
          endif
        endif
c
        if (kprm > 0) then
          call wtavrg_lon (temp_clon,wtpos,kprm,fixlon(ist,ifh),iwtret1)
          call wtavrg (temp_clat,wtpos,kprm,fixlat(ist,ifh),iwtret2)
          if (iwtret1 == 0 .and. iwtret2 == 0) then
            if (verb .ge. 3) then
              print *,' '
              write (6,173) storm(ist)%tcv_storm_id,ifhours(ifh)
     &             ,ifclockmins(ifh),fixlon(ist,ifh)
     &             ,360.-fixlon(ist,ifh),fixlat(ist,ifh)
  173         format ('At end of fixcenter:  ',a4,'  fhr= ',i4,':',i2.2
     &              ,'   Fix position=  ',f7.2,'E  (',f6.2,'W)',2x,f7.2)
              print *,' '
            endif
          else
            if ( verb .ge. 1 ) then
              print *,' '
              print *,'!!! ERROR IN FIXCENTER in call to wtavrg.'
              print *,'!!! Return Codes from wtavrg calls follow: '
              print *,'!!!   RCC from wtavrg for long fix: ',iwtret1
              print *,'!!!   RCC from wtavrg for lat  fix: ',iwtret2
              print *,'!!! This means a divide by zero would have '
              print *,'!!! been attempted, which means that the '
              print *,'!!! weights in wtpos are not > 0.  Check in'
              print *,'!!! subroutine  fixcenter where devia values'
              print *,'!!! are calculated to see if something is '
              print *,'!!! wrong there.  Values of wtpos array follow:'
              print *,'!!! ',wtpos
              print *,'!!! ist= ',ist,' ifh= ',ifh,' iclose= ',iclose
              print *,'!!! errmax= ',errmax,' kprm= ',kprm
              print *,' '
            endif
            fixlon(ist,ifh) = -999.0
            fixlat(ist,ifh) = -999.0
            ifret = 95
            return
          endif
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR IN FIXCENTER, kprm NOT > 0'
            print *,'!!! This means that, for whatever reason, the '
            print *,'!!! calcparm logical flag was set to .FALSE. for'
            print *,'!!! all of the parameters.  Thus, a center'
            print *,'!!! position could not be obtained for this storm'
            print *,'!!! ist= ',ist,' ifh= ',ifh,' iclose= ',iclose
            print *,'!!! errmax= ',errmax,' kprm= ',kprm
          endif

          fixlon(ist,ifh) = -999.0
          fixlat(ist,ifh) = -999.0
          ifret = 95
          return
        endif

      else

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'!!! NOTE: IN FIXCENTER, No storms are within errmax '
          print *,'!!! OR the calcparm logical flag was set to .FALSE. '
          print *,'!!! all of the parameters.  Thus, a center'
          print *,'!!! position could not be obtained for this storm'
          print *,'!!! ist= ',ist,' ifh= ',ifh,' iclose= ',iclose
          print *,'!!! errmax= ',errmax
        endif

        fixlon(ist,ifh) = -999.0
        fixlat(ist,ifh) = -999.0
        ifret = 95
        return
      endif 
 
c     Now calculate the average error of all the parms that are within
c     a radius errpmax (defined in error_parms, ~600km), and the std
c     dev of those errors.  This standard deviation will be used in
c     calculating the maximum allowable error for the next forecast 
c     time.
 
      if (itot4next > 0 .and. ifret /= 95) then
        trkerr_avg = trkerr_avg / float(itot4next)
        call stdevcalc (errdist,maxtp,use4next,trkerr_avg
     &                 ,stderr(ist,ifh),isret)
        if (isret /= 0) then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in FIXCENTER calculating std deviation '
            print *,'!!! for use in next forecast hours errmax.'
            print *,'!!! ist= ',ist,' ifh= ',ifh,' itot4next= '
     &             ,itot4next
          endif

          ifret = 95
        endif
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine avgcalc (xdat,kmax,valid,xavg,iaret)
c
c     ABSTRACT: This subroutine just calculates a straight average of
c     the parameters in the input array (xdat).  The logical array 
c     (valid) indicates whether or not to include a particular array
c     member or not in the calculation.
 
      USE verbose_output

      real      xdat(kmax)
      logical(1) valid(kmax)
c
      iaret = 0
c
      xsum = 0.0
      ict = 0
      do i=1,kmax
        if (valid(i)) then
          xsum = xsum + xdat(i)
          ict = ict + 1
        endif
      enddo 
c
      if (ict > 0) then
        xavg = xsum / float(ict)
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in avgcalc, ict NOT > 0'
        endif

        xavg = xdat(1)
        iaret = 95
      endif
c 
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine wtavrg (xdat,wt,kmax,xwtavg,iwtret)
c
c     ABSTRACT: This subroutine calculates a weighted average of the 
c     parameters in the input array (xdat) using the input weights
c     in the input array (wt).  It is used to calculate the center lat
c     and lon fix positions.
c
      USE verbose_output

      real     xdat(kmax),wt(kmax)
c
      iwtret = 0
c
      xwtavg = 0.0
      wtot = 0.0
      do i=1,kmax
        xwtavg = xwtavg + xdat(i)*wt(i)
        wtot = wtot + wt(i)
      enddo
c
      if (wtot > 0.0) then
        xwtavg = xwtavg / wtot
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in wtavrg, wtot NOT > 0'
        endif

        iwtret = 95
      endif
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine wtavrg_lon (xlon,wt,kmax,xwtavg,iwtret)
c
c     ABSTRACT: This subroutine calculates a weighted average of the
c     parameters in the input array (xlon) using the input weights
c     in the input array (wt).  This subroutine is specifically used
c     to find the center lon fix positions.  It contains code to 
c     account for wrapping around the Greenwich Meridian.
c

      USE verbose_output

      real     xlon(kmax),wt(kmax)
      integer  gt345_ct,lt15_ct
c
      iwtret = 0
      gt345_ct = 0
      lt15_ct  = 0

c     First check to see if we have lons that are both to the left 
c     and the right of the greenwich meridian

      do i = 1,kmax
        if (xlon(i) > 345.) then
          gt345_ct = gt345_ct + 1
        endif
        if (xlon(i) < 15.) then
          lt15_ct = lt15_ct + 1
        endif
      enddo

      if (gt345_ct > 0 .and. lt15_ct > 0) then
        ! We have some lons that are in the 300's (west of the GM), and
        ! some that are in the 0's (east of the GM).  We need to 
        ! standardize these if we want to get a meaningful average.
        do i = 1,kmax
          if (xlon(i) < 15.) then
            xlon(i) = xlon(i) + 360.0
          endif
        enddo
      endif

      xwtavg = 0.0
      wtot = 0.0
      do i=1,kmax
        xwtavg = xwtavg + xlon(i)*wt(i)
        wtot = wtot + wt(i)
      enddo
c
      if (wtot > 0.0) then
        xwtavg = xwtavg / wtot
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in wtavrg_lon, wtot NOT > 0'
        endif

        iwtret = 95
      endif

      if (xwtavg >= 360.0) then
        xwtavg = xwtavg - 360.0
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine stdevcalc (xdat,kmax,valid,xavg,stdx,isret)

      USE verbose_output

      real      xdat(kmax)
      logical(1) valid(kmax)

      isret = 0

      stdx = 0.0
      ict = 0
      do i=1,kmax
        if (valid(i)) then
          stdx = stdx + (xdat(i) - xavg)**2
          ict = ict + 1
        endif
      enddo
 
      if (ict > 0) then
        stdx = sqrt(stdx/float(ict))
        if (stdx == 0.0) then
c         This can happen if you have just 2 points; The mean position
c         will be exactly in the middle of the 2 points and so the
c         standard deviation around that mean point will be 0.  And
c         since the calling routine will quit if the returned standard
c         deviation is 0, we must force it to be 1 so the program
c         continues running.  Theoretically, it could also happen with
c         3 or more points, but the likelihood of the distances working
c         out to exactly equidistant for 3 points is not that good.
          stdx = 1.0
        endif
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in stdevcalc, ict NOT > 0'
        endif

        isret = 95
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_wind_circulation (uvgeslon,uvgeslat,imax,jmax
     &                     ,dx,dy
     &                     ,ist,level,valid_pt,cflag
     &                     ,ctlon,ctlat,fxval,trkrinfo
     &                     ,cmodel_type,maxmin,igwcret)
c
c     ABSTRACT: This subroutine calculates the center fix position for
c     the wind circulation near the storm center.  This center fix is 
c     done differently than for the other parms.  With this fix,
c     we limit the area that is searched.  This subroutine is not
c     called until center fixes have been made for the 5 other parms
c     (z850, z700, zeta850, zeta700, mslp).  Once those  fixes have been
c     made, a modified first guess is made of the average of the 
c     original guess position for this lead time and the 5 other parm 
c     fixes that have already been made for this lead time.  That 
c     modified guess position is passed into this subroutine as uvgeslon
c     and uvgeslat, and that's where the searching for the wind 
c     circulation is centered.
c
c     This subroutine works by converting the winds to Vt and Vr at 
c     each grid point evaluated, relative to each candidate center point
c     that is being evaluated at the time in the loop.  We then compute
c     the circulation at each of 24 azimuths surrounding the storm 
c     center, where circulation = Vt * (length of a 1/24 arc, in meters)  
c     This process is repeated for 7 successive radii and the results 
c     are summed up over all radii, approximating a solid disk 
c     circulation.  The point at which the circulation is maximized 
c     (NHEM) or minimized (SHEM) is the center of circulation.
c
c     grid_maxlat northernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_minlat southernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_maxlon easternmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_minlon westernmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     cmodel_type character, 'global' or 'regional'
c

      USE radii; USE grid_bounds; USE tracked_parms; USE trig_vals
      USE level_parms; USE trkrparms
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo

      integer date_time(8)
      character (len=10) big_ben(3)

      character(*)  cmodel_type,maxmin
      integer, parameter :: numdist=7,numazim=24
      integer imax,jmax,ist,level,igwcret,icvpret,idist,iazim
      real rdist(numdist),vr(numazim,numdist),vt(numazim,numdist)
      real vt_mean(numdist),circul_band(numdist)
      real grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
      real rads,ri,uvgeslon,uvgeslat,dx,dy,ctlon,ctlat,fxval
      real temp_grid_minlon,temp_guesslon,rlatt,rlont,bear
      real targlon,targlat,xintrp_u,xintrp_v,vt_azim_sum,degrees
      real circ_diff,circ_diff_sum,hemisphere,wind_mag_ctr,dist
      real xmin_circ_diff_mean,xmax_circ_diff_mean,tlon,tlat
      real dell,fmax,fmin,grid_buffer,circ_diff_mean
      real circumference,arclength
      real circul_disk,xmax_circul_disk,xmin_circul_disk
      integer ibiret1,ibiret2,igvtret,azimuth_ct,igiret,npts
      integer igibret,bimct
      integer circ_diff_ct,ir,nhalf,bskip1,bskip2,iskip,nlev
      integer ilonfix,jlatfix,ibeg,iend,jbeg,jend,i,j,k,iix,jix
      logical(1) cflag, valid_pt(imax,jmax)

c----------------
c

      print *,' '
      print *,'top of get_wind_circulation, '
      print *,' glatmax= ',glatmax
      print *,' glatmin= ',glatmin
      print *,' glonmax= ',glonmax
      print *,' glonmin= ',glonmin
      print *,' trkrinfo%gridtype= ',trkrinfo%gridtype
      print *,' cmodel_type= ',cmodel_type
      print *,' maxmin= ',maxmin
      print *,' imax= ',imax,'  jmax= ',jmax
      print *,' uvgeslon= ',uvgeslon,'  uvgeslat= ',uvgeslat
      print *,' dx= ',dx,' dy= ',dy,' ist= ',ist
      print *,' cflag= ',cflag
      print *,' ctlon= ',ctlon,' ctlat= ',ctlat
      print *,' fxval= ',fxval
      print *,' igwcret= ',igwcret

      igwcret = 0

      grid_maxlat = glatmax
      grid_minlat = glatmin
      grid_maxlon = glonmax
      grid_minlon = glonmin

      rads = rads_wind_circ
      ri = ri_wind_circ

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'At beg of get_wind_circulation, rads= ',rads
     &       ,' ri= ',ri,' dx= ',dx,' dy= ',dy
      endif

      dell = (dx+dy)/2.
      npts = rads/(dtk*dell)
      fmax  = -1.0e+15; fmin  =  1.0e+15
      ctlon = 0.0; ctlat = 0.0

c     Distances checked and the radial intervals are a function of 
c     the grid resolution....

      if (dell > 0.50) then
        rdist(1) =  50.
        rdist(2) =  85.
        rdist(3) = 120.
        rdist(4) = 155.
        rdist(5) = 190.
        rdist(6) = 225.
        rdist(7) = 260.
      else
        rdist(1) =  35.
        rdist(2) =  65.
        rdist(3) =  95.
        rdist(4) = 125.
        rdist(5) = 155.
        rdist(6) = 185.
        rdist(7) = 215.
      endif

      select case (level)
        case (850);  nlev = nlev850  ! check module level_parms for
        case (700);  nlev = nlev700  ! the values of these....
        case (500);  nlev = nlev500
        case (1020); nlev = levsfc
      end select

      print *,' in get_wind_circulation, nlev= ',nlev

      if (uvgeslat >= 0.0) then
        hemisphere = 1.0
      else
        hemisphere = -1.0
      endif

      call get_ij_bounds (npts,0,ri,imax,jmax,dx,dy
     &             ,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
     &             ,uvgeslon,uvgeslat
     &             ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend
     &             ,igibret)

      if (grid_minlon > 330. .and. grid_maxlon < 30.) then
        ! Our grid is straddling over the GM.  This can happen either
        ! with a global grid or with a regional grid.  How can it happen
        ! for a global grid?  Well, for the case in which this routine
        ! is called from subroutine  get_uv_center, where a smaller
        ! subgrid of data is passed in, and that smaller subgrid may
        ! straddle the GM.  Anyway, we need a workaround.
        ! This workaround will put the minimum longitude
        ! in terms of a negative number, e.g., as opposed to being say,
        ! 354, it will be -6.  You can then leave the grid_maxlon as is.
        temp_grid_minlon = grid_minlon - 360.
        if (uvgeslon > 330.) then
          ! If our grid is straddling the GM and we have adjusted the
          ! grid_minlon to be a negative number, then we also need to
          ! check on the guesslon and adjust it if it is also to west
          ! of the GM.
          temp_guesslon = uvgeslon - 360.
        else
          temp_guesslon = uvgeslon
        endif
      else
        temp_grid_minlon = grid_minlon
        temp_guesslon = uvgeslon
      endif

      if (cmodel_type == 'regional') then
        grid_buffer = 0.30
      else
        grid_buffer = 0.0
      endif

c     For the  wind circulation analysis, we will want to speed things 
c     up for finer resolution grids.  We can do this by skipping some 
c     of the points in the  wind circulation analysis.

      if (dell > 0.20) then
        bskip1 = 1
        bskip2 = 1
      else if (dell > 0.10 .and. dell <= 0.20) then
        bskip1 = 3
        bskip2 = 2
      else if (dell > 0.05 .and. dell <= 0.10) then
        bskip1 = 5
        bskip2 = 3
      else if (dell > 0.03 .and. dell <= 0.05) then
        bskip1 = 8
        bskip2 = 3
      else if (dell <= 0.03) then
        bskip1 = 10
        bskip2 = 4
      endif

c      bskip1 = 1
c      bskip2 = 1

      jix = 0

c      xmin_circ_diff_mean =  9999.0
c      xmax_circ_diff_mean = -9999.0

      xmin_circul_disk =  9999.0
      xmax_circul_disk = -9999.0

      if (verb .ge. 3) then
        print *,' '
        print *,'In get_wind_circulation, prior to first loop, '
        print *,'   npts= ',npts,' dell= ',dell,' rads= ',rads
        print *,' '
      endif

      bimct = 0

      jloop1: do j=-npts,npts,bskip1

        jix = jix + 1
        rlatt = uvgeslat + dell*float(j)

        iix = 0

        iloop1: do i=-npts,npts,bskip1

          iix = iix + 1
          rlont = temp_guesslon + dell*float(i)

c         If any points in the search grid would extend beyond the grid
c         boundaries, then check and see if this is global grid.  If it
c         is, and the extension occurred in the i-direction, then adjust
c         the longitude to allow for grid wrapping.  If it is a regional
c         grid, then just cycle the iloop.  In previous versions of the
c         tracker, we would exit with an error message, but doing it
c         this way allows us to continue tracking some systems that may
c         be close to the grid boundary.  Also, remember to factor in
c         the grid_buffer discussed in the doc block above for this
c         subroutine.

          if (rlont >= (grid_maxlon + dx - grid_buffer)) then
            if (trkrinfo%gridtype == 'global') then
              rlont = rlont - 360.  ! We just GM-wrapped for the full,
                                    ! regular, global grid
            else
              cycle iloop1
            endif
          endif

          if (rlont < (temp_grid_minlon + grid_buffer)) then
            if (trkrinfo%gridtype == 'global') then
              rlont = rlont + 360.  ! We just GM-wrapped for the full,
                                    ! regular, global grid
            else
              cycle iloop1
            endif
          endif

          if (rlatt > (grid_maxlat - grid_buffer) .or.
     &        rlatt < (grid_minlat + grid_buffer)) then
            cycle iloop1
          endif

c         Make sure that the point being investigated here as a
c         potential center has valid data at that point.  That is, for
c         some hires regional grids that have been rotated/converted
c         from a non-latlon grid to a latlon grid, there can be
c         locations within the (i,j) space that do not have valid data
c         at them. It makes no sense to consider a point such as this
c         as a potential center.
c         There is another simpler case here that we are watching out
c         for.  This is simply the case, again for model data where we
c         only have the innermost nest.  Depending on what we choose
c         for the variable "rads" above, with the way that "npts" is
c         defined for these iloops and jloops that we're in, we may be
c         searching over points that are simply well off the grid.
c         Therefore, it is critical to run through this
c         check_valid_point subroutine to make sure that we're not
c         going to inadvertantly be performing an analysis at one of
c         these "off-grid" points.  So.... if the return code from
c         check_valid_point comes back non-zero, simply cycle iloop
c         and go to the next point.

          call check_valid_point (imax,jmax,dx,dy,u(1,1,nlev),maxmin
     &        ,valid_pt,rlont,rlatt,grid_maxlat,grid_minlat,grid_maxlon
     &        ,temp_grid_minlon,trkrinfo,icvpret)

          if (icvpret /= 0) then
            if ( verb .ge. 1 ) then
              print *,'!!! NOT A VALID PT from call in '
              print *,'!!! get_wind_circulation:  icvpret= ',icvpret
            endif
            cycle iloop1
          endif

          call calcdist(rlont,rlatt,temp_guesslon,uvgeslat,dist,degrees)
          if (dist .gt. rads) cycle iloop1

c         Now go through each radius, starting from inner and working
c         to outer, and at each one, go around through all of the 24 
c         discrete azimuths, starting at 7.5 and adding 15 degrees 
c         clockwise each time, all the way up through 352.5.

          vt_mean = 0.0
          vt = 0.0
          vr = 0.0

          circul_band = 0.0
          circul_disk = 0.0

          radiusloop1: do idist = 1,numdist

            azimuth_ct = 0
            vt_azim_sum = 0.0

            ! Compute the length of a 1/numazim arc at this radius, and
            ! be sure to multiply by 1000 to convert from km to m for
            ! use in computing the circulation....

            circumference = 2 * pi * rdist(idist) * 1000.0
            arclength = circumference / float(numazim)

            azimloop1: do iazim = 1,numazim

              bear = ((iazim-1) * 15.) + 7.5

              call distbear (rlatt,rlont,rdist(idist)
     &                      ,bear,targlat,targlon)

ctmwc              if ( verb .ge. 3 ) then
ctmwc                print *,' '
ctmwc                print '(5(a11,f7.2))','  ctr lat= ',rlatt
ctmwc     &               ,'  ctr lon= ',rlont
ctmwc     &               ,'   rdist=  ',rdist(idist),'  targlat= ',targlat
ctmwc     &               ,'  targlon= ',targlon
ctmwc                print '(19x,a10,f7.2,35x,a9,f7.2)',' ctr lon= '
ctmwc     &               ,360.-rlont
ctmwc     &               ,'targlon= ',360.-targlon
ctmwc              endif

              ! These calls to bilin_int_uneven pass a variable "level"
              ! that contains the vertical level to pull the wind data
              ! from, either 850, 700 or surface (which will be 
              ! indicated by a value/code of 1020).
          
              call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,level,'u',xintrp_u
     &            ,valid_pt,bimct,ibiret1)

              call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,level,'v',xintrp_v
     &            ,valid_pt,bimct,ibiret2)

              if (ibiret1 == 0 .and. ibiret2 == 0) then
                call getvrvt (rlont,rlatt,targlon,targlat
     &                       ,xintrp_u,xintrp_v,vr(iazim,idist)
     &                       ,vt(iazim,idist),igvtret)
                azimuth_ct = azimuth_ct + 1
                circul_band(idist) = circul_band(idist)
     &                             + (vt(iazim,idist) * arclength)
c                vt_azim_sum = vt_azim_sum + vt(iazim,idist)
              else if (ibiret1 == 85 .or. ibiret2 == 85) then
                vr(iazim,idist) = -999.0
                vt(iazim,idist) = -999.0
              else
                igwcret = 95
                return
              endif

            enddo azimloop1

            if (azimuth_ct > 0) then
              ! Add the value for the circulation in this radial 
              ! band (circul_band(idist)) to the "solid disk" 
              ! circulation total.  Also, 
              ! Compute azimuthally-averaged Vt at this distance
              circul_disk = circul_disk + circul_band(idist)
              vt_mean(idist) = vt_azim_sum / float(azimuth_ct)
            else
c              vt_mean(idist) = -999.0
              print *,' '
            endif  

          enddo radiusloop1

          if (uvgeslat > 0.0) then
            if (circul_disk > xmax_circul_disk) then
              xmax_circul_disk = circul_disk
              ctlon = rlont
              ctlat = rlatt
            endif
          else
            if (circul_disk < xmin_circul_disk) then
              xmin_circul_disk = circul_disk
              ctlon = rlont
              ctlat = rlatt
            endif
          endif

CC-->      This section was commented out in Feb 2018 due to finding a
cc         bug/flaw in the whole "circulation difference" concept, and
cc         it has now been replaced throughout this subroutine with 
cc         more robust circulation computation logic.
cc
cc         Now get the wind magnitude at the candidate center of 
cc         circulation (i.e., the one that we just used for 
cc         computing all of the Vr and Vt in the previous azimloop1
cc         and radiusloop1).
c    
c          call bilin_int_uneven (rlatt,rlont
c     &        ,dx,dy,imax,jmax,trkrinfo,level,'u',xintrp_u,ibiret1)
c          call bilin_int_uneven (rlatt,rlont
c     &        ,dx,dy,imax,jmax,trkrinfo,level,'v',xintrp_v,ibiret2)
c          if (ibiret1 == 0 .and. ibiret2 == 0) then
c            wind_mag_ctr = sqrt (xintrp_u**2 + xintrp_v**2)
c          else
c            if ( verb .ge. 3 ) then
c              print *,' '
c              print *,'!!! NOTE: bilint_uneven failed for center'
c              print *,'!!! wind mag in get_wind_circulation.'
c              print *,'!!! rlont= ',rlont,' rlatt= ',rlatt
c              print *,'!!!  '
c            endif
c          endif
c
cc          print *,' '
cc          print *,'1st run,  wind_mag_ctr= ',wind_mag_ctr
c
c          circ_diff_ct  = 0
c          circ_diff_sum = 0.0
c          do ir = 1,numdist
cc              print *,'1st run, ir= ',ir,'  vtmean(ir)= ',vt_mean(ir)
c            if (vt_mean(ir) > -998.0) then
c              circ_diff = vt_mean(ir) - (hemisphere * wind_mag_ctr)
c              circ_diff_ct  = circ_diff_ct + 1
c              circ_diff_sum = circ_diff_sum + circ_diff
c            endif
c          enddo
c
c          print *,'1st run,  circ_diff_ct= ',circ_diff_ct
c
c          if (circ_diff_ct > 0) then
c
c            circ_diff_mean = circ_diff_sum / float(circ_diff_ct)
c
cc            print *,'1st run,  circ_diff_sum= ',circ_diff_sum
cc     &             ,'  circ_diff_mean= ',circ_diff_mean
c
c            if (uvgeslat > 0) then
c              if (circ_diff_mean > xmax_circ_diff_mean) then
c                xmax_circ_diff_mean = circ_diff_mean
c                ctlon = rlont
c                ctlat = rlatt
c              endif
c            else
c              if (circ_diff_mean < xmin_circ_diff_mean) then
c                xmin_circ_diff_mean = circ_diff_mean
c                ctlon = rlont
c                ctlat = rlatt
c              endif
c            endif
c          endif
c
cc          print *,'1st run,  xmax_circ_diff_mean= '
cc     &           ,xmax_circ_diff_mean
c
c          if (uvgeslat > 0) then
c            if (circ_diff_mean > xmax_circ_diff_mean) then
c              xmax_circ_diff_mean = circ_diff_mean
c              ctlon = rlont
c              ctlat = rlatt
c            endif
c          else
c            if (circ_diff_mean < xmin_circ_diff_mean) then
c              xmin_circ_diff_mean = circ_diff_mean
c              ctlon = rlont
c              ctlat = rlatt
c            endif
c          endif

        enddo iloop1

      enddo jloop1

      if (bimct > 0) then
        if (verb .ge. 3) then
          print *,' '
          print *,'Warning summary: From sub get_wind_circulation,'
          print *,'calls to sub bilin_int_uneven resulted in'
          print *,'(blocked) attempts to access points outside the'
          print *,'bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in jloop1 = ',bimct
          print *,' '
        endif
      endif

      if (uvgeslat >= 0.0) then
        write (6,61) 360.-ctlon,ctlat,xmax_circul_disk
      else
        write (6,63) 360.-ctlon,ctlat,xmin_circul_disk
      endif

  61  format (' After first run, Wind Circulation (NHEM) ctlon= ',f8.3
     &       ,'W  ctlat= ',f8.3,'  xmax_circul_disk = ',f15.1)
  63  format (' After first run, Wind Circulation (SHEM) ctlon= ',f8.3
     &       ,'W  ctlat= ',f8.3,'  xmin_circul_disk = ',f15.1)
 
c     If nhalf is specified as 0, then don't go through any more 
c     iterations of this routine, just exit with the value that we 
c     already got the first time through the loop, above.

      if (dell > 0.50) then
        nhalf = 4
      else if (dell > 0.20 .and. dell <= 0.50) then
        nhalf = 3
      else if (dell > 0.10 .and. dell <= 0.20) then
        nhalf = 2
      else if (dell > 0.05 .and. dell <= 0.10) then
        nhalf = 1
      else if (dell <= 0.05) then
c        nhalf = 0
        nhalf = 1
c        if ( verb .ge. 3 ) then        
c          print *,' '
c          print *,'In get_wind_circulation, dell is < 0.05 deg, so '
c          print *,'nhalf is set to 0 and only the first iteration of'
c          print *,'the search loop is done.'
c          print *,'  dell= ',dell,'  nhalf= ',nhalf
c        endif
      endif

      if (nhalf < 1) then
        if (uvgeslat > 0.0) then
          fxval = xmax_circul_disk
        else
          fxval = xmin_circul_disk
        endif
        return
      endif

c     If on our first pass through, we were dealing with a regional grid
c     that straddled the GM, then it becomes (for now) too much of a
c     coding hassle to deal with in the rest of this routine (i.e., in
c     all the nhalf iterations), so we will just go with the first run
c     through for the center fix and exit the routine.

      if (grid_minlon > 330. .and. grid_maxlon < 30.) then
        if (uvgeslat > 0.0) then
          fxval = xmax_circul_disk
        else
          fxval = xmin_circul_disk
        endif
        return
      endif          
          
c     ---------------------------------------------------------------
c     ---------------------------------------------------------------
c     Halve the grid spacing to refine the location and value of the
c     max/min value, but restrict the area of the new search grid.

c      npts = npts/2
      npts = max(npts,1)

c     -------------------------------------------------------------
c     First, recalculate the i and j beginning and ending points to
c     be used in the  barnes analysis subroutine.  Only do this once
c     for this grid-refinement (even though the grid is redefined 
c     nhalf times in this subroutine), but make sure to have the
c     possible search grid be big enough to allow the possibility of
c     the grid shifting way right or way left each time through the
c     loop (get_ij_bounds takes care of this).  Cut the value of 
c     rads in half (only do this once) so that any points beyond 
c     rads/2 are not considered as potential centers.

      rads = 0.5 * rads

      call get_ij_bounds (npts,nhalf,ri,imax,jmax,dx,dy
     &              ,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
     &              ,ctlon,ctlat,trkrinfo
     &              ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igibret)

      if (igibret /= 0) then
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_wind_circulation from call to '
          print *,'!!! get_ij_bounds just before nhalf loop.  '
          print *,'!!! Stopping processing for storm number ',ist
        endif
        igwcret = 92
        return
      endif

c     --------------------------------------------------------------
c     Now do the actual searching for the max/min value

      bimct = 0

      kloop: do k = 1,nhalf

        call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                     ,date_time)
        if ( verb .ge. 3 ) then
          write (6,32) k,date_time(5),date_time(6),date_time(7)
 32       format (1x,'TIMING: get_wind_circ kloop, k= ',i2,'   '
     &         ,i2.2,':',i2.2,':',i2.2)
        endif

        dell = 0.5*dell
        tlon = ctlon
        tlat = ctlat
c        xmin_circ_diff_mean =  9999.0
c        xmax_circ_diff_mean = -9999.0
        xmin_circul_disk =  9999.0
        xmax_circul_disk = -9999.0
        
        iskip = bskip2

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'get_wind_circ nhalf loop, k= ',k
          write (6,161) tlon,360.-tlon,tlat
          print *,'ilonfix= ',ilonfix,' jlatfix= ',jlatfix
     &         ,' npts= ',npts
          print *,'ibeg= ',ibeg,' jbeg= ',jbeg,' imax= ',imax
          print *,'iend= ',iend,' jend= ',jend,' jmax= ',jmax
          print *,'nhalf= ',nhalf,' iskip= ',iskip,' rads= ',rads
        endif

        if (verb .ge. 3) then
          print *,' '
          print *,'In get_wind_circulation, prior to loop k= ',k
          print *,'   npts= ',npts,' dell= ',dell,' rads= ',rads
          print *,' '
        endif

        jloop2: do j=-npts,npts,iskip

          rlatt = tlat + dell*float(j)

          iloop2: do i=-npts,npts,iskip

            rlont = tlon + dell*float(i)

            if (rlont >= (grid_maxlon + dx - grid_buffer)) then
              if (trkrinfo%gridtype == 'global') then
                rlont = rlont - 360.
              else
                cycle iloop2
              endif
            endif

            if (rlont < (grid_minlon + grid_buffer)) then
              if (trkrinfo%gridtype == 'global') then
                rlont = rlont + 360.
              else
                cycle iloop2
              endif
            endif

            if (rlatt > (grid_maxlat - grid_buffer) .or.
     &          rlatt < (grid_minlat + grid_buffer) .or.
     &          rlont >= (grid_maxlon + dx - grid_buffer) .or.
     &          rlont < (grid_minlon + grid_buffer)) then
              cycle iloop2
            endif

c           Again, check and make sure that the lat/lon point in
c           question here has valid data (see the explanation further
c           up in this subroutine inside iloop).

            call check_valid_point (imax,jmax,dx,dy,u(1,1,nlev),maxmin
     &          ,valid_pt,rlont,rlatt,grid_maxlat,grid_minlat
     &          ,grid_maxlon,grid_minlon,trkrinfo,icvpret)

            if (icvpret /= 0) then
              cycle iloop2
            endif

            call calcdist(rlont,rlatt,temp_guesslon,uvgeslat,dist
     &                   ,degrees)
            if (dist .gt. rads) cycle iloop2

c           Now go through each radius, starting from inner and working
c           to outer, and at each one, go around through all of the 24
c           discrete azimuths, starting at 7.5 and adding 15 degrees
c           clockwise each time, all the way up through 352.5.

            vt_mean = 0.0
            vt = 0.0
            vr = 0.0

            circul_band = 0.0
            circul_disk = 0.0

            radiusloop2: do idist = 1,numdist

              azimuth_ct = 0
              vt_azim_sum = 0.0

              ! Compute the length of a 1/numazim arc at this radius, and
              ! be sure to multiply by 1000 to convert from km to m for
              ! use in computing the circulation....

              circumference = 2 * pi * rdist(idist) * 1000.0
              arclength = circumference / float(numazim)

              azimloop2: do iazim = 1,numazim

                bear = ((iazim-1) * 15.) + 7.5

                call distbear (rlatt,rlont,rdist(idist)
     &                        ,bear,targlat,targlon)

ctmwc                if ( verb .ge. 3 ) then
ctmwc                  print *,' '
ctmwc                  print '(5(a11,f7.2))','  ctr lat= ',rlatt
ctmwc     &                 ,'  ctr lon= ',rlont
ctmwc     &                 ,'   rdist=  ',rdist(idist),'  targlat= ',targlat
ctmwc     &                 ,'  targlon= ',targlon
ctmwc                  print '(19x,a10,f7.2,35x,a9,f7.2)',' ctr lon= '
ctmwc     &                 ,360.-rlont
ctmwc     &                 ,'targlon= ',360.-targlon
ctmwc                endif

                call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,level,'u',xintrp_u
     &            ,valid_pt,bimct,ibiret1)
  
                call bilin_int_uneven (targlat,targlon
     &            ,dx,dy,imax,jmax,trkrinfo,level,'v',xintrp_v
     &            ,valid_pt,bimct,ibiret2)

                if (ibiret1 == 0 .and. ibiret2 == 0) then
                  call getvrvt (rlont,rlatt,targlon,targlat
     &                         ,xintrp_u,xintrp_v,vr(iazim,idist)
     &                         ,vt(iazim,idist),igvtret)
                  azimuth_ct = azimuth_ct + 1
                  circul_band(idist) = circul_band(idist)
     &                               + (vt(iazim,idist) * arclength)
c                  vt_azim_sum = vt_azim_sum + vt(iazim,idist)
                else if (ibiret1 == 85 .or. ibiret2 == 85) then
                  vr(iazim,idist) = -999.0
                  vt(iazim,idist) = -999.0
                else
                  igwcret = 95
                  return
                endif
 
              enddo azimloop2

              if (azimuth_ct > 0) then
                ! Add the value for the circulation in this radial
                ! band (circul_band(idist)) to the "solid disk"
                ! circulation total.  Also,
                ! Compute azimuthally-averaged Vt at this distance
                circul_disk = circul_disk + circul_band(idist)
                vt_mean(idist) = vt_azim_sum / float(azimuth_ct)
              else
c                vt_mean(idist) = -999.0
                print *,' '
              endif

            enddo radiusloop2

            if (uvgeslat > 0.0) then
              if (circul_disk > xmax_circul_disk) then
                xmax_circul_disk = circul_disk
                ctlon = rlont
                ctlat = rlatt
              endif
            else
              if (circul_disk < xmin_circul_disk) then
                xmin_circul_disk = circul_disk
                ctlon = rlont
                ctlat = rlatt
              endif
            endif

CC-->      This section was commented out in Feb 2018 due to finding a
cc         bug/flaw in the whole "circulation difference" concept, and
cc         it has now been replaced throughout this subroutine with 
cc         more robust circulation computation logic.
cc
cc           Now get the wind magnitude at the candidate center of
cc           circulation (i.e., the one that we just used for
cc           computing all of the Vr and Vt in the previous azimloop2
cc           and radiusloop2).
c
c            call bilin_int_uneven (rlatt,rlont
c     &          ,dx,dy,imax,jmax,trkrinfo,level,'u',xintrp_u,ibiret1)
c            call bilin_int_uneven (rlatt,rlont
c     &          ,dx,dy,imax,jmax,trkrinfo,level,'v',xintrp_v,ibiret2)
c            if (ibiret1 == 0 .and. ibiret2 == 0) then
c              wind_mag_ctr = sqrt (xintrp_u**2 + xintrp_v**2)
c            else
c              if ( verb .ge. 3 ) then
c                print *,' '
c                print *,'!!! NOTE: bilint_uneven failed for center'
c                print *,'!!! wind mag in get_wind_circulation.'
c                print *,'!!! rlont= ',rlont,' rlatt= ',rlatt
c                print *,'!!! '
c              endif
c            endif
c
cc            print *,'kloop k= ',k,'  wind_mag_ctr= ',wind_mag_ctr
c
c            circ_diff_ct  = 0
c            circ_diff_sum = 0.0
c            do ir = 1,numdist
cc              print *,'kloop k= ',k,'  vtmean(ir)= ',vt_mean(ir)
c              if (vt_mean(ir) > -998.0) then
c                circ_diff = vt_mean(ir) - (hemisphere * wind_mag_ctr)
c                circ_diff_ct  = circ_diff_ct + 1
c                circ_diff_sum = circ_diff_sum + circ_diff
c              endif
c            enddo
c
cc            print *,'kloop k= ',k,'  circ_diff_ct= ',circ_diff_ct
c
c            if (circ_diff_ct > 0) then
c
c              circ_diff_mean = circ_diff_sum / float(circ_diff_ct)
c
cc              print *,'kloop k=',k,'  circ_diff_sum= ',circ_diff_sum
cc     &               ,'  circ_diff_mean= ',circ_diff_mean
c
c              if (uvgeslat > 0.0) then
c                if (circ_diff_mean > xmax_circ_diff_mean) then
c                  xmax_circ_diff_mean = circ_diff_mean
c                  ctlon = rlont
c                  ctlat = rlatt
c                endif
c              else
c                if (circ_diff_mean < xmin_circ_diff_mean) then
c                  xmin_circ_diff_mean = circ_diff_mean
c                  ctlon = rlont
c                  ctlat = rlatt
c                endif
c              endif
c            endif
c
cc            print *,'kloop k= ',k,'  xmax_circ_diff_mean= '
cc     &             ,xmax_circ_diff_mean

          enddo iloop2

        enddo jloop2

        if ( verb .ge. 3 ) then
          if (uvgeslat >= 0.0) then
            print *,'---> xmax_circul_disk= ',xmax_circul_disk
            write (6,71) k,360.-ctlon,ctlat,xmax_circul_disk
          else
            print *,'---> xmin_circul_disk= ',xmin_circul_disk
            write (6,73) k,360.-ctlon,ctlat,xmin_circul_disk
          endif
        endif

      enddo kloop

      if (bimct > 0) then
        if (verb .ge. 3) then
          print *,' '
          print *,'Warning summary: From sub get_wind_circulation,'
          print *,'calls to sub bilin_int_uneven resulted in'
          print *,'(blocked) attempts to access points outside the'
          print *,'bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in kloop = ',bimct
          print *,' '
        endif
      endif

  71  format (' nhalf get_wind_circ, k= ',i2,' ctlon= ',f8.3,'W '
     &       ,'  ctlat= ',f8.3,' Wind Circulation (NHEM: Max) = '
     &       ,f15.1)
  73  format (' nhalf get_wind_circ, k= ',i2,' ctlon= ',f8.3,'W '
     &       ,'  ctlat= ',f8.3,' Wind Circulation (SHEM: Min) = '
     &       ,f15.1)
 161  format (' guesslon= ',f8.3,'E  (',f8.3,'W)   guesslat= ',f8.3)

      if (uvgeslat > 0.0) then
        fxval = xmax_circul_disk
      else
        fxval = xmin_circul_disk
      endif
c
      return
      end

c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_uv_center (uvgeslon,uvgeslat,imax,jmax,dx,dy
     &                     ,ist,level,valid_pt,cflag
     &                     ,ctlon,ctlat,xval,trkrinfo,igucret)
c
c     ABSTRACT: This subroutine calculates the center fix position for
c     the minimum in the wind speed near the storm center.  This center
c     fix is done differently than for the other parms.  With this fix,
c     we severely limit the area that is searched, because we do not 
c     want to confuse a wind minimum out on the periphery of a storm 
c     with the center wind minimum.  Therefore, this subroutine is not
c     called until center fixes have been made for the 5 other parms
c     (z850, z700, zeta850, zeta700, mslp).  Once those  fixes have been
c     made, a modified first guess is made of the average of the guess
c     position for this time and the 5 other parm fixes.  That modified
c     guess position is passed into this subroutine as uvgeslon and 
c     uvgeslat, and that's where the searching for the wind minimum 
c     is done.  To get the wind minimum, the u and v data are first 
c     interpolated down to a fine grid (see details below for exact
c     figures), and then a single-pass barnes analysis is done on that
c     fine grid.  The reason that we first interpolate the data (which
c     is different from how we do the other parms) is that if we just 
c     use the original grid resolution, we may not be able to 
c     accurately pick out a minimum in the wind field at the center.
c
      USE radii; USE grid_bounds; USE tracked_parms; USE trig_vals
      USE level_parms; USE trkrparms
      USE verbose_output

      type (trackstuff) trkrinfo

      real, allocatable ::  uold(:,:),vold(:,:),unew(:,:),vnew(:,:)
      real, allocatable ::  rlonold(:),rlatold(:),rlonnew(:),rlatnew(:)
      real, allocatable ::  vmag(:,:)
      real :: dx,dy
      real  :: grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
      character*1 ::   gotlat
      logical(1)        cflag, valid_pt(imax,jmax)
      logical(1), allocatable :: lbi(:,:)
c
      gotlat = 'n'
c
c     -----------------------------------------------------------------
c     INTERPOLATE INPUT GRID TO SMALLER GRID
c     -----------------------------------------------------------------
c
c     Get beginning and ending j points (on the input grid) for a 
c     smaller array that surrounds the storm.  It is this smaller array
c     that we will interpolate to a finer grid.
c
c     Calculate number of pts to either side of this j to search
c
      npts = ceiling(rads_vmag/(dtk*((dx+dy)/2.)))
c
      call get_ij_bounds (npts,0,ritrk_vmag,imax,jmax,dx,dy
     &             ,glatmax,glatmin,glonmax,glonmin,uvgeslon,uvgeslat
     &             ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if ( verb .ge. 3 ) then  
        print *,' '
        print *,' After get_ij D, ibeg jbeg = ',ibeg,jbeg
        print *,' After get_ij D, iend jend = ',iend,jend
      endif

      if (igiret /= 0) then

        if ( verb .ge. 1 ) then  
          print *,' '
          print *,'!!! ERROR in get_uv_center from call to '
          print *,'!!! get_ij_bounds, stopping processing for '
          print *,'!!! storm number ',ist
        endif

        igucret = 92
        return
      endif

      if (ibeg < 1) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then  
            print *,' '
            print *,'!!! NOTE: In get_uv_center, the ibeg returned from'
            print *,'!!! get_ij_bounds is < 1, but our gridtype is '
            print *,'!!! global, so we are going to leave it as is and '
            print *,'!!! account for the grid wrapping.'
            print *,' '
          endif

        else

          if ( verb .ge. 3 ) then  
            print *,' '
            print *,'!!! NOTE: In get_uv_center, the ibeg returned from'
            print *,'!!! get_ij_bounds is < 1, and our gridtype is NOT'
            print *,'!!! global, so we are going to redefine ibeg to 1.'
            print *,' '
          endif

          ibeg = 1
        endif
      endif

      if (jbeg < 1) jbeg = 1

      if (ibeg > imax .or. jbeg > jmax .or. jbeg < 1 .or.
     &    iend < 1 .or. jend < 1) then

        if ( verb .ge. 1 ) then  
          print *,' '
          print *,'ERROR in get_uv_center calculating ibeg, iend, jbeg'
          print *,'or jend.  ibeg= ',ibeg,' iend= ',iend,' jbeg= ',jbeg
          print *,'jend= ',jend
          print *,'uv center will not be calculated for this time.'
        endif

        igrret = 99
        return
      endif

      if (iend > imax) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then  
            print *,' '
            print *,'!!! NOTE: In get_uv_center, the iend returned from'
            print *,'!!! get_ij_bounds is > imax, but our gridtype is '
            print *,'!!! global, so we are going to leave it as is and'
            print *,'!!! account for the grid wrapping.'
            print *,' '
          endif

        else

          if ( verb .ge. 3 ) then  
            print *,' '
            print *,'!!! NOTE: In get_uv_center, the iend returned from'
            print *,'!!! get_ij_bounds is > imax, and our gridtype is'
            print *,'!!! NOT global, so we will redefine iend to imax.'
            print *,' '
          endif

          iend = imax
        endif
      endif

      if (jend > jmax) jend = jmax

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'In get_uv_center, ibeg= ',ibeg,' iend= ',iend
        print *,'                  jbeg= ',jbeg,' jend= ',jend
        print *,'  ilonfix= ',ilonfix,' jlatfix= ',jlatfix
      endif
c
      select case (level)
        case (850);  nlev = nlev850  ! check module level_parms for
        case (700);  nlev = nlev700  ! the values of these....
        case (500);  nlev = nlev500
        case (1020); nlev = levsfc
      end select

c     This next if statement determines how many times to interpolate
c     the input grid to a smaller grid.  Here are the grid sizes for 
c     some of the typical grids that will be used:
c
c      Original grid size     # of interps        Final grid size
c     --------------------    ------------     ---------------------
c     1.00 deg (111.19 km)        3             0.125 deg (13.9 km)
c     1.25 deg (138.99 km)        3             0.156 deg (17.4 km)
c     2.50 deg (277.99 km)        4             0.156 deg (17.4 km)

      if ((dx+dy)/2. > 1.2) then
        numinterp = 4
      else if ((dx+dy)/2. > 0.50 .and. (dx+dy)/2. <= 1.2) then
        numinterp = 3
      else if ((dx+dy)/2. > 0.25 .and. (dx+dy)/2. <= 0.50) then
        numinterp = 2
      else if ((dx+dy)/2. > 0.10 .and. (dx+dy)/2. <= 0.25) then
        numinterp = 1
      else if ((dx+dy)/2. <= 0.10) then
        numinterp = 0
      endif
  
      dell = (dx+dy)/2.
      imxold = iend - ibeg + 1
      jmxold = jend - jbeg + 1

c     --------------------------------------------------------------
c     Before interpolating, make sure that all the original
c     points have valid data.  If they don't then exit the
c     subroutine.  NOTE: This is NOT checking to see if ALL the pts
c     on the complete & full input grid have valid data; it only
c     checks those points that are within the box returned from
c     get_ij_bounds.

      do i=ibeg,iend

        if (i > imax) then
          if (trkrinfo%gridtype == 'global') then
            ip = i - imax   ! If wrapping past GM
          else

            if ( verb .ge. 1 ) then  
              print *,' '
              print *,'!!! ERROR: In get_uv_center, the '
              print *,'!!!    user-requested eastern search boundary'
              print *,'!!!    is beyond the eastern bounds of '
              print *,'!!!    this regional grid.  '
              print *,'!!!    PROCESSING WILL STOP.  '
              print *,'!!!    Subroutine location A....'
              print *,'!!!         '
              print *,'!!!   imax of regional grid    = ',imax
              print *,'!!!   User-requested eastern i = ',i
              print *,' '
            endif

            stop 94
          endif
        else
          ip = i
        endif

        if (i < 1) then
          if (trkrinfo%gridtype == 'global') then
            ip = i + imax   ! If wrapping past GM
          else

            if ( verb .ge. 1 ) then  
              print *,' '
              print *,'!!! ERROR: i < 1 in subroutine  get_uv_center'
              print *,'!!! for a non-global grid.  STOPPING....'
              print *,'!!! i= ',i
              print *,' '
            endif

            stop 97
          endif
        endif

        do j=jbeg,jend
          if (.not. valid_pt(ip,j)) goto 975
        enddo

      enddo

c     ------------------------------------
c     Now begin the interpolation process

      allocate (uold(imxold,jmxold),stat=iuo)
      allocate (vold(imxold,jmxold),stat=ivo)
      allocate (rlonold(imxold),stat=iloo)
      allocate (rlatold(jmxold),stat=ilao)
      if (iuo /= 0 .or. ivo /= 0 .or. iloo /= 0 .or. ilao /= 0) goto 970
 
      do intnum = 1,numinterp

        if (intnum == 1) then

          do i=ibeg,iend

            ik = i

            if (i < 1) then
              if (trkrinfo%gridtype == 'global') then
                ik = i + imax  !GM wrapping
              else

                if ( verb .ge. 1 ) then  
                  print *,'!!! ERROR in get_uv_center, i < 1'
                  print *,'!!! for a non-global grid at AA.'
                  print *,'!!! i = ',i
                endif

                igucret = 92
                return
              endif
            endif

            if (i > imax) then 
              if (trkrinfo%gridtype == 'global') then
                ik = i - imax  !GM wrapping
              else

                if ( verb .ge. 1 ) then  
                  print *,'!!! ERROR in get_uv_center, i > imax'
                  print *,'!!! for a non-global grid at AA.'
                  print *,'!!! i = ',i,' imax= ',imax
                endif

                igucret = 92
                return
              endif
            endif

            rlonold(i-ibeg+1) = glon(ik)
            do j=jbeg,jend
              uold(i-ibeg+1,j-jbeg+1) = u(ik,j,nlev)
              vold(i-ibeg+1,j-jbeg+1) = v(ik,j,nlev)
              if (gotlat == 'n') then
                rlatold(j-jbeg+1) = glat(j)
              endif
            enddo
            gotlat = 'y'    ! Only need to fill rlatold once
          enddo

        else

          deallocate (uold); deallocate (vold)
          deallocate (rlonold); deallocate (rlatold)
          allocate (uold(imxnew,jmxnew),stat=iuo)
          allocate (vold(imxnew,jmxnew),stat=ivo)
          allocate (rlonold(imxnew),stat=iloo)
          allocate (rlatold(jmxnew),stat=ilao)
          if (iuo /= 0 .or. ivo /= 0 .or.
     &        iloo /= 0 .or. ilao /= 0) goto 970

          gotlat = 'n'
          do i=1,imxnew
            rlonold(i) = rlonnew(i)
            do j=1,jmxnew
              uold(i,j) = unew(i,j)
              vold(i,j) = vnew(i,j)
              if (gotlat == 'n') then
                rlatold(j) = rlatnew(j)
              endif
            enddo
            gotlat = 'y'
          enddo

          imxold = imxnew
          jmxold = jmxnew
          deallocate (unew); deallocate (vnew)
          deallocate (rlonnew); deallocate (rlatnew)

        endif
 
        dell = 0.5 * dell
        imxnew = 2 * imxold - 1
        jmxnew = 2 * jmxold - 1

        allocate (unew(imxnew,jmxnew),stat=iuo)
        allocate (vnew(imxnew,jmxnew),stat=ivo)
        allocate (rlonnew(imxnew),stat=iloo)
        allocate (rlatnew(jmxnew),stat=ilao)
        if (iuo /= 0 .or. ivo /= 0 .or. 
     &      iloo /= 0 .or. ilao /= 0) goto 971

        call bilin_int_even (imxold,jmxold,uold
     &                      ,imxnew,jmxnew,unew,ibiret)
        call bilin_int_even (imxold,jmxold,vold
     &                      ,imxnew,jmxnew,vnew,ibiret)
c        call lin_int (imxold,imxnew,rlonold,rlonnew,iliret)
        call lin_int_lon (imxold,imxnew,rlonold,rlonnew,iliret)
        call lin_int     (jmxold,jmxnew,rlatold,rlatnew,iliret)
 
        chk_lonspc_old = rlonold(imxold) - rlonold(imxold - 1)
        chk_latspc_old = rlatold(jmxold) - rlatold(jmxold - 1)
        chk_lonspc_new = rlonnew(imxnew) - rlonnew(imxnew - 1)
        chk_latspc_new = rlatnew(jmxnew) - rlatnew(jmxnew - 1)

        grid_maxlat = rlatnew(1)
        grid_minlat = rlatnew(jmxnew)
        grid_minlon = rlonnew(1)
        grid_maxlon = rlonnew(imxnew)
        
        if ( verb .ge. 3 ) then  
          print *,' '
          print *,'In get_uv_center, intnum= ',intnum
          print *,'imxold= ',imxold,' imxnew= ',imxnew
          print *,'jmxold= ',jmxold,' jmxnew= ',jmxnew
          print *,'Grid boundaries of modified uv grid: '
          print *,'grid_maxlat= ',grid_maxlat,' grid_minlat= '
     &           ,grid_minlat
          print *,'grid_maxlon= ',grid_maxlon,' grid_minlon= '
     &           ,grid_minlon
        endif
 
      enddo
 
c     ------------------

      deallocate (uold); deallocate (vold)
      deallocate (rlonold); deallocate(rlatold)

      if (numinterp == 0) then

        ! No interpolations were done for this fine mesh grid, but we
        ! need to fill some of these arrays and define variables for
        ! subsequent subroutine calls just below here that require
        ! the variables imxnew, jmxnew, and the arrays unew and vnew.

        if (iend > imax) then
          if (trkrinfo%gridtype == 'global') then
            continue
          else

            if ( verb .ge. 1 ) then  
              print *,' '
              print *,'ERROR in get_uv_center:  Should not have gotten'
              print *,'to this point in get_uv_center for a regional '
              print *,'grid; iend should not > imax here !!!'
            endif

            igucret = 99
            return
          endif
        endif

        if (ibeg < 1) then
          if (trkrinfo%gridtype == 'global') then
            continue
          else

            if ( verb .ge. 1 ) then  
              print *,' '
              print *,'ERROR in get_uv_center:  Should not have gotten'
              print *,'to this point in get_uv_center for a regional'
              print *,'grid; ibeg should not < 1 here !!!'
            endif

            igucret = 99
            return
          endif
        endif

        imxnew = iend - ibeg + 1
        jmxnew = jend - jbeg + 1
        allocate (unew(imxnew,jmxnew),stat=iuo)
        allocate (vnew(imxnew,jmxnew),stat=ivo)
        allocate (rlonnew(imxnew),stat=iloo)
        allocate (rlatnew(jmxnew),stat=ilao)
        if (iuo /= 0 .or. ivo /= 0 .or.
     &      iloo /= 0 .or. ilao /= 0) goto 971
        gotlat = 'n'

        do i=ibeg,iend

          ip = i

          if (i > imax) then
            ! This HAS to be a global, wrapping grid, or else the if
            ! statement a few lines up would have caught this already.
            ip = i - imax   ! Wrapping past GM
          endif

          if (i < 1) then
            ! This HAS to be a global, wrapping grid, or else the if
            ! statement a few lines up would have caught this already.
            ip = i + imax   ! Wrapping past GM
          endif

          rlonnew(i-ibeg+1) = glon(ip)
          do j=jbeg,jend
            unew(i-ibeg+1,j-jbeg+1) = u(i,j,nlev)
            vnew(i-ibeg+1,j-jbeg+1) = v(i,j,nlev)
            if (gotlat == 'n') then
              rlatnew(j-jbeg+1) = glat(j)
            endif
          enddo
          gotlat = 'y'    ! Only need to fill rlatnew once
        enddo

      endif

      grid_maxlat = rlatnew(1)
      grid_minlat = rlatnew(jmxnew)
      grid_minlon = rlonnew(1)
      grid_maxlon = rlonnew(imxnew)

      if ( verb .ge. 3 ) then  
        print *,'Grid boundaries of modified uv grid in get_uv_center:'
        print *,'grid_maxlat= ',grid_maxlat,' grid_minlat= ',grid_minlat
        print *,'grid_maxlon= ',grid_maxlon,' grid_minlon= ',grid_minlon
      endif

      allocate (vmag(imxnew,jmxnew),stat=ivm)
      allocate (lbi(imxnew,jmxnew),stat=ilb)
      if (ivm /= 0 .or. ilb /= 0) goto 972
      call calc_vmag (unew,vnew,imxnew,jmxnew,vmag,icvret)
      deallocate (unew); deallocate (vnew)
 
      lbi = .TRUE.

      if ( verb .ge. 3 ) then  
        print *,' '
        print *,'Before call to find_maxmin, imxnew= ',imxnew
     &       ,'jmxnew= ',jmxnew,' ist= ',ist
        write (6,171) dell,uvgeslon,360.-uvgeslon,uvgeslat
 171    format (' dell= ',f7.3,' uvgeslon= ',f8.3,'E  (',f8.3,'W)'
     &       ,'  uvgeslat= ',f8.3)
      endif

c     Note that in the next call, I pass the 'global' argument to 
c     find_maxmin.  This defines what type of grid it is, so that the
c     proper grid_buffer can be chosen.  This grid_buffer is designed
c     to avoid having a center be chosen too close to the grid 
c     boundary.  However, in the case of vmag here, we are only using
c     a small subgrid, and we want to make sure we use *all* points
c     in that subgrid for searching, and that will occur if we set that
c     calling argument to 'global' as opposed to 'regional'.

      call find_maxmin (imxnew,jmxnew,dell,dell,'vmag'
     &   ,vmag,'min',ist,uvgeslon,uvgeslat,rlonnew,rlatnew,lbi
     &   ,trkrinfo,cflag,ctlon,ctlat,xval,grid_maxlat,grid_minlat
     &   ,grid_maxlon,grid_minlon,'global',ifmret)
      deallocate (vmag); deallocate (lbi)
      deallocate (rlonnew); deallocate (rlatnew)
c
      if (ifmret == 0) then
        goto 995
      else
        igucret = ifmret

        if ( verb .ge. 1 ) then  
          print *,' '
          print *,'!!! ERROR in get_uv_center in call to find_maxmin'
          print *,'!!! storm num = ',ist,' igucret = ',igucret
        endif

        goto 998
      endif
c
 970  continue
        
      if ( verb .ge. 1 ) then  
        print *,' '
        print *,'!!! ERROR ALLOCATING either uold, vold,'
        print *,'!!! rlonold or rlatold in get_uv_center'
        print *,'!!! Storm number = ',ist
        print *,'!!! intnum= ',intnum
        print *,'!!! imxnew= ',imxnew,' jmxnew= ',jmxnew
        print *,'!!! imxold= ',imxold,' jmxold= ',jmxold
        print *,'!!! iuo= ',iuo,' ivo= ',ivo
        print *,'!!! iloo= ',iloo,' ilao= ',ilao
      endif

      igucret = 97
      goto 998

 971  continue

      if ( verb .ge. 1 ) then  
        print *,' '
        print *,'!!! ERROR ALLOCATING either unew, vnew,'
        print *,'!!! rlonnew or rlatnew in get_uv_center'
        print *,'!!! Storm number = ',ist
        print *,'!!! intnum= ',intnum
        print *,'!!! imxnew= ',imxnew,' jmxnew= ',jmxnew
        print *,'!!! imxold= ',imxold,' jmxold= ',jmxold
        print *,'!!! iuo= ',iuo,' ivo= ',ivo
        print *,'!!! iloo= ',iloo,' ilao= ',ilao
      endif

      igucret = 97
      goto 998

 972  continue
      
      if ( verb .ge. 1 ) then  
        print *,' '
        print *,'!!! ERROR ALLOCATING either vmag or lbi in '
        print *,'!!! subroutine  get_uv_center'
        print *,'!!! Storm number = ',ist
        print *,'!!! imxnew= ',imxnew,' jmxnew= ',jmxnew
        print *,'!!! ivm= ',ivm,' ilb= ',ilb
      endif

      igucret = 97
      goto 998

 975  continue
      
      if ( verb .ge. 1 ) then  
        print *,' '
        print *,'!!! Inside  get_uv_center, at least one of the points'
        print *,'!!! is not a valid data point.  This point may be '
        print *,'!!! outside the valid data bounds of a regional grid'
        print *,'!!! i= ',i,' j= ',j
        print *,'!!! Storm number = ',ist
      endif

      igucret = 98
      goto 998
c
  995 continue
      igucret = 0 
c
  998 continue
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_uv_guess (guesslon,guesslat,clon,clat
     &                       ,calcparm,ist,ifh,maxstorm
     &                       ,uvgeslon,uvgeslat,igugret)
c
c     ABSTRACT: The purpose of this subroutine is to get a modified 
c               first guess lat/lon position before searching for the 
c               minimum in the wind field.  The reason for doing this is
c               to better refine the guess and avoid picking up a wind
c               wind minimum far away from the center.  So, use the 
c               first guess position (and give it strong weighting), and
c               then also use the  fix positions for the current time
c               (give the vorticity centers stronger weighting as well),
c               and then take the average of these positions.
c
c     INPUT:
c     guesslon  guess longitude for this forecast time 
c     guesslat  guess latitude for this forecast time 
c     clon      array with center longitude  fixes for the various parms
c     clat      array with center latitude  fixes for the various parms
c     calcparm  logical; tells whether or not a parm has a valid fix
c                   at this forecast hour
c     ist       index for current storm
c     ifh       index for current forecast hour
c     maxstorm  max # of storms that can be handled
c
c     OUTPUT:
c     uvgeslon  contains modified guess longitude position at which to
c                   look for the wind minimum
c     uvgeslat  contains modified guess latitude position at which to
c                   look for the wind minimum
c     igugret   return code for this subroutine (0=normal)
c----
c
      USE set_max_parms; USE level_parms; USE error_parms
      USE verbose_output

      logical(1) calcparm(maxtp,maxstorm)
      real      clon(maxstorm,maxtime,maxtp)
      real      clat(maxstorm,maxtime,maxtp)
      real      uvgeslon, uvgeslat
      real      guesslon,guesslat,degrees
      integer   gt345_ct,lt15_ct

      sumlon = 0.0
      sumlat = 0.0
      ict = 0
      gt345_ct = 0
      lt15_ct  = 0

c     NOTE: We need to be careful in this routine when averaging
c     the longitudes together, in case we cross the greenwich 
c     meridian, because then we may be averaging 345+ lons with 
c     lons that are less than 15, giving incorrect results.
c     Therefore, check for this, and if it occurs, add 360 onto
c     any of the <15 lons (add it twice for those lons being 
c     counted twice (guesslon and the vorticity centers)).

c     Weight the uv guess position by counting the storm's guess 
c     position twice.  

      sumlon = sumlon + 2.*guesslon
      sumlat = sumlat + 2.*guesslat
      ict = ict + 2

      if (guesslon > 345.) then
        gt345_ct = gt345_ct + 1 
      endif
      if (guesslon < 15.) then
        lt15_ct  = lt15_ct + 2   ! Yes, 2 is correct....
      endif

      do ip = 1,maxtp
        if ((ip > 2 .and. ip < 7) .or. ip == 10) then
          cycle   ! because 3-6 are for 850 & 700 u & v and 10 is 
                  ! for surface wind magnitude.
        else
          if (calcparm(ip,ist)) then
            call calcdist (guesslon,guesslat,clon(ist,ifh,ip)
     &                    ,clat(ist,ifh,ip),dist,degrees)
 
            if (dist < uverrmax) then
c
c             Give the vorticity centers 2x weighting as well
c 
              if (ip == 1 .or. ip == 2 .or. ip == 11) then
                sumlon = sumlon + 2.*clon(ist,ifh,ip)
                sumlat = sumlat + 2.*clat(ist,ifh,ip)
                ict = ict + 2
                if (clon(ist,ifh,ip) > 345.) then
                  gt345_ct = gt345_ct + 1       
                endif                           
                if (clon(ist,ifh,ip) < 15.) then
                  lt15_ct  = lt15_ct + 2  ! Yes, 2 is correct...
                endif
              else
                sumlon = sumlon + clon(ist,ifh,ip)
                sumlat = sumlat + clat(ist,ifh,ip)
                ict = ict + 1
                if (clon(ist,ifh,ip) > 345.) then
                  gt345_ct = gt345_ct + 1       
                endif                           
                if (clon(ist,ifh,ip) < 15.) then
                  lt15_ct  = lt15_ct + 1  ! Only 1 for non-zeta parms
                endif
              endif

            endif

          endif
        endif
      enddo
c 
      if (ict > 0) then
        if (gt345_ct > 0 .and. lt15_ct > 0) then
          ! We have some parms left of the GM and some to the right,
          ! so we will add (360*lt15_ct) to the sum of the lons (sumlon)
          uvgeslon = (sumlon + (360.*float(lt15_ct)))/ ict
        else
          uvgeslon = sumlon / ict
        endif
        if (uvgeslon >= 360.0) then
          uvgeslon = uvgeslon - 360.
        endif
        uvgeslat = sumlat / ict
        igugret = 0
      else

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in get_uv_guess, ict not > 0, ict= ',ict
          print *,'!!! vmag center will not be calculated for this'
          print *,'!!! storm -- at least not at this level'
          print *,'!!! Storm number = ',ist
        endif

        igugret = 91
      endif
c
      return
      end      
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine calc_vmag (xu,xv,imx,jmx,wspeed,icvret)
c
c     ABSTRACT: This subroutine calculates the magnitude of the wind
c     speed for an array of points, given real u and real v arrays.
c
      real    xu(imx,jmx),xv(imx,jmx),wspeed(imx,jmx)
c
      do i=1,imx
        do j=1,jmx
          wspeed(i,j) = sqrt( xu(i,j)*xu(i,j) + xv(i,j)*xv(i,j) )
        enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine bilin_int_even (imxold,jmxold,xold
     &                          ,imxnew,jmxnew,xnew,ibiret)
c
c     ABSTRACT: This subroutine does a bilinear interpolation on a 
c     grid of evenly spaced data.  Do NOT attempt to use this subroutine
c     with data that are not evenly spaced or you will get unpredictable
c     results.
c
      real      xold(imxold,jmxold), xnew(imxnew,jmxnew)
c
c
c  ---------------------------------------------------------------------
c         Latitude ---->          |
c                                 |
c  L   O  e  O  e  O  e  O  e  O  | O: original point from input array
c  o                              | 
c  n   e  1  2  1  2  1  2  1  e  | 1: interpolated, primary inter. pt
c  g                              |
c  i   O  2  O  2  O  2  O  2  O  | e: interpolated edge point
c  t                              |
c  u   e  1  2  1  2  1  2  1  e  | 2: interpolated, secondary inter. pt
c  d                              |
c  e   O  2  O  2  O  2  O  2  O  | Interpolations are done in the order
c                                 | as indicated above; First, the input
c  |   e  1  2  1  2  1  2  1  e  | 'O' pts are placed onto the new, 
c  |                              | larger grid. From that, the '1' pts
c  |   O  2  O  2  O  2  O  2  O  | can be interpolated.  Next, the edge
c  |                              | (e) pts are interpolated using an
c  v   e  1  2  1  2  1  2  1  e  | interpolation of two 'O' pts and one
c                                 | '1' pt.  Finally, the '2' pts are
c      O  e  O  e  O  e  O  e  O  | done using the 2 surrounding '0' and
c                                 | '1' pts.  Bilinear interpolation is
c                                 | made incredibly easier by the fact
c                                 | that the grid is evenly spaced.
c  ---------------------------------------------------------------------
c     NOTE: Remember that the arrays that are read in are indexed as
c     (lon,lat), so that in the diagram above, pt (1,1) is at the upper
c     left and pt (imax,jmax) is at the lower right, and each column is
c     a new latitude and each row is a new longitude.
c
c     -----------------------------------------------------------------
c     Put original (O) values from input array into new, expanded array
c     -----------------------------------------------------------------
c
      do i=1,imxold
        do j=1,jmxold
          xnew(2*i-1,2*j-1) = xold(i,j) 
        enddo
      enddo
c
c     ----------------------------------------------
c     Interpolate to get primary interior (1) points
c     ----------------------------------------------
c
      do i=1,imxold-1
        do j=1,jmxold-1
          xnew(2*i,2*j) = 0.25 * (xnew(2*i-1,2*j-1) + xnew(2*i+1,2*j-1)
     &                        +  xnew(2*i+1,2*j+1) + xnew(2*i-1,2*j+1))
        enddo
      enddo
c
c     ---------------------------
c     Interpolate edge (e) points
c     ---------------------------
c
c     ... Northernmost 'e' points ...
c
      j=1
      do i=1,imxold-1
        xnew(2*i,j) = 0.3333 * (xnew(2*i-1,j) + xnew(2*i+1,j) 
     &                                        + xnew(2*i,2))
      enddo
c
c     ... Southernmost 'e' points ...
c
      j = 2*jmxold - 1
      do i=1,imxold-1
        xnew(2*i,j) = 0.3333 * (xnew(2*i-1,j) + xnew(2*i+1,j)
     &                                        + xnew(2*i,j-1))
      enddo
c
c     ... Westernmost 'e' points ...
c
      i=1
      do j=1,jmxold-1
        xnew(i,2*j) = 0.3333 * (xnew(i,2*j-1) + xnew(i,2*j+1)
     &                                        + xnew(2,2*j))
      enddo
c
c     ... Easternmost 'e' points ...
c
      i = 2*imxold - 1
      do j=1,jmxold-1
        xnew(i,2*j) = 0.3333 * (xnew(i,2*j-1) + xnew(i,2*j+1)
     &                                        + xnew(i-1,2*j))
      enddo
c
c     ------------------------------------------------
c     Interpolate to get secondary interior (2) points
c     ------------------------------------------------
c
      do j=2,2*jmxold-2
        istep = mod(j+1,2)
        do i=istep+2,2*imxold-2,2
          xnew(i,j) = 0.25 * (xnew(i-1,j) + xnew(i,j-1) + xnew(i+1,j)
     &                     +  xnew(i,j+1))
        enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine lin_int (ioldmax,inewmax,xold,xnew,iliret)
c
c     ABSTRACT: This subroutine linearly interpolates evenly spaced
c               data from one grid to another.
c 
      real      xold(ioldmax), xnew(inewmax)
c
c     First just copy points from old grid onto new, larger grid
c
      do i=1,ioldmax
        xnew(2*i-1) = xold(i)
      enddo
c
c     Now interpolate to get the in-between points
c
      do i=1,ioldmax-1
        xnew(2*i) = 0.5 * (xnew(2*i-1) + xnew(2*i+1))
      enddo
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine lin_int_lon (ioldmax,inewmax,xold,xnew,iliret)
c
c     ABSTRACT: This subroutine linearly interpolates evenly spaced
c               data from one grid to another.  This particular 
c               routine is specifically used for interpolating 
c               longitudes, and it factors in the possibility of 
c               interpolating across the greenwich meridian.
c
      real      xold(ioldmax), xnew(inewmax)
c
c     First just copy points from old grid onto new, larger grid
c
      do i=1,ioldmax
        xnew(2*i-1) = xold(i)
      enddo
c
c     Now interpolate to get the in-between points, and make the
c     necessary adjustment when interpolating a longitude between,
c     for example, 359.5 and 0.0.
c
      do i=1,ioldmax-1
        if (xnew(2*i-1) > 350. .and. xnew(2*i+1) < 10.) then
          xnew(2*i) = 0.5 * (xnew(2*i-1) + (360. + xnew(2*i+1)))
        else
          xnew(2*i) = 0.5 * (xnew(2*i-1) + xnew(2*i+1))
        endif
      enddo
c
      return
      end

c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_zeta_values (fixlon,fixlat,imax,jmax,dx,dy
     &                     ,trkrinfo,imeanzeta,igridzeta,readflag
     &                     ,valid_pt,ist,ifh,maxstorm,inp,igzvret)
c
c     ABSTRACT: This subroutine finds the maximum and mean zeta values
c     at 850 & 700 mb, near a storm center.  It is called from 
c     subroutine  tracker, and its purpose is to report these values 
c     that will then be written out to a special, modified version of 
c     the atcfunix file.

      USE tracked_parms; USE radii; USE trig_vals; USE set_max_parms
      USE trkrparms; USE level_parms; USE grid_bounds; USE inparms
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo
      type (datecard) inp

      logical(1) readflag(14),valid_pt(imax,jmax),compflag
      character  cmaxmin*3,cvort_maxmin*3
      real     fixlon(maxstorm,maxtime),fixlat(maxstorm,maxtime)
      real     gridpoint_maxmin,xmeanzeta,dx,dy,re,ri,parmlon,parmlat
      integer  igridzeta(nlevgrzeta),imeanzeta(nlevgrzeta)
      integer  n,ix1,ix2,ilev,npts,imax,jmax,igzvret,ilonfix,jlatfix
      integer  idum,jdum,ibeg,jbeg,iend,jend,igiret,icount,iuret
      integer  ifilret,ist,ifh,ifmret,maxstorm

c     First, call  get_ij_bounds in order to get the (i,j) coordinates
c     of the (fixlon,fixlat) position that we need to search around.
c     These (i,j) coordinates are returned as ilonfix and jlatfix.

      npts = imax * jmax

      call get_ij_bounds (npts,0,ridlm,imax,jmax
     &     ,dx,dy,glatmax,glatmin,glonmax,glonmin
     &     ,fixlon(ist,ifh),fixlat(ist,ifh),trkrinfo
     &     ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (ilonfix > imax) then
        if (trkrinfo%gridtype == 'global') then
          ilonfix = ilonfix - imax   ! If wrapping past GM
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: In get_zeta_values, the '
            print *,'!!!    user-requested eastern boundary'
            print *,'!!!    is beyond the eastern bounds of '
            print *,'!!!    this regional grid.  '
            print *,'!!!         '
            print *,'!!!   imax of regional grid    = ',imax
            print *,'!!!   eastern ilonfix = ',ilonfix
            print *,'!!!         '
            print *,' '
          endif
          
          igzvret = 99
          return
        endif   
      endif

      if (ilonfix < 1) then
        if (trkrinfo%gridtype == 'global') then
          ilonfix = ilonfix + imax
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: ilonfix < 1 in subroutine'
            print *,'!!! get_zeta_values for a non-global grid.'
            print *,'!!! ilonfix= ',ilonfix
            print *,'!!!         '
            print *,' '
          endif

          igzvret = 99
          return
        endif   
      endif

      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,601) 
        write (6,603) 
        write (6,605) 
        write (6,607) 
        write (6,609) 
        write (6,*) ' '
        write (6,613) ist,ifh
        write (6,615) fixlon(ist,ifh),360.-fixlon(ist,ifh)
     &               ,fixlat(ist,ifh)
        write (6,617) ilonfix,jlatfix
      endif

 601  format(1x,'#---------------------------------------------------#')
 603  format(1x,'# Entering loop to determine the mean and gridpoint #')
 605  format(1x,'# max zeta values at 850 and 700 mb for the purpose #')
 607  format(1x,'# of reporting them on the modified atcfunix file.  #')
 609  format(1x,'#---------------------------------------------------#')
 613  format(1x,'--- In get_zeta_values, ist= ',i3,'  ifh= ',i3)
 615  format(1x,'    Fix location for this time: ',f7.2,'E  (',f6.2,'W)'
     &     ,2x,f7.2)
 617  format(1x,'    ilonfix= ',i4,'  jlatfix= ',i4)
        
      report_zeta_loop: do n=1,2

        gridpoint_maxmin = -99.0
        xmeanzeta = -99.0
        compflag  = .true.

        select case (n)
          case (1); ilev=850  ! For 850 mb
          case (2); ilev=700  ! For 700 mb
        end select

        if (zeta(ilonfix,jlatfix,n) > -9990.0) then

          ! -------------------------------------------
          ! We have valid zeta data for this level, so
          ! we first call  barnes now to get the mean zeta
          ! surrounding our found center position.
          ! -------------------------------------------

          if (fixlat(ist,ifh) > 0.0) then
            cvort_maxmin = 'max'
          else
            cvort_maxmin = 'min'
          endif

          call find_maxmin (imax,jmax,dx,dy,'zeta'
     &       ,zeta(1,1,n),cvort_maxmin,ist,fixlon(ist,ifh)
     &       ,fixlat(ist,ifh),glon,glat,valid_pt,trkrinfo
     &       ,compflag,parmlon,parmlat,xmeanzeta
     &       ,glatmax,glatmin,glonmax,glonmin,inp%modtyp,ifmret)
          if (ifmret == 0) then   ! Out of regional grid bounds
            imeanzeta(n) = int ((xmeanzeta * 1e6) + 0.5)
          else
            imeanzeta(n) = -99
            igridzeta(n) = -99

            if ( verb .ge. 3 ) then
              write (6,*) ' '
              write (6,519) 
              write (6,520) 
              write (6,521) 
            endif

 519        format (1x,' The call to find_maxmin in get_zeta_values')
 520        format (1x,' returned a nonzero return code.  The search')
 521        format (1x,' for zeta values will not be done.')
            exit report_zeta_loop  ! If out of grid bounds at 850, 
                                   ! then will also be out at 700...
          endif 
        else
            imeanzeta(n) = -99
            igridzeta(n) = -99
            exit report_zeta_loop
        endif

        if ( verb .ge. 3 ) then
          write (6,621) n,ilev,xmeanzeta,imeanzeta(n)
 621      format (1x,'+++ RPT_MEAN_ZETA: n= ',i2,' lev= ',i4
     &         ,' xmeanzeta= ',f9.6,'  imeanzeta (*1e6)= ',i8)
          write (6,*) '  --- mean zeta raw = ',xmeanzeta
        endif
     
        ! -----------------------------------------------
        ! Call fix_latlon_to_ij to get the nearest actual
        ! raw (grid) zeta data value, not the mean value.
        ! -----------------------------------------------
     
        call fix_latlon_to_ij (imax,jmax,dx,dy
     &     ,zeta(1,1,n),cvort_maxmin,valid_pt,fixlon(ist,ifh)
     &     ,fixlat(ist,ifh),xmeanzeta,idum,jdum
     &     ,gridpoint_maxmin,'tracker'
     &     ,glatmax,glatmin,glonmax,glonmin
     &     ,trkrinfo,ifilret)
        if (ifilret == 0) then
          igridzeta(n) = int ((gridpoint_maxmin * 1e6) + 0.5)
        else
          igridzeta(n) = -99
        endif

        if ( verb .ge. 3 ) then
          write (6,623) n,ilev,gridpoint_maxmin,igridzeta(n),ifilret
 623      format (1x,'+++ RPT_GRID_ZETA: n= ',i2,' lev= ',i4
     &         ,' grid zeta= ',f9.6,'  igrid zeta (*1e6)= ',i8
     &         ,' ifilret= ',i3)
          write (6,*) '  --- grid zeta raw= ',gridpoint_maxmin
        endif
    
      enddo report_zeta_loop

      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,631) 
        write (6,633)
        write (6,635)
        write (6,*) ' '
      endif
        
 631  format(1x,'#---------------------------------------------------#')
 633  format(1x,'# End of loop to get 850 & 700 zeta for atcf file.  #')
 635  format(1x,'#---------------------------------------------------#')

      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine find_maxmin (imax,jmax,dx,dy,cparm,fxy,maxmin,ist
     &             ,guesslon,guesslat,rlonv,rlatv,valid_pt,trkrinfo
     &             ,compflag,ctlon,ctlat,xval,grid_maxlat,grid_minlat
     &             ,grid_maxlon,grid_minlon,cmodel_type,ifmret)
c
c     This routine  finds the location (clon,clat) of and value of the
c     the max or min of fxy in the vicinity of slon,slat.  The value of
c     the input flag maxmin determines whether to look for a max or a
c     min value.  The max/min is determined by finding the point which 
c     gives the max/min value of a single point barnes analysis of fxy 
c     with e-folding radius re (km) and influence radius ri (km). The 
c     initial search is restricted to a radius rads around the point 
c     (slon,slat) on a grid with lon,lat spacing dx and dy. The location
c     is refined by reducing the spacing of the search grid by a factor
c     of two, nhalf times.
c
c     INPUT:
c     imax     Num pts in i direction on input grid
c     jmax     Num pts in j direction on input grid
c     dx       Grid spacing in i-direction on input grid
c     dy       Grid spacing in j-direction on input grid
c     cparm    Char string indicating what parm is being passed in
c     fxy      Real array of data values
c     maxmin   Char string indicating whether to search for a max or min
c     ist      Number of the storm being processed
c     guesslon Guess longitude of the storm
c     guesslat Guess latitude of the storm
c     rlonv    Array containing longitude values of input grid points
c     rlatv    Array containing latitude values of input grid points
c     valid_pt Logical bitmap masking non-valid grid points.  This is a
c              concern for the regional models, which are interpolated 
c              from Lam-Conf or NPS grids onto lat/lon grids, leaving 
c              grid points around the edges which have no valid data.
c     trkrinfo derived type detailing user-specified grid info
c     grid_maxlat northernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_minlat southernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_maxlon easternmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_minlon westernmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     cmodel_type character, 'global' or 'regional'
c
c     INPUT/OUTPUT:
c     compflag Logical; continue processing this storm or not (would be
c              set to FALSE if, for example, the guess position is 
c              outside the domain of a regional grid)
c
c     OUTPUT:
c     ctlon    Center longitude of storm found for this parameter
c     ctlat    Center latitude of storm found for this parameter
c     xval     Max or Min value found at the (ctlon,ctlat)
c     ifmret   Return code from this subroutine
c
c     UPDATE DEC 2009: For the HFIP HRH testing, it was found that 
c     due to the very limited domain size of some of the models, the 
c     barnes scheme was allowing points close to the grid boundaries
c     to erroneously be selected as the center point.  We add in a 
c     buffer (grid_buffer) here to prevent this from occurring.

      USE radii; USE grid_bounds; USE set_max_parms; USE level_parms
      USE trig_vals; USE trkrparms
      USE verbose_output

      implicit none
c
      type (trackstuff) trkrinfo

      character(*)  maxmin,cparm,cmodel_type
      logical(1)    compflag, valid_pt(imax,jmax)
      real    fxy(imax,jmax),rlonv(imax),rlatv(jmax)
      real    ctlon,ctlat,degrees,dx,dy,guesslon,guesslat,xval
      real    rads,re,ri,dell,fmax,fmin,rlatt,rlont,dist,ftemp,ritmp
      real    vmag_latmax,vmag_latmin,vmag_lonmax,vmag_lonmin,retmp
      real    tlon,tlat,grid_buffer,temp_grid_minlon,temp_guesslon
      real    grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
      integer imax,jmax,ist,bskip1,bskip2,iskip,ifmret,npts,maxvgrid
      integer ibeg,iend,jbeg,jend,ilonfix,jlatfix,igiret,icount,iret
      integer ibct,ibarnes_loopct,i,j,k,iix,jix,jvlatfix,ivlonfix
      integer nhalf,icvpret
      integer date_time(8)
      character (len=10) big_ben(3)
c
      ifmret = 0
      nhalf = 5
c
c     -----------------------------------------------------------
c     Set initial parms for use in find_maxmin.
c     Different radii used for V magnitude than for other parms, 
c     see discussion in module radii for more details.
c
      if (cparm == 'vmag') then

c       NOTE: The maxvgrid variable determines what size grid to send 
c             to subroutine  barnes. e.g., maxvgrid = 8 means send an 
c             8x8 grid; maxvgrid = 12 means send a 12x12 grid.  For 
c             ultra-fine mesh grids (finer than 0.04 deg, or 1/25 deg),
c             we expand to 12 in order to sample a few more points
c             around each grid point.

        if ((dx+dy)/2. > 0.04) then
          maxvgrid = 8
        else
          maxvgrid = 12
        endif

        rads = rads_vmag; re = retrk_vmag; ri = ritrk_vmag
        re = (float(maxvgrid)/4) * ((dx+dy)/2. * dtk) ! Basically, this
c               sets re equal to half the distance from the gridpoint
c               in question to the farthest point that will be
c               sampled when the (maxvgrid x maxvgrid) grid is passed
c               on to subroutine  barnes.  Thus, just ignore the 
c               parameter retrk_vmag, and use this instead.
      else if ((dx+dy)/2. < 1.26 .and. (dx+dy)/2. >= 0.40) then
        rads = rads_most; re = retrk_most; ri = ritrk_most
      else if ((dx+dy)/2. < 0.40 .and. (dx+dy)/2. >= 0.10) then
        rads = rads_fine; re = retrk_most; ri = ritrk_most
      else if ((dx+dy)/2. < 0.10) then
        rads = rads_hres; re = retrk_hres; ri = ritrk_most
      else
        rads = rads_coarse; re = retrk_coarse; ri = ritrk_coarse
      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'At beg of find_maxmin, rads= ',rads,' re= ',re 
     &       ,' ri= ',ri,' cparm= ',cparm,' dx= ',dx,' dy= ',dy
      endif

      dell = (dx+dy)/2.
      npts = rads/(dtk*dell)
      fmax  = -1.0e+12; fmin  =  1.0e+12
      ctlon = 0.0; ctlat = 0.0

      if (npts == 0) npts = 1

c     For the  barnes analysis, we will want to speed things up for
c     finer resolution grids.  We can do this by skipping some of 
c     the points in the  barnes analysis.

      if (dell > 0.20) then
        bskip1 = 2
        bskip2 = 1
      else if (dell > 0.10 .and. dell <= 0.20) then
        bskip1 = 4
        bskip2 = 2
      else if (dell > 0.05 .and. dell <= 0.10) then
        bskip1 = 6
        bskip2 = 3
      else if (dell > 0.03 .and. dell <= 0.05) then
        bskip1 = 10
        bskip2 = 5
      else if (dell <= 0.03) then
        bskip1 = 15
        bskip2 = 5
      endif

      if (cparm == 'vmag') then
        bskip1 = 1
        bskip2 = 1
      endif

c     If input parm is vmag, we've already done the minimizing by 
c     interpolating to the fine mesh grid, so we'll simply send the 
c     bounds that were input to this subroutine to barnes
c     as boundaries for the array to search.  For all other parms, 
c     however, no minimizing has been done yet, so we need to call 
c     get_ij_bounds to set the boundaries for a much smaller grid that
c     surrounds the storm (as opposed to having subroutine  barnes 
c     search the entire global grid).

      if (cparm == 'vmag') then

        if ( verb .ge. 3 ) then
          print *,'In find_maxmin, jmax= ',jmax,' imax= ',imax
        endif

        ibeg=1; jbeg=1; iend=imax; jend=jmax
        vmag_latmax = rlatv(1)    ! N-most lat of vmag subgrid
        vmag_latmin = rlatv(jmax) ! S-most lat of vmag subgrid
        vmag_lonmin = rlonv(1)    ! W-most lon of vmag subgrid
        vmag_lonmax = rlonv(imax) ! E-most lon of vmag subgrid

        if ( verb .ge. 3 ) then
          write (6,44) vmag_latmax,vmag_lonmin,360.-vmag_lonmin
     &                ,imax,jmax
          write (6,46) vmag_latmin,vmag_lonmax,360.-vmag_lonmax
        endif

 44     format (' vmag_latmax= ',f8.3,' vmag_lonmin= ',f8.3
     &         ,'E  (',f8.3,'W)  imax= ',i4,' jmax= ',i4)
 46     format (' vmag_latmin= ',f8.3,' vmag_lonmax= ',f8.3
     &         ,'E  (',f8.3,'W)')

        if (vmag_lonmin > 330. .and. vmag_lonmax < 30.) then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! WARNING: For a case of find_maxmin, our vmag'
            print *,'!!! subgrid is straddling the GM.  The code should'
            print *,'!!! be able to handle this, but if strange errors'
            print *,'!!! are occurring, check into the code either here'
            print *,'!!! in find_maxmin or get_uv_ctr.'
            print *,' '
          endif
        endif

        npts = ceiling(rads/(dtk*dell))

      else

        call get_ij_bounds (npts,0,ri,imax,jmax,dx,dy
     &             ,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
     &             ,guesslon,guesslat
     &             ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

        if (igiret /= 0) then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in find_maxmin from call to '
            print *,'!!! get_ij_bounds, stopping processing for'
            print *,'!!! storm number ',ist
          endif

          ifmret = 92
          return
        endif

      endif

c
c     ---------------------------------------------------------------
c
      if ( verb .ge. 3 ) then
        print *,' '
        write (6,39) guesslon,360.-guesslon,guesslat
 39     format (' guesslon= ',f8.3,'E  (',f8.3,'W)   guesslat= ',f8.3)
        if (cparm == 'vmag') then
          print *,'ilonfix= (unused) jlatfix= (unused)'
     &       ,' npts= ',npts
          print *,'ilonfix and jlatfix are meaningless for computing'
          print *,'vmag, so ignore the large values you see for them.'
        else 
          print *,'ilonfix= ',ilonfix,' jlatfix= ',jlatfix
     &       ,' npts= ',npts
        endif
        print *,'ibeg= ',ibeg,' jbeg= ',jbeg,' imax= ',imax
        print *,'iend= ',iend,' jend= ',jend,' jmax= ',jmax
      endif
        
      call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &     ,date_time)

      if ( verb .ge. 3 ) then
        write (6,31) date_time(5),date_time(6),date_time(7)
 31     format (1x,'TIMING: find_maxmin 1  ',i2.2,':',i2.2,':',i2.2)
      endif

      ibct=0
      ibarnes_loopct = 0

      if (grid_minlon > 330. .and. grid_maxlon < 30.) then
        ! Our grid is straddling over the GM.  This can happen either
        ! with a global grid or with a regional grid.  How can it happen
        ! for a global grid?  Well, for the case in which this routine
        ! is called from subroutine  get_uv_center, where a smaller
        ! subgrid of data is passed in, and that smaller subgrid may
        ! straddle the GM.  Anyway, we need a workaround.
        ! This workaround will put the minimum longitude
        ! in terms of a negative number, e.g., as opposed to being say,
        ! 354, it will be -6.  You can then leave the grid_maxlon as is.
        temp_grid_minlon = grid_minlon - 360.
        if (guesslon > 330.) then
          ! If our grid is straddling the GM and we have adjusted the
          ! grid_minlon to be a negative number, then we also need to
          ! check on the guesslon and adjust it if it is also to west
          ! of the GM.
          temp_guesslon = guesslon - 360.
        else
          temp_guesslon = guesslon
        endif
      else
        temp_grid_minlon = grid_minlon
        temp_guesslon = guesslon
      endif

      jix = 0

      if (cmodel_type == 'regional') then
        grid_buffer = 0.30
      else
        grid_buffer = 0.0
      endif

      jloop: do j=-npts,npts,bskip1

        jix = jix + 1
        rlatt = guesslat + dell*float(j)

        iix = 0

c        vlat(jix) = rlatt

        iloop: do i=-npts,npts,bskip1

          iix = iix + 1
          rlont = temp_guesslon + dell*float(i)

c          if (cparm == 'vmag') then
c            print *,' '
c            print '(a16,i6,a4,i6,2(a8,f8.3),a12,f8.3)'
c     &            ,'in find_max, i= ',i
c     &            ,' j= ',j,' rlatt= ',rlatt,' rlont= ',rlont
c     &            ,' 360-rlont= ',360.-rlont
c          endif

c         If any points in the search grid would extend beyond the grid
c         boundaries, then check and see if this is global grid.  If it
c         is, and the extension occurred in the i-direction, then adjust
c         the longitude to allow for grid wrapping.  If it is a regional
c         grid, then just cycle the iloop.  In previous versions of the
c         tracker, we would exit with an error message, but doing it
c         this way allows us to continue tracking some systems that may
c         be close to the grid boundary.  Also, remember to factor in
c         the grid_buffer discussed in the doc block above for this 
c         subroutine.

          if (rlont >= (grid_maxlon + dx - grid_buffer)) then
            if (trkrinfo%gridtype == 'global') then
              if (cparm == 'vmag') then
                cycle iloop  ! We are off the small vmag subgrid
              else
                rlont = rlont - 360.  ! We just GM-wrapped for the full,
                                      ! regular, global grid
              endif
            else
              cycle iloop
            endif
          endif

          if (rlont < (temp_grid_minlon + grid_buffer)) then
            if (trkrinfo%gridtype == 'global') then
              if (cparm == 'vmag') then
                cycle iloop  ! We are off the small vmag subgrid
              else
                rlont = rlont + 360.  ! We just GM-wrapped for the full,
                                      ! regular, global grid
              endif
            else
              cycle iloop
            endif
          endif

          if (rlatt > (grid_maxlat - grid_buffer) .or. 
     &        rlatt < (grid_minlat + grid_buffer)) then
            cycle iloop
          endif

c         Make sure that the point being investigated here as a
c         potential center has valid data at that point.  That is, for
c         some hires regional grids that have been rotated/converted
c         from a non-latlon grid to a latlon grid, there can be
c         locations within the (i,j) space that do not have valid data
c         at them. It makes no sense to consider a point such as this
c         as a potential center.
c         There is another simpler case here that we are watching out
c         for.  This is simply the case, again for model data where we
c         only have the innermost nest.  Depending on what we choose
c         for the variable "rads" above, with the way that "npts" is
c         defined for these iloops and jloops that we're in, we may be
c         searching over points that are simply well off the grid.
c         Therefore, it is critical to run through this
c         check_valid_point subroutine to make sure that we're not
c         going to inadvertantly be performing an analysis at one of
c         these "off-grid" points.  So.... if the return code from
c         check_valid_point comes back non-zero, simply cycle iloop
c         and go to the next point.

          call check_valid_point (imax,jmax,dx,dy,fxy,maxmin,valid_pt
     &        ,rlont,rlatt,grid_maxlat,grid_minlat,grid_maxlon
     &        ,temp_grid_minlon,trkrinfo,icvpret)

          if (icvpret /= 0) then
            if ( verb .ge. 1 ) then
              print *,'!!! NOT A VALID PT:  icvpret= ',icvpret
            endif

            cycle iloop
          endif

          call calcdist(rlont,rlatt,temp_guesslon,guesslat,dist,degrees)
          if (dist .gt. rads) cycle iloop

          if (cparm == 'vmag') then

c           This next bit of code gets the ij coordinates for an 8x8 
c           box around the current point under consideration. These ij
c           coordinates are sent to barnes so that barnes only loops 
c           64 times, as opposed to nearly 10,000 if the whole 97x97
c           array were sent.  So, fix rlatt to the grid point just 
c           northward of rlatt and fix rlont to the grid point just 
c           eastward of rlont.  Note that this makes for a modified 
c           barnes analysis in that we're  sort of specifying ahead of
c           time exactly which grid points will be included and we'll
c           be excluding some points that would be near the periphery
c           of each (rlont,rlatt)'s range, but as long as we're consis-
c           tent and do it this way for each point, it's well worth the
c           trade-off in cpu time.  Parameter maxvgrid determines what 
c           size array to send to barnes (maxvgrid=8 means 8x8)

            jvlatfix = int((vmag_latmax - rlatt)/dy + 1.)
            ivlonfix = int((rlont - temp_grid_minlon)/dx + 2.)
c            ivlonfix = int((rlont - vmag_lonmin)/dx + 2.)

            ibeg = ivlonfix - (maxvgrid/2)
            iend = ivlonfix + (maxvgrid/2 - 1)
            jbeg = jvlatfix - (maxvgrid/2 - 1)
            jend = jvlatfix + (maxvgrid/2)

            if (ibeg < 1 .or. jbeg < 1 .or.
     &          iend > imax .or. jend > jmax) then

              ! DO NOT quit if we find a boundary outside the grid 
              ! bounds.  Rather, just set the J violating bound(s) to
              ! the min or max limit, and for I bounds, allow the 
              ! program to continue down to subsequent code below,
              ! provided it's a global grid.

c             print *,'!!! '
c             print *,'!!! Before vmag adjustments, boundaries are: '
c             print *,'!!! rlont= ',rlont,' rlatt= ',rlatt,' dx= ',dx
c             print *,'!!! temp_grid_minlon= ',temp_grid_minlon
c             print *,'!!! vmag_latmax= ',vmag_latmax
c             print *,'!!! ivlonfix = ',ivlonfix,' jvlatfix = ',jvlatfix
c             print *,'!!! ibeg= ',ibeg,' iend= ',iend,' imax= ',imax
c             print *,'!!! jbeg= ',jbeg,' jend= ',jend,' jmax= ',jmax

              if (ibeg < 1) then
                if (trkrinfo%gridtype == 'global') then
                  continue   ! If wrapping past GM, there is code below
                             ! in this find_maxmin routine that can
                             ! modify the indices appropriately.  So...
                             ! do nothing here.
                else

                  if ( verb .ge. 1 ) then
                    print *,' '
                    print *,'!!! ERROR: In find_maxmin, the '
                    print *,'!!!    user-requested western boundary'
                    print *,'!!!    is beyond the western bounds of '
                    print *,'!!!    the vmag subgrid for this regional '
                    print *,'!!!    grid.  '
                    print *,'!!!         '
                    print *,'!!!   imax of regional grid    = ',imax
                    print *,'!!!   ivlonfix = ',ivlonfix,' ibeg= ',ibeg
                    print *,'!!!         '
                    print *,'!!! Vmag will not be computed for'
                    print *,'!!! this time.'
                    print *,' '
                  endif

                  ifmret = 99
                  return
                endif
              endif

              if (iend > imax) then
                if (trkrinfo%gridtype == 'global') then
                  continue   ! If wrapping past GM, there is code below
                             ! in this find_maxmin routine that can
                             ! modify the indices appropriately.  So...
                             ! do nothing here.
                else

                  if ( verb .ge. 1 ) then
                    print *,' '
                    print *,'!!! ERROR: In find_maxmin, the '
                    print *,'!!!    user-requested eastern boundary'
                    print *,'!!!    is beyond the eastern bounds of '
                    print *,'!!!    the vmag subgrid for this regional '
                    print *,'!!!    grid.  '
                    print *,'!!!         '
                    print *,'!!!   imax of regional grid    = ',imax
                    print *,'!!!   ivlonfix = ',ivlonfix,' iend= ',iend
                    print *,'!!!         '
                    print *,'!!! Vmag will not be computed for '
                    print *,'!!! this time.'
                    print *,' '
                  endif

                  ifmret = 99
                  return
                endif
              endif

              if (jbeg < 1) jbeg = 1
              if (jend > jmax) jend = jmax

              if ( verb .ge. 3 ) then
                print *,'!!! '
                print *,'!!! *AFTER* vmag adjustments, boundaries are: '
                print *,'!!! ibeg= ',ibeg,' iend= ',iend,' imax= ',imax
                print *,'!!! jbeg= ',jbeg,' jend= ',jend,' jmax= ',jmax
              endif

            endif

          endif

          if (cparm == 'vmag') then
            ri = re * 3
c            print '(a36,f10.4,a6,f10.4)'
c     &           ,'  + before call to vmag barnes, re= ',re,'  ri= ',ri
          endif

          ibct = ibct + 1
          call barnes(rlont,rlatt,rlonv,rlatv,imax,jmax,ibeg,jbeg
     &     ,iend,jend,fxy,valid_pt,bskip1,re,ri,ftemp,icount,'tracker'
     &     ,trkrinfo,iret)

          ibarnes_loopct = ibarnes_loopct + icount

          if (iret /= 0) then

            if ( verb .ge. 1 ) then
              print *,' '
              print *,'!!! Non-zero RCC from barnes...'
              print *,'!!! Exiting find_maxmin'
            endif

            compflag = .FALSE.
            ifmret = iret
            return
          endif

          if (maxmin == 'max') then
            if (ftemp > fmax) then
              fmax = ftemp
              ctlon = rlont
              ctlat = rlatt
            endif
          else
            if (ftemp < fmin) then
              fmin = ftemp
              ctlon = rlont
              ctlat = rlatt
            endif
          endif

        enddo iloop
      enddo jloop
 
      if ( verb .ge. 3 ) then
        print *,' '
        print *,'After 1st findmax loop, # calls to barnes = ',ibct
        print *,'Total # of barnes loop iterations = ',ibarnes_loopct
      endif

c
 55     format ('i= ',i3,' j= ',i3,'  rln= ',f7.3,'  rlt= ',f7.3
     &       ,'  barnval= ',f11.5)
 56     format ('k= ',i3,' i= ',i3,' j= ',i3,'  rln= ',f7.3,'  rlt= '
     &       ,f7.3,'  barnval= ',f11.5)

      if (ctlon < 0.) then
        ! We have grid-wrapped to find the ctlon, which was found to be
        ! < 0, so for reporting purposes and for the start of the next
        ! loop, set ctlon to positive degress east.
        ctlon = ctlon + 360.
      endif

      if (cparm == 'zeta') then

        if ( verb .ge. 3 ) then
          print *,'!!! Zeta check, fmax= ',fmax,' fmin= ',fmin
          write (6,61) 360.-ctlon,ctlat,fmax*100000.,fmin*100000.
        endif

      else

        if ( verb .ge. 3 ) then
          write (6,63) 360.-ctlon,ctlat,fmax,fmin
        endif

      endif
  61  format (' After first run, ctlon= ',f8.3,'W  ctlat= ',f8.3
     &       ,' fmax (x10e5) = ',e16.3,' fmin (x10e5) = ',e16.3)
  63  format (' After first run, ctlon= ',f8.3,'W  ctlat= ',f8.3
     &       ,' fmax = ',e16.3,' fmin = ',e16.3)
 111  format (i2,'  rlont= ',f7.2,'W   rlatt= ',f7.2,'  zeta= ',f13.8)

c     Through interpolation, the grid for vmag has already been
c     minimized considerably, we don't need to go through the 2nd part
c     of this subroutine, which halves the grid spacing.

      if (nhalf < 1 .or. cparm == 'vmag') then
        if (maxmin == 'max') then
          xval = fmax
        else
          xval = fmin
        endif
        return
      endif

c     If on our first pass through, we were dealing with a regional grid
c     that straddled the GM, then it becomes (for now) too much of a
c     coding hassle to deal with in the rest of this routine (i.e., in
c     all the nhalf iterations), so we will just go with the first run
c     through for the center fix and exit the routine.

      if (grid_minlon > 330. .and. grid_maxlon < 30.) then
        if (maxmin == 'max') then
          xval = fmax
        else
          xval = fmin
        endif
        return
      endif

c     -------------------------------------------------------------
c     If the grid spacing is
c     fine enough (I've chosen 0.2-deg as a min threshold), there is
c     no need to halve the grid more than 3 times, as halving a 
c     0.2-deg grid 3 times gives a resolution of 0.025-deg (2.7 km),
c     or a max error in the position estimate of 2.7/2 = 1.35 km.

      if ((dx+dy)/2. <= 0.2) then
        if ((dx+dy)/2. <= 0.05) then
          nhalf = 1
        else
          nhalf = 2
        endif
      endif

c     ---------------------------------------------------------------
c     ---------------------------------------------------------------
c     Halve the grid spacing to refine the location and value of the
c     max/min value, but restrict the area of the new search grid.

ctpm   npts = 3
      npts = npts/2
      npts = max(npts,1)

c     -------------------------------------------------------------
c     First, recalculate the i and j beginning and ending points to
c     be used in the  barnes analysis subroutine.  Only
c     do this once for this grid-refinement (even though the grid is
c     redefined 3 times in this subroutine), but make sure to have the
c     possible search grid be big enough to allow the possibility of
c     the grid shifting way right or way left each time through the
c     loop (get_ij_bounds takes care of this).

      call get_ij_bounds (npts,nhalf,ri,imax,jmax,dx,dy
     &              ,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
     &              ,ctlon,ctlat,trkrinfo
     &              ,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)

      if (igiret /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in find_maxmin from call to get_ij_bounds'
          print *,'!!! just before nhalf loop.  Stopping processing'
          print *,'!!! for storm number ',ist
        endif

        ifmret = 92
        return
      endif

c     --------------------------------------------------------------
c     Now do the actual searching for the max/min value 

      
      if ( verb .ge. 3 ) then
        print *,' '
      endif

      if ((dx+dy)/2. <= 1.25 .and. ri >= 300 .and. re >= 150) then
        retmp = re
        ritmp = ri
        re = re * 0.5
        ri = ri * 0.5

        if ( verb .ge. 3 ) then
          print *,'After first pass through barnes, re has been reduced'
          print *,'from ',retmp,' to ',re,', and ri has been reduced '
          print *,'from ',ritmp,' to ',ri
        endif

      else

        if ( verb .ge. 3 ) then
          print *,'After first pass through barnes, re and ri have NOT '
          print *,'been changed.  re = ',re,' ri = ',ri
        endif

      endif

      ibct=0
      ibarnes_loopct = 0
      do k=1,nhalf

        call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                     ,date_time)
        if ( verb .ge. 3 ) then
          write (6,32) k,date_time(5),date_time(6),date_time(7)
 32       format (1x,'TIMING: find_maxmin kloop, k= ',i2,'   ',i2.2,':'
     &         ,i2.2,':',i2.2)
        endif

        dell = 0.5*dell
        tlon = ctlon
        tlat = ctlat
        fmax = -1.0e+15; fmin = 1.0e+15

        iskip = bskip2

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'find_maxmin nhalf loop, cparm= ',cparm,' k= ',k
          write (6,161) tlon,360.-tlon,tlat
          print *,'ilonfix= ',ilonfix,' jlatfix= ',jlatfix
     &         ,' npts= ',npts
          print *,'ibeg= ',ibeg,' jbeg= ',jbeg,' imax= ',imax
          print *,'iend= ',iend,' jend= ',jend,' jmax= ',jmax
          print *,'nhalf= ',nhalf,' iskip= ',iskip
        endif

        jloop2: do j=-npts,npts,iskip

          rlatt = tlat + dell*float(j)

          iloop2: do i=-npts,npts,iskip

            rlont = tlon + dell*float(i)

            if (rlont >= (grid_maxlon + dx - grid_buffer)) then
              if (trkrinfo%gridtype == 'global') then
                rlont = rlont - 360.
              else
                cycle iloop2
              endif
            endif

            if (rlont < (grid_minlon + grid_buffer)) then
              if (trkrinfo%gridtype == 'global') then
                rlont = rlont + 360.
              else
                cycle iloop2
              endif
            endif

            if (rlatt > (grid_maxlat - grid_buffer) .or. 
     &          rlatt < (grid_minlat + grid_buffer) .or.
     &          rlont >= (grid_maxlon + dx - grid_buffer) .or. 
     &          rlont < (grid_minlon + grid_buffer)) then
              cycle iloop2
            endif

c           Again, check and make sure that the lat/lon point in
c           question here has valid data (see the explanation further
c           up in this subroutine inside iloop).

            call check_valid_point (imax,jmax,dx,dy,fxy,maxmin,valid_pt
     &          ,rlont,rlatt,grid_maxlat,grid_minlat,grid_maxlon
     &          ,grid_minlon,trkrinfo,icvpret)

            if (icvpret /= 0) then
              cycle iloop2
            endif

            ibct = ibct + 1
            call barnes(rlont,rlatt,rlonv,rlatv,imax,jmax,ibeg,jbeg
     &        ,iend,jend,fxy,valid_pt,iskip,re,ri,ftemp,icount,'tracker'
     &        ,trkrinfo,iret)

            ibarnes_loopct = ibarnes_loopct + icount

            if (iret /= 0) then

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! Non-zero RCC from barnes, k= ',k
                print *,'!!! Exiting find_maxmin'
              endif

              compflag = .FALSE.
              ifmret = iret
              return
            endif

            if (maxmin == 'max') then
              if (ftemp > fmax) then
                fmax = ftemp
                ctlon = rlont
                ctlat = rlatt
              endif
            else
              if (ftemp < fmin) then
                fmin = ftemp
                ctlon = rlont
                ctlat = rlatt
              endif
            endif

          enddo iloop2
        enddo jloop2

      if ( verb .ge. 3 ) then
        if (cparm == 'zeta') then
          write (6,71) k,360.-ctlon,ctlat,fmax*100000.,fmin*100000.
        else
          write (6,73) k,360.-ctlon,ctlat,fmax,fmin
        endif
      endif

      enddo

  71  format (' nhalf findmax, k= ',i2,' ctlon= ',f8.3,'W  ctlat= '
     &       ,f8.3,' fmax (x10e5) = ',e16.3,' fmin (x10e5) = ',e16.3)
  73  format (' nhalf findmax, k= ',i2,' ctlon= ',f8.3,'W  ctlat= '
     &       ,f8.3,' fmax = ',e16.3,' fmin = ',e16.3)

 161  format (' guesslon= ',f8.3,'E  (',f8.3,'W)   guesslat= ',f8.3)
 
      if ( verb .ge. 3 ) then
        print *,' '
        print *,'ppp after 2nd findmax loop, # calls to barnes =  '
     &         ,ibct
        print *,'ppp Total # of barnes loop iterations = '
     &         ,ibarnes_loopct
      endif
 
      if (maxmin == 'max') then
        xval = fmax
      else
        xval = fmin
      endif
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine barnes(flon,flat,rlon,rlat,iimax,jjmax,iibeg,jjbeg
     &        ,iiend,jjend,fxy,defined_pt,bskip,re,ri,favg,icount,ctype
     &        ,trkrinfo,iret)
c
c     ABSTRACT: This routine performs a single-pass barnes anaylsis
c     of fxy at the point (flon,flat). The e-folding radius (km)
c     and influence radius (km) are re and ri, respectively.
c
c     NOTE:  The input grid that is searched in this subroutine is most
c     likely NOT the model's full, original grid.  Instead, a smaller
c     subgrid of the original grid is searched.  The upper left and 
c     lower right grid point indices are passed into this subroutine 
c     (iibeg, jjbeg, iiend, jjend) for this subgrid.  These indices are
c     determined in the subroutine  get_ij_bounds, and the purpose of 
c     doing it this way is to limit the number of points for which the
c     subroutine has to calculate distances (for a global 1 deg grid,
c     the number of loop iterations is reduced from 65160 to somewhere
c     around 600).
c
c     NOTE: This subroutine will immediately exit with a non-zero
c     return code if it tries to access a grid point that does not have
c     valid data.  This would happen in the case of a regional grid, if
c     you try to access a point near the edge of the grid (remember that
c     because of the interpolation for the regional grids, there will be
c     areas around the edges that have no valid data).
c
c     INPUT:
c     flon    Lon value for center point about which barnes anl is done
c     flat    Lat value for center point about which barnes anl is done
c     rlon    Array of lon values for each grid point
c     rlat    Array of lat values for each grid point
c     iimax   Max number of pts in x-direction on input grid
c     jjmax   Max number of pts in y-direction on input grid
c     iibeg   i index for grid point to start barnes anlysis (upp left)
c     jjbeg   j index for grid point to start barnes anlysis (upp left)
c     iiend   i index for last grid point in barnes anlysis (low right)
c     jjend   j index for last grid point in barnes anlysis (low right)
c     fxy     Real array of data on which to perform barnes analysis
c     defined_pt Logical; bitmap array used for regional grids
c     bskip   integer to indicate number of grid points to skip during
c             a barnes loop, in order to speed processing
c     re      input e-folding radius for barnes analysis
c     ri      input influence radius for searching for min/max
c     ctype   character that lets subroutine know if this is a search
c             for the next position for the purposes of tc vitals or
c             for general tracking.  In the case of vitals, in
c             this barnes subroutine we are more lax and allow the
c             routine to keep searching even if we are close to the
c             grid boundary.  In a general tracking search, if we hit
c             the grid boundary even just once, we exit.
c     trkrinfo derived type detailing user-specified grid info
c
c     OUTPUT:
c     favg    Average value about the point (flon,flat)
c     iret    Return code from this subroutine
c
      USE trkrparms
      USE verbose_output

      type (trackstuff) trkrinfo

      real      fxy(iimax,jjmax), rlon(iimax), rlat(jjmax)
      real      degrees
      integer   bskip
      logical(1) defined_pt(iimax,jjmax)
      character(*) ctype

c     --------------------------

      res = re*re
      wts = 0.0
      favg = 0.0

      icount = 0

      do jix=jjbeg,jjend,bskip
        do iix=iibeg,iiend,bskip

          i = iix
          j = jix

          if (i < 1) then
            if (trkrinfo%gridtype == 'global') then
              i = iix + iimax
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i < 1 in subroutine  barnes for'
                print *,'!!! a non-global grid.  STOPPING....'
                print *,'!!! i= ',i
                print *,' '
              endif

              stop 97
            endif
          endif

          if (i > iimax) then
            if (trkrinfo%gridtype == 'global') then
              i = iix - iimax
            else

              if ( verb .ge. 1 ) then
                print *,' '
                print *,'!!! ERROR: i > imax in subroutine  barnes for'
                print *,'!!! a non-global grid.  STOPPING....'
                print *,'!!! i= ',i,' imax= ',iimax
                print *,' '
              endif

              stop 97
            endif
          endif

          icount = icount + 1

          call calcdist(flon,flat,rlon(i),rlat(j),dist,degrees)

          if (dist .gt. ri) cycle

          if (defined_pt(i,j)) then
            if (fxy(i,j) >-999.01 .and. fxy(i,j) <-998.99) then
              ! This is a patch.  Even though this (i,j) is a valid
              ! point, its zeta value has been set to -999 because a
              ! neighboring point in subroutine  rvcal was found
              ! to be out of the grid boundaries.
              cycle
            endif
            wt   = exp(-1.0*dist*dist/res)
            wts  = wts + wt
            favg = favg + wt*fxy(i,j)
          else
            if (ctype == 'vitals') then
              continue
            else
carw           print *,' '
carw           print *,'!!! UNDEFINED PT OUTSIDE OF GRID IN BARNES....'
carw           print *,'!!! i= ',i,' j= ',j
carw           print *,'!!! flon= ',flon,' flat= ',flat
carw           print *,'!!! rlon= ',rlon(i),' rlat= ',rlat(j)
carw           print *,'!!! re= ',re,' ri= ',ri
carw           print *,'!!! EXITING BARNES....'
carw           print *,' '
carw           iret = 95
carw           return
            endif
          endif
 
        enddo
      enddo
 
      if (wts > 1.0E-5) then
         favg = favg/wts
      else
         favg = 0.0
      endif
      iret = 0
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine get_ij_bounds (npts,nhalf,ri,imax,jmax,dx,dy
     &          ,rglatmax,rglatmin,rglonmax,rglonmin,geslon,geslat
     &          ,trkrinfo,ilonfix,jlatfix,ibeg,jbeg,iend,jend,igiret)
c
c     -----------------------------------------------------------
c     ABSTRACT: This subroutine figures out, based on ri, dx and dy and
c     the guess latitude and longitude positions, the farthest reaching
c     grid points that are searchable by an analysis subroutine.  The
c     purpose is to return indices for a subgrid that is much smaller 
c     than the original, full grid.  This smaller subgrid can then be 
c     passed to a subsequent analysis or interpolation subroutine, and 
c     work can be done on this smaller array.  This can help save time, 
c     especially in the  barnes analysis subroutine, as work will only 
c     be done on, say, a 20 x 20 array (400 pts) instead of on a 
c     360 x 181 array (65160 pts).  It's crucial, however, to make sure 
c     that the ibeg, jbeg, iend and jend are extended far enough out to 
c     fully encompass any search that would be done.  Below is a 
c     diagram showing the different grids....
c
c Full Global or Regional Model Grid  (Grid F) ----------->
c     ----------------------------------------------------------------
c  |  |                            (ibeg,jbeg)                       |
c  |  | x = ij position that the        |      (Grid R)              |
c  |  |     geslat/geslon is fixed to.  ._______________.            |
c  |  |                                 |               |            |
c  |  | Even though only the points     |    (Grid B)   |            |
c  |  | within Grid B will be checked   |   . . . . k   |            |
c  v  | later on for a max/min (in the  |   . . . . .   |            |
c     | case of a subsequent call to    |   . . x . e   |            |
c     | find_maxmin), the  barnes anal- |   . . . . .   |            |
c     | ysis will include all pts sur-  |   . . . . .   |            |
c     | rounding these Grid B points    |               |            |
c     | that are within a radius of ri. ._______________.            |
c     | So in the case of pt. k, that ri                             |
c     | radius may extend all the way to the Grid R     |            |
c     | boundary, thus we need to include those    (iend,jend)       |
c     | points within our ibeg-jbeg-iend-jend bounds.                |
c     |                                                              |
c     ----------------------------------------------------------------
c
c     Remember that the grids we deal with start north and increase 
c     south, so the northernmost latitude on the input grid will have 
c     a j index of 1.
c
c     INPUT:
c     npts     Num pts from x to edge of max/min search grid (Grid B)
c              (i.e., You define the size of Grid B by the value of
c               npts that you pass into this subroutine).
c     nhalf    Number of times the grid spacing will be halved
c     ri       Radius of influence (for use in barnes analysis)
c     imax     Number of points in x-direction on original grid
c     jmax     Number of points in y-direction on original grid
c     dx       Input grid spacing in i-direction on original grid
c     dy       Input grid spacing in j-direction on original grid
c     rglatmax Value of northern-most latitude on original grid
c     rglatmin Value of southern-most latitude on original grid
c     rglonmax Value of eastern-most longitude on original grid
c     rglonmin Value of western-most longitude on original grid
c     geslat   Value of latitude of guess position of storm
c     geslon   Value of longitude of guess position of storm
c
c     OUTPUT:
c     ilonfix  i index on full, input grid that storm is fixed to
c     jlatfix  j index on full, input grid that storm is fixed to
c     ibeg     i index for top left of sub-array (Grid R) of input grid
c     iend     i index for bot right of sub-array (Grid R) of input grid
c     jbeg     j index for top left of sub-array (Grid R) of input grid
c     jend     j index for bot right of sub-array (Grid R) of input grid
c     igiret   Return code from this subroutine
c
      USE trig_vals; USE trkrparms
      USE verbose_output

      type (trackstuff) trkrinfo
      real tmpangle
c
      igiret = 0
c
c     --------------------------------------
c     GET BEGINNING AND ENDING J POINTS....
c
c     (1) Calculate number of searchable, max/min pts, that is, the pts 
c         from x to the edge of Grid B.
c     (2) Calculate number of pts beyond the last search point in Grid 
c         B, but are within the bounds of Grid R and thus can be 
c         included in the  barnes analysis.
c     (3) Add (1) and (2) to get the max number of pts to subtract/add
c         to x to get jbeg and jend.

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'Beginning of get_ij_bounds...'
        print *,'  geslat= ',geslat,' geslon= ',geslon
        print *,' '
      endif


c     If nhalf > 0: This occurs in the case of a call from fmax, when
c     the grid spacing is halved nhalf times.  In this case, we have to
c     do extra work to figure out the maximum possible grid point.  For
c     this case:
c       jhlatpts = # of grid pts to last possible search pt (from x to
c                  edge of Grid B in above diagram), plus a cushion.
c       jripts   = # of searchable grid points within radius ri of last
c                  possible search pt (num pts between edge of Grid B
c                  and edge of Grid R in above diagram), plus a cushion
c       jbmaxlatpts = # of pts (in j direction) from x to the edge of
c                     Grid R to include in this subgrid. 
c
c     If nhalf = 0: In this case, the grid spacing will not be reduced,
c     so the number of pts in j direction from x to the edge of Grid
c     B will be the input parameter npts, and just multiply it by 2,
c     and add 2 for a cushion to get jmaxlatpts.  Typically, this sub
c     is called from find_maxmin, and in that sub, the first time that
c     this sub is called, nhalf will = 0.  Then, after a first-shot
c     center is found, the grid spacing will be cut in order to rerun 
c     barnes on a smaller grid, and that's when nhalf will be sent 
c     here as 3.
c
      if (nhalf > 0) then
        rdeg = 0.0
        do i = 1,nhalf
          rdeg = rdeg + float(npts) * (1./(float(i)*2)) * (dx+dy)/2
        enddo
        jhlatpts = ceiling(rdeg/dy) + 1
        jripts   = ceiling((ri + 1.)/(dtk*dx)) + 1
        jbmaxlatpts = jhlatpts + jripts
      else
        jbmaxlatpts = npts * 2 + 2
      endif
c
c
c     Roughly fix geslat to the grid point just poleward of geslat.
c

      if ( verb .ge. 3 ) then
        print *,' '
        print *,' +++ Near top of get_ij_bounds, '
        print *,' +++ geslat= ',geslat,'  geslon= ',geslon
        print *,' +++ rglatmax= ',rglatmax,' rglatmin= ',rglatmin
        print *,' +++ rglonmax= ',rglonmax,' rglonmin= ',rglonmin
        print *,' +++ imax= ',imax,' jmax= ',jmax
        print *,' +++ dx= ',dx,' dy= ',dy,' nhalf= ',nhalf
        print *,' +++ npts= ',npts
        if(nhalf>0) then
           print *,' +++ jhlatpts= ',jhlatpts,' jripts= ',jripts
        else
           print *,' +++ nhalf<=0 so jhlatpts and jripts unused'
        endif
        print *,' +++ jbmaxlatpts= ',jbmaxlatpts
      endif

      if (geslat >= 0.0) then
        jlatfix = int((rglatmax - geslat)/dy + 1.)
      else
        jlatfix = ceiling((rglatmax - geslat)/dy + 1.)
      endif

      if ( verb .ge. 3 ) then
        print *,' +++ jlatfix= ',jlatfix
      endif

      jbeg = jlatfix - jbmaxlatpts
      jend = jlatfix + jbmaxlatpts
      if (jbeg > jmax ) then

        if ( verb .ge. 1 ) then
          print *,'!!! ERROR in get_ij_bounds, jbeg > jmax'
          print *,'!!! jbeg = ',jbeg,' jmax= ',jmax
        endif

        igiret = igiret + 1
        return
      endif
      if (jend < 1) then

        if ( verb .ge. 1 ) then
          print *,'!!! ERROR in get_ij_bounds, jend < 1, jend = ',jend
        endif

        igiret = igiret + 1
        return
      endif
      if (jbeg < 1) jbeg = 1
      if (jend > jmax) jend = jmax

      if ( verb .ge. 3 ) then
        print *,' +++ jbeg= ',jbeg,' jend= ',jend
      endif

      ! If using a global grid, avoid using the pole points, or else
      ! you'll get a cosfac = 0 and then a divide by zero!!!

      if (jend == jmax .and. rglatmin == -90.0) then
        jend = jmax - 2
      endif
      if (jbeg == 1    .and. rglatmax == 90.0) then
        jbeg = 3
      endif

c     -----------------------------------------
c     NOW GET BEGINNING AND ENDING I POINTS....
c
c     Using the map factor (cos lat), figure out, based on ri, the
c     max distance beyond the last search point in x-direction (in
c     degrees) that could be searched at this guess latitude (geslat)
c     (i.e., in the diagram above, the max num pts from pt. e eastward
c     to the edge of Grid R).  Calculate how many grid points that is,
c     add 2 to it for a cushion, & add the number of points (npts)
c     within the defined search grid (Grid B) to get ibmaxlonpts.
c
c     April, 2007: A min statement was put on the calculation to
c     derive dlon, since with that cosine in there, the values of
c     of dlon could get pretty ridiculous as you approach the poles.
c     Also, the cosine factor (cosfac) used to be computed at the
c     most poleward latitude possible given the jend here.  For
c     similar concerns with cosines near the poles, I've scrapped
c     this to instead compute the cosine factor at the input
c     guess latitude. - tpm

      cosfac = cos (geslat * dtr)
      tmpangle = cosfac * dtk
      dlon   = min((ri /tmpangle ),20.0)
c      dlon   = min((ri / (cosfac * dtk)),20.0)
c
      if (nhalf > 0) then
        ihlonpts    = ceiling(rdeg/dx) + 1
        ibmaxlonpts = ihlonpts + ceiling(dlon/dx) + 2
      else
        ibmaxlonpts = npts + ceiling(dlon/dx) + 2
      endif

      if ( verb .ge. 3 ) then
         if(nhalf>0) then
            print *,' +++ rdeg= ',rdeg,' ri= ',ri,' cosfac= ',cosfac
            print *,' +++ dtr= ',dtr,' dtk= ',dtk,' dlon= ',dlon
         else
            print*,' +++ nhalf<=0 so rdeg,ri,cosfac,dtr,dtk,dlon unused'
         endif
        print *,' +++ ibmaxlonpts= ',ibmaxlonpts,' dx= ',dx,' dy= ',dy
      endif

c     Roughly fix geslon to the grid point just EASTward of geslon.

      ilonfix = int((geslon - rglonmin)/dx + 2.)

      ibeg = ilonfix - ibmaxlonpts
      iend = ilonfix + ibmaxlonpts

      if ( verb .ge. 3 ) then
        print *,' +++ (orig) ilonfix= ',ilonfix
        print *,' +++ (orig) ibeg= ',ibeg,' iend= ',iend
        print *,' +++ '
      endif

      if (ibeg > imax) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 1 ) then
            print *,'+++ NOTE: in get_ij_bounds, ibeg > imax'
            print *,'+++ for a global grid; GM wrapping expected from'
            print *,'+++ calling routine.  ibeg = ',ibeg,' imax= ',imax
          endif

        else

          if ( verb .ge. 1 ) then
            print *,'!!! ERROR in get_ij_bounds, ibeg > imax'
            print *,'!!! for a non-global grid'
            print *,'!!! ibeg = ',ibeg,' imax= ',imax
          endif

          igiret = igiret + 1
          return
        endif
      endif
      if (iend > imax) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then
            print *,'+++ NOTE: in get_ij_bounds, iend > imax'
            print *,'+++ for a global grid; GM wrapping expected from'
            print *,'+++ calling routine.  iend = ',iend,' imax= ',imax
          endif

        else
          ! For a regional grid, just set iend to be imax
          iend = imax
        endif
      endif
      if (ibeg < 1) then
        if (trkrinfo%gridtype == 'global') then
          
          if ( verb .ge. 3 ) then
            print *,'+++ NOTE: in get_ij_bounds, ibeg < 1'
            print *,'+++ for a global grid; GM wrapping expected from'
            print *,'+++ calling routine.  ibeg = ',ibeg,' imax= ',imax
          endif

        else
          ! For a regional grid, just set ibeg to be 1
          ibeg = 1
        endif
      endif
      if (iend < 1) then
        if (trkrinfo%gridtype == 'global') then

          if ( verb .ge. 3 ) then
            print *,'+++ NOTE: in get_ij_bounds, iend < 1'
            print *,'+++ for a global grid; GM wrapping expected from'
            print *,'+++ calling routine.  iend = ',iend,' imax= ',imax
          endif

        else

          if ( verb .ge. 3 ) then
            print *,'!!! ERROR in get_ij_bounds, iend < 1'
            print *,'!!! for a non-global grid'
            print *,'!!! iend = ',iend,' imax= ',imax
          endif

          igiret = igiret + 1
          return
        endif
      endif
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine check_bounds (guesslon,guesslat,ist,ifh,trkrinfo
     &                        ,icbret)
c
c     ABSTRACT:  This subroutine checks to make sure that the requested
c                storm is in fact within the model's grid boundaries;
c                this is only a concern for the regional models.
c
      USE def_vitals; USE grid_bounds; USE set_max_parms 
      USE trkrparms
      USE verbose_output 

      type (trackstuff) trkrinfo

      if (trkrinfo%gridtype == 'regional') then
        if (guesslon > glonmax .or. guesslon < glonmin .or.
     &       guesslat > glatmax .or. guesslat < glatmin) then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! IN check_bounds, Storm is outside of grid'
            print *,'!!! Storm ID =   ',storm(ist)%tcv_storm_id
            print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
            print *,'!!! ist= ',ist,' ifh= ',ifh
            print *,'!!! guess storm lon= ',guesslon
            print *,'!!! guess storm lat= ',guesslat
          endif

          icbret = 95
          goto 125
        else
          icbret = 0
        endif
      endif

      ! We have encountered problems with global grids where we 
      ! continue tracking almost the whole way to the pole.  While
      ! that's nice to do that, it creates problems for array 
      ! indices, especially in subroutine  getradii.  So we will cut
      ! our losses and eliminate tracking of storms within 
      ! 5 degrees of the pole for global grids.

      if ((trkrinfo%type == 'midlat' .or.
     &     trkrinfo%type == 'tcgen') .and.
     &     trkrinfo%gridtype == 'global')then
        if (guesslat > 85.0 .or. guesslat < -85.0) then

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! IN check_bounds, Storm is too close to the'
            print *,'!!! N or S Pole for global tracking.'
            print *,'!!! STOPPING TRACKING FOR THIS STORM DUE TO POLE'
            print *,'!!! Storm ID =   ',storm(ist)%tcv_storm_id
            print *,'!!! Storm Name = ',storm(ist)%tcv_storm_name
            print *,'!!! ist= ',ist,' ifh= ',ifh
            print *,'!!! guess storm lon= ',guesslon
            print *,'!!! guess storm lat= ',guesslat
          endif

          icbret = 95
        else
          icbret = 0
        endif
      endif

  125 continue
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine calcdist(rlonb,rlatb,rlonc,rlatc,xdist,degrees)
c
c     ABSTRACT: This subroutine computes the distance between two 
c               lat/lon points by using spherical coordinates to 
c               calculate the great circle distance between the points.
c                       Figure out the angle (a) between pt.B and pt.C,
c             N. Pole   then figure out how much of a % of a great 
c               x       circle distance that angle represents.
c              / \
c            b/   \     cos(a) = (cos b)(cos c) + (sin b)(sin c)(cos A)
c            /     \                                             
c        pt./<--A-->\c     NOTE: The latitude arguments passed to the
c        B /         \           subr are the actual lat vals, but in
c                     \          the calculation we use 90-lat.
c               a      \                                      
c                       \pt.  NOTE: You may get strange results if you:
c                         C    (1) use positive values for SH lats AND
c                              you try computing distances across the 
c                              equator, or (2) use lon values of 0 to
c                              -180 for WH lons AND you try computing
c                              distances across the 180E meridian.
c    
c     NOTE: In the diagram above, (a) is the angle between pt. B and
c     pt. C (with pt. x as the vertex), and (A) is the difference in
c     longitude (in degrees, absolute value) between pt. B and pt. C.
c
c     !!! NOTE !!! -- THE PARAMETER ecircum IS DEFINED (AS OF THE 
c     ORIGINAL WRITING OF THIS SYSTEM) IN KM, NOT M, SO BE AWARE THAT
c     THE DISTANCE RETURNED FROM THIS SUBROUTINE IS ALSO IN KM.
c
      USE trig_vals

      real degrees
c
      if (rlatb < 0.0 .or. rlatc < 0.0) then
        pole = -90.
      else
        pole = 90.
      endif
c
      distlatb = (pole - rlatb) * dtr
      distlatc = (pole - rlatc) * dtr
      difflon  = abs( (rlonb - rlonc)*dtr )
c
      cosanga = ( cos(distlatb) * cos(distlatc) + 
     &            sin(distlatb) * sin(distlatc) * cos(difflon))
 
c     This next check of cosanga is needed since I have had ACOS crash
c     when calculating the distance between 2 identical points (should
c     = 0), but the input for ACOS was just slightly over 1
c     (e.g., 1.00000000007), due to (I'm guessing) rounding errors.

      if (cosanga > 1.0) then
        cosanga = 1.0
      endif

      degrees    = acos(cosanga) / dtr
      circ_fract = degrees / 360.
      xdist      = circ_fract * ecircum
c
c     NOTE: whether this subroutine returns the value of the distance
c           in km or m depends on the scale of the parameter ecircum. 
c           At the original writing of this subroutine (7/97), ecircum
c           was given in km.
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine subtract_cor (imax,jmax,dy,level)
c
c     ABSTRACT: This subroutine  subtracts out the coriolis parameter
c     from the vorticity values.  It is needed because at the original
c     writing of this system, all of the forecast centers who included
c     vorticity included only absolute vorticity.
c
      USE tracked_parms; USE trig_vals; USE grid_bounds

      implicit none

      integer :: i,j,imax,jmax,level
      real    :: dy,coriolis,rlat
c
      do j=1,jmax
        rlat = glatmax - ((j-1) * dy)
        coriolis = 2. * omega * sin(rlat*dtr) 
        do i=1,imax
          zeta(i,j,level) = zeta(i,j,level) - coriolis
        enddo
      enddo
c
      return
      end
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine get_grib_file_name (ifh,gfilename,ifilename)

c     ABSTRACT: This subroutine uses various input regarding the model
c     and forecast hour and generates the name of the input grib file
c     for this particular forecast hour.  Remember that the lead time
c     is in minutes and that 5 spaces must be reserved for the lead
c     time (e.g., f00360).  File name should be something that looks
c     like either, e.g., "gfdl.6thdeg.katrina12l.2005082818.f00720",
c     or "gfdl.6thdeg.2005082818.f00720" (the part in there with the
c     storm name & ID is optional).  The grib index file name should
c     be exactly the same as the grib data file itself, but with the
c     character string ".ix" added onto the end of the name.
c
c     NOTE: Array iftotalmins is brought in via module tracked_parms.
c
C     INPUT:
c     ifh      integer array index for current lead time
c
c     OUTPUT:
c     gfilename GRIB file name
c     ifilename GRIB index file name

      USE gfilename_info; USE tracked_parms; USE atcf
      USE verbose_output

      implicit none

      character(*) gfilename,ifilename
      character  cfmin*5,cymdh*10
      integer    ifh,nlen1,nlen2,nlen3,nlen4,nlen5

c     Convert integer minutes to 5-position character, with
c     leading zeroes, and convert 10-digit integer date into
c     10-position character.  Then trim the various input variables
c     and combine all into the file name.

      write (cfmin,'(i5.5)') iftotalmins(ifh)
      write (cymdh,'(i10.10)') atcfymdh

      nlen1     = len_trim(gmodname)
      gfilename = trim(gmodname(1:nlen1))

      nlen2     = len_trim(rundescr)

      gfilename = trim(gfilename(1:nlen1))//'.'//trim(rundescr(1:nlen2))

      nlen3     = len_trim(atcfdescr)
      nlen4     = len_trim(gfilename)

c     If an extension to the name with the ATCF or storm name descriptor
c     was included, then add it to the name now.  Otherwise, just add
c     the starting date and the lead time in minutes.

      if (nlen3 > 0) then
        gfilename = trim(gfilename(1:nlen4))//'.'
     &              //trim(atcfdescr(1:nlen3))//'.'//cymdh//'.f'//cfmin
      else
        gfilename = trim(gfilename(1:nlen4))//'.'//cymdh//'.f'//cfmin
      endif

c     Create the name for the grib index file, which is just the name of
c     the grib file, with "ix" added to the end of it.

      nlen5     = len_trim(gfilename)
      ifilename = trim(gfilename(1:nlen5))//'.ix'

      if ( verb .ge. 3 ) then
        write (6,*) ' '
        write (6,72) 'gfilename',gfilename
        write (6,72) 'ifilename',ifilename
      endif

   72 format (1x,'In get_grib_file_name, file name for ',a9
     &          ,' is ',a120)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getdata_grib (readflag,valid_pt,imax,jmax
     &               ,ifh,need_to_flip_lats,need_to_flip_lons,inp
     &               ,lugb,lugi,trkrinfo)
c
c     ABSTRACT: This subroutine reads the input GRIB file for the
c     tracked parameters.  It then calls subroutines to convert the
c     data from a 1-d array into a 2-d array if the read was successful.
c
c     There are up to 9 fields that are read in that will be used to
c     locate the storm position.  There are an additional 4 variables
c     (500 mb u- and v-components and 10 m u- and v- components) that
c     will not be used for tracking, but only for helping to estimate
c     the next first guess position (500 mb winds) and for estimating
c     the max near-surface wind speeds in the vicinity of the storm
c     (10 m winds).
c
c     Fields read in are listed here.  Numbers indicate positioning in
c     the readflag logical array:
c
c     1.   850 mb absolute vorticity
c     2.   700 mb absolute vorticity
c     3.   850 mb u-component
c     4.   850 mb v-component
c     5.   700 mb u-component
c     6.   700 mb v-component
c     7.   850 mb geopotential height
c     8.   700 mb geopotential height
c     9.   MSLP
c     10.  10-m u-component
c     11.  10-m v-component
c     12.  500 mb u-component
c     13.  500 mb v-component
c     14.  300-500 mb mean temperature (I jerry-rigged this by storing
c               the data as being at the 401 mb level.)
c     15.  500 mb geopotential height
c     16.  200 mb geopotential height
c     17.  Land-Sea mask -- This is for tcgen applications only, and 
c               even there, it's optional.
c
c     INPUT:
c     imax        integer number of pts in i-direction on grid
c     jmax        integer number of pts in j-direction on grid
c     ifh         integer index for forecast hour
c     need_to_flip_lats logical flag read in from getgridinfo that
c                 indicates if data needs flipped north to south
c     need_to_flip_lons logical flag read in from getgridinfo that
c                 indicates if data needs flipped east to west
c     inp         of a derived type, contains user-input info
c     lugb        integer unit number of input grib file
c     lugi        integer unit number of input grib index file
c     trkrinfo    derived type that contains info on the type of
c                 tracker run that we are performing.
c
c     OUTPUT:
c     readflag    logical array, indicates if a parm was read in
c     valid_pt    logical array, indicates for each (i,j) if there is
c                 valid data at the point (used for regional grids)

      USE tracked_parms; USE level_parms; USE inparms; USE phase
      USE verbose_output; USE params; USE grib_mod; USE trkrparms

      implicit none
c
      type (trackstuff) trkrinfo
      type (datecard) inp
      type (gribfield) :: gfld
c
      integer, parameter :: jf=40000000
      integer, parameter :: nreadparms=17
      real, allocatable :: f(:)
      real :: dmin,dmax,firstval,lastval
      logical(1), allocatable :: lb(:)
      logical(1) valid_pt(imax,jmax),readflag(nreadparms)
      logical(1) ::  need_to_flip_lats,need_to_flip_lons
      logical(1) file_open
      logical :: unpack=.true.
      logical :: open_grb=.false.
      character*1 :: lbrdflag
      character*8 :: chparm(nreadparms)
      CHARACTER(len=8) :: pabbrev
      character (len=10) big_ben(3)
      integer   date_time(8)
      integer,dimension(200) :: jids,jpdt,jgdt
      integer :: listsec1(13), enable_timing
      integer, intent(in) :: imax,jmax
      integer   igparm(nreadparms),iglev(nreadparms)
      integer   iglevtyp(nreadparms)
      integer   ig2_parm_cat(nreadparms),ig2_parm_num(nreadparms)
      integer   ig2_lev_val(nreadparms),ig2_lev_typ(nreadparms)
      integer   cpsig2_parm_cat(nlevs_cps),cpsig2_parm_num(nlevs_cps)
      integer   cpsig2_lev_typ(nlevs_cps),cpsig2_lev_val(nlevs_cps)
      integer   ec_igparm(nreadparms),ec_iglev(nreadparms)
      integer   ec_iglevtyp(nreadparms)
      integer   cpsgparm(nlevs_cps),cpsglev(nlevs_cps)
      integer   cpsglevtyp(nlevs_cps)
      integer   ec_cpsgparm(nlevs_cps)
      integer   jpds(200),jgds(200),kpds(200),kgds(200)
      integer   igvret,ifa,ila,ip,ifh,i,j,k,kj,iret,kf,lugb,lugi
      integer   jskp,jdisc,np
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer   pdt_4p0_vert_level,pdt_4p0_vtime
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
c
      lbrdflag = 'n'
      enable_timing=trkrinfo%enable_timing
c     The following data statements contain the parameters that will be
c     used to search the grib files.  The first 9 parameters will all be
c     used to locate the storm position.  The last 4 parameters (500 mb
c     u- and v-components and 10 m u- and v- components) will not be
c     used for tracking, but only for helping to estimate the next first
c     guess position (500 mb winds) and for estimating the max near-
c     surface wind speeds in the vicinity of the storm (10 m winds).
c
c     ** NOTE: iglevtyp(12 & 13) and iglev(12 & 13) are initialized to
c              0 just to satisfy the IBM xlf compiler, which barks about
c              there being too few initial values in the list when I
c              only had 11 values there -- even though the real
c              initialization for these variables is done just about
c              10 lines below.
c    
c     ** NOTE: The new ECMWF hi-res data uses the ECMWF GRIB parameter
c              ID table, which has different values than the NCEP
c              table.  Therefore, we needed to add the variables and
c              data values for ec_igparm, ec_iglevtyp and ec_iglev.
c
c     July 2007: Read statements added for GP height for cyclone
c     phase space (CPS) algorithm.

c      data igparm   /41,41,33,34,33,34,7,7,1,33,34,33,34,11,7,7,81/
      data igparm   /41,41,33,34,33,34,7,7,2,33,34,33,34,11,7,7,81/
      data iglevtyp /100,100,100,100,100,100,100,100,102,0,0,100,100
     &              ,100,100,100,1/
      data iglev    /850,700,850,850,700,700,850,700,0,0,0,500,500,401
     &              ,500,200,0/

      data cpsgparm     /13*7/
      data ec_cpsgparm  /13*156/
      data cpsglevtyp /13*100/
      data cpsglev    /900,850,800,750,700,650,600,550,500,450,400
     &                ,350,300/

      data ec_igparm   /999,999,131,132,131,132,156,156,151,165,166
     &                 ,131,132,130,156,156,999/
      data ec_iglevtyp /100,100,100,100,100,100,100,100,1,0,0,100,100
     &                 ,100,100,100,999/
      data ec_iglev    /850,700,850,850,700,700,850,700,0,0,0,500,500
     &                 ,401,500,200,999/

      data chparm   /'absv','absv','ugrid','vgrid','ugrid','vgrid'
     &              ,'gphgt','gphgt','mslp','ugrid','vgrid','ugrid'
     &              ,'vgrid','temp','gphgt','gphgt','lmask'/

      data ig2_parm_cat /2,2,2,2,2,2,3,3,3,2,2,2,2,0,3,3,2/
      data ig2_parm_num /10,10,2,3,2,3,5,5,1,2,3,2,3,0,5,5,8/
      data ig2_lev_typ  /100,100,100,100,100,100,100,100,101,103,103
     &                  ,100,100,100,100,100,-9999/
      data ig2_lev_val  /850,700,850,850,700,700,850,700,0,10,10,500,500
     &                  ,401,500,200,-9999/
      data cpsig2_parm_cat /13*3/
      data cpsig2_parm_num /13*5/
      data cpsig2_lev_typ  /13*100/
      data cpsig2_lev_val  /900,850,800,750,700,650,600,550,500,450,400
     &                     ,350,300/

c     Model numbers used: (1) AVN, (2) MRF, (3) UKMET, (4) ECMWF,
c                (5) NGM, (6) Early Eta, (7) NAVGEM, (8) GDAS,
c                (10) NCEP Ensemble, (11) ECMWF Ensemble,
c                (13) SREF Ensemble,
c                (14) NCEP Ensemble (from ensstat mean fields),
c                (15) CMC, (16) CMC Ensemble, (17) HWRF,
c                (18) HWRF Ensemble, (19) HWRF-DAS (HDAS),
c                (20) NCEP Ensemble RELOCATION
c                (21) UKMET hi-res (from NHC)
c                (23) FNMOC Ensemble
c                (24) HWRF Basin-scale
      
      if (trkrinfo%gribver == 2) then

c       For GRIB2, we will check to see if the MSLP being searched for
c       is the standard MSLP (MSLP parm ID = 1) or if it is the 
c       so-called "Eta" or "Membrane" MSLP reduction that is included 
c       in the output for some models (like GFS and GDAS).  Note that
c       for 10m winds, with GRIB2, so far with all of the GRIB2 model
c       data we've seen to this point, they all have the same IDs for 
c       10m winds for all models, so no need to break out by model 
c       like we do for GRIB v1 in the else portion of this if statement.
 
        ig2_parm_num(9) = trkrinfo%g2_mslp_parm_id ! 1 = standard MSLP
                             ! reduction, 192 = "Eta" or "Membrane" 
                             ! reduction used in GFS, GDAS and others.

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'Before GRIB2 read, MSLP ID = ig2_parm_num(9) = '
     &           ,ig2_parm_num(9)
        endif

      else

c       For GRIB1, do the same check as done just above in the IF part
c       of this IF statement, but note that we need to also check to 
c       see what the GRIB1 parm IDs are for the sfc wind level type 
c       and value.  Most models list the level type as 105 (which means
c       height above the ground) and then a level value of 10.  But 
c       ECMWF and UKMET use a level type of 1 (which means ground or 
c       water surface) and a level value of 0.

        igparm(9) = trkrinfo%g1_mslp_parm_id ! 102 = standard MSLP
                             ! reduction, 130 = "Eta" or "Membrane" 
                             ! reduction used in GFS, GDAS and others. 

        iglevtyp(10) = trkrinfo%g1_sfcwind_lev_typ ! 105 for most
        iglevtyp(11) = trkrinfo%g1_sfcwind_lev_typ ! 105 for most
        iglev(10)    = trkrinfo%g1_sfcwind_lev_val ! 10 for most
        iglev(11)    = trkrinfo%g1_sfcwind_lev_val ! 10 for most

        ec_iglevtyp(10) = trkrinfo%g1_sfcwind_lev_typ ! = 1 for ECMWF
        ec_iglevtyp(11) = trkrinfo%g1_sfcwind_lev_typ ! = 1 for ECMWF
        ec_iglev(10)    = trkrinfo%g1_sfcwind_lev_val ! = 0 for ECMWF
        ec_iglev(11)    = trkrinfo%g1_sfcwind_lev_val ! = 0 for ECMWF

        if ( verb .ge. 3 ) then
          print *,' ' 
          print *,'Before GRIB1 read, MSLP ID = igparm(9) = '
     &           ,igparm(9)
          print *,'Before GRIB1 read, non-ECMWF sfcwind lev type = '
     &           ,iglevtyp(10)
          print *,'Before GRIB1 read, non-ECMWF sfcwind lev value = '
     &           ,iglev(10)
          print *,'Before GRIB1 read, ECMWF sfcwind lev type = '
     &           ,ec_iglevtyp(10)
          print *,'Before GRIB1 read, ECMWF sfcwind lev value = '
     &           ,ec_iglev(10)
        endif

      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'NOTE: Program is now in subroutine  getdata.  A return'
        print *,'code (iret) not equal to zero indicates that '
        print *,'subroutine getgb was unable to find the requested '
        print *,'parameter.  This could be simply because the parm is '
        print *,'not included in the grib file (this is likely for '
        print *,'ECMWF data, as they limit what they send us), or it '
        print *,'could indicate a problem with the grib index file.'
      endif


      if (allocated(f)) deallocate(f)
      if (allocated(lb)) deallocate(lb)
      allocate (f(imax*jmax),stat=ifa)
      allocate (lb(imax*jmax),stat=ila)
      if (ifa /= 0 .or. ila /= 0) then
        print *,' '
        print *,'!!! ERROR in getdata allocating f or lb array.'
        print *,'!!! ifa = ',ifa,' ila= ',ila
        print *,'!!! STOPPING EXECUTION'
        STOP 91
      endif

      if (trkrinfo%gribver == 2) then

        ! Reading from a GRIB v2 file....

        grib2_standard_parm_read_loop: do ip = 1,nreadparms

          if (ip == 17) then
            if (trkrinfo%use_land_mask == 'y' .or.
     &          trkrinfo%use_land_mask == 'Y') then
              continue
            else
              if (verb .ge. 3) then
                print *,' '             
                print *,'The use_land_mask flag has not been set to  '
                print *,'y or Y, so we will not try to read it in... '
                print *,' '             
                cycle grib2_standard_parm_read_loop
              endif
            endif
          endif

          !
          ! ---  Initialize Variables ---
          !

          gfld%idsect => NULL()
          gfld%local => NULL()
          gfld%list_opt => NULL()
          gfld%igdtmpl => NULL()
          gfld%ipdtmpl => NULL()
          gfld%coord_list => NULL()
          gfld%idrtmpl => NULL()
          gfld%bmap => NULL()
          gfld%fld => NULL()

          if (ip == 17) then
            jdisc=1 ! hydrological products.  At this point, used only 
                    ! for the land-sea mask.
          else
            jdisc=0 ! meteorological products
          endif
          jids=-9999
          jpdtn=trkrinfo%g2_jpdtn ! 0 = analysis or forecast; 
                                  ! 1 = ens fcst
          jgdtn=0 ! lat/lon grid
          jgdt=-9999
          jpdt=-9999

          npoints=0
          icount=0
          jskp=0

c         Search for input parameter by production template 4.0.  This
c         tave program is used primarily for temperature, but still we
c         will leave that as a variable and not-hard wire it in case we
c         choose to average something else in the future.

          ! We are looking for Temperature or GP Height here.  This
          ! block of code, or even the smaller subset block of code that
          ! contains the JPDT(1) and JPDT(2) assignments, can of course
          ! be modified if this program is to be used for interpolating
          ! other variables....

          ! Set defaults for JPDT, then override in array
          ! assignments below...

          JPDT(1:15)=(/-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &               ,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

          JPDT(1) = ig2_parm_cat(ip)
          JPDT(2) = ig2_parm_num(ip)

          if (inp%lt_units == 'minutes') then
            JPDT(8) = 0
            JPDT(9) = iftotalmins(ifh)
          else
            JPDT(8) = 1
            JPDT(9) = ifhours(ifh)
          endif

          JPDT(10) = ig2_lev_typ(ip)
          if (JPDT(10) == 100) then   ! isobaric surface
            JPDT(12) = ig2_lev_val(ip) * 100  !  GRIB2 levels are in Pa
          else
            JPDT(12) = ig2_lev_val(ip) ! This is going to be either mslp
     &                                 ! or 10m winds.
          endif

          if ( verb_g2 .ge. 1 ) then
            print *,'before getgb2 call, value of unpack = ',unpack
          endif

          inquire (unit=lugb, opened=file_open)
          if (file_open) then
            if (verb .ge. 3) then
              print *,'TEST b4 getgb2 getdata, unit lugb= ',lugb
     &               ,' is OPEN'
            endif
          else
            if (verb .ge. 3) then
              print *,'TEST b4 getgb2 getdata, unit lugb= ',lugb
     &               ,' is CLOSED'
            endif
          endif

          inquire (unit=lugi, opened=file_open)
          if (file_open) then
            if (verb .ge. 3) then
              print *,'TEST b4 getgb2 getdata, unit lugi= ',lugi
     &               ,' is OPEN'
            endif
          else
            if (verb .ge. 3) then
              print *,'TEST b4 getgb2 getdata, unit lugi= ',lugi
     &               ,' is CLOSED'
            endif
          endif

          if(enable_timing/=0) then
             call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &            ,date_time)
             write (6,531) date_time(5),date_time(6),date_time(7)
 531        format (1x,'TIMING: before getgb2-1',i2.2,':',i2.2,':',i2.2)
          endif
          call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt
     &             ,unpack,krec,gfld,iret)

          if(enable_timing/=0) then
             call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &            ,date_time)
             write (6,532) date_time(5),date_time(6),date_time(7)
 532         format (1x,'TIMING: after getgb2-1',i2.2,':',i2.2,':',i2.2)
          endif

          if ( verb .ge. 3 ) then
            print *,'iret from getgb2 in getdata = ',iret
          endif

          if (verb_g2 .ge. 1) then
            print *,'after getgb2 call, value of unpacked = '
     &             ,gfld%unpacked
            print *,'after getgb2 call, gfld%ngrdpts = ',gfld%ngrdpts
            print *,'after getgb2 call, gfld%ibmap = ',gfld%ibmap
          endif

          if ( iret == 0) then

            if ( verb .ge. 3 ) then
              print *,'+++ Good Read: getgb2 found parm: ',chparm(ip)
              print *,'+++       at level = ',ig2_lev_val(ip)
              if (inp%lt_units == 'minutes') then
                print *,'+++       Forecast time = ',iftotalmins(ifh)
     &               ,' minutes'
              else
                print *,'+++       Forecast time = ',ifhours(ifh)
     &                 ,' hours'
              endif
            endif

c           Determine packing information from GRIB2 file
c           The default packing is 40  JPEG 2000

            ipack = 40

            if (verb_g2 .ge. 1) then
              print *,' gfld%idrtnum = ', gfld%idrtnum
            endif

            !   Set DRT info  ( packing info )
            if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
              ipack = 0
            elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
              ipack = 2
            elseif ( gfld%idrtnum.eq.3 ) then  ! Complex & spatial
     &                                         ! packing
              ipack = 31
            elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) then
              ! JPEG 2000 packing
              ipack = 40
            elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
              ipack = 41
            endif

            if ( verb_g2 .ge. 1 ) then
              print *,'After check of idrtnum, ipack= ',ipack
              print *,'Number of gridpts= gfld%ngrdpts= ',gfld%ngrdpts
              print *,'Number of elements= gfld%igdtlen= ',gfld%igdtlen
              print *,'GDT num= gfld%igdtnum= ',gfld%igdtnum
            endif

            kf = gfld%ngrdpts  ! Number of gridpoints returned from read

            do np = 1,kf
              f(np)  = gfld%fld(np)
              if (gfld%ibmap == 0) then
                lb(np)  = gfld%bmap(np)
              else
                lb(np)  = .true.
              endif
            enddo

            readflag(ip) = .TRUE.
            call bitmapchk(kf,lb,f,dmin,dmax)

c           Convert logical bitmap to 2-d array (only need to do this
c           once since using same model for all variables).

            if (lbrdflag .eq. 'n') then
              call conv1d2d_logic (imax,jmax,lb,valid_pt
     &                                           ,need_to_flip_lats)
              lbrdflag = 'y'
            endif

            firstval=gfld%fld(1)
            lastval=gfld%fld(kf)

            if (verb_g2 .ge. 1) then
              print *,' '
              print *,' SECTION 0: discipl= ',gfld%discipline
     &               ,' gribver= ',gfld%version
              print *,' '
              print *,' SECTION 1: '

              do j = 1,gfld%idsectlen
                 print *,'     sect1, j= ',j,' gfld%idsect(j)= '
     &                  ,gfld%idsect(j)
              enddo

              if ( associated(gfld%local).AND.gfld%locallen.gt.0) then
                print *,' '
                print *,' SECTION 2: ',gfld%locallen,' bytes'
              else
                print *,' '
                print *,' SECTION 2 DOES NOT EXIST IN THIS RECORD'
              endif

              print *,' '
              print *,' SECTION 3: griddef= ',gfld%griddef
              print *,'            ngrdpts= ',gfld%ngrdpts
              print *,'            numoct_opt= ',gfld%numoct_opt
              print *,'            interp_opt= ',gfld%interp_opt
              print *,'            igdtnum= ',gfld%igdtnum
              print *,'            igdtlen= ',gfld%igdtlen

              print *,' '
              print '(a17,i3,a2)',' GRID TEMPLATE 3.',gfld%igdtnum,': '
              do j=1,gfld%igdtlen
                print *,'    j= ',j,' gfld%igdtmpl(j)= ',gfld%igdtmpl(j)
              enddo

c             Get parameter abbrev for record that was retrieved
              print *,' '
              print *,'     PDT num (gfld%ipdtnum) = ',gfld%ipdtnum
              print *,' '
              print '(a20,i3,a2)',' PRODUCT TEMPLATE 4.',gfld%ipdtnum
     &             ,': '
              do j=1,gfld%ipdtlen
                print *,'    sect 4  j= ',j,' gfld%ipdtmpl(j)= '
     &                 ,gfld%ipdtmpl(j)
              enddo
            endif
   
            pdt_4p0_vtime      = gfld%ipdtmpl(9)
            pdt_4p0_vert_level = gfld%ipdtmpl(12)

            pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1)
     &                          ,gfld%ipdtmpl(2))

            if (verb .ge. 3) then
              print *,' '
              write (6,131)
 131          format (' rec#   param     level  byy  bmm  bdd  bhh  '
     &               ,'fhr      npts  firstval    lastval     minval   '
     &               ,'   maxval')
              print '(i5,3x,a8,2x,6i5,2x,i8,4g12.4)'
     &            ,krec,pabbrev,pdt_4p0_vert_level/100,gfld%idsect(6)
     &            ,gfld%idsect(7),gfld%idsect(8),gfld%idsect(9)
     &            ,pdt_4p0_vtime,gfld%ngrdpts,firstval,lastval,dmin,dmax
            endif

            select case (chparm(ip))
              case ('absv')
                if (jpdt(12) == 85000) then
                  call conv1d2d_real (imax,jmax,f,zeta(1,1,1)
     &                                           ,need_to_flip_lats)
                else
                  call conv1d2d_real (imax,jmax,f,zeta(1,1,2)
     &                                           ,need_to_flip_lats)
                endif
              case ('ugrid')
                if (jpdt(12) == 85000) then
                  call conv1d2d_real (imax,jmax,f,u(1,1,1)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 70000) then
                  call conv1d2d_real (imax,jmax,f,u(1,1,2)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 50000) then
                  call conv1d2d_real (imax,jmax,f,u(1,1,3)
     &                                           ,need_to_flip_lats)
                else
                  ! Near-surface data
                  call conv1d2d_real (imax,jmax,f,u(1,1,4)
     &                                           ,need_to_flip_lats)
                endif
              case ('vgrid')
                if (jpdt(12) == 85000) then
                  call conv1d2d_real (imax,jmax,f,v(1,1,1)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 70000) then
                  call conv1d2d_real (imax,jmax,f,v(1,1,2)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 50000) then
                  call conv1d2d_real (imax,jmax,f,v(1,1,3)
     &                                           ,need_to_flip_lats)
                else
                  ! Near-surface data
                  call conv1d2d_real (imax,jmax,f,v(1,1,4)
     &                                           ,need_to_flip_lats)
                endif
              case ('gphgt')
                if (jpdt(12) == 85000) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,1)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 70000) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,2)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 50000) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,3)
     &                                           ,need_to_flip_lats)
                else if (jpdt(12) == 20000) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,4)
     &                                           ,need_to_flip_lats)
                endif
              case ('mslp')
                call conv1d2d_real (imax,jmax,f,slp
     &                                           ,need_to_flip_lats)
              case ('temp')
                call conv1d2d_real (imax,jmax,f,tmean
     &                                           ,need_to_flip_lats)
              case ('lmask')
                call conv1d2d_real (imax,jmax,f,lsmask
     &                                           ,need_to_flip_lats)
              case default

              if ( verb .ge. 1 ) then
                print *,'!!! ERROR: BAD CHPARM IN GETDATA = ',chparm(ip)
              endif

            end select

          else

            if ( verb .ge. 3 ) then
              print *,'!!! NOTE: getgb2 could not find parm: '
     &               ,chparm(ip)
              print *,'!!!       at level = ',ig2_lev_val(ip)
              if (inp%lt_units == 'minutes') then
                print *,'!!!       Forecast time = ',iftotalmins(ifh)
     &               ,' minutes'
              else
                print *,'!!!       Forecast time = ',ifhours(ifh)
     &                 ,' hours'
              endif
            endif

          endif

          call gf_free (gfld)

        enddo grib2_standard_parm_read_loop

c       *------------------------------------------------------------*
c        If we are attempting to determine the cyclone structure,
c        then read in data now that will allow us to do that.
c        This is the GRIB2 reading section.
c       *------------------------------------------------------------*

        if (phaseflag == 'y') then

          if (phasescheme == 'cps' .or. phasescheme == 'both') then

            ! Read in GP Height levels for cyclone phase space...

            grib2_cps_parm_lev_loop: do ip = 1,nlevs_cps

              !
              ! ---  Initialize Variables ---
              !
      
              gfld%idsect => NULL()
              gfld%local => NULL()
              gfld%list_opt => NULL()
              gfld%igdtmpl => NULL()
              gfld%ipdtmpl => NULL()
              gfld%coord_list => NULL()
              gfld%idrtmpl => NULL()
              gfld%bmap => NULL()
              gfld%fld => NULL()

              jdisc=0
              jids=-9999
              jpdtn=trkrinfo%g2_jpdtn ! 0 = analysis or forecast; 
                                      ! 1 = ens fcst
              jgdtn=0
              jgdt=-9999
              jpdt=-9999

              npoints=0
              icount=0
              jskp=0

              jpds = -1
              jgds = -1
              j=0

              ! Set defaults for JPDT, then override in array
              ! assignments below...

              JPDT(1:15)=(/-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &           ,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

              JPDT(1) = cpsig2_parm_cat(ip)
              JPDT(2) = cpsig2_parm_num(ip)

              if (inp%lt_units == 'minutes') then
                JPDT(8) = 0
                JPDT(9) = iftotalmins(ifh)
              else
                JPDT(8) = 1
                JPDT(9) = ifhours(ifh)
              endif

              JPDT(10) = cpsig2_lev_typ(ip)
              if (JPDT(10) == 100) then   ! isobaric surface
                JPDT(12) = cpsig2_lev_val(ip) * 100  ! GRIB2 levels 
                                                     ! are in Pa
              else
                if (verb .ge. 3) then
                  print *,' '
                  print *,'ERROR in getdata: JPDT(10) array value'
                  print *,'should only be 100 in this CPS section'
                  print *,'for GRIB2 data.'
                endif
              endif

              if(enable_timing/=0) then
                 call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                ,date_time)
                 write (6,731) date_time(5),date_time(6),date_time(7)
 731             format (1x,'TIMING: before getgb2-2',i2.2,':',i2.2,':'
     &                ,i2.2)
              endif
              call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn
     &                   ,jgdt,unpack,krec,gfld,iret)
              if(enable_timing/=0) then
                 call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                ,date_time)
                 write (6,732) date_time(5),date_time(6),date_time(7)
 732             format (1x,'TIMING: after getgb2-2',i2.2,':',i2.2,':'
     &                ,i2.2)
              endif

              if ( verb .ge. 3 ) then
                print *,'iret from getgb2 (PHASE) in getdata = ',iret
              endif

              if (verb_g2 .ge. 1) then
                print *,'after getgb2 call(PHASE),'
     &                 ,' value of unpacked = ',gfld%unpacked
                print *,'after getgb2 (PHASE) call, gfld%ngrdpts = '
     &                 ,gfld%ngrdpts
                print *,'after getgb2 (PHASE) call, gfld%ibmap = '
     &                 ,gfld%ibmap
              endif

              if (verb .ge. 3) then
                print *,' '
                if (inp%lt_units == 'minutes') then
                  print *,'After getgb2 (PHASE) call, j= ',j
     &                 ,' ifmins= ',iftotalmins(ifh),' parm # (ip) = '
     &                 ,ip,' iret= ',iret
                else
                  print *,'After getgb2 (PHASE) call, j= ',j
     &                 ,' ifhours= ',ifhours(ifh),' parm # (ip) = '
     &                 ,ip,' iret= ',iret
                endif
              endif

              if (iret == 0) then

c               Determine packing information from GRIB2 file.
c               The default packing is 40  JPEG 2000

                ipack = 40

                if (verb_g2 .ge. 1) then
                  print *,' gfld%idrtnum = ', gfld%idrtnum
                endif

                !   Set DRT info  ( packing info )
                if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
                  ipack = 0
                elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
                  ipack = 2
                elseif ( gfld%idrtnum.eq.3 ) then  ! Complex & spatial
     &                                             ! packing
                  ipack = 31
                elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) 
     &          then
                  ! JPEG 2000 packing
                  ipack = 40
                elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
                  ipack = 41
                endif

                if ( verb_g2 .ge. 1 ) then
                  print *,'After check of idrtnum, ipack= ',ipack
                  print *,'Number of gridpts= gfld%ngrdpts= '
     &                   ,gfld%ngrdpts
                  print *,'Number of elements= gfld%igdtlen= '
     &                   ,gfld%igdtlen
                  print *,'GDT num= gfld%igdtnum= ',gfld%igdtnum
                endif 

                kf = gfld%ngrdpts  ! Number of gridpoints returned 
                                 ! from read

                do np = 1,kf
                  f(np)  = gfld%fld(np)
                  if (gfld%ibmap == 0) then
                    lb(np)  = gfld%bmap(np)
                  else
                    lb(np)  = .true.
                  endif
                enddo

                call bitmapchk(kf,lb,f,dmin,dmax)

c               Convert logical bitmap to 2-d array (only need to do 
c               this once since using same model for all variables).

                if (lbrdflag .eq. 'n') then
                  call conv1d2d_logic (imax,jmax,lb,valid_pt
     &                                           ,need_to_flip_lats)
                  lbrdflag = 'y'
                endif

                firstval=gfld%fld(1)
                lastval=gfld%fld(kf)

                if (verb_g2 .ge. 1) then
                  print *,' '
                  print *,' SECTION 0: discipl= ',gfld%discipline
     &                   ,' gribver= ',gfld%version
                  print *,' '
                  print *,' SECTION 1: '

                  do j = 1,gfld%idsectlen
                    print *,'     sect1, j= ',j,' gfld%idsect(j)= '
     &                     ,gfld%idsect(j)
                  enddo

                  if ( associated(gfld%local).AND.gfld%locallen.gt.0)
     &            then
                    print *,' '
                    print *,' SECTION 2: ',gfld%locallen,' bytes'
                  else
                    print *,' '
                    print *,' SECTION 2 DOES NOT EXIST IN THIS RECORD'
                  endif

                  print *,' '
                  print *,' SECTION 3: griddef= ',gfld%griddef
                  print *,'            ngrdpts= ',gfld%ngrdpts
                  print *,'            numoct_opt= ',gfld%numoct_opt
                  print *,'            interp_opt= ',gfld%interp_opt
                  print *,'            igdtnum= ',gfld%igdtnum
                  print *,'            igdtlen= ',gfld%igdtlen

                  print *,' '
                  print '(a17,i3,a2)',' GRID TEMPLATE 3.'
     &                  ,gfld%igdtnum,': '
                  do j=1,gfld%igdtlen
                    print *,'    j= ',j,' gfld%igdtmpl(j)= '
     &                  ,gfld%igdtmpl(j)
                  enddo

c                 Get parameter abbrev for record that was retrieved
                  print *,' '
                  print *,'     PDT num (gfld%ipdtnum) = '
     &                   ,gfld%ipdtnum
                  print *,' '
                  print '(a20,i3,a2)',' PRODUCT TEMPLATE 4.'
     &                   ,gfld%ipdtnum
     &                 ,': '
                  do j=1,gfld%ipdtlen
                    print *,'    sect 4  j= ',j,' gfld%ipdtmpl(j)= '
     &                     ,gfld%ipdtmpl(j)
                  enddo
                endif

                pdt_4p0_vtime      = gfld%ipdtmpl(9)
                pdt_4p0_vert_level = gfld%ipdtmpl(12)

                pabbrev=param_get_abbrev(gfld%discipline
     &                  ,gfld%ipdtmpl(1),gfld%ipdtmpl(2))

                if (verb .ge. 3) then
                  print *,' '
                  write (6,231)
 231              format (' rec#   param     level  byy  bmm  bdd  '
     &             ,'bhh  '
     &             ,'fhr      npts  firstval    lastval     minval   '
     &             ,'   maxval')
                  print '(i5,3x,a8,2x,6i5,2x,i8,4g12.4)'
     &             ,krec,pabbrev,pdt_4p0_vert_level/100,gfld%idsect(6)
     &             ,gfld%idsect(7),gfld%idsect(8),gfld%idsect(9)
     &             ,pdt_4p0_vtime,gfld%ngrdpts,firstval,lastval,dmin
     &             ,dmax
                endif

c               Convert data to 2-d array

                call conv1d2d_real (imax,jmax,f,cpshgt(1,1,ip)
     &                             ,need_to_flip_lats)

              endif

              call gf_free (gfld)

            enddo grib2_cps_parm_lev_loop

          endif

        endif

      else

        !----------------------------------
        ! Reading from a GRIB v1 file....
        !----------------------------------

        grib1_read_loop: do ip = 1,nreadparms
              
          jpds = -1
          jgds = -1
          j=0     
              
          if (inp%model == 4) then  ! ECMWF hi-res data uses ECMWF table
            print *,' '
            print *,'WARNING: From the namelist, inp%model is set to a'
            print *,'   value of 4, which is for ECMWF, so in routine'
            print *,'   getdata_grib, the input jpds(5,6,7) parms are' 
            print *,'   going to have values that are specific for'
            print *,'   ECMWF GRIB1 data.'
            print *,' '
            jpds(5)  = ec_igparm(ip) 
            jpds(6)  = ec_iglevtyp(ip)
            jpds(7)  = ec_iglev(ip)
          else   ! All other models use NCEP-standard GRIB table
            jpds(5)  = igparm(ip)
            jpds(6)  = iglevtyp(ip) 
            jpds(7)  = iglev(ip)
          endif 

          print *,' '
          print *,' --- Before getgb, jpds(5)= ',jpds(5)
          print *,' ---             , jpds(6)= ',jpds(6)
          print *,' ---             , jpds(7)= ',jpds(7)
                
          if (jpds(5) == 999) then
            cycle 
          endif   

          if (inp%lt_units == 'minutes') then
            jpds(14) = iftotalmins(ifh)
          else
            jpds(14) = ifhours(ifh)
          endif

          if(enable_timing/=0) then
             call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &            ,date_time)
             write (6,831) date_time(5),date_time(6),date_time(7)
 831         format (1x,'TIMING: before getgb-1',i2.2,':',i2.2,':',i2.2)
          endif 

          call getgb (lugb,lugi,jf,j,jpds,jgds,
     &                          kf,k,kpds,kgds,lb,f,iret)

          if (enable_timing /= 0) then
            call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &           ,date_time)
            write (6,832) date_time(5),date_time(6),date_time(7)
 832        format (1x,'TIMING: after getgb-1',i2.2,':',i2.2,':',i2.2)
          endif     

          if ( verb .ge. 3 ) then
            print *,' '
            if (inp%lt_units == 'minutes') then
              print *,'After getgb call, j= ',j,' k= ',k
     &             ,' iftotalmins= '
     &             ,iftotalmins(ifh),' parm # (ip) = ',ip,' iret= ',iret
            else
              print *,'After getgb call, j= ',j,' k= ',k,' ifhours= '
     &             ,ifhours(ifh),' parm # (ip) = ',ip,' iret= ',iret
            endif
          endif

          if (iret == 0) then

            if ( verb .ge. 3 ) then
              print *,'+++ Good Read: getgb found parm: ',chparm(ip)
              print *,'+++       at level = ',jpds(7)
              if (inp%lt_units == 'minutes') then
                print *,'+++       Forecast time = ',iftotalmins(ifh)
     &               ,' minutes'
              else
                print *,'+++       Forecast time = ',ifhours(ifh)
     &                 ,' hours'
              endif
            endif
          
            readflag(ip) = .TRUE.
            call bitmapchk(kf,lb,f,dmin,dmax)

            if ( verb .ge. 3 ) then
              if (inp%lt_units == 'minutes') then
                write (6,29) 
              else
                write (6,31) 
              endif
 29           format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  fmin'
     &             ,'  npts  minval       maxval') 
 31           format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  fhr '
     &             ,'  npts  minval       maxval') 
              print '(i4,2x,8i5,i8,2g12.4)',
     &             k,(kpds(i),i=5,11),kpds(14),kf,dmin,dmax
            endif

c           Convert logical bitmap to 2-d array (only need to do this
c           once since using same model for all variables).

            if (lbrdflag .eq. 'n') then
              call conv1d2d_logic (imax,jmax,lb,valid_pt
     &                                           ,need_to_flip_lats)
              lbrdflag = 'y'
            endif


            select case (chparm(ip))
              case ('absv')
                if (jpds(7) == 850) then
                  call conv1d2d_real (imax,jmax,f,zeta(1,1,1)
     &                                           ,need_to_flip_lats)
                else
                  call conv1d2d_real (imax,jmax,f,zeta(1,1,2)
     &                                           ,need_to_flip_lats)
                endif
              case ('ugrid')
                if (jpds(7) == 850) then
                  call conv1d2d_real (imax,jmax,f,u(1,1,1)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 700) then
                  call conv1d2d_real (imax,jmax,f,u(1,1,2)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 500) then
                  call conv1d2d_real (imax,jmax,f,u(1,1,3)
     &                                           ,need_to_flip_lats)
                else
                  ! Near-surface data
                  call conv1d2d_real (imax,jmax,f,u(1,1,4)
     &                                           ,need_to_flip_lats)
                endif
              case ('vgrid')
                if (jpds(7) == 850) then
                  call conv1d2d_real (imax,jmax,f,v(1,1,1)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 700) then
                  call conv1d2d_real (imax,jmax,f,v(1,1,2)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 500) then
                  call conv1d2d_real (imax,jmax,f,v(1,1,3)
     &                                           ,need_to_flip_lats)
                else
                  ! Near-surface data
                  call conv1d2d_real (imax,jmax,f,v(1,1,4)
     &                                           ,need_to_flip_lats)
                endif
              case ('gphgt')
                if (jpds(7) == 850) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,1)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 700) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,2)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 500) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,3)
     &                                           ,need_to_flip_lats)
                else if (jpds(7) == 200) then
                  call conv1d2d_real (imax,jmax,f,hgt(1,1,4)
     &                                           ,need_to_flip_lats)
                endif
              case ('mslp')
                call conv1d2d_real (imax,jmax,f,slp
     &                                           ,need_to_flip_lats)
              case ('temp')
                call conv1d2d_real (imax,jmax,f,tmean
     &                                           ,need_to_flip_lats)
              case ('lmask')
                call conv1d2d_real (imax,jmax,f,lsmask
     &                                           ,need_to_flip_lats)
              case default

              if ( verb .ge. 1 ) then
                print *,'!!! ERROR: BAD CHPARM IN GETDATA = ',chparm(ip)
              endif

            end select

          else

            if ( verb .ge. 3 ) then
              print *,'!!! NOTE: getgb could not find parm: ',chparm(ip)
              print *,'!!!       at level = ',jpds(7)
              if (inp%lt_units == 'minutes') then
                print *,'!!!       Forecast time = ',iftotalmins(ifh)
     &               ,' minutes'
              else
                print *,'!!!       Forecast time = ',ifhours(ifh)
     &                 ,' hours'
              endif
            endif

          endif

        enddo grib1_read_loop

c       *------------------------------------------------------------*
c        If we are attempting to determine the cyclone structure,
c        then read in data now that will allow us to do that.
c       *------------------------------------------------------------*

        if (phaseflag == 'y') then

          if (phasescheme == 'cps' .or. phasescheme == 'both') then

            ! Read in GP Height levels for cyclone phase space...

            cps_grib1_lev_loop: do ip = 1,nlevs_cps

              jpds = -1
              jgds = -1
              j=0

              if (inp%model == 4) then
                ! Use different grib parm id for ECMWF GP height
                jpds(5)  = ec_cpsgparm(ip)
              else
                jpds(5)  = cpsgparm(ip)
              endif
              jpds(6)  = cpsglevtyp(ip)
              jpds(7)  = cpsglev(ip)

              if (inp%lt_units == 'minutes') then
                jpds(14) = iftotalmins(ifh)
              else    
                jpds(14) = ifhours(ifh)
              endif

              if(enable_timing/=0) then
                 call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                ,date_time)
                 write (6,841) date_time(5),date_time(6),date_time(7)
 841             format (1x,'TIMING: before getgb-2',i2.2,':',i2.2
     &                ,':',i2.2)
              endif
              call getgb (lugb,lugi,jf,j,jpds,jgds,
     &                         kf,k,kpds,kgds,lb,f,iret)
              if(enable_timing/=0) then
                 call date_and_time (big_ben(1),big_ben(2),big_ben(3)
     &                ,date_time)
                 write (6,842) date_time(5),date_time(6),date_time(7)
 842             format (1x,'TIMING: after getgb-2',i2.2,':',i2.2
     &                ,':',i2.2)
              endif

              if ( verb .ge. 3 ) then
                print *,' '
                if (inp%lt_units == 'minutes') then
                  print *,'After getgb (PHASE) call, j= ',j,' k= ',k
     &                 ,' ifmins= ',iftotalmins(ifh),' parm # (ip) = '
     &                 ,ip,' iret= ',iret
                else
                  print *,'After getgb (PHASE) call, j= ',j,' k= ',k
     &                 ,' ifhours= ',ifhours(ifh),' parm # (ip) = '
     &                 ,ip,' iret= ',iret
                endif
              endif

              if (iret == 0) then

                call bitmapchk(kf,lb,f,dmin,dmax)

                if ( verb .ge. 3 ) then
                  if (inp%lt_units == 'minutes') then    
                    write (6,39)
                  else  
                    write (6,41)
                  endif
 39               format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  '
     &                 ,'fmin   npts  minval       maxval')
 41               format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  '
     &                 ,'fhr   npts  minval       maxval')
                  print '(i4,2x,8i5,i8,2g12.4)',
     &                 k,(kpds(i),i=5,11),kpds(14),kf,dmin,dmax
                endif
                
c               Convert data to 2-d array

                call conv1d2d_real (imax,jmax,f,cpshgt(1,1,ip)
     &                             ,need_to_flip_lats)

              endif

            enddo cps_grib1_lev_loop

          endif

        endif

      endif
c
      deallocate (f)
      deallocate (lb)
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine getdata_netcdf (ncfile_id,readflag,valid_pt,imax,jmax
     &                  ,ifh,need_to_flip_lats,need_to_flip_lons
     &                  ,ncfile_tmax,netcdfinfo)
c
c     ABSTRACT: This subroutine reads the input NetCDF file for the 
c     tracked parameters for one lead time.
c
c     There are up to 9 fields that are read in that will be used to
c     locate the storm position.  There are an additional 4 variables
c     (500 mb u- and v-components and 10 m u- and v- components) that
c     will not be used for tracking, but only for helping to estimate
c     the next first guess position (500 mb winds) and for estimating
c     the max near-surface wind speeds in the vicinity of the storm
c     (10 m winds).
c
c     Fields read in are listed here.  Numbers indicate positioning in
c     the readflag logical array:
c
c     1.   850 mb absolute vorticity
c     2.   700 mb absolute vorticity
c     3.   850 mb u-component
c     4.   850 mb v-component
c     5.   700 mb u-component
c     6.   700 mb v-component
c     7.   850 mb geopotential height
c     8.   700 mb geopotential height
c     9.   MSLP
c     10.  10-m u-component
c     11.  10-m v-component
c     12.  500 mb u-component
c     13.  500 mb v-component
c     14.  300-500 mb mean temperature
c     15.  500 mb geopotential height
c     16.  200 mb geopotential height
c     17.  Land-Sea mask -- This is for tcgen applications only, and 
c               even there, it's optional.
c
c     If the user has requested to check the cyclone phase space for
c     this run (phaseflag set to 'y' and phasescheme set to 'cps'), 
c     then we need to have gp height data for 900-300 mb at every 50
c     mb.  Some of those levels for gp height data were already read
c     in with the read of the initial 17 parameters, but we will be
c     sure to read in the others, if requested.
c
c     INPUT:
c     ncfile_id   integer ID associated with the NetCDF file
c     imax        integer number of pts in i-direction on grid
c     jmax        integer number of pts in j-direction on grid
c     ifh         integer index for forecast hour
c     need_to_flip_lats logical flag read in from getgridinfo that
c                 indicates if data needs flipped north to south
c     need_to_flip_lons logical flag read in from getgridinfo that
c                 indicates if data needs flipped east to west
c     ncfile_tmax integer with max number of time levels in the input
c                 NetCDF file, as read in from the NetCDF file
c                 itself in subroutine  read_netcdf_fhours.
c
c     OUTPUT:
c     readflag    logical array, indicates if a parm was read in
c     valid_pt    logical array, indicates for each (i,j) if there is
c                 valid data at the point (used for regional grids)

      USE tracked_parms; USE level_parms; USE inparms; USE phase
      USE netcdf_parms; USE verbose_output

      implicit none
c
      type (netcdfstuff) netcdfinfo
      integer, parameter :: nreadparms=17,nreadparms_cps=13
      real, allocatable :: f(:)
      real :: dmin,dmax,xmissing_value,xfill_value
      logical(1) valid_pt(imax,jmax),readflag(nreadparms)
      logical(1) ::  need_to_flip_lats,need_to_flip_lons
      character*1  :: lbrdflag,match_check
      character*30 :: chparm(nreadparms)
      character*30 :: chparm_cps(nreadparms_cps)
      integer, intent(in) :: ncfile_id,imax,jmax
      integer :: igvret,ifa,ip,ifh,i,j,k,m,n,ncfile_tmax,nf_get_att_real
      integer :: nf_get_att_double,nf_inq_attlen,imvlen,ifvlen
      integer :: usertime,ncix,missing_val_length,nf_status
      integer :: nf_inq_varid,varid
c
      lbrdflag = 'n'

cnc      data cpsgparm   /13*7/
cnc      data cpsglevtyp /13*100/
cnc      data cpsglev    /900,850,800,750,700,650,600,550,500,450,400
cnc     &                ,350,300/

c      data chparm   /'vort850','vort700'
c     &              ,'u850','v850','u700','v700'
c     &              ,'h850','h700','slp','u_ref','v_ref'
c     &              ,'u500','v500','tm'/

c     Load the names of the NetCDF variables for the standard 
c     variables into the chparm array...

      chparm(1)  = netcdfinfo%rv850name
      chparm(2)  = netcdfinfo%rv700name
      chparm(3)  = netcdfinfo%u850name
      chparm(4)  = netcdfinfo%v850name
      chparm(5)  = netcdfinfo%u700name
      chparm(6)  = netcdfinfo%v700name
      chparm(7)  = netcdfinfo%z850name
      chparm(8)  = netcdfinfo%z700name
      chparm(9)  = netcdfinfo%mslpname
      chparm(10) = netcdfinfo%usfcname
      chparm(11) = netcdfinfo%vsfcname
      chparm(12) = netcdfinfo%u500name
      chparm(13) = netcdfinfo%v500name
      chparm(14) = netcdfinfo%tmean_300_500_name
      chparm(15) = netcdfinfo%z500name
      chparm(16) = netcdfinfo%z200name
      chparm(17) = netcdfinfo%lmaskname
 
      
      if (verb .ge. 3) then
        print *,' '
        print *,'NOTE: Program is now in subroutine  getdata_netcdf.'
      endif

      if (allocated(f)) deallocate(f)
      allocate (f(imax*jmax),stat=ifa)
      if (ifa /= 0) then
        print *,' '
        print *,'!!! ERROR in getdata_netcdf allocating f data array.'
        print *,'!!! ifa = ',ifa
        print *,'!!! STOPPING EXECUTION'
        STOP 91
      endif

      !---------------------------------------------------------------
      ! First go through the list of user-requested lead times that 
      ! were read in from subroutine  read_fhours and try to match up
      ! the lead times that were read in with the lead times that 
      ! we read in directly from the NetCDF file.  Get the index from
      ! the NetCDF file for that lead time and use that in the call to
      ! the read routine (get_var3_tlev_double).
      !---------------------------------------------------------------

      usertime = iftotalmins(ifh)

      match_check = 'n'

      find_index_loop: do m = 1,ncfile_tmax

        if (usertime == nctotalmins(m)) then
          ncix = m
          if (verb .ge. 1) then
            print *,'+++ Time match in getdata_netcdf for usertime= '
     &             ,usertime,'  netcdf file index= ncix= ',ncix
          endif
          match_check = 'y'
          exit find_index_loop
        endif

      enddo find_index_loop

      if (match_check == 'n') then
        print *,' '
        print *,'!!! ERROR in getdata_netcdf: '
        print *,'  For a NetCDF file, the user has '
        print *,'  requested to process a lead time, and that lead'
        print *,'  time does not exist in the NetCDF list of time'
        print *,'  values. '
        print *,'  ifh= ',ifh
        print *,'  usertime= iftotalmins(ifh)= ',iftotalmins(ifh)
        print *,'  STOPPING....'
        stop 99
      endif

      !---------------------------------------------------------------
      ! Now go through the read loop for the list of parameters
      !---------------------------------------------------------------

      netcdf_standard_parm_read_loop: do ip = 1,nreadparms

        if (chparm(ip) == 'X' .or. chparm(ip) == 'x') then
          if (verb .ge. 3) then
            print *,' '
            print *,'!!! NetCDF read NOT requested for parm # ',ip
          endif
          cycle netcdf_standard_parm_read_loop
        else
          if (verb .ge. 3) then
            print *,' '
            print *,'+++ NetCDF read requested for parm # ',ip
     &             ,' ... parm= ',chparm(ip)
          endif
        endif

        ! Note that I am sending a 1-d array, "f", to the netcdf read
        ! routine.   While that routine returns a 2-d array (which we 
        ! want), depending on the model & grid, we may need to flip the
        ! grid in the north-south direction.  I already have a routine
        ! for converting data from a 1-d to a 2-d array, and it has 
        ! the functionality for flipping a grid, so I programmed it as
        ! getting a 1-d array from the netcdf read routine and send that
        ! 1-d array to conv1d2d_real.

        call get_var3_tlev_double (ncfile_id,chparm(ip),imax,jmax,ncix
     &                  ,f,igvret)

        if (verb .ge. 3) then
          print *,' '
          print *,'After read, parm= ',chparm(ip),' ifh= ',ifh
     &           ,' lead time index= ',ltix(ifh),' parm# (ip) = ',ip
     &           ,' ncix= ',ncix,' igvret= ',igvret
        endif

        if (igvret == 0) then

c          call bitmapchk(kf,lb,f,dmin,dmax)

          readflag(ip) = .TRUE.
          dmin = minval(f)
          dmax = maxval(f)

          ! Need to get the value of the "missing_value" attribute for
          ! this variable from the list of attributes in the NetCDF
          ! file.  Only do this for the first lead time, since the 
          ! value of the "missing_value" obviously will not change 
          ! with lead time.

c          nf_status = nf_inq_attlen (ncfile_id,varid,"missing_value"
c     &               ,imvlen)
c          nf_status = nf_inq_attlen (ncfile_id,varid,"_FillValue"
c     &               ,ifvlen)

          ! These next two nf function calls retrieve the value of the
          ! "missing_value" attribute from the list of attributes for 
          ! the given variable being read in.  This is needed in order
          ! to know if a non-valid point is being accessed, as for a 
          ! regional grid, like the nested fvGFS.  In GRIB1/GRIB2 files,
          ! such regions would be bitmapped out, but in a NetCDF file,
          ! no such bitmap exists, so we have to check for missing 
          ! values.  In case it's a moving grid, we need to do this 
          ! for every lead time, since the "map of missing values" 
          ! will shift with lead time.  Once we have those missing
          ! values, we can loop through them and fill the valid_pt
          ! logical array so that, in the end, we will have the same
          ! logical bitmap for masking out missing data that we have
          ! with GRIB1/GRIB2 data.

          nf_status = nf_inq_varid (ncfile_id,chparm(ip),varid)

          print *,'nf_status from nf_inq_varid call = ',nf_status

          nf_status = nf_get_att_real (ncfile_id,varid,"missing_value"
     &               ,xmissing_value)

          print *,'nf_status from nf_get_att_real call = ',nf_status

c          nf_status = nf_get_att_real (ncfile_id,varid,"_FillValue"
c     &               ,xfill_value)
c          nf_status = nf90_inquire_attribute (ncfile_id,chparm(ip)
c     &               ,"missing_value",len=imvlen)
c          nf_status = nf90_inquire_attribute (ncfile_id,chparm(ip)
c     &               ,"_FillValue",len=ifvlen)
c
c          nf_status = nf90_get_att (ncfile_id,chparm(ip)
c     &               ,"missing_value",xmissing_value)
c          nf_status = nf90_get_att (ncfile_id,chparm(ip)
c     &               ,"_FillValue",xfill_value)

          if (verb .ge. 3) then
            write (6,31)
  31        format ('parmread lead time     parm#        parm_id   '
     &             ,23x,'minval       maxval')

            write (6,33) ifhours(ifh),ifclockmins(ifh),ip,chparm(ip)
     &                ,dmin,dmax
  33        format ('   ',i3,':',i2.2,14x,i3,10x,a30,1x,2g12.4)
            write (6,35) chparm(ip),xmissing_value
  35        format ('   --- ',a30,' missing value = ',g12.4)
          endif

          ! This call to conv1d2d_logic_netcdf creates
          ! a logical bitmap, so that in case we have 
          ! regional (non-global) data and an irregular grid (e.g., 
          ! the FV3 nested grid), we can mask out grid points that 
          ! have missing values as their data values.  There is not
          ! actually a native logical bitmap in NetCDF, so we will
          ! create one by examining the real data values and masking
          ! out grid points that have missing values.

          if (lbrdflag .eq. 'n') then
            call conv1d2d_logic_netcdf (imax,jmax,f,valid_pt
     &                    ,xmissing_value,need_to_flip_lats)
            lbrdflag = 'y'
          endif

          if (ip == 1) then         ! 850 mb absolute vorticity
            call conv1d2d_real (imax,jmax,f,zeta(1,1,1)
     &                                     ,need_to_flip_lats)
          else if (ip == 2) then    ! 700 mb absolute vorticity
            call conv1d2d_real (imax,jmax,f,zeta(1,1,2)
     &                                     ,need_to_flip_lats)
          else if (ip == 3) then    ! 850 mb u-comp
            call conv1d2d_real (imax,jmax,f,u(1,1,1)
     &                                     ,need_to_flip_lats)
          else if (ip == 4) then    ! 850 mb v-comp
            call conv1d2d_real (imax,jmax,f,v(1,1,1)
     &                                     ,need_to_flip_lats)
          else if (ip == 5) then    ! 700 mb u-comp
            call conv1d2d_real (imax,jmax,f,u(1,1,2)
     &                                     ,need_to_flip_lats)
          else if (ip == 6) then    ! 700 mb v-comp
            call conv1d2d_real (imax,jmax,f,v(1,1,2)
     &                                     ,need_to_flip_lats)
          else if (ip == 7) then    ! 850 mb gp height
            call conv1d2d_real (imax,jmax,f,hgt(1,1,1)
     &                                     ,need_to_flip_lats)
          else if (ip == 8) then    ! 700 mb gp height
            call conv1d2d_real (imax,jmax,f,hgt(1,1,2)
     &                                     ,need_to_flip_lats)
          else if (ip == 9) then    ! MSLP
            call conv1d2d_real (imax,jmax,f,slp
     &                                     ,need_to_flip_lats)
          else if (ip == 10) then   ! Near-sfc (10m) u-comp
            call conv1d2d_real (imax,jmax,f,u(1,1,4)
     &                                     ,need_to_flip_lats)
          else if (ip == 11) then   ! Near-sfc (10m) v-comp
            call conv1d2d_real (imax,jmax,f,v(1,1,4)
     &                                     ,need_to_flip_lats)
          else if (ip == 12) then   ! 500 mb u-comp
            call conv1d2d_real (imax,jmax,f,u(1,1,3)
     &                                     ,need_to_flip_lats)
          else if (ip == 13) then   ! 500 mb v-comp
            call conv1d2d_real (imax,jmax,f,v(1,1,3)
     &                                     ,need_to_flip_lats)
          else if (ip == 14) then   ! 300-500 mb mean Temp
            call conv1d2d_real (imax,jmax,f,tmean
     &                                     ,need_to_flip_lats)
          else if (ip == 15) then   ! 500 mb height
            call conv1d2d_real (imax,jmax,f,hgt(1,1,3)
     &                                     ,need_to_flip_lats)
          else if (ip == 16) then   ! 200 mb height
            call conv1d2d_real (imax,jmax,f,hgt(1,1,4)
     &                                     ,need_to_flip_lats)
          else if (ip == 17) then   ! Land-sea mask
            call conv1d2d_real (imax,jmax,f,lsmask
     &                                     ,need_to_flip_lats)
          else

            print *,'!!! NOTE: Parm not recognized. '
            print *,'!!!       ip is > 17.... ip= ',ip
            print *,'!!!       Forecast time level = ',ifh

          endif

        endif

      enddo netcdf_standard_parm_read_loop

c     *--------------------------------------------------------------*
c      If we are attempting to determine the cyclone structure using
c      Hart's cyclone phase space, then read in data now that will 
c      allow us to do that.  If we are instead just using the 
c      mid-level (300-500 mb) mean temperature to do that with a 
c      simple warm-core check, then that mean temperature field was
c      already read in above in the read loop for the standard 
c      variables.  The variables needed here for CPS are pretty 
c      straightforward:  gp height every 50 mb from 300 to 900 mb.
c      keep in mind that we have already read in a few of these 
c      gp height records for selected levels above.
c     *--------------------------------------------------------------*

      if (phaseflag == 'y') then

        if (phasescheme == 'cps' .or. phasescheme == 'both') then

          chparm_cps(1)  = netcdfinfo%z900name
          chparm_cps(2)  = netcdfinfo%z850name
          chparm_cps(3)  = netcdfinfo%z800name
          chparm_cps(4)  = netcdfinfo%z750name
          chparm_cps(5)  = netcdfinfo%z700name
          chparm_cps(6)  = netcdfinfo%z650name
          chparm_cps(7)  = netcdfinfo%z600name
          chparm_cps(8)  = netcdfinfo%z550name
          chparm_cps(9)  = netcdfinfo%z500name
          chparm_cps(10) = netcdfinfo%z450name
          chparm_cps(11) = netcdfinfo%z400name
          chparm_cps(12) = netcdfinfo%z350name
          chparm_cps(13) = netcdfinfo%z300name

          ! Read in GP Height levels for cyclone phase space...

          if (verb .ge. 3) then
            print *,' '
            print *,'--- Reads for CPS parms follow...'
            print *,' '
          endif

          netcdf_cps_parm_read_loop: do ip = 1,nreadparms_cps

            if (chparm_cps(ip) == 'X' .or. chparm_cps(ip) == 'x') then
              if (verb .ge. 3) then
                print *,'!!! ERROR: NetCDF read NOT requested for'
                print *,'!!! CPS parm # ',ip
                print *,'!!! You must have an error in your namelist.'
                print *,'!!! You have requested to do cyclone phase'
                print *,'!!! checking, so you need to include the '
                print *,'!!! NetCDF names for ALL requested gp height'
                print *,'!!! variables from 900 to 300 mb, every 50 '
                print *,'!!! mb,in the namelist.'
                print *,'!!! phaseflag is being set to NO (n), and '
                print *,'!!! phase-checking will NOT take place.'
                print *,'!!! If you want to run again and just do '
                print *,'!!! phase-checking with a simple warm-core'
                print *,'!!! check, then in the namelist set phaseflag'
                print *,'!!! to y and set phasescheme to vtt.'
                phaseflag = 'n'
              endif
              exit netcdf_cps_parm_read_loop
            else
              if (verb .ge. 3) then
                print *,'+++ NetCDF read requested for cps parm # ',ip
     &                 ,' ... parm= ',chparm_cps(ip)
              endif
            endif

            ! As above, we send a 1-d array, "f", to the netcdf read
            ! routine.   While that routine returns a 2-d array (which 
            ! we want), depending on the model & grid, we may need to 
            ! flip the grid in the north-south direction.  I already 
            ! have a routine for converting data from a 1-d to a 2-d 
            ! array, and it has the functionality for flipping a grid, 
            ! so I programmed it as getting a 1-d array from the netcdf
            ! read routine and send that 1-d array to conv1d2d_real.

            call get_var3_tlev_double (ncfile_id,chparm_cps(ip),imax
     &                  ,jmax,ncix,f,igvret)

            if (verb .ge. 3) then
              print *,' '
              print *,'After read, parm= ',chparm_cps(ip),' ifh= ',ifh
     &               ,' lead time index= ',ltix(ifh),' parm# (ip) = ',ip
     &               ,' ncix= ',ncix,' igvret= ',igvret
            endif

            if (igvret == 0) then

c              call bitmapchk(kf,lb,f,dmin,dmax)

              readflag(ip) = .TRUE.
              dmin = minval(f)
              dmax = maxval(f)

c              nf_status = nf_get_att_double (ncfile_id,chparm(ip)
c     &                   ,"missing_value",xmissing_value)
c              nf_status = nf_get_att_double (ncfile_id,chparm(ip)
c     &                   ,"_FillValue",xfill_value)

               nf_status = nf_inq_varid (ncfile_id,chparm_cps(ip),varid)
               nf_status = nf_get_att_real (ncfile_id,varid
     &                    ,"missing_value",xmissing_value)

              if (verb .ge. 3) then
                write (6,231)
 231            format ('parmread lead time     parm#        parm_id   '
     &                 ,23x,'minval       maxval')

                write (6,233) ifhours(ifh),ifclockmins(ifh),ip
     &                       ,chparm_cps(ip),dmin,dmax
 233            format ('   ',i3,':',i2.2,14x,i3,10x,a30,1x,2g12.4)
                write (6,235) chparm_cps(ip),xmissing_value
 235            format ('   --- ',a30,' missing value = ',g12.4)
              endif

              call conv1d2d_real (imax,jmax,f,cpshgt(1,1,ip)
     &                           ,need_to_flip_lats)
 
            endif
                 
          enddo netcdf_cps_parm_read_loop
      
        endif
      
      endif

      return
      end

c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine get_ncdim1 (ncid,var1_name,nmax)
c
c     ABSTRACT: This routine queries a netcdf file to get the
c     value of a requested file dimension (e.g., imax, jmax)
c
      implicit none

      include "netcdf.inc"

      integer,       intent(in)  :: ncid
      character*(*), intent(in)  :: var1_name
      integer,       intent(out) :: nmax
      integer                    :: status, var1id

      status = nf_inq_dimid (ncid,var1_name,var1id)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      status = nf_inq_dimlen (ncid,var1id,nmax)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      end subroutine get_ncdim1
c
c------------------------------------------------------------------
c       
c------------------------------------------------------------------
      subroutine get_var1_double (ncid,var1_name,nmax,var1)
c
c     ABSTRACT: This routine reads a netcdf file in order to return
c     a 1-dimensional array of data.

      implicit         none

      include "netcdf.inc"

      integer, intent(in):: ncid
      character*(*), intent(in)::  var1_name
      integer, intent(in):: nmax
      real, intent(out):: var1(nmax)

      integer ::  status, var1id

      status = nf_inq_varid (ncid,var1_name,var1id)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)
!     write(*,*) 'Got var1id', var1id

      status = nf_get_var_real (ncid,var1id,var1)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      end subroutine get_var1_double
c
c---------------------------------------------------------
c         
c---------------------------------------------------------
      subroutine get_var3_tlev_double (ncid,var3_name,imax,jmax,ncix
     &                         ,var3,igvret)
c
c     ABSTRACT: This routine reads a netcdf file and returns a 
c     2-dimensional synoptic variable at a particular lead time.
c     The lead time is specified by the ltix array, which is 
c     included in module tracked_parms and defined in subroutine 
c     read_fhours.
c
c     PARAMETERS
c
c     INPUT:
c     ncid   integer that contains the NetCDF file ID
c     var3_name  character name of NetCDF input file
c     imax   integer x-dimension of input data
c     jmax   integer y-dimension of input data
c     ncix   integer index of time level for where this time level 
c            actually is inside the NetCDF data.  Do NOT confuse this 
c            with the index of where this forecast hour is in the 
c            user's list of input forecast hours, as they may be
c            different.  For example, the user may request times that
c            are every 6 hours, but the NetCDF file might have times
c            that are every hour, so the indices for those two arrays
c            will be different.  Be sure to use the one (ncix) that 
c            indicates where the data actually starts in the 
c            NetCDF file.
c
c     OUTPUT:
c     var3   real array with real values returned from NetCDF read
c     igvret integer return code from this routine

      USE tracked_parms; USE verbose_output; USE netcdf_parms

      implicit         none

      include "netcdf.inc"
c
      integer, intent(in)       :: ncid,ncix
      character*(*), intent(in) :: var3_name
      integer, intent(in)       :: imax,jmax
      real, intent(out)         :: var3(imax,jmax)
      integer :: istart(3),ilength(3)
      integer :: status,var3id,igvret

      if (verb .ge. 3) then
        print *,' '
        print *,'In get_var3_tlev_double, ncix=  ',ncix
        print *,' nctotalmins(ncix)= ',nctotalmins(ncix)
      endif

      istart(1) = 1
      istart(2) = 1
      istart(3) = ncix

      ilength(1) = imax
      ilength(2) = jmax
      ilength(3) = 1

      igvret = 0

      status = nf_inq_varid (ncid,var3_name,var3id)

      if (status /= NF_NOERR) then
        print *,' '
        print *,'NOTE: Could not find variable ',var3_name,' at time'
     &         ,' index ncix= ',ncix
     &         ,' nctotalmins(ncix)= ',nctotalmins(ncix)

        igvret = 92
        return
      endif

      status = nf_get_vara_real (ncid,var3id,istart,ilength,var3)
      if (status .ne. NF_NOERR) call handle_netcdf_err(status)

      end subroutine get_var3_tlev_double
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine handle_netcdf_err (status)
c
c     ABSTRACT: This subroutine is an error handling routine for NetCDF-
c     related functions.

      implicit         none

      include          "netcdf.inc"

      integer          status
c
      if (status /= nf_noerr) then
        print *,' '
        print *,'Tracker NetCDF error: '
        print *, nf_strerror(status)
        stop 'Stopped'
      endif

      end subroutine handle_netcdf_err
c
c-------------------------------------------------------------------
c                                                   
c-------------------------------------------------------------------
      subroutine bitmapchk (n,ld,d,dmin,dmax)
c
c     This subroutine checks the bitmap for non-existent data values.
c     Since the data from the regional models have been interpolated
c     from either a polar stereographic or lambert conformal grid
c     onto a lat/lon grid, there will be some gridpoints around the
c     edges of this lat/lon grid that have no data; these grid 
c     points have been bitmapped out by Mark Iredell's interpolater.
c     To provide another means of checking for invalid data points
c     later in the program, set these bitmapped data values to a 
c     value of -999.0.  The min and max of this array are also 
c     returned if a user wants to check for reasonable values.
c
      logical(1) ld
      dimension  ld(n),d(n)
c
      dmin=1.E15
      dmax=-1.E15
c
      do i=1,n
        if (ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        else
          d(i) = -999.0
        endif
      enddo
c
      return
      end
c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine conv1d2d_logic (imax,jmax,lb1d,lb2d,need_to_flip_lats)
c
c     ABSTRACT: This subroutine converts a 1-dimensional input 
c     array of logical data (lb1d) into a 2-dimensional output
c     array (dimension imax,jmax) of logical data (lb2d).
c
c     This subroutine was updated in 6/2000 to add the scanning mode
c     flag (iscanflag) as an input.  This is in order to handle grids
c     that are flipped.  Most grids -- NCEP, UKMET, ECMWF -- have
c     point (1,1) as the uppermost left point on the grid, and the
c     data goes from north to south.  Some grids -- GFDL and the new
c     NAVGEM grid -- are flipped; their point (1,1) is the lowermost
c     left point, and their data goes from south to north.  So if
c     the scanning mode flag indicates northward scanning data
c     (bit 2 in the flag is turned on), we catch it in this
c     subroutine and flip the data ourselves for our own arrays,
c     since this whole program is structured around the data going
c     from north to south.  As of the writing of this, only the
c     first 3 bits of the scanning flag are used, which is why we
c     can use the mod statement in the code below.
c
c     UPDATE 8/2009: I removed the scanning mode flag, since that is
c     GRIB-specific.  The north-south determination is now handled with
c     the logical flag need_to_flip_lats.
c
c     PARAMETERS:
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input box
c     jmax     Number of gridpoints in j direction in input box
c     lb1d     1-d array containing logical bitmap values
c     iscanflag This is kgds(11), an integer value in the GDS,
c              which holds the scanning mode for the data values
c
c     OUTPUT:
c     lb2d     2-d array containing logical bitmap values
c
      logical(1) lb1d(imax*jmax),lb2d(imax,jmax)
      logical(1) :: need_to_flip_lats
      integer :: ilat,ilatix,ilon,imax,jmax
c
      if (need_to_flip_lats) then

        ! Input data is south to north; flip the data while
        ! converting to 2-d grid....

        do ilat=1,jmax
          ilatix = jmax - ilat + 1
          do ilon=1,imax
            lb2d(ilon,ilatix) = lb1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      else

        ! Input data is north to south.  Just convert the
        ! data onto a 2-d grid, do not flip it....

        do ilat=1,jmax
          do ilon=1,imax
            lb2d(ilon,ilat) = lb1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      endif
c
      return
      end

c
c------------------------------------------------------------------
c
c------------------------------------------------------------------
      subroutine conv1d2d_logic_netcdf (imax,jmax,dat1d,lb2d
     &                               ,xmissing_val,need_to_flip_lats)
c
c     ABSTRACT: The purpose of this routine is to create a 2-d logical
c     bitmap to be used for masking out regions with missing data, 
c     such as for a regional grid with irregular boundaries (such as 
c     we've seen for the regional / nested FV3).  This bitmap will 
c     have the same functionality as a GRIB1/GRIB2 bitmap.  The trick
c     is that NetCDF does not have a logical bitmap within its 
c     definition, so we need to make one.  We do this by reading in 
c     the "missing_value" attribute for any variable, then here we 
c     scan through all the data values retrieved from the NetCDF read,
c     and then for all grid points with missing values we set the 
c     valid_pt flag to .false.
c
c     Note the use of the need_to_flip_lats flag.  This is in order to 
c     handle grids that are flipped.  Most grids -- NCEP, UKMET, ECMWF
c     -- have point (1,1) as the uppermost left point on the grid, and
c     the data goes from north to south.  Some grids -- GFDL and the 
c     new NAVGEM grid -- are flipped; their point (1,1) is the lowermost
c     left point, and their data goes from south to north.  So if
c     the need_to_flip_lats flag was set to TRUE in getgridinfo, meaning
c     that we have northward scanning data, we catch it in this
c     subroutine and flip the data ourselves for our own arrays,
c     since this whole program is structured around the data going
c     from north to south.
c
c     PARAMETERS:
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input box
c     jmax     Number of gridpoints in j direction in input box
c     dat1d    1-d array containing floating point data values
c     xmissing_val real value of missing value for the given variable
c                  that was read in for the calling routine
c     need_to_flip_lats  logical flag, set in getgridinfo, that
c              indicates if data is correctly N-to-S, or if it is
c              S-to-N and needs to be flipped.
c
c     OUTPUT:
c     lb2d     2-d array containing logical bitmap values
c
      USE verbose_output

      implicit none

      logical(1) lb2d(imax,jmax)
      logical(1) need_to_flip_lats
      integer    ilat,ilatix,ilon,imax,jmax,tct,fct,mct
      real       dat1d(imax*jmax)
      real       xmissing_val
c
      tct = 0
      fct = 0
      mct = 0

      if (verb >= 3) then
        print *,' '
        print *,'TOP of conv1d2d_logic_netcdf, xmissing_val= '
     &         ,xmissing_val
        print *,' '
      endif
c
      if (need_to_flip_lats) then

        ! Input data is south to north; flip the data while
        ! converting to 2-d grid....

        do ilat=1,jmax
          ilatix = jmax - ilat + 1
          do ilon=1,imax
            if (dat1d(ilon+(ilat-1)*imax) == xmissing_val) then
              lb2d(ilon,ilatix) = .false.
c              print *,'LBSF FLIP: ilon= ',ilon,' ilatix= ',ilatix
c              fct = fct + 1
            else
              lb2d(ilon,ilatix) = .true.
c              tct = tct + 1
            endif
          enddo
        enddo

      else

        ! Input data is north to south.  Just convert the
        ! data onto a 2-d grid, do not flip it....

        do ilat=1,jmax
          do ilon=1,imax
            if (dat1d(ilon+(ilat-1)*imax) == xmissing_val) then
c              print *,'LBSF no-flip: ilon= ',ilon,' ilat= ',ilat
              lb2d(ilon,ilat) = .false.
c              fct = fct + 1
            else
              lb2d(ilon,ilat) = .true.
c              tct = tct + 1
            endif
          enddo
        enddo

      endif

c      print *,' '
c      print *,' LB STATS: tct= ',tct,' fct= ',fct,' mct= ',mct
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine conv1d2d_real (imax,jmax,dat1d,dat2d,need_to_flip_lats)
c
c     ABSTRACT: This subroutine converts a 1-dimensional input 
c     array of real data (dat1d) into a 2-dimensional output
c     array (dimension imax,jmax) of real data (dat2d).
c
c     This subroutine was updated in 6/2000 to add the scanning mode
c     flag (iscanflag) as an input.  This is in order to handle grids
c     that are flipped.  Most grids -- NCEP, UKMET, ECMWF -- have
c     point (1,1) as the uppermost left point on the grid, and the
c     data goes from north to south.  Some grids -- GFDL and the new
c     NAVGEM grid -- are flipped; their point (1,1) is the lowermost
c     left point, and their data goes from south to north.  So if
c     the scanning mode flag indicates northward scanning data
c     (bit 2 in the flag is turned on), we catch it in this
c     subroutine and flip the data ourselves for our own arrays,
c     since this whole program is structured around the data going
c     from north to south.  As of the writing of this, only the
c     first 3 bits of the scanning flag are used, which is why we
c     can use the mod statement in the code below.
c
c     UPDATE 8/2009: I removed the scanning mode flag, since that is
c     GRIB-specific.  The north-south determination is now handled with
c     the logical flag need_to_flip_lats.
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input box
c     jmax     Number of gridpoints in j direction in input box
c     dat1d    1-d real array of data
c     need_to_flip_lats  logical flag, set in getgridinfo, that
c              indicates if data is correctly N-to-S, or if it is
c              S-to-N and needs to be flipped.
c
c     OUTPUT:
c     dat2d    2-d real array of data
c
      logical(1) :: need_to_flip_lats
      real    dat1d(imax*jmax),dat2d(imax,jmax)
c
      if (need_to_flip_lats) then

        ! Input data is south to north; flip the data while
        ! converting to 2-d grid....

        do ilat=1,jmax
          ilatix = jmax - ilat + 1
          do ilon=1,imax
            dat2d(ilon,ilatix) = dat1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      else

        ! Input data is north to south.  Just convert the
        ! data onto a 2-d grid, do not flip it....

        do ilat=1,jmax
          do ilon=1,imax
            dat2d(ilon,ilat) = dat1d(ilon+(ilat-1)*imax)
          enddo
        enddo

      endif
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_nlists (inp,trkrinfo,netcdfinfo)
c
c     ABSTRACT: This subroutine simply reads in the namelists that are
c     created in the shell script.  Namelist datein contains the 
c     starting date information, plus the model identifier.  Namelist
c     stswitch contains the flags for processing for each storm.
c
      USE inparms; USE set_max_parms; USE atcf; USE trkrparms; USE phase
      USE structure; USE gfilename_info
      USE verbose_output; USE waitfor_parms; USE netcdf_parms
      USE tracking_parm_prefs

      implicit none

      integer ifh
      type (datecard) inp
      type (trackstuff) trkrinfo
      type (netcdfstuff) netcdfinfo
c
      namelist/datein/inp
      namelist/atcfinfo/atcfnum,atcfname,atcfymdh,atcffreq
      namelist/trackerinfo/trkrinfo
      namelist/phaseinfo/phaseflag,phasescheme,wcore_depth
      namelist/structinfo/structflag,ikeflag
      namelist/fnameinfo/gmodname,rundescr,atcfdescr
      namelist/verbose/verb,verb_g2
      namelist/waitinfo/use_waitfor,wait_min_age,wait_min_size
     &                 ,wait_max_wait,wait_sleeptime
     &                 ,use_per_fcst_command,per_fcst_command
      namelist/netcdflist/netcdfinfo
      namelist/parmpreflist/user_wants_to_track_zeta850
     &       ,user_wants_to_track_zeta700,user_wants_to_track_wcirc850
     &       ,user_wants_to_track_wcirc700,user_wants_to_track_gph850
     &       ,user_wants_to_track_gph700,user_wants_to_track_mslp
     &       ,user_wants_to_track_wcircsfc,user_wants_to_track_zetasfc
     &       ,user_wants_to_track_thick500850
     &       ,user_wants_to_track_thick200500
     &       ,user_wants_to_track_thick200850

c     Set namelist default values:
      use_per_fcst_command='t'
      per_fcst_command=' '
      atcffreq=600
      trkrinfo%enable_timing=1
      trkrinfo%want_oci=.false.
      trkrinfo%gribver=1   ! Set to GRIB1 as default, can be set to
                           ! something else in the namelist input.

      read (5,NML=datein,END=801)
  801 continue
      read (5,NML=atcfinfo,END=807)
  807 continue
      print *,'just before trackerinfo read namelist'
      read (5,NML=trackerinfo,END=809)
  809 continue
      print *,'just after trackerinfo read namelist'
      read (5,NML=phaseinfo,END=811)
  811 continue
      read (5,NML=structinfo,END=815)
  815 continue
      read (5,NML=fnameinfo,END=817)
  817 continue
      read (5,NML=waitinfo,END=821)
  821 continue
      read (5,NML=netcdflist,END=823)
  823 continue
      read (5,NML=parmpreflist,END=825)
  825 continue
      read (5,NML=verbose,END=819,ERR=833)
  819 continue
      goto 837
  833 continue
      verb = 1
  837 continue

      print *,'in read_nlists, verb= ',verb

      if ( verb .ge. 0 ) then
        print *,' '
        print *,'After datein namelist in trak.f, namelist '
     &         ,'parms follow:'
        print *,'Forecast initial year  = byy = ',inp%byy
        print *,'Forecast initial month = bmm = ',inp%bmm
        print *,'Forecast initial day   = bdd = ',inp%bdd
        print *,'Forecast initial hour  = bhh = ',inp%bhh
        print *,'Forecast model identifier = model= ',inp%model
        print *,'Forecast model type = modtyp= ',inp%modtyp
        print *,'Forecast model data lead time units= lt_units= '
     &       ,inp%lt_units
        print *,'Forecast model data sequencing setup= file_seq= '
     &       ,inp%file_seq
        print *,'Forecast model nest type = ',inp%nesttyp
c         
        print *,' '
        print *,'Values read in from atcfinfo namelist: '
        write (6,89) atcfnum,atcfname
        write (6,90) atcfymdh
        write (6,92) atcffreq
 89     format ('ATCF ID = ',i2,'  ATCF Name = ',a4)
 90     format ('ATCF date (initial date on output atcf records) = '
     &         ,i10)
 92     format ('ATCF output frequency (in hours*100) = atcffreq = ',i6)
c
        print *,' '
        print *,'Values read in from trackerinfo namelist follow: '
        write (6,101) ' western boundary  = westbd  = ',trkrinfo%westbd
        write (6,101) ' eastern boundary  = eastbd  = ',trkrinfo%eastbd
        write (6,101) ' northern boundary = northbd = ',trkrinfo%northbd
        write (6,101) ' southern boundary = southbd = ',trkrinfo%southbd
        write (6,102) ' tracker type = ',trkrinfo%type
        write (6,103) ' mslp threshold = mslpthresh = '
     &       ,trkrinfo%mslpthresh
        write (6,120) ' Flag for using backup mslp gradient check= '
     &       ,'use_backup_mslp_grad_check = '
     &       ,trkrinfo%use_backup_mslp_grad_check
        write (6,103) ' v850 threshold = v850thresh = '
     &       ,trkrinfo%v850thresh
        write (6,122) ' Flag for using backup 850 mb Vt check= '
     &       ,'use_backup_850_vt_check = '
     &       ,trkrinfo%use_backup_850_vt_check
        write (6,123) ' Max allowable distance between the '
     &            ,'tracker-found fixes for mslp and 850 zeta = '
     &            ,trkrinfo%max_mslp_850
        write (6,104) ' model grid type = ',trkrinfo%gridtype
        write (6,101) ' Contour interval to be used = ',trkrinfo%contint
        write (6,106) ' Flag for whether or not roci will be computed'
     &       ,' and written out for tracker-type case = '
     &       ,trkrinfo%want_oci
        write (6,105) ' Flag for whether or not vitals will be written '
     &       ,'out = ',trkrinfo%out_vit
        write (6,109) ' Flag for whether or not a land mask will be '
     &       ,'used for tcgen candidate low filtering = '
     &       ,trkrinfo%use_land_mask
        write (6,110) ' Flag for input data type (grib or netcdf) = '
     &                ,trkrinfo%inp_data_type
        write (6,107) ' Flag for which GRIB version (1 or 2) the input'
     &       ,' data will be in = ',trkrinfo%gribver
        write (6,108) ' Flag for input GRIB2 JPDTN (0 or 1) = '
     &                ,trkrinfo%g2_jpdtn
        write (6,112) ' Flag for input GRIB2 MSLP ID (1 or 192) = '
     &                ,trkrinfo%g2_mslp_parm_id
        write (6,114) ' Flag for input GRIB1 MSLP ID (102 or 130) = '
     &                ,trkrinfo%g1_mslp_parm_id
        write (6,116) ' Flag for input GRIB1 sfcwind level type '
     &                ,'(PDS Octet 10... should be 1 or 105) = '
     &                ,trkrinfo%g1_sfcwind_lev_typ
        write (6,118) ' Flag for input GRIB1 sfcwind level value '
     &                ,'(PDS Octets 11 & 12... usually 0 or 10) = '
     &                ,trkrinfo%g1_sfcwind_lev_val

 101    format (a31,f7.2)
 102    format (a16,a7)
 103    format (a31,f7.4)
 104    format (a19,a8)
 106    format (a46,a41,L1)
 105    format (a48,a6,a1)
 109    format (a45,a41,a1)
 110    format (a45,a6)
 107    format (a47,a19,i1)
 108    format (a39,i2)
 112    format (a43,i4)
 114    format (a45,i4)
 116    format (a41,a39,i4)
 118    format (a42,a43,i4)
 120    format (a44,a29,a1)
 122    format (a40,a26,a1)
 123    format (a36,a44,f7.1)

        print *,' '
        print *,' '
        print *,'Values read in from netcdflist namelist: '
        print *,' '
        write (6,300) netcdfinfo%num_netcdf_vars  ! Total *possible* 
                                  ! number of input NetCDF variables,
                                  ! including those that are included
                                  ! in the input file and those that
                                  ! are not.
        write (6,370) netcdfinfo%netcdf_filename ! full path filename
        write (6,301) 
        write (6,302) netcdfinfo%rv850name  ! 850 mb rel vort
        write (6,304) netcdfinfo%rv700name  ! 700 mb rel vort
        write (6,306) netcdfinfo%u850name   ! 850 mb u-comp
        write (6,308) netcdfinfo%v850name   ! 850 mb v-comp
        write (6,310) netcdfinfo%u700name   ! 700 mb u-comp
        write (6,312) netcdfinfo%v700name   ! 700 mb v-comp
        write (6,314) netcdfinfo%z850name   ! 850 mb gp height
        write (6,316) netcdfinfo%z700name   ! 700 mb gp height
        write (6,318) netcdfinfo%mslpname   ! mslp
        write (6,320) netcdfinfo%usfcname   ! near-sfc u-comp
        write (6,322) netcdfinfo%vsfcname   ! near-sfc v-comp
        write (6,324) netcdfinfo%u500name   ! 500 mb u-comp
        write (6,326) netcdfinfo%v500name   ! 500 mb v-comp
        write (6,328) netcdfinfo%tmean_300_500_name !Mean T @ 300-500 mb
        write (6,330) netcdfinfo%z500name   ! 500 mb gp height
        write (6,332) netcdfinfo%z200name   ! 200 mb gp height
        write (6,334) netcdfinfo%lmaskname  ! Land mask
        write (6,336) netcdfinfo%z900name   ! 900 mb gp height
        write (6,338) netcdfinfo%z800name   ! 800 mb gp height
        write (6,340) netcdfinfo%z750name   ! 750 mb gp height
        write (6,342) netcdfinfo%z650name   ! 650 mb gp height
        write (6,344) netcdfinfo%z600name   ! 600 mb gp height
        write (6,346) netcdfinfo%z550name   ! 550 mb gp height
        write (6,348) netcdfinfo%z450name   ! 450 mb gp height
        write (6,350) netcdfinfo%z400name   ! 400 mb gp height
        write (6,352) netcdfinfo%z350name   ! 350 mb gp height
        write (6,354) netcdfinfo%z300name   ! 300 mb gp height
        write (6,355) netcdfinfo%time_name  ! Name of time variable
                                            ! (usually it is "time")
        write (6,356) netcdfinfo%lon_name   ! longitudes
        write (6,358) netcdfinfo%lat_name   ! latitudes
        write (6,359) netcdfinfo%time_units ! This will be either "days"
                                      ! or "hours".  If it's "hours", 
                                      ! then all the time data values 
                                      ! are for hours since the initial 
                                      ! time.  Same thing for "days",
                                      ! however if it is "days", then
                                      ! know that a value of 0.25 will
                                      ! be the same as a 6-hour lead 
                                      ! time.

 300    format ('Total *possible* number of input NetCDF variables,'
     &         ,/,'     including those that are included in the input'
     &         ,/,'     NetCDF file and those that are not = ',i4)
 370    format ('Input NetCDF filename = ',a180)
 301    format (' ',/
     &         ,'List of NetCDF variables follows.  A value of X ',/
     &         ,'indicates the variable is not included in the ',/
     &         ,'input file and no attempt will be made to read in ',/
     &         ,'that variable: ',/,' ')
 302    format ('NetCDF variable name for 850 mb vort =       ',a30)
 304    format ('NetCDF variable name for 700 mb vort =       ',a30)
 306    format ('NetCDF variable name for 850 mb u-comp =     ',a30)
 308    format ('NetCDF variable name for 850 mb v-comp =     ',a30)
 310    format ('NetCDF variable name for 700 mb u-comp =     ',a30)
 312    format ('NetCDF variable name for 700 mb v-comp =     ',a30)
 314    format ('NetCDF variable name for 850 mb gp height =  ',a30)
 316    format ('NetCDF variable name for 700 mb gp height =  ',a30)
 318    format ('NetCDF variable name for MSLP =              ',a30)
 320    format ('NetCDF variable name for near-sfc u-comp =   ',a30)
 322    format ('NetCDF variable name for near-sfc v-comp =   ',a30)
 324    format ('NetCDF variable name for 500 mb u-comp =     ',a30)
 326    format ('NetCDF variable name for 500 mb v-comp =     ',a30)
 328    format ('NetCDF variable name for 300-500 mb Mean T = ',a30)
 330    format ('NetCDF variable name for 500 mb gp height =  ',a30)
 332    format ('NetCDF variable name for 200 mb gp height =  ',a30)
 334    format ('NetCDF variable name for land-sea mask =     ',a30)
 336    format ('NetCDF variable name for 900 mb gp height =  ',a30)
 338    format ('NetCDF variable name for 800 mb gp height =  ',a30)
 340    format ('NetCDF variable name for 750 mb gp height =  ',a30)
 342    format ('NetCDF variable name for 650 mb gp height =  ',a30)
 344    format ('NetCDF variable name for 600 mb gp height =  ',a30)
 346    format ('NetCDF variable name for 550 mb gp height =  ',a30)
 348    format ('NetCDF variable name for 450 mb gp height =  ',a30)
 350    format ('NetCDF variable name for 400 mb gp height =  ',a30)
 352    format ('NetCDF variable name for 350 mb gp height =  ',a30)
 354    format ('NetCDF variable name for 300 mb gp height =  ',a30)
 355    format ('NetCDF variable name for time =              ',a30)
 356    format ('NetCDF variable name for longitudes =        ',a30)
 358    format ('NetCDF variable name for latitudes =         ',a30)
 359    format ('NetCDF time value (hours|days) =             ',a30)

        print *,' '
        print *,' '
        print *,'Values read in from parmpreflist namelist: '
        print *,' '
        write (6,402) user_wants_to_track_zeta850
        write (6,404) user_wants_to_track_zeta700
        write (6,406) user_wants_to_track_wcirc850
        write (6,408) user_wants_to_track_wcirc700
        write (6,410) user_wants_to_track_gph850
        write (6,412) user_wants_to_track_gph700
        write (6,414) user_wants_to_track_mslp
        write (6,416) user_wants_to_track_wcircsfc
        write (6,418) user_wants_to_track_zetasfc
        write (6,420) user_wants_to_track_thick500850
        write (6,422) user_wants_to_track_thick200500
        write (6,424) user_wants_to_track_thick200850

 402    format ('user_wants_to_track_zeta850=     ',a2)
 404    format ('user_wants_to_track_zeta700=     ',a2)
 406    format ('user_wants_to_track_wcirc850=    ',a2)
 408    format ('user_wants_to_track_wcirc700=    ',a2)
 410    format ('user_wants_to_track_gph850=      ',a2)
 412    format ('user_wants_to_track_gph700=      ',a2)
 414    format ('user_wants_to_track_mslp=        ',a2)
 416    format ('user_wants_to_track_wcircsfc=    ',a2)
 418    format ('user_wants_to_track_zetasfc=     ',a2)
 420    format ('user_wants_to_track_thick500850= ',a2)
 422    format ('user_wants_to_track_thick200500= ',a2)
 424    format ('user_wants_to_track_thick200850= ',a2)
        
        print *,' '
        print *,'Values read in from phaseinfo namelist: '
        write (6,211) phaseflag,phasescheme
        write (6,212) wcore_depth
 211    format ('Storm phase flag = ',a1,'  Phase scheme = ',a4)
 212    format ('Storm phase, warm core depth (wcore_depth) = ',f7.2)
        
        print *,' '
        print *,'Values read in from structinfo namelist: '
        write (6,93) structflag
        write (6,95) ikeflag
 93     format ('Structure flag = ',a1)
 95     format ('IKE flag = ',a1)
        
        print *,' '
        print *,'Values read in for grib file name from fnameinfo'
     &       ,' namelist: '
        write (6,131) gmodname
        write (6,133) rundescr
        write (6,135) atcfdescr
 131    format ('Model name description = gmodname = ',a4)
 133    format ('Forecast run description = rundescr = ',a40)
 135    format ('Optional ATCF / Storm name description = atcfdescr = '
     &       ,a40)

        print *,' '
        print *,'Value read in for verbose output for most output:'
        write (6,141) verb
 141    format ('Value read in for verbose flag = verb = ',i2)

        print *,' '
        print *,'Value read in for verbose output for grib2 output:'
        write (6,142) verb_g2
 142    format ('Value read in for GRIB2 verbose flag = verb_g2 = ',i2)

        print *,' '
        print *,'Values read in from waitinfo namelist:'
        write (6,151) use_waitfor
        write (6,152) wait_min_age
        write (6,153) wait_min_size
        write (6,154) wait_max_wait
        write (6,155) wait_sleeptime
        if(len_trim(per_fcst_command)>0) then
           write (6,156) trim(per_fcst_command)
        else
c          No command specified, so disable the feature
           use_per_fcst_command='n'
        endif
 151    format ('Flag for input file waiting = use_waitfor = ',a1)
 152    format ('min age (time in seconds since last mod) = '
     &         ,'wait_min_age = ',i8)
 153    format ('min file size in bytes = wait_min_size = ',i12)
 154    format ('max number of seconds to wait for each file = '
     &         ,'wait_max_wait = ',i6)
 155    format ('number of seconds to sleep between checks = '
     &         ,'wait_sleeptime = ',i6)
 156    format ('command to run after every forecast time = "',A,'"')
c
        if (use_waitfor == 'y') then
          if (inp%file_seq == 'multi') then
            continue
          else
            print *,' '
            print *,'!!! ERROR: The use_waitfor flag is set to "y".'
            print *,'    This requires that the inp%file_seq flag be'
            print *,'    set to "multi", but you have specified '
            print *,'    something else.  '
            print *,'    inp%file_seq = ',inp%file_seq
            print *,'    STOPPING....'
            print *,' '
            STOP 95
          endif
        endif
c
      endif
      return
      end
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_fhours (ifhmax)
c
c     ABSTRACT: This subroutine reads in a text file that contains the
c     forecast times that will be read in.  The format of the file is
c     in "MMMMM", i.e., minutes, for example, for a forecast going out
c     to 120h, the file would look like this:
c
c                            For reference, here
c                            are the times that
c                            match up with the
c                            minutes on the left:
c
c      1        0                    0:00
c      2      240                    4:00
c      3      270                    4:30
c      4      300                    5:00
c      5      330                    5:30
c      6      360                    6:00
c      7      600                   10:00
c      8      630                   10:30
c      9      660                   11:00
c     10      690                   11:30
c     11      720                   12:00
c     12      960                   16:00
c     13      990                   16:30
c      .       .                      .
c      .       .                      .
c      .       .                      .
c     87     7200                  120:00
c
c     Note that we are now allowing for sub-hourly time intervals.
c
      USE tracked_parms
      USE verbose_output

      implicit none
c
      integer, parameter :: iunit_fh=15
      integer itmphrs(750),itmpmins(750),input_mins(750),itmpltix(750)
      integer ifhmax,inphr,inpmin,ict,i,ifa,ifma,icma,ira,inpltix,ila
      real    xminfract

      itmphrs  = -99
      itmpmins = -99

      if (allocated(ifhours)) deallocate (ifhours)
      if (allocated(iftotalmins))  deallocate (iftotalmins)
      if (allocated(ifclockmins))  deallocate (ifclockmins)
      if (allocated(fhreal))  deallocate (fhreal)
      if (allocated(ltix))  deallocate (ltix)

      ict = 0
      do while (.true.)

        if ( verb .ge. 3 ) then
          print *,'Top of while loop in read_fhours'
        endif

        read (iunit_fh,85,end=130) inpltix,inpmin
        write (6,85) inpltix,inpmin

        if (inpmin >= 0 .and. inpmin < 150000) then
          ict = ict + 1
          itmpltix(ict) = inpltix
          itmphrs(ict)  = inpmin / 60
          itmpmins(ict) = mod(inpmin,60)
          input_mins(ict) = inpmin
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: Input minutes not between 0 and 150000'
            print *,'!!!        inpmin= ',inpmin
            print *,'!!! STOPPING EXECUTION'
          endif

          STOP 91
        endif

        if ( verb .ge. 3 ) then
          print *,'readloop, ict= ',ict,' inpmin= ',inpmin
        endif

      enddo

  130 continue

      ifhmax = ict

 85   format (i4,1x,i5)

      if ( verb .ge. 3 ) then
        print *,' '
      endif

      allocate (ifhours(ifhmax),stat=ifa)
      allocate (iftotalmins(ifhmax),stat=ifma)
      allocate (ifclockmins(ifhmax),stat=icma)
      allocate (fhreal(ifhmax),stat=ira)
      allocate (ltix(ifhmax),stat=ila)
      if (ifa /= 0 .or. ifma /= 0 .or. icma /= 0 .or. ira /= 0 .or.
     &    ila /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in read_fhours allocating either ifhours,'
          print *,'!!! iftotalmins, ifclockmins or fhreal.'
          print *,'!!! ifa = ',ifa,' ifma= ',ifma,' ira= ',ira
          print *,'!!! icma= ',icma,' ila= ',ila
          print *,'!!! STOPPING EXECUTION'
        endif

        STOP 91
      endif

      do i = 1,ifhmax

        ltix(i) = itmpltix(i)
        xminfract = float(itmpmins(i)) / 60.
        fhreal(i) = float(itmphrs(i)) + xminfract
        ifhours(i) = itmphrs(i)
        ifclockmins(i)  = itmpmins(i)
        iftotalmins(i)  = input_mins(i)

        if (i > 1) then
          if (fhreal(i) > fhreal(i-1)) then
            continue
          else

            if ( verb .ge. 3 ) then
              print *,' '
              print *,'!!! ERROR: In read_fhours, the time read in '
              print *,'!!! is not greater than the previous time.'
              print *,'!!! i= ',i
              print *,'!!! fhreal(i)=   ',fhreal(i)
              print *,'!!! fhreal(i-1)= ',fhreal(i-1)
              print *,'!!! STOPPING EXECUTION'
            endif

            STOP 91
          endif
        endif

        if ( verb .ge. 3 ) then
          write (6,87) i,ltix(i),iftotalmins(i),fhreal(i),ifhours(i)
     &         ,ifclockmins(i)
        endif

      enddo

   87 format (1x,'i= ',i3,'  input lead time index= ',i4,' minutes= '
     &       ,i5,' real_lead_time= ',f6.2,' clock_lead_time= ',i3,':'
     &       ,i2)
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_tcv_card (lucard,maxstorm,trkrinfo,numtcv,iret)
c
c     ABSTRACT: This subroutine reads in the updated TC Vitals file
c               for the current time and prints out those cards (storms)
c               that have been selected to be processed.  It also 
c               takes the initial positions from the tcv card for each
c               storm and puts them into the slonfg & slatfg arrays.
c               Note that this routine is reading in vitals in the 
c               standard format for TCs only.  Any genesis vitals are
c               read in in subroutine  read_gen_vitals.
c
c     INPUT:    
c     lucard    integer unit number for tcvitals card
c     trkrinfo  derived type that contains info on the type of 
c               tracker run that we are performing.
c
c     OUTPUT:   
c     maxstorm  max # of storms to be handled for this case
c     numtcv    number of storms read off of the input tcvitals file
c     iret      return code from this subroutine
c
c     OTHER:
c     stormswitch 1,2 or 3 (see more description under Main pgm section)
c     slonfg     first guess array for longitude
c     slatfg     first guess array for latitude
c     storm      contains the tcvitals info
c     (storm, stormswitch, slonfg and slatfg are allocatable and are 
c      defined in module def_vitals)

      USE def_vitals; USE set_max_parms; USE trkrparms
      USE verbose_output

      implicit none

      logical(1) :: vit_file_exists
      type (tcvcard) tmpstorm(maxstorm_tc)
      type (trackstuff) trkrinfo
      integer    isa,issa,ioa,iaa,ita,iret,ict,maxstorm
      integer    i,ii,lucard,numtcv
c------

      ! Check to see if the TC Vitals file exists.  If so, then open it
      ! using the unit specified in lucard.

      inquire (file="tcvit_rsmc_storms.txt",exist=vit_file_exists)

      if (vit_file_exists) then

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ TC Vitals file for existing, RSMC-numbered'
     &           ,'    storms exists and will be opened with '
     &           ,'    unit= lucard= ',lucard 
        endif

        open (unit=lucard,file="tcvit_rsmc_storms.txt",status='old'
     &       ,err=887)

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ TC vitals file tcvit_rsmc_storms.txt has '
          print *,'    been opened with unit= lucard= ',lucard
        endif

      else

        if (trkrinfo%type == 'tracker') then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in read_tcv_card.  The fortran inquire'
            print *,'!!! statement indicates that the tcvitals file for'
            print *,'!!! already-existing, RSMC-numbered storms does '
            print *,'!!! NOT exist.  This TC Vitals file is needed for'
            print *,'!!! a tracker case.  Check to see that the '
            print *,'!!! TC Vitals file exists in this directory and'
            print *,'!!! is named tcvit_rsmc_storms.txt'
            print *,'!!! STOPPING....'
            print *,'!!! '
            iret=99
            return
          endif

        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! NOTE:  In read_tcv_card, the fortran inquire'
            print *,'!!! statement indicates that the tcvitals file for'
            print *,'!!! already-existing, RSMC-numbered storms does '
            print *,'!!! NOT exist.  While this TC Vitals file is '
            print *,'!!! needed for tracker cases, you are running'
            print *,'!!! either a midlat or tcgen case here, and so '
            print *,'!!! that file is not needed... although you can '
            print *,'!!! run with using tc vitals for those genesis'
            print *,'!!! cases if you want to.  You may want to check'
            print *,'!!! and make sure this is what you intend.  If '
            print *,'!!! you do want to use it, the TC Vitals file '
            print *,'!!! should be in this directory and it should be'
            print *,'!!! named tcvit_rsmc_storms.txt'
            print *,'!!! '
          endif

        endif

      endif

      ii=1

      if (vit_file_exists) then
        do while (.true. .and. ii <= maxstorm_tc)
          read (lucard,21,END=801,ERR=891) tmpstorm(ii)
          ii = ii + 1
        enddo
   21   format (a4,1x,a3,1x,a9,1x,i8,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x
     &         ,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)
  801   continue
      endif

      numtcv = ii - 1

      if (trkrinfo%type == 'midlat' .or. trkrinfo%type == 'tcgen') then
        ! For the mid-latitude or tc genesis cases, the max number
        ! of storms (maxstorm) allowed to be tracked throughout a
        ! forecast is defined in module set_max_parms.

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'In read_tcv_card, tracker type of "midlat" or '
          print *,'"tcgen" indicates that this run of the  tracker is'
          print *,'for a midlat or a tcgen case....'
        endif

        maxstorm = maxstorm_mg
        allocate (stormswitch(maxstorm),stat=isa)
        allocate (storm(maxstorm),stat=issa)
        allocate (slonfg(maxstorm,maxtime),stat=ioa)
        allocate (slatfg(maxstorm,maxtime),stat=iaa)
        allocate (stcvtype(maxstorm),stat=ita)
        if (isa /= 0 .or. ioa /= 0 .or. iaa /= 0 .or. issa /= 0 .or.
     &      ita /= 0) then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in read_tcv_card allocating stormswitch,'
            print *,'!!! slonfg, storm, slatfg or stcvtype arrays.  '
            print *,'!!! isa = ',isa,' ioa= ',ioa,' iaa= ',iaa,' issa= '
            print *,'!!! ',issa,' ita= ',ita
          endif

          iret = 97
          return
        endif
        slonfg = 0.0; slatfg = 0.0
        stcvtype = 'FOF' ! Found On the Fly by tracker (not on tcvitals)
        stormswitch = 3    ! Initialize whole array to case of '3'
        if (numtcv > 0) then
          if ( verb .ge. 3 ) then
            print *,' '
            print *,'Following are the already-existing storms that'
            print *,'were read in from the tc vitals file: '
            print *,' '       
          endif

          ict = 0
          do i=1,numtcv
            stormswitch(i) = 1
            storm(i) = tmpstorm(i)
            ict = ict + 1

            if ( verb .ge. 3 ) then
              write (*,31) storm(i)
            endif
          
            if (storm(i)%tcv_lonew == 'W') then
              slonfg(i,1) =  360. - float(storm(i)%tcv_lon)/10.0
            else
              slonfg(i,1) = float(storm(i)%tcv_lon)/10.0
            endif
            if (storm(i)%tcv_latns == 'S') then
              slatfg(i,1) = -1. * float(storm(i)%tcv_lat)/10.0
            else
              slatfg(i,1) = float(storm(i)%tcv_lat)/10.0
            endif
            stcvtype(i) = 'TCV' ! Storm listed on tcvitals

c            if (trkrinfo%type == 'midlat') then
c              storm(i)%tcv_center = 'MIDL'
c            else if (trkrinfo%type == 'tcgen') then
c              storm(i)%tcv_center = 'TCG '
c            endif
c            write (storm(i)%tcv_storm_id,'(i4.4)') i
c            write (storm(i)%tcv_storm_name,'(i4.4)') i

          enddo
        endif
        iret=0
        return
      else
        ! For the  tracker cases, the max number of storms (maxstorm) 
        ! allowed to be tracked throughout a forecast is defined by 
        ! the number of vitals read in above.

        maxstorm = numtcv

        if (maxstorm > 0) then
          continue
        else
          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in read_tcv_card, num storms to be '
            print *,'!!! processed is not greater than 0 for a tracker'
            print *,'!!! case.  Check to see that the TC Vitals file'
            print *,'!!! has been created and exists in the working'
            print *,'!!! directory.  That TC vitals file should be'
            print *,'!!! named tcvit_rsmc_storms.txt'
            print *,'!!! STOPPING...'
            print *,'!!! '
            iret=99
            return
          endif
        endif

        allocate (stormswitch(maxstorm),stat=isa)
        allocate (storm(maxstorm),stat=issa)
        allocate (slonfg(maxstorm,maxtime),stat=ioa)
        allocate (slatfg(maxstorm,maxtime),stat=iaa)
        allocate (stcvtype(maxstorm),stat=ita)
        if (isa /= 0 .or. ioa /= 0 .or. iaa /= 0 .or. issa /= 0 .or.
     &      ita /= 0) then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in read_tcv_card allocating stormswitch,'
            print *,'!!! slonfg, storm, slatfg or stcvtype arrays.  '
            print *,'!!! isa = ',isa,' ioa= ',ioa,' iaa= ',iaa,' issa= '
            print *,'!!! ',issa,' ita= ',ita
          endif

          iret = 97
          return   
        endif

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'Following are the storms to be processed: '
          print *,' '
        endif

        slonfg = 0.0; slatfg = 0.0
        stcvtype = '   '  ! Not needed for regular tracker run....
        ict=0
        do i=1,maxstorm
          stormswitch(i) = 1
          storm(i) = tmpstorm(i)
          ict = ict + 1

          if ( verb .ge. 3 ) then
            write (*,31) storm(i)
          endif

          if (storm(i)%tcv_lonew == 'W') then
            slonfg(i,1) =  360. - float(storm(i)%tcv_lon)/10.0
          else
            slonfg(i,1) = float(storm(i)%tcv_lon)/10.0
          endif
          if (storm(i)%tcv_latns == 'S') then
            slatfg(i,1) = -1. * float(storm(i)%tcv_lat)/10.0
          else
            slatfg(i,1) = float(storm(i)%tcv_lat)/10.0
          endif
        enddo
 
        if (ict.gt.0) then
          iret = 0
          return
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in read_tcv_card, num storms to be '
            print *,'!!! processed is not greater than 0 for a tracker'
            print *,'!!! case.  Check to see that the TC Vitals file'
            print *,'!!! has been created and exists in the working'
            print *,'!!! directory.  That TC vitals file should be'
            print *,'!!! named tcvit_rsmc_storms.txt'
            print *,'!!! Stopping....'
            print *,' '
          endif

          iret = 99
          return

        endif

      endif

   31 format (a4,1x,a3,1x,a9,1x,i8.8,1x,i4.4,1x,i3,a1,1x,i4,a1,1x
     &       ,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

 887  continue

      if ( verb .ge. 1 ) then
        print *,'!!! ERROR in read_tcv_card opening rsmc TC vitals'
        print *,'!!! file named tcvit_rsmc_storms.txt.  A file with'
        print *,'!!! that name needs to be in your working directory.'
        print *,'!!! It should contain the input TC vitals for '
        print *,'!!! already-existing storms that have rsmc-issued'
        print *,'!!! storm IDs.'
      endif

      iret = 97
      return

 891  continue

      if ( verb .ge. 1 ) then
        print *,'!!! ERROR in read_tcv_card reading unit ',lucard
      endif

      iret = 98
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine read_gen_vitals (lgvcard,maxstorm,trkrinfo,numtcv,iret)
c
c     ABSTRACT: This subroutine reads in a modified TC Vitals file
c     for the current time and prints out those cards (storms) that
c     have been selected to be processed.  It also takes the initial 
c     positions from the tcv card for each storm and puts them into 
c     the slonfg & slatfg arrays.
c
c     The reason that these are referred to as modified tcvitals is
c     that the format is different from standard TC vitals format.
c     These vitals are created by a previous run of this tracker 
c     executable, and the storm identifier is different than that 
c     for a standard tcvitals.  The storm
c     identifier contains the date/time that the storm was first 
c     identified, and the lat/lon position at which it was first
c     identified.
c
c     EXAMPLE:  The following is a standard TC Vitals record, split
c               up over 3 lines:
c
c       NHC  01L ALBERTO   20060614 1200 343N 0807W 035 093 1004 1012 
c            0278 15 222 -999 -999 -999 -999 M -999 -999 -999 -999 72 
c            520N  410W  -999 -999 -999 -999
c
c     EXAMPLE:  The following is the format for the "genesis" vitals,
c               split over 3 lines, for the same system:
c
c       2006061000_F000_210N_0853W_01L 20060614 1200 343N 0807W 035 093
c            1004 1012 0278 15 222 -999 -999 -999 -999 M -999 -999 
c            -999 -999 72 520N  410W  -999 -999 -999 -999
c
c     EXAMPLE:  If the vitals record is for a non-officially numbered
c               system (i.e., any system that's not a TC being tracked
c               by NHC or JTWC), then the storm number is replaced 
c               by the characters "FOF", for "Found On the Fly" by 
c               the  tracker.
c
c       2006071500_F000_150N_0681W_FOF 20060718 1200 185N 0792W 035 093
c            1004 1012 0278 15 222 -999 -999 -999 -999 M -999 -999 
c            -999 -999 72 520N  410W  -999 -999 -999 -999
c
c       NOTE: The "F000" in there at character positions 12-15 are to 
c             indicate the forecast hour within that forecast cycle 
c             that the storm was first detected.  For a vitals record,
c             this is always going to be 000 for fhr=0h, and really,
c             it's not even needed.  However, I'm keeping it in there
c             in order to keep the storm ID format exactly the same 
c             as the  output_atcf_sink forecast track record, which 
c             does have a use for that "FXXX" identifier in the 
c             output.
c
c     INPUT:
c     lgvcard    integer unit number for tcgen-tcvitals card
c
c     OUTPUT:
c     maxstorm  max # of storms to be handled for this case
c     iret      return code from this subroutine
c
c     INPUT/OUTPUT: 
c     numtcv    As an input, this variable contains the number of 
c               *tropical* cyclone vitals (i.e., regular tcvitals) that
c               were read off of the input tcvitals file in subroutine
c               read_tcv_card.  This variable will be incremented for 
c               each "modified" vitals record that is read in this 
c               subroutine, and so as output, this variable will 
c               contain the combined total of tcvitals and modified 
c               vitals records.
c
c     OTHER:
c     stormswitch 1,2 or 3 (see more description under Main pgm section)
c     slonfg     first guess array for longitude
c     slatfg     first guess array for latitude
c     storm      contains the tcvitals info
c     (storm, stormswitch, slonfg and slatfg are allocatable and are
c      defined in module def_vitals)
c               
      USE def_vitals; USE set_max_parms; USE trkrparms; USE gen_vitals
      USE verbose_output

      implicit none

      type (gencard) tmpstorm(maxstorm_mg)
      type (trackstuff) trkrinfo
      logical(1) :: vit_file_exists
      integer    iret,maxstorm
      integer    i,ii,lgvcard,numtcv,num_mod_vit,vitix,iga
c------

      ! Check to see if the genesis TC Vitals file exists.  If so, then
      ! open it using the unit specified in lgvcard.

      inquire (file="tcvit_genesis_storms.txt",exist=vit_file_exists)

      if (vit_file_exists) then

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ TC Vitals file for genesis'
     &           ,'    storms exists and will be opened with '
     &           ,'    unit= lgvcard= ',lgvcard
        endif

        open (unit=lgvcard,file="tcvit_genesis_storms.txt",status='old'
     &       ,err=887)

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'+++ TC vitals file tcvit_genesis_storms.txt has '
          print *,'    been opened with unit= lgvcard= ',lgvcard
        endif

      endif

      ! Read in all of the "genesis vitals" into a temp array.  The 
      ! index for the first array member is one past the number of 
      ! tc vitals that were read in in subroutine  read_tcv_card.

      ii = numtcv + 1

      if (vit_file_exists) then

        do while (.true. .and. ii <= maxstorm_mg)
          read (lgvcard,24,END=801,ERR=891) tmpstorm(ii)
          ii = ii + 1
        enddo

   24   format (i10,2x,i3,1x,i3,a1,1x,i4,a1,1x,a3,1x,i8,1x,i4,1x,i3,a1
     &      ,1x,i4,a1,1x,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

  801   continue

      endif


      num_mod_vit = ii - numtcv - 1

      allocate (gstorm(maxstorm_mg),stat=iga)
      if (iga /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in read_gen_vitals allocating gstorm array'
          print *,'!!! iga = ',iga
        endif

        iret = 97
        return   
      endif   

      ! Initialize all "genesis dates" to 99999.  Any new genesis 
      ! vitals that are read in below will bring in real dates, and 
      ! then we can test the date in output_gen_vitals to know if a
      ! storm was already defined or not at the beginning of this 
      ! executable or if it was a new storm that was found.

      do i = 1,maxstorm_mg
        gstorm(i)%gv_gen_date = 99999
      enddo

      ! If there are any TC vitals (i.e., officially named TCs
      ! that are being numbered/tracked by either NHC or JTWC), then
      ! we want to take the important information from those vitals
      ! and put that into genesis vitals.  This will enable us to 
      ! output *all* of these systems in the "gen_vitals" or 
      ! "gstorm" format.  The one difference here is that for the
      ! genesis date, we use the starting date of this forecast, not
      ! the time that the storm first formed.  Also, set the genesis
      ! forecast hour (gv_gen_fhr) to be 0 for TCs that have a 
      ! TC vitals record.

      if (numtcv > 0) then
        do i = 1,numtcv
          gstorm(i)%gv_gen_date  = storm(i)%tcv_ymd * 100 +
     &                             storm(i)%tcv_hhmm / 100
          gstorm(i)%gv_gen_fhr   = 0
          gstorm(i)%gv_gen_lat   = storm(i)%tcv_lat
          gstorm(i)%gv_gen_latns = storm(i)%tcv_latns
          gstorm(i)%gv_gen_lon   = storm(i)%tcv_lon
          gstorm(i)%gv_gen_lonew = storm(i)%tcv_lonew
          gstorm(i)%gv_gen_type  = storm(i)%tcv_storm_id
          gstorm(i)%gv_obs_ymd   = storm(i)%tcv_ymd
          gstorm(i)%gv_obs_hhmm  = storm(i)%tcv_hhmm
          gstorm(i)%gv_obs_lat   = storm(i)%tcv_lat
          gstorm(i)%gv_obs_latns = storm(i)%tcv_latns
          gstorm(i)%gv_obs_lon   = storm(i)%tcv_lon
          gstorm(i)%gv_obs_lonew = storm(i)%tcv_lonew
          if ( verb .ge. 3 ) then
            write (*,34) gstorm(i)
          endif
        enddo
      endif

      if (num_mod_vit > 0) then

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'Following are the vitals for storms that were'
          print *,'read in from the modified (genesis) tc vitals file: '
          print *,' '
        endif

        do i=1,num_mod_vit
          vitix = i + numtcv
          stormswitch(vitix) = 1
          ! On the following line we are filling the array gstorm, 
          ! which is new in this subroutine.  Note, however, that we
          ! are not necessarily starting it at 1, but at the point in
          ! the array after any TC Vitals may have been read in.
          gstorm(vitix) = tmpstorm(vitix)

          if ( verb .ge. 3 ) then
            write (*,34) gstorm(vitix)
          endif

          ! For the sake of consistency (and sanity!!), we need to also
          ! use the same "storm" array as was used in read_tcv_card, 
          ! since this "storm" array is used often throughout the rest
          ! of this executable.

          write (storm(vitix)%tcv_storm_id,'(i4.4)') vitix
          write (storm(vitix)%tcv_storm_name,'(i4.4)') vitix
          storm(vitix)%tcv_ymd   = gstorm(vitix)%gv_obs_ymd
          storm(vitix)%tcv_hhmm  = gstorm(vitix)%gv_obs_hhmm
          storm(vitix)%tcv_lat   = gstorm(vitix)%gv_obs_lat
          storm(vitix)%tcv_latns = gstorm(vitix)%gv_obs_latns
          storm(vitix)%tcv_lon   = gstorm(vitix)%gv_obs_lon
          storm(vitix)%tcv_lonew = gstorm(vitix)%gv_obs_lonew
          storm(vitix)%tcv_stdir = gstorm(vitix)%gv_stdir
          storm(vitix)%tcv_stspd = gstorm(vitix)%gv_stspd

          if (trkrinfo%type == 'midlat') then
            storm(vitix)%tcv_center = 'MIDL'
          else if (trkrinfo%type == 'tcgen') then
            storm(vitix)%tcv_center = 'TCG '
          endif
      
          if (gstorm(vitix)%gv_obs_lonew == 'W') then
            slonfg(vitix,1) =  360. - float(gstorm(vitix)%gv_obs_lon)
     &                         / 10.0
          else
            slonfg(vitix,1) = float(gstorm(vitix)%gv_obs_lon)/10.0
          endif
          if (gstorm(vitix)%gv_obs_latns == 'S') then
            slatfg(vitix,1) = -1. * float(gstorm(vitix)%gv_obs_lat)/10.0
          else
            slatfg(vitix,1) = float(gstorm(vitix)%gv_obs_lat)/10.0
          endif
          stcvtype(vitix) = 'FOF' ! Storm "Found On the Fly" by tracker
        
        enddo
      endif

   34 format (i10,1x,'F',i3.3,1x,i3.3,a1,1x,i4.4,a1,1x,a3,1x,i8,1x,i4.4
     &       ,1x,i3.3,a1,1x,i4.4,a1,1x,i3,1x,i3,3(1x,i4),1x,i2,1x,i3,1x
     &       ,4(i4,1x),a1)

c     Update the total number of vitals that have been read in

      numtcv = numtcv + num_mod_vit

      goto 895
c     

 887  continue

      if ( verb .ge. 1 ) then 
        print *,'!!! ERROR in read_gen_vitals opening genesis vitals'
        print *,'!!! file named tcvit_genesis_storms.txt'
      endif
   
      iret = 97
      return


 891  continue
      
      if ( verb .ge. 1 ) then
        print *,'!!! ERROR in read_gen_vitals reading unit ',lgvcard
      endif

      iret = 98

  895 continue
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getgridinfo_grib (imax,jmax,ifh,dx,dy,lugb,lugi
     &                ,trkrinfo,need_to_flip_lats,need_to_flip_lons
     &                ,inp,iggret)
c
c     ABSTRACT: The purpose of this subroutine is just to get the max
c     values of i and j and the dx and dy grid spacing intervals for the
c     grid to be used in the rest of the program.  So just read the 
c     grib file to get the lon and lat data.  Also, get the info for 
c     the data grid's boundaries.  This boundary information will be 
c     used later in the tracking algorithm, and is accessed via Module 
c     grid_bounds.
c
      USE grid_bounds; USE trkrparms; USE tracked_parms; USE inparms
      USE verbose_output; USE params; USE grib_mod

      implicit none

      type (trackstuff) trkrinfo
      type (datecard) inp

      type(gribfield) :: gfld
      logical(1) :: need_to_flip_lats,need_to_flip_lons
      logical(1), allocatable :: lb(:)
      logical :: unpack=.true.
      logical :: open_grb=.false.
      CHARACTER(len=8) :: pabbrev
      integer,dimension(200) :: jids,jpdt,jgdt
      integer, parameter :: jf=40000000
      integer :: listsec1(13)
      integer   pdt_4p0_vert_level,pdt_4p0_vtime
      real      xhold,xlondiff,xlatdiff,temp,firstval,lastval
      real, allocatable :: f(:)
      real, allocatable :: tmplon(:),tmplat(:)
      real, intent(out) :: dx,dy
      integer   jpds(200),jgds(200),igetpds(200),igetgds(200)
      integer, intent(in)  :: ifh
      integer, intent(out) :: imax,jmax
      integer   iia,ija,ila,midi,midj,i,j,iix,jix,ifa,iret
      integer   iscanflag,iggret,kf,k,lugb,lugi,jskp,jdisc
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000

      iggret = 0

      allocate (lb(jf),stat=ila); allocate (f(jf),stat=ifa)
      if (ila /= 0 .or. ifa /= 0) then
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in getgridinfo allocating either lb or f'
          print *,'!!! ila = ',ila,' ifa= ',ifa
        endif
        iggret = 97
        return
      endif

      if (trkrinfo%gribver == 2) then

        ! Search for a record from a GRIB2 file

        !
        ! ---  Initialize Variables ---
        !

        gfld%idsect => NULL()
        gfld%local => NULL()
        gfld%list_opt => NULL()
        gfld%igdtmpl => NULL()
        gfld%ipdtmpl => NULL()
        gfld%coord_list => NULL()
        gfld%idrtmpl => NULL()
        gfld%bmap => NULL()
        gfld%fld => NULL()

        jdisc=0 ! meteorological products
        jids=-9999
        jpdtn=trkrinfo%g2_jpdtn ! 0 = analysis or forecast; 1 = ens fcst
        jgdtn=0 ! lat/lon grid
        jgdt=-9999
        jpdt=-9999

        npoints=0
        icount=0
        jskp=0

c       Search for Temperature or GP Height by production template....

        JPDT(1:15)=(/-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &           ,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

        !  Request a record on a lat/lon grid.

        jgdtn = 0

        !  Request a record at the current forecast lead time.

        if (inp%lt_units == 'minutes') then
          jpdt(8) = 0
          jpdt(9) = iftotalmins(ifh)
        else
          jpdt(8) = 1
          jpdt(9) = ifhours(ifh)
        endif

        if (verb >= 3) then
          print *,'before getgb2 call, lugb= ',lugb,' lugi= ',lugi
        endif

        call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt
     &             ,unpack,krec,gfld,iret)
        if ( iret.ne.0) then
          print *,' '
          print *,' ERROR: getgb2 error in getgridinfo = ',iret
          print *,' FATAL ERROR: cannot proceed without info '
          print *,' from getgridinfo.  STOPPING....'
          stop 95
        endif

c       Determine packing information from GRIB2 file
c       The default packing is 40  JPEG 2000

        ipack = 40

        if ( verb_g2 .ge. 1 ) then
          print *,' '
          print *,' -- BEGIN getgridinfo diagnostics for GRIB2 file ---'
          print *,' --       at ifh= ',ifh
          print *,' '
          print *,' gfld%idrtnum = ', gfld%idrtnum
        endif

        !   Set DRT info  ( packing info )
        if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
          ipack = 0
        elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
          ipack = 2
        elseif ( gfld%idrtnum.eq.3 ) then  ! Complex & spatial packing
          ipack = 31
        elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) then
          ! JPEG 2000 packing
          ipack = 40
        elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
          ipack = 41
        endif

        if ( verb_g2 .ge. 1 ) then
          print *,'After check of idrtnum, ipack= ',ipack
          print *,'Number of gridpts= gfld%ngrdpts= ',gfld%ngrdpts
          print *,'Number of elements= gfld%igdtlen= ',gfld%igdtlen
          print *,'PDT num= gfld%ipdtnum= ',gfld%ipdtnum
          print *,'GDT num= gfld%igdtnum= ',gfld%igdtnum
        endif

        imax = gfld%igdtmpl(8)
        jmax = gfld%igdtmpl(9)
        dx   = float(gfld%igdtmpl(17))/1.e6
        dy   = float(gfld%igdtmpl(17))/1.e6
        kf   = gfld%ngrdpts

        if (verb_g2 .ge. 1) then

          print *,' '
          print *,' SECTION 0: discipl= ',gfld%discipline
     &           ,' gribver= ',gfld%version

          print *,' '
          print *,' SECTION 1: '

          do j = 1,gfld%idsectlen
            print *,'     sect1, j= ',j,' gfld%idsect(j)= '
     &             ,gfld%idsect(j)
          enddo

          if ( associated(gfld%local).AND.gfld%locallen.gt.0) then
            print *,' '
            print *,' SECTION 2: ',gfld%locallen,' bytes'
          else
            print *,' '
            print *,' SECTION 2 DOES NOT EXIST IN THIS RECORD'
          endif

          print *,' '
          print *,' SECTION 3: griddef= ',gfld%griddef
          print *,'            ngrdpts= ',gfld%ngrdpts
          print *,'            numoct_opt= ',gfld%numoct_opt
          print *,'            interp_opt= ',gfld%interp_opt
          print *,'            igdtnum= ',gfld%igdtnum
          print *,'            igdtlen= ',gfld%igdtlen

          print *,' '
          print '(a17,i3,a2)',' GRID TEMPLATE 3.',gfld%igdtnum,': '
          do j=1,gfld%igdtlen
            print *,'    j= ',j,' gfld%igdtmpl(j)= ',gfld%igdtmpl(j)
          enddo

c         Get parameter abbrev for record that was retrieved
          print *,' '
          print *,'     PDT num (gfld%ipdtnum) = ',gfld%ipdtnum
          print *,' '
          print '(a20,i3,a2)',' PRODUCT TEMPLATE 4.',gfld%ipdtnum,': '
          do j=1,gfld%ipdtlen
            print *,'    sect 4  j= ',j,' gfld%ipdtmpl(j)= '
     &             ,gfld%ipdtmpl(j)
          enddo

        endif


        pdt_4p0_vtime      = gfld%ipdtmpl(9)
        pdt_4p0_vert_level = gfld%ipdtmpl(12)

        pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1)
     &                          ,gfld%ipdtmpl(2))

        firstval=gfld%fld(1)
        lastval=gfld%fld(kf)

        if (verb .ge. 3) then
          print *,' '
          write (6,131)
 131      format (' rec#   param     level  byy  bmm  bdd  bhh  '
     &           ,'fhr      npts  firstval    lastval')
          print '(i5,3x,a8,2x,6i5,2x,i8,4g12.4)'
     &        ,krec,pabbrev,pdt_4p0_vert_level/100,gfld%idsect(6)
     &           ,gfld%idsect(7),gfld%idsect(8),gfld%idsect(9)
     &           ,pdt_4p0_vtime,gfld%ngrdpts,firstval,lastval
        endif

        if (verb_g2 .ge. 1) then

          print *,' '
          print *,' -- END getgridinfo diagnostics for GRIB2 file ---'
          print *,' --     at ifh= ',ifh
          print *,' '
          print *,' '
          print *,' '

        endif

        need_to_flip_lons = .false.

        iscanflag = gfld%igdtmpl(19)
        if (mod(iscanflag,128) >= 64) then
          ! Input data is south to north...
          glatmin = float(gfld%igdtmpl(12))/1.e6
          glatmax = float(gfld%igdtmpl(15))/1.e6
          need_to_flip_lats = .true.
        else
          ! Input data is north to south...
          glatmin = float(gfld%igdtmpl(15))/1.e6
          glatmax = float(gfld%igdtmpl(12))/1.e6
          need_to_flip_lats = .false.
        endif

        glonmin = float(gfld%igdtmpl(13))/1.e6
        glonmax = float(gfld%igdtmpl(16))/1.e6

        if (verb .ge. 3) then
          print *,'In getgridinfo: glatmin= ',glatmin
          print *,'                glatmax= ',glatmax
          print *,'                glonmin= ',glonmin
          print *,'                glonmax= ',glonmax
        endif

        call gf_free (gfld)

      else

        !------------------------------------------
        ! Search for a record from a GRIB1 file
        !------------------------------------------

        jpds = -1
        jgds = -1

        jgds(1) = 0   ! Request a record that's on a lat/lon grid

        if ( verb .ge. 3 ) then
          print *,'before getgb in getgridinfo, ifh= ',ifh
          write (6,402) ifhours(ifh),ifclockmins(ifh)
 402      format (1x,'*       Forecast hour: ',i4,':',i2.2)
          print *,'       ifhours(ifh)= ',ifhours(ifh)
          print *,'   iftotalmins(ifh)= ',iftotalmins(ifh)
        endif

        !  Request a record at the current forecast lead time.

        if (inp%lt_units == 'minutes') then
          jpds(14) = iftotalmins(ifh)
        else
          jpds(14) = ifhours(ifh)
        endif

        j=0

c        jpds(14) = 0   ! test
c
        write(*,980) jpds(1),jpds(2)
        write(*,981) jpds(3),jpds(4)
        write(*,982) jpds(5),jpds(6)
        write(*,983) jpds(7),jpds(8)
        write(*,984) jpds(9),jpds(10)
        write(*,985) jpds(11),jpds(12)
        write(*,986) jpds(13),jpds(14)
        write(*,987) jpds(15),jpds(16)
        write(*,988) jpds(17),jpds(18)
        write(*,989) jpds(19),jpds(20)
        write(*,990) jpds(21),jpds(22)
        write(*,991) jpds(23),jpds(24)
        write(*,992) jpds(25)
        write(*,880) jgds(1),jgds(2)
        write(*,881) jgds(3),jgds(4)
        write(*,882) jgds(5),jgds(6)
        write(*,883) jgds(7),jgds(8)
        write(*,884) jgds(9),jgds(10)
        write(*,885) jgds(11),jgds(12)
        write(*,886) jgds(13),jgds(14)
        write(*,887) jgds(15),jgds(16)
        write(*,888) jgds(17),jgds(18)
        write(*,889) jgds(19),jgds(20)
        write(*,890) jgds(21),jgds(22)

  980   format('    jpds(1)  = ',i7,'  jpds(2)  = ',i7)
  981   format('    jpds(3)  = ',i7,'  jpds(4)  = ',i7)
  982   format('    jpds(5)  = ',i7,'  jpds(6)  = ',i7)
  983   format('    jpds(7)  = ',i7,'  jpds(8)  = ',i7)
  984   format('    jpds(9)  = ',i7,'  jpds(10) = ',i7)
  985   format('    jpds(11) = ',i7,'  jpds(12) = ',i7)
  986   format('    jpds(13) = ',i7,'  jpds(14) = ',i7)
  987   format('    jpds(15) = ',i7,'  jpds(16) = ',i7)
  988   format('    jpds(17) = ',i7,'  jpds(18) = ',i7)
  989   format('    jpds(19) = ',i7,'  jpds(20) = ',i7)
  990   format('    jpds(21) = ',i7,'  jpds(22) = ',i7)
  991   format('    jpds(23) = ',i7,'  jpds(24) = ',i7)
  992   format('    jpds(25) = ',i7)
  880   format('    jgds(1)  = ',i7,'  jgds(2)  = ',i7)
  881   format('    jgds(3)  = ',i7,'  jgds(4)  = ',i7)
  882   format('    jgds(5)  = ',i7,'  jgds(6)  = ',i7)
  883   format('    jgds(7)  = ',i7,'  jgds(8)  = ',i7)
  884   format('    jgds(9)  = ',i7,'  jgds(10) = ',i7)
  885   format('    jgds(11) = ',i7,'  jgds(12) = ',i7)
  886   format('    jgds(13) = ',i7,'  jgds(14) = ',i7)
  887   format('    jgds(15) = ',i7,'  jgds(16) = ',i7)
  888   format('    jgds(17) = ',i7,'  jgds(18) = ',i7)
  889   format('    jgds(19) = ',i7,'  jgds(20) = ',i7)
  890   format('    jgds(20) = ',i7,'  jgds(22) = ',i7)

        print *,'lugb= ',lugb,' lugi= ',lugi
        print *,'before ggi getgb jpds(14) = ',jpds(14)
        print *,'before ggi getgb jgds(1) = ',jgds(1)

        call getgb(lugb,lugi,jf,j,jpds,jgds,
     &                       kf,k,igetpds,igetgds,lb,f,iret)

        if (iret.ne.0) then

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR in getgridinfo calling getgb'
            print *,'!!! Return code from getgb = iret = ',iret
          endif

          iggret = iret
        else
          iggret=0
          imax = igetgds(2)
          jmax = igetgds(3)
          dx   = float(igetgds(9))/1000.
          dy   = float(igetgds(10))/1000.
        endif

c        write(*,780) igetpds(1),igetpds(2)
c        write(*,781) igetpds(3),igetpds(4)
c        write(*,782) igetpds(5),igetpds(6)
c        write(*,783) igetpds(7),igetpds(8)
c        write(*,784) igetpds(9),igetpds(10)
c        write(*,785) igetpds(11),igetpds(12)
c        write(*,786) igetpds(13),igetpds(14)
c        write(*,787) igetpds(15),igetpds(16)
c        write(*,788) igetpds(17),igetpds(18)
c        write(*,789) igetpds(19),igetpds(20)
c        write(*,790) igetpds(21),igetpds(22)
c        write(*,791) igetpds(23),igetpds(24)
c        write(*,792) igetpds(25)
c        write(*,680) igetgds(1),igetgds(2)
c        write(*,681) igetgds(3),igetgds(4)
c        write(*,682) igetgds(5),igetgds(6)
c        write(*,683) igetgds(7),igetgds(8)
c        write(*,684) igetgds(9),igetgds(10)
c        write(*,685) igetgds(11),igetgds(12)
c        write(*,686) igetgds(13),igetgds(14)
c        write(*,687) igetgds(15),igetgds(16)
c        write(*,688) igetgds(17),igetgds(18)
c        write(*,689) igetgds(19),igetgds(20)
c        write(*,690) igetgds(21),igetgds(22)
c
c  780   format('    kpds(1)  = ',i7,'  kpds(2)  = ',i7)
c  781   format('    kpds(3)  = ',i7,'  kpds(4)  = ',i7)
c  782   format('    kpds(5)  = ',i7,'  kpds(6)  = ',i7)
c  783   format('    kpds(7)  = ',i7,'  kpds(8)  = ',i7)
c  784   format('    kpds(9)  = ',i7,'  kpds(10) = ',i7)
c  785   format('    kpds(11) = ',i7,'  kpds(12) = ',i7)
c  786   format('    kpds(13) = ',i7,'  kpds(14) = ',i7)
c  787   format('    kpds(15) = ',i7,'  kpds(16) = ',i7)
c  788   format('    kpds(17) = ',i7,'  kpds(18) = ',i7)
c  789   format('    kpds(19) = ',i7,'  kpds(20) = ',i7)
c  790   format('    kpds(21) = ',i7,'  kpds(22) = ',i7)
c  791   format('    kpds(23) = ',i7,'  kpds(24) = ',i7)
c  792   format('    kpds(25) = ',i7)
c  680   format('    kgds(1)  = ',i7,'  kgds(2)  = ',i7)
c  681   format('    kgds(3)  = ',i7,'  kgds(4)  = ',i7)
c  682   format('    kgds(5)  = ',i7,'  kgds(6)  = ',i7)
c  683   format('    kgds(7)  = ',i7,'  kgds(8)  = ',i7)
c  684   format('    kgds(9)  = ',i7,'  kgds(10) = ',i7)
c  685   format('    kgds(11) = ',i7,'  kgds(12) = ',i7)
c  686   format('    kgds(13) = ',i7,'  kgds(14) = ',i7)
c  687   format('    kgds(15) = ',i7,'  kgds(16) = ',i7)
c  688   format('    kgds(17) = ',i7,'  kgds(18) = ',i7)
c  689   format('    kgds(19) = ',i7,'  kgds(20) = ',i7)
c  690   format('    kgds(20) = ',i7,'  kgds(22) = ',i7)

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'In getgridinfo, grid dimensions follow:'
          print *,'imax= ',imax,' jmax= ',jmax
          print *,'  dx= ',dx,'  dy= ',dy
        endif


c       ----------------------------------------------------------------
c       Get boundaries of the data grid.  NOTE: gds(4) is referred to in
c       GRIB documenatation as the "Latitude of origin", which might 
c       imply "minimum Latitude".  However, for the grids that we'll be
c       using in this program, the "Latitude of origin" will be listed 
c       under gds(4) as the northernmost point (eg., in MRF, 
c       gds(4) = 90), so for this program, use gds(4) as your max lat, 
c       and gds(7) as your min lat. However, in case NCEP, UKMET or 
c       ECMWF change their convention and begin flipping their grids, a
c       check is made to make sure that the max lat is not less than the
c       min lat.
c
c       BUGFIX (August, 2001): It is possible to have an input grid 
c       which goes from south to north (such as NAVGEM).  In this case,
c       we flip the data in subroutine conv1d2d_real.  However, the max 
c       and min latitudes listed in the GRIB GDS will be confused, so we
c       need to check the value of the GRIB scanning mode flag here.

        need_to_flip_lons = .false.

        iscanflag = igetgds(11)
        if (mod(iscanflag,128) >= 64) then
          ! Input data is south to north...
          glatmin = float(igetgds(4))/1000.
          glatmax = float(igetgds(7))/1000.
          need_to_flip_lats = .true.
        else
          ! Input data is north to south...
          glatmin = float(igetgds(7))/1000.
          glatmax = float(igetgds(4))/1000.
          need_to_flip_lats = .false.
        endif

        glonmin = float(igetgds(5))/1000.
        glonmax = float(igetgds(8))/1000.

      endif

c     After this point in this subroutine, nothing is GRIB1 / GRIB2
c     specific, so it does not need to be within the if/then 
c     statement above that differentiated between GRIB / GRIB2.

c17Jul2014      if (glonmin < 0.0) glonmin = 360. - abs(glonmin)
c17Jul2014      if (glonmax < 0.0) glonmax = 360. - abs(glonmax)

      if (glonmin >= 0.0 .and. glonmax >= 0.0) then
        if (glonmin > glonmax) then
          if (verb .ge. 3) then
            print *,' '
            print *,'NOTE:  For this file, the GRIB PDS indicates that'
            print *,'       the min longitude (glonmin) is greater'
            print *,'       than the max longitude (glonmax) where both'
            print *,'       longitudes are greater than 0.'
            print *,'       This means that the grid is spanning '
            print *,'       across the GM.'
            print *,'       We will adjust glonmax by adding 360 to it'
            print *,'       so that, in the end, glonmax > glonmin.' 
            print *,'       '
            print *,'       GRID MIN & MAX LON (ORIGINAL):'
            print *,'         original glonmin= ',glonmin
            print *,'         original glonmax= ',glonmax
            print *,'       '
            glonmax = glonmax + 360. 
            print *,'       '
            print *,'       GRID MIN & MAX LON '
            print *,'         (MODIFIED FOR GM WRAPPING):'
            print *,'         glonmin (same as original)= ',glonmin
            print *,'         glonmax (modified)=         ',glonmax
            print *,'       '
          endif
        endif
      elseif (glonmin < 0.0 .and. glonmax >= 0.0) then
        ! An example of this is the MPAS data, which starts and ends
        ! at the dateline and is specified as glonmin=-179.875,
        ! glonmax=179.875.  Convert to be positive and go from 
        ! 180.125 to 539.875.
        if (verb .ge. 3) then
          print *,' '
          print *,'NOTE: glonmin is < 0, glonmax > 0, so glonmin'
          print *,'      will be converted to be > 0 and 360 will'
          print *,'      be added to glonmax.'
          print *,'      BEFORE CONVERSION: '
          print *,'         glonmin= ',glonmin
          print *,'         glonmax= ',glonmax
        endif
        glonmin = 360. - abs(glonmin)
        glonmax = 360. + abs(glonmax)
        if (verb .ge. 3) then
          print *,'      AFTER CONVERSION: '
          print *,'         glonmin= ',glonmin
          print *,'         glonmax= ',glonmax
        endif
      elseif (glonmin < 0.0 .and. glonmax < 0.0) then
        ! Examples of this are GFDL and HWRF.  In this case, make
        ! both glonmin and glonmax positive.
        if (verb .ge. 3) then
          print *,' '
          print *,'NOTE: glonmin is < 0 and glonmax < 0, so both'
          print *,'      will be converted to be > 0.'
          print *,'      BEFORE CONVERSION: '
          print *,'         glonmin= ',glonmin
          print *,'         glonmax= ',glonmax
        endif
        glonmin = 360. - abs(glonmin)
        glonmax = 360. - abs(glonmax)
        if (verb .ge. 3) then
          print *,'      AFTER CONVERSION: '
          print *,'         glonmin= ',glonmin
          print *,'         glonmax= ',glonmax
        endif
      elseif (glonmin >= 0.0 .and. glonmax < 0.0) then
        ! An example of this is the GFS data, which goes from 
        ! glonmin=0.0 to glonmax=-0.5.  Convert it here to go
        ! from glonmin=0.0 to glonmax=359.5
        if (verb .ge. 3) then
          print *,' '
          print *,'NOTE: glonmin is >= 0 and glonmax < 0, so'
          print *,'      glonmax will be converted to be > 0.'
          print *,'      BEFORE CONVERSION: '
          print *,'         glonmin= ',glonmin
          print *,'         glonmax= ',glonmax
        endif
        glonmax = 360. - abs(glonmax)
        if (verb .ge. 3) then
          print *,'      AFTER CONVERSION: '
          print *,'         glonmin= ',glonmin
          print *,'         glonmax= ',glonmax
        endif
      endif

c17Jul2014      if (glonmin < 0.0) then
c17Jul2014        glonmin = 360. - abs(glonmin)
c17Jul2014        if (glonmax <= 0.0) then
c17Jul2014          glonmax = 360. - abs(glonmax)
c17Jul2014        else
c17Jul2014          glonmax = 360 + abs(glonmax)
c17Jul2014        endif
c17Jul2014      endif

      if (glatmax < glatmin) then
        temp    = glatmax
        glatmax = glatmin
        glatmin = temp
      endif

      if (glonmin > 200.0 .and. glonmin <= 360.) then
        if (glonmax < 50.) then
          ! Likely GM-wrapping in current record
          glonmax = glonmax + 360.
        endif
      endif
c
      if ( verb .ge. 3 ) then
        print *,' '
        print *,'Data Grid Lat/Lon boundaries follow:'
        write (6,81) glatmin,glonmin
 81     format (' Min Lat: ',f8.3,'  Min Lon: ',f8.3)
        write (6,83) glatmax,glonmax
 83     format (' Max Lat: ',f8.3,'  Max Lon: ',f8.3)
        print *,' '
        print *,'NOTE: For regional grids, valid data points might'
        print *,'NOT extend all the way to the gds-defined grid '
        print *,'boundary, due to the fact that data have been '
        print *,'interpolated from a NPS or Lamb-Conf grid onto a '
        print *,'lat/lon grid.  This program checks the logical '
        print *,'bitmap for valid data points, but just keep this in'
        print *,'mind if trying to debug errors that occur near the'
        print *,'grid boundaries for regional models.'
      endif

c     ----------------------------------------------------------------
c     Fill glat and glon with the lat & lon values for the grid.  This
c     info will be used in subroutine  barnes

      if (allocated(glat)) deallocate(glat)
      if (allocated(glon)) deallocate(glon)

      allocate (glat(jmax),stat=ija)
      allocate (glon(imax),stat=iia)
      if (ija /= 0 .or. iia /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in getgridinfo allocating glon or glat'
          print *,'!!! ija = ',ija,' iia= ',iia
        endif

        if (allocated(lb)) deallocate (lb,stat=ila)
        if (allocated(f))  deallocate (f,stat=ifa)

        if (ila /= 0 .or. ifa /= 0) then
          print *,' '
          print *,'!!! ERROR in getgridinfo deallocating arrays.'
          print *,'!!! ila= ',ila,' ifa= ',ifa
          print *,'!!! EXITING....'
          stop 98
        endif

        iggret = 96
        return
      endif

      do j=1,jmax
        glat(j) = glatmax - (j-1)*dy
      enddo
      do i=1,imax
        glon(i) = glonmin + (i-1)*dx
      enddo

      if (allocated(lb)) deallocate (lb,stat=ila)
      if (allocated(f))  deallocate (f,stat=ifa)

      if (ila /= 0 .or. ifa /= 0) then
        print *,' '
        print *,'!!! ERROR in getgridinfo deallocating arrays.'
        print *,'!!! ila= ',ila,' ifa= ',ifa
        print *,'!!! EXITING....'
        stop 98
      endif

c     --------------------------------------------------------------
c     Finally, check to see if the requested boundary limits that
c     the user input are contained within this grid (for example, 
c     someone running this tracker on a regional grid may have 
c     forgotten to change the input grid bounds from a global grid 
c     run).  Modify the user-input bounds as needed.  
c
c     NOTE: Only check these bounds for a genesis run on a regional
c     grid, whether that be a 'midlat' or a 'tcgen' run.

      if (trkrinfo%gridtype == 'regional' .and. 
     &    trkrinfo%type /= 'tracker') then

        if (trkrinfo%eastbd > glonmax) then
          xhold = trkrinfo%eastbd
          trkrinfo%eastbd = glonmax - 5.0
          
          if ( verb .ge. 3 ) then
            write (6,90)
            write (6,91)
            write (6,92)
            write (6,93)
            write (6,94)
            write (6,95)
            write (6,96)
            write (6,97) 'EASTERN LONGITUDE'
            write (6,98) xhold
            write (6,99) trkrinfo%eastbd
            write (6,91)
          endif

        endif

        if (trkrinfo%westbd < glonmin) then
          xhold = trkrinfo%westbd
          trkrinfo%westbd = glonmin + 5.0

          if ( verb .ge. 3 ) then
            write (6,90)
            write (6,91)
            write (6,92)
            write (6,93)
            write (6,94)
            write (6,95)
            write (6,96)
            write (6,97) 'WESTERN LONGITUDE'
            write (6,98) xhold
            write (6,99) trkrinfo%westbd
            write (6,91)
          endif

        endif

        if (trkrinfo%northbd > glatmax) then
          xhold = trkrinfo%northbd
          trkrinfo%northbd = glatmax - 5.0
          if ( verb .ge. 3 ) then
            write (6,90)            
            write (6,91)
            write (6,92)
            write (6,93)
            write (6,94)
            write (6,95)
            write (6,96)
            write (6,97) 'NORTHERN LATITUDE'
            write (6,98) xhold            
            write (6,99) trkrinfo%northbd
            write (6,91)
          endif

        endif

        if (trkrinfo%southbd < glatmin) then
          xhold = trkrinfo%southbd
          trkrinfo%southbd = glatmin + 5.0

          if ( verb .ge. 3 ) then
            write (6,90)            
            write (6,91)
            write (6,92)
            write (6,93)
            write (6,94)
            write (6,95)
            write (6,96)
            write (6,97) 'SOUTHERN LATITUDE'
            write (6,98) xhold            
            write (6,99) trkrinfo%southbd
            write (6,91)
          endif

        endif

      endif

  90  format (///)
  91  format (' *********************************************')
  92  format (' WARNING: A USER-REQUESTED BOUNDARY IS BEYOND')
  93  format (' THE BOUNDARY OF THE DATA, AS DEFINED IN THE ')
  94  format (' GRIB FILE.  THE USER BOUNDARY WILL BE MODIFIED')
  95  format (' TO MATCH THE BOUNDARY OF THE DATA FILE.')
  96  format (' ')
  97  format (' USER-INPUT BOUNDARY AT FAULT: ',A20)
  98  format (' USER-INPUT BOUNDARY VALUE: ',f8.2)
  99  format (' NEW BOUNDARY VALUE: ',f8.2)

c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getgridinfo_netcdf (ncfile_id,imax,jmax,dx,dy
     &                   ,trkrinfo,need_to_flip_lats,need_to_flip_lons
     &                   ,inp,netcdfinfo,iggret)
c
c     ABSTRACT: The purpose of this subroutine is just to get the max
c     values of i and j and the dx and dy grid spacing intervals for the
c     grid to be used in the rest of the program.  So just query the 
c     netcdf file to get the lon and lat data.  Also, get the info for 
c     the data grid's boundaries.  This boundary information will be 
c     used later in the tracking algorithm, and is accessed via Module 
c     grid_bounds.
c
      USE grid_bounds; USE trkrparms; USE inparms
      USE verbose_output; USE netcdf_parms

      implicit none
c
      type (trackstuff) trkrinfo
      type (netcdfstuff) netcdfinfo
      type (datecard) inp

      logical(1) :: need_to_flip_lats,need_to_flip_lons
      real      xhold,xlondiff,xlatdiff
      real, allocatable :: tmplon(:),tmplat(:)
      real, intent(out) :: dx,dy
      integer   iscanflag,iggret
      integer, intent(in)  :: ncfile_id
      integer, intent(out) :: imax,jmax
      integer :: iia,ija,midi,midj,i,j,iix,jix
c

      iggret = 0

      call get_ncdim1(ncfile_id,netcdfinfo%lon_name,imax)
      call get_ncdim1(ncfile_id,netcdfinfo%lat_name,jmax)

      if (allocated(tmplon)) deallocate (tmplon)
      if (allocated(tmplat)) deallocate (tmplat)
      allocate (tmplon(imax),stat=iia)
      allocate (tmplat(jmax),stat=ija)
      if (iia /= 0 .or. ija /= 0) then
        print *,' '
        print *,'!!! ERROR in sub getgridinfo_netcdf allocating arrays.'
        print *,'!!! iia = ',iia,' ija= ',ija
        iggret = 94
        return
      endif

      if (verb .ge. 1) then
        print *,'in getgridinfo_netcdf, ncfile_id= ',ncfile_id
      endif

      call get_var1_double (ncfile_id,netcdfinfo%lon_name,imax,tmplon)
      call get_var1_double (ncfile_id,netcdfinfo%lat_name,jmax,tmplat)

c     Compute the dx and dy by picking values out of the middle of 
c     the lat and lon arrays....

      midi = imax/2
      midj = jmax/2

      dx = abs(tmplon(midi) - tmplon(midi-1))
      dy = abs(tmplat(midj) - tmplat(midj-1))

      if (verb .ge. 1) then
        print *,' '
        print *,'In getgridinfo, grid dimensions follow:'
        print *,'imax= ',imax,' jmax= ',jmax
        print *,'dx=   ',dx,' dy= ',dy
        print *,' '
        write (6,112) midi,dx
        write (6,113) midj,dy

 112    format(1x,' DX:  midi= ',i4,' dx= ',f8.4)
 113    format(1x,' DY:  midj= ',i4,' dy= ',f8.4)
      endif


c     ------------------------------------------------------------------
c     Get boundaries of the data grid.  Note that it is possible to have
c     an input grid which goes from south to north (in fact, it appears
c     that many NetCDF files are constructed this way).  Keep in mind,
c     however, that the  tracker has been written such that point (1,1)
c     should be the upper-leftmost point on the grid, while point 
c     (imax,jmax) should be the lower-rightmost point.  If we check and
c     find that we're dealing with data that instead starts from the 
c     south and increases northward, we flip the data in subroutine 
c     conv1d2d_real.  Similarly here, we make sure to test so that when
c     we are done in this routine, glatmax refers to the northernmost
c     latitude and glatmin the southernmost latitude.

      if (tmplon(imax) > tmplon(1)) then
        glonmin = tmplon(1)
        glonmax = tmplon(imax)
      else
        glonmin = tmplon(imax)
        glonmax = tmplon(1)
      endif

      if (tmplat(1) > tmplon(jmax)) then
        glatmax = tmplat(1)
        glatmin = tmplat(jmax)
      else
        glatmax = tmplat(jmax)
        glatmin = tmplat(1)
      endif

      print *,' '
      print *,'Data Grid Lat/Lon boundaries follow:'
      write (6,81) glatmin,glonmin
  81  format (' Min Lat: ',f8.3,'  Min Lon: ',f8.3)
      write (6,83) glatmax,glonmax
  83  format (' Max Lat: ',f8.3,'  Max Lon: ',f8.3)

c     ----------------------------------------------------------------
c     Fill glat and glon with the lat & lon values for the grid.  This
c     info will be used in subroutine  barnes

      if (allocated(glon)) deallocate (glon)
      if (allocated(glat)) deallocate (glat)

      allocate (glat(jmax),stat=ija) 
      allocate (glon(imax),stat=iia)
      if (ija /= 0 .or. iia /= 0) then
        print *,' '
        print *,'!!! ERROR in getgridinfo allocating glon or glat'
        print *,'!!! ija = ',ija,' iia= ',iia
        iggret = 96
        return
      endif

      ! If the lat or lon grids are flipped (i.e., the lats increase 
      ! from south to north, or the lons increase westward), then we 
      ! will need to flip both the data arrays as well as the arrays
      ! that are holding the values of the lats and lons....

      need_to_flip_lats = .false.
      need_to_flip_lons = .false.

      if (tmplat(1) > tmplon(jmax)) then
        do j=1,jmax
          glat(j) = tmplat(j)
        enddo
      else
        do j=1,jmax
          jix = jmax - j + 1
          glat(jix) = tmplat(j)
        enddo
        need_to_flip_lats = .true.
      endif

      if (tmplon(imax) > tmplon(1)) then
        do i=1,imax
          glon(i) = tmplon(i)
        enddo
      else
        do i=1,imax
          iix = imax - i + 1
          glon(iix) = tmplon(i)
        enddo
        need_to_flip_lons = .true.
      endif

c      do i = 1,imax
c        print *,'i= ',i,' glon(i)= ',glon(i)
c      enddo
c      do j = 1,jmax
c        print *,'j= ',j,' glat(j)= ',glat(j)
c      enddo

c     ---------------------------------------------------------------
c     Finally, check to see if the requested boundary limits that
c     the user input are contained within this grid (for example, 
c     someone running this tracker on a regional grid may have forgotten
c     to change the input grid bounds from a global grid run).  Modify 
c     the user-input bounds as needed.
c
c     NOTE: Only check these bounds for a genesis run on a regional
c     grid, whether that be a 'midlat' or a 'tcgen' run.

      if (trkrinfo%gridtype == 'regional' .and.
     &    trkrinfo%type /= 'tracker') then

        if (trkrinfo%eastbd > glonmax) then
          xhold = trkrinfo%eastbd
          trkrinfo%eastbd = glonmax - 5.0
          write (6,90)
          write (6,91)
          write (6,92)
          write (6,93)
          write (6,94)
          write (6,95)
          write (6,96)
          write (6,97) 'EASTERN LONGITUDE'
          write (6,98) xhold
          write (6,99) trkrinfo%eastbd
          write (6,91)
        endif

        if (trkrinfo%westbd < glonmin) then
          xhold = trkrinfo%westbd
          trkrinfo%westbd = glonmin + 5.0
          write (6,90)
          write (6,91)
          write (6,92)
          write (6,93)
          write (6,94)
          write (6,95)
          write (6,96)
          write (6,97) 'WESTERN LONGITUDE'
          write (6,98) xhold
          write (6,99) trkrinfo%westbd
          write (6,91)
        endif

        if (trkrinfo%northbd > glatmax) then
          xhold = trkrinfo%northbd
          trkrinfo%northbd = glatmax - 5.0
          write (6,90)            
          write (6,91)
          write (6,92)
          write (6,93)
          write (6,94)
          write (6,95)
          write (6,96)
          write (6,97) 'NORTHERN LATITUDE'
          write (6,98) xhold            
          write (6,99) trkrinfo%northbd
          write (6,91)
        endif

        if (trkrinfo%southbd < glatmin) then
          xhold = trkrinfo%southbd
          trkrinfo%southbd = glatmin + 5.0
          write (6,90)            
          write (6,91)
          write (6,92)
          write (6,93)
          write (6,94)
          write (6,95)
          write (6,96)
          write (6,97) 'SOUTHERN LATITUDE'
          write (6,98) xhold            
          write (6,99) trkrinfo%southbd
          write (6,91)
        endif

      endif

  90  format (///)
  91  format (' *********************************************')
  92  format (' WARNING: A USER-REQUESTED BOUNDARY IS BEYOND')
  93  format (' THE BOUNDARY OF THE DATA, AS DEFINED IN THE ')
  94  format (' GRIB FILE.  THE USER BOUNDARY WILL BE MODIFIED')
  95  format (' TO MATCH THE BOUNDARY OF THE DATA FILE.')
  96  format (' ')
  97  format (' USER-INPUT BOUNDARY AT FAULT: ',A20)
  98  format (' USER-INPUT BOUNDARY VALUE: ',f8.2)
  99  format (' NEW BOUNDARY VALUE: ',f8.2)

c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_netcdf_hours (ncfile,ncfile_id,ncfile_tmax,ifhmax
     &                            ,ncfile_has_hour0,netcdfinfo,irnhret)
c
c     ABSTRACT: The purpose of this subroutine is to read the "time"
c     dimension and "time data" from the NetCDF file so that we know
c     how many time levels there are and what those time levels are.
c     One reason for doing this is that some models, like the GFDL
c     FV3, do not output hour 0 data, so we need to check this first
c     before running through the tracking processing for the various 
c     hours.  We will take the list of hours read in here directly from
c     the NetCDF file and compare that against the *requested* list of 
c     forecast hours that the user has entered.  The user might not be 
c     aware that there is no hour 0 data for a given model.  We compare
c     these two lists of forecast hours and then write a message if 
c     there is a lead time that is not in the NetCDF file.
c   
c     INPUT:
c     ncfile     character name of NetCDF file
c     ncfile_id  integer id associated with NetCDF file after open
c     ifhmax     integer max number of lead times that the user has 
c                requested on the input lead times data file.  This
c                value was set in subroutine read_fhours.
c     netcdfinfo variable of user-defined type netcdfstuff (from 
c                module netcdf_parms).
c
c     OUTPUT:
c     ncfile_tmax integer max number of lead times that are in the 
c                 NetCDF file, as read in from this subroutine
c     ncfile_has_hour0 character flag (y|n) that tells whether or not 
c                 the input NetCDF data file actually has an hour0 
c                 record in it or not.
c
      USE netcdf_parms; USE tracked_parms; USE verbose_output

      implicit none
c
      type (netcdfstuff) netcdfinfo

      character :: ncfile*180,ncfile_has_hour0*1,match_check*1
      integer, intent(in)  :: ncfile_id
      integer, intent(out) :: ncfile_tmax
      integer :: infta,k,m,n,ifhmax,irnhret,usertime
c

      irnhret = 0
      ncfile_has_hour0 = 'n'

      !-----------------------------------------------------------
      ! First read the NetCDF file to get the number of time levels,
      ! which will be returned in "ncfile_tmax"....
      !-----------------------------------------------------------

      print *,' '
      print *,'in read_netcdf_hours...'
      print *,'ncfile_id= ',ncfile_id
      print *,'netcdfinfo%time_name= ',netcdfinfo%time_name
      print *,'ncfile_tmax= ',ncfile_tmax

      call get_ncdim1(ncfile_id,netcdfinfo%time_name,ncfile_tmax)

      if (verb .ge. 1) then
        print *,'in getgridinfo_netcdf, ncfile_id= ',ncfile_id
        print *,'Num netcdf time levs=  ncfile_tmax= ',ncfile_tmax
      endif

      if (allocated(netcdf_file_time_values)) then
        deallocate (netcdf_file_time_values)
      endif

      allocate (netcdf_file_time_values(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' '
        print *,'!!! ERROR in sub read_netcdf_hours allocating'
        print *,'!!! netcdf_file_time_values array.  infta = ',infta
        irnhret = 94
        return
      endif


      !-----------------------------------------------------------
      ! Now read in the actual time values that are stored in the 
      ! NetCDF file....
      !-----------------------------------------------------------

      call get_var1_double (ncfile_id,netcdfinfo%time_name,ncfile_tmax
     &                     ,netcdf_file_time_values)

      if (verb .ge. 1) then
        do k = 1,ncfile_tmax
          print *,'k= ',k,' netcdf_file_time_values(k)= '
     &                     ,netcdf_file_time_values(k)
        enddo
      endif

      !------------------------------------------------------------
      ! Now convert the NetCDF time values into minutes in order to 
      ! be able to compare with the user-requested list of lead 
      ! times.  Remember that the NetCDF lead times will be listed
      ! either as hours or as fractions of days.
      !------------------------------------------------------------

      if (allocated(nctotalmins)) then
        deallocate (nctotalmins)
      endif 

      allocate (nctotalmins(ncfile_tmax),stat=infta)
      if (infta /= 0) then
        print *,' ' 
        print *,'!!! ERROR in sub read_netcdf_hours allocating '
        print *,'!!! nctotalmins array.  infta = ',infta
        irnhret = 94
        return
      endif

      do k = 1,ncfile_tmax

        if (netcdfinfo%time_units == 'hours') then
          nctotalmins(k) = int(netcdf_file_time_values(k)) * 60
        elseif (netcdfinfo%time_units == 'days') then
          nctotalmins(k) = int(netcdf_file_time_values(k) * 60. * 24.)
        else
          print *,' '
          print *,'!!! ERROR: In read_netcdf_hours, the value of'
          print *,'    netcdfinfo%time_units is neither hours nor days.'
          print *,'    netcdfinfo%time_units= ',netcdfinfo%time_units
          print *,'    STOPPING....'
          print *,' '
          stop 99
        endif
 
        if (verb .ge. 1) then
          write (6,71) k,netcdf_file_time_values(k),nctotalmins(k)
        endif

      enddo

   71 format (1x,i5,'  netcdf_file_time_values(k)= ',f8.4
     &             ,'  nctotalmins(k)= ',i10)

      !------------------------------------------------------------
      ! Now go through the list of user-requested lead times that 
      ! were read in from subroutine read_fhours and try to match
      ! the two lists up.  The big one to watch out for is whether
      ! or not the NetCDF file actually has an hour 0 lead time.
      !------------------------------------------------------------

      userloop: do n = 1,ifhmax
  
        usertime = iftotalmins(n)

        match_check = 'n'

        netcdfloop: do m = 1,ncfile_tmax

          if (usertime == nctotalmins(m)) then
            if (verb .ge. 1) then
              print *,'+++ Time match for usertime= ',usertime
            endif
            match_check = 'y'
          endif
        
        enddo netcdfloop 

        if (match_check == 'n') then

          if (usertime == 0) then
            print *,' '
            print *,'Warning: For a NetCDF file, the user has requested'
            print *,'to read in an hour 0 file, however a scan of the'
            print *,'time data values in the NetCDF file indicates'
            print *,'that there is no hour 0 data in this file. '
            print *,'We will substitute either missing values or '
            print *,'the values from the TC Vitals data in the '
            print *,'hour 0 record and then start searching at the '
            print *,'next lead time.'
            ncfile_has_hour0 = 'n'
          else
            print *,' '
            print *,'!!! ERROR: For a NetCDF file, the user has'
            print *,'  requested to process a particular lead time that'
            print *,'  does not exist in the NetCDF list of time '
            print *,'  values.'
            print *,'  n= ',n
            print *,'  usertime= iftotalmins(n)= ',iftotalmins(n)
            print *,'  STOPPING....'
            stop 99
          endif

        elseif (match_check == 'y') then

          if (usertime == 0) then
            if (verb .ge. 1) then
              print *,' ' 
              print *,'+++ For the input NetCDF file, an hour0 data '
              print *,'    record exists in the data file.'
            endif
            ncfile_has_hour0 = 'y'
          endif

        endif

      enddo userloop 
c
      return
      end
c
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine check_valid_point (imax,jmax,dx,dy,fxy,cmaxmin
     &        ,valid_pt,rlont,rlatt,grid_maxlat,grid_minlat,grid_maxlon
     &        ,grid_minlon,trkrinfo,icvpret)
c
c     ABSTRACT: This subroutine checks to see if the input lat/lon
c     point is associated with four surrounding (i,j) locations that
c     have valid data.  The writing of this routine was prompted by the
c     HFIP project in February, 2009.  Some of their high resolution
c     data for their inner nests contained grids that had been rotated
c     from native map projections to regular lat/lon grids, but that
c     rotation left "empty" spots on the lat/lon grid where there is
c     no data.  Then when searching in find_maxmin, we were running
c     barnes iterations from these lat/lon locations where there was
c     no data, which would give artificially low values at those
c     lat/lon locations (because the barnes scheme would only include
c     points that were relatively far away where there was valid data).
c     So in this routine, we call subroutine  fix_latlon_to_ij in order
c     to get the nearest (i,j) coordinates, and then we check all of
c     these points to make sure that valid data exist.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     dx       grid spacing in i-direction
c     dy       grid spacing in j-direction
c     fxy      real array of input data values
c     cmaxmin  character that tells if searching for max or min
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     rlatt,rlont    input lat/lon about which we will check the
c              surrounding (i,j) locations for valid data.
c     grid_maxlat northernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_minlat southernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_maxlon easternmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     grid_minlon westernmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset.
c     trkrinfo derived type containing grid info on user boundaries
c
c     OUTPUT:
c     icvpret  return code from this routine.  A value of 0 means that
c              all is okay and the input point is surrounded by valid
c              data.

      USE trkrparms

      implicit none
c
      type (trackstuff) trkrinfo

      integer    imax,jmax,ifix,jfix
      integer    ifilret,icvpret
      character(*)  cmaxmin
      logical(1) valid_pt(imax,jmax)
      real       fxy(imax,jmax)
      real       rlont,rlatt,xdum,gridpoint_maxmin
      real       dx,dy,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
c
      call fix_latlon_to_ij (imax,jmax,dx,dy,fxy,cmaxmin
     &        ,valid_pt,rlont,rlatt
     &        ,xdum,ifix,jfix,gridpoint_maxmin,'checker'
     &        ,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
     &        ,trkrinfo,ifilret)

      if (ifilret /= 0) then
        icvpret = 99
        return
      endif

      if (valid_pt(ifix,jfix)) then
        icvpret = 0
      else
        icvpret = 99
      endif
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine fix_latlon_to_ij (imax,jmax,dx,dy,fxy,cmaxmin
     &                 ,valid_pt,parmlon,parmlat,xdataval
     &                 ,ifix,jfix,gridpoint_maxmin,ccall
     &                 ,grid_maxlat,grid_minlat,grid_maxlon,grid_minlon
     &                 ,trkrinfo,ifilret)
c
c     ABSTRACT: This subroutine takes an input lat/lon position and
c     assigns it to a nearby (i,j) gridpoint.  If this is being used
c     before the call to check_closed_contour after the  barnes analysis
c     to see if we have a storm or not, then the lat/lon position that
c     is input into this subroutine is one which was obtained from a
c     barnes analysis, so it is essentially an area-weighted average
c     of nearby points.  What we need to do in this subroutine is find
c     the actual nearby gridpoint which does have the actual raw max or
c     min value.  Then we return the (i,j) coordinates of that point as
c     well as that raw data value at that point.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     dx       grid spacing of the data grid in i-direction
c     dy       grid spacing of the data grid in j-direction
c     fxy      real array of input data values
c     cmaxmin  character that tells if searching for max or min
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     parmlon  lon at which input parameter center was found, or the lon
c              for the mean storm center fix (check calling routine)
c     parmlat  lat at which input parameter center was found, or the lat
c              for the mean storm center fix (check calling routine)
c     xdataval barnes-obtained value of parameter at (parmlon,parmlat)
c     ccall    character that tells if this call is part of a tracker 
c              fix routine or just from the check_valid_point routine 
c              ('tracker' or 'checker')
c     grid_maxlat northernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset, or it may be the 
c              original grid itself.
c     grid_minlat southernmost latitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset, or it may be the 
c              original grid itself.
c     grid_maxlon easternmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset, or it may be the 
c              original grid itself.
c     grid_minlon westernmost longitude on the input grid being sent to
c              this routine.  This grid may be a subset of the original
c              full grid from the original dataset, or it may be the 
c              original grid itself.
c     trkrinfo derived type containing grid info on user boundaries
c
c     OUTPUT:
c     ifix     i-index for gridpoint to which the max or min is assigned
c     jfix     j-index for gridpoint to which the max or min is assigned
c     gridpoint_maxmin  value of fxy at (ifix,jfix).  This will be
c              different from the input value xdataval, which came from
c              the  barnes averaging.  This is the raw value at the
c              gridpoint.

      USE grid_bounds; USE trkrparms
      USE verbose_output

      implicit none
c
      type (trackstuff) trkrinfo

      integer    imax,jmax,istart,iend,jstart,jend,ifix,jfix
      integer    ipfix,jpfix,i,j,ifilret,iix,jix,grfact
      character(*)  cmaxmin,ccall
      logical(1) valid_pt(imax,jmax)
      real       fxy(imax,jmax)
      real       parmlon,parmlat,xdataval,gridpoint_maxmin
      real       xplon,yplat,dmin,dmax,dx,dy,grdspc
      real       grid_maxlat,grid_minlat,grid_maxlon,grid_minlon

      ifilret = 0

c      print *,' '
c      print *,'-------------------------------------------- '
c      print *,'Top of fix_latlon_to_ij, call type = ',ccall
c      print *,'parmlon= ',parmlon,' parmlat= ',parmlat
c      print *,'max lon = ',grid_maxlon,' max lat = ',grid_maxlat
c      print *,'min lon = ',grid_minlon,' min lat = ',grid_minlat

c     Fix parmlat to the *nearest* j-point (i.e., round it....)

      if (parmlat >= 0.0) then    ! N. Hemisphere
        jpfix = int((grid_maxlat - parmlat)/dy + 1.0 + 0.5)
      else                        ! S. Hemisphere
        jpfix = ceiling((grid_maxlat - parmlat)/dy + 1.0 - 0.5)
      endif

c     Fix parmlon to the *nearest* i-point (i.e., round it....)
      
      ipfix = int((parmlon - grid_minlon)/dx + 1.0 + 0.5)
      
c     Calculate the longitude and latitude of these ipfix and
c     jpfix points....
      
      xplon = grid_minlon + (ipfix-1)*dx
      yplat = grid_maxlat - (jpfix-1)*dy
      
c     We want to do a simple search in the very few points around
c     this (ipfix,jpfix) point to find the raw max or min data
c     value.  First we need to set up a 4x4 box to search:
c     
c               o     o     o     o
c     
c     
c               o     a     b     o
c                      +
c     
c               o     c     d     o
c     
c     
c               o     o     o     o
c     
c     In the above diagram, if "+" is the lat/lon location of our
c     barnes-found point (i.e., the input (parmlon,parmlat)), and
c     a-b-c-d is the square of points surrounding "+", we only want
c     to look out 1 layer of points further.  So first we need to
c     know, for each case we're looking at, if "+" got assigned to
c     a or b or c or d.  By the way, if the parmlon falls directly
c     on a gridpoint in either the i or j direction, we will only
c     look at the 2 gridpoints on either side of that point, as
c     opposed to having 4 points set up as in the box above.
c
c     UPDATE (4-Feb-2011): For fine resolution grids, it is 
c     possible to have the gridpoint max/min be more than 1 or 2 grid
c     points away from the barnes-averaged max.  So allow for this
c     here, with a check of grdspc ((dx+dy)/2) below and the 
c     addition of the "grfact" multiplier for fine resolution grids.

c      print *,'ipfix= ',ipfix,' xplon= ',xplon
c      print *,'jpfix= ',jpfix,' yplat= ',yplat

       grdspc = (dx+dy)*0.5
       if (grdspc <= 0.025) then
         grfact = 20
       else if (grdspc > 0.025 .and. grdspc <= 0.05) then
         grfact = 12
       else if (grdspc > 0.05  .and. grdspc <= 0.10) then
         grfact = 6
       else if (grdspc > 0.10  .and. grdspc <= 0.20) then
         grfact = 3
       else if (grdspc > 0.20  .and. grdspc <= 0.30) then
         grfact = 2
       else
         grfact = 1
       endif
      
      if (xplon < parmlon) then         !(ipfix is at either a or c)
        istart = ipfix - (1*grfact)
        iend   = ipfix + (2*grfact)
      else if (xplon > parmlon) then    !(ipfix is at either b or d)
        istart = ipfix - (2*grfact)
        iend   = ipfix + (1*grfact)
      else if (xplon == parmlon) then   !(parmlon is exactly ipfix)
        istart = ipfix - (1*grfact)
        iend   = ipfix + (1*grfact)
      endif

      if (yplat < parmlat) then         !(jpfix is at either c or d)
        jstart = jpfix - (2*grfact)
        jend   = jpfix + (1*grfact)
      else if (yplat > parmlat) then    !(jpfix is at either a or b)
        jstart = jpfix - (1*grfact)
        jend   = jpfix + (2*grfact)
      else if (yplat == parmlat) then   !(parmlat is exactly jpfix)
        jstart = jpfix - (1*grfact)
        jend   = jpfix + (1*grfact)
      endif

c      print *,'istart= ',istart,' iend= ',iend
c      print *,'jstart= ',jstart,' jend= ',jend
c      print *,' '

c     Make sure the edges of our box are within the grid bounds...

      if (jstart > jmax ) then

        if ( verb .ge. 1 ) then
          print *,'!!! ERROR in fix_latlon_to_ij, jstart > jmax'
          print *,'!!! ',ccall,'  jstart = ',jstart,' jmax= ',jmax
        endif


        ifilret = 99
        return
      endif
      if (jend < 1) then

        if ( verb .ge. 1 ) then
          print *,'!!! ERROR in fix_latlon_to_ij, ',ccall
     &         ,' jend < 1, jend = ',jend
        endif

        ifilret = 99
        return
      endif
      if (jstart < 1) jstart = 1
      if (jend > jmax) jend = jmax

      if (istart > imax ) then
        if (trkrinfo%gridtype == 'global') then
          continue  ! GM wrapping will be handled in loop below...
        else

          if ( verb .ge. 1 ) then
            print *,'!!! ERROR in fix_latlon_to_ij, istart > imax'
            print *,'!!! istart = ',istart,' imax= ',imax
          endif

          ifilret = 99
          return
        endif
      endif

      if (iend < 1) then
        if (trkrinfo%gridtype == 'global') then
          continue  ! GM wrapping will be handled in loop below...
        else

          if ( verb .ge. 1 ) then
            print *,'!!! ERROR in fix_latlon_to_ij, iend < 1, iend = '
     &           ,iend,' call type = ',ccall
          endif

          ifilret = 99
          return
        endif
      endif

      if (iend > imax) then
        if (trkrinfo%gridtype == 'global') then
          continue  ! GM wrapping will be handled in loop below...
        else
          iend = imax  ! For a regional grid, just cut it off
        endif
      endif

      if (istart < 1) then
        if (trkrinfo%gridtype == 'global') then
          continue  ! GM wrapping will be handled in loop below...
        else
          istart = 1  ! For a regional grid, just cut it off
        endif
      endif

c     Now look for the max or min value....

      dmax = -9.99e12
      dmin =  9.99e12
      ifix = ipfix
      jfix = jpfix

      do iix = istart,iend
        do jix = jstart,jend

          i = iix
          j = jix

          if (i < 1) then
            i = iix + imax  !GM wrapping
          endif
          if (i > imax) then
            i = iix - imax  !GM wrapping
          endif

          if (valid_pt(i,j)) then
            continue
          else      

c            if ( verb .ge. 1 ) then
c              print *,' '
c              print *,'!!! ERROR: In fix_latlon_to_ij, we tried to '
c              print *,'!!! access an invalid data point.'
c              print *,'!!! ',ccall,' i= ',i,' j= ',j
c              print *,'!!! ipfix= ',ipfix,' jpfix= ',jpfix
c              print *,'!!! parmlon= ',parmlon,' parmlat= ',parmlat
c              print *,' '
c            endif

            ifilret = 98
            return
          endif
            
          if (cmaxmin == 'min') then
            if (fxy(i,j) < dmin) then
              dmin = fxy(i,j)
              ifix = i
              jfix = j
            endif
          else
            if (fxy(i,j) > dmax) then
              dmax = fxy(i,j)
              ifix = i
              jfix = j
            endif
          endif

        enddo
      enddo 
            
      if (cmaxmin == 'min') then
        gridpoint_maxmin = dmin
      else  
        gridpoint_maxmin = dmax
      endif 

c      print *,'  End of fix_latlon_to_ij, gridpoint_maxmin = '
c     &       ,gridpoint_maxmin

c
      return
      end  
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine rvcal (imax,jmax,dlon,dlat,z,vp)
c
c     ABSTRACT: This routine calculates the relative vorticity (zeta)
c     from u,v on an evenly-spaced lat/lon grid. Centered finite 
c     differences are used on the interior points and one-sided 
c     differences are used on the boundaries.
c
c     NOTE: There are 3 critical arrays in this subroutine, the first
c     being zeta and the 2nd and 3rd being u and v.  There is a 
c     critical difference in the array indexing for the levels.  For
c     zeta, the array is dimensioned with levels from 1 to 3, with 
c     1 = 850, 2 = 700, 3 = sfc.  However, there is an extra level 
c     for the winds, such that the level dimension goes 1 = 850, 
c     2 = 700, 3 = 500, 4 = sfc.  So we need to adjust for that in
c     this routine.
c
c     LOCAL VARIABLES:
c
      USE tracked_parms; USE trig_vals; USE grid_bounds
      USE verbose_output

      implicit none

      dimension cosfac(jmax),tanfac(jmax)
      real      tmpzeta(imax,jmax)
      real      xlondiff,xlatdiff,dlon,dlat,dfix
      real      dlat_edge,dlat_inter,dlon_edge,dlon_inter
      real      rlat(jmax),cosfac,tanfac
      integer   z,iscanflag,nlat,nlon,i,j,imax,jmax,w
      integer   ii,jj
      logical(1) vp(imax,jmax)

c     --------------------------

c     Figure out what level of data we have and what the array 
c     indices should be.

      if (z == 1) then
        ! z = 1 for 850 mb zeta, w = 1 for 850 mb winds
        w = 1  
      else if (z == 2) then
        ! z = 2 for 700 mb zeta, w = 2 for 700 mb winds
        w = 2  
      else if (z == 3) then
        ! z = 3 for sfc zeta, w = 4 for sfc (10m) winds
        w = 4
      endif

c     Calculate grid increments for interior and edge points.

c     IMPORTANT: If dtk is defined in module trig_vals in km, then
c     we need to multiply by 1000 here to get meters.  If it's defined
c     as meters, just let it be.  Since the wind values are given in 
c     meters, that's why we need the dlon values to be in meters.

      if (dtk < 750.) then     ! chances are, dtk was defined as km
        dfix = 1000.0
      else                     ! dtk was already defined as meters
        dfix = 1.0
      endif

      dlon_edge = dtk * dfix * dlon          ! Di dist over 1 grid pt
      dlat_edge = dtk * dfix * dlat          ! Dj dist over 1 grid pt
      dlon_inter = dtk * dfix * 2.0 * dlon   ! Di dist over 2 grid pts
      dlat_inter = dtk * dfix * 2.0 * dlat   ! Dj dist over 2 grid pts


c     Calculate required trig functions.  These are functions of 
c     latitude.  Remember that the grid must go from north to south.
c     This north-to-south requirement has 
c     already been checked in subroutine  getgridinfo.  If necessary,
c     any flipping of the latitudes was done there, and flipping of
c     the data, again if necessary, was done in subroutine  getdata.

      do j=2,jmax-1
         rlat(j) = glatmax - ((j-1) * dlat)
         cosfac(j) = cos(dtr*rlat(j))
         tanfac(j) = (tan(dtr*rlat(j)))/erad
      enddo

c     Set trig factors at end points to closest interior point
c     to avoid a singularity if the domain includes the poles,
c     which it will for the global grids (MRF, GDAS, GFS, UKMET,NCE)

      cosfac(1) = cosfac(2)
      tanfac(1) = tanfac(2)
      cosfac(jmax) = cosfac(jmax-1)
      tanfac(jmax) = tanfac(jmax-1)

c     NOTE: These next bits of vorticity calculation code assume that 
c           the input grid is oriented so that point (1,1) is the upper
c           left-most (NW) and point (imax,jmax) is the lower right-
c           most point.  Any other grids will probably crash the 
c           program due to array out of bounds errors.
c     NOTE: Before each calculation is done, the logical array is 
c           checked to make sure that all the data points in this 
c           calculation have valid data (ie., that the points are not
c           outside a regional model's boundaries).
c
c !!! IMPORTANT NOTE: While testing this, I uncovered a bug, which was
c     that I had the "j+1" and "j-1" reversed.  Just from a physical 
c     understanding, the du/dy term at a point is calculated by taking 
c     the u value north of the point minus the u value south of the 
c     point. Intuitively, this is u(j+1) - u(j-1).  However, we have 
c     designed this program to have the northernmost point as
c     the beginning of the grid (i.e., for the global grids, j=1 at 90N,
c     and j increases southward).  Thus, if you would do u(j+1) -
c     u(j-1), you would actually be taking the u value south of the 
c     point minus the u value north of the point, EXACTLY THE OPPOSITE
c     OF WHAT YOU WANT.  Therefore, the vorticity calculations have
c     been changed so that we now have u(j-1) - u(j+1).
c
c     UPDATE FEB 2009:  With limited domain grids that have missing
c     data on them (such as you would have for a grid that has been
c     converted from a non-lat/lon grid to a lat/lon grid), we were
c     running into problems below with the setting of zeta values to
c     a missing value of -999.  In place of this, the easiest thing to
c     do is to simply assign a value of the background coriolis value
c     to that point.  No, this is not correct, but it is the easiest
c     workaround for this right now.  Setting it to zero would be too
c     far off.  Setting it to the coriolis component has a net effect
c     of not having much impact on the  barnes scheme result.
c
c     ---------------
c     Interior points
c     ---------------
    
      if ( verb .ge. 3 ) then
        print *,'Just before inter rvcalc, dlon_inter = ',dlon_inter
     &       ,' dlat_inter = ',dlat_inter
      endif

      do j=2,jmax-1
       do i=2,imax-1
c
        if (vp(i,j) .and. vp(i+1,j) .and. vp(i-1,j) .and. 
     &      vp(i,j+1) .and. vp(i,j-1)) then
c 
         zeta(i,j,z)= (v(i+1,j,w) - v(i-1,j,w))/(dlon_inter * cosfac(j))
     &               - (u(i,j-1,w) - u(i,j+1,w))/(dlat_inter)
     &               + tanfac(j)*u(i,j,w)

        else
c         zeta(i,j,z)= -999.
         zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
        endif
c
       enddo
      enddo
c
c     -----------------------------
c     Bottom (Southernmost) points
c     -----------------------------
c
      j=jmax
      do i=2,imax-1
c
       if (vp(i,j) .and. vp(i+1,j) .and. vp(i-1,j) .and. 
     &     vp(i,j-1)) then
c
         zeta(i,j,z)= (v(i+1,j,w) - v(i-1,j,w))/(dlon_inter * cosfac(j))
     &              - (u(i,j-1,w) - u(i,j,w))/(dlat_edge) 
     &              + tanfac(j)*u(i,j,w)
       else
c         zeta(i,j,z)= -999.
         zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
       endif
c
      enddo
c
c     --------------------------
c     Top (Northernmost) points
c     --------------------------
c
      j=1
      do i=2,imax-1
c
       if (vp(i,j) .and. vp(i+1,j) .and. vp(i-1,j) .and.  
     &     vp(i,j+1)) then
c
         zeta(i,j,z)= (v(i+1,j,w) - v(i-1,j,w))/(dlon_inter * cosfac(j))
     &              - (u(i,j,w) - u(i,j+1,w))/(dlat_edge)
     &              + tanfac(j)*u(i,j,w)
       else
c         zeta(i,j,z)= -999.
         zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
       endif
c
      enddo
c
c     -------------------------------
c     Left edge (Westernmost) points
c     -------------------------------
c
      i=1
      do j=2,jmax-1
c
       if (vp(i,j) .and. vp(i+1,j) .and. vp(i,j+1) .and.  
     &     vp(i,j-1)) then
c
         zeta(i,j,z) = (v(i+1,j,w) - v(i,j,w))/(dlon_edge * cosfac(j))
     &               - (u(i,j-1,w) - u(i,j+1,w))/(dlat_inter)
     &               + tanfac(j)*u(i,j,w)
       else
c         zeta(i,j,z)= -999.
         zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
       endif
c
      enddo
c
c     --------------------------------
c     Right edge (Easternmost) points
c     --------------------------------
c
      i=imax
      do j=2,jmax-1
c
       if (vp(i,j) .and. vp(i-1,j) .and. vp(i,j+1) .and.  
     &     vp(i,j-1)) then
c
         zeta(i,j,z) = (v(i,j,w) - v(i-1,j,w))/(dlon_edge * cosfac(j))
     &               - (u(i,j-1,w) - u(i,j+1,w))/(dlat_inter)
     &               + tanfac(j)*u(i,j,w)
       else 
c         zeta(i,j,z)= -999.
         zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
       endif
c
      enddo
c
c     ---------
c     SW corner
c     ---------
      i=1
      j=jmax
      if (vp(i,j) .and. vp(i+1,j) .and. vp(i,j-1) ) then 
c
        zeta(i,j,z) = (v(i+1,j,w)-v(i,j,w))/(dlon_edge * cosfac(j))
     &              - (u(i,j-1,w)-u(i,j,w))/(dlat_edge)
     &              + tanfac(j)*u(i,j,w)
      else
c        zeta(i,j,z)= -999.
        zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
      endif
c
c     ---------
c     NW corner
c     ---------
      i=1
      j=1
      if (vp(i,j) .and. vp(i+1,j) .and. vp(i,j+1) ) then
c
        zeta(i,j,z) = (v(i+1,j,w) - v(i,j,w))/(dlon_edge * cosfac(j))
     &              - (u(i,j,w) - u(i,j+1,w))/(dlat_edge)
     &              + tanfac(j)*u(i,j,w)
      else
c        zeta(i,j,z)= -999.
        zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
      endif
c
c     ---------
c     NE corner
c     ---------
      i=imax
      j=1
      if (vp(i,j) .and. vp(i-1,j) .and. vp(i,j+1) ) then
c
        zeta(i,j,z) = (v(i,j,w) - v(i-1,j,w))/(dlon_edge * cosfac(j))
     &              - (u(i,j,w) - u(i,j+1,w))/(dlat_edge)
     &              + tanfac(j)*u(i,j,w)
      else
c        zeta(i,j,z)= -999.
        zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
      endif
c
c     ---------
c     SE corner
c     ---------
      i=imax
      j=jmax
      if (vp(i,j) .and. vp(i-1,j) .and. vp(i,j-1) ) then
c
        zeta(i,j,z) = (v(i,j,w)-v(i-1,j,w))/(dlon_edge * cosfac(j))
     &              - (u(i,j-1,w)-u(i,j,w))/(dlat_edge)
     &              + tanfac(j)*u(i,j,w)
      else
c        zeta(i,j,z)= -999.
        zeta(i,j,z) = 2. * omega * sin(rlat(j)*dtr)
      endif
c
      do ii=1,imax
        do jj=1,jmax
          tmpzeta(ii,jj) = zeta(ii,jj,z) * 1.e5
        enddo
      enddo

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine thickness_calc (imax,jmax,vp)
c
c     ABSTRACT: This routine calculates the  thicknesses for three 
c     different layers: 200-500, 500-850 and 200-850 mb.
c
c     LOCAL VARIABLES:
c
      USE tracked_parms; USE verbose_output

      implicit none

      integer   i,j,layer,upper,lower,imax,jmax
      logical(1) vp(imax,jmax)

c     --------------------------

c     The array indices for the 3 different thickness layers are
c     as follows:
c       1: 500-850
c       2: 200-500
c       3: 200-850
c
c     The array indices for the levels for the 4 different GP height 
c     arrays (as assigned in subroutine  getdata) are as follows:
c       1: 850 mb
c       2: 700 mb
c       3: 500 mb
c       4: 200 mb


      do layer = 1,3

        select case (layer)
          case (1); upper=3; lower=1;
          case (2); upper=4; lower=3;
          case (3); upper=4; lower=1;
        end select

        do j = 1,jmax
          do i = 1,imax

            if (vp(i,j)) then
              thick(i,j,layer) = hgt(i,j,upper) - hgt(i,j,lower)
            else
              thick(i,j,layer) = -999.0
            endif

          enddo
        enddo

      enddo
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine first_ges_center (imax,jmax,dx,dy,cparm,fxy
     &            ,cmaxmin,trkrinfo,ifh,valid_pt,maxstorm,masked_out
     &            ,stormct,contour_info,maxmini,maxminj,ifgcret)
c
c     ABSTRACT: This subroutine scans an array and picks out areas of 
c     max or min, then loads those center positions into the first-
c     guess lat & lon arrays to be used by subroutine  tracker for 
c     locating the very specific low center positions.
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input grid
c     jmax     Number of gridpoints in j direction in input grid
c     dx       Grid spacing in i-direction for the input grid
c     dy       Grid spacing in j-direction for the input grid
c     cparm    Char string indicating what parm is being passed in
c     fxy      Real array of data values
c     finf     Logical. Field of influence.  Dimension same as fxy
c     cmaxmin  Char string to indicate if search is for a max or a min
c     trkrinfo Derived type that holds/describes various tracker parms,
c              including the contour interval to be used
c     ifh      Index for the forecast hour
c     valid_pt Logical bitmap masking non-valid grid points.  This is a
c              concern for the regional models, which are interpolated
c              from Lam-Conf or NPS grids onto lat/lon grids, leaving
c              grid points around the edges which have no valid data.
c     maxstorm max # of storms that can be handled in this run
c
c     INPUT/OUTPUT:
c     masked_out Logical. T = data point is already accounted for, under
c                the influence of another nearby max or min center, 
c                F = data point is available to be scanned by this 
c                subroutine for max or min centers.
c     stormct  Integer: keeps and increments a running tab of the number
c              of storms that have been tracked at any time across all
c              forecast hours
c     contour_info Type cint_stuff from module contours.  Contains 
c                  contour information
c
c     OUTPUT:
c     maxmini  Integer array containing i-indeces of max/min locations
c     maxminj  Integer array containing j-indeces of max/min locations
c     ifgcret  return code from this subroutine
c
c     OTHER:
c     storm    Contains the tcvitals for the storms (module def_vitals)

      USE trkrparms; USE grid_bounds; USE set_max_parms; USE def_vitals
      USE contours; USE tracked_parms
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info

      integer       i,j,n,isstart,ifamret,ibeg,jbeg,iend,jend
      integer       ifh,maxstorm,imax,jmax,itemp,ifgcret
      integer       stormct,oldstormct,mm
      logical(1)    valid_pt(imax,jmax),masked_out(imax,jmax)
      character(*)  cparm,cmaxmin
      integer       maxmini(maxstorm),maxminj(maxstorm)
      real          fxy(imax,jmax)
      real          dmax,dmin,dx,dy,dbuffer,tmp

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'*-------------------------------------------------*'
        print *,'* At top of first_ges_center                      *' 
        write (6,102) ifhours(ifh),ifclockmins(ifh)
 102    format (1x,'* Searching for new lows at hour ',i4,':',i2.2)
        print *,'*-------------------------------------------------*'
      endif


c     First check the user-supplied grid boundaries to see if we will 
c     scan the entire array or just a portion of it.

      if (trkrinfo%northbd < -998.0 .or. trkrinfo%southbd < -998.0 .or.
     &    trkrinfo%westbd < -998.0  .or. trkrinfo%eastbd < -998.0) then
        ! User did not specify a subgrid, so scan the whole domain
        ibeg = 1
        iend = imax
        jbeg = 1
        jend = jmax
      else

c        if (trkrinfo%westbd > 360.0 .or. trkrinfo%eastbd < 0.0 .or.
c     &      trkrinfo%westbd <   0.0 .or.

        if (trkrinfo%westbd > 360.0 .or. 
     &      trkrinfo%northbd > 90.0 .or. trkrinfo%northbd <-90.0 .or.
     &      trkrinfo%southbd > 90.0 .or. trkrinfo%southbd <-90.0 .or.
     &      trkrinfo%westbd  >= trkrinfo%eastbd .or.
     &      trkrinfo%southbd >= trkrinfo%northbd) then

          if (trkrinfo%westbd  > trkrinfo%eastbd) then

            if (trkrinfo%westbd < 360.0 .and.
     &          trkrinfo%eastbd >= 0.0)then

              ! In this special case, the user has specified that the 
              ! western boundary be to the west of the Greenwich 
              ! meridian and the eastern boundary be to the east of it.

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'++ NOTE: The user supplied grid lon boundaries'
                print *,'++       span across the Greenwich meridian.'
                print *,'++    '
                print *,'++        Western boundary: ',trkrinfo%westbd
                print *,'++        Eastern boundary: ',trkrinfo%eastbd
                print *,'++        Northern boundary: ',trkrinfo%northbd
                print *,'++        Southern boundary: ',trkrinfo%southbd
                print *,' '
              endif

              ! Calculate the beginning and ending i and j points for
              ! this case of spanning the Greenwich meridian.  The 
              ! beginning and ending j points are, obviously, the same
              ! as for the regular case below in the else.  The 
              ! i-beginning point will also be the same as for the 
              ! regular case.  However, the i-ending point will be 
              ! modified for the meridian wrap; it will be > imax.

              jbeg = int(((glatmax + dy - trkrinfo%northbd) 
     &               / dy) + 0.5)
              jend = int(((glatmax + dy - trkrinfo%southbd) 
     &               / dy) + 0.5)
              ibeg = int(((trkrinfo%westbd - glonmin + dx)  
     &               / dx) + 0.5)
c              iend = int(((trkrinfo%eastbd - glonmin + dx)  
c     &               / dx) + 0.5)
              iend = int(((trkrinfo%eastbd - glonmin + dx)  
     &               / dx) + 0.5) + imax

              goto 377

            endif
          endif

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: Error in first_ges_center.  There is a'
            print *,'!!!        problem with the user-supplied grid '
            print *,'!!!        boundaries.  Please check them and '
            print *,'!!!        resubmit the program.'
            print *,'!!!'
            print *,'!!!        Western boundary: ',trkrinfo%westbd
            print *,'!!!        Eastern boundary: ',trkrinfo%eastbd
            print *,'!!!        Northern boundary: ',trkrinfo%northbd
            print *,'!!!        Southern boundary: ',trkrinfo%southbd
            print *,' '
          endif

          ifgcret = 91
          return

 377      continue

        else
          ! Calculate the beginning and ending i and j points....
          jbeg = int(((glatmax + dy - trkrinfo%northbd) / dy)
     &              + 0.5)
          jend = int(((glatmax + dy - trkrinfo%southbd) / dy)
     &              + 0.5)
          ibeg = int(((trkrinfo%westbd - glonmin + dx)  / dx)
     &              + 0.5)
          iend = int(((trkrinfo%eastbd - glonmin + dx)  / dx)
     &              + 0.5)
        endif
      endif

c     Scan the requested portion of the grid and pick out the max and
c     min data values, figure out what the max and min contour levels
c     will be, and fill an array with the values of the various 
c     intermediate, incremental contour levels.

      if (trkrinfo%contint <= 0) then


        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR: Error in first_ges_center.  For a midlat'
          print *,'!!!        or tcgen run of the  tracker, the contour'
          print *,'!!!        interval supplied by the user is not '
          print *,'!!!        greater than 0.'
          print *,'!!! '
          print *,'!!! User-supplied contint = ',trkrinfo%contint
          print *,' '
        endif

        ifgcret = 91
        return
      endif

      dmin =  9.99e20
      dmax = -9.99e20

      do j = jbeg,jend
        do i = ibeg,iend
          if (i > imax) then
            itemp = i - imax   ! If wrapping past GM
          else
            itemp = i
          endif
          if (valid_pt(itemp,j)) then
            if (fxy(itemp,j) < dmin) dmin = fxy(itemp,j)    
            if (fxy(itemp,j) > dmax) dmax = fxy(itemp,j)    
          endif
        enddo
      enddo

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'*--------------------------------------------*'
        print *,'In first_ges_center, dmin= ',dmin,' dmax= ',dmax
      endif


c     We want to allow for storms moving out of the sub-region,
c     in which case we might hit slightly lower or higher 
c     contours than were found in the sub-region, so allow for 
c     an extra buffer and modify dmin and dmax....

      dbuffer = (dmax - dmin) / 2.0
      dmax = dmax + dbuffer
      dmin = dmin - dbuffer

      if ( verb .ge. 3 ) then
        print *,'after adjustment, dmin= ',dmin,' dmax= ',dmax
      endif

c      Next 2 lines changed for compiler compatibility on
c      other platforms....
c      contour_info%xmaxcont = dmax - amod(dmax,trkrinfo%contint)
c      contour_info%xmincont = dmin - amod(dmin,trkrinfo%contint)

      tmp = trkrinfo%contint
      contour_info%xmaxcont = dmax - mod(dmax,tmp)
      contour_info%xmincont = dmin - mod(dmin,tmp)

      if ( verb .ge. 3 ) then
        print *,'A1 contour_info%xmaxcont= ',contour_info%xmaxcont
        print *,'A1 contour_info%xmincont= ',contour_info%xmincont
      endif

      if (contour_info%xmincont > contour_info%xmaxcont) then
        contour_info%xmincont = contour_info%xmaxcont
      endif

c      if (dmin > contour_info%xmincont) then 
c        contour_info%xmincont=contour_info%xmincont + trkrinfo%contint
c      endif
c      if (dmax < contour_info%xmaxcont) then 
c        contour_info%xmaxcont=contour_info%xmaxcont - trkrinfo%contint
c      endif

      if ( verb .ge. 3 ) then
        print *,'A2 contour_info%xmaxcont= ',contour_info%xmaxcont
        print *,'A2 contour_info%xmincont= ',contour_info%xmincont
        print *,'maxconts= ',maxconts
      endif

c     NOTE: In the loop below, the contour_info%contvals array is now
c     (5/2003) no longer used in subsequent subroutines.  But we still
c     need to figure out the value of the contvals as we iterate the 
c     loop so we can know when we've surpassed dmax and can stop 
c     incrementing contour_info%numcont, which we do need in subsequent
c     subroutines.

      contour_info%numcont = 0
      do n = 1,maxconts
        contour_info%numcont = contour_info%numcont + 1
        contour_info%contvals(n) = contour_info%xmincont + 
     &                             float(n-1)*trkrinfo%contint
c        print *,'n= ',n,' contour_info%contvals(n)= '
c     &                 ,contour_info%contvals(n)
        if (contour_info%contvals(n) >= dmax) exit
      enddo
    
      oldstormct = stormct
      call find_all_maxmins (imax,jmax,ibeg,iend,jbeg,jend,fxy
     &        ,valid_pt,masked_out,contour_info,dx,dy
     &        ,trkrinfo,cmaxmin,maxstorm,stormct,maxmini
     &        ,maxminj,ifamret)

      if (stormct > 0) then
        continue
      else 

        if ( verb .ge. 3 ) then
          print *,' '
          print *,' '
          print *,'!!! ************************************************'
          print *,'!!! '
          print *,'!!! NOTE: In first_ges_center, the value of stormct'
          print *,'!!! returned from find_all_maxmins is not greater'
          print *,'!!! than 0.  This means there are no new centers'
          print *,'!!! to track, which is not likely.  Perhaps you are'
          print *,'!!! searching over too small of an area??'
          print *,'!!! '
          print *,'!!! ************************************************'
          print *,' '
        endif

      endif

      print *,'ifh= ',ifh,' oldstormct= ',oldstormct
      print *,  '           stormct= ',stormct

      do mm = 1,300
        print *,'mm= ',mm,' maxmini(mm)= ',maxmini(mm)
     &         ,' maxminj(mm)= ',maxminj(mm)
      enddo

      if (stormct > oldstormct .and. stormct > 0) then
        isstart = oldstormct + 1

        if ( verb .ge. 3 ) then
          write (6,*) ' '
          write (6,*) 'New search: '
          write (6,*) 'Possible new max/min locations at ifh= ',ifh
          write (6,*) '--------------------------------------------'
        endif

        do n = isstart,stormct
          if (trkrinfo%type == 'midlat') then
            storm(n)%tcv_center = 'MIDL'
          else if (trkrinfo%type == 'tcgen') then
            storm(n)%tcv_center = 'TCG '
          endif
          slonfg(n,ifh) = glonmin + (maxmini(n)-1)*dx
          slatfg(n,ifh) = glatmax - (maxminj(n)-1)*dy
          storm(n)%tcv_stspd = -99
          storm(n)%tcv_stdir = -99
          write (storm(n)%tcv_storm_id,'(i4.4)') n
          write (storm(n)%tcv_storm_name,'(i4.4)') n
          stormswitch(n) = 1
          if (cparm == 'mslp') then

            if ( verb .ge. 3 ) then
              write (6,71) maxmini(n),maxminj(n),slonfg(n,ifh)
     &             ,360.-slonfg(n,ifh),slatfg(n,ifh)
     &             ,slp(maxmini(n),maxminj(n))/100.0
            endif

          endif
        enddo
      else

        if ( verb .ge. 3 ) then
          print *,' '
          print *,' New search: '
          print *,'!!! NOTE: No new storms found in find_all_maxmins'
          print *,'!!! at ifh = ',ifh,'  stormct= ',stormct
          print *,'!!! oldstormct= ',oldstormct
          print *,' '
        endif

      endif

  71  format (1x,'i= ',i4,'  j= ',i4,'   lon: ',f7.2,'E  (',f6.2,'W)'
     &       ,2x,' lat: ',f6.2,'    mslp: ',f6.1,' mb')
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine find_all_maxmins (imax,jmax,ibeg,iend,jbeg,jend,fxy
     &          ,valid_pt,masked_out,contour_info,dx,dy
     &          ,trkrinfo,cmaxmin,maxstorm,stormct,maxmini
     &          ,maxminj,ifamret)
c
c     ABSTRACT: This subroutine will search an area delineated by  
c     input i and j indeces in order to find all local maxes or mins 
c     in that area.  The (i,j) locations of the maxes/mins are returned
c     in the maxmini and maxminj arrays.  The input 3-character string
c     cmaxmin will tell the subroutine to look for a "max" or a "min".
c
c     INPUT:
c     imax     Number of gridpoints in i direction in input grid
c     jmax     Number of gridpoints in j direction in input grid
c     ibeg     i-index for upper left location of grid to search
c     iend     i-index for lower right location of grid to search
c     jbeg     j-index for upper left location of grid to search
c     jend     j-index for lower right location of grid to search
c     fxy      Real array of data values
c     valid_pt Logical bitmap masking non-valid grid points.  This is a
c              concern for the regional models, which are interpolated
c              from Lam-Conf or NPS grids onto lat/lon grids, leaving
c              grid points around the edges which have no valid data.
c     masked_out Logical. T = data point is already accounted for, under
c                the influence of another nearby max or min center,
c                F = data point is available to be scanned by this
c                subroutine for max or min centers.
c     contour_info Type cint_stuff from module contours containing the
c                  the following 4 variables:
c     1. xmincont Real value for min contour level in the fxy data array
c     2. xmaxcont Real value for max contour level in the fxy data array
c     3. contvals Real array holding values of cont levels at this time
c     4. numcont  Number of contour intervals found at this time
c     dx       Grid spacing in x-direction
c     dy       Grid spacing in y-direction
c     trkrinfo derived type containing various user-input tracker parms
c     cmaxmin  String that declares if "min" or "max" is being searched
c     maxstorm max # of storms that can be handled in this run
c
c     INPUT/OUTPUT:
c     stormct  Integer: keeps and increments a running tab of the number
c              of storms that have been tracked at any time across all 
c              forecast hours
c
c     OUTPUT:
c     maxmini  integer array containing i-indeces of the max/min points
c     maxminj  integer array containing j-indeces of the max/min points
c     ifamret  return code from this subroutine

      USE trkrparms; USE set_max_parms; USE contours
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info
      integer    stormct,i,j,ibeg,iend,jbeg,jend,ix,jx,ixp1,ixm1
      integer    ip,jp,maxstorm,jxp1,jxm1,ifamret,isret,iaret,iclmret
      integer    isoiret,icccret,igicwret,imax,jmax
      character ccflag*1,get_last_isobar_flag*1,point_is_over_water*1
      character(*) cmaxmin
      logical(1) still_finding_valid_maxmins,rough_gradient_check_okay
      logical(1) valid_pt(imax,jmax),masked_out(imax,jmax)
      integer    maxmini(maxstorm),maxminj(maxstorm)
      real       fxy(imax,jmax)
      real       xavg,stdv,search_cutoff,dmin,dmax,sphere_cutoff
      real       plastbar,rlastbar,fract_land,dx,dy

c-----
      still_finding_valid_maxmins = .true.


c      print *,'ctm beg of find_all_maxmins, maxstorm= ',maxstorm


c     First, we want to get the mean and standard deviation of the input
c     field to be searched.  We can use the standard deviation info as
c     part of our guideline for when to stop searching for maxes & mins.
c     We will set the search cut-off threshold at 1/2 standard deviation
c     above the mean for min searches.  So, for the example of mslp, if
c     the mean pressure over the whole domain is 1010 mb and the 
c     standard deviation is 12 mb, then when we are searching, if the
c     lowest available (i.e., hasn't been found in a previous iteration
c     of this loop) pressure is 1016, then it's time to stop searching.

      call avgcalc   (fxy,imax*jmax,valid_pt,xavg,iaret)
      call stdevcalc (fxy,imax*jmax,valid_pt,xavg,stdv,isret)
      if (iaret /= 0 .or. isret /= 0) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR: In find_all_maxmins, the calls to avgcalc'
          print *,'!!!        and/or stdevcalc returned an error.'
          print *,'!!!    iaret= ',iaret,'  isret= ',iaret
          print *,' '
        endif

        ifamret = 98
        return
      endif

      if (cmaxmin == 'min') then
        search_cutoff = xavg + stdv*0.5
      else
        search_cutoff = xavg - stdv*0.5
      endif

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'In find_all_maxmins, search_cutoff= ',search_cutoff
        print *,' '
      endif

c     Now begin to search the domain.  We do a simple gridpoint scan,
c     and once we find the max/min value, we pass the (i,j) coordinates
c     at that point to a routine to check for a closed contour.  Then
c     we mask out those points in the contour (or, if there is not a 
c     closed contour, just the 8 points immediately surrounding the low
c     center) and we do another iteration of search_loop to look for 
c     more lows.  We mask out points we've found so that on subsequent
c     iterations of search_loop, we don't find the same old center 
c     again and again and again.....

      search_loop: do while (still_finding_valid_maxmins)

        dmin =  9.99e20
        dmax = -9.99e20

        jloop: do j = jbeg,jend
          iloop: do i = ibeg,iend

            ip = i
            jp = j

            if (ip > imax) then 
              if (trkrinfo%gridtype == 'global') then
                ip = i - imax   ! If wrapping past GM
              else
                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! WARNING: In find_all_maxmins, the '
                  print *,'!!!   user-requested eastern search boundary'
                  print *,'!!!   is beyond the eastern bounds of '
                  print *,'!!!   this regional grid.  The search'
                  print *,'!!!   will not extend to the user-requested'
                  print *,'!!!   grid boundary.'
                  print *,'!!!         '
                  print *,'!!!   imax of regional grid    = ',imax
                  print *,'!!!   User-requested eastern i = ',ip
                  print *,' '
                endif

                exit iloop

              endif
            endif

            if (valid_pt(ip,jp) .and..not. masked_out(ip,jp)) then
              if (cmaxmin == 'min') then
                if (fxy(ip,jp) < dmin) then
                  dmin = fxy(ip,jp)
                  ix = ip
                  jx = jp
                endif
              else
                if (fxy(ip,jp) > dmax) then
                  dmax = fxy(ip,jp)
                  ix = ip
                  jx = jp
                endif
              endif
            endif

          enddo iloop
        enddo jloop
 
        if (cmaxmin == 'min') then
          if (dmin < search_cutoff) then
            continue
          else
            still_finding_valid_maxmins = .false.
            exit search_loop
          endif
        else
          if (dmax > search_cutoff) then
            continue
          else
            still_finding_valid_maxmins = .false.
            exit search_loop
          endif
        endif

c       As a rough first check, see if the neighboring points on all
c       4 sides have a gradient sloping down into the found min point,
c       or at least that there is a flat field not having a gradient 
c       sloping away from the center point.

        call get_ijplus1_check_wrap (imax,jmax,ix,jx,ixp1,jxp1,ixm1
     &                              ,jxm1,trkrinfo,igicwret)

        if (igicwret /= 0) then
          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! NOTE: In find_all_maxmins, the center we found'
            print *,'!!!     is too close to the grid boundary and will'
            print *,'!!!     NOT be checked for a closed contour.'
            print *,'!!!  ix= ',ix,' jx= ',jx,'  fxy= ',fxy(ix,jx) 
            print *,'!!! '
            print *,' '
          endif

          masked_out(ix,jx) = .true. 
          cycle search_loop
        endif

        if (cmaxmin == 'min') then
          if (fxy(ix,jx) <= fxy(ixp1,jx) .and. 
     &        fxy(ix,jx) <= fxy(ix,jxm1) .and.
     &        fxy(ix,jx) <= fxy(ixm1,jx) .and.
     &        fxy(ix,jx) <= fxy(ix,jxp1)) then
            rough_gradient_check_okay = .true.
          else
            rough_gradient_check_okay = .false.
          endif
        else
          if (fxy(ix,jx) >= fxy(ixp1,jx) .and.
     &        fxy(ix,jx) >= fxy(ix,jxm1) .and.
     &        fxy(ix,jx) >= fxy(ixm1,jx) .and.
     &        fxy(ix,jx) >= fxy(ix,jxp1)) then
            rough_gradient_check_okay = .true.
          else
            rough_gradient_check_okay = .false.
          endif
        endif
 
        if (rough_gradient_check_okay) then

          if ( verb .ge. 3 ) then
            print *,'Found a possible max/min at ix= ',ix,' jx= ',jx
          endif


c         From this rough check, we appear to have a gradient sloping
c         in towards the center point.  Now call the subroutine to 
c         check whether or not there is in fact a closed contour
c         surrounding this local maximum or minimum.

          get_last_isobar_flag = 'n'
          ccflag = 'n'
          call check_closed_contour (imax,jmax,ix,jx,fxy,valid_pt
     &             ,masked_out,ccflag,cmaxmin,trkrinfo
     &             ,1,contour_info,get_last_isobar_flag,plastbar
     &             ,rlastbar,icccret)

          if (ccflag == 'y') then
            if (stormct < maxstorm) then
              stormct = stormct + 1

              if ( verb .ge. 3 ) then
                print *,'AAA stormct= ',stormct,'  ix= ',ix,' jx= ',jx
              endif

              ! For a tcgen case, we will add in one additional check,
              ! and that is to ensure the point is (mostly) over water.
              ! Only do this check if the user has requested it (some
              ! of the global models do not have a land-sea mask 
              ! included in the grib data files).

              point_is_over_water = 'u'

              if (trkrinfo%use_land_mask == 'y') then
                call check_land_mask (imax,jmax,ix,jx,fract_land
     &              ,valid_pt,dx,dy,point_is_over_water,iclmret)
                if (iclmret /= 0) then
                  print *,' '
                  print *,'!!! ERROR from check_land_mask for ix= ',ix
     &                   ,' jx= ',jx
                  print *,'!!! STOPPING PROGRAM'
                  stop 95
                endif
              endif
 
              if (point_is_over_water /= 'n') then
                maxmini(stormct) = ix
                maxminj(stormct) = jx
              endif

            else

              if ( verb .ge. 3 ) then
                print *,'---max stormct reached, stormct= ', stormct
              endif

            endif
          else

            if ( verb .ge. 3 ) then
              print *,'!!! contour check negative, ccflag= ',ccflag
            endif
          endif

          if ( verb .ge. 3 ) then
            print *,' '
            print *,'*-----------------------------------------------*'
            print *,'* After check_closed_contour...                 *'
            print *,'*-----------------------------------------------*'
            print *,' '
          endif

        endif

c       Regardless of whether or not the found point turns out to have
c       a closed contour, we don't want to find this local minimum or
c       its 8 surrounding points again in a search on a subsequent 
c       iteration of this loop.

        masked_out(ix,jx)     = .true.
        masked_out(ix,jxp1)   = .true.
        masked_out(ixp1,jxp1) = .true.
        masked_out(ixp1,jx)   = .true.
        masked_out(ixp1,jxm1) = .true.
        masked_out(ix,jxm1)   = .true.
        masked_out(ixm1,jxm1) = .true.
        masked_out(ixm1,jx)   = .true.
        masked_out(ixm1,jxp1) = .true.

      enddo search_loop

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine mask_based_on_wind_circ (imax,jmax,dx,dy,level
     &                     ,valid_pt,masked_outc,trkrinfo
     &                     ,ctlon,ctlat,cmodel_type,imbowret)
c
c     ABSTRACT: This subroutine masks out grid points for a storm that
c     is currently being tracked.  It is called after a fix has been 
c     made at the current forecast hour.  It is only used as a backup,
c     that is, if the mslp data were not there and/or a fix position
c     for mslp could not be made, then that means that the mask would 
c     not be able to get updated using the routine in subroutine  
c     check_closed_contour.  But we still do need to update that mask,
c     so we will instead do it based on wind circulation.  We will go
c     out radially from the center, starting at 40 km, then every 
c     40 km from there on out.  When the mean cyclonic Vt drops below
c     3 m/s, stop searching, and then mask out all grid points within
c     that last-searched radius.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     ix       i index for location of local max or min
c     jx       j index for location of local max or min
c     fxy      input data array
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     masked_outc Logical. T = data point is already accounted for, 
c                under the influence of another nearby max or min 
c                center; F = data point is available to be scanned by 
c                this subroutine for max or min centers.
c     ctlon    Fix longitude for the input parameter to this routine
c     ctlon    Fix latitude for the input parameter to this routine
c     cmodel_type  character, 'global' or 'regional'

      USE set_max_parms; USE trkrparms; USE grid_bounds 
      USE verbose_output; USE level_parms

      implicit none

      type (trackstuff) trkrinfo

      character(*)  cmodel_type
      integer, parameter :: numazim=24
      integer    imax,jmax,level,imbowret,nlev,iazim,i,j
      integer    ibiret1,ibiret2,azimuth_ct,igvtret
      integer    jnfix,jsfix,iefix,iwfix,bimct
      real       vr(numazim),vt(numazim)
      real       dx,dy,ctlon,ctlat,rdist,bear,targlat,targlon
      real       xintrp_u,xintrp_v,grid_buffer,xmax_rdist_reached
      real       vt_mean,vt_azim_sum,xbear,dist,degrees
      logical(1) valid_pt(imax,jmax),masked_outc(imax,jmax)
      logical(1) searching_valid_pts

      imbowret = 0

      select case (level)
        case (850);  nlev = nlev850  ! check module level_parms for
        case (700);  nlev = nlev700  ! the values of these....
        case (500);  nlev = nlev500
        case (1020); nlev = levsfc
      end select

      if (cmodel_type == 'regional') then
        grid_buffer = 0.30
      else
        grid_buffer = 0.0
      endif

      searching_valid_pts = .true.

      rdist = 40.0                 ! units in km
      xmax_rdist_reached = rdist   ! units in km

      bimct = 0

      radial_loop: do while (searching_valid_pts)

        azimuth_ct  = 0
        vt_azim_sum = 0.0
        vt = -999.0
        vr = -999.0

        azimloop: do iazim = 1,numazim

          bear = ((iazim-1) * 15.) + 7.5

          call distbear (ctlat,ctlon,rdist,bear,targlat,targlon)

          if (targlon >= glonmax) then
            if (trkrinfo%gridtype == 'global') then
              targlon = targlon - 360.  ! We just GM-wrapped for the 
                                        ! full, regular, global grid.
            else
              xmax_rdist_reached = rdist
              exit radial_loop
            endif
          endif

          if (targlon < glonmin) then
            if (trkrinfo%gridtype == 'global') then
              targlon = targlon + 360.  ! We just GM-wrapped for the
                                        ! full, regular, global grid.
            else
              xmax_rdist_reached = rdist
              exit radial_loop
            endif
          endif

          if (targlat > glatmax .or. targlat < glatmin) then
            xmax_rdist_reached = rdist
            exit radial_loop
          endif

          ! These calls to bilin_int_uneven pass a variable, level,
          ! that contains the vertical level to pull the wind data
          ! from, either 850, 700 or surface (which will be
          ! indicated by a value/code of 1020).

          call bilin_int_uneven (targlat,targlon
     &        ,dx,dy,imax,jmax,trkrinfo,level,'u',xintrp_u
     &            ,valid_pt,bimct,ibiret1)

          call bilin_int_uneven (targlat,targlon
     &        ,dx,dy,imax,jmax,trkrinfo,level,'v',xintrp_v
     &            ,valid_pt,bimct,ibiret2)
    
          if (ibiret1 == 0 .and. ibiret2 == 0) then
            call getvrvt (ctlon,ctlat,targlon,targlat
     &                   ,xintrp_u,xintrp_v,vr(iazim)
     &                   ,vt(iazim),igvtret)
            azimuth_ct = azimuth_ct + 1
            vt_azim_sum = vt_azim_sum + vt(iazim)
          else 
            ! If ibiret /= 0, then we have reached out too far (likely
            ! a regional grid).  So, pull the plug and just set the 
            ! xmax_rdist_reached to the last diagnosed value of rdist.
            xmax_rdist_reached = rdist
            exit radial_loop
          endif

        enddo azimloop

        if (azimuth_ct > 0) then
          ! Compute azimuthally-averaged Vt at this distance
          vt_mean = vt_azim_sum / float(azimuth_ct)
        else
          vt_mean = -999.0
        endif

        if ( verb .ge. 3 ) then
          print *,'mbow: rdist= ',rdist,' azimuth_ct= ',azimuth_ct
     &           ,'  vt_azim_sum= ',vt_azim_sum,' vt_mean= ',vt_mean
        endif

        if (ctlat >= 0.0) then
          if (vt_mean >= 3.0) then
            ! For a NH storm, if the cyclonic mean Vt >= 3.0, increment
            ! rdist and cycle through to the next iteration of 
            ! radial_loop.
            rdist = rdist + 40.0
          else
            xmax_rdist_reached = rdist
            exit radial_loop
          endif
        else
          if (vt_mean <= -3.0 .and. vt_mean > -998.0) then
            ! For a SH storm, if the cyclonic mean Vt <= -3.0, increment
            ! rdist and cycle through to the next iteration of
            ! radial_loop.
            rdist = rdist + 40.0
          else
            xmax_rdist_reached = rdist
            exit radial_loop
          endif
        endif

      enddo radial_loop

      if (bimct > 0) then
        if (verb .ge. 3) then
          print *,' '
          print *,'Warning summary: From sub mask_based_on_wind_circ,'
          print *,'calls to sub bilin_int_uneven resulted in'
          print *,'(blocked) attempts to access points outside the'
          print *,'bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in radial_loop= ',bimct
          print *,' '
        endif
      endif

      if ( verb .ge. 3 ) then
        print *,'mbow: After radial_loop, rdist= ',rdist
     &         ,'    xmax_rdist_reached= ',xmax_rdist_reached
      endif

c     -----------------------------------------------------------------
c     At this point, we are done searching radially outwards away from
c     the storm center.  The max radial distance we reached is called
c     xmax_rdist_reached.  By getting to this spot in the subroutine, 
c     that means that we bumped out of radial_loop above because the 
c     rdist being used in that loop got to a radius at which the mean
c     cyclonic Vt no longer was strong enough to continue the search
c     outward, so we need to reduce it by 40 km here (back to the value
c     for the last successful search).  At a minimum, we will mask to a 
c     radius of 80 km.

      if (xmax_rdist_reached > 80.0) then
        xmax_rdist_reached = xmax_rdist_reached - 40.0
      else
        xmax_rdist_reached = 80.0
      endif

      if ( verb .ge. 3 ) then
         print *,'mbow: After adjustment of xmax_rdist_reached, rdist= '
     &          ,rdist,'    xmax_rdist_reached= ',xmax_rdist_reached
       endif

      bearloop: do i = 1,4

        ! Now find the values of the longitude for the farthest west
        ! and east points and find the values of the latitude for the
        ! farthest north and south points.  The i and j indices 
        ! associated with these lons and lats will be used to define
        ! the bounds of the grid over which we scan to find points 
        ! that will update the mask.

        select case (i)
          case (1); xbear =   0.0;
          case (2); xbear =  90.0;
          case (3); xbear = 180.0;
          case (4); xbear = 270.0;
        end select

        call distbear (ctlat,ctlon,xmax_rdist_reached,xbear
     &                ,targlat,targlon)

        if ( verb .ge. 3 ) then
           print *,'mbow: distbear for i= ',i,' targlon= ',targlon
     &            ,' targlat= ',targlat
        endif

        if (targlon >= glonmax) then
          if (trkrinfo%gridtype == 'global') then
            targlon = targlon - 360.  ! We just GM-wrapped for the
                                      ! full, regular, global grid.
          else
            print *,' '
            print *,'WARNING: In subroutine mask_based_on_wind_circ,'
            print *,'targlon > glonmax for a regional grid, so we '
            print *,'cannot update the mask.'
            print *,'targlon= ',targlon,'  glonmax= ',glonmax
            imbowret = 95
            return
          endif
        endif

        if (targlon < glonmin) then
          if (trkrinfo%gridtype == 'global') then
            targlon = targlon + 360.  ! We just GM-wrapped for the
                                      ! full, regular, global grid.
          else
            print *,' '
            print *,'WARNING: In subroutine mask_based_on_wind_circ,'
            print *,'targlon < glonmin for a regional grid, so we '
            print *,'cannot update the mask.'
            print *,'targlon= ',targlon,'  glonmin= ',glonmin
            imbowret = 95
            return
          endif
        endif

        if (targlat > glatmax .or. targlat < glatmin) then
            print *,' '
            print *,'WARNING: In subroutine mask_based_on_wind_circ,'
            print *,'either targlat > glatmx or targlat < glatmin, so'
            print *,'we cannot update the mask.'
            print *,'targlat= ',targlat,'  glatmin= ',glatmin
            print *,'                      glatmin= ',glatmin
            imbowret = 95
            return
          cycle bearloop
        endif

        ! Get the i & j starting and ending points for our loop where 
        ! we will update the mask....

        if (i == 1) then

          ! Get j for northern latitude.  Fix targlat to the *nearest*
          ! j-point (i.e., round it....)

          if (targlat >= 0.0) then    ! N. Hemisphere
            jnfix = int((glatmax - targlat)/dy + 1.0 + 0.5)
          else                        ! S. Hemisphere
            jnfix = ceiling((glatmax - targlat)/dy + 1.0 - 0.5)
          endif

        elseif (i == 2) then

          ! Get i for eastern longitude.  Fix targlon to the *nearest*
          ! i-point (i.e., round it....)

          iefix = int((targlon - glonmin)/dx + 1.0 + 0.5)

        elseif (i == 3) then

          ! Get i for southern latitude.  Fix targlat to the *nearest*
          ! j-point (i.e., round it....)

          if (targlat >= 0.0) then    ! N. Hemisphere
            jsfix = int((glatmax - targlat)/dy + 1.0 + 0.5)
          else                        ! S. Hemisphere
            jsfix = ceiling((glatmax - targlat)/dy + 1.0 - 0.5)
          endif

        elseif (i == 4) then

          ! Get i for western longitude.  Fix targlon to the *nearest*
          ! i-point (i.e., round it....)

          iwfix = int((targlon - glonmin)/dx + 1.0 + 0.5)

        endif

      enddo bearloop

      if ( verb .ge. 3 ) then
         print *,'mbow:  iwfix= ',iwfix,' iefix= ',iefix
     &          ,' jnfix= ',jnfix,' jsfix= ',jsfix
      endif

      do i = iwfix,iefix
        do j = jnfix,jsfix

          call calcdist (glon(i),glat(j),ctlon,ctlat,dist,degrees)

          if (dist < xmax_rdist_reached) then
            masked_outc(i,j) = .true.
          endif

        enddo
      enddo

      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine check_closed_contour (imax,jmax,ix,jx,fxy,valid_pt
     &           ,masked_out,closed_contour,cmaxmin,trkrinfo
     &           ,num_requested_contours,contour_info
     &           ,get_last_isobar_flag,plastbar,rlastbar,icccret)
c
c     ABSTRACT: This subroutine checks a field of data around an input
c     (ix,jx) data point to see if a closed contour exists around 
c     that data point.  It can check for a closed contour on a max or a 
c     min field, depending on the value of the input variable 'cmaxmin'.
c     The algorithm works by examining rings of the 8 data points 
c     surrounding a data point that is in the contour interval.  For
c     example, in the diagram below, the X represents the location of
c     the local minimum value which was passed into this routine with
c     the coordinates (ix,jx), let's say it's 985 mb.  And let's assume
c     that the data values at points A-I are all in the 4 mb contour 
c     interval of 985-989 mb, and that all the surrounding points have
c     data values >= 989.  To test for a closed contour, we first check
c     the ring of 8 points immediately around point X to see what their
c     data values are.  If a data value is found that is below the 
c     lower limit of this contour interval (985 mb) or lower than the
c     local minimum value at the X point that we initially targeted 
c     (985 mb), then we do NOT have a closed contour, and we exit this
c     subroutine.  But in our example, that's not the case, and we have
c     5 points (B,D,E,F,G) that are in the interval.  So in our next 
c     iteration of the loop, we set up 5 rings, each one set up around 
c     the points found in the first iteration (B,D,E,F,G), and we check 
c     the 8 points around each of those points.  A logical array is 
c     used so that as soon as a point is found, it is flagged as being 
c     found.  In this way, when we look at the ring around point D, for
c     example, we won't pick point X again and set up another ring 
c     around it in the next ring iteration and end up in an infinite 
c     loop, going back and forth between point X and point D.  While 
c     checking the 8 points in a ring, if a found data value is above 
c     our contour interval (i.e., >= 989 mb), we just ignore the 
c     point; we only mark points that are in our contour interval, 
c     and again, if we find a point below our contour interval, we 
c     exit the subroutine with a flag indicating a closed contour was
c     NOT found.  So in this method, we keep spreading out from the 
c     initial local minimum and creating and checking new rings until 
c     we either: (a) Hit the edge of the regional grid, in which case 
c     we consider a closed contour NOT found, (b) Run into a data 
c     point that has been marked as being under the influence of 
c     another nearby low, in which case we consider a closed contour 
c     NOT found, (c) Run into a point which is below (above) our 
c     contour interval for a min (max) check, in which case we 
c     consider a closed contour NOT found, or (d) we run out of 
c     points to keep searching, we have no rings left to create and 
c     check because all of the surrounding points are above (below) 
c     our contour interval for a min (max) check, and by default we 
c     consider this a closed contour and return to the calling 
c     subroutine a flag indicating such.
c
c               + + + + + + + + + + 
c               + + + + + + + + + + 
c               + + A B + + + + + + 
c               + + C D X E + + + + 
c               + + + + F G + + + + 
c               + + + + + H I + + + 
c               + + + + + + + + + + 
c               + + + + + + + + + + 
c
c     UPDATE: This subroutine was updated to keep searching for 
c     multiple closed contours until it can't find anymore.  The 
c     input parameter num_requested_contours dictates how many 
c     contours to search for.  In the case of just trying to roughly
c     locate new centers and establish that there is a closed 
c     circulation, num_requested_contours will = 1, and we will exit
c     after finding that 1 contour.  But for a check after making a
c     full center fix, we set num_requested_contours = 999 so that 
c     we can keep searching for all closed contours around the low.
c     In this 999 case, you will eventually get to a point where
c     there is no closed contour.  In that case, in the standard
c     output you will see a message telling you that you hit a point
c     that is not in the contour and that there is no closed contour,
c     but you will also notice that the ccflag = y, meaning there is
c     a closed contour (because you have found at least 1 closed 
c     contour along the way).  The reason to keep searching for more
c     closed contours is that we can then return the value of the 
c     outermost closed isobar.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     ix       i index for location of local max or min
c     jx       j index for location of local max or min
c     fxy      input data array
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     masked_out Logical. T = data point is already accounted for, under
c                the influence of another nearby max or min center,
c                F = data point is available to be scanned by this
c                subroutine for max or min centers.
c     cmaxmin  character string ('max' or 'min') that tells this 
c              routine what we're looking for.
c     trkrinfo   derived type that holds/describes various tracker parms
c     contour_info Type cint_stuff from module contours.  Contains
c                  contour information
c     num_requested_contours  For the simple  first_ges_center check, 
c              this will be 1 (we just want to know if there's at
c              least 1 closed contour).  For the verifying check after
c              we've found a center, this will be 9999 (i.e., just keep
c              searching for more contours)
c     get_last_isobar_flag  character ('y' or 'n') to indicate whether
c              or not to report on the value of the last closed isobar
c              and the radius of the last closed isobar.
c
c     OUTPUT:
c     closed_contour character; A returned value of 'y' indicates that
c              this routine was able to find a closed contour. 
c     plastbar Contains the value of the last closed isobar (unrounded)
c     rlastbar Contains the mean radius of the last closed isobar 
c
c     LOCAL:
c     num_pts_in_all_contours Counter for the number of pts inside of 
c              the contour we're looking at
c     next_ring_ct Counter for the number of points that have been 
c              tagged to be used as center points for the next 
c              iteration of multiple_ring_loop.
c     next_contour_ct Counter for the number of points that have been
c              tagged to be used as center points in the first iteration
c              through single_contour_scan_loop as we begin to scan 
c              points in the *next* contour interval.  This counter gets
c              incremented when, for example, we are searching points 
c              around a current center point and we find one that is not
c              in our current interval, but rather is in the next 
c              interval.  We want to remember this point and store the 
c              location, so we increment this counter and store the 
c              location in next_contour_i and next_contour_j arrays.
c     beyond_contour_ct Counter for the number of points that have been
c              tagged to be used as center points for some subsequent 
c              iteration of successive_contours_loop.  This is 
c              different from next_contour_ct, which is used to hold 
c              the locations of points that are definitely in the 
c              *next* contour interval.  Here, we have points that we 
c              just store in a pool of potential points to be searched
c              in future iterations.  These points can come about in 
c              cases where there is a very intense, very compact low 
c              with a tight pressure gradient, such that multiple 
c              contour intervals could be spanned in between 2 adjacent
c              gridpoints (this is especially the case if the contour
c              interval you have chosen is small).  You need to be 
c              careful with how you handle this array.  Once you find 
c              that you have searchable points in next_contour_i or 
c              next_contour_j, do not just simply empty out this 
c              beyond_contour count and its i and j arrays.  The 
c              reason being that some of these "beyond" points may end
c              up being used and searched in subsequent iterations, but
c              not if we just delete them now.

   
      USE set_max_parms; USE trkrparms; USE contours; USE grid_bounds
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo
      type (cint_stuff) contour_info

      integer    i,j,ir,iria,irja,irx,jrx,ix,jx,imax,jmax
      integer    nb,ibx,jby,nct,iflip
      integer    mr,ringct,ixp1,ixm1,jxp1,jxm1,nring,iter
      integer    icenx,jcenx,icccret,next_ring_ct,igicwret
      integer    num_pts_in_all_contours,next_contour_ct
      integer    beyond_contour_ct
      integer    num_pts_in_one_contour
      integer    num_requested_contours,num_found_contours
      integer    nm,im,jm,inall,insingle,isc_count,rlast_distct
      character  found_a_point_in_our_contour*1,closed_contour*1
      character  found_a_point_below_contour*1
      character  found_a_point_above_contour*1,get_last_isobar_flag*1
      character(*) cmaxmin
      logical(1) still_scanning
      logical(1) valid_pt(imax,jmax),masked_out(imax,jmax)
      logical(1) point_is_already_in_our_contour(imax,jmax)
      logical(1) point_is_already_in_next_contour(imax,jmax)
      logical(1) point_is_already_in_beyond_pool(imax,jmax)
      integer    isni,isnj,inci,incj,ibci,ibcj,ihmi,ihmj,itmi,itmj
      integer, allocatable ::  search_next_i(:)
      integer, allocatable ::  search_next_j(:)
      integer, allocatable ::  next_contour_i(:)
      integer, allocatable ::  next_contour_j(:)
      integer, allocatable ::  beyond_contour_i(:)
      integer, allocatable ::  beyond_contour_j(:)
      integer, allocatable ::  hold_mask_i_loc(:)
      integer, allocatable ::  hold_mask_j_loc(:)
      integer, allocatable ::  temp_mask_i_loc(:)
      integer, allocatable ::  temp_mask_j_loc(:)
      integer, allocatable ::  ringposi(:),ringposj(:)
      real,allocatable :: ringpos(:,:)
      real       fxy(imax,jmax),contvals(maxconts)
      real       contlo,conthi,xcentval,contlo_next,conthi_next
      real       dist,degrees,rlast_distsum,plastbar,rlastbar
c
      if (allocated(search_next_i))    deallocate (search_next_i)
      if (allocated(search_next_j))    deallocate (search_next_j)
      if (allocated(next_contour_i))   deallocate (next_contour_i)
      if (allocated(next_contour_j))   deallocate (next_contour_j)
      if (allocated(beyond_contour_i)) deallocate (beyond_contour_i)
      if (allocated(beyond_contour_j)) deallocate (beyond_contour_j)
      if (allocated(hold_mask_i_loc))  deallocate (hold_mask_i_loc)
      if (allocated(hold_mask_j_loc))  deallocate (hold_mask_j_loc)
      if (allocated(temp_mask_i_loc))  deallocate (temp_mask_i_loc)
      if (allocated(temp_mask_j_loc))  deallocate (temp_mask_j_loc)
      allocate (search_next_i(imax*jmax),stat=isni)
      allocate (search_next_j(imax*jmax),stat=isnj)
      allocate (next_contour_i(imax*jmax),stat=inci)
      allocate (next_contour_j(imax*jmax),stat=incj)
      allocate (beyond_contour_i((imax*jmax)/2),stat=ibci)
      allocate (beyond_contour_j((imax*jmax)/2),stat=ibcj)
      allocate (hold_mask_i_loc(imax*jmax),stat=ihmi)
      allocate (hold_mask_j_loc(imax*jmax),stat=ihmj)
      allocate (temp_mask_i_loc(imax*jmax),stat=itmi)
      allocate (temp_mask_j_loc(imax*jmax),stat=itmj)
      if (isni /= 0 .or. isnj /= 0 .or. inci /= 0 .or. incj /= 0 .or.
     &    ibci /= 0 .or. ibcj /= 0 .or. ihmi /= 0 .or. ihmj /= 0 .or.
     &     itmi /= 0 .or. itmj /= 0) then
        
        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR in check_closed_contour allocating'
          print *,'!!! various search, hold and temp arrays.'
          print *,'!!! isni = ',isni,' isnj= ',isnj
          print *,'!!! inci = ',inci,' incj= ',incj
          print *,'!!! ibci = ',ibci,' ibcj= ',ibcj
          print *,'!!! ihmi = ',ihmi,' ihmj= ',ihmj
          print *,'!!! itmi = ',itmi,' itmj= ',itmj
          print *,' '
        endif

        STOP 98
      endif

      closed_contour = 'n'
      xcentval = fxy(ix,jx)
      num_found_contours = 0
      next_contour_ct = 0
      beyond_contour_ct = 0
      num_pts_in_all_contours = 0
      hold_mask_i_loc = 0
      hold_mask_j_loc = 0
      beyond_contour_i = 0
      beyond_contour_j = 0
      point_is_already_in_our_contour  = .false.
      point_is_already_in_beyond_pool  = .false.
      icccret = 0
      isc_count = 0
      plastbar = -999.0
      rlastbar = -999.0

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'*-----------------------------------------------*'
        print *,'* Top of check_closed_contour, ix= ',ix,' jx= ',jx
        print *,'*-----------------------------------------------*'
        print *,' '
        print *,'fxy(ix,jx)= ',fxy(ix,jx),' xcentval= ',xcentval
      endif

c     First, set up the contour intervals that will be used.  In
c     the original version of this code, we used preset 
c     standard intervals (984,988,992,996,1000,1004....).  But upon
c     further review, it was decided that this was too arbitrary.
c     So instead, we consider the found min (max) value to be the 
c     bottom (top) of the list of contour intervals.  In this way,
c     we can clearly specify and screen storms based on the "depth" 
c     of the pressure field as compared to the surroundings.

      i = 1
      do while (i <= maxconts)
        if (cmaxmin == 'min') then
          contvals(i) = xcentval + float(i-1)*trkrinfo%contint 
          i = i + 1
        else
          iflip = maxconts - i + 1
          contvals(iflip) = xcentval - float(i-1)*trkrinfo%contint
          i = i + 1
        endif
      enddo

c     This successive_contours loop is the master loop....

      successive_contours_loop: do while (num_found_contours <
     &     num_requested_contours)

c       Find the contour interval in which the center value resides.
c       Note that the lower bound is included for a min check, while
c       the upper bound is included for a max check.  Note also that
c       this subroutine can be used to find the last closed contour,
c       and part of that functionality shows up in the next while 
c       statement where we reference "num_found_contours" in the 
c       array indeces for the contour values.  Basically, the way we
c       do this is, for example, if our central value is 990.4 mb and 
c       our contour interval is 4 mb, then in the first run through 
c       successive_contours_loop we see if we have a closed contour in
c       the interval 990.4-994.4.  If yes, then the next time through 
c       this loop, we see if we have a closed contour in the interval 
c       994.4-998.4.  If yes, then the next loop check is for 998.4-
c       1002.4, and so on....  We stop searching if we find a value 
c       that is either below the xcentval input into this subroutine
c       or below the lower value of the current contour interval (this
c       would mean a change in the gradient and would indicate that, 
c       in the case of mslp, we are heading down towards another,
c       different low).

        isc_count = isc_count + 1

        point_is_already_in_next_contour = .false.

        i = 1
        do while (i < maxconts)
          if (cmaxmin == 'min') then
            if (contvals(i) <= xcentval .and. xcentval < contvals(i+1)) 
     &      then

              if ( verb .ge. 3 ) then
                print *,'At A, num_found_contours= ',num_found_contours
              endif

              contlo = contvals(i+num_found_contours)
              conthi = contvals(i+1+num_found_contours)

              if ( verb .ge. 3 ) then
                print *,'At A, contlo= ',contlo,' conthi= ',conthi
              endif
              exit

            endif
          else
            if (contvals(i) < xcentval .and. xcentval <= contvals(i+1)) 
     &      then
              contlo = contvals(i-num_found_contours)
              conthi = contvals(i-num_found_contours+1)
              exit
            endif
          endif
          i = i + 1
        enddo

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'num_found_contours= ',num_found_contours
          print *,'contlo= ',contlo,' conthi= ',conthi
          print *,'xcentval= ',xcentval
        endif


c       This single_contour_scan_loop is the main loop for searching 
c       for one individual contour.  If it is determined that a contour
c       exists, control is returned to the successive_contours_loop,
c       and if more contours were requested to be found, then the 
c       search continues onward & outward....

        temp_mask_i_loc = 0
        temp_mask_j_loc = 0

        iter  = 1
        num_pts_in_one_contour = 0
        still_scanning = .true.

        rlast_distsum = 0.0
        rlast_distct  = 0

        single_contour_scan_loop: do while (still_scanning)

c          print *,' '
c          print *,'    top of single contour scan loop'
c          print *,'+++ iter= ',iter
c          print *,'    N1: next_contour_ct= ',next_contour_ct

          if (iter == 1 .and. num_found_contours == 0) then 
            ! For the first iteration, we have only the first ring, 
            ! which is centered on the input minimum/maximum point.
            ringct = 1
            search_next_i(1) = ix 
            search_next_j(1) = jx 

c            point_is_already_in_our_contour(ix,jx) = .true.
c            num_pts_in_one_contour = num_pts_in_one_contour + 1
c            temp_mask_i_loc(num_pts_in_one_contour) = ix
c            temp_mask_j_loc(num_pts_in_one_contour) = jx

          else if (iter == 1 .and. num_found_contours > 0) then
            ! This is the first iteration in a *new* contour.
            ! That is, we have already found 1 or more previous
            ! contours while in previous iterations of 
            ! successive_contours_loop and we are now beginning 
            ! to look for the next contour.

c            print *,'    N2: next_contour_ct= ',next_contour_ct

            if (next_contour_ct == 0) then
              ! This would be for the special case in which, for
              ! example, you've got a very intense, compact storm
              ! that "skips" a contour.  That is, suppose the 
              ! min pressure of a storm is 982 mb, and we are 
              ! utilizing a 4-mb contour interval, but all 
              ! surrounding data points are, say, 987 mb or 
              ! higher.  Then, next_contour_ct would be 0 since no
              ! data points were found in the next contour interval
              ! of 982-986 mb, but we can continue searching since the 
              ! gradient is still sloping the correct way.  The code in
              ! this if statement handles this special case.
              
              if ( verb .ge. 3 ) then
                print *,' '
                print *,'ALERT: next_contour_ct = 0 '
              endif

              if (cmaxmin == 'min') then
                contlo_next = conthi
                conthi_next = conthi + trkrinfo%contint

c                print *,'b4 ZZ, ringct= ',ringct
c                print *,'at ZZ, bcc= ',beyond_contour_ct
c     &                 ,'contlo_next= ',contlo_next
c     &                 ,'conthi_next= ',conthi_next

                bey_con_min_loop: do nb = 1,beyond_contour_ct

                  ibx = beyond_contour_i(nb)
                  jby = beyond_contour_j(nb)

                  if (.not. point_is_already_in_beyond_pool(ibx,jby))
     &            then
                    ! If this point is no longer in our pool of "beyond
                    ! contour" points, then just cycle out of this
                    ! iteration....
                    cycle bey_con_min_loop
                  endif

c                  print *,'-- ZZ, ibx= ',ibx,' jby= ',jby
c     &                   ,' fxy(ibx,jby)= ',fxy(ibx,jby)

                  if (fxy(ibx,jby) >= contlo_next .and.
     &                fxy(ibx,jby) <  conthi_next) then

c                    print *,'>> ZZ HIT!!, ibx= ',ibx,' jby= ',jby
c
c                    print *,' +++ BEYOND in NEXT: i= ',ibx,' j= ',jby
c     &                     ,' fxy= ',fxy(ibx,jby)

                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = ibx
                    next_contour_j(next_contour_ct) = jby

                    ! This point has now been identified as being in 
                    ! the "next" contour interval, i.e., no longer in
                    ! the "beyond" contour pool.  Therefore, set the 
                    ! logical flag to indicate that this point is no
                    ! longer in the "beyond" contour pool.

                    point_is_already_in_beyond_pool(ibx,jby) = .false.

                  endif

c                  print *,'.. ZZ, next_contour_ct= ',next_contour_ct

                enddo bey_con_min_loop
              else
                contlo_next = contlo - trkrinfo%contint
                conthi_next = contlo

c                print *,'At A, beyond_contour_ct= ',beyond_contour_ct
c                print *,'  contlo_next = ',contlo_next
c                print *,'  conthi_next = ',conthi_next

                bey_con_max_loop: do nb = 1,beyond_contour_ct

c                  print *,'in bey_con_max_loop, nb= ',nb

                  ibx = beyond_contour_i(nb)
                  jby = beyond_contour_j(nb)

                  if (.not. point_is_already_in_beyond_pool(ibx,jby))
     &            then     
                    ! If this point is no longer in our pool of "beyond
                    ! contour" points, then just cycle out of this
                    ! iteration....
                    cycle bey_con_max_loop
                  endif

c                  print *,'ibx= ',ibx,' jby= ',jby,' data= '
c     &                   ,fxy(ibx,jby)

                  if (fxy(ibx,jby) >  contlo_next .and.
     &                fxy(ibx,jby) <= conthi_next) then

                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = ibx
                    next_contour_j(next_contour_ct) = jby

c                    print *,' ++ HIT! ibx= ',ibx,' jby= ',jby

                    ! This point has now been identified as being in
                    ! the "next" contour interval, i.e., no longer in
                    ! the "beyond" contour pool.  Therefore, set the
                    ! logical flag to indicate that this point is no
                    ! longer in the "beyond" contour pool.

                    point_is_already_in_beyond_pool(ibx,jby) = .false.

                  endif
                enddo bey_con_max_loop
              endif

              if (next_contour_ct > 0) then
                ringct = next_contour_ct
              else

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! XXX next_contour_ct not > 0 !!!'
                  print *,'next_contour_ct= ',next_contour_ct
                  print *,'beyond_contour_ct= ',beyond_contour_ct
                  print *,'ringct= ',ringct
                  print *,'next_ring_ct= ',next_ring_ct
                  print *,'cycling to top of successive_contours_loop..'
                  print *,' '
                endif

                ! The number of rings that we have available to search
                ! in the next contour interval is 0, so cycle all the 
                ! way back to the top of the outer loop, which is
                ! successive_contours_loop, so that we can increase the
                ! contour bounds and search inside those new bounds.
                ! Again, this is for the case in which we have an 
                ! intense, compact storm and we are using a small 
                ! contour interval, such that we are essentially 
                ! "skipping" over one of these intervals in one of the
                ! loop iterations.   We need to bump up the
                ! num_found_contours by one in order to increase the
                ! array index in the contvals array at the top of the
                ! successive_contours_loop.  It is kosher to do this
                ! since the reason we are cycling back to the top of
                ! that loop is that we are skipping over a contour
                ! interval.

                num_found_contours = num_found_contours + 1
                cycle successive_contours_loop

              endif

            else

              ringct = next_contour_ct

            endif

            do nring = 1,ringct
              search_next_i(nring) = next_contour_i(nring)
              search_next_j(nring) = next_contour_j(nring)
c              print *,'at A, nring= ',nring,' next_contour_i(nring)= '
c     &               ,next_contour_i(nring),' next_contour_j(nring)= '
c     &               ,next_contour_j(nring)
            enddo

            next_contour_ct = 0

          else
            ringct = next_ring_ct 
          endif

          if (allocated(ringposi)) deallocate (ringposi)
          if (allocated(ringposj)) deallocate (ringposj)
          allocate (ringposi(ringct),stat=iria)
          allocate (ringposj(ringct),stat=irja)
          if (iria /= 0 .or. irja /= 0) then

            if ( verb .ge. 1 ) then
              print *,' '
              print *,'!!! ERROR in check_closed_contour allocating'
              print *,'!!! various ring arrays.  iria = ',iria
              print *,'!!! irja = ',irja
              print *,' '
            endif

            STOP 98
          endif

ctm
c          print *,' '
c          print *,'ringct= ',ringct

          do nring = 1,ringct
            ringposi(nring) = search_next_i(nring)
            ringposj(nring) = search_next_j(nring)
ctm
c            print *,'nring= ',nring,' ringposi= ',ringposi(nring)
c     &                             ,' ringposj= ',ringposj(nring)
          enddo

          next_ring_ct = 0

          ! This next loop reviews the points that have been 
          ! labelled for the "beyond_contour" pool.  As we get further
          ! into successive iterations of successive_contours_loop, 
          ! some of these previously "beyond" points are now within 
          ! the contour interval range that we are checking, so we 
          ! need to go through the list of "beyond" points and remove
          ! any that are no longer in that "beyond" category....

          check_beyond_loop: do nb = 1,beyond_contour_ct

            ibx = beyond_contour_i(nb)
            jby = beyond_contour_j(nb)

            if (.not. point_is_already_in_beyond_pool(ibx,jby))
     &      then
              ! This point may have been removed already in a 
              ! previous iteration of successive_contours_loop.
              ! If this point is no longer in our pool of "beyond
              ! contour" points, then just cycle out of this
              ! iteration....
              cycle check_beyond_loop
            endif

            ! Check to see if any of the points being searched in the
            ! upcoming multiple_ring_loop are points that had previously
            ! been saved as "beyond_contour" points.  If so, remove 
            ! their status as "beyond_contour" points by setting the 
            ! logical flag to false.

            do nring = 1,ringct

              if (ibx == ringposi(nring) .and. jby == ringposj(nring))
     &        then
c                print *,' '
c                print *,'!!! beyond remove: ibx= ',ibx,' jby= ',jby
                point_is_already_in_beyond_pool(ibx,jby) = .false.
              endif

            enddo

          enddo check_beyond_loop


c         In each iteration of single_contour_scan_loop, we can have a
c         different number of rings to analyze.  In the first
c         iteration, we only have 1 ring, the initial ring around the
c         local max/min that was input to this subroutine.  Subsequent
c         iterations will have a variable number of rings, depending on
c         how many new data points within our contour interval were 
c         found in the previous iteration.

          multiple_ring_loop: do mr = 1,ringct

            icenx = ringposi(mr)
            jcenx = ringposj(mr)

ctm
c            print *,'  --- iter= ',iter,' mr= ',mr,' icenx= ',icenx
c     &             ,' jcenx= ',jcenx,' imax= ',imax,' jmax= ',jmax

            call get_ijplus1_check_wrap (imax,jmax,icenx,jcenx,ixp1,jxp1
     &                                  ,ixm1,jxm1,trkrinfo,igicwret)

            if (igicwret /= 0) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'!!! NO CLOSED CONTOUR: The call to '
                print *,'!!! get_ijplus1_check_wrap indicates the'
                print *,'!!! max/min contour extends past the edge of'
                print *,'!!! our regional grid. '
                print *,' '
                print *,' '
              endif


              do nm = 1,num_pts_in_all_contours
                im = hold_mask_i_loc(nm)
                jm = hold_mask_j_loc(nm)
                masked_out(im,jm) = .true.
              enddo

              deallocate (ringposi); deallocate (ringposj)
              deallocate (search_next_i); deallocate (search_next_j)
              deallocate (next_contour_i); deallocate (next_contour_j)
              deallocate (beyond_contour_i) 
              deallocate (beyond_contour_j)
              deallocate (hold_mask_i_loc) 
              deallocate (hold_mask_j_loc)
              deallocate (temp_mask_i_loc) 
              deallocate (temp_mask_j_loc)
              icccret = 0
              return
            endif

c           For each individual ring, we check all 8 points surrounding
c           the center point.  The points are numbered for each ring as
c           shown in the diagram to the right of the "select case" 
c           statement just below.  REMEMBER: The j in our grids 
c           increases from north to south, so that for a global grid,
c           j=1 is at 90N and j=jmax is at 90S.

            individual_ring_loop: do ir = 1,9

              select case (ir)
                case (1); irx=ixm1; jrx=jcenx;!     2       3       4 
                case (2); irx=ixm1; jrx=jxm1; !                      
                case (3); irx=icenx;jrx=jxm1; !                     
                case (4); irx=ixp1; jrx=jxm1; !     1 (icenx,jcenx) 5
                case (5); irx=ixp1; jrx=jcenx;!                     
                case (6); irx=ixp1; jrx=jxp1; !                     
                case (7); irx=icenx;jrx=jxp1; !     8       7       6
                case (8); irx=ixm1; jrx=jxp1; !                     
                case (9); irx=icenx; jrx=jcenx; ! = center pt of ring
              end select

c             Make sure the point we are looking at has valid data.  
c             This is an issue only on regional grids, where we have a
c             buffer of bitmapped (null) data points surrounding the 
c             real grid.

c             print *,'ind ring loop: ir= ',ir,' irx= ',irx,' jrx= ',jrx

              if (.not. valid_pt(irx,jrx)) then
                
                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! NOTE: In check_closed_contour, while '
                  print *,'!!!    checking points around (icenx,jcenx)='
                  print *,'!!!    (',icenx,',',jcenx,'), we hit a non-'
                  print *,'!!!    valid point, meaning we are near the'
                  print *,'!!!    bounds of the grid, or at least the '
                  print *,'!!!    bounds of the valid data for this '
                  print *,'!!!    grid.  We will skip the'
                  print *,'!!!    search for this center.'
                  print *,'!!! '
                  print *,'!!! (i,j) of non-valid pt = ('
     &                   ,irx,',',jrx,')'
                  print *,'!!! '
                endif

                do nm = 1,num_pts_in_all_contours
                  im = hold_mask_i_loc(nm)
                  jm = hold_mask_j_loc(nm)
                  masked_out(im,jm) = .true.
                enddo

                deallocate (ringposi); deallocate (ringposj)
                deallocate (search_next_i); deallocate (search_next_j)
                deallocate (next_contour_i); deallocate (next_contour_j)
                deallocate (beyond_contour_i)
                deallocate (beyond_contour_j)
                deallocate (hold_mask_i_loc)
                deallocate (hold_mask_j_loc)
                deallocate (temp_mask_i_loc)
                deallocate (temp_mask_j_loc)
                icccret = 0
                return
              endif

c             Check to make sure that the point we are looking at is
c             not considered under the influence of another nearby low.

              if (masked_out(irx,jrx)) then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! NOTE: In check_closed_contour, while '
                  print *,'!!!    checking points around (icenx,jcenx)='
                  print *,'!!!    (',icenx,',',jcenx,'), we hit a point'
                  print *,'!!!    that has been masked out, meaning it'
                  print *,'!!!    belongs under the influence of '
                  print *,'!!!    another nearby low, so we will skip'
                  print *,'!!!    the search for this center....'
                  print *,'!!!  '
                  print *,'!!! Min central value      = ',xcentval
                  print *,'!!! (i,j) of central value = (',ix,',',jx,')'
                  print *,'!!!  '
                  print *,'!!! Masked-out value found = ',fxy(irx,jrx)
                  print *,'!!! (i,j) of masked value  = (',irx,','
     &                 ,jrx,')'
                  print *,'!!!  '
                  print *,'!!! Lower bound of contour interval = '
     &                   ,contlo
                  print *,'!!! Upper bound of contour interval = '
     &                   ,conthi
                  print *,'!!! Contour interval = ',trkrinfo%contint
                  print *,'!!!    '
                  print *,'!!! closed_contour flag = ',closed_contour
                  print *,'!!!    '
                endif

                do nm = 1,num_pts_in_all_contours
                  im = hold_mask_i_loc(nm)
                  jm = hold_mask_j_loc(nm)
                  masked_out(im,jm) = .true.
                enddo

                deallocate (ringposi); deallocate (ringposj)
                deallocate (search_next_i); deallocate (search_next_j)
                deallocate (next_contour_i); deallocate (next_contour_j)
                deallocate (beyond_contour_i)
                deallocate (beyond_contour_j)
                deallocate (hold_mask_i_loc)
                deallocate (hold_mask_j_loc)
                deallocate (temp_mask_i_loc)
                deallocate (temp_mask_j_loc)
                icccret = 0
                return
              endif

c             If we have already hit this point on a previous ring 
c             check, then just ignore this point and cycle past it.

              if (point_is_already_in_our_contour(irx,jrx)) then
ctm
c                print *,' '
c                print *,'Pt. AAA, already-in-contour.....'
c                print *,'irx= ',irx,' jrx= ',jrx
                cycle individual_ring_loop
              endif

c             For a MIN check, check to see if the data point is below 
c             the contour interval or is below the local minimum value 
c             passed into this subroutine.  In either case, exit and 
c             consider this to NOT be a closed contour.
c             For a MAX check, check to see if the data point is above 
c             the contour interval or is above the local maximum value 
c             passed into this subroutine.  In either case, exit and 
c             consider this to NOT be a closed contour.
c             
c             For example, for mslp, this would be as we're moving 
c             outward away from lower pressures to higher pressures,
c             and then all of a sudden we come upon a lower pressure.
c             This probably means we're heading toward another low
c             pressure area, so mark the point and return to the 
c             calling routine.

              found_a_point_below_contour = 'n'
              found_a_point_above_contour = 'n'
              if (cmaxmin == 'min') then
                if (fxy(irx,jrx) < xcentval .or. fxy(irx,jrx) < contlo)
     &          then
                  found_a_point_below_contour = 'y'
                endif
              else 
                if (fxy(irx,jrx) > xcentval .or. fxy(irx,jrx) > conthi)
     &          then
                  found_a_point_above_contour = 'y'
                endif
              endif

              if (found_a_point_below_contour == 'y' .or.
     &            found_a_point_above_contour == 'y') then

                if ( verb .ge. 3 ) then
                  print *,' '
                  print *,'!!! NOTE: In check_closed_contour, while '
                  print *,'!!!    checking points around (icenx,jcenx)='
                  print *,'!!!    (',icenx,',',jcenx,'), we hit a data'
                  print *,'!!!    value that is less (greater) than the'
                  print *,'!!!    current contour interval bound for a'
                  print *,'!!!    min (max) and/or is less (greater) '
                  print *,'!!!    than the minimum (maximum) central '
                  print *,'!!!    value that we are centering the '
                  print *,'!!!    search on.'
                  print *,'!!!    '
                  print *,'!!! Central value      = ',xcentval
                  print *,'!!! (i,j) of central value = (',ix,',',jx,')'
                  print *,'!!!  '
                  print *,'!!! Flagged value found    = ',fxy(irx,jrx)
                  print *,'!!! (i,j) of flagged value = (',irx,','
     &                 ,jrx,')'
                  print *,'!!!   '
                  print *,'!!! Lower bound of contour interval = '
     &                   ,contlo
                  print *,'!!! Upper bound of contour interval = '
     &                   ,conthi
                  print *,'!!! Contour interval = ',trkrinfo%contint
                  print *,'!!!    '
                  print *,'!!! closed_contour flag = ',closed_contour
                  print *,'!!! ' 
                endif

                do nm = 1,num_pts_in_all_contours
                  im = hold_mask_i_loc(nm)
                  jm = hold_mask_j_loc(nm)
                  masked_out(im,jm) = .true.
                enddo

                deallocate (ringposi); deallocate (ringposj)
                deallocate (search_next_i); deallocate (search_next_j)
                deallocate (next_contour_i); deallocate (next_contour_j)
                deallocate (beyond_contour_i)
                deallocate (beyond_contour_j)
                deallocate (hold_mask_i_loc)
                deallocate (hold_mask_j_loc)
                deallocate (temp_mask_i_loc)
                deallocate (temp_mask_j_loc)
                icccret = 0
                return
              endif 

c             If we've made it this far, then we at least know that the
c             gradient is still heading in the right direction.  Do the
c             check now to see if the value at this point is within our
c             specific contour interval (there is the possibility that
c             the value is beyond our interval, which will be checked
c             for just below, and if that's the case, then that point
c             will be processed in a subsequent iteration of this loop
c             that encompasses that correct contour interval).

              found_a_point_in_our_contour = 'n'
              if (cmaxmin == 'min') then
                if (fxy(irx,jrx) >= contlo .and. fxy(irx,jrx) < conthi)
     &          then
                  found_a_point_in_our_contour = 'y'
                endif
              else
                if (fxy(irx,jrx) > contlo .and. fxy(irx,jrx) <= conthi) 
     &          then
                  found_a_point_in_our_contour = 'y'
                endif
              endif

              if (found_a_point_in_our_contour == 'y') then
                ! We've found a data point in our interval, something 
                ! that is inside the closed contour, and it hasn't been
                ! marked as being found in a previous iteration of this 
                ! loop, so mark it now and store the (i,j) location so 
                ! that we can scan a ring around this point in a 
                ! successive iteration of this loop for more potential 
                ! points within this interval...

                point_is_already_in_our_contour(irx,jrx) = .true.

                next_ring_ct = next_ring_ct + 1
                search_next_i(next_ring_ct) = irx
                search_next_j(next_ring_ct) = jrx

c                print *,'at B, next_ring_ct= ',next_ring_ct
c     &               ,' search_next_i()= ',search_next_i(next_ring_ct)
c     &               ,' search_next_j()= ',search_next_j(next_ring_ct)

                num_pts_in_one_contour = num_pts_in_one_contour + 1
                temp_mask_i_loc(num_pts_in_one_contour) = irx
                temp_mask_j_loc(num_pts_in_one_contour) = jrx

                if (get_last_isobar_flag == 'y') then
                  call calcdist (glon(ix),glat(jx)
     &                          ,glon(irx),glat(jrx),dist,degrees)
                  rlast_distsum = rlast_distsum + dist
                  rlast_distct  = rlast_distct + 1
                endif

ctm
c                print *,' '
c                print *,' PT IN! irx= ',irx,' jrx= ',jrx,' xval= '
c     &                 ,fxy(irx,jrx)
c                print *,'next_ring_ct= ',next_ring_ct
c                print *,'num_pts_in_one_contour= '
c     &                 ,num_pts_in_one_contour
              endif

c             If we've made it this far AND the 
c             found_a_point_in_our_contour flag indicates that this
c             point is not in our contour interval, then by default that
c             means that this point is for a contour interval beyond 
c             what we're currently looking at.  E.g., if we're looking 
c             at the contours around a 972 mb low and we're moving 
c             outward and currently checking the 984-988 mb contour 
c             interval, it means that we found, say, a gridpoint with 
c             991 mb.  So we want to mark that point for a future 
c             iteration of this loop that would be checking the 
c             988-992 mb contour interval.

              if (found_a_point_in_our_contour /= 'y' .and. 
     &            .not. point_is_already_in_next_contour(irx,jrx)) then
                ! We've found a data point that is beyond our interval,
                ! so this is not a concern for finding the bounds of 
                ! our current contour interval, but we want to mark 
                ! these points and remember them for the next iteration
                ! of successive_scan_loop.  (For example, suppose we 
                ! are currently searching for points in the 984-988 mb
                ! range, and we find a point that is 990 -- mark it 
                ! here to be remembered when we scan for 988-992 mb).
                if (cmaxmin == 'min') then
                  contlo_next = conthi
                  conthi_next = conthi + trkrinfo%contint
                  if (fxy(irx,jrx) >= contlo_next .and. 
     &                fxy(irx,jrx) <  conthi_next) then
                    ! "NEXT_CONTOUR" Comment:
                    ! We've found a point that is in the very next
                    ! contour interval....
                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = irx
                    next_contour_j(next_contour_ct) = jrx
                    point_is_already_in_next_contour(irx,jrx) = .true.
                  else if (fxy(irx,jrx) >= conthi_next) then
                    ! "BEYOND_CONTOUR" Comment:
                    ! This point is at least 1 contour interval beyond
                    ! the next contour interval.  Dump the info into 
                    ! these i and j arrays.  This info will be used if
                    ! in the next iteration of single_contour_scan_loop,
                    ! next_contour_ct = 0.  That would mean that we 
                    ! have, e.g., an intensely deep low with a sharp
                    ! mslp gradient that essentially "skips" over a 
                    ! contour interval.  E.g., if using a 4 mb interval,
                    ! we go from 947 to 953 AND there are NO
                    ! intervening gridpoints in the 948-952 interval.
                    beyond_contour_ct = beyond_contour_ct + 1
                    beyond_contour_i(beyond_contour_ct) = irx
                    beyond_contour_j(beyond_contour_ct) = jrx
                    point_is_already_in_beyond_pool(irx,jrx) = .true.
c                    print *,'bcc= ',beyond_contour_ct
c     &                     ,'beyond_contour_i()= '
c     &                     ,beyond_contour_i(beyond_contour_ct)
c     &                     ,'beyond_contour_j()= '
c     &                     ,beyond_contour_j(beyond_contour_ct)
                  endif
                else
                  contlo_next = contlo - trkrinfo%contint
                  conthi_next = contlo
                  if (fxy(irx,jrx) >  contlo_next .and. 
     &                fxy(irx,jrx) <= conthi_next) then
                    ! See "NEXT_CONTOUR" comment just above....
                    next_contour_ct = next_contour_ct + 1
                    next_contour_i(next_contour_ct) = irx
                    next_contour_j(next_contour_ct) = jrx
                    point_is_already_in_next_contour(irx,jrx) = .true.
c                    print *,'NEXT ncc= ',next_contour_ct
c     &                     ,'next_contour_i()= '
c     &                     ,next_contour_i(next_contour_ct)
c     &                     ,'next_contour_j()= '
c     &                     ,next_contour_j(next_contour_ct)
c     &                     ,' fxy= ',fxy(irx,jrx)
                  else if (fxy(irx,jrx) <= contlo_next) then
                    ! See "BEYOND_CONTOUR" comment just above....
                    beyond_contour_ct = beyond_contour_ct + 1
                    beyond_contour_i(beyond_contour_ct) = irx
                    beyond_contour_j(beyond_contour_ct) = jrx
                    point_is_already_in_beyond_pool(irx,jrx) = .true.
c                    print *,'BEYOND bcc= ',beyond_contour_ct
c     &                     ,'beyond_contour_i()= '
c     &                     ,beyond_contour_i(beyond_contour_ct)
c     &                     ,'beyond_contour_j()= '
c     &                     ,beyond_contour_j(beyond_contour_ct)
c     &                     ,' fxy= ',fxy(irx,jrx)
                  endif
                endif
              endif 
               
            enddo individual_ring_loop

          enddo multiple_ring_loop

          if (next_ring_ct > 0) then
            iter = iter + 1
          else
            icccret = 0
            still_scanning = .false.
            if (allocated(ringposi)) deallocate (ringposi)
            if (allocated(ringposj)) deallocate (ringposj)
            num_found_contours = num_found_contours + 1
            closed_contour = 'y'
            if (num_found_contours == 1) then

              if ( verb .ge. 3 ) then
                print *,' '
                print *,'+++ Closed contour found '
              endif

            endif
          endif

        enddo single_contour_scan_loop

        do insingle = 1,num_pts_in_one_contour
          num_pts_in_all_contours = num_pts_in_all_contours + 1
          inall = num_pts_in_all_contours
          hold_mask_i_loc(inall) = temp_mask_i_loc(insingle) 
          hold_mask_j_loc(inall) = temp_mask_j_loc(insingle) 
        enddo

        if (get_last_isobar_flag == 'y') then
          if (cmaxmin == 'min') then
            plastbar = conthi
          else
            plastbar = contlo
          endif
          if (rlast_distct > 0) then
            rlastbar = rlast_distsum / float(rlast_distct)
            rlastbar = rlastbar * 0.539638  ! convert km to nm
          else            
            rlastbar = -999.0
          endif           
        endif

      enddo successive_contours_loop

      if ( verb .ge. 3 ) then
        print *,' '
        print *,'END SUM: num of iterations = ',isc_count
      endif

      do nm = 1,num_pts_in_all_contours
        im = hold_mask_i_loc(nm)
        jm = hold_mask_j_loc(nm)
        masked_out(im,jm) = .true.
      enddo

      if (allocated(search_next_i))    deallocate (search_next_i)
      if (allocated(search_next_j))    deallocate (search_next_j)
      if (allocated(next_contour_i))   deallocate (next_contour_i)
      if (allocated(next_contour_j))   deallocate (next_contour_j)
      if (allocated(beyond_contour_i)) deallocate (beyond_contour_i)
      if (allocated(beyond_contour_j)) deallocate (beyond_contour_j)
      if (allocated(hold_mask_i_loc))  deallocate (hold_mask_i_loc)
      if (allocated(hold_mask_j_loc))  deallocate (hold_mask_j_loc)
      if (allocated(temp_mask_i_loc))  deallocate (temp_mask_i_loc)
      if (allocated(temp_mask_j_loc))  deallocate (temp_mask_j_loc)
      if (allocated(ringposi))         deallocate (ringposi)
      if (allocated(ringposj))         deallocate (ringposj)
c
      return
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine check_land_mask (imax,jmax,ix,jx,fract_land,valid_pt
     &                            ,dx,dy,point_is_over_water,iclmret)
c
c     ABSTRACT: This subroutine looks at the values for the land-sea 
c     mask surrounding an input (i,j) position to determine if less 
c     than 50% of the area surrounding the input (i,j) position within
c     75 km radius is land.
c
c     INPUT:
c     imax     Num pts in i-direction on grid
c     jmax     Num pts in j-direction on grid
c     ix       i index for location of local max or min
c     jx       j index for location of local max or min
c     valid_pt Logical; bitmap indicating if valid data at that pt
c     dx       Grid spacing in x-direction
c     dy       Grid spacing in y-direction
c
c     OUTPUT:
c     fract_land  Fraction of points/area that is covered by land
c     point_is_over_water  y/n: A value of 'y' is returned if <50%
c                          of the points/area is covered by land
c     iclmret  Return code from this routine
c
      USE grid_bounds; USE tracked_parms
      USE trkrparms; USE verbose_output

      implicit none

      type (trackstuff) trkrinfo

      integer date_time(8)
      character (len=10) big_ben(3)

      logical(1) valid_pt(imax,jmax)
      character  point_is_over_water*1
      integer, parameter :: numazim=8
      integer  iazim,ibiret1,imax,jmax,ix,jx,iclmret,imct,bimct
      real     bear,targlat,targlon,xplon,yplat,rdist,xintrp_mask
      real     fract_land,dx,dy,xmask_sum
c
      iclmret = 0

c     First, calculate the longitude and latitude of the input ix and
c     jx points.  If the xplon value ends up being >360.0 (this can 
c     happen for basin-scale HWRF), don't worry about it.  Just leave
c     it be, as the trigonometry will work out the same for lons >360.

      xplon = glonmin + (ix-1)*dx
      yplat = glatmax - (jx-1)*dy

      rdist = 75.0  ! (We will always look only 75 km radius out for 
                    !  this particular land-sea mask application)

      imct = 0

c     Now go around the storm via azimloop and get interpolated 
c     values of the land-sea mask at each azimuth at a radial 
c     distance of 75 km from the center point....

      xmask_sum = 0.0

      bimct = 0

      azimloop: do iazim = 1,numazim

        bear = ((iazim-1) * 15.) + 45.0

        call distbear (yplat,xplon,rdist,bear,targlat,targlon)

        ! These calls to bilin_int_uneven pass a variable, level,
        ! that is used for applications of interpolating wind 
        ! data.  Here, we are instead interpolating the land-sea
        ! mask data, so we don't care about the level, so just 
        ! pass a dummy value of 850, which never gets used.

        call bilin_int_uneven (targlat,targlon
     &      ,dx,dy,imax,jmax,trkrinfo,850,'m',xintrp_mask
     &            ,valid_pt,bimct,ibiret1)

        if (ibiret1 == 0) then
          xmask_sum = xmask_sum + xintrp_mask
          imct = imct + 1
        else
          iclmret = 95
          return
        endif

      enddo azimloop

      if (bimct > 0) then
        if (verb .ge. 3) then
          print *,' '
          print *,'Warning summary: From sub check_land_mask,'
          print *,'calls to sub bilin_int_uneven resulted in'
          print *,'(blocked) attempts to access points outside the'
          print *,'bounds of a regional grid.  Total # of access'
          print *,'attempts for this call in azimloop= ',bimct
          print *,' '
        endif
      endif

c     Now get the mask value directly at the point that was input to 
c     this routine....

      if (valid_pt(ix,jx)) then
        xmask_sum = xmask_sum + lsmask(ix,jx)
        imct = imct + 1
      endif

c     Now get the mean land fraction....

      if (imct > 0) then

        fract_land = xmask_sum / float(imct)
        if (fract_land < 0.50) then
          point_is_over_water = 'y'
          if ( verb .ge. 3 ) then
            print *,' '
            print *,'+++ Land check yes: Point is over water. '
            print *,'    Land check value: fract_land= ',fract_land
          endif
        else
          point_is_over_water = 'n'
          if ( verb .ge. 3 ) then
            print *,' '
            print *,'!!! Land check NO: Point is over land. '
            print *,'    Land check value: fract_land= ',fract_land
          endif
        endif

      else

        if ( verb .ge. 3 ) then
          print *,' '
          print *,'!!! ERROR: Land check: imct = 0, which means no'
          print *,'    valid points were found to do the check.'
          print *,'    ix= ',ix,' jx= ',jx 
        endif
        iclmret = 95
        return

      endif
c
      return 
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine get_ijplus1_check_wrap (imax,jmax,i,j,iplus1,jplus1
     &                            ,iminus1,jminus1,trkrinfo,igicwret)
c
c     ABSTRACT: This subroutine takes an (i,j) position input and 
c     returns the four neighboring (i,j) points to the east, south, 
c     west and north.  The routine checks for wrap around the GM, so 
c     that if, for example, you are on a global 360x181 grid and you
c     are at point i=360, then i+1 = 361, so you need something to 
c     adjust that back to i = 1.  Likewise, if you are at i=1 and 
c     looking for point i-1, it will adjust it to be point 360 
c     instead of the meaningless point 0 (i=0).

      USE trkrparms
      USE verbose_output

      implicit none

      type (trackstuff) trkrinfo
      integer   i,j,imax,jmax,iplus1,jplus1,iminus1,jminus1,igicwret

      igicwret = 0

      jplus1  = j + 1
      jminus1 = j - 1
      iplus1  = i + 1
      iminus1 = i - 1

      if (iplus1 > imax) then
        if (trkrinfo%gridtype == 'global') then
          iplus1 = iplus1 - imax   ! If wrapping east of GM
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: Error in get_ijplus1_check_wrap.  The'
            print *,'!!!        user-requested eastern search boundary'
            print *,'!!!        is too close to the eastern bounds of'
            print *,'!!!        this regional grid.  When we check '
            print *,'!!!        neighboring points, we are going past'
            print *,'!!!        the edge of the grid by one point. '
            print *,'!!!        Cut back your requested eastern  '
            print *,'!!!        boundary by a degree or 2 in the  '
            print *,'!!!        script and resubmit....'
            print *,'!!!         '
            print *,'!!!   imax of regional grid    = ',imax
            print *,'!!!   User-requested eastern i = ',iplus1
            print *,' '
          endif

          igicwret = 98
          return
        endif
      endif

      if (iminus1 < 1) then
        if (trkrinfo%gridtype == 'global') then
          iminus1 = imax + iminus1  ! If wrapping west of GM
        else

          if ( verb .ge. 1 ) then
            print *,' '
            print *,'!!! ERROR: Error in get_ijplus1_check_wrap.  The'
            print *,'!!!        user-requested western search boundary'
            print *,'!!!        is too close to the western bounds of'
            print *,'!!!        this regional grid.  When we check '
            print *,'!!!        neighboring points, we are going past'
            print *,'!!!        the edge of the grid by one point. '
            print *,'!!!        Cut back your requested western  '
            print *,'!!!        boundary by a degree or 2 in the  '
            print *,'!!!        script and resubmit....'
            print *,'!!!         '
            print *,'!!!   User-requested western i = ',iminus1
            print *,' '
          endif

          igicwret = 98
          return
        endif
      endif

      if (jplus1 > jmax .or. jminus1 < 1) then

        if ( verb .ge. 1 ) then
          print *,' '
          print *,'!!! ERROR: Error in get_ijplus1_check_wrap.  The '
          print *,'!!!     user-requested northern or southern search'
          print *,'!!!     boundary is too close to the bounds of the'
          print *,'!!!     grid.  Cut back your requested northern or'
          print *,'!!!     southern boundary by a degree or 2 in the'
          print *,'!!!     script and resubmit....'
          print *,'!!! '
          print *,'!!!   User-requested northern j = ',jminus1
          print *,'!!!   User-requested southern j = ',jplus1
          print *,'!!!   jmax of grid              = ',jmax
          print *,'    '
        endif

        igicwret = 91
        return
      endif
    
      return
      end

c------------------------------------------------------------------
c
c------------------------------------------------------------------
      SUBROUTINE qsort(x,ind,n)
c
c     Code converted using TO_F90 by Alan Miller
c     Date: 2002-12-18  Time: 11:55:47

      IMPLICIT NONE
      INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)

      REAL (dp), INTENT(IN)  :: x(n)
      INTEGER, INTENT(OUT)   :: ind(n)
      INTEGER, INTENT(IN)    :: n

c     ***************************************************************************
c
c                                                              ROBERT RENKA
c                                                      OAK RIDGE NATL. LAB.
c
c        THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO SORT A REAL (dp)
c      ARRAY X INTO INCREASING ORDER.  THE ALGORITHM IS AS FOLLOWS.  IND IS
c      INITIALIZED TO THE ORDERED SEQUENCE OF INDICES 1,...,N, AND ALL INTERCHANGES
c      ARE APPLIED TO IND.  X IS DIVIDED INTO TWO PORTIONS BY PICKING A CENTRAL
c      ELEMENT T.  THE FIRST AND LAST ELEMENTS ARE COMPARED WITH T, AND
c      INTERCHANGES ARE APPLIED AS NECESSARY SO THAT THE THREE VALUES ARE IN
c      ASCENDING ORDER.  INTERCHANGES ARE THEN APPLIED SO THAT ALL ELEMENTS
c      GREATER THAN T ARE IN THE UPPER PORTION OF THE ARRAY AND ALL ELEMENTS
c      LESS THAN T ARE IN THE LOWER PORTION.  THE UPPER AND LOWER INDICES OF ONE
c      OF THE PORTIONS ARE SAVED IN LOCAL ARRAYS, AND THE PROCESS IS REPEATED
c      ITERATIVELY ON THE OTHER PORTION.  WHEN A PORTION IS COMPLETELY SORTED,
c      THE PROCESS BEGINS AGAIN BY RETRIEVING THE INDICES BOUNDING ANOTHER
c      UNSORTED PORTION.
c
c      INPUT PARAMETERS -   N - LENGTH OF THE ARRAY X.
c
c                           X - VECTOR OF LENGTH N TO BE SORTED.
c
c                         IND - VECTOR OF LENGTH >= N.
c
c      N AND X ARE NOT ALTERED BY THIS ROUTINE.
c
c      OUTPUT PARAMETER - IND - SEQUENCE OF INDICES 1,...,N PERMUTED IN THE SAME
c                               FASHION AS X WOULD BE.  THUS, THE ORDERING ON
c                               X IS DEFINED BY Y(I) = X(IND(I)).
c
c     *********************************************************************

      ! NOTE -- IU AND IL MUST BE DIMENSIONED >= LOG(N) WHERE LOG HAS BASE 2.

      !*********************************************************************

      INTEGER   :: iu(21), il(21)
      INTEGER   :: m, i, j, k, l, ij, it, itt, indx
      REAL      :: r
      REAL (dp) :: t

      ! LOCAL PARAMETERS -

      ! IU,IL =  TEMPORARY STORAGE FOR THE UPPER AND LOWER
      !            INDICES OF PORTIONS OF THE ARRAY X
      ! M =      INDEX FOR IU AND IL
      ! I,J =    LOWER AND UPPER INDICES OF A PORTION OF X
      ! K,L =    INDICES IN THE RANGE I,...,J
      ! IJ =     RANDOMLY CHOSEN INDEX BETWEEN I AND J
      ! IT,ITT = TEMPORARY STORAGE FOR INTERCHANGES IN IND
      ! INDX =   TEMPORARY INDEX FOR X
      ! R =      PSEUDO RANDOM NUMBER FOR GENERATING IJ
      ! T =      CENTRAL ELEMENT OF X

      IF (n <= 0) RETURN

      ! INITIALIZE IND, M, I, J, AND R

      DO  i = 1, n
        ind(i) = i
      END DO
      m = 1
      i = 1
      j = n
      r = .375

      ! TOP OF LOOP

   20 IF (i >= j) GO TO 70
      IF (r <= .5898437) THEN
        r = r + .0390625
      ELSE
        r = r - .21875
      END IF

      ! INITIALIZE K

   30 k = i

      ! SELECT A CENTRAL ELEMENT OF X AND SAVE IT IN T

      ij = i + r*(j-i)
      it = ind(ij)
      t = x(it)

      ! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
      !   INTERCHANGE IT WITH T

      indx = ind(i)
      IF (x(indx) > t) THEN
        ind(ij) = indx
        ind(i) = it
        it = indx
        t = x(it)
      END IF

      ! INITIALIZE L

      l = j

      ! IF THE LAST ELEMENT OF THE ARRAY IS LESS THAN T,
      !   INTERCHANGE IT WITH T
      indx = ind(j)
      IF (x(indx) >= t) GO TO 50
      ind(ij) = indx
      ind(j) = it
      it = indx
      t = x(it)

      ! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
      !   INTERCHANGE IT WITH T

      indx = ind(i)
      IF (x(indx) <= t) GO TO 50
      ind(ij) = indx
      ind(i) = it
      it = indx
      t = x(it)
      GO TO 50

      ! INTERCHANGE ELEMENTS K AND L

   40 itt = ind(l)
      ind(l) = ind(k)
      ind(k) = itt

      ! FIND AN ELEMENT IN THE UPPER PART OF THE ARRAY WHICH IS
      !   NOT LARGER THAN T

   50 l = l - 1
      indx = ind(l)
      IF (x(indx) > t) GO TO 50

      ! FIND AN ELEMENT IN THE LOWER PART OF THE ARRAY WHCIH IS NOT SMALLER THAN T

   60 k = k + 1
      indx = ind(k)
      IF (x(indx) < t) GO TO 60

      ! IF K <= L, INTERCHANGE ELEMENTS K AND L

      IF (k <= l) GO TO 40

      ! SAVE THE UPPER AND LOWER SUBSCRIPTS OF THE PORTION OF THE
      !   ARRAY YET TO BE SORTED

      IF (l-i > j-k) THEN
        il(m) = i
        iu(m) = l
        i = k
        m = m + 1
        GO TO 80
      END IF

      il(m) = k
      iu(m) = j
      j = l
      m = m + 1
      GO TO 80


      ! BEGIN AGAIN ON ANOTHER UNSORTED PORTION OF THE ARRAY

   70 m = m - 1
      IF (m == 0) RETURN
      i = il(m)
      j = iu(m)

   80 IF (j-i >= 11) GO TO 30
      IF (i == 1) GO TO 20
      i = i - 1

      ! SORT ELEMENTS I+1,...,J.  NOTE THAT 1 <= I < J AND J-I < 11.

   90 i = i + 1
      IF (i == j) GO TO 70
      indx = ind(i+1)
      t = x(indx)
      it = indx
      indx = ind(i)
      IF (x(indx) <= t) GO TO 90
      k = i

  100 ind(k+1) = ind(k)
      k = k - 1
      indx = ind(k)
      IF (t < x(indx)) GO TO 100

      ind(k+1) = it
      GO TO 90
      END SUBROUTINE qsort
