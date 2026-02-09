C******************************************************************
C
C  BRIEF DESCRIPTION OF PROGRAM MODULES:
C
C   backspaceFile - Repositions the specified file back "numRcrds" records.
C   doWriteAidRcd - Write the aid record to the output data file.
C   getAidDTG - Gets the first aid data for the specified DTG from
C              the input file.
C   getAidTAU - Gets the first A_RECORD record for the specified tau
C              from the supplied AID_DATA.  
C   getARecord - Reads one record of specified type from the input file.
C   getBigAidDTG - Gets all the aid data for the specified DTG from
C                  the input file
C   getSingleTAU - Gets the aid data for the specified tau from the
C              supplied AID_DATA.
C   getTech - Gets data for a specified aid technique from the supplied
C             BIG_AID_DATA structure and returns an AID_DATA structure
C   newWriteAidRcd - Write the aid record to the output data file.
C                    NHC version
C   processArcd - Assigns the data for a A_RECORD structure.
C   readARecord - Reads one AID_DATA data info from the input file.
C   readBest - Read one record from the best track file,
C              including the basin and cyclone number.
C   readBT   - Read one record from the best track file.
C   readNext - Reads the next ATCF_RECORD record from the input file.
C   writeAid - Write the aid record to the output data file.
C   writeAidRcd - Write the aid record to the output data file.
C                JTWC, NPMOC version
C   
C******************************************************************


C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBT
C
C  DESCRIPTION:  Read one record from the best track file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBT (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     cent - century of posit (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C   sampson, nrl    nov 9 98   added cent
C
C........................END PROLOGUE..................................
C
      subroutine readBT (datFile,cent,dtg,flat,ns,flon,ew,iwind,ios)   
c
c         formal parameters
      integer           datFile
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios

      read( datFile, '(8x,a2,a8,17x,f3.1,a1,2x,f4.1,a1,2x,i3)', 
     1     iostat=ios ) cent, dtg, flat, ns, flon, ew, iwind
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readBest
C
C  DESCRIPTION:  Read one record from the best track file.
C                Same as readBT except also reads the basin and 
C                cyclone number.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readBest (datFile, basin, cycnum, cent, dtg, flat, ns, 
C                           flon, ew, iwind, ios )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of best track file
C
C  OUTPUT PARAMETERS:
C     basin - basin read from the best track file
C     cycnum - cyclone num read from the best track file
C     cent - century (2 characters, e.g. 19)
C     dtg - YYMMDDHH, date-time-group read from the best track file
C     flat - latitude read from the best track file
C     ns - one character north/south indicator for latitude
C     flon - longitude read from the best track file
C     ew - one character east/west indicator for longitude
C     iwind - max wind read from the best track file
C     ios - input/output status upon completion or the read operation
C           0 - successful read
C           negative - end-of-file
C           positive - error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C   sampson, nrl    nov 9 98   added cent
C
C........................END PROLOGUE..................................
C
      subroutine readBest (datFile, basin, cycnum, cent, dtg, flat, ns, 
     &     flon, ew, iwind, ios )
c
c         formal parameters
      integer           datFile
      character*2       basin
      character*2       cycnum
      character*8       dtg
      character*2       cent
      real              flat, flon
      character*1       ns, ew
      integer           iwind
      integer           ios

      read( datFile, '(a2,2x,a2,2x,a2,a8,17x,f3.1,a1,2x,f4.1,a1,2x,i3)', 
     1     iostat=ios )basin,cycnum,cent,dtg,flat,ns,flon,ew,iwind
C
      RETURN
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  doWriteAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call doWriteAidRcd (datFile,stormID,cdtg,techname,itau,llwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     itau - forecast period
C     llwnd - array of integers dimensioned (3) where 
C             and the three components are lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine doWriteAidRcd (datFile, stormID, cdtg, techname, 
     1     itau, llwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           itau
      integer           llwnd(llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif

cx    Check for model runs where lat/lon out of range - convert to 0's
cx    sampson nrl oct 26, 1998
cx    sampson nrl aug 19, 2000  changed lon check for less than zero
cx    Handle cases of forecasts crossing 0 longitude,  ajs 1/17/01
      if( llwnd(2) .lt. 0 ) then
          llwnd(2) = llwnd(2) + 3600
      endif

      if( llwnd(1) .lt. -900  .or. 
     1    llwnd(1) .gt.  900  .or.
     2    llwnd(2) .lt.    0  .or. 
     3    llwnd(2) .gt.  3600 ) then
         llwnd(1) = 0
         llwnd(2) = 0
      endif
cx    Check for model runs where wind out of range - convert to 0's
cx    sampson nrl oct 26, 1998
      if( llwnd(3) .lt. 0 .or. llwnd(3) .gt. 300) 
     1     llwnd(3) = 0
      
      if( llwnd(1) .ne. 0 .or. llwnd(2) .ne. 0 
     1     .or. llwnd(3) .ne. 0) then
c     Convert from -900 thru 900 to 900S thru 900N
         ns = 'N'
         ilat = llwnd(1)
         if( ilat .lt. 0 ) then
            ilat = -ilat
            ns = 'S'
         endif
c     Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
         ew = 'W'
         ilon = llwnd(2)
         if( ilon .gt. 1800 ) then
            ilon = 3600 - ilon
            ew = 'E'
         endif
c     Write the aid record...
cx    only if the lat and lon are meaningful
         if ( ilon .le. 1800 .and. ilat .lt. 900 ) then
            write(datFile,9080) basin, stormnum, cdtg, 
     1           technum, techname, itau, ilat, ns, ilon, ew, 
     1           llwnd(3)
 9080       format( A2,', ',A2,', ',A10,', ',A2,', ',A4,', ',
     1           I3,', ',I3,A1,', ',I4,A1,', ',I3 )
         endif
      endif
      
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file,
C                JTWC, NPMOC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call writeAidRcd (datFile,stormID,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (10,3) where 
C               the first dimension is the TAUs 12, 24, 36, 48, 72, ...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  sampson, nrl  Apr 99     R120 - has only 72, 96 and 120 hr fcsts
C  sampson, nrl  Jan 00     C120 - 120 hour forecast, 12 hr interval
C  sampson, nrl  Apr 00     XTRP - 120 hour forecast, 12 hr interval
C........................END PROLOGUE..................................
C
      subroutine writeAidRcd (datFile, stormID, cdtg, techname, 
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(numtau,llw)
c
c         local variables
      integer           ii, jj, itau
      integer           llwnd(llw)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      loopend = 5
      if (techname.eq.'C120' .or. techname.eq.'XTRP') loopend = numtau

c     Loop on the taus: 12, 24, 36, 48 and 72
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       long-range forecasts (e.g., C120) ... sampson NRL Jan 00
cx       does all taus in 12 hr increments
         else if ( techname.eq.'C120' .or. techname.eq.'XTRP') then
	    itau = ii * 12
cx       special case for extended forecasts (R120) ... sampson NRL Apr 99
         else if ( techname .eq. 'R120' ) then
	    itau = ii * 12 + 60
cx       old case for 72 hour forecasts ... 
         else
            itau = ii * 12
            if( itau .eq. 60 ) itau = 72
         endif
         do jj = 1, llw
            llwnd(jj) = ltlnwnd(ii,jj)
         enddo
         call doWriteAidRcd(datFile, stormID, cdtg, techname, itau, 
     &                      llwnd )
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  newWriteAidRcd
C
C  DESCRIPTION:  Write the aid record to the output data file,
C                NHC version
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call newWriteAidRcd (datFile,stormID,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp021997
C     cdtg - current dtg, eg. 1998060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (newnumtau,llw) where 
C               the first dimension is the TAUs 0, 12, 24, 36, 48, 60, 72...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine newWriteAidRcd (datFile, stormID, cdtg, techname, 
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*8       stormID
      character*10      cdtg
      character*4       techname
      integer           ltlnwnd(newnumtau,llw)
c
c         local variables
      integer           ii, jj, itau
      integer           llwnd(llw)
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c

      loopend = newnumtau
c     For CARQ and WRNG loop on the taus: -24, -18, -12, -6 and 0
      if (techname.eq.'CARQ' .or. techname.eq.'WRNG') loopend = 5

c     For all other aids loop on the taus: 
c        0, 12, 24, 36, 48, 60, 72, 84, 96, 108 and 120
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       do all taus in 12 hr increments
         else
	    itau = (ii-1) * 12
         endif
         do jj = 1, llw
            llwnd(jj) = ltlnwnd(ii,jj)
         enddo
         call doWriteAidRcd(datFile, stormID, cdtg, techname, itau, 
     &                      llwnd )
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  writeAid
C
C  DESCRIPTION:  Write the aid record to the output data file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call writeAid (datFile,stormID,century,cdtg,techname,ltlnwnd)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     stormID - storm id, eg. cp0297
C     century - 1st two digits of the storm year, eg. 19
C     cdtg - current dtg, eg. 98060912
C     techname - objective aid name, eg. CLIM
C     ltlnwnd - array of integers dimensioned (numtau,llw) where 
C               the first dimension is the TAUs 12, 24, 36, 48, 60, 72 ...
C               and the second dimension is lat, lon and wind
C
C  OUTPUT PARAMETERS: NONE
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C  sampson, nrl  Apr 99     R120 - has only 72, 96 and 120 hr fcsts
C  sampson, nrl  Jan 00     C120 - 120 hour forecast, 12 hr interval
C  sampson, nrl  Apr 00     XTRP - 120 hour forecast, 12 hr interval
C........................END PROLOGUE..................................
C
      subroutine writeAid (datFile, stormID, century, cdtg, techname,
     1     ltlnwnd )

      include 'dataioparms.inc'
c
c         formal parameters
      integer           datFile
      character*6       stormID
      character*2       century
      character*8       cdtg
      character*4       techname
      integer           ltlnwnd(numtau,llw)
c
c         local variables
      character*2       basin
      character*2       stormnum
      character*1       ns, ew
      character*2       technum
      integer           ii, itau
      integer           ilat, ilon
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      basin = stormID(1:2)
      call upcase( basin, 2 )
      stormnum = stormID(3:4)

c     Assign the objective aid technique number
      if( techname .eq. 'CARQ' ) then
         technum = '01'
      else if( techname .eq. 'WRNG' ) then
         technum = '02'
      else
         technum = '03'
      endif
cx  
      loopend = 5
      if (techname.eq.'C120' .or. techname.eq.'XTRP') loopend = numtau

c     Loop on the taus: 12, 24, 36, 48 and 72
      do ii = 1, loopend
         if( techname .eq. 'CARQ' .or. techname .eq. 'WRNG' ) then
            itau = -( (5-ii) * 6)
cx       long-range forecasts (e.g., C120) ... sampson NRL Jan 00
cx       does all taus in 12 hr increments
         else if ( techname.eq.'C120' .or. techname.eq.'XTRP' ) then
	    itau = ii * 12
cx       special case for extended forecasts (R120) ... sampson NRL Apr 99
         else if ( techname .eq. 'R120' ) then
	    itau = ii * 12 + 60
cx       old case for 72 hour forecasts ... 
         else
            itau = ii * 12
            if( itau .eq. 60 ) itau = 72
         endif
cx       Check for model runs where lat/lon out of range - convert to 0's
cx       sampson nrl oct 26, 1998
	 if( ltlnwnd(ii,1) .lt. -900  .or. 
     1	     ltlnwnd(ii,1) .gt.  900  .or.
     2       ltlnwnd(ii,2) .lt. -1800 .or. 
     3       ltlnwnd(ii,2) .gt.  3600 ) then
      	       ltlnwnd(ii,1) = 0
      	       ltlnwnd(ii,2) = 0
         endif
cx       Check for model runs where wind out of range - convert to 0's
cx       sampson nrl oct 26, 1998
	 if( ltlnwnd(ii,3) .lt. 0 .or. ltlnwnd(ii,3) .gt. 300) 
     1	       ltlnwnd(ii,3) = 0

         if( ltlnwnd(ii,1) .ne. 0 .or. ltlnwnd(ii,2) .ne. 0 
     1        .or. ltlnwnd(ii,3) .ne. 0) then
c           Convert from -900 thru 900 to 900S thru 900N
            ns = 'N'
            ilat = ltlnwnd(ii,1)
            if( ilat .lt. 0 ) then
               ilat = -ilat
               ns = 'S'
            endif
c           Convert from 0 thru 3600 (WH < 1800 < EH) to 1800W thru 1800E
            ew = 'W'
            ilon = ltlnwnd(ii,2)
            if( ilon .gt. 1800 ) then
               ilon = 3600 - ilon
               ew = 'E'
            endif
c           Write the aid record...
cx          only if the lat and lon are meaningful
	    if ( ilon .lt. 1800 .and. ilat .lt. 900 ) then
               write(datFile,9080) basin, stormnum, century, cdtg, 
     1              technum, techname, itau, ilat, ns, ilon, ew, 
     1              ltlnwnd(ii,3)
 9080         format( A2,', ',A2,', ',A2,A8,', ',A2,', ',A4,', ',
     1              I3,', ',I3,A1,', ',I4,A1,', ',I3 )
            endif
         endif
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readNext
C
C  DESCRIPTION:  Reads the next ATCF_RECORD record from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readNext (datFile,record,ios)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C
C  OUTPUT PARAMETERS:
C     rcd - structure to read record into
C     ios - return 0 success, neg for end of file, pos for error
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readNext (datFile, rcd, ios )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      type (ATCF_RECORD) rcd
      integer            ios
c
c         local variables
      character line*200
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c     Zero out the dtg, tech, lat, lon and vmax just in case
      rcd%DTG = '          '
      rcd%tech = '    '
      rcd%latns = '    '
      rcd%lonew = '     '
      rcd%vmax = '   '
c
c     Read one data record.
      read( datFile, '(a200)', iostat=ios ) line
      if( ios .eq. 0 ) then
c
c     Get the individual fields from the data record.
         read( line, '(a2,2x,a2,2x,a10,2x,a2,2x,a4,2x,a3)' )
     &        rcd%basin, rcd%cyNum, rcd%DTG, rcd%technum, rcd%tech, 
     &        rcd%tau
         read( line, '(35x,a4,2x,a5,2x,a3)' )
     &        rcd%latns, rcd%lonew, rcd%vmax
         read( line, '(53x,a4,2x,a2,2x,a3,2x,a3,4(2x,a4))' )
     &        rcd%mslp, rcd%ty, rcd%rad, rcd%windcode, rcd%radii(1), 
     &        rcd%radii(2), rcd%radii(3), rcd%radii(4)
         read( line, '(97x,a4,2x,a4,5(2x,a3),2x,a3,2x,a3,2x,a3)' )
     &        rcd%radp, rcd%rrp, rcd%mrd, rcd%gusts, rcd%eye, 
     &        rcd%unused, rcd%maxseas, rcd%initials, rcd%dir, rcd%speed
         read( line, '(149x,a10,2x,a1)' )  
     &        rcd%stormname, rcd%depth
         read( line, '(164x,a2,2x,a3,4(2x,a3))' )  
     &        rcd%seas, rcd%seascode, rcd%seasrad(1), rcd%seasrad(2),
     &        rcd%seasrad(3), rcd%seasrad(4)
      endif
C     
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getARecord
C
C  DESCRIPTION:  Reads one record of specified type from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getARecord (datFile,"CARQ",aidRcd,result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     technique - "CARQ", "WRNG", "JTWC" ...
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getARecord (datFile, technique, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      character*4     technique
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      logical*2   found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      found = .false.
c
c     Loop on reading records until a record is found
c     of the type specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, result )
c
c        If tech name matches specified record type then process.
         if( technique .eq. aidRcd%aRecord(1)%tech ) found = .true.
c
      enddo
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  readARecord
C
C  DESCRIPTION:  Reads one AID_DATA data info from the input file.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call readARecord (datFile,aidRcd,result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine readARecord (datFile, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer         datFile
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      integer            ii
      integer            readStat
      type (ATCF_RECORD) recrd
      character          savDTG*10
      character          savtech*4
      logical*2          done
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 0
c
c     Read the next record in the data file
      call readNext( datFile, recrd, readStat )
c
c     Save the date-time-group and the technique
      if( readStat .eq. 0 ) then
         result = 1
         savDTG = recrd%DTG
         savtech = recrd%tech
      endif
c
c     Read all of the tau's for this DTG and tech
      ii=0
      aidRcd%numrcrds = 0
      done = .false.
      do while (result.eq.1 .and. readStat.eq.0
     &          .and. ii.lt.AidTauMax .and. .not.done)
         ii = ii + 1
c
c        Process the A_RECORD
         call processArcd( aidRcd%aRecord(ii), recrd, result )
c        Copy the ATCF_RECORD
         aidRcd%atcfRcd(ii) = recrd
         aidRcd%numrcrds = ii
c
c        Read the next record in the data file
         call readNext( datFile, recrd, readStat )
c
c        If read a new dtg or tech then flag done and backup one record
         if( savDTG.ne.recrd%DTG .or. savtech.ne.recrd%tech ) then
            done = .true.
            backspace( datFile )
         endif
      enddo

      if( readStat .gt. 0 ) result = 0
      if( readStat .lt. 0 ) result = readStat
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getSingleTAU
C
C  DESCRIPTION:  Gets the aid data for the specified tau from the
C                supplied AID_DATA.  Returns the data for the tau 
C                in tauData.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getSingleTAU (aidRcd, 72, tauData, result)
C
C  INPUT PARAMETERS:  
C     aidRcd - supplied AID_DATA structure
C     tau    - requested tau
C
C  OUTPUT PARAMETERS:
C     tauData - struct to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getSingleTAU ( aidRcd, tau, tauData, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (AID_DATA) aidRcd, tauData
      integer         tau
      integer         result
c
c         local variables
      integer   ii, jj
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tau in the AID_DATA structure.
      found = .false.
      ii = 1
      jj = 1
      tauData%numrcrds = 0
      do while( ii.le.aidRcd%numrcrds )
         if( aidRcd%aRecord(ii)%tau .eq. tau ) then
            found = .true.
            tauData%aRecord(jj) = aidRcd%aRecord(ii)
            tauData%atcfRcd(jj) = aidRcd%atcfRcd(ii)
            jj = jj + 1
            tauData%numrcrds = tauData%numrcrds + 1
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getAidTAU
C
C  DESCRIPTION:  Gets the first A_RECORD record for the specified tau from
C                the supplied AID_DATA.
C                Note: this only get the first RAD (35, 50, 65, 100 kt),
C                for the requested tau.  If all the records for the tau
C                are needed then use getSingleTAU().
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getAidTAU (aidRcd, 72, aRecord, result)
C
C  INPUT PARAMETERS:  
C     aidRcd - supplied AID_DATA structure
C     tau    - requested tau
C
C  OUTPUT PARAMETERS:
C     aRecord - struct to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getAidTAU ( aidRcd, tau, aRecord, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (AID_DATA) aidRcd
      integer         tau
      type (A_RECORD) aRecord
      integer         result
c
c         local variables
      integer   ii
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tau in the AID_DATA structure.
      found = .false.
      ii = 1
      do while( ii.le.aidRcd%numrcrds .and. .not.found )
         if( aidRcd%aRecord(ii)%tau .eq. tau ) then
            found = .true.
            aRecord = aidRcd%aRecord(ii)
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getAidDTG
C
C  DESCRIPTION:  Gets the first aid data for the specified DTG 
C                from the input file
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call getAidDTG (datFile, dtg, aidRcd, result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     dtg    - requested dtg (date-time-group)
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getAidDTG ( datFile, dtg, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
      character*8       dtg
      type (AID_DATA)   aidRcd
      integer           result
c
c         local variables
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      found = .false.
c
c     Loop on reading records until a record is found
c     with the dtg specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, result )
c
c        If dtg matches specified dtg then process.
         if( dtg .eq. aidRcd%aRecord(1)%DTG(3:10) ) found = .true.
c
      enddo
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getTech
C
C  DESCRIPTION:  Gets the data for a specified aid technique
C                from the supplied BIG_AID_DATA structure and
C                returns an AID_DATA structure
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call getTech (bigAidRcd, tech, aidRcd, result)
C
C  INPUT PARAMETERS:  
C     bigAidRcd - BIG_AID_DATA structure containing all records for a dtg
C     tech - requested obj aid technique
C
C  OUTPUT PARAMETERS:
C     aidRcd - structure to read data into
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getTech ( bigAidRcd, tech, aidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (BIG_AID_DATA) bigAidRcd
      character*4     tech
      type (AID_DATA) aidRcd
      integer         result
c
c         local variables
      integer   ii, jj
      logical*2 found
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
c
c     Find the requested tech in the BIG_AID_DATA structure.
      found = .false.
      ii = 1
      jj = 1
      aidRcd%numrcrds = 0
      do while( ii .le. bigAidRcd%numrcrds )
         if( bigAidRcd%aRecord(ii)%tech .eq. tech ) then
            found = .true.
            aidRcd%aRecord(jj) = bigAidRcd%aRecord(ii)
            aidRcd%atcfRcd(jj) = bigAidRcd%atcfRcd(ii)
            jj = jj + 1
            aidRcd%numrcrds = aidRcd%numrcrds + 1
         endif
         ii = ii + 1
      enddo

      if( .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  getBigAidDTG
C
C  DESCRIPTION:  Gets all the aid data for the specified DTG 
C                from the input file 
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call getBigAidDTG (datFile, dtg, bigAidRcd, result)
C
C  INPUT PARAMETERS:  
C     datFile - unit number of output data file
C     dtg    - requested dtg (date-time-group)
C
C  OUTPUT PARAMETERS:
C     bigAidRcd - structure to read data into
C     result - return 0 for fail, neg for end-of-file, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine getBigAidDTG ( datFile, dtg, bigAidRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer           datFile
c      character*8       dtg
      character*10       dtg
      type (BIG_AID_DATA)   bigAidRcd
      type (AID_DATA)   aidRcd
      integer           readStat
      integer           result
      integer           ii, jj
c
c         local variables
      logical*2 found
      logical*2 dtgmatch
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1
      readStat = 1
      found = .false.
      dtgmatch = .false.
c
c     Loop on reading records until a record is found
c     with the dtg specified
      do while( result.eq.1 .and. .not.found )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, readStat )
         result = readStat
c
c        If dtg matches specified dtg then process.
         if( dtg .eq. aidRcd%aRecord(1)%DTG ) found = .true.
c
      enddo

      if( found ) dtgmatch = .true.
c     If found assign the aidRcd read into bigAidRcd 
      if( dtgmatch ) then
         do ii=1, aidRcd%numrcrds
            bigAidRcd%aRecord(ii) = aidRcd%aRecord(ii)
            bigAidRcd%atcfRcd(ii) = aidRcd%atcfRcd(ii)
         enddo
         bigAidRcd%numrcrds = aidRcd%numrcrds
      endif

c     Loop on reading records as long as dtg matches specified dtg
      do while( readStat.eq.1 .and. dtgmatch )
c
c        Read the next record in the data file.
         call readARecord( datFile, aidRcd, readStat )
         result = readStat
c
c        If dtg matches specified dtg then process.
         if( dtg .ne. aidRcd%aRecord(1)%DTG ) dtgmatch = .false.
c        If matching dtg then assign aidRcd into bigAidRcd
         if( readStat.eq.1 .and. dtgmatch ) then
            jj = bigAidRcd%numrcrds + 1
            do ii=1, aidRcd%numrcrds
               bigAidRcd%aRecord(jj) = aidRcd%aRecord(ii)
               bigAidRcd%atcfRcd(jj) = aidRcd%atcfRcd(ii)
               jj = jj + 1
            enddo
            bigAidRcd%numrcrds = bigAidRcd%numrcrds + aidRcd%numrcrds
         endif
c
      enddo
      
c     Backup the file to just after the last matching dtg.
      if( found .and. .not. dtgmatch .and. aidRcd%numrcrds .gt. 0 )
     &   call backspaceFile( datFile, aidRcd%numrcrds )
c
      if( result.ne.0 .and. .not.found ) result = 0
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  backspaceFile
C
C  DESCRIPTION:  Repositions the specified file back "numRcrds" records.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  Sept 2000
C
C  USAGE:  call backspaceFile ( datFile, numRcrds )
C
C  INPUT PARAMETERS:  
C     datFile - unit number of the objective aids file
C     numRcrds - number of records to back up
C
C  OUTPUT PARAMETERS:
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine backspaceFile (datFile, numRcrds)
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      integer            datFile
      integer            numRcrds
      integer            ii
c
c         local variables
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      do ii=1, numRcrds
         backspace datFile
      enddo
C
      END


C
C........................START PROLOGUE.................................
C
C  SUBPROGRAM NAME:  processArcd
C
C  DESCRIPTION:  Assigns the data for a A_RECORD structure.
C
C  PROGRAMMER, DATE:  Ann Schrader   (SAIC)  June 1998
C
C  USAGE:  call processArcd ( aidRcd%aRecord(ii), recrd, result )
C
C  INPUT PARAMETERS:  
C     atcfRcd - ATCF_RECORD struct containing data
C
C  OUTPUT PARAMETERS:
C     aidRcd - A_RECORD struct to receive data
C     result - return 0 for fail, 1 for success
C
C  ERROR CONDITIONS:
C
C........................MAINTENANCE SECTION............................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C  METHOD:
C
C  RECORD OF CHANGES:
C
C........................END PROLOGUE..................................
C
      subroutine processArcd (aidRcd, atcfRcd, result )
c
      implicit none
c
      INCLUDE  'dataformats.inc'
c
c         formal parameters
      type (A_RECORD)    aidRcd
      type (ATCF_RECORD) atcfRcd
      integer            result
c
c         local variables
c . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
      result = 1

      aidRcd%basin = atcfRcd%basin
      read( atcfRcd%cyNum, '(i2)' ) aidRcd%cyNum
      aidRcd%DTG = atcfRcd%DTG
      read( atcfRcd%technum, '(i2)' ) aidRcd%technum
      aidRcd%tech = atcfRcd%tech
      read( atcfRcd%tau, '(i3)' ) aidRcd%tau
      read( atcfRcd%latns, '(f3.1,a1)' ) aidRcd%lat, aidRcd%NS
      read( atcfRcd%lonew, '(f4.1,a1)' ) aidRcd%lon, aidRcd%EW
      read( atcfRcd%vmax, '(i3)' ) aidRcd%vmax
      read( atcfRcd%mslp, '(i4)' ) aidRcd%mslp
      aidRcd%ty = atcfRcd%ty
      read( atcfRcd%rad, '(i3)' ) aidRcd%rad
      aidRcd%windcode = atcfRcd%windcode
      read( atcfRcd%radii(1), '(i4)' ) aidRcd%radii(1)
      read( atcfRcd%radii(2), '(i4)' ) aidRcd%radii(2)
      read( atcfRcd%radii(3), '(i4)' ) aidRcd%radii(3)
      read( atcfRcd%radii(4), '(i4)' ) aidRcd%radii(4)
      read( atcfRcd%radp, '(i4)' ) aidRcd%radp
      read( atcfRcd%rrp, '(i4)' ) aidRcd%rrp
      read( atcfRcd%mrd, '(i3)' ) aidRcd%mrd
      read( atcfRcd%gusts, '(i3)' ) aidRcd%gusts
      read( atcfRcd%eye, '(i3)' ) aidRcd%eye
      read( atcfRcd%maxseas, '(i3)' ) aidRcd%maxseas
      aidRcd%initials = atcfRcd%initials
      read( atcfRcd%dir, '(i3)' ) aidRcd%dir
      read( atcfRcd%speed, '(i3)' ) aidRcd%speed
      aidRcd%stormname = atcfRcd%stormname
      aidRcd%depth = atcfRcd%depth
      read( atcfRcd%seas, '(i2)' ) aidRcd%seas
      aidRcd%seascode = atcfRcd%seascode
      read( atcfRcd%seasrad(1), '(i3)' ) aidRcd%seasrad(1)
      read( atcfRcd%seasrad(2), '(i3)' ) aidRcd%seasrad(2)
      read( atcfRcd%seasrad(3), '(i3)' ) aidRcd%seasrad(3)
      read( atcfRcd%seasrad(4), '(i3)' ) aidRcd%seasrad(4)
C
      END
