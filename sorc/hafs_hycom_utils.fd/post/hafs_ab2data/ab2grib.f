      subroutine ab2grib (array, iii, jjj,
     &         yy,mmm,dd,runhour,verfhour,
     &         flag,ibms,namec,k,lflag,depth,gridlabel)

      implicit none
      integer iii, jjj
C Arguments:
      real array(iii,jjj)
      INTEGER yy, mmm, dd, runhour, verfhour,gridlabel

      REAL flag
      INTEGER ibms            !1 for use bit mask, 0 else

      REAL ssh(iii, jjj)
      LOGICAL lbm(iii, jjj)
      INTEGER mxbit
      REAL xlat1, xlon1, xlat2, xlon2, delx, dely, colat1
      INTEGER igen, icen, ilpds, idrt
      INTEGER iptv
      INTEGER parmno, level
      INTEGER itl, il1, il2, iftu
      INTEGER itr, ip1, ip2, ids, inm, ina
      INTEGER proj, ortru
      INTEGER griblen,lcount
      real cell_no

      real, parameter :: flag1 = 2.0**100
      real, parameter :: flag2 = -1.E30
      real, parameter :: flag3 = -1.E25


      
      CHARACTER, allocatable, dimension(:):: grib
      INTEGER lgrib, ierr
      INTEGER i,j,k,lflag
      character*(*) namec
      integer table,parms9,parms10
      real    increment,multiplicand,additive,depth

C Include these for bacio-related things to work properly:
      INCLUDE "locale.inc"
      INCLUDE "clib.inc"
      include  "grib_block.h"
      CHARACTER*80 fname
      logical outputout
      outputout=.false.
      outputout=.true.

C     Parameter identification and packing material
      mxbit = 20    ! maximum number of bits to use, 0 -> infinite

      if (mxbit.gt.0) griblen = (200 + iii*jjj*(mxbit+1)/8 )
      if (mxbit.eq.0) griblen = (200 + iii*jjj*(20+1)/8 )

      allocate(grib(griblen))

      call grib_maps(namec,lflag,
     $     table,parms9,parms10,increment,multiplicand,additive,cell_no)


      lcount=0
      lbm=.false.
      do j=1,jjj
         do i=1,iii
            IF (ibms .EQ. 1) THEN
              if (array(i,j) .ne. flag .and.
     1            array(i,j) .ne. flag1 .and.
     2            array(i,j) .ne. flag2 .and.
     3            array(i,j) .gt. flag3 .and.
     4            array(i,j) .lt. -flag3) then 
              lbm(i,j) = .true.
              endif
            endif
            ssh(i,j)=array(i,j)*multiplicand + additive
            if (array(i,j) .ne. flag .and.
     1          array(i,j) .ne. flag1 .and.
     2          array(i,j) .ne. flag2) then
            if (array(i,j) .lt. flag3 .or.
     1          array(i,j) .gt. -flag3) then 
                    lcount=lcount+1
                    print *, 'dhi ',namec,lcount,i,j,array(i,j)
            endif
            endif
         enddo 
      enddo 


C     Center and grib table identification
      ilpds = 28  !pds length
      icen  =  7  !center
      igen  = 47 
      idrt   = 6
      colat1 = 0   ! dummy when gribbing hycom
C     Lat long bounding boxes
      xlat1 = -30.0
      xlon1 = -30.0
      xlat2 = +30.0
      xlon2 = +30.0
      delx = (xlon2 - xlon1)/float(iii - 1)
      dely = (xlat2 - xlat1)/float(jjj - 1)

c k and depth used to determine il1, il2
c     print *,lflag,depth,parms9,parms10,increment,multiplicand,additive
      if (lflag.eq.0 .and. depth.eq.0.0) then  ! 2d
        il1 = 1
        il2 = 0
      else       ! lflag=1; , depth set for 3z
        if (lflag.eq.1) then
           il1 = 1      ! il1, il2 are vertical level info
           il2 = depth  ! see ON388 table 3
        else     ! 3d; check parms10
c
c I don't believe we have any 108
c       if (parms10.eq.108) then    ! level is in 2 words
c         il1=depth/100
c il2 should NOT be depth (which is zz(k)), it needs to be zz(k+1), which is not passed in
c need to fix this later
c         il2=depth/100

          if (parms10.eq.109) then    ! level is in 1 word
            il1=mod(int(depth)/256,256)
            il2=mod(int(depth),256)
            il1=mod(int(k)/256,256)
            il2=mod(int(k),256)
c             print *, '109109', parms10,il1,il2,depth,k
          elseif (parms10.eq.110) then
            il1=k
            il2=k+1
          else   ! hmm, lets set this as a default
            il1=1
            il2=0
c           print *, 'default set for il1,il2'
          endif
        endif
      endif

      iftu = 1     ! forecast time unit, 1 -> hours (table 4)
      itr  = 10    ! forecast time range, 10 -> valid time in P1
      ip1  = verfhour   !  valid time, relative to reference time
      ip2  = 0     ! Averaging period (table 5)

      ina  = 0
      inm  = 0

      IDS = -LOG10(increment)
      iptv   = table
      parmno = parms9
      itl = parms10


c     print *, 'dhi1 values:',namec,iptv,icen,igen,ibms,parmno,itl,
c    &            il1,il2,yy, mmm, dd,runhour,ip1,ids,lflag,lcount

      if (outputout) then
      write(6,*)trim(namec),k,lflag,depth,'namec,k,lflag,depth'
      call flush(6)

      CALL gribit(ssh, lbm, idrt, iii, jjj, mxbit, colat1,
     &                  ilpds, iptv, icen,igen,ibms,parmno,ITL,IL1,IL2,
     &                  yy, mmm, dd,runhour,
     &                  IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  xlat1,xlon1,xlat2,xlon2,delx,dely,ORTRU,PROJ,
     &                  gridlabel,cell_no,
     &                  grib,lgrib,ierr)

C Write out the gribbed bits:
      IF (ierr .NE. 0
     &         .or. griblen .lt. lgrib) THEN
        PRINT *,'error ',ierr,' while trying to construct grib message '
        PRINT *,'size allocated for grib message is ',griblen
        PRINT *,'size calculated for grib message is ',lgrib
        return
      ELSE
c       print *,'calling bacio with BAWRITE for ',
c    &      namec,k,fdes,start,newpos,nactual
        ierr = bacio(BAWRITE, start, newpos,
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname,
     2                grib)
CD        PRINT *,'nactual, lgrib ',nactual, lgrib, griblen
        IF (ierr .NE. 0) THEN
          PRINT *,'bacio returns ierr = ',ierr
        ENDIF
        start = newpos
      ENDIF

      endif    ! outputout out is true
      RETURN
      END

      subroutine open_unit (io,frmt)
      implicit none
      common /gates/ lgopen
      logical :: lgopen
      integer, save :: jcalls
      include  "grib_block.h"
      include  "locale.inc"
      include  "clib.inc"
      integer io,lp
      character*(*) frmt
      integer ierr,l
      character*1      grib1
      character*80     fname
      data jcalls/0/
c     open file...
      lp=6
      jcalls=jcalls+1
         if (jcalls.eq.1) then

            if(io.lt.10) then
               write(fname, 9011) io
 9011          format ('fort.',I1)
            elseif(io.lt.100) then
               write(fname, 9012) io
 9012          format ('fort.',I2)
            elseif(io.lt.1000) then
               write(fname, 9013) io
 9013          format ('fort.',I3)
            endif
            l=len_trim(fname)

            print *,'calling bacio with BAOPEN_WONLY '
            ierr = bacio(
     $           BAOPEN_WONLY, start, newpos, SIZEOF_CHARACTER, 0,
     $           nactual, fdes, fname(1:l), grib1)
            if (ierr .ne. 0) then
               write(lp,*)
     $              'Failed to open the grib file, unit = ',io
               call flush(lp)
               stop '(Open failed)'
            endif
            lgopen=.true.
         endif
      return
      end



      subroutine close_unit (io,frmt)
      implicit none
      common /gates/ lgopen
      logical :: lgopen
      include  "grib_block.h"
      include  "locale.inc"
      include  "clib.inc"
      integer io,lp
      character*(*) frmt
      integer ierr,l
      character*1      grib1
      character*80     fname
c     close file...
      lp=6
      if (frmt(1:5).eq.'grib1' .or.
     &         frmt(1:5).eq.'GRIB1') then
c
c         grib-1 format
c

C hardcoded  io to 51 (gets reset in calling routine)
         io=51
         print *,'unit aNd status2 ',io,lgopen
         if (lgopen) then

            if(io.lt.10) then
               write(fname, 9001) io
 9001          format ('fort.',I1)
            elseif(io.lt.100) then
               write(fname, 9002) io
 9002          format ('fort.',I2)
            elseif(io.lt.1000) then
               write(fname, 9003) io
 9003          format ('fort.',I3)
            endif
            l=len_trim(fname)


            print *,'calling bacio with BACLOSE ', start,newpos 
            ierr = bacio(BACLOSE, start, newpos, SIZEOF_CHARACTER, 0,
     1                nactual, fdes, fname(1:l), grib1)
            if (ierr .ne. 0) then
               write(lp, *)
     $          'Failed to close grib io file cleanly, ierr = ',ierr
               call flush(lp)
               stop '(Close failed)'
            endif
         endif
      endif
      return
      end
