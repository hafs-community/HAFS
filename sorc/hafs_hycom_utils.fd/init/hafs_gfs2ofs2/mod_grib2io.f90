!
module mod_grib2io
       use grib_mod
       use params
 private
 public rdgrib
 logical, dimension(200),save :: opn=.true.
contains
!


      subroutine rdgrib(lugb,grbfile,xgfld,iparms,jpdtno, &
        jdisc,pack,xpts,ypts)
       integer,dimension(200) :: jids,jpdt,jgdt
       integer,dimension(4) :: iparms
       real,dimension(5000000)      :: xgfld
       logical :: unpack,expand
       character, intent(in) ::  grbfile*(*)
       character (len=200) :: idxfile

       integer   jpdtno,jdisc,pack,xpts,ypts
       logical, dimension(200),save :: opn=.true.
       logical, save :: opned
       type(gribfield) :: gfld

      call start()
      if (pack.eq.0) then
       unpack=.false.
      else
       unpack=.true.
      endif

      expand=.false.


      lugi=lugb+1
!!!
      idxfile=TRIM(grbfile)//'.idx'
!      print *,"LUGB ",lugb,"  GRBFILE  ",grbfile
!      print *,"IPARMS ",iparms(1),iparms(2),iparms(3),iparms(4)
      if (opn(lugb)) then
      inquire(unit=lugb,opened=opned)
      if(opned) then
        print *,' mod_grib2io: error: unit lugb=',lugb,' is in use'
        stop
      endif
      endif
      call baopenr(lugb,trim(grbfile),ierr)
      IF(ierr.NE.0) THEN
        PRINT *,'mod_grib2io: error opening file ',grbfile
!        STOP
      ELSE
!        WRITE(*,*) 'mod_grib2io: opening GRIB file as unit ',lugb
      ENDIF

!      print *,"LUGI ",lugi," IDXFILE ",idxfile
      inquire(unit=lugi,opened=opned)
      if(opned) then
        print *,' mod_grib2io: error: unit lugi=',lugi,' is in use'
        stop
      endif
      call baopenr(lugi,trim(idxfile),ierr)
      if(ierr.ne.0) then
        print *,'error opening file ',idxfile
        stop
      else
!       WRITE(*,*) 'mod_grib2io: opening IDX as unit ',lugi
      endif
      opn(lugb)=.false.
!      endif
!

      if (pack.eq.1) then
          jskp = 0
         jdisc=jdisc
         jgdtn=-1
         jids=-9999
         jpdt=-9999
         jgdt=-9999
         jpdtn=jpdtno       
!
        jpdt(1) = iparms(1)
        jpdt(2) = iparms(2)
        jpdt(10) = iparms(3)
        jpdt(12) = iparms(4)

       endif 

      if (pack.eq.0) then
          jskp = 0
         jdisc=-1
         jgdtn=-1
         jids=-9999
         jpdt=-9999
         jpdtn=-1
         jgdt=-9999
      endif
!
!                 **    NOTICE   ***
!
      call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt, &
                unpack,jskp,gfld,iret)
!
         if ( iret.ne.0) then
            print *,' getgb2 error = ',iret, ' ip ',ip
         endif
!
       if (pack.eq.1) then
         do i=1,gfld%ngrdpts
         xgfld(i)=gfld%fld(i)
         enddo ! enddo for i
        endif

        if (pack.eq.0) then
           xpts=gfld%igdtmpl(8)
           ypts=gfld%igdtmpl(9)
           print *,' GRID TEMPLATE 3.',xpts,ypts
        endif
!

!
! Free memory when done with field
!
       npts=gfld%ngrdpts
!       print *, ' From subroutine npts ',npts
       call gf_free(gfld)
      return
      end subroutine rdgrib

END MODULE mod_grib2io
