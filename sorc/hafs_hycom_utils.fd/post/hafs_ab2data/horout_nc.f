      subroutine horout(array,
     &                  artype,yrflag,time3,iexpt,lhycom,
     &                  name,namec,units, k,ltheta, frmt,io,
     &                  fdate, verfhour)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF fortran 90 interface
      implicit none
      include  "grib_block.h"
      include  "locale.inc"
      include  "clib.inc"
      
c
      character*(*)    name,namec,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,k,io,gridlabel
      integer	       ierr
      character*1      grib1
      integer  fdate(4)           ! yyyy, mm, dd, hh of forecast run
      integer  verfhour           ! verifying hour/lead
      INTEGER, save :: ibms = 1
      character*80 fname

      double precision time3(3)
      real             array(ii,jj),thetak
      CHARACTER *1 grib
      real, parameter :: flag = 2.0**100
c     real, parameter :: flag = -1.E30
c
c     write out array to unit io based on frmt.
c
c     array size and frmt        must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.< c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c       frmt=='GRIB1'         for GRIB1 format
c       frmt=='grib1'         for GRIB1 format
c
c     This version supports frmt=='netCDF' and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,l,iyear,month,iday,ihour
      real             :: hmin,hmax
      double precision :: dt,dt0,year
c
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF' .or.
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          iotype = 4
          if     (laxis) then
            write(lp,'(/2a/)') 'horout - NetCDF I/O (lat/lon axes)'
          else
            write(lp,'(/2a/)') 'horout - NetCDF I/O (curvilinear)'
          endif
          call flush(lp)
        elseif (frmt(1:l).eq.'grib1' .or.
     &         frmt(1:l).eq.'GRIB1') then
c
c         grib-1 format
c
          iotype = 5
          laxis = .false.
          write(lp,'(/a,a/)') 'horout - grib-1 I/O'
          call flush(lp)

        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop 9
        endif
c
c       initialize label.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (label(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (label(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            dt0 = 15.0
          elseif (yrflag.eq.1) then
            dt0 = 15.25
          elseif (yrflag.eq.2) then
            dt0 = 0.0
          else
            dt0 = 0.0
          endif
          cell = (time3(2)+dt+dt0) - (time3(1)-dt+dt0)
          if     (artype.eq.2) then
            write(label(51:72),114) ' mean: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          else
            write(label(51:72),114) ' sdev: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          endif
        endif
        if (lhycom) then
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
c     complete the label
c
      if     (k.eq.0) then
        label(33:50)=name
      elseif (ltheta) then
        write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
      else
        write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
      endif
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr(array, ip,.false., hmin,hmax, io, .false.)
        write(io,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(io)
        write(lp,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(lp)
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        write(io) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value)
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do j= 1,jj
          do i= 1,ii
            if     (array(i,j).ne.fill_value) then
              write(io,frmt) plon(i,j),plat(i,j),array(i,j)
            endif
          enddo
        enddo
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        write(io,frmt) array
        call flush(io)
        write(lp,'(a)') label(33:81)
        call flush(lp)
      elseif (iotype.eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        write(*,*) 'ncfile ',ncfile
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)') 'error in horout_nc- ',Ename,' not defined'
          call flush(lp)
          stop 9
        endif
c
        call ncrange(array,ii,jj,1, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c          create a new NetCDF and write data to it
c
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          if     (laxis) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Longitude", ii,pLonDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Y",         jj,pYDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "X",         ii,pXDimID))
          endif
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdf2d"))
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdf2d"))
          endif
          ! create the variables and attributes
          call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                             MTDimID,MTVarID))
          if     (yrflag.eq.0) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "360_day"))
          elseif (yrflag.eq.1) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          elseif (yrflag.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          else
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 1900-12-31 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "standard"))
          endif
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          if     (artype.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          elseif (artype.eq.3) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          endif
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               pLatDimID,pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               pLonDimID,pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          else
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif
          ! model 2d variable
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Date"))
          else
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,   MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Longitude Latitude date"))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin, hmax/)))
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:50)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          call ncheck(nf90_put_var(ncfileID,MTVarID,time3(3)))
          call ncheck(nf90_put_var(ncfileID,datVarID,date   ))
          if     (laxis) then
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(1,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,1)))
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c          Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
          if     (laxis) then
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Longitude",pLonDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Y",        pYDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "X",        pXDimID))
          endif
          ! switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Date"))
          else
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,   MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Longitude Latitude date"))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin, hmax/)))
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:50)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               label(33:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! get varID and write to array
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close file 
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(lp)
      elseif (iotype.eq.5) then
c
c       grib-1 I/O
c
         call open_unit(io,frmt)

         read(22,*) gridlabel
         rewind(22)
         print *,'calling 1ab2grib for ',name, namec, k
         call ab2grib ( array, ii,jj,
     &        fdate(1),fdate(2),fdate(3),fdate(4),verfhour,
     &        flag,ibms,namec,k,0,0.0,gridlabel)


      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop 9
      endif
      return
      end

      subroutine horout_3d(array,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,units, kf,kl,ltheta, frmt,io,
     &                     fdate,verfhour)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
      include  "grib_block.h"
      include  "locale.inc"
      include  "clib.inc"
      integer  fdate(4)           ! yyyy, mm, dd, hh of forecast run
      integer  verfhour           ! verifying hour/lead
      INTEGER, save :: ibms = 1
      character*1 grib1
      integer	       ierr
      character*80 fname

c
      character*(*)    name,namec,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,kf,kl,io,gridlabel
      double precision time3(3)
      real             array(ii,jj,kl),thetak
      real, parameter :: flag = 2.0**100
c
c     write out a 3-d layer array to unit io based on frmt.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.< c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c       frmt=='GRIB1'         for GRIB1 format
c       frmt=='grib1'         for GRIB1 format
c
c     This version supports frmt=='netCDF' and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour
      real             :: hmin(999),hmax(999)
      double precision :: dt,dt0,year
c
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout_3d - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout_3d - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout_3d - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF' .or. 
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          iotype = 4
          if     (laxis) then
            write(lp,'(/2a/)') 'horout_3d - NetCDF I/O (lat/lon axes)'
          else
            write(lp,'(/2a/)') 'horout_3d - NetCDF I/O (curvilinear)'
          endif
          call flush(lp)
        elseif (frmt(1:l).eq.'grib1' .or.
     &          frmt(1:l).eq.'GRIB1') then
c
c         grib-1 format
c
          iotype = 5
          laxis = .false.
          write(lp,'(/a,a/)') 'horout - grib-1 I/O'
          call flush(lp)


        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_3d - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop 9
        endif
c
c       initialize label.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (label(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (label(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            dt0 = 15.0
          elseif (yrflag.eq.1) then
            dt0 = 15.25
          elseif (yrflag.eq.2) then
            dt0 = 0.0
          else
            dt0 = 0.0
          endif
          cell = (time3(2)+dt+dt0) - (time3(1)-dt+dt0)
          if     (artype.eq.2) then
            write(label(51:72),114) ' mean: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          else
            write(label(51:72),114) ' sdev: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          endif
        endif
        if (lhycom) then
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr3(array(1,1,kf),kl-kf+1,
     +               ip,.false., hmin(kf),hmax(kf), io, .false.)
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(io)
          write(lp,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(lp)
        enddo
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value).
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          do j= 1,jj
            do i= 1,ii
              if     (array(i,j,k).ne.fill_value) then
                write(io,frmt) plon(i,j),plat(i,j),array(i,j,k)
              endif
            enddo
          enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif(iotype.eq.4) then
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        write(*,*) 'ncfile ',ncfile
        if     (ncfile.eq.' ') then
          write(lp,*) 'ncfile ',ncfile
          write(lp,'(/3a/)')'error in horout_nc- ',Ename,' not defined'
          call flush(lp)
          stop 9
        endif
c
        call ncrange(array(1,1,kf),ii,jj,kl-kf+1, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c         create a new NetCDF and write data to it
c
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          if     (laxis) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Longitude", ii,pLonDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Y",         jj,pYDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "X",         ii,pXDimID))
          endif
          call ncheck(nf90_def_dim(ncfileID,"Layer",kl-kf+1,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdf2d"))
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdf2d"))
          endif
          ! create the variables and attributes
          call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                             MTDimID,MTVarID))
          if     (yrflag.eq.0) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "360_day"))
          elseif (yrflag.eq.1) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          elseif (yrflag.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          else
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 1900-12-31 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "standard"))
          endif
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          if     (artype.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          elseif (artype.eq.3) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          endif
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               pLatDimID,pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               pLonDimID,pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          else
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif
          if     (ltheta) then
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_float,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","SigmaTheta"))     
          else
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_int,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","layer"))     
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "positive","down"))
          endif
          ! model 3d variable
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Date"))
          else
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Longitude Latitude date"))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          call ncheck(nf90_put_var(ncfileID,MTVarID,time3(3)))
          call ncheck(nf90_put_var(ncfileID,datVarID,date   ))
          if     (laxis) then
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(1,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,1)))
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          if     (ltheta) then
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               theta(kf:kl)))
          else
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               (/(k, k=kf,kl)/)))
          endif
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:ii,1:jj,kf:kl)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c         Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          call ncheck(nf90_inq_dimid(ncfileID,"MT",    MTDimID))
          call ncheck(nf90_inq_dimid(ncfileID,"Layer",lyrDimID))
          if     (laxis) then
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Longitude",pLonDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Y",        pYDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "X",        pXDimID))
          endif
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Date"))
          else
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Longitude Latitude date"))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "units",trim(units)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! inquire variable ID
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:ii,1:jj,kf:kl)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)

      elseif (iotype.eq.5) then
c
c       grib-1 I/O
c
        call open_unit(io,frmt)

        read(22,*) gridlabel
        rewind(22)
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          print *,'calling 2ab2grib for ',name, namec, k
          CALL ab2grib ( array(1:ii,1:jj,k), ii,jj,
     &         fdate(1),fdate(2),fdate(3),fdate(4),verfhour,
     &          flag,ibms,namec,k,0,-1.0,gridlabel)

          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo





      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_3d - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop 9
      endif
      return
      end

      subroutine horout_3z(array,zz,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,units, kz, frmt,io,
     &                     fdate,verfhour)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
      include  "grib_block.h"
      include  "locale.inc"
      include  "clib.inc"
      integer	       ierr
      character*1      grib1
      integer  fdate(4)           ! yyyy, mm, dd, hh of forecast run
      integer  verfhour           ! verifying hour/lead
      INTEGER, save :: ibms = 1
      character*80 fname
      real, parameter :: flag = 2.0**100
c
      character*(*)    name,namec,units,frmt
      logical          lhycom, lexist
      integer          artype,yrflag,iexpt,kz,io,gridlabel
      double precision time3(3)
      real             array(ii,jj,kz),zz(kz)
c
c     write out a 3-d z-level array to unit io based on frmt.
c
c     3-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF I/O,
c       frmt=='netCDF'        for NetCDF I/O,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.< c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c       frmt=='NetCDF' for NetCDF I/O,
c       frmt=='NetCDF' for NetCDF I/O,
c       frmt=='netCDF' for NetCDF I/O,
c       frmt=='HYCOM'  for HYCOM .[ab] I/O,
c       frmt=='BIN'    for unformatted sequential I/O,
c       frmt=='(...)'  for   formatted sequential I/O with format frmt.
c
c     This version supports frmt=='netCDF' and needs 
c     version 3.5 of the NetCDF library, from: 
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l,iyear,month,iday,ihour
      real             :: hmin(999),hmax(999)
      double precision :: dt,dt0,year
c
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'jan','feb','mar','apr','may','jun',
     &                 'jul','aug','sep','oct','nov','dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'HYCOM')  then
c
c         HYCOM .[ab] I/O.
c
          call zbiost(ii,jj)
          iotype = 1
          write(lp,'(/a/)') 'horout_3z - HYCOM I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'BIN')    then
c
c         unformatted sequential I/O.
c
          iotype = 2
          write(lp,'(/a/)') 'horout_3z - unformatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:8).eq.'(2f10.4,' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O (lon lat value).
c
          iotype = -3
          write(lp,'(/a,a/)') 'horout - formatted sequential I/O',
     &                        ' (longitude latitude value)'
          call flush(lp)
        elseif (frmt(1:1).eq.'(' .and. frmt(l:l).eq.')') then
c
c         formatted sequential I/O.
c
          iotype = 3
          write(lp,'(/a/)') 'horout_3z - formatted sequential I/O'
          call flush(lp)
        elseif (frmt(1:l).eq.'netCDF' .or.
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          laxis = .true.
          do i= 2,ii
            laxis = laxis .and. 
     &              maxval(abs(plat(1,:)-plat(i,:))).le.1.e-2
          enddo
          do j= 2,jj
            laxis = laxis .and. 
     &              maxval(abs(plon(:,1)-plon(:,j))).le.1.e-2
          enddo
c
          iotype = 4
          if     (laxis) then
            write(lp,'(/2a/)') 'horout_3z - NetCDF I/O (lat/lon axes)'
          else
            write(lp,'(/2a/)') 'horout_3z - NetCDF I/O (curvilinear)'
          endif
          call flush(lp)
        elseif (frmt(1:l).eq.'grib1' .or.
     &          frmt(1:l).eq.'GRIB1') then
c
c         grib-1 format
c
          iotype = 5
          laxis = .false.
          write(lp,'(/a,a/)') 'horout - grib-1 I/O'
          call flush(lp)

        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_3z - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop 9
        endif
c
c       initialize label.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          if     (yrflag.lt.3) then
            write (label(51:72),112) time3(3)/year,cmonth(month),iday
          else
            write (label(51:72),113) cmonth(month),iday,iyear
          endif
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            dt0 = 15.0
          elseif (yrflag.eq.1) then
            dt0 = 15.25
          elseif (yrflag.eq.2) then
            dt0 = 0.0
          else
            dt0 = 0.0
          endif
          cell = (time3(2)+dt+dt0) - (time3(1)-dt+dt0)
          if     (artype.eq.2) then
            write(label(51:72),114) ' mean: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          else
            write(label(51:72),114) ' sdev: ',(time3(1)-dt+dt0)/year,
     &                                        (time3(2)+dt+dt0)/year
          endif
        endif
        if (lhycom) then
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (label(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 112    format ('  year',f7.2,' (',a3,i3.2,')')
 113    format ('  date: ',a3,i3.2,',',i5,'  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      if     (iotype.eq.1) then
c
c       HYCOM .[ab] I/O.
c
        call zbiopi(lopen, io)
        if     (.not.lopen) then
          call zbiopn('new', io)
          call zhopen(io, 'formatted', 'new', 0)
        endif
        call zbiowr3(array,kz,
     +               ip,.false., hmin,hmax, io, .false.)
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(io)
          write(lp,'(a,a,2g15.6)') label(33:81),':',hmin(k),hmax(k)
          call flush(lp)
        enddo
      elseif (iotype.eq.2) then
c
c       unformatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'unformatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.-3) then
c
c       formatted sequential I/O (lon lat value).
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          do j= 1,jj
            do i= 1,ii
              if     (array(i,j,k).ne.fill_value) then
                write(io,frmt) plon(i,j),plat(i,j),array(i,j,k)
              endif
            enddo
          enddo
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.3) then
c
c       formatted sequential I/O
c
        inquire(unit=io, opened=lopen)
        if     (.not.lopen) then
          call zhopen(io, 'formatted', 'new', 0)
        endif
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif (iotype.eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        write(*,*) 'ncfile ',ncfile
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')'error in horout_nc- ',Ename,' not defined'
          call flush(lp)
c          stop 9
        endif
c
        call ncrange(array,ii,jj,kz, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
          ! create a new NetCDF and write data to it
          call ncheck(nf90_create(trim(ncfile),nf90_noclobber,ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          if     (laxis) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Longitude", ii,pLonDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Y",         jj,pYDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "X",         ii,pXDimID))
          endif
          call ncheck(nf90_def_dim(ncfileID,"Z",kz,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.0"))
          if (lhycom) then
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "HYCOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM archive file"))
            elseif (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM mean archive file"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "source",
     &                                 "HYCOM std. archive file"))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archv2ncdf3z"))
          else
            ncenv = ' '
            call getenv('CDF_TITLE',ncenv)
            if     (ncenv.eq.' ') then
              ncenv = "MICOM"
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               trim(ncenv)))
            ncenv = ' '
            call getenv('CDF_INST',ncenv)
            if     (ncenv.ne.' ') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "institution",
     &                                 trim(ncenv)))
            endif
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "title",
     &                               "MICOM"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "source",
     &                               "MICOM archive file"))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "experiment",
     &                               label(75:78)))
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "history",
     &                               "archm2ncdf3z"))
          endif
          ! create the variables and attributes
          call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                             MTDimID,MTVarID))
          if     (yrflag.eq.0) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "360_day"))
          elseif (yrflag.eq.1) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          elseif (yrflag.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          else
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                               "days since 1900-12-31 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "standard"))
          endif
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          if     (artype.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "mean"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          elseif (artype.eq.3) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "cell_extent",
     &                               cell))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_methods",
     &                               "standard_deviation"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "cell_extent",
     &                               cell))
          endif
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               pLatDimID,pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               pLonDimID,pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          else
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif
          call ncheck(nf90_def_var(ncfileID,"Z", nf90_float,lyrDimID,
     &                             lyrVarID))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "units","m"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "positive","down"))
          ! model 3Z variable
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Date"))
          else
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Longitude Latitude date"))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          call ncheck(nf90_put_var(ncfileID,MTVarID,time3(3)))
          call ncheck(nf90_put_var(ncfileID,datVarID,date   ))
          if     (laxis) then
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(1,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,1)))
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          call ncheck(nf90_put_var(ncfileID,lyrVarID,zz(:)))
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:,:)))
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c        Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
          call ncheck(nf90_inq_dimid(ncfileID,"Z",lyrDimID))
          if     (laxis) then
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Longitude",pLonDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Y",        pYDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "X",        pXDimID))
          endif
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          if     (laxis) then
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Date"))
          else
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "coordinates",
     &                               "Longitude Latitude date"))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! inquire variable ID
          call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
          !write values into array
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:,:)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      elseif (iotype.eq.5) then
c
c       grib-1 I/O
c
        call open_unit(io,frmt)

        read(22,*) gridlabel
        rewind(22)
        do k= 1,kz
          if     (zz(k).le.9999.99) then
            write(label(33:50),'(a,f7.2,a)') 'z=',zz(k),name
          else
            write(label(33:50),'(a,f8.1,a)') 'z=',zz(k),name
          endif
          print *,'calling 3ab2grib for ',name, namec, k
          CALL ab2grib ( array(1:ii,1:jj,k), ii,jj,
     &         fdate(1),fdate(2),fdate(3),fdate(4),verfhour,
     &          flag,ibms,namec,k,1,zz(k),gridlabel)
        enddo

      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_3z - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop 9
      endif
      return
      end

      subroutine ncheck(status)
      use mod_xc   ! HYCOM communication API
      use netcdf   ! NetCDF fortran 90 interface
      implicit none
c
      integer, intent(in) :: status
c
c     subroutine to handle NetCDF errors
c
      if (status /= nf90_noerr) then
        write(lp,'(/a)')   'error in horout - from NetCDF library'
        write(lp,'(a/)')   trim(nf90_strerror(status))
        call flush(lp)
        stop 9
      end if
      end subroutine ncheck

      subroutine ncrange(h,ii,jj,kk, fill_value, hmin,hmax)
      implicit none
c
      integer, intent(in ) :: ii,jj,kk
      real,    intent(in ) :: h(ii,jj,kk),fill_value
      real,    intent(out) :: hmin,hmax
c
c     return range of array, ignoring fill_value
c
      integer i,j,k
      real    hhmin,hhmax
c
      hhmin =  abs(fill_value)
      hhmax = -abs(fill_value)
      do k= 1,kk
        do j= 1,jj
          do i= 1,ii
            if     (h(i,j,k).ne.fill_value) then
              hhmin = min(hhmin,h(i,j,k))
              hhmax = max(hhmax,h(i,j,k))
            endif
          enddo
        enddo
      enddo
      hmin = hhmin
      hmax = hhmax
      end subroutine ncrange
