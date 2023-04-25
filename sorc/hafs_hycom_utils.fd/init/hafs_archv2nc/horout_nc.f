      subroutine horout_y(array,
     &                  artype,yrflag,time3,iexpt,lhycom,
     &                  name,namec,names,units, frmt,io)
c----------------------------------------------------------------------------
C  wrapper to collect distributed array into a single array on proc 1
C  and
C  netCDF it out.
C  Dan Moore - QinetiQ - August 2010
C  serial version
C  Alexandra Bozec - COAPS/FSU - August 2016  (horout_yz; horout_z)
c----------------------------------------------------------------------------
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,lexist
      integer          artype,yrflag,iexpt,io,l
      double precision time3(3)
      real             array(jdm)
      integer          zloc(1)
c
      real, allocatable :: zlat(:)

      allocate(zlat(jdm))

      ! get the latitude in a 1-dim array
      zloc=maxloc(plat(:,jdm))
      do l=1,jdm
        zlat(l)=plat(zloc(1),l) !! column of the highest latitude
      enddo

      call horout_y_mpi(array,artype,yrflag,time3,
     & iexpt,lhycom,name,namec,names,units, frmt,io,jdm,zlat,
     &        lp,nstep)

      deallocate(zlat)


      return
      end

      subroutine horout_y_mpi(array,artype,yrflag,time3,
     & iexpt,lhycom,name,namec,names,units, frmt,io,jj,flat,
     &        lp,nstep)
      use netcdf   ! NetCDF Interface

      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom, lexist
      integer          artype,yrflag,iexpt,io,jj,lp,nstep
      double precision time3(3)
      real   array(jj),flat(jj)
c
c     write out a 1-d z-level array to unit io based on frmt.
c
c     1-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF-4 classic I/O,
c       frmt=='netCDF'        for NetCDF-4 classic I/O,
c
c     This version supports frmt=='netCDF' and needs
c     version 4.3 or later of the NetCDF library, from:
c     http://www.unidata.ucar.edu/packages/netcdf/
c
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,tauVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6,cdate*19
c
      logical          :: lopen
      integer          :: i,j,k,l
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999),alon
      double precision :: dt,tau,yr0,year
      integer*2        :: m_value
      integer          :: navo_code
      real             :: scale_f,add_off
      character*240    :: namecv,namelv
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
c        initialization.
c
        l = len_trim(frmt)
c
c         NetCDF I/O.
cc
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        write(lp,*) 'ihour =',ihour
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')

c
        label = labeli
cc
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange_1d(array,jj, fill_value, hmin(1),hmax(1))
c
        inquire(file= ncfile, exist=lexist)

        if (.not.lexist) then
            ! create a new NetCDF and write data to it
            ! netcdf-4 classic model, netcdf version 4.3 and later
            call nchek('nf90_create',
     &                  nf90_create(trim(ncfile),
     &                              or(nf90_clobber,
     &                                 or(nf90_hdf5,
     &                                    nf90_classic_model)),
     &                              ncfileID))
            ! define the dimensions
            call nchek('nf90_def_dim( MT',
     &                  nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))


              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "HYCOM"
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "title",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "experiment",
     &                                 label(75:78)))
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf3z"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif


          ! create the variables and attributes
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "standard"))
            endif

            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((flat(jj)-flat(1))-
     &                  (flat(2)-flat(1))*(jj-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
          ! model 3Z variable


              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLatDimID,MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &        "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &        "valid_range",
     &        (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &            "long_name",
     &            trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &            "long_name",
     &            trim(name)//label(73:81)))
          else
              call ncheck(nf90_put_att(ncfileID,varID,
     &            "long_name",
     &            trim(name)//label(51:55)//label(73:81)))
          endif

          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables

            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 (/flat(:)/)))     !1-d Latitudes

          ! write to model variable
           call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:)))

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
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))

          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable


              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLatDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",fill_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "valid_range",
     &                               (/hmin(1), hmax(1)/)))
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            else
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                   trim(name)//label(51:55)//label(73:81)))
            endif

          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
            ! inquire variable ID
            call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
            ! write to model variable
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:)))

          ! close file
          call ncheck(nf90_close(ncfileID))
      endif

        write(lp,'(a49,a,2g15.6)')
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      return
      end

      subroutine horout_yz(array,zz,
     &                  artype,yrflag,time3,iexpt,lhycom,
     &                  name,namec,names,units, k, frmt,io)
c----------------------------------------------------------------------------
C  wrapper to collect distributed array into a single array on proc 1
C  and
C  netCDF it out.
C  Dan Moore - QinetiQ - August 2010
C  serial version
C  Alexandra Bozec - COAPS/FSU - August 2016  (horout_yz; horout_z)
c----------------------------------------------------------------------------
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,k,io,l
      double precision time3(3)
      real             array(jdm,k)
      real             zz(k)
      integer          zloc(1)
c
      real, allocatable :: zlat(:)

      allocate(zlat(jdm))
      ! get the latitude in a 1-dim array
      zloc=maxloc(plat(:,jdm))
      do l=1,jdm
        zlat(l)=plat(zloc(1),l) !! column of the highest latitude
      enddo

      call horout_yz_mpi(array,zz,artype,yrflag,time3,
     & iexpt,lhycom,name,namec,names,units, k, frmt,io,jdm,zlat,
     &        lp,nstep)

      deallocate(zlat)

      return

      end

      subroutine horout_yz_mpi(array,zz,artype,yrflag,time3,
     & iexpt,lhycom,name,namec,names,units, kz, frmt,io,jj,flat,
     &        lp,nstep)
      use netcdf   ! NetCDF Interface

      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom, lexist
      integer          artype,yrflag,iexpt,kz,io,ii,jj,lp,nstep,nk
      double precision time3(3)
      real   array(jj,kz),zz(kz),flat(jj)
c
c     write out a 2-d z-level array to unit io based on frmt.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF-4 classic I/O,
c       frmt=='netCDF'        for NetCDF-4 classic I/O,
c
c     This version supports frmt=='netCDF' and needs
c     version 4.3 or later of the NetCDF library, from:
c     http://www.unidata.ucar.edu/packages/netcdf/
c
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,tauVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6,cdate*19
c
      logical          :: lopen
      integer          :: i,j,k,l
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999),alon
      double precision :: dt,tau,yr0,year
      integer*2        :: m_value
      integer          :: navo_code
      real             :: scale_f,add_off
      character*240    :: namecv,namelv
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
c        initialization.
c
        l = len_trim(frmt)
c
c         NetCDF I/O.
c
        laxis = .true.
        iotype=4
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        write(lp,*) 'ihour =',ihour
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')

c
        label = labeli
c
        if (abs(iotype).eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '

        call getenv(Ename,ncfile)

        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange_2d(array,jj,kz, fill_value, hmin(1),hmax(1))
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
          ! create a new NetCDF and write data to it
          ! netcdf-4 classic model, netcdf version 4.3 and later
          call nchek('nf90_create',
     &                nf90_create(trim(ncfile),
     &                            or(nf90_clobber,
     &                               or(nf90_hdf5,
     &                                  nf90_classic_model)),
     &                            ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA or NAVO
            call nchek('nf90_def_dim( MT',
     &                  nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
          endif
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Depth",  kz,lyrDimID))

            if     (iotype.eq.4) then !standard
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "HYCOM"
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "title",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "experiment",
     &                                 label(75:78)))
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf3z"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
          endif
          ! create the variables and attributes
          if     (iotype.eq.4) then !not for MERSEA or NAVO
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "standard"))
            endif

            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
        endif !not MERSEA or NAVO
            call ncheck(nf90_def_var(ncfileID,"Depth", nf90_float,
     &                               lyrDimID,lyrVarID))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "standard_name","depth"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "units","m"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "positive","down"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))

              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((flat(jj)-flat(1))-
     &                  (flat(2)-flat(1))*(jj-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
          ! model 3Z variable


              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &        "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &        "valid_range",
     &        (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &            "long_name",
     &            trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &            "long_name",
     &            trim(name)//label(73:81)))
          else
              call ncheck(nf90_put_att(ncfileID,varID,
     &            "long_name",
     &            trim(name)//label(51:55)//label(73:81)))
          endif


          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables

            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
          if     (laxis) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 (/flat(:)/)))     !1-d Latitudes
              call nchek('nf90_put_var( zz',
     &                    nf90_put_var(ncfileID,lyrVarID,zz(:)))
          endif
          ! write to model variable
           call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:,:)))

          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
      else
c
c        Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          if     (iotype.eq.4) then !standard
            call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
            call ncheck(nf90_inq_dimid(ncfileID,"Depth",lyrDimID))
          endif

            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "Latitude",pLatDimID))

          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable


              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",fill_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "valid_range",
     &                               (/hmin(1), hmax(1)/)))
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            else
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                   trim(name)//label(51:55)//label(73:81)))
            endif


          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
            ! inquire variable ID
            call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
            ! write to model variable
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:,:)))

          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)')
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
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
        stop
      endif
      return
      end


      subroutine horout(array,
     &                  artype,yrflag,time3,iexpt,lhycom,
     &                  name,namec,names,units, k,ltheta, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF fortran 90 interface
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,k,io
      double precision time3(3)
      real             array(ii,jj)
c
c     write out array to unit io based on frmt.
c
c     array size and frmt        must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c     io may be modified by this subroutine
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF deflation level (if any) is taken from environment
c      variable CDF_DEFLATE.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c     NAVO convention: public release notice turned on by environment
c      variable CDF_PUBLIC.
c     NAVO convention: hours since analysis is taken from environment
c      variable CDF_TAU.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF-4 classic I/O,
c       frmt=='netCDF'        for NetCDF-4 classic I/O,
c       frmt=='MERSEA'        for NetCDF-4 classic I/O with MERSEA conventions,
c       frmt=='NAVO'          for NetCDF-4 classic I/O with NAVO   conventions
c       frmt=='NAVOlike'      for NetCDF-4 classic I/O consistent with NAVO but
c                                  using float arrays and HYCOM names,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version supports frmt=='netCDF' ('MERSEA','NAVO') and needs
c     version 4.3 or later of the NetCDF library, from:
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer*2, allocatable :: array2(:,:)
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,tauVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6,cdate*19
c
      logical          :: lopen,lugrid,lvgrid
      integer          :: i,j,l,iosave
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin,hmax,alon
      double precision :: dt,tau,yr0,year
      integer*2        :: m_value
      integer          :: navo_code
      real             :: scale_f,add_off
      character*240    :: namecv,namelv
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      integer,          save :: deflate =  0
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      iosave = io
      lugrid = .false.
      lvgrid = .false.
      if     (io.lt.0) then
        io = -io
        if     (namec(1:1).eq.'u') then
          lugrid = .true.
        elseif (namec(1:1).eq.'v') then
          lvgrid = .true.
        endif
      endif
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
        elseif (frmt(1:l).eq.'MERSEA') then
c
c         NetCDF I/O, with MERSEA layout.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout - ',
     &        'MERSEA requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -4
          write(lp,'(/2a/)') 'horout - MERSEA I/O (lat/lon axes)'
          call flush(lp)
        elseif (frmt(1:l).eq.'NAVO') then
c
c         NetCDF I/O, with NAVO layout.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout - ',
     &        'NAVO requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -5
          write(lp,'(/2a/)') 'horout - NAVO I/O (lat/lon axes)'
          call flush(lp)
        elseif (frmt(1:l).eq.'NAVOlike') then
c
c         NetCDF I/O, with NAVO layout but HYCOM names.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout - ',
     &        'NAVO requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -6
          write(lp,'(/2a/)') 'horout - NAVOlike I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       deflation level
c
        ncenv = ' '
        call getenv('CDF_DEFLATE',ncenv)
        if     (ncenv.eq.' ') then
          deflate = 0
        else
          read(ncenv,*) deflate
        endif
        write(lp,*) 'deflate  = ',deflate
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        if     (time3(1).lt.0.0) then
          labeli(51:72) = ''
        else
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        endif  !time3 >= 0
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
c     complete the label
c
      write(lp,'(a,i4)') 'long_name: k = ',k
      call flush(lp)
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
      if     (k.le.0) then
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
      elseif(abs(iotype).eq.4 .or. iotype.le.-5) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
*       write(lp,'(3a)') trim(Ename),' = ',trim(ncfile)
*       call flush(lp)
c
        call ncrange(array,ii,jj,1, fill_value, hmin,hmax)
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c         create a new NetCDF and write data to it
c         netcdf-4 classic model, netcdf version 4.3 and later
c
          call nchek('nf90_create',
     &                nf90_create(trim(ncfile),
     &                            or(nf90_clobber,
     &                               or(nf90_hdf5,
     &                                  nf90_classic_model)),
     &                            ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA  or NAVO
            if     (time3(1).ge.0.0) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
            endif  !time3 >= 0
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "longitude", ii,pLonDimID))
          elseif (iotype.le.-5) then !NAVO,NAVOlike
            write(lp,*) 'lat = jj = ',jj
            write(lp,*) 'lon = ii = ',ii
            call nchek('nf90_def_dim( lat',
     &                  nf90_def_dim(ncfileID,
     &                               "lat",       jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "lon",       ii,pLonDimID))
          elseif (laxis) then
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
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call nchek('nf90_def_dim( time',
     &                  nf90_def_dim(ncfileID,
     &                               "time", nf90_unlimited,MTDimID))
          endif
          ! create the global attributes
          if     (iotype.ne.-5) then !except NAVO
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "Conventions",
     &                               "CF-1.6"))
          endif
          if (lhycom) then
            if     (iotype.eq.4) then !standard
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "HYCOM"
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "title",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "experiment",
     &                                 label(75:78)))
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf2d"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
              if     (lugrid) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","u-grid"))
              elseif (lvgrid) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","v-grid"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","p-grid"))
              endif
            elseif (iotype.eq.-4) then !MERSEA
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "HYCOM"
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "title",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "experiment",
     &                                 label(75:78)))
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf2d"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
              if     (lugrid) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","u-grid"))
              elseif (lvgrid) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","v-grid"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","p-grid"))
              endif
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              write(ncenv,
     &          '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &          iyear,month,iday,ihour
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "field_date",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('MERSEA_B_DATE',ncenv)
              if     (ncenv.eq.'TODAY') then
                write(ncenv,
     &            '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &            iyear,month,iday,ihour
              endif
              if     (ncenv.ne.' ') then
                read(ncenv,'(i4,1x,i2,1x,i2,1x,i2)')
     &            iyrms,monms,idms,ihrms
                if     (iyrms.lt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "forecast"))
                elseif (iyrms.gt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "hindcast"))
                else   !iyrms.eq.iyear
                  if     (monms.lt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "forecast"))
                  elseif (monms.gt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "hindcast"))
                  else   !monms.eq.month
                    if     (idms.lt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "forecast"))
                    elseif (idms.gt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "hindcast"))
                    else   !idms.eq.iday
                      if     (ihrms.lt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "forecast"))
                      elseif (ihrms.gt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "hindcast"))
                      else   !ihrms.eq.ihour
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "nowcast"))
                      endif  !ihrms
                    endif !idms
                  endif !monms
                endif !iyrms
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_date",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('MERSEA_B_TYPE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_type",
     &                                   trim(ncenv)))
              endif
            elseif (iotype.le.-5) then !NAVO,NAVOlike
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "title",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('CDF_PUBLIC',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "classification_level",
     &                                   "UNCLASSIFIED"))
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "distribution_statement",
     &      "Approved for public release; distribution unlimited."))
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "downgrade_date",
     &                                   "not applicable"))
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "classification_authority",
     &                                   "not applicable"))
              endif !PUBLIC
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call nchek('nf90_put_att( institution',
     &                      nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf2d"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
              if     (lugrid) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","u-grid"))
              elseif (lvgrid) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","v-grid"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "comment","p-grid"))
              endif
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              if     (iotype.eq.-5) then !NAVO only
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "Conventions",
     &                                   "CF-1.6 NAVO_netcdf_v1.1"))
              endif
            endif !iotype
          else !micom
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
          if     (iotype.eq.4) then !not for MERSEA or NAVO
            if     (time3(1).ge.0.0) then
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "gregorian"))  !same as standard
            endif
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
            endif  !time3 >= 0
          endif !not MERSEA or NAVO
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call nchek('nf90_def_var( time',
     &                  nf90_def_var(ncfileID,"time",  nf90_double,
     &                               MTDimID,MTVarID))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name","Valid Time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "hours since 2000-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "time_origin",
     &                          "2000-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "gregorian"))  !same as standard
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            if     (iotype.eq.-5) then !NAVO only
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "NAVO_code",13))
            endif
c
            ncenv = ' '
            call getenv('CDF_TAU',ncenv)
            if     (ncenv.ne.' ') then
              read(ncenv,*) tau
              call nchek('nf90_def_var( time',
     &                    nf90_def_var(ncfileID,"tau",  nf90_double,
     &                                 MTDimID,tauVarID))
              call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                 "long_name","Tau"))
              call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                 "units",
     &                                 "hours since analysis"))
              call fordate(time3(3)-tau/24.d0,3, iyrms,monms,idms,ihrms)
              write(cdate,"(i4.4,'-',i2.2,'-',i2.2,' ',i2.2,':00:00')")
     &          iyrms,monms,idms,ihrms
              call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                 "time_origin",
     &                                 cdate))
              if     (iotype.eq.-5) then !NAVO only
                call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                   "NAVO_code",56))
              endif
            endif
          endif !NAVO
          if     (laxis) then
            if     (iotype.le.-5) then !NAVO,NAVOlike
              call nchek('nf90_def_var( lat',
     &                    nf90_def_var(ncfileID,"lat",  nf90_double,
     &                                 pLatDimID,pLatVarID))
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "long_name","Latitude"))
            elseif (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
            if     (iotype.eq.-5) then !NAVO only
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "NAVO_code",1))
            endif
            if     (iotype.le.-5) then !NAVO,NAVOlike
              call nchek('nf90_def_var( lon',
     &                    nf90_def_var(ncfileID,"lon", nf90_double,
     &                                 pLonDimID,pLonVarID))
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "long_name","Longitude"))
            elseif (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            write(lp,*) 'plon all = ',(plon(ii,1)-plon(1,1))
            write(lp,*) 'plon 1st = ',(plon( 2,1)-plon(1,1))*(ii-1),
     &                                (plon( 2,1)-plon(1,1))
            write(lp,*) 'plon err = ',
     &              abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)),(ii-1)
            call flush(lp)
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "axis","X"))
            if     (iotype.eq.-5) then !NAVO only
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "NAVO_code",2))
            endif
          else !.not.laxis
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "axis","Y"))
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "axis","X"))
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
           if     (deflate.gt.0) then
              call ncheck(nf90_def_var_deflate(ncfileID,pLatVarID,
     &                                         shuffle=1,deflate=1,
     &                                         deflate_level=deflate))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            if     (deflate.gt.0) then
              call ncheck(nf90_def_var_deflate(ncfileID,pLonVarID,
     &                                         shuffle=1,deflate=1,
     &                                         deflate_level=deflate))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif !laxis:else
          ! model 2d variable
          if     (iotype.eq.4) then !standard
            if     (time3(1).ge.0.0) then
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,   MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
            else  !no time axis
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID/),
     &                                 varID))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude"))
            endif
            endif  !time3 >= 0;else
          elseif (iotype.eq.-5) then !NAVO only
            if     (names.ne." ") then
              namelv = ''  !default is no long name 
            else
              namelv = name
            endif
            if     (namec.eq.'ssh') then
              namecv    = 'surf_el'
              namelv    = 'Water Surface Elevation'
              navo_code = 32
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'u_barotropic_velocity') then
              namecv    = 'water_bu'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'v_barotropic_velocity') then
              namecv    = 'water_bv'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'bot_u') then
              namecv    = 'water_u_bottom'
              namelv    = 'Eastward Water Velocity'
              navo_code = 17
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'bot_v') then
              namecv    = 'water_v_bottom'
              namelv    = 'Northward Water Velocity'
              navo_code = 18
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'bot_temperature') then
              namecv    = 'water_temp_bottom'
              namelv    = 'Water Temperature'
              navo_code = 15
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'bot_salinity') then
              namecv    = 'salinity_bottom'
              namelv    = 'Salinity'
              navo_code = 16
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'sst' .or.
     &              namec.eq.'sss'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'ssu'   .or.
     &              namec.eq.'ssv'   .or.
     &              namec.eq.'siu'   .or.
     &              namec.eq.'siv'   .or.
     &              namec.eq.'surtx' .or.
     &              namec.eq.'surty' .or.
     &              namec.eq.'uocn'  .or.
     &              namec.eq.'vocn'  .or.
     &              namec.eq.'uvel'  .or.
     &              namec.eq.'vvel'      ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'sih' .or.
     &              namec.eq.'hi'  .or.
     &              namec.eq.'hs'      ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'sic'  .or.
     &              namec.eq.'aice'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.0001
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'albsni') then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.004
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'frzmlt'     .or.
     &              namec.eq.'fswdn'      .or.
     &              namec.eq.'flwdn'      .or.
     &              namec.eq.'flwup_ai'   .or.
     &              namec.eq.'fswabs_ai'  .or.
     &              namec.eq.'flat_ai'    .or.
     &              namec.eq.'fsens_ai'   .or.
     &              namec.eq.'fhocn_ai'   .or.
     &              namec.eq.'fswthru_ai'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.1
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'Tsfc'  .or.
     &              namec.eq.'Tair'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.002
              if     (units.eq.'degC') then
                add_off   = 0.0    !maintains accuracy at zero degC
              elseif (units.eq.'K') then
                add_off   = 273.15 !maintains accuracy at zero degC
              else
                write(lp,'(/4a/)') 
     &            'error in horout - wrong units (',
     &            trim(units),
     &            ') for ',trim(namec)
                call flush(lp)
                stop
              endif
            elseif (namec.eq.'snow_ai' .or.
     &              namec.eq.'rain_ai' .or.
     &              namec.eq.'congel'  .or.
     &              namec.eq.'frazil'  .or.
     &              namec.eq.'snoice'  .or.
     &              namec.eq.'meltt'   .or.
     &              namec.eq.'meltb'   .or.
     &              namec.eq.'meltl'   .or.
     &              namec.eq.'evap_ai' .or.
     &              namec.eq.'fresh_ai'    ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'strength') then
              namecv    = namec
              navo_code =  0
              scale_f   = 40.0
              add_off   =  0.0   !maintains accuracy at zero
            elseif (namec.eq.'opening' .or. 
     &              namec.eq.'divu'        ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 1.0
              add_off   = 0.0   !maintains accuracy at zero
            elseif (namec.eq.'fswfac') then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.0001
              add_off   = 1.0    !maintains accuracy at one
            elseif (namec.eq.'straire' .or.
     &              namec.eq.'strairn' .or.
     &              namec.eq.'strcore' .or.
     &              namec.eq.'strcorn' .or.
     &              namec.eq.'strocne' .or.
     &              namec.eq.'strocnn'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'fsalt_ai') then
              namecv    = namec
              navo_code = 0
              add_off   = 0.0    !maintains accuracy at zero
              if     (units.eq.'kg/m^2/s') then
                scale_f   = 1.e-8
              elseif (units.eq.'kg/m^2/day') then
                scale_f   = 0.001
              else
                write(lp,'(/4a/)') 
     &            'error in horout - wrong units (',
     &            trim(units),
     &            ') for ',trim(namec)
                call flush(lp)
                stop
              endif
            else
              write(lp,'(/a)')   'error in horout - unknown name'
              write(lp,'(3a)')   'namec  = "',trim(namec),'"'
              write(lp,'(a,i4)') 'io     = ',io
              write(lp,'(a,i4)') 'iotype = ',iotype
              call flush(lp)
              stop
            endif
            m_value   = -30000
            call nchek('nf90_def_var( namecv',
     &                  nf90_def_var(ncfileID,trim(namecv),nf90_short,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                               varID))
            if     (namelv.ne." ") then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",trim(namelv)))
            endif
          elseif (iotype.eq.-6) then !NAVOlike
            call nchek('nf90_def_var( namec',
     &                  nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                               varID))
          else !MERSEA      
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID/),
     &                               varID))
          endif                                                         
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if      (iotype.eq.-5) then !NAVO only
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "missing_value",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "scale_factor",scale_f))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "add_offset",add_off))
            if     (navo_code.ne.0) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "NAVO_code",navo_code))
            endif
            if     (navo_code.eq.15) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "comment","in-situ temperature"))
            endif
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",fill_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "valid_range",
     &                               (/hmin, hmax/)))
            write(lp,'(a,i4)') 'long_name: k = ',k
            call flush(lp)
            if     (k.lt.0) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 name))
            elseif (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 label(33:50)//label(73:81)))
            elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 label(33:50)//label(73:81)))
            else
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 label(33:55)//label(73:81)))
            endif
          endif !NAVO:else
          if     (deflate.gt.0) then
            call ncheck(nf90_def_var_deflate(ncfileID,VarID,
     &                                       shuffle=1,deflate=1,
     &                                       deflate_level=deflate))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          if     (iotype.eq.4) then !standard
            if     (time3(1).ge.0.0) then
              call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
              call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
            endif
          elseif (iotype.le.-5) then !NAVO,NAVOlike
            call ncheck(nf90_put_var(ncfileID,MTVarID,
     &                               (time3(3)-36160.d0)*24.d0)) !hrs since 2000
c
            ncenv = ' '
            call getenv('CDF_TAU',ncenv)
            if     (ncenv.ne.' ') then
              read(ncenv,*) tau
              call ncheck(nf90_put_var(ncfileID,tauVarID,tau))
            endif
          endif
          if     (laxis) then
            if (iotype.le.-5) then !NAVO,NAVOlike
              if     (lugrid) then
                call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                              dble((/ulat(1,:)/))))    !1-d Latitudes
                call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                              dble((/ulon(:,1)/))))    !1-d Longtudes
                write(lp,'(/a,i4/)') 'horout - u-grid: io =',io
                call flush(lp)
              elseif (lvgrid) then
                call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                              dble((/vlat(1,:)/))))    !1-d Latitudes
                call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                              dble((/vlon(:,1)/))))    !1-d Longtudes
                write(lp,'(/a,i4/)') 'horout - v-grid: io =',io
                call flush(lp)
              else
                call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                              dble((/plat(1,:)/))))    !1-d Latitudes
                call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                              dble((/plon(:,1)/))))    !1-d Longtudes
                write(lp,'(/a,i4/)') 'horout - p-grid: io =',io
                call flush(lp)
              endif
            else
              if     (lugrid) then
                call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                   (/ulat(1,:)/)))     !1-d Latitudes
                call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                   (/ulon(:,1)/)))     !1-d Longtudes
                write(lp,'(/a,i4/)') 'horout - u-grid: io =',io
                call flush(lp)
              elseif (lvgrid) then
                call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                   (/vlat(1,:)/)))     !1-d Latitudes
                call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                   (/vlon(:,1)/)))     !1-d Longtudes
                write(lp,'(/a,i4/)') 'horout - v-grid: io =',io
                call flush(lp)
              else
                call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                   (/plat(1,:)/)))     !1-d Latitudes
                call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                   (/plon(:,1)/)))     !1-d Longtudes
                write(lp,'(/a,i4/)') 'horout - p-grid: io =',io
                call flush(lp)
              endif
            endif
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            if     (lugrid) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,ulat(:,:)))
              if     (maxval(ulon(1:ii,1:jj)).gt.180.1) then
                do j= 1,jj
                  do i= 1,ii
                    alon = mod(ulon(i,j)+1080.0,360.0)
                    if     (alon.ge.180.0) then
                      alon = alon - 360.0
                    endif
                    ulon(i,j) = alon  !between -180E and 180E
                  enddo !i
                enddo !j
              endif
              call ncheck(nf90_put_var(ncfileID,pLonVarID,ulon(:,:)))
              write(lp,'(/a,i4/)') 'horout - u-grid: io =',io
              call flush(lp)
            elseif (lvgrid) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,vlat(:,:)))
              if     (maxval(vlon(1:ii,1:jj)).gt.180.1) then
                do j= 1,jj
                  do i= 1,ii
                    alon = mod(vlon(i,j)+1080.0,360.0)
                    if     (alon.ge.180.0) then
                      alon = alon - 360.0
                    endif
                    vlon(i,j) = alon  !between -180E and 180E
                  enddo !i
                enddo !j
              endif
              call ncheck(nf90_put_var(ncfileID,pLonVarID,vlon(:,:)))
              write(lp,'(/a,i4/)') 'horout - v-grid: io =',io
              call flush(lp)
            else
              call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
              if     (maxval(plon(1:ii,1:jj)).gt.180.1) then
                do j= 1,jj
                  do i= 1,ii
                    alon = mod(plon(i,j)+1080.0,360.0)
                    if     (alon.ge.180.0) then
                      alon = alon - 360.0
                    endif
                    plon(i,j) = alon  !between -180E and 180E
                  enddo !i
                enddo !j
              endif
              call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
              write(lp,'(/a,i4/)') 'horout - p-grid: io =',io
              call flush(lp)
            endif !grid
          endif
          ! write to model variable
          if (iotype.eq.-5) then !NAVO only
            allocate( array2(ii,jj) )
            do j= 1,jj
              do i= 1,ii
                if     (array(i,j).ne.fill_value) then
c                 nint is i*4, min:max prevents i*2 overflow
                  array2(i,j) = max( -2**15, min( 2**15-1,
     &                            nint((array(i,j)-add_off)/scale_f) ) )
                else
                  array2(i,j) = m_value
                endif
              enddo !i
            enddo !j
*             write(lp,*) 'aij = ',
*    &          array( ii/2,jj/2),
*    &          array2(ii/2,jj/2),
*    &          array2(ii/2,jj/2)*scale_f+add_off
            call nchek('nf90_put_var( array2',
     &                  nf90_put_var(ncfileID,varID,array2(:,:)))
            deallocate( array2 )
          else
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:,:)))
          endif
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c          Write data to existing NetCDF file
c
          ! open NetCDF file
          call nchek('nf90_open',
     &                nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          if     (iotype.eq.4) then !standard
            if     (time3(1).ge.0.0) then
            call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
            endif  !time3 >= 0
          elseif (iotype.le.-5) then !NAVO,NAVOlike
            call ncheck(nf90_inq_dimid(ncfileID,"time",MTDimID))
          endif
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "lat",     pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "lon",     pLonDimID))
          elseif (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "longitude",pLonDimID))
          elseif (laxis) then
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
          if     (iotype.eq.4) then !standard
            if     (time3(1).ge.0.0) then
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID,   MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
            else  !no time axis
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID/),
     &                                 varID))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pXDimID,   pYDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude"))
            endif
            endif  !time3 >= 0;else
          elseif (iotype.eq.-5) then !NAVO only
            if     (names.ne." ") then
              namelv = ''  !default is no long name 
            else
              namelv = name
            endif
            if     (namec.eq.'ssh') then
              namecv    = 'surf_el'
              namelv    = 'Water Surface Elevation'
              navo_code = 32
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'u_barotropic_velocity') then
              namecv    = 'water_bu'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'v_barotropic_velocity') then
              namecv    = 'water_bv'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'bot_u') then
              namecv    = 'water_u_bottom'
              namelv    = 'Eastward Water Velocity'
              navo_code = 17
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'bot_v') then
              namecv    = 'water_v_bottom'
              namelv    = 'Northward Water Velocity'
              navo_code = 18
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'bot_temperature') then
              namecv    = 'water_temp_bottom'
              namelv    = 'Water Temperature'
              navo_code = 15
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'bot_salinity') then
              namecv    = 'salinity_bottom'
              namelv    = 'Salinity'
              navo_code = 16
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'sst' .or.
     &              namec.eq.'sss'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'ssu'   .or.
     &              namec.eq.'ssv'   .or.
     &              namec.eq.'siu'   .or.
     &              namec.eq.'siv'   .or.
     &              namec.eq.'surtx' .or.
     &              namec.eq.'surty' .or.
     &              namec.eq.'uocn'  .or.
     &              namec.eq.'vocn'  .or.
     &              namec.eq.'uvel'  .or.
     &              namec.eq.'vvel'      ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'sih' .or.
     &              namec.eq.'hi'  .or.
     &              namec.eq.'hs'      ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'sic'  .or.
     &              namec.eq.'aice'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.0001
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'albsni') then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.004
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'frzmlt'     .or.
     &              namec.eq.'fswdn'      .or.
     &              namec.eq.'flwdn'      .or.
     &              namec.eq.'flwup_ai'   .or.
     &              namec.eq.'fswabs_ai'  .or.
     &              namec.eq.'flat_ai'    .or.
     &              namec.eq.'fsens_ai'   .or.
     &              namec.eq.'fhocn_ai'   .or.
     &              namec.eq.'fswthru_ai'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.1
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'Tsfc'  .or.
     &              namec.eq.'Tair'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.002
              if     (units.eq.'degC') then
                add_off   = 0.0    !maintains accuracy at zero degC
              elseif (units.eq.'K') then
                add_off   = 273.15 !maintains accuracy at zero degC
              else
                write(lp,'(/4a/)') 
     &            'error in horout - wrong units (',
     &            trim(units),
     &            ') for ',trim(namec)
                call flush(lp)
                stop
              endif
            elseif (namec.eq.'snow_ai' .or.
     &              namec.eq.'rain_ai' .or.
     &              namec.eq.'congel'  .or.
     &              namec.eq.'frazil'  .or.
     &              namec.eq.'snoice'  .or.
     &              namec.eq.'meltt'   .or.
     &              namec.eq.'meltb'   .or.
     &              namec.eq.'meltl'   .or.
     &              namec.eq.'evap_ai' .or.
     &              namec.eq.'fresh_ai'    ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0    !maintains accuracy at zero
            elseif (namec.eq.'strength') then
              namecv    = namec
              navo_code =  0
              scale_f   = 40.0
              add_off   =  0.0   !maintains accuracy at zero
            elseif (namec.eq.'opening' .or. 
     &              namec.eq.'divu'        ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 1.0
              add_off   = 0.0   !maintains accuracy at zero
            elseif (namec.eq.'fswfac') then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.0001
              add_off   = 1.0    !maintains accuracy at one
            elseif (namec.eq.'straire' .or.
     &              namec.eq.'strairn' .or.
     &              namec.eq.'strcore' .or.
     &              namec.eq.'strcorn' .or.
     &              namec.eq.'strocne' .or.
     &              namec.eq.'strocnn'     ) then
              namecv    = namec
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'fsalt_ai') then
              namecv    = namec
              navo_code = 0
              add_off   = 0.0    !maintains accuracy at zero
              if     (units.eq.'kg/m^2/s') then
                scale_f   = 1.e-8
              elseif (units.eq.'kg/m^2/day') then
                scale_f   = 0.001
              else
                write(lp,'(/4a/)') 
     &            'error in horout - wrong units (',
     &            trim(units),
     &            ') for ',trim(namec)
                call flush(lp)
                stop
              endif
            else
              write(lp,'(/a)')   'error in horout - unknown name'
              write(lp,'(3a)')   'namec  = "',trim(namec),'"'
              write(lp,'(a,i4)') 'io     = ',io
              write(lp,'(a,i4)') 'iotype = ',iotype
              call flush(lp)
              stop
            endif
            m_value   = -30000
            call nchek('nf90_def_var( namecv',
     &                  nf90_def_var(ncfileID,trim(namecv),nf90_short,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                               varID))
            if     (namelv.ne." ") then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",trim(namelv)))
            endif
          elseif (iotype.eq.-6) then !NAVOlike
            call nchek('nf90_def_var( namec',
     &                  nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, MTDimID/),
     &                               varID))
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID/),
     &                               varID))
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if      (iotype.eq.-5) then !NAVO only
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "missing_value",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "scale_factor",scale_f))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "add_offset",add_off))
            if     (navo_code.ne.0) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "NAVO_code",navo_code))
            endif
            if     (navo_code.eq.15) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "comment","in-situ temperature"))
            endif
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",fill_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "valid_range",
     &                               (/hmin, hmax/)))
            write(lp,'(a,i4)') 'long_name: k = ',k
            call flush(lp)
            if     (k.lt.0) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 name))
            elseif (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 label(33:50)//label(73:81)))
            elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 label(33:50)//label(73:81)))
            else
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 label(33:55)//label(73:81)))
            endif
          endif !NAVO:else
          if     (deflate.gt.0) then
            call ncheck(nf90_def_var_deflate(ncfileID,VarID,
     &                                       shuffle=1,deflate=1,
     &                                       deflate_level=deflate))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          ! get varID and write to array
          if (iotype.eq.-5) then !NAVO only
            call ncheck(nf90_inq_varid(ncfileID,trim(namecv),varID))
            allocate( array2(ii,jj) )
            do j= 1,jj
              do i= 1,ii
                if     (array(i,j).ne.fill_value) then
c                 nint is i*4, min:max prevents i*2 overflow
                  array2(i,j) = max( -2**15, min( 2**15-1,
     &                            nint((array(i,j)-add_off)/scale_f) ) )
                else
                  array2(i,j) = m_value
                endif
              enddo !i
            enddo !j
*             write(lp,*) 'aij = ',
*    &          array( ii/2,jj/2),
*    &          array2(ii/2,jj/2),
*    &          array2(ii/2,jj/2)*scale_f+add_off
            !write values into array
            call nchek('nf90_put_var( array2',
     &                  nf90_put_var(ncfileID,varID,array2(:,:)))
            deallocate( array2 )
          else
            call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
            !write values into array
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:,:)))
          endif
          ! close file 
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a,a,2g15.6)') label(33:81),':',hmin,hmax
        call flush(lp)
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
        stop
      endif
      io = iosave
      return
      end

      subroutine horout_3d(array,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units,
     &                     kf,kl,ltheta, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          artype,yrflag,iexpt,kf,kl,io
      double precision time3(3)
      real             array(ii,jj,kl)
c
c     write out a 3-d layer array to unit io based on frmt.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c     io may be modified by this subroutine
c
c     calls horout_3t to do the work
c
      real tsur(0:kl)
      tsur(:) = 0.0
      call horout_3t(array,
     &               artype,yrflag,time3,iexpt,lhycom,
     &               name,namec,names,units,
     &               kf,kl,ltheta,.false.,tsur, frmt,io)
      return
      end

      subroutine horout_3t(array,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units,
     &                     kf,kl,ltheta,ltsur,tsur, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,ltsur,lexist
      integer          artype,yrflag,iexpt,kf,kl,io
      double precision time3(3)
      real             array(ii,jj,kl),tsur(0:kl)
c
c     write out a 3-d layer array to unit io based on frmt.
c
c     horout_3d calls this routine, and most error stops
c     reference horout_3d which is the more common call path.
c
c     2-d array size and frmt    must be identical in all calls.
c     artype,yrflag,time3,lhycom must be identical in all calls.
c     io may be modified by this subroutine
c
c     at most one of ltheta and ltsur can be .true..
c
c     the output filename is taken from environment variable FOR0xx,
c      where  xx = io, with default fort.xx.
c     the array  filename is taken from environment variable FORxxxA,
c      where xxx = io, with default fort.xxxa
c     the NetCDF filename is taken from environment variable CDFxxx,
c      where xxx = io, with no default.
c     the NetCDF vertical chunking is taken from environment
c      variable CDF_KCHUNK.  If not defined or 0 the default chunking
c      is used, and a value greater than kl-kf+1 is set to kl-kf+1.
c     the NetCDF deflation level (if any) is taken from environment
c      variable CDF_DEFLATE.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c     NAVO convention: public release notice turned on by environment
c      variable CDF_PUBLIC.
c     NAVO convention: hours since analysis is taken from environment
c      variable CDF_TAU.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF-4 classic I/O,
c       frmt=='netCDF'        for NetCDF-4 classic I/O,
c       frmt=='MERSEA'        for NetCDF-4 classic I/O with MERSEA conventions,
c       frmt=='NAVO'          for NetCDF-4 classic I/O with NAVO   conventions
c       frmt=='NAVOlike'      for NetCDF-4 classic I/O consistent with NAVO but
c                                  using float arrays and HYCOM names,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version supports frmt=='netCDF' ('MERSEA','NAVO') and needs
c     version 4.3 or later of the NetCDF library, from:
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
      logical          :: lopen,lugrid,lvgrid
      integer          :: i,j,k,l,iosave
      integer          :: ichunk,jchunk
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999),alon
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      integer,          save :: kchunk  =  0
      integer,          save :: deflate =  0
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (ltheta .and. ltsur) then
        write(lp,'(/2a/)')   'error in horout_3t - ',
     &    'ltheta and ltsur are both .true.'
        call flush(lp)
        stop
      endif
c
      iosave = io
      lugrid = .false.
      lvgrid = .false.
      if     (io.lt.0) then
        io = -io
        if     (namec(1:1).eq.'u') then
          lugrid = .true.
        elseif (namec(1:1).eq.'v') then
          lvgrid = .true.
        endif
      endif
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
        elseif (frmt(1:l).eq.'MERSEA') then
c
c         NetCDF I/O, with MERSEA layout.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout_3d - ',
     &        'MERSEA requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -4
          write(lp,'(/2a/)') 'horout_3d - MERSEA I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_3d - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       vertical dimension chunking
c
        ncenv = ' '
        call getenv('CDF_KCHUNK',ncenv)
        if     (ncenv.eq.' ') then
          kchunk = 0
        else
          read(ncenv,*) kchunk
          kchunk = min( kchunk, kl-kf+1 )
        endif
        write(lp,*) 'kchunk   =',kchunk
c
c       deflation level
c
        ncenv = ' '
        call getenv('CDF_DEFLATE',ncenv)
        if     (ncenv.eq.' ') then
          deflate = 0
        else
          read(ncenv,*) deflate
        endif
        write(lp,*) 'deflate  = ',deflate
c
c       initialize labeli.
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
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
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
        call zbiowr3(array(1,1,kf),kl-kf+1,
     +               ip,.false., hmin(kf),hmax(kf), io, .false.)
        do k= kf,kl
          if     (ltheta) then
            write(label(33:50),'(a,f5.2,   a)') 'sig=',theta(k),name
          elseif (ltsur) then
            write(label(33:50),'(a,f5.2,   a)') 'iso=',tsur(k),name
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
          elseif (ltsur) then
            write(label(33:50),'(a,f5.2,   a)') 'iso=',tsur(k),name
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
          elseif (ltsur) then
            write(label(33:50),'(a,f5.2,   a)') 'iso=',tsur(k),name
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
          elseif (ltsur) then
            write(label(33:50),'(a,f5.2,   a)') 'iso=',tsur(k),name
          else
            write(label(33:50),'(a,i2.2,1x,a)') 'layer=',k,name
          endif
          write(io,frmt) array(:,:,k)
          call flush(io)
          write(lp,'(a)') label(33:81)
          call flush(lp)
        enddo
      elseif(abs(iotype).eq.4) then
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array(1,1,kf),ii,jj,kl-kf+1, fill_value,
     &                                         hmin(1),hmax(1))
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
c
c         create a new NetCDF and write data to it
c         netcdf-4 classic model, netcdf version 4.3 and later
c
          call ncheck(nf90_create(trim(ncfile),
     &                            or(nf90_clobber,
     &                               or(nf90_hdf5,
     &                                  nf90_classic_model)),
     &                            ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "longitude", ii,pLonDimID))
          elseif (laxis) then
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
          if     (ltsur) then
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Isotherm",kl-kf+1,lyrDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,
     &                               "Layer",   kl-kf+1,lyrDimID))
          endif
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.6"))
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
            ncenv = ' '
            call getenv('CDF_HIST',ncenv)
            if     (ncenv.eq.' ') then
              if     (ltsur) then
                ncenv = "archv2ncdf2t"
              else
                ncenv = "archv2ncdf2d"
              endif
            endif
            if     (ncenv.ne.'NONE') then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "history",
     &                                 trim(ncenv)))
            endif
            if     (lugrid) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "comment","u-grid"))
            elseif (lvgrid) then
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "comment","v-grid"))
            else
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "comment","p-grid"))
            endif
            if     (iotype.eq.-4) then !MERSEA
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              write(ncenv,
     &          '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &          iyear,month,iday,ihour
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "field_date",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('MERSEA_B_DATE',ncenv)
              if     (ncenv.eq.'TODAY') then
                write(ncenv,
     &            '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &            iyear,month,iday,ihour
              endif
              if     (ncenv.ne.' ') then
                read(ncenv,'(i4,1x,i2,1x,i2,1x,i2)')
     &            iyrms,monms,idms,ihrms
                if     (iyrms.lt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "forecast"))
                elseif (iyrms.gt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "hindcast"))
                else   !iyrms.eq.iyear
                  if     (monms.lt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "forecast"))
                  elseif (monms.gt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "hindcast"))
                  else   !monms.eq.month
                    if     (idms.lt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "forecast"))
                    elseif (idms.gt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "hindcast"))
                    else   !idms.eq.iday
                      if     (ihrms.lt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "forecast"))
                      elseif (ihrms.gt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "hindcast"))
                      else   !ihrms.eq.ihour
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "nowcast"))
                      endif  !ihrms
                    endif !idms
                  endif !monms
                endif !iyrms
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_date",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('MERSEA_B_TYPE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_type",
     &                                   trim(ncenv)))
              endif
            endif
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
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "gregorian"))  !same as standard
            endif
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
          endif !not MERSEA
          if     (ltheta) then
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_float,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","SigmaTheta"))     
          elseif (ltsur) then
            call ncheck(nf90_def_var(ncfileID,"Isotherm", nf90_float,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","degC"))     
          else
            call ncheck(nf90_def_var(ncfileID,"Layer", nf90_int,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "units","layer"))     
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "positive","down"))
          endif
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          if     (laxis) then
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
            if     (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "axis","X"))
          else !.not.laxis
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "axis","Y"))
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "axis","X"))
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
           if     (deflate.gt.0) then
              call ncheck(nf90_def_var_deflate(ncfileID,pLatVarID,
     &                                         shuffle=1,deflate=1,
     &                                         deflate_level=deflate))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            if     (deflate.gt.0) then
              call ncheck(nf90_def_var_deflate(ncfileID,pLonVarID,
     &                                         shuffle=1,deflate=1,
     &                                         deflate_level=deflate))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif !laxis:else
          ! model 3d variable
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk/)))
            endif
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          if     (deflate.gt.0) then
            call ncheck(nf90_def_var_deflate(ncfileID,VarID,
     &                                       shuffle=1,deflate=1,
     &                                       deflate_level=deflate))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
          endif
          if     (laxis) then
            if     (lugrid) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 (/ulat(1,:)/)))     !1-d Latitudes
              call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                 (/ulon(:,1)/)))     !1-d Longtudes
              write(lp,'(/a,i4/)') 'horout_3d - u-grid: io =',io
              call flush(lp)
            elseif (lvgrid) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 (/vlat(1,:)/)))     !1-d Latitudes
              call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                 (/vlon(:,1)/)))     !1-d Longtudes
              write(lp,'(/a,i4/)') 'horout_3d - v-grid: io =',io
              call flush(lp)
            else
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 (/plat(1,:)/)))     !1-d Latitudes
              call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                 (/plon(:,1)/)))     !1-d Longtudes
              write(lp,'(/a,i4/)') 'horout_3d - p-grid: io =',io
              call flush(lp)
            endif
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            if     (lugrid) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,ulat(:,:)))
              if     (maxval(ulon(1:ii,1:jj)).gt.180.1) then
                do j= 1,jj
                  do i= 1,ii
                    alon = mod(ulon(i,j)+1080.0,360.0)
                    if     (alon.ge.180.0) then
                      alon = alon - 360.0
                    endif
                    ulon(i,j) = alon  !between -180E and 180E
                  enddo !i
                enddo !j
              endif
              call ncheck(nf90_put_var(ncfileID,pLonVarID,ulon(:,:)))
              write(lp,'(/a,i4/)') 'horout_3d - u-grid: io =',io
              call flush(lp)
            elseif (lvgrid) then
              call ncheck(nf90_put_var(ncfileID,pLatVarID,vlat(:,:)))
              if     (maxval(vlon(1:ii,1:jj)).gt.180.1) then
                do j= 1,jj
                  do i= 1,ii
                    alon = mod(vlon(i,j)+1080.0,360.0)
                    if     (alon.ge.180.0) then
                      alon = alon - 360.0
                    endif
                    vlon(i,j) = alon  !between -180E and 180E
                  enddo !i
                enddo !j
              endif
              call ncheck(nf90_put_var(ncfileID,pLonVarID,vlon(:,:)))
              write(lp,'(/a,i4/)') 'horout_3d - v-grid: io =',io
              call flush(lp)
            else
              call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
              if     (maxval(plon(1:ii,1:jj)).gt.180.1) then
                do j= 1,jj
                  do i= 1,ii
                    alon = mod(plon(i,j)+1080.0,360.0)
                    if     (alon.ge.180.0) then
                      alon = alon - 360.0
                    endif
                    plon(i,j) = alon  !between -180E and 180E
                  enddo !i
                enddo !j
              endif
              call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
              write(lp,'(/a,i4/)') 'horout_3d - p-grid: io =',io
              call flush(lp)
            endif
          endif
          if     (ltheta) then
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               theta(kf:kl)))
          elseif (ltsur) then
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               tsur(kf:kl)))
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
          if     (iotype.eq.4) then !not for MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,"MT",    MTDimID))
          endif
          if     (ltsur) then
            call ncheck(nf90_inq_dimid(ncfileID,"Isotherm",lyrDimID))
          else
            call ncheck(nf90_inq_dimid(ncfileID,   "Layer",lyrDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "longitude",pLonDimID))
          elseif (laxis) then
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
          if     (iotype.eq.4) then !not for MERSEA
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk/)))
            endif
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                 trim(name)//label(51:55)//label(73:81)))
          endif
          if     (deflate.gt.0) then
            call ncheck(nf90_def_var_deflate(ncfileID,VarID,
     &                                       shuffle=1,deflate=1,
     &                                       deflate_level=deflate))
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
        stop
      endif
      return
      end

      subroutine horout_3z(array,zz,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units, kz, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom, lexist
      integer          artype,yrflag,iexpt,kz,io
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
c     the NetCDF vertical chunking is taken from environment
c      variable CDF_KCHUNK.  If not defined or 0 the default
c      chunking is used, and a value greater than kz is set to kz.
c     the NetCDF deflation level (if any) is taken from environment
c      variable CDF_DEFLATE.
c     the NetCDF title and institution are taken from environment
c      variables CDF_TITLE and CDF_INST.
c     NAVO convention: public release notice turned on by environment
c      variable CDF_PUBLIC.
c     NAVO convention: hours since analysis is taken from environment
c      variable CDF_TAU.
c
c     Supported I/O types are:
c       frmt=='NetCDF'        for NetCDF-4 classic I/O,
c       frmt=='netCDF'        for NetCDF-4 classic I/O,
c       frmt=='MERSEA'        for NetCDF-4 classic I/O with MERSEA conventions,
c       frmt=='NAVO'          for NetCDF-4 classic I/O with NAVO   conventions
c       frmt=='NAVOlike'      for NetCDF-4 classic I/O consistent with NAVO but
c                                  using float arrays and HYCOM names,
c       frmt=='HYCOM'         for HYCOM .[ab] I/O,
c       frmt=='BIN'           for unformatted sequential I/O,
c       frmt=='(...)'         for   formatted sequential I/O with format frmt.
c       frmt=='(2f10.4,...)'  for   formatted sequential I/O of the form
c                                   longitude latitude value (skipping land)
c
c     This version supports frmt=='netCDF' ('MERSEA','NAVO') and needs
c     version 4.3 or later of the NetCDF library, from:
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer*2, allocatable :: array2(:,:,:)
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLonDimID,pLatVarID,pLonVarID,
     &                    lyrDimID,lyrVarID
      integer          :: pYDimID,pXDimID,pYVarID,pXVarID
      integer          :: MTDimID,MTVarID,tauVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6,cdate*19
c
      logical          :: lopen
      integer          :: i,j,k,l
      integer          :: ichunk,jchunk
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999),alon
      double precision :: dt,tau,yr0,year
      integer*2        :: m_value
      integer          :: navo_code
      real             :: scale_f,add_off
      character*240    :: namecv,namelv
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      integer,          save :: kchunk  =  0
      integer,          save :: deflate =  0
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      logical,          save :: laxis
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
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
        elseif (frmt(1:l).eq.'MERSEA') then
c
c         NetCDF I/O, with MERSEA layout.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout_3z - ',
     &        'MERSEA requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -4
          write(lp,'(/2a/)') 'horout_3z - MERSEA I/O (lat/lon axes)'
          call flush(lp)
        elseif (frmt(1:l).eq.'NAVO') then
c
c         NetCDF I/O, with NAVO layout.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout_3z - ',
     &        'NAVO requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -5
          write(lp,'(/2a/)') 'horout_3z - NAVO I/O (lat/lon axes)'
          call flush(lp)
        elseif (frmt(1:l).eq.'NAVOlike') then
c
c         NetCDF I/O, with NAVO layout but HYCOM names.
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
          if     (.not. laxis) then
            write(lp,'(/2a/)')   'error in horout - ',
     &        'NAVO requires lat/lon axes'
            call flush(lp)
            stop
          endif
c
          iotype = -6
          write(lp,'(/2a/)') 'horout_2z - NAVOlike I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_3z - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       vertical dimension chunking
c
        ncenv = ' '
        call getenv('CDF_KCHUNK',ncenv)
        if     (ncenv.eq.' ') then
          kchunk = 0
        else
          read(ncenv,*) kchunk
          kchunk = min( kchunk, kz )
        endif
        write(lp,*) 'kchunk   =',kchunk
c
c       deflation level
c
        ncenv = ' '
        call getenv('CDF_DEFLATE',ncenv)
        if     (ncenv.eq.' ') then
          deflate = 0
        else
          read(ncenv,*) deflate
        endif
        write(lp,*) 'deflate  = ',deflate
c
c       initialize labeli.
c
        if     (yrflag.eq.0) then
          year  = 360.0d0
        elseif (yrflag.lt.3) then
          year  = 366.0d0
        else
          year  = 365.25d0
        endif
        call fordate(time3(3),yrflag, iyear,month,iday,ihour)
        write(lp,*) 'ihour    =',ihour
        date    = (iday + 100 * month + 10000 * iyear) +
     &            (time3(3)-int(time3(3)))
        if     (artype.eq.1) then
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
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
      elseif (abs(iotype).eq.4 .or. iotype.le.-5) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array,ii,jj,kz, fill_value, hmin(1),hmax(1))
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
          ! create a new NetCDF and write data to it
          ! netcdf-4 classic model, netcdf version 4.3 and later
          call ncheck(nf90_create(trim(ncfile),
     &                            or(nf90_clobber,
     &                               or(nf90_hdf5,
     &                                  nf90_classic_model)),
     &                            ncfileID))
          ! define the dimensions
          if     (iotype.eq.4) then !not for MERSEA or NAVO
            call nchek('nf90_def_dim( MT',
     &                  nf90_def_dim(ncfileID,
     &                               "MT", nf90_unlimited,MTDimID))
          endif
          if     (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_dim(ncfileID,
     &                               "latitude",  jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "longitude", ii,pLonDimID))
          elseif (iotype.le.-5) then !NAVO,NAVOlike
            write(lp,*) 'lat = jj = ',jj
            write(lp,*) 'lon = ii = ',ii
            call nchek('nf90_def_dim( lat',
     &                  nf90_def_dim(ncfileID,
     &                               "lat",       jj,pLatDimID))
            call ncheck(nf90_def_dim(ncfileID,
     &                               "lon",       ii,pLonDimID))
          elseif (laxis) then
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
          if     (iotype.eq.-4 .or. iotype.le.-5) then !MERSEA or NAVO
            call nchek('nf90_def_dim( depth',
     &                  nf90_def_dim(ncfileID,"depth",kz,lyrDimID))
          else
            call ncheck(nf90_def_dim(ncfileID,"Depth",kz,lyrDimID))
          endif
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call nchek('nf90_def_dim( time',
     &                  nf90_def_dim(ncfileID,
     &                               "time", nf90_unlimited,MTDimID))
          endif
          ! create the global attributes
          if     (iotype.ne.-5) then !except NAVO
            call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                               "Conventions",
     &                               "CF-1.6"))
          endif
          if (lhycom) then
            if     (iotype.eq.4) then !standard
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "HYCOM"
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "title",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "experiment",
     &                                 label(75:78)))
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf3z"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
            elseif (iotype.eq.-4) then !MERSEA
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "HYCOM"
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "title",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "experiment",
     &                                 label(75:78)))
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf3z"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              write(ncenv,
     &          '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &          iyear,month,iday,ihour
              write(lp,*) 'ihour =',ihour
              write(lp,*) 'field_date ='
              write(lp,*) trim(ncenv)
              call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                 "field_date",
     &                                 trim(ncenv)))
              ncenv = ' '
              call getenv('MERSEA_B_DATE',ncenv)
              if     (ncenv.eq.'TODAY') then
                write(ncenv,
     &            '(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":00:00")')
     &            iyear,month,iday,ihour
              endif
              if     (ncenv.ne.' ') then
                read(ncenv,'(i4,1x,i2,1x,i2,1x,i2)')
     &            iyrms,monms,idms,ihrms
                if     (iyrms.lt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "forecast"))
                elseif (iyrms.gt.iyear) then
                  call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                     "forecast_type",
     &                                     "hindcast"))
                else   !iyrms.eq.iyear
                  if     (monms.lt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "forecast"))
                  elseif (monms.gt.month) then
                    call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                       "forecast_type",
     &                                       "hindcast"))
                  else   !monms.eq.month
                    if     (idms.lt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "forecast"))
                    elseif (idms.gt.iday) then
                      call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                         "forecast_type",
     &                                         "hindcast"))
                    else   !idms.eq.iday
                      if     (ihrms.lt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "forecast"))
                      elseif (ihrms.gt.ihour) then
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "hindcast"))
                      else   !ihrms.eq.ihour
                        call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                           "forecast_type",
     &                                           "nowcast"))
                      endif  !ihrms
                    endif !idms
                  endif !monms
                endif !iyrms
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_date",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('MERSEA_B_TYPE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "bulletin_type",
     &                                   trim(ncenv)))
              endif
            elseif (iotype.le.-5) then !NAVO,NAVOlike
              ncenv = ' '
              call getenv('CDF_TITLE',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "title",
     &                                   trim(ncenv)))
              endif
              ncenv = ' '
              call getenv('CDF_PUBLIC',ncenv)
              if     (ncenv.ne.' ') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "classification_level",
     &                                   "UNCLASSIFIED"))
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "distribution_statement",
     &      "Approved for public release; distribution unlimited."))
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "downgrade_date",
     &                                   "not applicable"))
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "classification_authority",
     &                                   "not applicable"))
              endif !PUBLIC
              ncenv = ' '
              call getenv('CDF_INST',ncenv)
              if     (ncenv.ne.' ') then
                call nchek('nf90_put_att( institution',
     &                      nf90_put_att(ncfileID,nf90_global,
     &                                   "institution",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.1) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM archive file"))
              elseif (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM mean archive file"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "source",
     &                                   "HYCOM std. archive file"))
              endif
              ncenv = ' '
              call getenv('CDF_HIST',ncenv)
              if     (ncenv.eq.' ') then
                ncenv = "archv2ncdf3z"
              endif
              if     (ncenv.ne.'NONE') then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "history",
     &                                   trim(ncenv)))
              endif
              if     (artype.eq.2) then
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "daily average"))
              else
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "field_type",
     &                                   "instantaneous"))
              endif
              if     (iotype.eq.-5) then !NAVO only
                call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                                   "Conventions",
     &                                   "CF-1.6 NAVO_netcdf_v1.1"))
              endif
            endif !iotype
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
          if     (iotype.eq.4) then !not for MERSEA or NAVO
            call ncheck(nf90_def_var(ncfileID,"MT",  nf90_double,
     &                               MTDimID,MTVarID))
            if     (yrflag.eq.0) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "360_day"))
            elseif (yrflag.eq.1) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-16 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            elseif (yrflag.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "model time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 0001-01-01 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "366_day"))
            else
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "long_name",
     &                                 "time"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "units",
     &                            "days since 1900-12-31 00:00:00"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "calendar",
     &                                 "gregorian"))  !same as standard
            endif
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                               MTDimID,datVarID))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "long_name",
     &                               "date"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "units",
     &                               "day as %Y%m%d.%f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "C_format",
     &                               "%13.4f"))
            call ncheck(nf90_put_att(ncfileID,datVarID,
     &                               "FORTRAN_format",
     &                               "(f13.4)"))
            if     (artype.eq.2) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "mean"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            elseif (artype.eq.3) then
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "cell_extent",
     &                                 cell))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_methods",
     &                                 "standard_deviation"))
              call ncheck(nf90_put_att(ncfileID,datVarID,
     &                                 "cell_extent",
     &                                 cell))
            endif
          endif !not MERSEA or NAVO
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call nchek('nf90_def_var( time',
     &                  nf90_def_var(ncfileID,"time",  nf90_double,
     &                               MTDimID,MTVarID))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name","Valid Time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "hours since 2000-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "time_origin",
     &                          "2000-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "gregorian"))  !same as standard
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "axis","T"))
            if     (iotype.eq.-5) then !NAVO only
              call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                                 "NAVO_code",13))
            endif
c
            ncenv = ' '
            call getenv('CDF_TAU',ncenv)
            if     (ncenv.ne.' ') then
              read(ncenv,*) tau
              call nchek('nf90_def_var( time',
     &                    nf90_def_var(ncfileID,"tau",  nf90_double,
     &                                 MTDimID,tauVarID))
              call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                 "long_name","Tau"))
              call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                 "units",
     &                                 "hours since analysis"))
              call fordate(time3(3)-tau/24.d0,3, iyrms,monms,idms,ihrms)
              write(cdate,"(i4.4,'-',i2.2,'-',i2.2,' ',i2.2,':00:00')")
     &          iyrms,monms,idms,ihrms
              call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                 "time_origin",
     &                                 cdate))
              if     (iotype.eq.-5) then !NAVO only
                call ncheck(nf90_put_att(ncfileID,tauVarID,
     &                                   "NAVO_code",56))
              endif
            endif
          endif !NAVO
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call nchek('nf90_def_var( depth',
     &                  nf90_def_var(ncfileID,"depth", nf90_double,
     &                               lyrDimID,lyrVarID))
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "long_name","Depth"))
          elseif (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_def_var(ncfileID,"depth", nf90_float,
     &                               lyrDimID,lyrVarID))
          else
            call ncheck(nf90_def_var(ncfileID,"Depth", nf90_float,
     &                               lyrDimID,lyrVarID))
          endif
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "standard_name","depth"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "units","m"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "positive","down"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          if     (iotype.eq.-5) then !NAVO only
            call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                               "NAVO_code",5))
          endif
          if     (laxis) then
            if     (iotype.le.-5) then !NAVO,NAVOlike
              call nchek('nf90_def_var( lat',
     &                    nf90_def_var(ncfileID,"lat",  nf90_double,
     &                                 pLatDimID,pLatVarID))
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "long_name","Latitude"))
            elseif (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                                 pLatDimID,pLatVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            if     (abs((plat(1,jj)-plat(1,1))-
     &                  (plat(1, 2)-plat(1,1))*(jj-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "axis","Y"))
            if     (iotype.eq.-5) then !NAVO only
              call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                                 "NAVO_code",1))
            endif
            if     (iotype.le.-5) then !NAVO,NAVOlike
              call nchek('nf90_def_var( lon',
     &                    nf90_def_var(ncfileID,"lon", nf90_double,
     &                                 pLonDimID,pLonVarID))
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "long_name","Longitude"))
            elseif (iotype.eq.-4) then !MERSEA
              call ncheck(nf90_def_var(ncfileID,"longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            else
              call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                                 pLonDimID,pLonVarID))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)-plon(1,1))-
     &                  (plon( 2,1)-plon(1,1))*(ii-1)).lt.0.1) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "point_spacing","even"))  !ferret
            endif
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "axis","X"))
            if     (iotype.eq.-5) then !NAVO only
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "NAVO_code",2))
            endif
          else !.not.laxis
            call ncheck(nf90_def_var(ncfileID,"Y", nf90_int,
     &                               pYDimID,pYVarID))
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pYVarID,
     &                               "axis","Y"))
            call ncheck(nf90_def_var(ncfileID,"X", nf90_int,
     &                               pXDimID,pXVarID))
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "point_spacing","even"))  !ferret
            call ncheck(nf90_put_att(ncfileID,pXVarID,
     &                               "axis","X"))
            call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                               (/pXDimID, pYDimID/), pLatVarID))
           if     (deflate.gt.0) then
              call ncheck(nf90_def_var_deflate(ncfileID,pLatVarID,
     &                                         shuffle=1,deflate=1,
     &                                         deflate_level=deflate))
            endif
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "standard_name","latitude"))
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "units","degrees_north"))
            call ncheck(nf90_def_var(ncfileID,"Longitude", nf90_float,
     &                               (/pXDimID, pYDimID/), pLonVarID))
            if     (deflate.gt.0) then
              call ncheck(nf90_def_var_deflate(ncfileID,pLonVarID,
     &                                         shuffle=1,deflate=1,
     &                                         deflate_level=deflate))
            endif
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "standard_name","longitude"))
            call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                               "units","degrees_east"))
            if     (abs((plon(ii,1)+(plon(2,1)-plon(1,1)))-
     &                  (plon( 1,1)+ 360.0) ).lt.1.e-2) then
              call ncheck(nf90_put_att(ncfileID,pLonVarID,
     &                                 "modulo","360 degrees"))  !ferret
            endif
          endif !laxis:else
          ! model 3Z variable
          if     (iotype.eq.4) then !standard
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
          elseif (iotype.eq.-5) then !NAVO only
            namelv = ''  !default is no long name
            if     (namec.eq.'u') then
              namecv    = 'water_u'
              namelv    = 'Eastward Water Velocity'
              navo_code = 17
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'v') then
              namecv    = 'water_v'
              namelv    = 'Northward Water Velocity'
              navo_code = 18
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'w_velocity') then
              namecv    = 'water_w'
              namelv    = 'Upward Water Velocity'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'water_temp') then
              namecv    = namec
              namelv    = 'Water Temperature'
              navo_code = 15
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'water_ptemp') then
              namecv    = namec
              namelv    = 'Water Potential Temperature'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'salinity') then
              namecv    = namec
              namelv    = 'Salinity'
              navo_code = 16
              scale_f   = 0.001
              add_off   = 20.0
            else
              write(lp,'(/a)')   'error in horout_3z - unknown name'
              write(lp,'(3a)')   'namec  = "',trim(namec),'"'
              write(lp,'(a,i4)') 'io     = ',io
              write(lp,'(a,i4)') 'iotype = ',iotype
              call flush(lp)
              stop
            endif
            m_value   = -30000
            call nchek('nf90_def_var( namecv',
     &                  nf90_def_var(ncfileID,trim(namecv),nf90_short,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,2, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
            if     (namelv.ne." ") then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",trim(namelv)))
            endif
          elseif (iotype.eq.-6) then !NAVOlike
            call nchek('nf90_def_var( namec',
     &                  nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                            (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk/)))
            endif
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if      (iotype.eq.-5) then !NAVO only
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "missing_value",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "scale_factor",scale_f))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "add_offset",add_off))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "NAVO_code",navo_code))
            if     (navo_code.eq.15) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "comment","in-situ temperature"))
            endif
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",fill_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "valid_range",
     &                               (/hmin(1), hmax(1)/)))
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            else
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                   trim(name)//label(51:55)//label(73:81)))
            endif
          endif !NAVO:else
          if     (deflate.gt.0) then
            call ncheck(nf90_def_var_deflate(ncfileID,VarID,
     &                                       shuffle=1,deflate=1,
     &                                       deflate_level=deflate))
          endif
          ! leave def mode
          call ncheck(nf90_enddef(ncfileID))
          ! write data into coordinate variables
          if     (iotype.eq.4) then !standard
            call ncheck(nf90_put_var(ncfileID,MTVarID, time3(3)))
            call ncheck(nf90_put_var(ncfileID,datVarID,date    ))
          elseif (iotype.le.-5) then !NAVO,NAVOlike
            call ncheck(nf90_put_var(ncfileID,MTVarID,
     &                               (time3(3)-36160.d0)*24.d0)) !hrs since 2000
c
            ncenv = ' '
            call getenv('CDF_TAU',ncenv)
            if     (ncenv.ne.' ') then
              read(ncenv,*) tau
              call ncheck(nf90_put_var(ncfileID,tauVarID,tau))
            endif
          endif
          if     (laxis) then
            if (iotype.le.-5) then !NAVO,NAVOlike
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 dble((/plat(1,:)/)))) !1-d Latitudes
              call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                 dble((/plon(:,1)/)))) !1-d Longtudes
            else
              call ncheck(nf90_put_var(ncfileID,pLatVarID,
     &                                 (/plat(1,:)/)))     !1-d Latitudes
              call ncheck(nf90_put_var(ncfileID,pLonVarID,
     &                                 (/plon(:,1)/)))     !1-d Longtudes
            endif
          else
            call ncheck(nf90_put_var(ncfileID,pYVarID,
     &                               (/(j, j=1,jj)/)))
            call ncheck(nf90_put_var(ncfileID,pXVarID,
     &                               (/(i, i=1,ii)/)))
            call ncheck(nf90_put_var(ncfileID,pLatVarID,plat(:,:)))
            if     (maxval(plon(1:ii,1:jj)).gt.180.1) then
              do j= 1,jj
                do i= 1,ii
                  alon = mod(plon(i,j)+1080.0,360.0)
                  if     (alon.ge.180.0) then
                    alon = alon - 360.0
                  endif
                  plon(i,j) = alon  !between -180E and 180E
                enddo !i
              enddo !j
            endif
            call ncheck(nf90_put_var(ncfileID,pLonVarID,plon(:,:)))
          endif
          if (iotype.le.-5) then !NAVO,NAVOlike
            call nchek('nf90_put_var( zz',
     &                  nf90_put_var(ncfileID,lyrVarID,dble(zz(:))))
          else
            call nchek('nf90_put_var( zz',
     &                  nf90_put_var(ncfileID,lyrVarID,zz(:)))
          endif
          ! write to model variable
          if (iotype.eq.-5) then !NAVO only
            allocate( array2(ii,jj,kz) )
            do k= 1,kz
              do j= 1,jj
                do i= 1,ii
                  if     (array(i,j,k).ne.fill_value) then
c                   nint is i*4, min:max prevents i*2 overflow
                    array2(i,j,k) = max( -2**15, min( 2**15-1,
     &                                nint((array(i,j,k)-add_off)
     &                                     /scale_f) ) )
                  else
                    array2(i,j,k) = m_value
                  endif
                enddo !i
              enddo !j
            enddo !k
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array2(:,:,:)))
            deallocate( array2 )
          else
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:,:,:)))
          endif
          ! close NetCDF file
          call ncheck(nf90_close(ncfileID))
        else
c
c        Write data to existing NetCDF file
c
          ! open NetCDF file
          call ncheck(nf90_open(trim(ncfile), nf90_write, ncfileID))
          ! get dimension ID's
          if     (iotype.eq.4) then !standard
            call ncheck(nf90_inq_dimid(ncfileID,"MT",MTDimID))
            call ncheck(nf90_inq_dimid(ncfileID,"Depth",lyrDimID))
          elseif (iotype.le.-5) then !NAVO,NAVOlike
            call ncheck(nf90_inq_dimid(ncfileID,"time",MTDimID))
            call ncheck(nf90_inq_dimid(ncfileID,"depth",lyrDimID))
          else !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,"depth",lyrDimID))
          endif
          if     (iotype.le.-5) then !NAVO,NAVOlike
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "lat",     pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "lon",     pLonDimID))
          elseif (iotype.eq.-4) then !MERSEA
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "latitude",pLatDimID))
            call ncheck(nf90_inq_dimid(ncfileID,
     &                                 "longitude",pLonDimID))
          elseif (laxis) then
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
          if     (iotype.eq.4) then !standard
            if     (laxis) then
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pLonDimID, pLatDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Date"))
            else
              call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                                 (/pXDimID,   pYDimID,
     &                                   lyrDimID, MTDimID/),
     &                                 varID))
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "coordinates",
     &                                 "Longitude Latitude Date"))
            endif
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
          elseif (iotype.eq.-5) then !NAVO only
            namelv = ''  !default is no long name
            if     (namec.eq.'u') then
              namecv    = 'water_u'
              namelv    = 'Eastward Water Velocity'
              navo_code = 17
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'v') then
              namecv    = 'water_v'
              namelv    = 'Northward Water Velocity'
              navo_code = 18
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'w_velocity') then
              namecv    = 'water_w'
              namelv    = 'Upward Water Velocity'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 0.0
            elseif (namec.eq.'water_temp') then
              namecv    = namec
              namelv    = 'Water Temperature'
              navo_code = 15
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'water_ptemp') then
              namecv    = namec
              namelv    = 'Water Potential Temperature'
              navo_code = 0
              scale_f   = 0.001
              add_off   = 20.0
            elseif (namec.eq.'salinity') then
              namecv    = namec
              namelv    = 'Salinity'
              navo_code = 16
              scale_f   = 0.001
              add_off   = 20.0
            else
              write(lp,'(/a)')   'error in horout_3z - unknown name'
              write(lp,'(3a)')   'namec  = "',trim(namec),'"'
              write(lp,'(a,i4)') 'io     = ',io
              write(lp,'(a,i4)') 'iotype = ',iotype
              call flush(lp)
              stop
            endif
            m_value   = -30000
            call nchek('nf90_def_var( namecv',
     &                  nf90_def_var(ncfileID,trim(namecv),nf90_short,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,2, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
            if     (namelv.ne." ") then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",trim(namelv)))
            endif
          elseif (iotype.eq.-6) then !NAVOlike
            call nchek('nf90_def_var( namec',
     &                  nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                               (/pLonDimID, pLatDimID,
     &                                 lyrDimID, MTDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk,1/)))
            endif
          else !MERSEA
            call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                            (/pLonDimID, pLatDimID, lyrDimID/),
     &                               varID))
            if     (kchunk.gt.0) then
              call horout_chunk(ii,jj,kchunk,4, lp, ichunk,jchunk)
              call ncheck(nf90_def_var_chunking(ncfileID,VarID,
     &                                          NF90_CHUNKED,
     &                                  (/ichunk,jchunk,kchunk/)))
            endif
          endif
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          if      (iotype.eq.-5) then !NAVO only
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "missing_value",m_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "scale_factor",scale_f))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "add_offset",add_off))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "NAVO_code",navo_code))
            if     (navo_code.eq.15) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "comment","in-situ temperature"))
            endif
          else
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "_FillValue",fill_value))
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "valid_range",
     &                               (/hmin(1), hmax(1)/)))
            if     (artype.eq.1) then
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                                 trim(name)//label(73:81)))
            else
              call ncheck(nf90_put_att(ncfileID,varID,
     &                                 "long_name",
     &                   trim(name)//label(51:55)//label(73:81)))
            endif
          endif !NAVO:else
          if     (deflate.gt.0) then
            call ncheck(nf90_def_var_deflate(ncfileID,VarID,
     &                                       shuffle=1,deflate=1,
     &                                       deflate_level=deflate))
          endif
          ! leave define mode
          call ncheck(nf90_enddef(ncfileID))
          if (iotype.eq.-5) then !NAVO only
            ! inquire variable ID
            call ncheck(nf90_inq_varid(ncfileID,trim(namecv),varID))
            ! write to model variable
              allocate( array2(ii,jj,kz) )
              do k= 1,kz
                do j= 1,jj
                  do i= 1,ii
                    if     (array(i,j,k).ne.fill_value) then
c                     nint is i*4, min:max prevents i*2 overflow
                      array2(i,j,k) = max( -2**15, min( 2**15-1,
     &                                  nint((array(i,j,k)-add_off)
     &                                       /scale_f) ) )
                    else
                      array2(i,j,k) = m_value
                    endif
                  enddo !k
                enddo !j
              enddo !i
              call nchek('nf90_put_var( array',
     &                    nf90_put_var(ncfileID,varID,array2(:,:,:)))
              deallocate( array2 )
          else
            ! inquire variable ID
            call ncheck(nf90_inq_varid(ncfileID,trim(namec),varID))
            ! write to model variable
            call nchek('nf90_put_var( array',
     &                  nf90_put_var(ncfileID,varID,array(:,:,:)))
          endif
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
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
        stop
      endif
      return
      end

      subroutine horout_jk(array, platj,jlatn,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units,
     &                     kf,kl,ltheta, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom,ltheta,lexist
      integer          jlatn,artype,yrflag,iexpt,kf,kl,io
      double precision time3(3)
      real             array(jlatn,kl),
     &                 platj(jlatn)
c
c     write out a 2-d layer array to unit io based on frmt.
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
c       frmt=='NetCDF' for NetCDF-4 classic I/O,
c       frmt=='netCDF' for NetCDF-4 classic I/O,
c
c     This version supports frmt=='netCDF' ('MERSEA','NAVO') and needs
c     version 4.3 or later of the NetCDF library, from:
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLatVarID,
     &                    lyrDimID,lyrVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999)
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'netCDF' .or. 
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          iotype = 4
          write(lp,'(/a/)') 'horout_jk - NetCDF I/O (lat axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_jk - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
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
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
c
      if    (iotype.eq.4) then
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array(1,kf), 1,jlatn,kl-kf+1,
     &                            fill_value, hmin(1),hmax(1))
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
         ! create a new NetCDF and write data to it
          ! netcdf-4 classic model, netcdf version 4.3 and later
          call ncheck(nf90_create(trim(ncfile),
     &                            or(nf90_clobber,
     &                               or(nf90_hdf5,
     &                                  nf90_classic_model)),
     &                            ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          call ncheck(nf90_def_dim(ncfileID,
     &                             "Latitude",  jlatn,pLatDimID))
          call ncheck(nf90_def_dim(ncfileID,"Layer",kl-kf+1,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.6"))
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
     &                               "archv2ncdfsf"))
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
     &                               "archm2ncdfsf"))
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
     &                               "gregorian"))  !same as standard
          endif
          call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                             "axis","T"))
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "C_format",
     &                             "%13.4f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "FORTRAN_format",
     &                             "(f13.4)"))
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
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                             pLatDimID,pLatVarID))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "standard_name","latitude"))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "units","degrees_north"))
          if     (abs((platj(jlatn)-platj(1))-
     &                (platj(2)    -platj(1))*(jlatn-1)).lt.0.1) then
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "point_spacing","even"))  !ferret
          endif
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "axis","Y"))
          ! model 3d variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
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
          call ncheck(nf90_put_var(ncfileID,pLatVarID,platj(:)))
          if     (ltheta) then
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               theta(kf:kl)))
          else
            call ncheck(nf90_put_var(ncfileID,lyrVarID,
     &                               (/(k, k=kf,kl)/)))
          endif
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,
     &                             array(1:jlatn,kf:kl)))
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
          call ncheck(nf90_inq_dimid(ncfileID,
     &                               "Latitude",pLatDimID))
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
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
     &                             array(1:jlatn,kf:kl)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_jk - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_jz(array,zz, platj,jlatn,
     &                     artype,yrflag,time3,iexpt,lhycom,
     &                     name,namec,names,units, kz, frmt,io)
      use mod_plot ! HYCOM I/O interface
      use mod_xc   ! HYCOM communication API
      use mod_zb   ! HYCOM I/O interface for subregion
      use netcdf   ! NetCDF Interface
      
      implicit none
c
      character*(*)    name,namec,names,units,frmt
      logical          lhycom, lexist
      integer          jlatn,artype,yrflag,iexpt,kz,io
      double precision time3(3)
      real             array(jlatn,kz),zz(kz),
     &                 platj(jlatn)
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
c       frmt=='NetCDF' for NetCDF-4 classic I/O,
c       frmt=='netCDF' for NetCDF-4 classic I/O,
c
c     This version supports frmt=='netCDF' and needs
c     version 4.3 or later of the NetCDF library, from:
c     http://www.unidata.ucar.edu/packages/netcdf/
c
      integer          :: ncfileID, status, varID
      integer          :: pLatDimID,pLatVarID,
     &                    lyrDimID,lyrVarID
      integer          :: MTDimID,MTVarID,datVarID
      character        :: ncfile*240,ncenv*240
      character        :: Ename*6
c
      logical          :: lopen
      integer          :: i,j,k,l
      integer, save    :: iyear,month,iday,ihour,
     &                    iyrms,monms,idms,ihrms
      real             :: hmin(999),hmax(999)
      double precision :: dt,yr0,year
c
      character*81,     save :: labeli  = ' '
      character*81,     save :: label   = ' '
      integer,          save :: iotype  = -1
      double precision, save :: date    = 0.d0
      double precision, save :: cell    = 0.d0
      real,        parameter :: fill_value = 2.0**100
c
      character cmonth(12)*3
      data      cmonth/'Jan','Feb','Mar','Apr','May','Jun',
     &                 'Jul','Aug','Sep','Oct','Nov','Dec'/
c
      if     (iotype.eq.-1) then
c
c        initialization.
c
        l = len_trim(frmt)
        if     (frmt(1:l).eq.'netCDF' .or.
     &          frmt(1:l).eq.'NetCDF'     ) then
c
c         NetCDF I/O.
c
          iotype = 4
          write(lp,'(/2a/)') 'horout_jz - NetCDF I/O (lat/lon axes)'
          call flush(lp)
        else
c
c         unknown I/O type.
c
          write(lp,'(/a)')   'error in horout_jz - unknown I/O type'
          write(lp,'(3a)')   'frmt   = "', frmt(1:len_trim( frmt)),'"'
          write(lp,'(a,i4)') 'io     = ',io
          call flush(lp)
          stop
        endif
c
c       initialize labeli.
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
          write (labeli(51:72),123) cmonth(month),iday,iyear,ihour
        elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
          write (labeli(51:72),223) cmonth(month),iday,iyear
        else  ! mean or sdev archive
          write(lp,*) 'time3 = ',time3
          dt = 0.5*(time3(2)-time3(1))/(nstep-1)
          if     (yrflag.eq.0) then
            yr0 = 15.0/year
          elseif (yrflag.eq.1) then
            yr0 = 15.25/year
          elseif (yrflag.eq.2) then
            yr0 = 0.0
          else
            yr0 = 1901.0
          endif
          cell = (time3(2)+dt) - (time3(1)-dt)
          if     (artype.eq.2) then
            write(labeli(51:72),114) ' mean: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          else
            write(labeli(51:72),114) ' sdev: ',yr0+(time3(1)-dt)/year,
     &                                         yr0+(time3(2)+dt)/year
          endif
        endif
        if (lhycom) then
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'H'
        else
          write (labeli(73:81),115) iexpt/10,mod(iexpt,10),'M'
        endif
 123    format ('   ',a3,i3.2,',',i5.4,i3.2,'Z   ')
 223    format ('   ',a3,i3.2,',',i5.4,' MEAN  ')
 114    format (a7,f7.2,'-',f7.2)
 115    format (' [',i2.2,'.',i1.1,a1,']')
      endif  !initialization
c
      label = labeli
      if     (artype.eq.3 .and. index(name,'/mass').ne.0) then
        label(52:55) = 'eddy'
      endif
c
      if     (abs(iotype).eq.4) then
c
c       NetCDF I/O
c
        write(Ename,'(a3,i3.3)') 'CDF',io
        ncfile = ' '
        call getenv(Ename,ncfile)
        if     (ncfile.eq.' ') then
          write(lp,'(/3a/)')   'error in horout - ',Ename,' not defined'
          call flush(lp)
          stop
        endif
c
        call ncrange(array, 1,jlatn,kz, fill_value, hmin(1),hmax(1))
c
        inquire(file= ncfile, exist=lexist)
        if (.not.lexist) then
          ! create a new NetCDF and write data to it
          ! netcdf-4 classic model, netcdf version 4.3 and later
          call ncheck(nf90_create(trim(ncfile),
     &                            or(nf90_clobber,
     &                               or(nf90_hdf5,
     &                                  nf90_classic_model)),
     &                            ncfileID))
          ! define the dimensions
          call ncheck(nf90_def_dim(ncfileID,
     &                             "MT", nf90_unlimited,MTDimID))
          call ncheck(nf90_def_dim(ncfileID,
     &                             "Latitude",  jlatn,pLatDimID))
          call ncheck(nf90_def_dim(ncfileID,"Depth",kz,lyrDimID))
          ! create the global attributes
          call ncheck(nf90_put_att(ncfileID,nf90_global,
     &                             "Conventions",
     &                             "CF-1.6"))
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
     &                               "archv2ncdfjz"))
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
     &                               "archm2ncdfjz"))
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
     &                          "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "360_day"))
          elseif (yrflag.eq.1) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 0001-01-16 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          elseif (yrflag.eq.2) then
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "model time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 0001-01-01 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "366_day"))
          else
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "long_name",
     &                               "time"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "units",
     &                          "days since 1900-12-31 00:00:00"))
            call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                               "calendar",
     &                               "gregorian"))  !same as standard
          endif
          call ncheck(nf90_put_att(ncfileID,MTVarID,
     &                             "axis","T"))
          call ncheck(nf90_def_var(ncfileID,"Date", nf90_double,
     &                             MTDimID,datVarID))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "long_name",
     &                             "date"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "units",
     &                             "day as %Y%m%d.%f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "C_format",
     &                             "%13.4f"))
          call ncheck(nf90_put_att(ncfileID,datVarID,
     &                             "FORTRAN_format",
     &                             "(f13.4)"))
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
          call ncheck(nf90_def_var(ncfileID,"Depth", nf90_float,
     &                             lyrDimID,lyrVarID))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "units","m"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "positive","down"))
          call ncheck(nf90_put_att(ncfileID,lyrVarID,
     &                             "axis","Z"))
          call ncheck(nf90_def_var(ncfileID,"Latitude",  nf90_float,
     &                             pLatDimID,pLatVarID))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "standard_name","latitude"))
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "units","degrees_north"))
          if     (abs((platj(jlatn)-platj(1))-
     &                (platj(2)    -platj(1))*(jlatn-1)).lt.0.1) then
            call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                               "point_spacing","even"))  !ferret
          endif
          call ncheck(nf90_put_att(ncfileID,pLatVarID,
     &                             "axis","Y"))
          ! model 3Z variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
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
          call ncheck(nf90_put_var(ncfileID,pLatVarID,platj(:)))
          call ncheck(nf90_put_var(ncfileID,lyrVarID,zz(:)))
          ! write to model variable
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
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
          call ncheck(nf90_inq_dimid(ncfileID,"Depth",lyrDimID))
          call ncheck(nf90_inq_dimid(ncfileID,
     &                               "Latitude",pLatDimID))
          !  switch to define mode
          call ncheck(nf90_redef(ncfileID))
          ! define new variable
          call ncheck(nf90_def_var(ncfileID,trim(namec),nf90_float,
     &                             (/pLatDimID,
     &                               lyrDimID, MTDimID/),
     &                             varID))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "coordinates",
     &                             "Date"))
          if     (names.ne." ") then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "standard_name",trim(names)))
          endif
          call ncheck(nf90_put_att(ncfileID,varID,"units",trim(units)))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "_FillValue",fill_value))
          call ncheck(nf90_put_att(ncfileID,varID,
     &                             "valid_range",
     &                             (/hmin(1), hmax(1)/)))
          if     (artype.eq.1) then
            call ncheck(nf90_put_att(ncfileID,varID,
     &                               "long_name",
     &                               trim(name)//label(73:81)))
          elseif (artype.eq.2 .and. time3(2)-time3(1).lt.1.1) then  !daily mean
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
          call ncheck(nf90_put_var(ncfileID,varID,array(:,:)))
          ! close file
          call ncheck(nf90_close(ncfileID))
        endif
        write(lp,'(a49,a,2g15.6)') 
     &    trim(name)//label(51:81),':',hmin(1),hmax(1)
        call flush(lp)
      else
c
c       should never get here.
c
        write(lp,'(/a)')   'error in horout_jz - inconsistent call'
        write(lp,'(3a)')   'label  = "',label(33:len_trim(label)),'"'
        write(lp,'(3a)')   'frmt   = "', frmt( 1:len_trim( frmt)),'"'
        write(lp,'(a,i4)') 'io     = ',io
        write(lp,'(a,i4)') 'iotype = ',iotype
        call flush(lp)
        stop
      endif
      return
      end

      subroutine horout_chunk(ii,jj,kchunk,nb, lp, ichunk,jchunk)
      implicit none
c
      integer, intent(in ) :: ii,jj,kchunk,nb,lp
      integer, intent(out) :: ichunk,jchunk
c
c     return 3-d chunksizes assuming kchunk for the vertical
c     nb is the number of bytes per element
c
      integer mxchunk,nchunk,nchunk2
c
      mxchunk = 4*1024*1024
c     nchunk  = (ii*jj*kchunk*nb+mxchunk)/mxchunk
      mxchunk = (mxchunk+kchunk*nb)/(kchunk*nb)  !to prevent integer overflow
      nchunk  = (ii*jj+mxchunk)/mxchunk
      nchunk2 = sqrt(nchunk+0.1)+1.0
*     write(lp,*) 'nchunk  = ',nchunk
*     write(lp,*) 'nchunk2 = ',nchunk2
      ichunk  = (ii+nchunk2)/nchunk2
      jchunk  = (jj+nchunk2)/nchunk2
      write(lp,*) 'ichunk   =',ichunk
      write(lp,*) 'jchunk   =',jchunk
      write(lp,*) 'kchunk   =',kchunk
      write(lp,*) 'chunk siz=',ichunk*jchunk*kchunk*nb
      end subroutine horout_chunk

      subroutine nchek(cnf90,status)
      use mod_xc   ! HYCOM communication API
      use netcdf   ! NetCDF fortran 90 interface
      implicit none
c
      character*(*), intent(in) :: cnf90
      integer, intent(in) :: status
c
c     subroutine to handle NetCDF errors
c
*     write(lp,'(a)') trim(cnf90)  !debug only
*     call flush(lp)
*
      if (status /= nf90_noerr) then
        write(lp,'(/a)')   'error in horout - from NetCDF library *'
        write(lp,'(a)' )   trim(nf90_strerror(status))
        write(lp,'(a/)')   trim(cnf90)
        call flush(lp)
        stop
      end if
      end subroutine nchek

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
        stop
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

      subroutine ncrange_2d(h,ii,jj, fill_value, hmin,hmax)
      implicit none
c
      integer, intent(in ) :: ii,jj
      real,    intent(in ) :: h(ii,jj),fill_value
      real,    intent(out) :: hmin,hmax
c
c     return range of array, ignoring fill_value
c
      integer i,j
      real    hhmin,hhmax
c
      hhmin =  abs(fill_value)
      hhmax = -abs(fill_value)
      do j= 1,jj
        do i= 1,ii
          if     (h(i,j).ne.fill_value) then
              hhmin = min(hhmin,h(i,j))
              hhmax = max(hhmax,h(i,j))
          endif
        enddo
      enddo
      hmin = hhmin
      hmax = hhmax
      end subroutine ncrange_2d

      subroutine ncrange_1d(h,ii, fill_value, hmin,hmax)
      implicit none
c
      integer, intent(in ) :: ii
      real,    intent(in ) :: h(ii),fill_value
      real,    intent(out) :: hmin,hmax
c
c     return range of array, ignoring fill_value
c
      integer i
      real    hhmin,hhmax
c
      hhmin =  abs(fill_value)
      hhmax = -abs(fill_value)
      do i= 1,ii
        if     (h(i).ne.fill_value) then
            hhmin = min(hhmin,h(i))
            hhmax = max(hhmax,h(i))
        endif
      enddo
      hmin = hhmin
      hmax = hhmax
      end subroutine ncrange_1d

