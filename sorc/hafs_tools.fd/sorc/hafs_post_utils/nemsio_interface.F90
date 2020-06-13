module nemsio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! post-utils :: nemsio_interface
  ! Copyright (C) 2019 Henry R. Winterbottom

  ! Email: henry.winterbottom@noaa.gov

  ! This program is free software: you can redistribute it and/or
  ! modify it under the terms of the GNU General Public License as
  ! published by the Free Software Foundation, either version 3 of the
  ! License, or (at your option) any later version.

  ! This program is distributed in the hope that it will be useful,
  ! but WITHOUT ANY WARRANTY; without even the implied warranty of
  ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ! General Public License for more details.

  ! You should have received a copy of the GNU General Public License
  ! along with this program. If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use gridprojs_interface
  use kinds_interface
  use namelist_interface
  use nemsio_module
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: nemsio_interface_close
  public :: nemsio_interface_open
  public :: nemsio_interface_write
  interface nemsio_interface_open
     module procedure fv3_open
  end interface nemsio_interface_open
  interface nemsio_interface_write
     module procedure fv3_write
  end interface nemsio_interface_write
  
  ! Define local variables

  integer(nemsio_intkind)                                               :: iret
  
  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_init_write.f90

  ! DESCRIPTION:

  ! This subroutine initializes all attributes and variable arrays
  ! required to write to a NEMSIO-formatted file.

  ! INPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable; it is assumed
  !   that the number of vertical levels (dimz), the remapping
  !   projection spectral truncation (ntrunc), and the filename
  !   (filename) attributes have been defined prior to calling this
  !   routine.

  ! * json; a FORTRAN fv3_json_glbl_struct variable array.

  ! OUTPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable containing all
  !   attributes and variable arrays required to write to a
  !   NEMSIO-formatted file.

  !-----------------------------------------------------------------------

  subroutine fv3_init_write(nemsiometa,vcoord,json)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: json(:)
    type(nemsiometa_struct)                                             :: nemsiometa
    real(r_kind)                                                        :: vcoord(:,:)

    ! Define variables computed within routine

    type(specgrids_grid)                                                :: specgrids
    integer                                                             :: n2d
    integer                                                             :: n3d

    ! Define counting variables

    integer                                                             :: i, j
    
    !=====================================================================

    ! Define local variables

    nemsiometa%idate(1:6) = 0
    nemsiometa%idate(7)   = 1
    read(analdate(1:4),   '(i4)') nemsiometa%idate(1)
    read(analdate(6:7),   '(i2)') nemsiometa%idate(2)
    read(analdate(9:10),  '(i2)') nemsiometa%idate(3)
    read(analdate(12:13), '(i2)') nemsiometa%idate(4)
    read(analdate(15:16), '(i2)') nemsiometa%idate(5)
    read(analdate(18:19), '(i2)') nemsiometa%idate(6)
    nemsiometa%modelname  = 'FV3GFS'
    nemsiometa%version    = 198410    
    nemsiometa%gdatatype  = 'bin4'
    specgrids%ntrunc      = nemsiometa%ntrunc
    call specgrids_compute(specgrids)
    nemsiometa%dimx       = specgrids%nlons
    nemsiometa%dimy       = specgrids%nlats
    n2d                   = 0
    n3d                   = 0

    ! Loop through local variable

    do i = 1, size(json)

       ! Check local variable and proceed accordingly

       if(json(i)%levtype .eq. 2) n2d = n2d + 1
       if(json(i)%levtype .eq. 3) n3d = n3d + 1

    end do ! do i = 1, size(json)

    ! Define local variables

    nemsiometa%nrec      = n2d + nemsiometa%dimz*n3d
    nemsiometa%nmeta     = 8
    nemsiometa%nmetavari = 4
    nemsiometa%nmetavarr = 1
    nemsiometa%nmetaaryi = 1
    nemsiometa%nframe    = 0
    nemsiometa%nfminute  = 0
    nemsiometa%nfsecondd = 1
    nemsiometa%nfsecondn = 0
    nemsiometa%ntrac     = 3
    nemsiometa%idrt      = 0   
    nemsiometa%ncldt     = 3
    nemsiometa%idvc      = 2
    nemsiometa%rlat_min  = minval(specgrids%lats)
    nemsiometa%rlat_max  = maxval(specgrids%lats)
    nemsiometa%rlon_min  = minval(specgrids%lons)
    nemsiometa%rlon_max  = maxval(specgrids%lons)    
    call variable_interface_setup_struct(nemsiometa)
    nemsiometa%lat       = specgrids%lats
    nemsiometa%lon       = specgrids%lons    
    
    ! Deallocate memory for local variables

    call specgrids_cleanup(specgrids)

    ! Define local variables

    nemsiometa%reclev(:)    = 1
    nemsiometa%varrname(1)  = 'zhour'
    nemsiometa%variname(1)  = 'cu_physics'
    nemsiometa%varival(1)   = 4
    nemsiometa%variname(2)  = 'mp_physics'
    nemsiometa%varival(2)   = 1000 
    nemsiometa%variname(3)  = 'IVEGSRC'
    nemsiometa%varival(3)   = 2
    nemsiometa%variname(4)  = 'NVCOORD'
    nemsiometa%varival(4)   = 2
    nemsiometa%aryilen(1)   = nemsiometa%dimy/2
    nemsiometa%aryiname(1)  = 'lpl'
    nemsiometa%aryival(:,1) = nemsiometa%dimx
    nemsiometa%vcoord       = 0.0
    
    ! Loop through local variable

    do i = 1, nemsiometa%dimz
    
       ! Define local variables

       nemsiometa%vcoord((nemsiometa%dimz - i + 1),1,1) = vcoord(i,1)
       nemsiometa%vcoord((nemsiometa%dimz - i + 1),2,1) = vcoord(i,2)

    end do ! do i = 1, nemsiometa%dimz

    ! Define local variables

    n2d = 0
    
    ! Loop through local variable

    do i = 1, size(json)

       ! Check local variable and proceed accordingly

       if(json(i)%levtype .eq. 2) then

          ! Define local variables

          n2d                       = n2d + 1
          nemsiometa%recname(n2d)   =                                      &
               & trim(adjustl(json(i)%nems_variable_name))
          nemsiometa%reclevtyp(n2d) =                                      &
               & trim(adjustl(json(i)%nems_vcoord_name))

       end if ! if(json(i)%levtype .eq. 2)
          
    end do ! do i = 1, size(json)

    ! Define local variables

    n3d = 0
    
    ! Loop through local variable

    do i = 1, size(json)    

       ! Check local variable and proceed accordingly

       if(json(i)%levtype .eq. 3) then

          ! Loop through local variable

          do j = 1, nemsiometa%dimz

             ! Define local variables
          
             nemsiometa%recname(j + n2d + nemsiometa%dimz*n3d)   =         &
                  & trim(adjustl(json(i)%nems_variable_name))
             nemsiometa%reclevtyp(j + n2d + nemsiometa%dimz*n3d) =         &
                  & trim(adjustl(json(i)%nems_vcoord_name))
             nemsiometa%reclev(j + n2d + nemsiometa%dimz*n3d)    = j
             
          end do ! do j = 1, nemsiometa%dimz

          ! Define local variables

          n3d = n3d + 1
          
       end if ! if(json(i)%levtype .eq. 3)
       
    end do ! do i = 1, size(json) 
    
    !=====================================================================

  end subroutine fv3_init_write
  
  !=======================================================================

  ! SUBROUTINE:

  ! fv3_open.f90

  ! DESCRIPTION:

  ! This subroutine opens a NEMSIO-formatted file in accordance with
  ! the user specifications.

  ! INPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable; it is assumed
  !   that the number of vertical levels (dimz), the remapping
  !   projection spectral truncation (ntrunc), and the filename
  !   (filename) attributes have been defined prior to calling this
  !   routine.

  ! * json; a FORTRAN fv3_json_glbl_struct variable array.

  ! OPTIONAL INPUT VARIABLES:

  ! * is_read; a FORTRAN logical variable specifying to open the
  !   NEMSIO-formatted file for reading only; currently does nothing.

  ! * is_rdwr; a FORTRAN logical variable specifying to open the
  !   NEMSIO-formatted file for reading and writing.

  ! * is_write; a FORTRAN logical variable specifying to open the
  !   NEMSIO-formatted file for writing only.
  
  ! OUTPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable containing the
  !   NEMSIO-formatted file attributes.

  !-----------------------------------------------------------------------

  subroutine fv3_open(nemsiometa,json,vcoord,is_read,is_rdwr,is_write)

    ! Define variables passed to routine

    type(fv3_json_glbl_struct)                                          :: json(:)
    type(nemsiometa_struct)                                             :: nemsiometa
    logical,            optional                                        :: is_read
    logical,            optional                                        :: is_rdwr
    logical,            optional                                        :: is_write
    real(r_kind),       optional                                        :: vcoord(:,:)
    
    ! Define variables computed within routine

    logical                                                             :: is_lread
    logical                                                             :: is_lrdwr
    logical                                                             :: is_lwrite
    
    !=====================================================================

    ! Define local variables

    is_lread  = .false.
    is_lrdwr  = .false.
    is_lwrite = .false.

    ! Check local variable and proceed accordingly

    if(present(is_read))  is_lread  = is_read
    if(present(is_rdwr))  is_lrdwr  = is_rdwr
    if(present(is_write)) is_lwrite = is_write

    ! Define local variables
    
    call nemsio_init(iret=iret)

    ! Check local variable and proceed accordingly

    if(is_lread) then

       ! Check local variable and proceed accordingly

       if(iret /= 0) then

          ! Define local variables

          write(6,501) iret
          stop

       end if ! if(iret /= 0)

    end if ! if(is_lread)

    ! Check local variable and proceed accordingly

    if(is_lrdwr) then    

       ! Define local variables

       call nemsio_open(nemsiometa%gfile,                                  &
            & trim(adjustl(nemsiometa%filename)),'rdwr',iret=iret)

       ! Check local variable and proceed accordingly

       if(iret /= 0) then

          ! Define local variables

          write(6,502) iret
          stop

       end if ! if(iret /= 0)
       
    end if ! if(is_lrdwr)
       
    ! Check local variable and proceed accordingly

    if(is_lwrite) then

       ! Check local variable and proceed accordingly

       if(.not. present(vcoord)) then

          ! Define local variables

          write(6,500)
          stop

       end if ! if(.not. present(vcoord))

       ! Define local variables
       
       call fv3_init_write(nemsiometa,vcoord,json)       
       call nemsio_open(nemsiometa%gfile,                                  &
            & trim(adjustl(nemsiometa%filename)),'write',iret=iret,        &
            & modelname=trim(nemsiometa%modelname),                        &
            & version=nemsiometa%version,gdatatype=nemsiometa%gdatatype,   &
            & dimx=nemsiometa%dimx,dimy=nemsiometa%dimy,                   &
            & dimz=nemsiometa%dimz,rlon_min=nemsiometa%rlon_min,           &
            & rlon_max=nemsiometa%rlon_max,rlat_min=nemsiometa%rlat_min,   &
            & rlat_max=nemsiometa%rlat_max,lon=nemsiometa%lon,             &
            & lat=nemsiometa%lat,idate=nemsiometa%idate,                   &
            & nrec=nemsiometa%nrec,nframe=nemsiometa%nframe,               &
            & idrt=nemsiometa%idrt,ncldt=nemsiometa%ncldt,                 &
            & idvc=nemsiometa%idvc,nfhour=nemsiometa%nfhour,               &
            & nfminute=nemsiometa%nfminute,nfsecondn=nemsiometa%nfsecondn, &
            & nmeta=nemsiometa%nmeta,nfsecondd=nemsiometa%nfsecondd,       &
            & extrameta=.true.,nmetaaryi=nemsiometa%nmetaaryi,             &
            & recname=nemsiometa%recname,nmetavari=nemsiometa%nmetavari,   &
            & variname=nemsiometa%variname,varival=nemsiometa%varival,     &
            & varrval=nemsiometa%varrval,aryilen=nemsiometa%aryilen,       &
            & nmetavarr=nemsiometa%nmetavarr,varrname=nemsiometa%varrname, &
            & reclevtyp=nemsiometa%reclevtyp,reclev=nemsiometa%reclev,     &
            & aryiname=nemsiometa%aryiname,aryival=nemsiometa%aryival,     &
            & vcoord=nemsiometa%vcoord,jcap=nemsiometa%ntrunc)
       
       ! Check local variable and proceed accordingly

       if(iret /= 0) then

          ! Define local variables

          write(6,503) iret
          stop

       end if ! if(iret /= 0)
       
    end if ! if(is_lwrite)

    ! Define local variables

500 format('FV3_OPEN: When writing a NEMSIO-formatted file, the ',         &
         & 'vertical coordinate values must be specified. Aborting!!!')
501 format('FV3_OPEN: nemsio_open for reading failed with error ',i,       &
         & '. Aborting!!!')
502 format('FV3_OPEN: nemsio_open for reading and writing failed with ',   &
         & 'error ',i,'. Aborting!!!')
503 format('FV3_OPEN: nemsio_open for writing failed with error ',i,       &
         & '. Aborting!!!')

    !=====================================================================

  end subroutine fv3_open

  !=======================================================================

  ! SUBROUTINE:

  ! fv3_write.f90

  ! DESCRIPTION:

  ! This subroutine writes a variable array to a NEMSIO-formatted
  ! record.

  ! INPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable containing (at
  !   minimum) a defined value for the gfile attribute.

  ! * nemsiovar; a FORTRAN nemsiovar_struct variable containing the
  !   attributes describing the NEMSIO-formatted variable and the
  !   array of values corresponding to the variable.
  
  !-----------------------------------------------------------------------

  subroutine fv3_write(nemsiometa,nemsiovar)

    ! Define variables passed to routine

    type(nemsiometa_struct)                                             :: nemsiometa
    type(nemsiovar_struct)                                              :: nemsiovar

    ! Define variables computed within routine

    character(nemsio_charkind)                                          :: levtyp
    character(nemsio_charkind)                                          :: recname
    integer                                                             :: level
    
    !=====================================================================

    ! Define local variables

    levtyp  = trim(adjustl(nemsiovar%nems_vcoord_name))
    recname = trim(adjustl(nemsiovar%nems_variable_name))
    level   = nemsiovar%level
    call nemsio_writerecv(nemsiometa%gfile,trim(adjustl(recname)),         &
         & levtyp=trim(adjustl(levtyp)),lev=level,data=nemsiovar%var,      &
         & iret=iret)

    !=====================================================================
    
  end subroutine fv3_write
  
  !=======================================================================

  ! SUBROUTINE:

  ! nemsio_interface_close.f90

  ! DESCRIPTION:

  ! This subroutine closes an open NEMSIO-formatted file.

  ! INPUT VARIABLES:

  ! * nemsiometa; a FORTRAN nemsiometa_struct variable containing a
  !   pointer to an open NEMSIO-formatted file (gfile).

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_close(nemsiometa)

    ! Define variables passed to routine

    type(nemsiometa_struct)                                             :: nemsiometa
  
    !=====================================================================

    ! Define local variables

    call nemsio_close(nemsiometa%gfile,iret=iret)

    !=====================================================================
  
  end subroutine nemsio_interface_close
      
  !=======================================================================

end module nemsio_interface
