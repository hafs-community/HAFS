module netcdf_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: netcdf_interface
  ! Copyright (C) 2020 Henry R. Winterbottom

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
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface
  use netcdf
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: ncdimid
  public :: ncfileid
  public :: ncstatus
  public :: ncvarid
  public :: netcdf_interface_close
  public :: netcdf_interface_getattr
  public :: netcdf_interface_getdim
  public :: netcdf_interface_getvar
  public :: netcdf_interface_open
  public :: netcdf_interface_putattr
  public :: netcdf_interface_putvar
  public :: netcdf_interface_writedef
  interface netcdf_interface_getattr
     module procedure getattr_char
     module procedure getattr_real
  end interface netcdf_interface_getattr
  interface netcdf_interface_getvar
     module procedure getvar_real
     module procedure getvar_real_1d
     module procedure getvar_real_2d
     module procedure getvar_real_3d
     module procedure getvar_real_4d
  end interface netcdf_interface_getvar
  interface netcdf_interface_putattr
     module procedure putattr_char
     module procedure putattr_int
     module procedure putattr_real
  end interface netcdf_interface_putattr
  interface netcdf_interface_putvar
     module procedure putvar_char_1d
     module procedure putvar_real_1d
     module procedure putvar_real_2d
     module procedure putvar_real_3d
     module procedure putvar_real_4d
  end interface netcdf_interface_putvar
  
  ! Define local variables

  integer                                                               :: ncdimid
  integer                                                               :: ncfileid
  integer                                                               :: ncstatus
  integer                                                               :: ncvarid

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! getattr_char.f90

  ! DESCRIPTION:

  ! This subroutine retrieves a character attribute from a netcdf
  ! file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * attrname; a FORTRAN character string specifying the netcdf
  !   attribute.

  ! * var; a FORTRAN character string to contain the netcdf attribute
  !   value.

  ! OPTIONAL INPUT VARIABLES:

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key; if not present, a global netcdf attribute is
  !   assumed.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN character string specifying the netcdf attribute
  !   value.

  !-----------------------------------------------------------------------

  subroutine getattr_char(filename,attrname,var,varname)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: attrname
    character(len=100), optional                                        :: varname
    character(len=*)                                                    :: var

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)

    ! Check local variable and proceed accordingly

    if(present(varname)) then

       ! Define local variables

       ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
       ncstatus = nf90_get_att(ncfileid,ncvarid,trim(adjustl(attrname)),   &
            & var)

    else   ! if(present(varname))

       ! Define local variables

       ncstatus = nf90_get_att(ncfileid,nf90_global,                       &
            & trim(adjustl(attrname)),var)

    end if ! if(present(varname))

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine getattr_char

  !=======================================================================

  ! SUBROUTINE:

  ! getattr_real.f90

  ! DESCRIPTION:

  ! This subroutine retrieves a 4-byte real-valued attribute from a
  ! netcdf file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * attrname; a FORTRAN character string specifying the netcdf
  !   attribute.

  ! * var; a FORTRAN 4-byte real-valued variable to contain the netcdf
  !   attribute value.

  ! OPTIONAL INPUT VARIABLES:

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key; if not present, a global netcdf attribute is
  !   assumed.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN 4-byte real-valued variable specifying the netcdf
  !   attribute value.

  !-----------------------------------------------------------------------

  subroutine getattr_real(filename,attrname,var,varname)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: attrname
    character(len=100), optional                                        :: varname
    real(r_kind)                                                        :: var

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)

    ! Check local variable and proceed accordingly

    if(present(varname)) then

       ! Define local variables

       ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
       ncstatus = nf90_get_att(ncfileid,ncvarid,trim(adjustl(attrname)),   &
            & var)

    else   ! if(present(varname))

       ! Define local variables

       ncstatus = nf90_get_att(ncfileid,nf90_global,                       &
            & trim(adjustl(attrname)),var)

    end if ! if(present(varname))

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine getattr_real

  !=======================================================================

  ! SUBROUTINE:

  ! getvar_real.f90

  ! DESCRIPTION:

  ! This subroutine reads a scalar variable from a user specified
  ! netcdf file specified by the user supplied netcdf API key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 4-byte precision variable to contain the contents
  !   of the user specified netcdf variable.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN 4-byte precision variable containing the contents
  !   of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine getvar_real(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,var)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine getvar_real

  !=======================================================================

  ! SUBROUTINE:

  ! getvar_real_1d.f90

  ! DESCRIPTION:

  ! This subroutine reads a 1-dimensional variable from a user
  ! specified netcdf file specified by the user supplied netcdf API
  ! key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 1-dimensional 4-byte precision variable array to
  !   contain the contents of the user specified netcdf variable.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN 1-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine getvar_real_1d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:)

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: workgrid
    integer                                                             :: d1

    !=====================================================================

    ! Define local variables

    d1 = size(var(:))

    ! Allocate memory for local variables

    if(.not. allocated(workgrid)) allocate(workgrid(d1))

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,workgrid)
    var      = workgrid
    call netcdf_interface_close()

    ! Deallocate memory for local variables
    
    if(allocated(workgrid)) deallocate(workgrid)

    !=====================================================================

  end subroutine getvar_real_1d

  !=======================================================================

  ! SUBROUTINE:

  ! getvar_real_2d.f90

  ! DESCRIPTION:

  ! This subroutine reads a 2-dimensional variable from a user
  ! specified netcdf file specified by the user supplied netcdf API
  ! key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 2-dimensional 4-byte precision variable array to
  !   contain the contents of the user specified netcdf variable.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN 2-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine getvar_real_2d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:,:)

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:),             allocatable :: workgrid
    integer                                                             :: d1
    integer                                                             :: d2

    !=====================================================================

    ! Define local variables

    d1 = size(var(:,1))
    d2 = size(var(1,:))

    ! Allocate memory for local variables

    if(.not. allocated(workgrid)) allocate(workgrid(d1,d2))

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,workgrid)
    var      = workgrid
    call netcdf_interface_close()

    ! Deallocate memory for local variables
    
    if(allocated(workgrid)) deallocate(workgrid)

    !=====================================================================

  end subroutine getvar_real_2d

  !=======================================================================

  ! SUBROUTINE:

  ! getvar_real_3d.f90

  ! DESCRIPTION:

  ! This subroutine reads a 3-dimensional variable from a user
  ! specified netcdf file specified by the user supplied netcdf API
  ! key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 3-dimensional 4-byte precision variable array to
  !   contain the contents of the user specified netcdf variable.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN 3-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine getvar_real_3d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:,:,:)

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:,:),           allocatable :: workgrid
    integer                                                             :: d1
    integer                                                             :: d2
    integer                                                             :: d3

    !=====================================================================

    ! Define local variables

    d1 = size(var(:,1,1))
    d2 = size(var(1,:,1))
    d3 = size(var(1,1,:))

    ! Allocate memory for local variables

    if(.not. allocated(workgrid)) allocate(workgrid(d1,d2,d3))

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,workgrid)
    var      = workgrid
    call netcdf_interface_close()

    ! Deallocate memory for local variables
    
    if(allocated(workgrid)) deallocate(workgrid)

    !=====================================================================

  end subroutine getvar_real_3d

  !=======================================================================

  ! SUBROUTINE:

  ! getvar_real_4d.f90

  ! DESCRIPTION:

  ! This subroutine reads a 4-dimensional variable from a user
  ! specified netcdf file specified by the user supplied netcdf API
  ! key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 4-dimensional 4-byte precision variable array to
  !   contain the contents of the user specified netcdf variable.

  ! OUTPUT VARIABLES:

  ! * var; a FORTRAN 4-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine getvar_real_4d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:,:,:,:)

    ! Define variables computed within routine

    real(r_kind),               dimension(:,:,:,:),         allocatable :: workgrid
    integer                                                             :: d1
    integer                                                             :: d2
    integer                                                             :: d3
    integer                                                             :: d4

    !=====================================================================

    ! Define local variables

    d1 = size(var(:,1,1,1))
    d2 = size(var(1,:,1,1))
    d3 = size(var(1,1,:,1))    
    d4 = size(var(1,1,1,:))

    ! Allocate memory for local variables

    if(.not. allocated(workgrid)) allocate(workgrid(d1,d2,d3,d4))

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_get_var(ncfileid,ncvarid,workgrid)
    var      = workgrid
    call netcdf_interface_close()

    ! Deallocate memory for local variables
    
    if(allocated(workgrid)) deallocate(workgrid)

    !=====================================================================

  end subroutine getvar_real_4d

  !=======================================================================

  ! SUBROUTINE:

  ! netcdf_interface_close.f90

  ! DESCRIPTION:

  ! This subroutine closes a Network Common Data Format (netcdf) file
  ! defined by the FORTRAN integer variable ncfileid.

  !-----------------------------------------------------------------------

  subroutine netcdf_interface_close()

    !=====================================================================

    ! Define local variables

    ncstatus = nf90_close(ncfileid)

    !=====================================================================

  end subroutine netcdf_interface_close

  !=======================================================================

  ! SUBROUTINE:

  ! netcdf_interface_getdim.f90

  ! DESCRIPTION:

  ! This subroutine assigns the dimension value associated with the
  ! netcdf API key specified by the user.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * dimname; a FORTRAN character string specifying the netcdf API
  !   key for the respective netcdf dimension variable.

  ! * dimval; a FORTRAN integer variable to contain the netcdf
  !   dimension value.

  ! OUTPUT VARIABLES:

  ! * dimval; a FORTRAN integer value specifying the netcdf dimension
  !   value.

  !-----------------------------------------------------------------------

  subroutine netcdf_interface_getdim(filename,dimname,dimval)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename
    character(len=100)                                                  :: dimname
    integer                                                             :: dimval

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.true.,.false.,.false.)
    ncstatus = nf90_inq_dimid(ncfileid,trim(adjustl(dimname)),ncdimid)
    ncstatus = nf90_inquire_dimension(ncfileid,ncdimid,len=dimval)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine netcdf_interface_getdim

  !=======================================================================

  ! SUBROUTINE:

  ! netcdf_interface_open.f90

  ! DESCRIPTION:

  ! This subroutine opens a Network Common Data Format (netcdf) file
  ! with permissions in accordance with the specifications of the
  ! user; the global variable ncfileid is defined by this subroutine
  ! until terminated by the subroutine netcdf_interface_close.f90.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * is_read; a FORTRAN logical variable specifying to open the
  !   netcdf file with read-only permissions.

  ! * is_rdwr; a FORTRAN logical variable specifying to open the
  !   netcdf file with read and write permissions.

  ! * is_write; a FORTRAN logical variable specifying to open a new
  !   (and clobber any previous existence of a) the netcdf file with
  !   write permissions.

  !-----------------------------------------------------------------------

  subroutine netcdf_interface_open(filename,is_read,is_rdwr,is_write)

    ! Define variables passed to routine

    character(len=500)                                                  :: filename
    logical                                                             :: is_read
    logical                                                             :: is_rdwr
    logical                                                             :: is_write

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_read) then

       ! Define local variables

       ncstatus = nf90_open(trim(adjustl(filename)),nf90_nowrite,          &
            & ncfileid)

    end if ! if(is_read)

    ! Check local variable and proceed accordingly

    if(is_rdwr) then

       ! Define local variables

       ncstatus = nf90_open(trim(adjustl(filename)),nf90_write,            &
            & ncfileid)

    end if ! if(is_rdwr)

    ! Check local variable and proceed accordingly

    if(is_write) then

       ! Define local variables

       ncstatus = nf90_create(path=trim(adjustl(filename)),                &
            & cmode=or(nf90_clobber,nf90_64bit_offset),ncid=ncfileid)

    end if ! if(is_write)
    
    !=====================================================================

  end subroutine netcdf_interface_open

  !=======================================================================

  ! SUBROUTINE:

  ! netcdf_interface_writedef.f90

  ! DESCRIPTION:

  ! This subroutine writes the Network Common Data Format (netcdf)
  ! file dimension and variable definition section.

  ! INPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable containing the
  !   dimension and variable definitions for the netcdf file.

  !-----------------------------------------------------------------------

  subroutine netcdf_interface_writedef(varinfo)

    ! Define variables passed to routine

    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       ncstatus = nf90_def_dim(ncfileid,trim(adjustl(varinfo%dimname(i))),  &
            & varinfo%dimval(i),varinfo%dimid(i))

    end do ! do i = 1, varinfo

    ! Loop through local variable

    do i = 1, varinfo%nvars

       ! Allocate memory for local variables

       if(.not. allocated(dimid)) allocate(dimid(varinfo%varndims(i)))
       
       ! Loop through local variable

       do j = 1, varinfo%varndims(i)

          ! Define local variables

          dimid(j) = varinfo%vardimid(i,j)

       end do ! do j = 1, varinfo%varndims(i)

       ! Check local variable and proceed accordingly

       if(varinfo%vartype(i) .eq. 'float') then

          ! Define local variables

          ncstatus = nf90_def_var(ncfileid,                                 &
               & trim(adjustl(varinfo%varname(i))),nf90_float,              &
               & dimid(1:varinfo%varndims(i)),varinfo%varid(i))

       end if ! if(varinfo%vartype(i) .eq. 'float')

       ! Check local variable and proceed accordingly

       if(varinfo%vartype(i) .eq. 'integer') then

          ! Define local variables

          ncstatus = nf90_def_var(ncfileid,                                 &
               & trim(adjustl(varinfo%varname(i))),nf90_int,                &
               & dimid(1:varinfo%varndims(i)),varinfo%varid(i))

       end if ! if(varinfo%vartype(i) .eq. 'integer')

       ! Check local variable and proceed accordingly

       if(varinfo%vartype(i) .eq. 'char') then

          ! Define local variables

          ncstatus = nf90_def_var(ncfileid,                                 &
               & trim(adjustl(varinfo%varname(i))),nf90_char,               &
               & dimid(1:varinfo%varndims(i)),varinfo%varid(i))

       end if ! if(varinfo%vartype(i) .eq. 'char')

       ! Loop through local variable

       do j = 1, varinfo%varnattrs(i)

          ! Define local variables

          ncstatus = nf90_put_att(ncfileid,varinfo%varid(i),                &
               & trim(adjustl(varinfo%varattrs(i,j,1))),                    &
               & trim(adjustl(varinfo%varattrs(i,j,2))))

          ! Check local variable and proceed accordingly

          if(varinfo%vartype(i) .eq. 'float') then
             
             ! Define local variables

             ncstatus = nf90_put_att(ncfileid,varinfo%varid(i),             &
                  & '_FillValue',spval)

          end if !  if(varinfo%vartype(i) .eq. 'float')

          ! Check local variable and proceed accordingly

          if(varinfo%vartype(i) .eq. 'integer') then
             
             ! Define local variables

             ncstatus = nf90_put_att(ncfileid,varinfo%varid(i),             &
                  & '_FillValue',-99)

          end if !  if(varinfo%vartype(i) .eq. 'integer')

       end do ! do j = 1, varinfo%varnattrs(i)

       ! Deallocate memory for local variables

       if(allocated(dimid)) deallocate(dimid)

    end do ! do i = 1, varinfo%nvars

    ! Define local variables

    ncstatus = nf90_enddef(ncfileid)

    !=====================================================================

  end subroutine netcdf_interface_writedef

  !=======================================================================

  ! SUBROUTINE:

  ! putattr_char.f90

  ! DESCRIPTION:

  ! This subroutine writes a character attribute to a netcdf file.

  ! INPUT VARIABLES:

  ! * attrname; a FORTRAN character string specifying the netcdf
  !   attribute.

  ! * var; a FORTRAN character string to contain the netcdf attribute
  !   value.

  ! OPTIONAL INPUT VARIABLES:

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key; if not present, a global netcdf attribute is
  !   assumed.

  !-----------------------------------------------------------------------

  subroutine putattr_char(attrname,var,varname)

    ! Define variables passed to routine
    
    character(len=100)                                                  :: attrname
    character(len=100), optional                                        :: varname
    character(len=*)                                                    :: var

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(present(varname)) then

       ! Define local variables

       ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
       ncstatus = nf90_put_att(ncfileid,ncvarid,trim(adjustl(attrname)),   &
            & var)

    else   ! if(present(varname))

       ! Define local variables

       ncstatus = nf90_put_att(ncfileid,nf90_global,                       &
            & trim(adjustl(attrname)),var)

    end if ! if(present(varname))

    !=====================================================================

  end subroutine putattr_char

  !=======================================================================

  ! SUBROUTINE:

  ! putattr_int.f90

  ! DESCRIPTION:

  ! This subroutine writes a 4-byte integer-value variable attribute
  ! to a netcdf file.

  ! INPUT VARIABLES:

  ! * attrname; a FORTRAN character string specifying the netcdf
  !   attribute.

  ! * var; a FORTRAN 4-byte integer variable containing the netcdf
  !   attribute value.

  ! OPTIONAL INPUT VARIABLES:

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key; if not present, a global netcdf attribute is
  !   assumed.

  !-----------------------------------------------------------------------

  subroutine putattr_int(attrname,var,varname)

    ! Define variables passed to routine
    
    character(len=100)                                                  :: attrname
    character(len=100), optional                                        :: varname
    integer                                                             :: var

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(present(varname)) then

       ! Define local variables

       ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
       ncstatus = nf90_put_att(ncfileid,ncvarid,trim(adjustl(attrname)),   &
            & var)

    else   ! if(present(varname))

       ! Define local variables

       ncstatus = nf90_put_att(ncfileid,nf90_global,                       &
            & trim(adjustl(attrname)),var)

    end if ! if(present(varname))

    !=====================================================================

  end subroutine putattr_int
  
  !=======================================================================

  ! SUBROUTINE:

  ! putattr_real.f90

  ! DESCRIPTION:

  ! This subroutine writes a 4-byte real-valued variable attribute to
  ! a netcdf file.

  ! INPUT VARIABLES:

  ! * attrname; a FORTRAN character string specifying the netcdf
  !   attribute.

  ! * var; a FORTRAN 4-byte real-valued variable containing the netcdf
  !   attribute value.

  ! OPTIONAL INPUT VARIABLES:

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key; if not present, a global netcdf attribute is
  !   assumed.

  !-----------------------------------------------------------------------

  subroutine putattr_real(attrname,var,varname)

    ! Define variables passed to routine
    
    character(len=100)                                                  :: attrname
    character(len=100), optional                                        :: varname
    real(r_kind)                                                        :: var

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(present(varname)) then

       ! Define local variables

       ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
       ncstatus = nf90_put_att(ncfileid,ncvarid,trim(adjustl(attrname)),   &
            & var)

    else   ! if(present(varname))

       ! Define local variables

       ncstatus = nf90_put_att(ncfileid,nf90_global,                       &
            & trim(adjustl(attrname)),var)

    end if ! if(present(varname))

    !=====================================================================

  end subroutine putattr_real

  !=======================================================================

  ! SUBROUTINE:

  ! putvar_char_1d.f90

  ! DESCRIPTION:

  ! This subroutine write a 1-dimensional variable to a user specified
  ! netcdf file in accordance with the user supplied netcdf API key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 1-dimensional character variable array containing
  !   the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine putvar_char_1d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    character(len=*)                                                    :: var(:)

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.true.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,varname,ncvarid)
    ncstatus = nf90_put_var(ncfileid,ncvarid,var)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine putvar_char_1d
  
  !=======================================================================

  ! SUBROUTINE:

  ! putvar_real_1d.f90

  ! DESCRIPTION:

  ! This subroutine write a 1-dimensional variable to a user specified
  ! netcdf file in accordance with the user supplied netcdf API key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 1-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine putvar_real_1d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:)

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.true.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_put_var(ncfileid,ncvarid,var)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine putvar_real_1d

  !=======================================================================

  ! SUBROUTINE:

  ! putvar_real_2d.f90

  ! DESCRIPTION:

  ! This subroutine write a 2-dimensional variable to a user specified
  ! netcdf file in accordance with the user supplied netcdf API key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 2-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine putvar_real_2d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:,:)

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.true.,.false.)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)
    ncstatus = nf90_put_var(ncfileid,ncvarid,var)
    call netcdf_interface_close()

    !=====================================================================

  end subroutine putvar_real_2d

  !=======================================================================

  ! SUBROUTINE:

  ! putvar_real_3d.f90

  ! DESCRIPTION:

  ! This subroutine write a 3-dimensional variable to a user specified
  ! netcdf file in accordance with the user supplied netcdf API key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 3-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine putvar_real_3d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:,:,:)

    ! Define variables computed within routine

    integer                                                             :: start(3)
    integer                                                             :: count(3)
    
    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.true.,.false.)
    count    = (/size(var(:,1,1)),size(var(1,:,1)),1/)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)

    ! Loop through local variable

    do i = 1, size(var(1,1,:))

       ! Define local variables

       start    = (/1,1,i/)
       ncstatus = nf90_put_var(ncfileid,ncvarid,var(:,:,i),start=start,    &
            & count=count)

    end do ! do i = 1, size(var(1,1,:))

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine putvar_real_3d

  !=======================================================================

  ! SUBROUTINE:

  ! putvar_real_4d.f90

  ! DESCRIPTION:

  ! This subroutine write a 4-dimensional variable to a user specified
  ! netcdf file in accordance with the user supplied netcdf API key.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the netcdf file.

  ! * varname; a FORTRAN character string specifying the netcdf
  !   variable API key.

  ! * var; a FORTRAN 4-dimensional 4-byte precision variable array
  !   containing the contents of the user specified netcdf variable.

  !-----------------------------------------------------------------------

  subroutine putvar_real_4d(filename,varname,var)

    ! Define variables passed to routine
    
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    real(r_kind)                                                        :: var(:,:,:,:)

    ! Define variables computed within routine

    integer                                                             :: start(4)
    integer                                                             :: count(4)
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    call netcdf_interface_open(filename,.false.,.true.,.false.)
    count    = (/size(var(:,1,1,1)),size(var(1,:,1,1)),1,1/)
    ncstatus = nf90_inq_varid(ncfileid,trim(adjustl(varname)),ncvarid)

    ! Loop through local variable

    do i = 1, size(var(1,1,1,:))

       ! Loop through local variable

       do j = 1, size(var(1,1,:,i))

          ! Define local variables

          start    = (/1,1,j,i/)
          ncstatus = nf90_put_var(ncfileid,ncvarid,var(:,:,j,i),           &
               & start=start,count=count)

       end do ! do j = 1, size(var(1,1,:,i))

    end do ! do i = 1, size(var(1,1,1,:))

    ! Define local variables

    call netcdf_interface_close()

    !=====================================================================

  end subroutine putvar_real_4d

  !=======================================================================

end module netcdf_interface
