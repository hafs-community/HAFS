module netcdf_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tempdrop-sonde :: netcdf_interface
  ! Copyright (C) 2017 Henry R. Winterbottom

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
  public :: netcdf_interface_open
  public :: netcdf_interface_writedef

  ! Define local variables

  integer                                                               :: ncdimid
  integer                                                               :: ncfileid
  integer                                                               :: ncstatus
  integer                                                               :: ncvarid

  !-----------------------------------------------------------------------

contains

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

  ! netcdf_interface_write_def.f90

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

       if(trim(adjustl(varinfo%vartype(i))) .eq. 'float') then

          ! Define local variables

          ncstatus = nf90_def_var(ncfileid,                                 &
               & trim(adjustl(varinfo%varname(i))),nf90_float,              &
               & dimid(1:varinfo%varndims(i)),varinfo%varid(i))

       end if ! if(trim(adjustl(varinfo%vartype(i))) .eq. 'float')

       ! Check local variable and proceed accordingly

       if(trim(adjustl(varinfo%vartype(i))) .eq. 'double') then

          ! Define local variables

          ncstatus = nf90_def_var(ncfileid,                                 &
               & trim(adjustl(varinfo%varname(i))),nf90_double,             &
               & dimid(1:varinfo%varndims(i)),varinfo%varid(i))

       end if ! if(trim(adjustl(varinfo%vartype(i))) .eq. 'double')

       ! Loop through local variable

       do j = 1, varinfo%varnattrs(i)

          ! Define local variables

          ncstatus = nf90_put_att(ncfileid,varinfo%varid(i),                &
               & trim(adjustl(varinfo%varattrs(i,j,1))),                    &
               & trim(adjustl(varinfo%varattrs(i,j,2))))

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

       ncstatus = nf90_create(trim(adjustl(filename)),nf90_clobber,        &
            & ncfileid)

    end if ! if(is_write)
    
    !=====================================================================

  end subroutine netcdf_interface_open

  !=======================================================================

end module netcdf_interface
