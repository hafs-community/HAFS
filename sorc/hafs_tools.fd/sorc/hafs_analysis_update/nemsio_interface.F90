module nemsio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: nemsio_interface
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
  ! along with this program. If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use constants_interface
  use gridprojs_interface
  use kinds_interface
  use namelist_interface
  use nemsio_module
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: nemsio_interface_date_read
  public :: nemsio_interface_date_write
  public :: nemsio_interface_init
  public :: nemsio_interface_read
  public :: nemsio_interface_write

  ! Define local variables

  type(nemsio_gfile)                                                    :: gfile
  integer(nemsio_intkind)                                               :: iret

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! compute_grid.f90

  ! DESCRIPTION:

  ! This subroutine computes the geographical Gaussian grid
  ! described/defined by the NEMS spectral truncation.

  ! INPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing (at minimum)
  !   the NEMS spectral truncation.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the
  !   geographical Gaussian grid described/defined by the NEMS
  !   spectral truncation.

  !-----------------------------------------------------------------------

  subroutine compute_grid(nemsio)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nemsio

    ! Define variables computed within routine

    type(specgrids_grid)                                                :: grid

    !=====================================================================

    ! Check local variable

    if(nemsio%jcap .gt. 0) then
    
       ! Define local variables

       grid%ntrunc = nemsio%jcap

    else   ! if(nemsio%jcap .gt. 0)

       ! Define local variables

       grid%ntrunc = nint(nemsio%nx/2.0)

    end if ! if(nemsio%jcap .gt. 0)

    ! Define local variables

    if(debug) write(6,500) grid%ntrunc
    
    ! Compute local variables

    call specgrids_compute(grid)

    ! Define local variables

    if(debug) write(6,501) grid%nlons, grid%nlats
    nemsio%lat = grid%lats
    nemsio%lon = grid%lons

    ! Deallocate memory for local variables

    call specgrids_cleanup(grid)

    ! Define local variables

500 format('COMPUTE_GRID: Computing spectral transform grid of ',          &
         & 'trunction T',i4,'.')
501 format('COMPUTE_GRID: Spectral grid dimensions (nx,ny) = ',i4,1x,i4,   &
         & '.')

    !=====================================================================

  end subroutine compute_grid

  !=======================================================================

  ! SUBROUTINE:
  
  ! nemsio_interface_date_read.f90

  ! DESCRIPTION:

  ! This subroutine is the interface layer to the NEMS header and
  ! collects the idate variable array values.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the attributes
  !   of the idate variable array.

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_date_read(filename,nemsio)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call nemsio_open(gfile,trim(adjustl(filename)),'read',iret=iret)
    call nemsio_getfilehead(gfile,iret=iret,idate=nemsio%idate)
    call nemsio_close(gfile,iret=iret)

    !=====================================================================

  end subroutine nemsio_interface_date_read

  !=======================================================================

  ! SUBROUTINE:
  
  ! nemsio_interface_date_write.f90

  ! DESCRIPTION:

  ! This subroutine is the interface layer to the NEMS header and
  ! writes the idate variable array values to the NEMS-formatted file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_date_write(filename,nemsio)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename

    ! Define variables computed within routine

    integer(nemsio_intkind)                                             :: idate(7)

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Loop through local variable

    do i = 1, size(idate)

       ! Define local variables

       idate(i) = nemsio%idate(i)

    end do ! do i = 1, size(idate)

    ! Define local variables
    
    call nemsio_open(gfile,trim(adjustl(filename)),'rdwr',iret=iret)
    call nemsio_setheadvar(gfile,varname='idate',varval=idate,iret=iret)
    call nemsio_close(gfile,iret=iret)

    !=====================================================================

  end subroutine nemsio_interface_date_write
  
  !=======================================================================

  ! SUBROUTINE:

  ! nemsio_interface_init.f90

  ! DESCRIPTION:

  ! This subroutine parses a user specified NEMS formatted file header
  ! and returns the header attributes describing the grid.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the values
  !   retrieved from the NEMS formatted file.

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_init(filename,nemsio)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename
  
    !=====================================================================
    
    ! Define local variables
    
    call nemsio_init(iret=iret)
    call nemsio_open(gfile,trim(adjustl(filename)),'read',iret=iret)
    call nemsio_getfilehead(gfile,iret=iret,dimx=nemsio%nx,dimy=           &
         & nemsio%ny,dimz=nemsio%nz,jcap=nemsio%jcap,idate=nemsio%idate)

    ! Check local variable and proceed accordingly

    if(nemsio%jcap .le. 0) then

       ! Define local variables

       nemsio%jcap = ensgen_ntrunc

    end if ! if(nemsio%jcap .le. 0)

    ! Define local variables
    
    call variable_interface_setup_struct(nemsio)

    ! Compute local variables

    call compute_grid(nemsio)
    
    ! Define local variables
    
    call nemsio_close(gfile,iret=iret)

    !=====================================================================

  end subroutine nemsio_interface_init

  !=======================================================================

  ! SUBROUTINE:
  
  ! nemsio_interface_read.f90

  ! DESCRIPTION:

  ! This subroutine is the interface layer to read different NEMS
  ! variable types from an external NEMS-formatted file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable containing (at mininum)
  !   the NEMS formatted file variable name.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the values
  !   retrieved from the NEMS formatted file.

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_read(filename,nemsio)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call nemsio_open(gfile,trim(adjustl(filename)),'read',iret=iret)
    call nemsio_readrecv(gfile,trim(adjustl(nemsio%varname)),levtyp=       &
         & trim(adjustl(nemsio%levtype)),lev=nemsio%lev,data=nemsio%var,   &
         & iret=iret)
    call nemsio_close(gfile,iret=iret)

    !=====================================================================

  end subroutine nemsio_interface_read
  
  !=======================================================================

  ! SUBROUTINE:
  
  ! nemsio_interface_write.f90

  ! DESCRIPTION:

  ! This subroutine is the interface layer to write NEMS variable
  ! types to an external NEMS-formatted file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable containing (at mininum)
  !   the NEMS formatted file name and the variable attributes to be
  !   written.

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_write(filename,nemsio)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call nemsio_open(gfile,trim(adjustl(filename)),'rdwr',iret=iret)
    call nemsio_writerecv(gfile,trim(adjustl(nemsio%varname)),levtyp=      &
         & trim(adjustl(nemsio%levtype)),lev=nemsio%lev,data=nemsio%var,   &
         & iret=iret)
    call nemsio_close(gfile,iret=iret)

    !=====================================================================

  end subroutine nemsio_interface_write

  !=======================================================================

end module nemsio_interface
