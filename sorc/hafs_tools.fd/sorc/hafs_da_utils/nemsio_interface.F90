module nemsio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: nemsio_interface
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

  use constants_interface
  use gridprojs_interface
  use kinds_interface
  use nemsio_module
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: nemsio_interface_init
  public :: nemsio_interface_read

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
    integer                                                             :: ncoords
    
    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    grid%ntrunc = nemsio%jcap

    ! Check local variable and proceed accordingly

    if(grid%ntrunc .gt. 0) then
    
       ! Compute local variables
       
       call specgrids_compute(grid)

       ! Define local variables

       nemsio%lat = grid%lats
       nemsio%lon = grid%lons

       ! Deallocate memory for local variables

       call specgrids_cleanup(grid)

    end if ! if(grid%ntrunc .gt. 0)

    !=====================================================================

  end subroutine compute_grid
  
  !=======================================================================

  ! SUBROUTINE:

  ! compute_hybprs.f90

  ! DESCRIPTION:

  ! This subroutine computes the pressure levels described by the
  ! surface pressure and the NEMS vertical coordinate; the interface
  ! pressure levels (i.e., the pressure defined at the vertical
  ! coordinate) and the mid-layer pressure (i.e., the pressure level
  ! at which the NEMS profile variables are defined).

  ! INPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing (at miniumum)
  !   the surface pressure.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the computed
  !   interface pressure values, the mid-layer pressure levels (e.g.,
  !   the infcprs and mdlyprs, respectively), and the vertical
  !   coordinate values (ak, bk, and ck respectively).

  !-----------------------------------------------------------------------

  subroutine compute_hybprs(nemsio)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nemsio

    ! Define variables computed within routine

    real(nemsio_realkind),      dimension(:,:,:),           allocatable :: vcoord

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(vcoord)) allocate(vcoord((nemsio%nz + 1),3,2))
    
    ! Define local variables
    
    call nemsio_getfilehead(gfile,iret=iret,vcoord=vcoord)
    nemsio%ak = vcoord(:,1,1)
    nemsio%bk = vcoord(:,2,1)    
    nemsio%ck = vcoord(:,3,1)

    ! Loop through local variable

    do i = 1, (nemsio%nz + 1)

       ! Compute local variables

       nemsio%infcprs(:,i) = nemsio%ak(i) + nemsio%bk(i)*nemsio%psfc
       
    end do ! do i = 1, (nemsio%nz + 1)

    ! Loop through local variable

    do i = 1, nemsio%nz

       ! Compute local variables

       nemsio%mdlyprs(:,i) = 0.5*(nemsio%infcprs(:,i) +                    &
            & nemsio%infcprs(:,i+1))
       
    end do ! do i = 1, nemsio%nz
       
    ! Deallocate memory for local variables

    if(allocated(vcoord)) deallocate(vcoord)

    !=====================================================================
    
  end subroutine compute_hybprs
  
  !=======================================================================

  ! SUBROUTINE:

  ! nemsio_interface_init.f90

  ! DESCRIPTION:

  ! This subroutine parses a user specified NEMS formatted file and
  ! reads the surface pressure and orography variables (psfc and orog,
  ! respectively) and computes the pressure interface level and
  ! mid-layer pressure values (infcprs and mdlyprs, respectively).

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
    call variable_interface_setup_struct(nemsio)
    call nemsio_getfilehead(gfile,iret=iret,lat=nemsio%lat,                &
         & lon=nemsio%lon)   
    call read_sfc(nemsio)

    ! Compute local variables

    call compute_grid(nemsio)
    call compute_hybprs(nemsio)
    
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

  ! * nemsio; a FORTRAN nems_struct variable containing the values
  !   retrieved from the NEMS formatted file.

  ! * varname; a FORTRAN nemsio_charkind variable containing the user
  !   specified variable NEMS variable to retrieve from the NEMS
  !   formatted file.

  ! * is_sfcvar; a FORTRAN logical variable specifying whether the
  !   NEMS variable is a 'sfc'-type variable.

  ! * is_prfvar; a FORTRAN logical variable specifying whether the
  !   NEMS variable is a 'mid layer'-type variable.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the values
  !   retrieved from the NEMS formatted file; if is_sfcvar is true,
  !   the retrieved variable is within the sfcvar attribute; if
  !   is_prfvar is true, the retrieved variable is within the prfvar
  !   attribute.

  !-----------------------------------------------------------------------

  subroutine nemsio_interface_read(filename,nemsio,varname,is_sfcvar,      &
       & is_prfvar)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename
    character(nemsio_charkind)                                          :: varname
    logical                                                             :: is_prfvar
    logical                                                             :: is_sfcvar

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(is_prfvar) call read_nemsio_prfvar(filename,nemsio,varname)
    if(is_sfcvar) call read_nemsio_sfcvar(filename,nemsio,varname)

    !=====================================================================

  end subroutine nemsio_interface_read

  !=======================================================================

  ! SUBROUTINE:

  ! read_nemsio_prfvar.f90

  ! DESCRIPTION:

  ! This subroutine parses a NEMS formatted file and returns the user
  ! specified NEMS mid-layer (profile variable) array.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! * varname; a FORTRAN nemsio_charkind variable containing the user
  !   specified variable NEMS 'mid-layer' variable to retrieve from
  !   the NEMS formatted file.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the values
  !   retrieved from the NEMS formatted file; if the NEMS API returns
  !   a value other than zero, the variable array within the FORTRAN
  !   nems_struct variable (prfvar) will contain values specified by
  !   the 'spval' attribute.

  !-----------------------------------------------------------------------

  subroutine read_nemsio_prfvar(filename,nemsio,varname)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename
    character(nemsio_charkind)                                          :: varname

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables
    
    call nemsio_init(iret=iret)
    call nemsio_open(gfile,trim(adjustl(filename)),'read',iret=iret)
    
    ! Loop through local variable

    do i = 1, nemsio%nz

       ! Define local variables

       call nemsio_readrecv(gfile,trim(adjustl(varname)),levtyp=           &
            & 'mid layer',lev=i,data=nemsio%prfvar(:,i),iret=iret)

       ! Check local variable and proceed accordingly

       if(iret .ne. 0) nemsio%prfvar(:,i) = spval
       
    end do ! do i = 1, nemsio%nz

    ! Define local variables
    
    call nemsio_close(gfile,iret=iret)

    !=====================================================================
    
  end subroutine read_nemsio_prfvar

  !=======================================================================

  ! SUBROUTINE:

  ! read_nemsio_sfcvar.f90

  ! DESCRIPTION:

  ! This subroutine parses a NEMS formatted file and returns the user
  ! specified NEMS surface variable array.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the NEMS formatted file.

  ! * nemsio; a FORTRAN nems_struct variable.

  ! * varname; a FORTRAN nemsio_charkind variable containing the user
  !   specified variable NEMS surface variable to retrieve from
  !   the NEMS formatted file.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the values
  !   retrieved from the NEMS formatted file; if the NEMS API returns
  !   a value other than zero, the variable array within the FORTRAN
  !   nems_struct variable (sfcvar) will contain values specified by
  !   the 'spval' attribute.

  !-----------------------------------------------------------------------

  subroutine read_nemsio_sfcvar(filename,nemsio,varname)

    ! Define variables passed to routine
    
    type(nems_struct)                                                   :: nemsio
    character(len=500)                                                  :: filename
    character(nemsio_charkind)                                          :: varname

    ! Define counting variables

    integer                                                             :: i
    
    !=====================================================================

    ! Define local variables
    
    call nemsio_init(iret=iret)
    call nemsio_open(gfile,trim(adjustl(filename)),'read',iret=iret)
    call nemsio_readrecv(gfile,trim(adjustl(varname)),levtyp='sfc',        &
         & lev=1,data=nemsio%sfcvar(:),iret=iret)

    ! Check local variable and proceed accordingly

    if(iret .ne. 0) nemsio%prfvar(:,i) = spval

    ! Define local variables
    
    call nemsio_close(gfile,iret=iret)

    !=====================================================================
    
  end subroutine read_nemsio_sfcvar

  !=======================================================================

  ! SUBROUTINE:

  ! read_sfc.f90

  ! DESCRIPTION:

  ! This subroutine parses the NEMS formatted file and returns the
  ! surface variable values for pressure (psfc; Pascals) and
  ! topography height (orog; meters).

  ! INPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing (at miniumum)
  !   the allocated/initialized variable arrays.

  ! OUTPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable containing the surface
  !   pressure (Pascals) and topography heights (meters) contained
  !   within the NEMS formatted files (psfc and orog, respectively).

  !-----------------------------------------------------------------------

  subroutine read_sfc(nemsio)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nemsio

    !=====================================================================

    ! Define local variables

    call nemsio_readrecv(gfile,'pres',levtyp='sfc',lev=1,                  &
         & data=nemsio%psfc,iret=iret)
    call nemsio_readrecv(gfile,'hgt',levtyp='sfc',lev=1,                   &
         & data=nemsio%orog,iret=iret)

    !=====================================================================

  end subroutine read_sfc

  !=======================================================================

end module nemsio_interface
