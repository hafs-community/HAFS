module math_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! obs-to-bufr :: math_methods_interface
  ! Copyright (C) 2018 Henry R. Winterbottom

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

  use constants_interface
  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: math_methods_sort_array
  public :: math_methods_spline_interp

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_remove_duplicates.f90

  ! DESCRIPTION:

  ! This subroutine ingests a FORTRAN spline_struct variable and
  ! removes duplicate values from the independent variable array.

  ! INPUT VARIABLES:

  ! * grid_in; a FORTRAN spline_struct variable containing possible
  !   duplicate values within the independent variable array, xa.

  ! * grid_out; a FORTRAN spline_struct variable to contain the
  !   ingested spline_struct variable grid_in, but devoid of duplicate
  !   values.

  ! OUTPUT VARIABLES:

  ! * grid_out; a FORTRAN spline_struct variable, initialized as
  !   grid_in but no longer containing duplicate values within the
  !   independent variable array, xa.

  !-----------------------------------------------------------------------

  subroutine math_methods_remove_duplicates(grid_in,grid_out)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid_in
    type(spline_struct)                                                 :: grid_out

    ! Define variables computed within routine

    logical,                    dimension(:),               allocatable :: mask
    integer                                                             :: num

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(mask)) allocate(mask(grid_in%n))

    ! Define local variables

    mask = .false.

    ! Loop through local variable

    do i = 1, grid_in%n

       ! Define local variables

       num = count(grid_in%xa(i)==grid_in%xa)

       ! Check local variable and proceed accordingly

       if(num == 1) then

          ! Define local variables

          mask(i) = .true.

       else   ! if(num == 1)

          ! Check local variable and proceed accordingly

          if(.not. any(grid_in%xa(i)==grid_in%xa .and. mask)) then

             ! Define local variables

             mask(i) = .true.

          end if ! if(.not. any(grid_in%xa(i)==grid_in%xa .and. mask))

       end if ! if(num == 1)

    end do ! do i = 1, grid_in%n

    ! Define local variables

    grid_out%n  = count(mask)
    call variable_interface_setup_struct(grid_out)
    grid_out%xa = pack(grid_in%xa,mask)
    grid_out%ya = pack(grid_in%ya,mask)

    ! Deallocate memory for local variables

    if(allocated(mask)) deallocate(mask)

    !=====================================================================

  end subroutine math_methods_remove_duplicates

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_sort_array.f90

  ! DESCRIPTION:

  ! This subroutine implements the SLATEC ssort routine to sort a
  ! dependent array (ya) relative to a sorted independent array (xa);
  ! the arrays may be sorted in either the ascending or descending
  ! direction.

  ! REFERENCES:

  ! Singleton, R.C., 1969. Algorithm 347: an efficient algorithm for
  ! sorting with minimal storage [M1]. Communications of the ACM,
  ! 12(3), pp.185-186.

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the independent (xa)
  !   and dependent (ya) variable arrays to be sorted.

  ! * ascend; a FORTRAN logical variable specifying whether the arrays
  !   are to be sorted in the ascending direction.

  ! * descend; a FORTRAN logical variable specifying whether the
  !   arrays are to be sorted in the descending direction.

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the sorted independent
  !   (xa) and dependent (ya) variable arrays.

  !-----------------------------------------------------------------------

  subroutine math_methods_sort_array(grid,ascend,descend)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid
    logical                                                             :: ascend
    logical                                                             :: descend

    ! Define variables computed within routine

    real(r_kind),               dimension(:),               allocatable :: xa
    real(r_kind),               dimension(:),               allocatable :: ya
    integer                                                             :: kflag
    integer                                                             :: n

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(ascend)  kflag = 2
    if(descend) kflag = -2

    ! Define local variables

    n = grid%n

    ! Check local variable and proceed accordingly

    if(n .ge. 2) then

       ! Allocate memory for local variables
       
       if(.not. allocated(xa)) allocate(xa(n))
       if(.not. allocated(ya)) allocate(ya(n))

       ! Define local variables

       xa      = grid%xa
       ya      = grid%ya
       call ssort(xa,ya,n,kflag)
       grid%xa = xa
       grid%ya = ya

       ! Deallocate memory for local variables
       
       if(allocated(xa)) deallocate(xa)
       if(allocated(ya)) deallocate(ya)

    end if ! if(n .ge. 2)

    !=====================================================================

  end subroutine math_methods_sort_array

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_spline_interp.f90

  ! DESCRIPTION:

  ! This subroutine interpolates, using cubic splines, to find the
  ! value of a variable at a location specified by the user; this
  ! subroutine implements the sort2 subroutine of SLATEC and the
  ! spline_cubic_set and spline_cubic_val subroutines of SPLINE.

  ! REFERENCES:

  ! de Boor, C., 1978: A practical guide to splines. Springer-Verlag,
  ! 346pp.

  ! https://people.sc.fsu.edu/~jburkardt/f_src/spline/spline.html

  ! INPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the location (xa) and
  !   variable (ya) arrays as well as the interpolation location (x).

  ! OUTPUT VARIABLES:

  ! * grid; a FORTRAN spline_struct containing the interpolated value
  !   (y).

  !-----------------------------------------------------------------------

  subroutine math_methods_spline_interp(grid)

    ! Define variables passed to routine

    type(spline_struct)                                                 :: grid

    ! Define variables computed within routine

    type(spline_struct)                                                 :: gridl
    real(r_double),             dimension(:),               allocatable :: xa
    real(r_double),             dimension(:),               allocatable :: ya
    real(r_double),             dimension(:),               allocatable :: y2a
    real(r_double)                                                      :: yp
    real(r_double)                                                      :: ypp
    real(r_double)                                                      :: yp1
    real(r_double)                                                      :: ypn
    real(r_double)                                                      :: x
    real(r_double)                                                      :: y
    integer                                                             :: n

    !=====================================================================

    ! Define local variables

    call math_methods_remove_duplicates(grid,gridl)
    x = dble(grid%x)
    n = gridl%n

    ! Allocate memory for local variables

    if(.not. allocated(xa))  allocate(xa(n))
    if(.not. allocated(ya))  allocate(ya(n))
    if(.not. allocated(y2a)) allocate(y2a(n))

    ! Check local variable and proceed accordingly

    if(gridl%n .ge. 2) then

       ! Define local variables

       call math_methods_sort_array(gridl,.true.,.false.)
       yp1 = dble(0.0)
       ypn = dble(0.0)
       xa  = dble(gridl%xa)
       ya  = dble(gridl%ya)

       ! Compute local variables

       call spline_cubic_set(n,xa(1:n),ya(1:n),3,yp1,3,ypn,y2a)
       call spline_cubic_val(n,xa(1:n),ya(1:n),y2a(1:n),x,y,yp,ypp)

    end if ! if(gridl%n .ge. 2)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(gridl)
    if(allocated(xa))  deallocate(xa)
    if(allocated(ya))  deallocate(ya)
    if(allocated(y2a)) deallocate(y2a)

    ! Define local variables

    grid%y = real(y)

    !=====================================================================

  end subroutine math_methods_spline_interp

  !=======================================================================

end module math_methods_interface
