module math_methods_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK

  ! da-utils :: math_methods_interface
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
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface
  use variable_interface

  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: math_methods_stats

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! math_methods_stats.f90

  ! DESCRIPTION:

  ! This subroutine is the driver routine to compute statistical
  ! values from a distribution of a user specified variable.

  ! INPUT VARIABLES:

  ! * vargrid; a FORTRAN vargrid_struct variable containing the data
  !   values; may contain missing datum (e.g., spval values).

  ! * statgrid; a FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable now containing the
  !   statistical values computed from the user specified variable.

  !-----------------------------------------------------------------------

  subroutine math_methods_stats(vargrid,statgrid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid

    !=====================================================================

    ! Define local variables

    call stats_init(statgrid)

    ! Compute local variables

    call stats_mean(vargrid,statgrid)
    call stats_vari(vargrid,statgrid)
    call stats_rmse(vargrid,statgrid)

    !=====================================================================

  end subroutine math_methods_stats

  !=======================================================================

  ! SUBROUTINE: 

  ! stats_init.f90

  ! DESCRIPTION:

  ! This subroutine initializes a statgrid_struct variable.

  ! INPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; an initialized FORTRAN statgrid_struct variable.

  !-----------------------------------------------------------------------

  subroutine stats_init(statgrid)

    ! Define variables passed to routine

    type(statgrid_struct)                                               :: statgrid

    !=====================================================================

    ! Define local variables

    statgrid%bias   = spval
    statgrid%mean   = spval
    statgrid%rmse   = spval
    statgrid%stdev  = spval
    statgrid%vari   = spval
    statgrid%nvals  = 0

    !=====================================================================

  end subroutine stats_init

  !=======================================================================

  ! SUBROUTINE:

  ! stats_mean.f90

  ! DESCRIPTION:

  ! This subroutine computes the mean value from an array of
  ! non-missing data values.

  ! INPUT VARIABLES:

  ! * vargrid; a FORTRAN vargrid_struct variable containing the data
  !   values; may contain missing datum (e.g., spval values).

  ! * statgrid; a FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable containing the
  !   computed mean value and total number of valid data (mean and
  !   nvals attributes, accordingly).

  !-----------------------------------------------------------------------

  subroutine stats_mean(vargrid,statgrid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid

    ! Define variables computed within routine

    real(r_kind)                                                        :: sum
    integer                                                             :: nvals

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    sum   = 0.0
    nvals = 0

    ! Loop through local variable

    do i = 1, vargrid%nvals

       ! Check local variable and proceed accordingly

       if(vargrid%var(i) .ne. spval) then

          ! Compute local variables

          sum   = sum + vargrid%var(i)
          nvals = nvals + 1

       end if ! if(vargrid%var(i) .ne. spval)

    end do ! do i = 1, vargrid%nvals       

    ! Check local variable and proceed accordingly

    if(nvals .gt. 0) then

       ! Define local variables

       statgrid%nvals = nvals
       statgrid%mean  = sum/real(statgrid%nvals)

    end if ! if(nvals .gt. 0)

    !=====================================================================

  end subroutine stats_mean

  !=======================================================================

  ! SUBROUTINE:

  ! stats_rmse.f90

  ! DESCRIPTION:

  ! This subroutine computes the root mean-squared error from an array
  ! of non-missing data values.

  ! INPUT VARIABLES:

  ! * vargrid; a FORTRAN vargrid_struct variable containing the data
  !   values; may contain missing datum (e.g., spval values).

  ! * statgrid; a FORTRAN statgrid_struct variable.

  ! OUTPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable containing the root
  !   mean-squared error for the valid data (the rmse attribute).

  !-----------------------------------------------------------------------

  subroutine stats_rmse(vargrid,statgrid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid

    ! Define variables computed within routine

    real(r_kind)                                                        :: sum

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(statgrid%mean .eq. spval) call stats_mean(vargrid,statgrid)
    if(statgrid%mean .ne. spval) then

       ! Define local variables
       
       sum = 0.0
    
       ! Loop through local variable
    
       do i = 1, vargrid%nvals
       
          ! Check local variable and proceed accordingly
       
          if(vargrid%var(i) .ne. spval) then
             
             ! Compute local variables

             sum = sum + vargrid%var(i)*vargrid%var(i)

          end if ! if(vargrid%var(i) .ne. spval)

       end do ! do i = 1, vargrid%nvals

       ! Compute local variables

       statgrid%rmse = sqrt(sum/statgrid%nvals)

    end if ! if(statgrid%mean .ne. spval)

    !=====================================================================

  end subroutine stats_rmse

  !=======================================================================

  ! SUBROUTINE:

  ! stats_vari.f90

  ! DESCRIPTION:

  ! This subroutine computes the standard deviation and variance
  ! values from an array of non-missing data values.

  ! INPUT VARIABLES:

  ! * vargrid; a FORTRAN vargrid_struct variable containing the data
  !   values; may contain missing datum (e.g., spval values).

  ! * statgrid; a FORTRAN statgrid_struct variable; if the mean value
  !   is defined as the missing data value, this subroutine will
  !   attempt to compute the mean value.

  ! OUTPUT VARIABLES:

  ! * statgrid; a FORTRAN statgrid_struct variable containing the
  !   computed standard deviation and variance values using the valid
  !   data (stdev and vari attributes, accordingly).

  !-----------------------------------------------------------------------

  subroutine stats_vari(vargrid,statgrid)

    ! Define variables passed to routine

    type(vargrid_struct)                                                :: vargrid
    type(statgrid_struct)                                               :: statgrid

    ! Define variables computed within routine

    real(r_kind)                                                        :: sum
    real(r_kind)                                                        :: sumsq

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(statgrid%mean .eq. spval) call stats_mean(vargrid,statgrid)
    if(statgrid%mean .ne. spval) then

       ! Define local variables

       sum   = 0.0
       sumsq = 0.0
       
       ! Loop through local variable

       do i = 1, vargrid%nvals

          ! Check local variable and proceed accordingly

          if(vargrid%var(i) .ne. spval) then

             ! Compute local variables

             sum   = sum + (vargrid%var(i) - statgrid%mean)
             sumsq = sumsq + ((vargrid%var(i) - statgrid%mean)*           &
                  & (vargrid%var(i) - statgrid%mean))

          end if ! if(vargrid%var(i) .ne. spval)

       end do ! do i = 1, vargrid%nvals

       ! Check local variable and proceed accordingly

       if(statgrid%nvals .gt. 1) then

          ! Compute local variables

          statgrid%vari  = (sumsq - (sum*sum)/statgrid%nvals)/            &
               & (statgrid%nvals - 1)
          statgrid%stdev = sqrt(statgrid%vari)

       end if ! if(statgrid%nvals .gt. 1)

    end if ! if(statgrid%mean .ne. spval)

    !=====================================================================

  end subroutine stats_vari

  !=======================================================================

end module math_methods_interface
