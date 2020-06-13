! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module write_module

implicit none

TYPE :: AnimalType
  integer :: m_age
  integer :: m_paws
contains
  procedure :: writetype
  generic :: write(formatted) => writetype
ENDTYPE

contains

subroutine writetype(animal, unit, iotype, v_list, iostat, iomsg)
  ! Argument names here from the std, but you can name them differently.
  class(AnimalType), intent(in) :: animal ! Object to write.
  integer, intent(in) :: unit             ! Internal unit to write to.
  character(*), intent(in) :: iotype      ! LISTDIRECTED or DTxxx
  integer, intent(in) :: v_list(:)        ! parameters from fmt spec.
  integer, intent(out) :: iostat          ! non zero on error, etc.
  character(*), intent(inout) :: iomsg    ! define if iostat non zero.

  write (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) &
      "I am a dog"
end subroutine writetype

end module

! ------------------------------------------------------------------------

program test_write
use write_module
implicit none

  type(AnimalType) :: animal

  animal = AnimalType(8,4)

  write(0,'(A,DT)') 'Custom writing: ',animal

end program
