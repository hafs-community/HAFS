! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module final_module

implicit none

integer :: final_counted = 0
integer :: destroy_counted = 0

TYPE :: AnimalType
  character(len=20), private :: m_kind = "unidentified"
  logical :: constructed = .false.
contains
  procedure :: speak
  final :: AnimalType__dtor
ENDTYPE

interface AnimalType
  module procedure AnimalType__ctor
end interface

interface assignment(=)
  module procedure AnimalType__assignment
end interface

contains

subroutine speak(self)
  class(AnimalType), intent(in) :: self
  write(0,'(2A)') "I am a ",self%m_kind
end subroutine

subroutine AnimalType__dtor(self)
  type(AnimalType), intent(inout) :: self

  write(0,'(2A)') "Final animal ",self%m_kind
  final_counted = final_counted + 1

  ! Destruction guard needed for portability
  if( self%constructed ) then
    write(0,'(2A)') "    Destroy animal ",self%m_kind
    destroy_counted = destroy_counted + 1
  endif
end subroutine

function AnimalType__ctor(animaltype_) result(self)
  type(AnimalType) :: self
  character(len=*) :: animaltype_
  self%m_kind = animaltype_
  write(0,'(3A,I0)') "Constructing animal ",self%m_kind, " -- address = ",loc(self)
  self%constructed = .true.
end function

subroutine AnimalType__assignment(animal_out,animal_in)
  type(AnimalType), intent(out) :: animal_out
  class(AnimalType), intent(in) :: animal_in
  write(0,'(3A,I0,A,I0)') '   Copying ',animal_in%m_kind, " -- from address ", loc(animal_in), " to address ", loc(animal_out)
  animal_out%m_kind = animal_in%m_kind
  animal_out%constructed = animal_in%constructed
end subroutine

end module

! ------------------------------------------------------------------------

subroutine scope_test
use final_module
implicit none

  type(AnimalType) :: dog
  type(AnimalType) :: cat

  dog = AnimalType("dog")  ! Cray       : final called on temporary AnimalType("dog"); missing final call on dog before assignment
                           ! Intel      : final called on dog before assignment; and on temporary AnimalType("dog")
                           ! PGI 14.4   : final NOT called at all, possibly compiler bug
                           ! GNU 4.9    : final called on dog before assignment; missing call on temporary AnimalType("dog")
  call dog%speak()

  ! final called on dog when out of scope
end subroutine

! -------------------------------------------------------

subroutine assignment_test
use final_module
implicit none

  type(AnimalType) :: dog
  type(AnimalType) :: animal

  dog = AnimalType("dog")    ! final called on dog before assignment
  call dog%speak()
  write(0,'(A)') "-- animal = dog"
  animal = dog               ! final called on animal before assignment
  call animal%speak()

  ! final called on dog when out of scope
  ! final called on animal when out of scope
end subroutine

! -------------------------------------------------------

program test_final
use final_module
implicit none
  logical :: test_failed = .false.

  final_counted = 0
  destroy_counted = 0

  write(0,'(A)') " "
  write(0,'(A)') ">>>>>> begin scope_test"
  call scope_test
  write(0,'(A)') "<<<<<< end scope_test"
  write(0,'(A)') " "

  write(0,'(A,I0)') "final_counted = ", final_counted
  write(0,'(A,I0)') "destroy_counted = ", destroy_counted

  if( destroy_counted < 1 ) then
    test_failed = .true.
    write(0,'(A)') "ASSERTION FAILED: destroy_counted < 1"
  endif

  final_counted = 0
  destroy_counted = 0

  write(0,'(A)') " "
  write(0,'(A)') ">>>>>> begin assignment_test"
  call assignment_test
  write(0,'(A)') "<<<<<< end assignment_test"
  write(0,'(A)') " "

  write(0,'(A,I0)') "final_counted = ", final_counted
  write(0,'(A,I0)') "destroy_counted = ", destroy_counted

  if( destroy_counted < 2 ) then
    test_failed = .true.
    write(0,*) "ASSERTION FAILED: destroy_counted < 2"
  endif
  if( test_failed ) STOP 1

end program
