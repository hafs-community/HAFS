! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

module sb_module
implicit none
integer :: a = 1

interface
  module subroutine sb
  end subroutine
end interface

contains
end module sb_module

! -------------------------------------------------------

submodule (sb_module) sb_submod1
implicit none
integer :: b = 2

contains

module subroutine sb()
  a = b
end subroutine

end submodule sb_submod1

! -------------------------------------------------------

program test_submodule
use sb_module
implicit none
write(0,*) a
call sb()
write(0,*) a
end program