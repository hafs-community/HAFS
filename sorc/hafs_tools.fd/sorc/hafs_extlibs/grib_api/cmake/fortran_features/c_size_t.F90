! (C) Copyright 2011- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation nor
! does it submit to any jurisdiction.

program test_c_sizeof
use, intrinsic :: iso_c_binding, only : c_size_t, c_int, c_long

write(0,*) "c_int    = ",c_int
write(0,*) "c_long   = ",c_long
write(0,*) "c_size_t = ",c_size_t

end program