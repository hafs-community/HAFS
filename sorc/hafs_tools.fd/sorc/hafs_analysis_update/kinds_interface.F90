module kinds_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! analysis-update :: kinds_interface
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

  use f95_precision
  
  ! Define interfaces and attributes for module routines

  implicit none
  private
  public :: default_integer
  public :: default_real
  public :: i_byte
  public :: i_kind
  public :: i_llong
  public :: i_long
  public :: i_short
  public :: num_bytes_for_i_byte
  public :: num_bytes_for_i_kind
  public :: num_bytes_for_i_llong
  public :: num_bytes_for_i_long  
  public :: num_bytes_for_i_short
  public :: num_bytes_for_r_double
  public :: num_bytes_for_r_quad
  public :: num_bytes_for_r_kind
  public :: num_bytes_for_r_single
  public :: r_double
  public :: r_kind
  public :: r_quad
  public :: r_single
  public :: dp_intel

  !-----------------------------------------------------------------------

  ! Define local variables  

  integer,                                                    parameter :: &
       & default_integer = 3
  integer,                                                    parameter :: &
       & default_real = 1
  integer,                                                    parameter :: &
       & llong_t = selected_int_kind(16)
  integer,                                                    parameter :: &
       & num_bytes_for_i_byte = 1
  integer,                                                    parameter :: &
       & num_bytes_for_i_short = 2
  integer,                                                    parameter :: &
       & num_bytes_for_i_long = 4
  integer,                                                    parameter :: &
       & num_bytes_for_i_llong = 8
  integer,                                                    parameter :: &
       & num_i_kinds = 4
  integer,            dimension(num_i_kinds),                 parameter :: &
       & integer_byte_sizes =                                              &
       & (/num_bytes_for_i_byte,num_bytes_for_i_short,                     &
       & num_bytes_for_i_long,num_bytes_for_i_llong/)
  integer,                                                    parameter :: &
       & num_bytes_for_i_kind = integer_byte_sizes(default_integer)
  integer,                                                    parameter :: &
       & i_byte = selected_int_kind(1)      
  integer,                                                    parameter :: &
       & i_short = selected_int_kind(4)      
  integer,                                                    parameter :: &
       & i_long = selected_int_kind(8)          
  integer,                                                    parameter :: &
       & i_llong = max(llong_t,i_long)
  integer,            dimension(num_i_kinds),                 parameter :: &
       & integer_types = (/i_byte,i_short,i_long,i_llong/)
  integer,                                                    parameter :: &
       & i_kind = integer_byte_sizes(default_integer)
  integer,                                                    parameter :: &
       & num_r_kinds = 3
  integer,                                                    parameter :: &
       & r_single = selected_real_kind(6)
  integer,                                                    parameter :: &
       & r_double = selected_real_kind(15)
  integer,                                                    parameter :: &
       & quad_t = selected_real_kind(20)
  integer,                                                    parameter :: &
       & r_quad = max(quad_t,r_double)
  integer,                                                    parameter :: &
       & num_bytes_for_r_single = 4
  integer,                                                    parameter :: &
       & num_bytes_for_r_double = 8
  integer,                                                    parameter :: &
       & num_bytes_for_r_quad = 16
  integer,            dimension(num_r_kinds),                 parameter :: &
       & real_kinds = (/r_single,r_double,r_quad/)
  integer,            dimension(num_r_kinds),                 parameter :: &
       & real_byte_sizes = (/num_bytes_for_r_single,                       &
       & num_bytes_for_r_double,num_bytes_for_r_quad/)
  integer,                                                    parameter :: &
       & r_kind = real_kinds(default_real)
  integer,                                                    parameter :: &
       & num_bytes_for_r_kind = real_byte_sizes(default_real)
  integer,                                                    parameter :: &
       & dp_intel = dp
  
  !=======================================================================

end module kinds_interface
