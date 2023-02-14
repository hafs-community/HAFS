  subroutine xersav ( messg, nmessg, nerr, level, count )

!*****************************************************************************80
!
!! XERSAV records that an error occurred.
!
!  Modified:
!
!    05 April 2007
!
!  Author:
!
!    Ron Jones
!
!  Reference:
!
!    Ron Jones, David Kahaner,
!    XERROR, The SLATEC Error Handling Package,
!    Technical Report SAND82-0800,
!    Sandia National Laboratories, 1982.
!
!    Ron Jones, David Kahaner,
!    XERROR, The SLATEC Error Handling Package,
!    Software: Practice and Experience,
!    Volume 13, Number 3, 1983, pages 251-257.
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1989,
!    ISBN: 0-13-627258-4,
!    LC: TA345.K34.
!
!  Parameters:
!
!    Input, character ( len = * ) MESSG, as in XERROR.
!
!    Input, integer ( kind = 4 ) NMESSG, as in XERROR, except that, when
!    NMESSG = 0, the tables will be dumped and cleared; and when NMESSG
!    < 0,
!    the tables will be dumped, but not cleared.
!
!    Input, integer ( kind = 4 ) NERR, the error number.  NERR should
!    not be 0.
!
!    Input, integer ( kind = 4 ) LEVEL, the error severity level.
!    * 2, this is an unconditionally fatal error.
!    * 1, this is a recoverable error.  It is normally non-fatal, unless
!         KONTRL has been reset by XSETF.
!    * 0, this is a warning message only.
!    *-1, this is a warning message which is to be printed at most once,
!         regardless of how many times this call is executed.
!
!    Output, integer ( kind = 4 ) COUNT, the number of times this
!    message has
!    been seen, or zero if the table has overflowed and does not contain
!    this message specifically.
!    When NMESSG = 0, COUNT will not be altered.
!
  implicit none

  integer ( kind = 4 ) count
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1mach
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) iunit
  integer ( kind = 4 ), save, dimension ( 10 ) :: kount = (/ &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  integer ( kind = 4 ), save :: kountx = 0
  integer ( kind = 4 ) kunit
  integer ( kind = 4 ) level
  integer ( kind = 4 ), save, dimension ( 10 ) :: levtab = (/ &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  integer ( kind = 4 ) lun(5)
  character ( len = 20 ) mes
  character ( len = * ) messg
  character ( len = 20 ), save, dimension ( 10 ) :: mestab
  integer ( kind = 4 ) nerr
  integer ( kind = 4 ), save, dimension ( 10 ) :: nertab = (/ &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  integer ( kind = 4 ) nmessg
  integer ( kind = 4 ) nunit
!
!  Dump the table
!
  if ( nmessg <= 0 ) then

    if ( kount(1) == 0 ) then
      return
    end if
!
!  Print to each unit
!
    call xgetua ( lun, nunit )

    do kunit = 1, nunit

      iunit = lun(kunit)
      if ( iunit == 0 ) then
        iunit = i1mach(4)
      end if
!
!  Print table header
!
      write ( iunit, '(a)' ) ' '
      write ( iunit, '(a)' ) &
      '          Error message summary'
      write ( iunit, '(a)' ) &
      'Message start             NERR     Level     Count'
!
!  Print body of table.
!
      do i = 1, 10

        if ( kount(i) == 0 ) then
          exit
        end if

        write ( iunit, '(a20,3i10)' ) &
          mestab(i), nertab(i), levtab(i), kount(i)

      end do
!
!  Print number of other errors.
!
      if ( kountx /= 0 ) then
        write ( iunit, '(a)' ) ' '
        write ( iunit, '(a,i10)' ) &
          'Other errors not individually tabulated = ', kountx
      end if

      write ( iunit, '(a)' ) ' '

    end do

    if ( nmessg < 0 ) then
      return
    end if
!
!  Clear the error tables.
!
    kount(1:10) = 0
    kountx = 0
!
!  Process a message.
!
!  Search for this message, or else an empty slot for this message,
!  or else determine that the error table is full.
!
  else

    mes(1:20) = messg(1:20)

    do i = 1, 10

      ii = i
!
!  An empty slot was found for the new message.
!
      if ( kount(i) == 0 ) then
        mestab(ii) = mes
        nertab(ii) = nerr
        levtab(ii) = level
        kount(ii)  = 1
        count = 1
        return
      end if
!
!  Message found in table.
!
      if ( mes == mestab(i) .and.  &
           nerr == nertab(i) .and. &
           level == levtab(i) ) then
        kount(ii) = kount(ii) + 1
        count = kount(ii)
        return
      end if

    end do
!
!  The table is full.
!
    kountx = kountx + 1
    count = 1

  end if

  return
  end subroutine xersav
