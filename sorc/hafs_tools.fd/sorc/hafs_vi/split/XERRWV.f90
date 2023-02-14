subroutine xerrwv ( messg, nmessg, nerr, level, ni, i1, i2, nr, r1, r2 )

!*****************************************************************************80
!
!! XERRWV processes an error message that includes numeric information.
!
!  Discussion:
!
!    This routine processes a diagnostic message, in a manner determined
!    by the value of LEVEL and the current value of the library error
!    control flag KONTRL.
!
!    See XSETF for details about KONTRL.
!
!    In addition, up to two integer values and two real values may be
!    printed along with the message.
!
!  Example:
!
!    call xerrwv ( 'SMOOTH -- NUM (=I1) was zero.', 29, 1, 2, 1, num,
!      0, 0, 0.0, 0.0 )
!
!    call xerrwv ( &
!      'QUADXY -- Requested error (R1) less than minimum(R2).', &
!      54, 77, 1, 0, 0, 0, 2, errreq, errmin )
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
!    Input, character ( len = * ) MESSG, the message to be processed.
!
!    Input, integer ( kind = 4 ) NMESSG, the number of characters in
!    MESSG.
!
!    Input, integer ( kind = 4 ) NERR, the error number associated with
!    this message.  NERR must not be zero.
!
!    Input, integer ( kind = 4 ) LEVEL, the error category.
!    * 2, this is an unconditionally fatal error.
!    * 1, this is a recoverable error.  It is normally non-fatal, unless
!         KONTRL has been reset by XSETF.
!    * 0, this is a warning message only.
!    *-1, this is a warning message which is to be printed at most once,
!         regardless of how many times this call is executed.
!
!    Input, integer ( kind = 4 ) NI, the number of integer values to be
!    printed. (0 to 2)
!
!    Input, integer ( kind = 4 ) I1, I2, the first and second integer
!    values.
!
!    Input, integer ( kind = 4 ) NR, the number of real values to be
!    printed.
!    (0 to 2)
!
!    Input, real ( kind = 8 ) R1, R2, the first and second real values.
!
  implicit none

  character ( len = 37 ) form
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i1mach
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) ifatal
  integer ( kind = 4 ) isizei
  integer ( kind = 4 ) isizef
  integer ( kind = 4 ) iunit
  integer ( kind = 4 ) j4save
  integer ( kind = 4 ) junk
  integer ( kind = 4 ) kdummy
  integer ( kind = 4 ) kount
  integer ( kind = 4 ) kunit
  integer ( kind = 4 ) lerr
  integer ( kind = 4 ) level
  integer ( kind = 4 ) lkntrl
  integer ( kind = 4 ) llevel
  integer ( kind = 4 ) lmessg
  integer ( kind = 4 ) lun(5)
  integer ( kind = 4 ) maxmes
  character ( len = * ) messg
  integer ( kind = 4 ) mkntrl
  integer ( kind = 4 ) nerr
  integer ( kind = 4 ) ni
  integer ( kind = 4 ) nmessg
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) nunit
  real ( kind = 8 ) r1
  real ( kind = 8 ) r2
  logical set
  integer ( kind = 4 ) value
  integer ( kind = 4 ) which
!
!  Get flags
!
  which = 2
  value = 0
  set = .false.

  lkntrl = j4save ( which, value, set )

  which = 4
  value = 0
  set = .false.

  maxmes = j4save ( which, value, set )
!
!  Check for valid input
!
  if ( nmessg <= 0 .or. &
       nerr == 0 .or.   &
       level < -1 .or. &
       2 < level ) then

    if ( 0 < lkntrl ) then
      call xerprt ( 'Fatal error in...', 17 )
    end if

    call xerprt( 'XERROR -- Invalid input', 23 )

    if ( 0 < lkntrl ) then
      call xerprt ( 'Job abort due to fatal error.', 29 )
    end if

    if ( 0 < lkntrl ) then
      call xersav ( ' ', 0, 0, 0, kdummy )
    end if

    call xerabt ( 'XERROR -- invalid input', 23 )
    return

  end if
!
!  Record the message.
!
  which = 1
  value = nerr
  set = .true.

  junk = j4save ( which, value, set )

  call xersav ( messg, nmessg, nerr, level, kount )
!
!  Let the user override.
!
  lmessg = nmessg
  lerr = nerr
  llevel = level

  call xerctl ( messg, lmessg, lerr, llevel, lkntrl )
!
!  Reset to original values.
!
  lmessg = nmessg
  lerr = nerr
  llevel = level

  lkntrl = max ( -2, min ( 2, lkntrl ) )
  mkntrl = abs ( lkntrl )
!
!  Decide whether to print message
!
  if ( .NOT. (llevel < 2 .and. lkntrl == 0 ) ) then
!  end if

  elseif ( .NOT. ( ( llevel == -1 .and. min ( 1, maxmes ) < kount ) .or. &
         ( llevel == 0 .and. maxmes < kount ) .or. &
         ( llevel == 1 .and. maxmes < kount .and. mkntrl == 1 ) .or. &
         ( llevel == 2  .and. max ( 1, maxmes ) < kount ) ) ) then
!    end if

    if ( 0 < lkntrl ) then

      call xerprt ( ' ', 1 )

      if ( llevel == -1 ) then
        call xerprt &
        ( 'Warning message...this message will only be printed once.',57)
      else if ( llevel == 0 ) then
        call xerprt ( 'Warning in...', 13 )
      else if ( llevel == 1 ) then
        call xerprt ( 'Recoverable error in...', 23 )
      else if ( llevel == 2 ) then
        call xerprt ( 'Fatal error in...', 17 )
      end if

    end if
!
!    Message.
!
    call xerprt ( messg, lmessg )
    call xgetua(lun,nunit)
    isizei = 1 + int ( log10 ( real ( i1mach(9), kind = 8 ) ) )
    isizef = 1 + int ( log10 ( real ( i1mach(10), kind = 8 )**i1mach(14) ))

    do kunit = 1, nunit

      iunit = lun(kunit)

      do i = 1, min ( ni, 2 )
        write (form,21) i,isizei
     21 format ('(11x,21hin above message, i',i1,'=,i',i2,')   ')
        if ( iunit == 0 ) then
          if ( i == 1 ) write (*,form) i1
          if ( i == 2 ) write (*,form) i2
        else
          if ( i == 1 ) write (iunit,form) i1
          if ( i == 2 ) write (iunit,form) i2
        end if
      end do

      do i = 1, min ( nr, 2 )
        write (form,23) i,isizef+10,isizef
     23 format ('(11x,21hin above message, r',i1,'=,e',i2,'.',i2,')')
        if ( iunit == 0 ) then
          if ( i == 1 ) write (*,form) r1
          if ( i == 2 ) write (*,form) r2
        else
          if ( i == 1 ) write (iunit,form) r1
          if ( i == 2 ) write (iunit,form) r2
        end if
      end do
!
!    Print the error number.
!
      if ( 0 < lkntrl ) then

        if ( iunit == 0 ) then
          write ( *, '(a,i10)' ) '  Error number = ', lerr
        else
          write ( iunit, '(a,i10)' ) '  Error number = ', lerr
        end if

      end if

    end do
!
!    Traceback
!
    if ( 0 < lkntrl ) then
      call fdump ( )
    end if

  else

    if ( llevel == 2 .or. ( llevel == 1 .and. mkntrl ==2 ) ) then
      ifatal = 1
    else
      ifatal = 0
    end if
!
!    Quit here if message is not fatal.
!
    if ( ifatal <= 0 ) then
      return
    end if
!
!    Print reason for abort and error summary.
!
    if ( 0 < lkntrl .and. kount <= max ( 1, maxmes ) ) then

      if ( llevel == 1 ) then
        call xerprt ( 'Job abort due to unrecovered error.', 35 )
      end if

      if ( llevel == 2 ) then
        call xerprt ( 'Job abort due to fatal error.', 29 )
      end if

      call xersav ( ' ', -1, 0, 0, kdummy )

    end if
!
!    Abort
!
    if ( llevel == 2 .and. max ( 1, maxmes ) < kount ) then
      lmessg = 0
    end if

    call xerabt ( messg, lmessg )

    return
  endif
end subroutine xerrwv
