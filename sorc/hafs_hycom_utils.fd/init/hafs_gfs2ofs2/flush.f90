subroutine flush(iunit)
implicit none
integer iunit
!
! --- wrapper for flush system call under AIX.
!
integer*4 iunit4
!
iunit4=iunit
!dan call flush_(iunit4)
return
end subroutine flush
