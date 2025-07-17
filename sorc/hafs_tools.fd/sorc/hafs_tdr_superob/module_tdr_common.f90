MODULE TDR_COMMON

implicit none

CONTAINS

SUBROUTINE check_iostat_open(subname, filename, iostat)
!
! Description:
!   Checks I/O status from open statement. If non-zero, write error message and stop.
!
! Input:
!   subname  - Name of subroutine containing open statement
!   filename - Name of file to open
!   iostat   - I/O status from open statement
!
implicit none

character(*), intent(in) :: subname, filename
integer, intent(in)      :: iostat

if (iostat .ne. 0) then
  write(6,*) '-- ERROR (',trim(subname),'): Could not open file ',trim(filename),', iostat=',iostat
  write(6,*) ' '
  write(6,*) 'Stopping...'
  write(6,*) ' '
  stop
endif

END SUBROUTINE check_iostat_open

!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE check_iostat_read(subname, filename, iostat, eoferror)
!
! Description:
!   Checks I/O status from read statement. If non-zero, write error message and stop.
!
! Input:
!   subname   - Name of subroutine containing read statement
!   filename  - Name of file to read
!   iostat    - I/O status from read statement
!   eoferrror - Logical for whether or not EOF is an error condition
!
implicit none

character(*), intent(in)  :: subname, filename
integer, intent(in)       :: iostat
logical, intent(in)       :: eoferror ! If .true., end-of-file condition is an error condition

if (iostat .ne. 0) then
  if (iostat .lt. 0) then
    if (eoferror .eq. .true.) then
      write(6,*) '-- ERROR (',trim(subname),'): Unexpected EOF reached reading file ',trim(filename),', iostat=',iostat
      write(6,*) ' '
      write(6,*) 'Stopping...'
      write(6,*) ' '
      stop
    else
      write(6,*) '-- Info (',trim(subname),'): Reached EOF reading file ',trim(filename)
      write(6,*) ' '
    endif
  else
    write(6,*) '-- ERROR (',trim(subname),'): Could not read file ',trim(filename),', iostat=',iostat
    write(6,*) ' '
    write(6,*) 'Stopping...'
    write(6,*) ' '
    stop
  endif
endif

END SUBROUTINE check_iostat_read

!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE check_iostat_write(subname, filename, iostat)
!
! Description:
!   Checks I/O status from write statement. If non-zero, write error message and stop.
!
! Input:
!   subname  - Name of subroutine containing write statement
!   filename - Name of file being written to
!   iostat   - I/O status from write statement
!
implicit none

character(*), intent(in) :: subname, filename
integer, intent(in)      :: iostat

if (iostat .ne. 0) then
  write(6,*) '-- ERROR (',trim(subname),'): Could not write to file ',trim(filename),', iostat=',iostat
  write(6,*) ' '
  write(6,*) 'Stopping...'
  write(6,*) ' '
  stop
endif

END SUBROUTINE check_iostat_write

!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE check_file_missing(subname, filename)
!
! Description:
!   Checks whether file is missing. If so, write error message and stop.
!
! Input:
!   subname  - Name of subroutine where call occurs
!   filename - Name of file to check
!
implicit none

character(*), intent(in) :: subname, filename
logical                  :: fileexists

inquire(file=filename, exist=fileexists)
if (.not. fileexists) then
  write(6,*) '-- ERROR (',trim(subname),'): Could not find  ',trim(filename),'. Stopping...'
  write(6,*) ' '
  write(6,*) 'Stopping...'
  write(6,*) ' '
  stop
endif

END SUBROUTINE check_file_missing

!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE check_delete_existing(subname, filename)
!
! Description:
!   Deletes existing file
!
! Input:
!   subname  - Name of subroutine where call occurs
!   filename - Name of file to check
!   lu       - Unit of file to check
!
implicit none

character(*), intent(in) :: subname, filename
integer                  :: lu
integer                  :: iost
logical                  :: fileexists

lu = 1

inquire(file=filename, exist=fileexists)
if (fileexists) then
  write(6,*) '-- Info (',trim(subname),'): Existing file ',trim(filename),' found. Deleting...'
  open(unit=lu, file=filename, status='old', iostat=iost)
  if (iost .eq. 0) close(unit=lu, status='delete')
endif

END SUBROUTINE


END MODULE TDR_COMMON
