      subroutine chk_endianc(mendian)
!----------------------------------------------------------------------
!$$$ documentation block
!
!  get_mendian:   to obtain machine endianness
!
!  programmer: J. Wang        date: Aug, 2012
!
!  Input:  
!    no input argument
!  OUTPUT: 
!    mendian: character(16) machine endianness
!
!----------------------------------------------------------------------
!
        implicit none
!
        character(16),intent(out)  :: mendian
!
!----------------------------------------------------------------------
        INTEGER,PARAMETER :: ASCII_0 = 48,ASCII_1 = 49,ASCII_2 = 50,    &
     &                     ASCII_3 = 51
        INTEGER(4)        :: I
        common// I
!
!***** code start
!     
        I = ASCII_0 + ASCII_1*256 + ASCII_2*(256**2) + ASCII_3*(256**3)
        call findendian(mendian)
!
!     ------------------------------------------------------------------
!
      end subroutine chk_endianc
!
!
!-----------------------------------------------------------------------
!
      subroutine findendian(mendian)
!
        implicit none
!---
        character(16),intent(out)        :: mendian
!
!--- local vars
        character               :: i*4
        common//  i
!     ------------------------------------------------------------------
        if(i .eq. '0123') then
          mendian='little_endian'
          return
        elseif (i .eq. '3210') then
          mendian='big_endian'
          return
        else
          mendian='mixed_endian'
          return
        endif
!
!     ------------------------------------------------------------------
!
      end subroutine findendian

