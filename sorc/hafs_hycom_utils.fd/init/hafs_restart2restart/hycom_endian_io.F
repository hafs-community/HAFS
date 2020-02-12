#if defined(ENDIAN_IO)
      subroutine endian_swap(a,n)
      implicit none
c
      integer,         intent(in)    :: n
      integer(kind=4), intent(inout) :: a(n)  ! 4-bytes
c
c**********
c*
c 1)  swap the endian-ness of the array.
c
c 2)  assumes integer(kind=1) and integer(kind=4) ocupy one and four
c     bytes respectively.
c*
c**********
c
      integer         k
      integer(kind=4) ii4,   io4     ! 4-bytes
      integer(kind=1) ii1(4),io1(4)  ! 1-byte
      equivalence    (ii4,ii1(1)), (io4,io1(1))  ! non-standard f90
c
      do k= 1,n
        ii4 = a(k)
        io1(1) = ii1(4)
        io1(2) = ii1(3)
        io1(3) = ii1(2)
        io1(4) = ii1(1)
        a(k) = io4
      enddo
      return
      end subroutine endian_swap
#else
      subroutine endian_swap(a,n)
      implicit none
c
      integer,         intent(in)    :: n
      integer(kind=4), intent(inout) :: a(n)  ! 4-bytes
c
c     dummy version, do nothing.
c
      return
      end subroutine endian_swap
#endif /* ENDIAN_IO:else */
