      subroutine extrot_p(work,n,m,io,jo,array,no,mo, scale, wrot)
      implicit none
c
      integer n,m,io,jo,no,mo
      real    work(m,n),array(no,mo),scale,wrot(n,m)
c
c --- rotate work into wrot and then
c --- array = scale*wrot(io:io+no-1,jo:jo+mo-1)
c
c --- for the p-grid of global domains with arctic bi-polar patch.
c --- it will also work for closed and near-global domains.
c
      integer i,j
c
      do j= 1,m-1
        do i= 1,n-1
          wrot(i,j) = scale*work(m-j,i)
        enddo
        wrot(n,j) = 0.0
      enddo
      do i= 1,n
        wrot(i,m) = 0.0
      enddo
c
      call extrct_p(wrot,n,m,io,jo,array,no,mo)
c
      return
      end

      subroutine extrot_q(work,n,m,io,jo,array,no,mo, scale, wrot)
      implicit none
c
      integer n,m,io,jo,no,mo
      real    work(m,n),array(no,mo),scale,wrot(n,m)
c
c --- rotate work into wrot and then
c --- array = scale*wrot(io:io+no-1,jo:jo+mo-1)
c
c --- for the q-grid of global domains with arctic bi-polar patch.
c --- it will also work for closed and near-global domains.
c
      integer i,j
c
      do j= 1,m-1
        do i= 1,n-1
          wrot(i,j) = scale*work(m-j,i)
        enddo
        wrot(n,j) = 0.0
      enddo
      do i= 1,n
        wrot(i,m) = 0.0
      enddo
c
      call extrct_q(wrot,n,m,io,jo,array,no,mo)
c
      return
      end

      subroutine extrot_u(work,n,m,io,jo,array,no,mo, scale, wrot)
      implicit none
c
      integer n,m,io,jo,no,mo
      real    work(m,n),array(no,mo),scale,wrot(n,m)
c
c --- rotate work into wrot and then
c --- array = scale*wrot(io:io+no-1,jo:jo+mo-1)
c
c --- for the u-grid of global domains with arctic bi-polar patch.
c --- it will also work for closed and near-global domains.
c
      integer i,j
c
      do j= 1,m-1
        do i= 1,n-1
          wrot(i,j) = scale*work(m-j,i)
        enddo
        wrot(n,j) = 0.0
      enddo
      do i= 1,n
        wrot(i,m) = 0.0
      enddo
c
      call extrct_u(wrot,n,m,io,jo,array,no,mo)
c
      return
      end

      subroutine extrot_v(work,n,m,io,jo,array,no,mo, scale, wrot)
      implicit none
c
      integer n,m,io,jo,no,mo
      real    work(m,n),array(no,mo),scale,wrot(n,m)
c
c --- rotate work into wrot and then
c --- array = scale*wrot(io:io+no-1,jo:jo+mo-1)
c
c --- for the v-grid of global domains with arctic bi-polar patch.
c --- it will also work for closed and near-global domains.
c
      integer i,j
c
      do j= 1,m-1
        do i= 1,n-1
          wrot(i,j) = scale*work(m-j,i)
        enddo
        wrot(n,j) = 0.0
      enddo
      do i= 1,n
        wrot(i,m) = 0.0
      enddo
c
      call extrct_v(wrot,n,m,io,jo,array,no,mo)
c
      return
      end

