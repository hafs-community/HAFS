subroutine fill_nmm_gridg(gin,nx,ny,b,igtype)

!  convert input staggered grid to output filled grid

!   --> gin:   input staggered grid
!   --> nx,ny: input grid dimensions
!  <--  b:     output filled grid
!   --> igtype: =1, then (1,1) on staggered grid is at corner of grid
!               =2, then (1,1) is staggered

implicit none

integer nx,ny,igtype
real(4) gin(nx,ny),b(2*nx-1,ny)

integer i,im,ip,j,jm,jp
real(4) fill,test

fill=.95*huge(fill) ; test=.95*fill
do j=1,ny
 do i=1,2*nx-1
  b(i,j)=fill
 end do
end do

!  first transfer all staggered points to appropriate
!   points on filled output grid

if(igtype.eq.1) then
 do j=1,ny,2
  do i=1,nx
   b(2*i-1,j)=gin(i,j)
  end do
 end do
 do j=2,ny,2
  do i=1,nx-1
   b(2*i,j)=gin(i,j)
  end do
 end do
else
 do j=1,ny,2
  do i=1,nx-1
   b(2*i,j)=gin(i,j)
  end do
 end do
 do j=2,ny,2
  do i=1,nx
   b(2*i-1,j)=gin(i,j)
  end do
 end do
end if

!  now fill in holes

! top and bottom rows:

do j=1,ny,ny-1
 do i=1,2*nx-1
  if(b(i,j).gt.test) then
   ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
   im=i-1 ; if(im.lt.1) im=i+1
   b(i,j)=.5*(b(im,j)+b(ip,j))
  end if
 end do
end do

! left and right rows:

do j=1,ny
 jp=j+1 ; if(jp.gt.ny) jp=j-1
 jm=j-1 ; if(jm.lt.1) jm=j+1
 do i=1,2*nx-1,2*nx-2
  if(b(i,j).gt.test) b(i,j)=.5*(b(i,jm)+b(i,jp))
 end do
end do

! interior points

do j=1,ny
 jp=j+1 ; if(jp.gt.ny) jp=j-1
 jm=j-1 ; if(jm.lt.1) jm=j+1
 do i=1,2*nx-1
  if(b(i,j).gt.test) then
   ip=i+1 ; if(ip.gt.2*nx-1) ip=i-1
   im=i-1 ; if(im.lt.1) im=i+1
   b(i,j)=.25*(b(ip,j)+b(im,j)+b(i,jp)+b(i,jm))
  end if
 end do
end do

end subroutine fill_nmm_gridg

