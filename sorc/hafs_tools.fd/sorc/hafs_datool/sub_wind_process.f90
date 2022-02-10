!========================================================================================
  subroutine cal_uv_coeff_fv3(nx, ny, grid_lat, grid_lon, cangu, sangu, cangv, sangv)
!-----------------------------------------------------------------------------
! This subroutine is adopated from gsi/mod_fv3_lola.f90
! Yonghui Weng, 20210319
!       --
!-----------------------------------------------------------------------------
  use constants
  implicit none

  integer, intent(in) :: nx, ny
  real,dimension(nx+1,ny+1), intent(in) :: grid_lat, grid_lon ! FV3 
!  real, allocatable, dimension(:,:), intent(out) :: cangu, sangu, cangv, sangv
  real, dimension(nx,ny+1), intent(out) :: cangu, sangu
  real, dimension(nx+1,ny), intent(out) :: cangv, sangv 

  real, allocatable, dimension(:,:)  :: x, y, z
  integer      :: i, j
  real         :: sq180, rlat, diff, rlon, xr, yr, zr, xu, yu, zu, uval, ewval, nsval, xv, yv, zv, vval 


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! find coefficients for wind conversion btw FV3 & earth
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  allocate  (   cangu(nx,ny+1),sangu(nx,ny+1),cangv(nx+1,ny),sangv(nx+1,ny) )

!   1.  compute x,y,z at cell cornor from grid_lon, grid_lat

  allocate( x(nx+1,ny+1), y(nx+1,ny+1), z(nx+1,ny+1) )
  do j=1,ny+1
     do i=1,nx+1
        x(i,j)=cos(grid_lat(i,j)*deg2rad)*cos(grid_lon(i,j)*deg2rad)
        y(i,j)=cos(grid_lat(i,j)*deg2rad)*sin(grid_lon(i,j)*deg2rad)
        z(i,j)=sin(grid_lat(i,j)*deg2rad)
     enddo
  enddo

!  2   find angles to E-W and N-S for U edges
  sq180=180.**2
  do j=1,ny+1
     do i=1,nx
!      center lat/lon of the edge
        rlat=0.50*(grid_lat(i,j)+grid_lat(i+1,j))
        diff=(grid_lon(i,j)-grid_lon(i+1,j))**2
        if(diff < sq180)then
           rlon=0.50*(grid_lon(i,j)+grid_lon(i+1,j))
        else
           rlon=0.50*(grid_lon(i,j)+grid_lon(i+1,j)-360.)
        endif
!    vector to center of the edge
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
!     vector of the edge
        xu= x(i+1,j)-x(i,j)
        yu= y(i+1,j)-y(i,j)
        zu= z(i+1,j)-z(i,j)
!    find angle with cross product
        uval=sqrt((xu**2+yu**2+zu**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangu(i,j)=(-yr*xu+xr*yu)/ewval/uval
        sangu(i,j)=(-xr*zr*xu-zr*yr*yu+(xr*xr+yr*yr)*zu) / nsval/uval
     enddo
  enddo

!  3   find angles to E-W and N-S for V edges
  do j=1,ny
     do i=1,nx+1
        rlat=0.50*(grid_lat(i,j)+grid_lat(i,j+1))
        diff=(grid_lon(i,j)-grid_lon(i,j+1))**2
        if(diff < sq180)then
           rlon=0.50*(grid_lon(i,j)+grid_lon(i,j+1))
        else
           rlon=0.50*(grid_lon(i,j)+grid_lon(i,j+1)-360.)
        endif
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
        xv= x(i,j+1)-x(i,j)
        yv= y(i,j+1)-y(i,j)
        zv= z(i,j+1)-z(i,j)
        vval=sqrt((xv**2+yv**2+zv**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangv(i,j)=(-yr*xv+xr*yv)/ewval/vval
        sangv(i,j)=(-xr*zr*xv-zr*yr*yv+(xr*xr+yr*yr)*zv) / nsval/vval
     enddo
  enddo

!  4,  clean up
  deallocate(x, y, z)

  return
  end subroutine cal_uv_coeff_fv3 
!========================================================================================
  subroutine earthuv2fv3(nx, ny, u, v, cangu, sangu, cangv, sangv, u_out, v_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    earthuv2fv3
!   prgmmr: wu                      2017-06-15
!
! abstract: project earth UV to fv3 UV and interpolate to edge of the cell
!
! program history log:
!
!
!   input argument list:
!    nx,ny - dimensions
!    u,v -  earth wind components at center of the cell
!    cangu, sangu, cangv, sangv 
!
!   output argument list:
!    u_out,v_out - output fv3 winds on the cell boundaries
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer, intent(in) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real, intent(in   ) :: u(nx,ny+1),v(nx+1,ny)
  real, intent(in   ) :: cangu(nx,ny+1),sangu(nx,ny+1)
  real, intent(in   ) :: cangv(nx+1,ny), sangv(nx+1,ny)

  real, intent(  out) :: u_out(nx,ny+1),v_out(nx+1,ny)

  integer             :: i,j,i1,j1
!!!!!!! earth u/v to covariant u/v
!  j=1
!  do i=1,nx
!     u_out(i,j)= u(i,j)*cangu(i,j)+v(i,j)*sangu(i,j)
!  end do
!
!  do j=2,ny
!     do i=1,nx
!        u_out(i,j)=0.50*( (u(i,j)+u(i,j-1))*cangu(i,j)+(v(i,j)+v(i,j-1))*sangu(i,j) )
!     end do
!  end do
!  j=ny
!  do i=1,nx
!     u_out(i,j+1)= u(i,j)*cangu(i,j+1)+v(i,j)*sangu(i,j+1)
!  end do
!
!  do j=1,ny
!     v_out(1,j)=u(1,j)*cangv(1,j)+v(1,j)*sangv(1,j)
!     do i=2,nx
!        v_out(i,j)=0.50*( (u(i,j)+u(i-1,j))*cangv(i,j)+(v(i,j)+v(i-1,j))*sangv(i,j) )
!     end do
!     v_out(nx+1,j)=u(nx,j)*cangv(nx+1,j)+v(nx,j)*sangv(nx+1,j)
!  end do

  do j = 1, ny+1; do i = 1, nx
     j1=min(j,ny)
     u_out(i,j)= u(i,j)*cangu(i,j)+v(i,j1)*sangu(i,j)
  enddo; enddo
  
  do j = 1, ny; do i = 1, nx+1
     i1=min(i,nx)
     v_out(i,j)=u(i1,j)*cangv(i,j)+v(i,j)*sangv(i,j)
  enddo; enddo
  
  return
  end subroutine earthuv2fv3
!========================================================================================
  subroutine fv3uv2earth(nx, ny, u, v, cangu, sangu, cangv, sangv, u_out, v_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3uv2earth
!   prgmmr: wu                      2017-06-15
!
! abstract: project fv3 UV to earth UV and interpolate to the center of the cells
!
! program history log:
!
!
!   input argument list:
!    nx,ny - dimensions
!    u,v -  earth wind components at center of the cell
!    cangu, sangu, cangv, sangv
!
!   output argument list:
!    u_out,v_out - output earth wind components at center of the cell
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer, intent(in) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real, intent(in   ) :: u(nx,ny+1),v(nx+1,ny)
  real, intent(in   ) :: cangu(nx,ny+1),sangu(nx,ny+1)
  real, intent(in   ) :: cangv(nx+1,ny), sangv(nx+1,ny)

  real, intent(  out) :: u_out(nx,ny+1),v_out(nx+1,ny)

  integer             :: i, j, i1, j1

!  do j=1,ny
!     do i=1,nx
!        u_out(i,j)=0.50 *( (u(i,j)*sangv(i,j)-v(i,j)*sangu(i,j))/(cangu(i,j)*sangv(i,j)-sangu(i,j)*cangv(i,j)) &
!                       +(u(i,j+1)*sangv(i+1,j)-v(i+1,j)*sangu(i,j+1))/(cangu(i,j+1)*sangv(i+1,j)-sangu(i,j+1)*cangv(i+1,j)))
!        v_out(i,j)=0.50 *( (u(i,j)*cangv(i,j)-v(i,j)*cangu(i,j))/(sangu(i,j)*cangv(i,j)-cangu(i,j)*sangv(i,j)) &
!                       +(u(i,j+1)*cangv(i+1,j)-v(i+1,j)*cangu(i,j+1))/(sangu(i,j+1)*cangv(i+1,j)-cangu(i,j+1)*sangv(i+1,j)))
!     end do
!  end do

  do j = 1, ny+1; do i = 1, nx
     j1=min(j,ny)
     u_out(i,j)=(u(i,j)*sangv(i,j1)-v(i,j1)*sangu(i,j))/(cangu(i,j)*sangv(i,j1)-sangu(i,j)*cangv(i,j1))
  enddo; enddo

  do j = 1, ny; do i = 1, nx+1
     i1=min(i,nx)
     v_out(i,j)=(u(i1,j)*cangv(i,j)-v(i,j)*cangu(i1,j))/(sangu(i1,j)*cangv(i,j)-cangu(i1,j)*sangv(i,j))
  enddo; enddo

  return
  end subroutine fv3uv2earth
!========================================================================================
! /work2/noaa/hwrf/noscrub/yweng/hafs_test/hafs_20211112/sorc/hafs_gsi.fd/src/gsi/general_tll2xy_mod.f90
!========================================================================================
