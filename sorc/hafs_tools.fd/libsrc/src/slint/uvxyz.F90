! =================================================================================!
! This file contains subroutines that converts wind at a given location between 
! its u, v components representation in latitude/logitude coordinate system and 
! x, y, z components representation R^3 Cartesian coordinate system.
!
! The x, y, z components representation of the wind lies in the plane taht is 
! tangent at the given latitude/logitude location. 
!
! (ux, uy, uz) = J_F * (u / cos(\lambda), v), 
! where J_F is the Jacobian matrix of F: [-pi/2,pi/2]x[0,2pi]-> R^3
!    
! The formulas for the two conversions are
!
! (ux, uy, uz) = (-u*sin(\lambda) - v*cos(\lambda)*sin(\theta),
!                  u*cos(\lambda) - v*sin(\lambda)*sin(\theta),
!                  v*cos(\theta) ) 
!
! (u, v) = (uy*cos(\lambda)-ux*sin(\lambda), uz/cos(\theta)), abs(\theta) < pi/4
! (u, v) = (uy*cos(\lambda)-ux*sin(\lambda), 
!           (-ux*cos(\lambda)-uy*sin(\lambda))/sin(\theta), otherwise. 
!
! Ning Wang,  Nov. 2012, Original version
! ================================================================================!
SUBROUTINE uv2xyz(u, v, lat,lon, ux, uy, uz)
     IMPLICIT NONE

     REAL, INTENT(IN) :: u, v, lat, lon
     REAL, INTENT(OUT) :: ux, uy, uz

     ux = -u * sin(lon) - v * cos(lon) * sin(lat)
     uy = u * cos(lon) - v * sin(lon) * sin(lat)
     uz = v * cos(lat)

END SUBROUTINE uv2xyz     
 
SUBROUTINE xyz2uv(u, v, lat,lon, ux, uy, uz)
     IMPLICIT NONE

     REAL, INTENT(IN) :: ux, uy, uz, lat, lon
     REAL, INTENT(OUT) :: u, v
     REAL :: pi

     pi  = acos(-1.0)
    
     u = uy * cos(lon) - ux * sin(lon)
     IF (abs(lat) < pi/4.0) THEN 
       v = uz / cos(lat) 
     ELSE
       v = (-ux * cos(lon) - uy * sin(lon)) / sin(lat)
     ENDIF 

END SUBROUTINE xyz2uv     
     
