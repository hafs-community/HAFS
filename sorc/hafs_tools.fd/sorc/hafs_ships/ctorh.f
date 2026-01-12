      subroutine ctorh(x,y,r,theta)
c     This routine converts from Cartesion coordinates
c     to radial coordinates, where theta is in
c     degrees measured clockwise from
c     the +y-axis (standard meteorological heading).
c
      r = sqrt(x*x + y*y)
c
      if (r .le. 0.0) then
         theta = 0.0
         return
      endif
c
      rtd = 57.296
      theta = rtd*acos(x/r)
      if (y .lt. 0.0) theta = 360.0 - theta
c
c     Convert theta to heading
      theta = 90.0 - theta
      if (theta .lt. 0.0) theta = theta + 360.0
c
      return
      end
      subroutine rhtoc(r,thetah,x,y)
c     This routine converts from radial coordinates
c     to Cartesian coordinates, where theta is in
c     degrees measured lockwise from
c     the +y-axis (standard meteorological heading).
c
c     Convert theta from heading to standard definition
      theta = 90.0 - thetah
      if (theta .lt. 0.0) theta = theta + 360.0
c
      rtd = 57.296
      x = r*cos(theta/rtd)
      y = r*sin(theta/rtd)
c
      return
      end
