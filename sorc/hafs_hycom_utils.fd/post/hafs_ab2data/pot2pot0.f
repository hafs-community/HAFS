      function pot2pot0(t,s)
c -- potential temperature at 2000db mapped to potential temperature at 0db
c -- temperatures are in degC, salinity is PPM.
c -- pade approximation to Feistel approximation

      implicit none
      real t,s,pot2pot0
      real c0,c1,c2,c3,b0,b1,b2
      parameter( c0=-0.1002972, c1= 0.98397639,
     &     c2=0.00013528999, c3 = -3.5892447e-006, b0= 0.99992366,
     & b1=-2.4368554e-005, b2= -4.3004172e-005)
      pot2pot0=(c0+t*(c1+t*(c2+s*c3)))/(b0+b1*s+b2*t)
      return
      end

