      subroutine stndz(p,z,t,theta)
c     This routine calculates the standard height z (m) from the
c     pressure p (mb). The temperature t (K) and potential temperature
c     theta (K) at p are also calculated.
C
      g   = 9.80665
      r   = 287.05
      cp  = 1004.0
      b   = 0.0065
      p0  = 1013.25
      t0  = 288.15
      p00 = 1000.0
      p1  = 226.32
      t1  = 216.65
      z1  = 11000.0
      cap = r/cp
      a   = r*b/g
C
      z2  = 20000.0
      b2  = -0.0010
      p2  = 54.75
      t2  = t1
      a2  = r*b2/g
C
      if     (p .ge. p1) then
         z = (t0/b)*(1.0 - (p/p0)**a)
         t = t0 - b*z
      elseif (p .lt. p1 .and. p .ge. p2) then
         z = z1 + (r*t1/g)*alog(p1/p)
         t = t1
      else
	 z = z2 + (t2/b2)*(1.0 - (p/p2)**a2)
	 t = t2 - b2*(z-z2)
      endif
C
      theta = t*( (p00/p)**cap )
c
      return
      end
