      real function cd(wsph)
! borrowed from HYCOM
      implicit none
      real wsph
      cd = 0.862e-3 + 0.088e-3 * wsph - 0.00089e-3 * wsph**2
      return
      end function cd
