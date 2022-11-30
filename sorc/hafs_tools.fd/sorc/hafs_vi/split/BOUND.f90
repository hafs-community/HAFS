      SUBROUTINE BOUND(NMX,XR,ro)
      use xxx
      use posit
      implicit none
      integer,intent(in) :: nmx
      real, dimension(nmx):: XR,ro
      real:: theta,x,y,p,q,pi,fact
      integer::ix,iy,ix1,iy1,i

      PI = 4.*ATAN(1.0)
      fact=1.0
      DO I=1,NMX
        THETA= 2.*PI*FLOAT(I-1)/FLOAT(NMX)
        X=RO(i)/fact*COS(THETA)+XC +1.
        Y=RO(i)*SIN(THETA)+YC +1.
        IX=INT(X/DX)
        IY=INT(Y/DY)
        IX1=IX+1
        IY1=IY+1
        P=X/DX-FLOAT(IX)
        Q=Y/DY-FLOAT(IY)
        XR(I)=(1.-P)*(1.-Q)*XF(IX,IY) +(1.-P)*Q*XF(IX,IY+1)+(1.-Q)*P*XF(IX+1,IY) + P*Q*XF(IX+1,IY+1)
      ENDDO

      RETURN
      END
