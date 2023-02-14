        subroutine rodist
        use vect,only:rovect,xvect,yvect,nmx
        use posit, only: xold,yold,xcorn,ycorn
        implicit none
        real:: theta
        integer::ip
        real:: r,yo,pi,pi180,xc,yc,fact

!c
        pi=4.0*atan(1.0)
        PI180 = 4.*ATAN(1.0)/180.
        yo=yold*pi180
!c qliu        fact=cos(yo)
        fact=1.0
        xc=xold-xcorn
        yc=yold-ycorn
!c
        do ip=1,nmx
          theta=float(ip-1)/float(nmx)*2.*pi
          r=rovect(ip)
          xvect(ip)=r*cos(theta)/fact +xc
          yvect(ip)=r*sin(theta) +yc
        enddo
!c

        return
        end subroutine rodist
