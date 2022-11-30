        subroutine amatrix
        use matrix, only: a, capd2,nmx
        use posit, only: xold,yold,xcorn,ycorn
        use vect, only: xvect,yvect!,rovect

        implicit none
        real:: dpij,PI180,fact,yo
        integer:: ip,jp

!c
        PI180 = 4.*ATAN(1.0)/180.
        yo=yold*pi180

!        print*,'yold',yold

!c qliu        fact=cos(yo)
        fact=1.0
!c       capd2=(3.15)*(3.15)
        capd2=(2.25)*(2.25)
        do ip=1,nmx
          do jp=ip,nmx
            dpij=(fact*(xvect(ip)-xvect(jp)))**2 +(yvect(ip)-yvect(jp))**2
            a(ip,jp)= exp(-dpij/capd2)
            a(jp,ip)= a(ip,jp)
          enddo
        enddo
100     format(5f8.4)
        return
        end subroutine amatrix
