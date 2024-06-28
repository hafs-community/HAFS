      SUBROUTINE SROTM (N,SX,INCX,SY,INCY,SPARAM)
      implicit none
!C
!C     APPLY THE MODIFIED GIVENS TRANSFORMATION, H, TO THE 2 BY N MATRIX
!C
!C     (SX**T) , WHERE **T INDICATES TRANSPOSE. THE ELEMENTS OF SX ARE IN
!C     (DX**T)
!C
!C     SX(LX+I*INCX), I = 0 TO N-1, WHERE LX = 1 IF INCX .GE. 0, ELSE
!C     LX = (-INCX)*N, AND SIMILARLY FOR SY USING USING LY AND INCY.
!C     WITH SPARAM(1)=SFLAG, H HAS ONE OF THE FOLLOWING FORMS..
!C
!C     SFLAG=-1.E0     SFLAG=0.E0        SFLAG=1.E0     SFLAG=-2.E0
!C
!C       (SH11  SH12)    (1.E0  SH12)    (SH11  1.E0)    (1.E0  0.E0)
!C     H=(          )    (          )    (          )    (          )
!C       (SH21  SH22),   (SH21  1.E0),   (-1.E0 SH22),   (0.E0  1.E0).
!C     SEE  SROTMG FOR A DESCRIPTION OF DATA STORAGE IN SPARAM.
!C

!Adopted from Qingfu Liu (NCEP/EMC). Another similar version is found online (SLATEC https://people.math.sc.edu/Burkardt/f_src/slatec/slatec.f90)
!! SROTM applies a modified Givens transformation.
!
!***LIBRARY   SLATEC (BLAS)
!***CATEGORY  D1A8
!***TYPE      SINGLE PRECISION (SROTM-S, DROTM-D)
!***KEYWORDS  BLAS, LINEAR ALGEBRA, MODIFIED GIVENS ROTATION, VECTOR
!***AUTHORS  Lawson, C. L., (JPL)
!           Hanson, R. J., (SNLA)
!           Kincaid, D. R., (U. of Texas)
!           Krogh, F. T., (JPL)
!   REVISED by Chuan-Kai Wang (NCEP/EMC 2022)
!***DESCRIPTION
!
!                B L A S  Subprogram
!    Description of Parameters
!
!     --Input--
!        N  number of elements in input vector(s)
!       SX  single precision vector with N elements
!     INCX  storage spacing between elements of SX
!       SY  single precision vector with N elements
!     INCY  storage spacing between elements of SY
!   SPARAM  5-element vector. SPARAM(1) is SFLAG described below.
!           Locations 2-5 of SPARAM contain elements of the
!           transformation matrix H described below.
!
!     --Output--
!       SX  rotated vector (unchanged if N  <=  0)
!       SY  rotated vector (unchanged if N  <=  0)


      integer incx,incy,n,i,kx,ky,nsteps
      real,DIMENSION(1):: SX(*),SY(*)
      real, dimension(5):: SPARAM(5)
      real zero,two,sflag,w,z,sh11,sh12,sh21,sh22
      DATA ZERO,TWO/0.E0,2.E0/
!C
      SFLAG=SPARAM(1)
      IF(N .LE. 0 .OR.(SFLAG+TWO.EQ.ZERO)) then
        return
      endif
      IF((INCX.EQ.INCY.AND. INCX .GT.0)) then
         NSTEPS=N*INCX
         if(sflag.eq.0) then
           SH12=SPARAM(4)
           SH21=SPARAM(3)
           DO I=1,NSTEPS,INCX
             W=SX(I)
             Z=SY(I)
             SX(I)=W+Z*SH12
             SY(I)=W*SH21+Z
           enddo
           return
         else if(sflag.gt.0) then
           SH11=SPARAM(2)
           SH22=SPARAM(5)
           DO I=1,NSTEPS,INCX
             W=SX(I)
             Z=SY(I)
             SX(I)=W*SH11+Z
             SY(I)=-W+SH22*Z
           enddo
           return
         else
           SH11=SPARAM(2)
           SH12=SPARAM(4)
           SH21=SPARAM(3)
           SH22=SPARAM(5)
           DO I=1,NSTEPS,INCX
             W=SX(I)
             Z=SY(I)
             SX(I)=W*SH11+Z*SH12
             SY(I)=W*SH21+Z*SH22
           enddo
           return
         endif
      else
        KX=1
        KY=1
        IF(INCX .LT. 0) KX=1+(1-N)*INCX
        IF(INCY .LT. 0) KY=1+(1-N)*INCY
        if(sflag.eq.0) then
          SH12=SPARAM(4)
          SH21=SPARAM(3)
          do i=1,n
            W=SX(KX)
            Z=SY(KY)
            SX(KX)=W+Z*SH12
            SY(KY)=W*SH21+Z
            KX=KX+INCX
            KY=KY+INCY
          enddo
          return
        else if(sflag.gt.0) then
          SH11=SPARAM(2)
          SH22=SPARAM(5)
          do i=1,n
            W=SX(KX)
            Z=SY(KY)
            SX(KX)=W*SH11+Z
            SY(KY)=-W+SH22*Z
            KX=KX+INCX
            KY=KY+INCY
          enddo
          return
        else
          SH11=SPARAM(2)
          SH12=SPARAM(4)
          SH21=SPARAM(3)
          SH22=SPARAM(5)
          do i=1,n
            W=SX(KX)
            Z=SY(KY)
            SX(KX)=W*SH11+Z*SH12
            SY(KY)=W*SH21+Z*SH22
            KX=KX+INCX
            KY=KY+INCY
          enddo
        endif
      endif
      RETURN
      END SUBROUTINE SROTM
