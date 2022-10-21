 module setparms
   implicit none
   integer, parameter:: real_single = 4, real_double = real_single * 2
   integer, parameter:: int_single  = 4, int_double  = int_single * 2
 end module setparms

 module fileconst
 integer kunit,itim
 end module fileconst

 module tcvit
 implicit none
 integer,parameter :: nst=10
 character,save ::  TCVT(NST)*95
 end module tcvit

 module stname
 implicit none
 integer,parameter :: nst=10
 character,save ::  ST_NAME(NST)*3,STMNAME(NST)*3
 end module stname

 module rsfc
 implicit none
 integer,parameter :: nst=10
 real,save:: STRPSF(NST),STVMAX(NST),STRPSF_06(NST)
 end module rsfc

 module nhc
 implicit none
 integer,parameter :: nst=10
 integer,save:: KSTM,IC_N(NST),JC_N(NST)
 end module nhc

 module nhc1
 implicit none
 integer,parameter :: nst=10
 real,save:: SLON_N(NST),SLAT_N(NST),CLON_N(NST),CLAT_N(NST)
 end module nhc1

 module nhc2
 implicit none
 integer,save:: MDX,MDY
 end module nhc2

 module nhc3
 implicit none
 real,save:: AMDX,AMDY
 end module nhc3

 module mcoef3
 implicit none
 real*4,save:: FHOUR,DUMMY(245)
 end module mcoef3

 module tr
 implicit none
 integer, parameter:: nsg=720000
 integer,save:: IB,ING(NSG),JNG(NSG)
 end module tr

 module matrix
 implicit none
 integer, parameter :: nmx=24
 real,save::a(nmx,nmx),capd2
 end module matrix

 module posit
 implicit none
 real,save:: XOLD,YOLD,XCORN,YCORN
 real,save:: CLON_NEW,CLAT_NEW,SLON,SLAT,CLON,CLAT,RAD
 equivalence(XOLD,CLON_NEW)
 equivalence(YOLD,CLAT_NEW)
 equivalence(SLON,XCORN)
 equivalence(SLAT,YCORN)
 end module posit

 module xxx
 implicit none
 integer, parameter :: imx=41, jmx=41
 real,save:: XF(IMX,JMX),XC,YC,DX,DY
 end module xxx

 module vect
 implicit none
 integer, parameter:: nmx=24
 real,save:: R0(nmx),rovect(nmx),xvect(nmx),yvect(nmx)
 equivalence(R0,rovect)
 end module vect
