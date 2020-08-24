c-----------------------------------------------------------------------------
      integer, parameter ::
     &  sigver=4  !9-term sigma-2
csig0&  sigver=3  !9-term sigma-0
c
      real*8  sig,dsigdt,dsigds,tofsig,sofsig,sigloc,dsiglocdt,dsiglocds
c
      real*8  r8
      real*4  r4
c
      real*8  sofsig_a,sofsig_b,sofsig_c
      real*8  a0,a1,a2,cubr,cubq,cuban,cubrl,cubim
      real*8  c1l,c2l,c3l,c4l,c5l,c6l,c7l
c
      real*8  r,s,t,prs
c
      real*8, parameter ::
     &   ahalf=1.0/2.0,
     &   a3rd =1.0/3.0, athird =a3rd,
     &   a4th =1.0/4.0, afourth=a4th
c
c --- sigma-theta as a function of temp (deg c) and salinity (psu)
c --- (9-term polynomial fit to T:[-2:30],S:[18:38])
c
c --- coefficients for sigma-0.
csig0 real*8, parameter ::
csig0&   c1=-4.311829E-02,  !const. coefficent
csig0&   c2= 5.429948E-02,  !T      coefficent
csig0&   c3= 8.011774E-01,  !   S   coefficent
csig0&   c4=-7.641336E-03,  !T^2    coefficent
csig0&   c5=-3.258442E-03,  !T  S   coefficent
csig0&   c6= 3.757643E-05,  !T^3    coefficent
csig0&  rc6=1.0/c6,
csig0&   c7= 3.630361E-05,  !T^2S   coefficent
csig0&   c8= 8.675546E-05,  !   S^2 coefficent
csig0&   c9= 3.995086E-06   !T  S^2 coefficent
c --- coefficients for sigma-2.
      real*8, parameter ::
     &   c1= 9.903308E+00,  !const. coefficent
     &   c2=-1.618075E-02,  !T      coefficent
     &   c3= 7.819166E-01,  !   S   coefficent
     &   c4=-6.593939E-03,  !T^2    coefficent
     &   c5=-2.896464E-03,  !T  S   coefficent
     &   c6= 3.038697E-05,  !T^3    coefficent
     &  rc6= 1.0/c6,
     &   c7= 3.266933E-05,  !T^2S   coefficent
     &   c8= 1.180109E-04,  !   S^2 coefficent
     &   c9= 3.399511E-06   !T  S^2 coefficent
c
c --- HYCOM pressure to bar, for locally referenced equations
      real*8, parameter :: prs2pb=1.e-5     !Pascals to bar
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real*8, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
c --- auxiliary statements for finding root of cubic polynomial
      a0(s,r)=(c1+s*(c3+s*c8)-r)*rc6  !constant  coefficient
      a1(s)  =(c2+s*(c5+s*c9)  )*rc6  !linear    coefficient
      a2(s)  =(c4+s* c7        )*rc6  !quadratic coefficient
                                      !cubic     coefficient is c6*rc6=1.0
      cubq(s)  =a3rd*     a1(s)                   -(a3rd*a2(s))**2
      cubr(r,s)=a3rd*(0.5*a1(s)*a2(s)-1.5*a0(s,r))-(a3rd*a2(s))**3
c --- if q**3+r**2>0, water is too dense to yield real root at given
c --- salinitiy. setting q**3+r**2=0 in that case is equivalent to
c --- lowering sigma until a double real root is obtained.
      cuban(r,s)=a3rd*atan2(sqrt(max(0.d0,-(cubq(s)**3+cubr(r,s)**2))),
     &                      cubr(r,s))
      cubrl(r,s)=sqrt(-cubq(s))*cos(cuban(r,s))
      cubim(r,s)=sqrt(-cubq(s))*sin(cuban(r,s))
c
c --- -----------------
c --- equation of state
c --- -----------------
c
c --- sigma-theta as a function of temp (deg c) and salinity (psu)
c --- (polynomial fit that is cubic in T and quadratic in S)
c
      sig(t,s)=(c1+s*(c3+s* c8)+
     &             t*(c2+s*(c5+s*c9)+t*(c4+s*c7+t*c6)))
c
c --- d(sig)/dt
      dsigdt(t,s)=(c2+s*(c5+s*c9)+2.0*t*(c4+s*c7+1.5*t*c6))
c
c --- d(sig)/ds
      dsigds(t,s)=(c3+t*(c5+t*c7)+2.0*s* c8)
c
c --- temp (deg c) as a function of sigma and salinity (psu)
c --- find a cubic polynominal root of t**3+a2*t**2+a1*t+a0=0
      tofsig(r,s)=-cubrl(r,s)+sqrt(3.0)*cubim(r,s)-a3rd*a2(s)
c
c --- salinity (psu) as a function of sigma and temperature (deg c)
c --- find a quadratic polynominal root of a*s**2+b*s+c=0
      sofsig_a(r,t)=(c8+t* c9)                !quadratic coefficient
      sofsig_b(r,t)=(c3+t*(c5+t* c7))         !linear    coefficient
      sofsig_c(r,t)=(c1+t*(c2+t*(c4+t*c6))-r) !constant  coefficient
      sofsig(r,t)=(2.d0*sofsig_c(r,t))/
     &            (-sofsig_b(r,t)
     &             -sign(sqrt(sofsig_b(r,t)**2-
     &                        4.d0*sofsig_a(r,t)*sofsig_c(r,t)),
     &                   sofsig_b(r,t)))
c
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1l(prs)=alphap(1)+prs2pb*prs*(betap(1)+prs2pb*prs*gammap(1))
      c2l(prs)=alphap(2)+prs2pb*prs*(betap(2)+prs2pb*prs*gammap(2))
      c3l(prs)=alphap(3)+prs2pb*prs*(betap(3)+prs2pb*prs*gammap(3))
      c4l(prs)=alphap(4)+prs2pb*prs*(betap(4)+prs2pb*prs*gammap(4))
      c5l(prs)=alphap(5)+prs2pb*prs*(betap(5)+prs2pb*prs*gammap(5))
      c6l(prs)=alphap(6)+prs2pb*prs*(betap(6)+prs2pb*prs*gammap(6))
      c7l(prs)=alphap(7)+prs2pb*prs*(betap(7)+prs2pb*prs*gammap(7))
      sigloc(t,s,prs)=c1l(prs)+c3l(prs)*s+
     &       t*(c2l(prs)+c5l(prs)*s+t*(c4l(prs)+c7l(prs)*s+c6l(prs)*t))
      dsiglocdt(t,s,prs)=(c2l(prs)+c5l(prs)*s+
     &       2.0*t*(c4l(prs)+c7l(prs)*s+1.5*c6l(prs)*t))
      dsiglocds(t,s,prs)=(c3l(prs)+t*(c5l(prs)+t*c7l(prs)))
c
c --- auxiliary statement for real to real*8 conversion
      r8(r4) = r4
c-----------------------------------------------------------------------------
