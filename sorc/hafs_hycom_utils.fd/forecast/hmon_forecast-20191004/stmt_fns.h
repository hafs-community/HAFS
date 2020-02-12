c-----------------------------------------------------------------------------
      integer, parameter ::
     &  sigver=6  !17-term sigma-2
csig0&  sigver=5  !17-term sigma-0
c
      real    sig,dsigdt,dsigds,tofsig,sofsig,kappaf,
     &        sigloc,dsiglocdt,dsiglocds
c
      real    sig_n,sig_d,sig_q, dsigdt_n,dsigdt_d, dsigds_n,dsigds_d
      real    kappaf1
      real    sigloc_n,sigloc_d,sigloc_q,
     &        dsiglocdt_n,dsiglocdt_d, dsiglocds_n,dsiglocds_d
c
      real    r,s,t,pdb,prs
      integer kkf
c
      real, parameter ::
     &   aone =1.0,
     &   ahalf=1.0/2.0,
     &   a3rd =1.0/3.0, athird =a3rd,
     &   a4th =1.0/4.0, afourth=a4th
c
      real, parameter ::
     &   c1= 1.0e-01,      !not used, required to compile mxkrtm.f
     &   c2= 1.0e-02,      !not used, required to compile mxkrtm.f
     &   c3= 1.0e-03,      !not used, required to compile mxkrtm.f
     &   c4= 1.0e-04,      !not used, required to compile mxkrtm.f
     &   c5= 1.0e-05,      !not used, required to compile mxkrtm.f
     &   c6= 1.0e-06,      !not used, required to compile mxkrtm.f
     &   c7= 1.0e-07       !not used, required to compile mxkrtm.f
c
c --- Jackett, McDougall, Feistel, Wright and Griffies (2006), 
c --- Algorithms for Density, Potential Temperature, Conservative
c --- Temperature, and the Freezing Temperature of Seawater, JAOT
c
c --- coefficients for 25-term rational function sigloc().
      real, parameter ::
     &   c001= 9.9984085444849347d+02,     !num. constant    coefficent
     &   c002= 7.3471625860981584d+00,     !num.    T        coefficent
     &   c003=-5.3211231792841769d-02,     !num.    T^2      coefficent
     &   c004= 3.6492439109814549d-04,     !num.    T^3      coefficent
     &   c005= 2.5880571023991390d+00,     !num.       S     coefficent
     &   c006= 6.7168282786692355d-03,     !num.    T  S     coefficent
     &   c007= 1.9203202055760151d-03,     !num.       S^2   coefficent
     &   c008= 1.0000000000000000d+00,     !den. constant    coefficent
     &   c009= 7.2815210113327091d-03,     !den.    T        coefficent
     &   c010=-4.4787265461983921d-05,     !den.    T^2      coefficent
     &   c011= 3.3851002965802430d-07,     !den.    T^3      coefficent
     &   c012= 1.3651202389758572d-10,     !den.    T^4      coefficent
     &   c013= 1.7632126669040377d-03,     !den.       S     coefficent
     &   c014= 8.8066583251206474d-06,     !den.    T  S     coefficent
     &   c015= 1.8832689434804897d-10,     !den.    T^3S     coefficent
     &   c016= 5.7463776745432097d-06,     !den.    T  S^1.5 coefficent
     &   c017= 1.4716275472242334d-09      !den.    T^3S^1.5 coefficent
      real, parameter ::
     &   c018= 1.1798263740430364d-02,     !num. P           coefficent
     &   c019= 9.8920219266399117d-08,     !num. P  T^2      coefficent
     &   c020= 4.6996642771754730d-06,     !num. P     S     coefficent
     &   c021= 2.5862187075154352d-08,     !num. P^2         coefficent
     &   c022= 3.2921414007960662d-12,     !num. P^2T^2      coefficent
     &   c023= 6.7103246285651894d-06,     !den. P           coefficent
     &   c024= 2.4461698007024582d-17,     !den. P^2T^3      coefficent
     &   c025= 9.1534417604289062d-18      !den. P^3T        coefficent
c --- additional coefficients for dsiglocdt().
      real, parameter ::
     &   c031= 7.3471625860981580d+00,     !num. constant    coefficent
     &   c032=-1.0642246358568354d-01,     !num.    T        coefficent
     &   c033= 1.0947731732944364d-03,     !num.    T^2      coefficent
     &   c034= 6.7168282786692355d-03,     !num.       S     coefficent
     &   c035= 7.2815210113327090d-03,     !den. constant    coefficent
     &   c036=-8.9574530923967840d-05,     !den.    T        coefficent
     &   c037= 1.0155300889740728d-06,     !den.    T^2      coefficent
     &   c038= 5.4604809559034290d-10,     !den.    T^3      coefficent
     &   c039=-8.8066583251206470d-06,     !den.       S     coefficent
     &   c040= 5.6498068304414700d-10,     !den.    T^2S     coefficent
     &   c041= 2.9432550944484670d-09,     !den.    T  S^1.5 coefficent
     &   c042= 1.9784043853279823d-07,     !num. P  T        coefficent
     &   c043= 6.5842828015921320d-12,     !num. P^2T        coefficent
     &   c044= 7.3385094021073750d-17,     !den. P^2T^2      coefficent
     &   c045= 9.1534417604289060d-18      !den. P^3         coefficent
c --- additional coefficients for dsiglocds().
      real, parameter ::
     &   c051= 2.5880571023991390d+00,     !num. constant    coefficent
     &   c052= 6.7168282786692355d-03,     !num.    T        coefficent
     &   c053= 3.8406404111520300d-03,     !num.       S     coefficent
     &   c054= 1.7632126669040377d-03,     !den. constant    coefficent
     &   c055=-8.8066583251206470d-06,     !den.    T        coefficent
     &   c056= 1.8832689434804897d-10,     !den.    T^3      coefficent
     &   c057= 8.6195665118148150d-06,     !den.       S^0.5 coefficent
     &   c058= 2.2074413208363504d-09,     !den.    T^2S^0.5 coefficent
     &   c059= 4.6996642771754730d-06      !num. P           coefficent
c
      real, parameter :: sqrmin=0.d0       !sqrt arg can't be negative
c --- reference pressure.
      real, parameter :: prs2pdb=1.d-4     !Pascals to dbar
csig0 real, parameter :: pref=   0.d0      !ref. pressure in Pascals, sigma0
      real, parameter :: pref=2000.d4      !ref. pressure in Pascals, sigma2
      real, parameter :: rpdb=pref*prs2pdb !ref. pressure in dbar
c --- coefficients for 17-term rational function sig() at rpdb.
      real, parameter ::
     &   c101=c001+(c018-c021*rpdb)*rpdb, !num. constant    coefficent
     &   c103=c003+(c019-c022*rpdb)*rpdb, !num.    T^2      coefficent
     &   c105=c005+c020*rpdb,             !num.       S     coefficent
     &   c108=c008+c023*rpdb,             !den. constant    coefficent
     &   c109=c009-c025*rpdb**3,          !den.    T        coefficent
     &   c111=c011-c024*rpdb**2           !den.    T^3      coefficent
c --- additional coefficients for dsigdt().
      real, parameter ::
     &   c132=c032+(c042-c043*rpdb)*rpdb, !num.    T        coefficent
     &   c135=c035-c045*rpdb**3,          !den. constant    coefficent
     &   c137=c037-c044*rpdb**2           !den.    T^2      coefficent
c --- additional coefficients for dsigds().
      real, parameter ::
     &   c151=c051+c059*rpdb              !num. constant    coefficent
c
c --- coefficients for kappa^(theta)
c --- new values (w.r.t. t-toff,s-soff,prs) from Shan Sun, Sep.2004
c --- 1=Arctic/Antarctic; 2=Atlantic; 3=Mediterranean
      real, parameter ::
     &   rhoref=1.d3  !rhoref=qthref kg/m^3
      real, parameter ::
     &   sclkap=1.e-11
      real, parameter, dimension(3) ::
     &  toff = (/  0.0,             3.0,            13.0 /)
     & ,soff = (/ 34.5,            35.0,            38.5 /)
     & ,qttt = (/ -3.03869354E-05, -3.03869352E-05, -3.03869353E-05 /)
     & ,qtt  = (/  4.56625601E-03,  4.29277358E-03,  3.38116552E-03 /)
     & ,qt   = (/ -2.88801209E-01, -2.61828868E-01, -1.81335007E-01 /)
     & ,qs   = (/ -1.08670290E-01, -1.05131061E-01, -9.33336309E-02 /)
     & ,qst  = (/  7.90503772E-04,  7.71096940E-04,  1.07270585E-03 /)
     & ,qpt  = (/  1.07813750E-09,  1.00638435E-09,  7.57239852E-10 /)
     & ,qpst = (/  1.41541548E-11,  1.48598578E-11,  3.89226107E-12 /)
     & ,qptt = (/ -1.31383708E-11, -1.31383707E-11, -1.31383708E-11 /)
c
c --- -----------------
c --- equation of state
c --- -----------------
c
c --- sigma at rpdb (dbar) as a function of pot.temp (deg c) and salinity (psu)
c
      sig_n(t,s) = c101 + t*(c002+t*(c103+t*c004)) +
     &                    s*(c105-t*c006+s*c007)
      sig_d(t,s) = c108 + t*(c109+t*(c010+t*(c111+t*c012))) +
     &                    s*(c013-t*(c014+t*t*c015) +
     &  sqrt(max(sqrmin,s))*(c016+t*t*c017))
      sig_q(t,s) = aone/sig_d(t,s)
      sig(  t,s) = sig_n(t,s)*sig_q(t,s) - rhoref
c
c --- d(sig)/dt
      dsigdt_n(t,s) = c031 + t*(c132+t*c033) -
     &                       s* c034
      dsigdt_d(t,s) = c135 + t*(c036+t*(c137+t*c038)) +
     &                       s*(c039-t*t*c040+
     &             sqrt(max(sqrmin,s))*t*c041 )
      dsigdt(  t,s) = (dsigdt_n(t,s)-
     &                 dsigdt_d(t,s)*sig_n(t,s)*sig_q(t,s))*sig_q(t,s)
c
c --- d(sig)/ds
c --- additional coefficients for dsigds().
      dsigds_n(t,s) = c151 - t*c052 + s*c053
      dsigds_d(t,s) = c054 +       t*(c055-t*t*c056) +
     &           sqrt(max(sqrmin,s))*(c057+t*t*c058)
      dsigds(  t,s) = (dsigds_n(t,s)-
     &                 dsigds_d(t,s)*sig_n(t,s)*sig_q(t,s))*sig_q(t,s)
c
c --- temp (deg c) as a function of sigma and salinity (psu)
c --- NOT AVAILABLE AS AN EXPRESSION - DO NOT USE
      tofsig(r,s)=99.0
c
c --- salinity (psu) as a function of sigma and temperature (deg c)
c --- NOT AVAILABLE AS AN EXPRESSION - DO NOT USE
      sofsig(r,t)=99.0
c
c --- locally referenced sigma, using the 25-term equation of state.
c --- t: potential temperature (degC); s: salinity (psu); prs: pressure (dbar)
      sigloc_n(t,s,pdb) =      c001 +
     &                      t*(c002 +
     &                      t*(c003 +
     &                      t* c004  )) +
     &                      s*(c005 -
     &                      t* c006 +
     &                      s* c007  ) +
     &                    pdb*(c018 +
     &                    t*t* c019 +
     &                      s* c020 -
     &                    pdb*(c021 +
     &                    t*t* c022  ))
      sigloc_d(t,s,pdb) =      c008 +
     &                      t*(c009 +
     &                      t*(c010 +
     &                      t*(c011 +
     &                      t* c012  ))) +
     &                      s*(c013 -
     &                      t*(c014 +
     &                    t*t* c015  ) +
     &    sqrt(max(sqrmin,s))*(c016 +
     &                    t*t* c017  )) +
     &                    pdb*(c023 -
     &             pdb*t*(t*t* c024 +
     &                    pdb* c025  ))
      sigloc_q(t,s,pdb) = aone/sigloc_d(t,s,pdb)
      sigloc(t,s,prs)=sigloc_n(t,s,prs*prs2pdb)*
     &                sigloc_q(t,s,prs*prs2pdb) - rhoref
c
c --- d(sig)/dt
      dsiglocdt_n(t,s,pdb) =   c031 +
     &                      t*(c032 +
     &                      t* c033  ) -
     &                      s* c034 +
     &                  pdb*t*(c042 -
     &                    pdb* c043  )
      dsiglocdt_d(t,s,pdb) =   c035 +
     &                      t*(c036 +
     &                      t*(c037 +
     &                      t* c038  )) +
     &                      s*(c039 -
     &                    t*t* c040 +
     &  sqrt(max(sqrmin,s))*t* c041  ) -
     &           pdb*pdb*(t*t* c044 +
     &                    pdb* c045  )
      dsiglocdt(t,s,prs)=(dsiglocdt_n(t,s,prs*prs2pdb)-
     &                    dsiglocdt_d(t,s,prs*prs2pdb)*
     &                       sigloc_n(t,s,prs*prs2pdb)*
     &                       sigloc_q(t,s,prs*prs2pdb) ) *
     &                       sigloc_q(t,s,prs*prs2pdb)
c
c --- d(sig)/ds
      dsiglocds_n(t,s,pdb) =   c051 -
     &                      t* c052 +
     &                      s* c053 +
     &                    pdb* c059
      dsiglocds_d(t,s,pdb) =   c054 +
     &                      t*(c055 -
     &                    t*t* c056  ) +
     &    sqrt(max(sqrmin,s))*(c057 +
     &                    t*t* c058  )
      dsiglocds(t,s,prs)=(dsiglocds_n(t,s,prs*prs2pdb)-
     &                    dsiglocds_d(t,s,prs*prs2pdb)*
     &                       sigloc_n(t,s,prs*prs2pdb)*
     &                       sigloc_q(t,s,prs*prs2pdb) ) *
     &                       sigloc_q(t,s,prs*prs2pdb)
c
c --- thermobaric compressibility coefficient (integral from prs to pref)
c ---     Sun et.al. (1999) JPO 29 pp 2719-2729.
c --- kappaf1 used internally to simplify offsetting T and S,
c --- always invoke via kappaf.
c --- offset limits based on stability estimates from:
c ---     Hallberg (2005) Ocean Modelling 8 pp 279-300.
c --- t: potential temperature (degC); s: salinity (psu);
c --- r: potential density (sigma); prs: pressure; kkf: ref.state
c ---     example: kappaf(4.5,34.5,36.406,1.e7,1) = -0.12301201
c ---     example: kappaf(4.5,34.5,36.406,1.e7,2) = -0.03356404
c ---     example: kappaf(4.5,34.5,36.406,1.e7,3) =  0.05201003
      kappaf1(t,s,r,prs,kkf)=(r+rhoref)*
     &  (exp(sclkap*(prs-pref)*
     &        ( s*( qs(kkf)+t* qst(kkf) ) +
     &          t*( qt(kkf)+t*(qtt(kkf)+t*qttt(kkf))+
     &              0.5*(prs+pref)*
     &              (qpt(kkf)+s*qpst(kkf)+t*qptt(kkf)) ) ) )
     &   -1.0)
      kappaf(t,s,r,prs,kkf)=
     &     kappaf1(max(-1.2,         t-toff(kkf) ),  !Hallberg,T-only: -1.8,0.9
     &             max(-3.0,min(1.5, s-soff(kkf))),  !Hallberg,S-only: -4.2,2.1
     &             r,prs,kkf)
c
c> Revision history
c>
c> May  2000 - conversion to SI units
c> Jul  2000 - removed rarely used functions, constants via parameter
c> Jan  2002 - removed geometery functions
c> Dec  2002 - new thermobaricity fit with toff=0.0,soff=34.0
c> Jun  2003 - removed sigma4
c> Jun  2003 - added locally referenced sigma
c> Sep  2004 - added kkf to kappaf, select one of three reference states
c> Aug  2006 - more restrictive kappaf1 offset limits
c> Sep  2006 - 9-term polynominal fit to T:[-2:30],S:[18:38]
c> May  2007 - added sigver
c> Mar  2009 - modified limits in kappaf
c> Mar  2009 - more accurate kappaf, with potential density
c> Oct  2010 - 17-term rational function equation of state
c-----------------------------------------------------------------------------
