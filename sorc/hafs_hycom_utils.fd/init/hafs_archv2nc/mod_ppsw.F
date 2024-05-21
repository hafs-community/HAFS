      module mod_ppsw
c
c --- parts of ppsw.f in a module
c
c separate set of subroutines, adapted from WHOI CTD group
c     real function atg(s,t,p)
c     real function theta(s,t0,p0,pr)
c     function p80(dpth,xlat)
c
c n fofonoff & r millard
c
      contains

      real function atg(s,t,p)
c ****************************
c adiabatic temperature gradient deg c per decibar
c ref: bryden,h.,1973,deep-sea res.,20,401-408
c units:
c       pressure        p        decibars
c       temperature     t        deg celsius (ipts-68)
c       salinity        s        (ipss-78)
c       adiabatic       atg      deg. c/decibar
c checkvalue: atg=3.255976e-4 c/dbar for s=40 (ipss-78),
c t=40 deg c,p0=10000 decibars
      ds = s - 35.0
      atg = (((-2.1687e-16*t+1.8676e-14)*t-4.6206e-13)*p
     x+((2.7759e-12*t-1.1351e-10)*ds+((-5.4481e-14*t
     x+8.733e-12)*t-6.7795e-10)*t+1.8741e-8))*p
     x+(-4.2393e-8*t+1.8932e-6)*ds
     x+((6.6228e-10*t-6.836e-8)*t+8.5258e-6)*t+3.5803e-5
      end function atg

      real function theta(s,t0,p0,pr)
c ***********************************
c to compute local potential temperature at pr
c using bryden 1973 polynomial for adiabatic lapse rate
c and runge-kutta 4-th order integration algorithm.
c ref: bryden,h.,1973,deep-sea res.,20,401-408
c fofonoff,n.,1977,deep-sea res.,24,489-491
c units:
c       pressure        p0       decibars
c       temperature     t0       deg celsius (ipts-68)
c       salinity        s        (ipss-78)
c       reference prs   pr       decibars
c       potential tmp.  theta    deg celsius
c checkvalue: theta= 36.89073 c,s=40 (ipss-78),t0=40 deg c,
c p0=10000 decibars,pr=0 decibars
c
c      set-up intermediate temperature and pressure variables
      p=p0
      t=t0
c**************
      h = pr - p
      xk = h*atg(s,t,p)
      t = t + 0.5*xk
      q = xk
      p = p + 0.5*h
      xk = h*atg(s,t,p)
      t = t + 0.29289322*(xk-q)
      q = 0.58578644*xk + 0.121320344*q
      xk = h*atg(s,t,p)
      t = t + 1.707106781*(xk-q)
      q = 3.414213562*xk - 4.121320344*q
      p = p + 0.5*h
      xk = h*atg(s,t,p)
      theta = t + (xk-2.0*q)/6.0
      end function theta

c
c pressure from depth from saunder's formula with eos80.
c reference: saunders,peter m., practical conversion of pressure
c            to depth., j.p.o. , april 1981.
c r millard
c march 9, 1983
c check value: p80=7500.004 dbars;for lat=30 deg., depth=7321.45 meters
      real function p80(dpth,xlat)
      parameter pi=3.141592654
      plat=abs(xlat*pi/180.)
      d=sin(plat)
      c1=5.92e-3+d**2*5.25e-3
      p80=((1-c1)-sqrt(((1-c1)**2)-(8.84e-6*dpth)))/4.42e-6
      end function p80

      end module mod_ppsw
