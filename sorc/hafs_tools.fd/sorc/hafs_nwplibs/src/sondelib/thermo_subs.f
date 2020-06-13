C A collection of subroutines for thermodynamic analysis. 
C Adapted from "Thermodynamic Analysis Procedures at the
C National Severe Storms Forecast Center", Doswell, et al 1982
C 9th conf on wx fcst and anal, AMS p304-309
      function wobf(T)
C compute Wobus function = thw(saturated) - thw(dry)
      x=t-20.0
      if(x.le.0)then
C       curve fit for colder temperatures 
        pol=1.0 + x*(-8.8416605e-3 + x*(1.4714143e-4 +
     *        x*(-9.671989e-7 + x*(-3.2607217e-8 +
     *        x*(-3.8598073e-10)))))
        pol=pol*pol 
        wobf=15.130/(pol*pol) 
      else
C       curve fit for warmer temperatures 
        pol=1.0 + x*(3.6182989e-3 + x*(-1.3603273e-5 +
     *        x*(4.9618922e-7 + x*(-6.1059365e-9 +
     *        x*(3.9401551e-11 + x*(-1.2588129e-13 +
     *        x*(1.6688280e-16))))))) 
        pol=pol*pol 
        wobf=29.930/(pol*pol)+0.960*x-14.800
      end if
      return
      end 
C ************************* 
      function tconof(temp,dewpt) 
C computes condensation temperature (deg C) by lifting. 
C temp and dewpt must be in same units deg C or K 
      s=temp-dewpt
      t=temp
      if(100.0.lt.temp)then ! temp must be in K so change 
        t=temp-273.16 
      end if
      dlt=s*(1.2185+0.001278*t + s*(-0.00219 + s*11.73e-6 - t*5.20e-6)) 
      tconof=t-dlt
      return
      end 
C **************************
      function satlft(thm,p)
C compute temp (deg C) where Theta Moist (thm in deg C) crosses P(mb) 
C consider the exponential for potential temperature as R/Cp
      rocp=0.28571428 
      if(abs(p-1000.0).le.0.0010)then 
        satlft=thm
        return
      end if
      pwrp=(p*0.001)**rocp
      t1=(thm+273.16)*pwrp-273.16 
      e1=wobf(t1)-wobf(thm) 
      rate=1.0
C 
10    t2=t1-e1*rate 
      e2=(t2+273.16)/pwrp-273.16
      e2=e2+wobf(t2)-wobf(e2)-thm 
      eor=e2*rate 
      if(abs(eor).gt.0.001)then ! repeat iteration
        rate=(t2-t1)/(e2-e1)
        t1=t2 
        e1=e2 
        go to 10
      end if
      satlft=t2-eor 
      return
      end 
C ********************
      function vapfw(T) 
C compute sat vap press over water . vapfw in mbs and t in deg C. 
      x=t 
      if(t.gt.100)then ! convert to C 
              x=t-273.16
      end if
C curve fit for range -50 to 100 deg C. 
      pol=0.99999683 + x*(-0.90826951e-2 +
     *        x*(0.78736169e-4 + x*(-0.61117958e-6 +
     *        x*(0.43884187e-8 + x*(-0.29883885e-10 + 
     *        x*(0.21874425e-12 + x*(-0.17892321e-14 +
     *        x*(0.11112018e-16 + x*(-0.30994571e-19))))))))) 
      pol=pol*pol 
      pol=pol*pol 
      vapfw=6.1078/(pol*pol)
      return
      end 
C **************************************
      function dptof(ew)
C compute dewpoint in deg C given water vapor pressure in mB
C tolerance can be set to any degree desired
C code modified to avoid infinite pinging as per p. 27 of NWS ERCP # 9, 
C "Stability Analysis Program" by Hugh Stone, 1983. 
      tol=0.0001
      if((ew.le.0.21382876e-9).or.(ew.gt.1013.0))then 
              dptof=-10000.0
              return
      end if
C first guess from inverting Tetens formula 
      x=alog(ew/6.1078) 
      bot=17.269388-x 
      dptof=(237.3*x)/bot 
      bot=bot*ew
      deltm=0.0 
100   edp=vapfw(dptof)
C correct guess by dT/dew, calculated from inverse of Tetens formula
      dtde=(dptof+237.3)/bot
      delt=dtde*(ew-edp)
      dptof=dptof+delt
C check that iteration is not in an endless cycle, a rare situation.
      dm=delt-deltm 
      if(abs(dm).lt.1.0e-7)then ! if dm too small iteration endless.
              tol=abs(delt) 
              write(1,*)' had to change dptof tol to',tol 
      end if
      deltm=-delt 
      if(abs(delt).gt.tol) go to 100 ! crank it through again 
C change so dewpoint always less than temp. compat with tol is forced 
      dptof=dptof-tol 
      return
      end 
C *************************** 
      function wmrof(p,td)
C compute mixing ratio (g/kg) from td in deg C and p in mB
      t=td
      if(t.gt.100)then ! convert from deg K to deg C
              t=td-273.16 
      end if
C curve fit for non-ideal gas 
      x=0.02*(t-12.5+7500.0/p)
      wfw=1.0 + 0.0000045*p + 0.0014*x*x
C now compute according to standard formula 
      fwesw=wfw*vapfw(t)
      wmrof=621.97*(fwesw/(p-fwesw))
      return
      end 
C **********
C  use the above routines to find some standard stuff 
      function dew_pt(T, RH)
C T in deg C, RH in % 
      if((RH.gt.100.0).or.(RH.lt.0.0))then ! must be caca or missing
              dew_pt=-99.0
              return
      end if
      es=vapfw(T) ! saturation vapor pressure at T
      ew=RH*0.01*es ! actual saturation vapor pressure
      dew_pt=dptof(ew)
      return
      end 
C ********* 
      function theta(T,P) 
      rocp=0.28571428 
C T in deg C, P in mB...return potential temperature in deg C 
      theta=(T+273.16)*(1000.0/P)**(RoCp) - 273.16
      return
      end 
C********** 
      function thetam(T,Theta)
C given temp and pot temp, both in deg C, calculate Theta M, in deg C 
C theta m is the moist adiabat through theta at level given by T
      thetam=theta-wobf(theta)+wobf(T)
      return
      end 
C ********
      function thetaw(Theta, T_con) 
C given pot temp and condensation temperature of a parcel, both in deg C, 
C calculate Theta W, wet bulb potential temp, in deg C
      thetaw=theta-wobf(theta)+wobf(T_con)
      return
      end 
C ************
      function thetae(T,P,Tc,Plcl)
C given T and condensation temperature in deg C and P and Plcl (p of Tc) in mB, 
C compute theta e by Boltons(1980, MWR 108, p 1053) approximation 
      r=wmrof(Plcl,Tc)
C that is mixing ratio at T = saturation mixing ratio at TC and Plcl
      thetae=(T+273.16)*(1000.0/P)**(0.2854*(1.0-0.00028*r))
      thetae=thetae*exp((3.376/(Tc+273.16)-0.00254)*r*(1+0.00081*r))
      thetae=thetae-273.16
      return
      end 
C ******* 
      subroutine LCL(T,Td,P,Tc,Plcl,Thm)
C given temp and dewpoint, in deg C, and P in mB, find
C the lifting condensation level Plcl(mB), convective 
C temperature Tc and moist adiabat Thm.. (in deg C) 
      rocp=0.28571428 
      exponent=-(1.0/rocp)
      Tpot=theta(T,P)+273.16 ! so ratio is correct
      Tc=tconof(T,Td) 
      Plcl=1000.0*(Tpot/(Tc+273.16))**exponent
      thm=wobf(Tc)+Tpot-273.16-wobf(Tpot-273.16)
cd    write(1,*)'for T and P',t,p,' Theta M is',thm 
      return
      end 
C ************
      subroutine lift_fc(thm,istart,press,temp,nlev,ilev,lfc,T_lfc) 
C given moist adiabat thm(deg C), and environmental sounding
C find level where lifted temperature is first warmer than
C environment.
      dimension press(nlev),temp(nlev)
      real thm,lfc,T_lfc
C     if(istart.lt.1)write(1,*)' Shit! istart=',istart
      do i=istart,nlev
        tlift=satlft(thm,press(i))
        if(tlift.gt.temp(i))then
              lfc=press(i)
              ilev=i
              T_lfc=tlift 
              go to 100 
        end if
      end do
      ilev=-99
      T_lfc=-99.0 
      lfc=-99.0 
100   return
      end 
C ******************
      subroutine APE_moist(P,hgt,T,nlev,istart,thm,iend,badval,area)
C compute energy area, considering a parcel lifted from P(istart) 
C along the moist adiabat specified by thm. T(nlev) and thm are 
C in deg C, press(nlev) is in mB. Returned variables are the ending 
C level (where slab area changes sign) and the area (in m**2/s**).
C badval is for bad temps 12 july 88 main should check if istart=iend 
C mean values are between p(iprev) and p(i), assuming p(iprev) > p(i).
      dimension P(nlev),hgt(nlev),T(nlev) 
C 
      grav=9.81 !m/s**2 
      area=0
      iend=istart 
      iprev=istart
      do i=istart+1,nlev
       if(T(i).ne.badval)then 
        delP=(P(iprev)-P(i))*0.5
        Tmean=(T(iprev)+T(i))*0.5 
        Tlift=satlft(thm,P(i) + delP) 
        slab=grav*(hgt(i)-hgt(iprev))*(Tlift-Tmean)/(Tmean+273.16)
C     write(1,*)'T,Tlift:',Tmean,Tlift,'CAPE slab:',slab,' at i =',i
        if(slab*area.lt.0)then ! sign change so stop integration
              iend=i
              return
        end if
        area=area+slab
        iprev = i 
       end if 
      end do
      return
      end 
C ******************
      subroutine APE_dry(P,hgt,T,nlev,istart,thd,iend,badT,area)
C compute energy area, considering a parcel lifted from press(istart) 
C along the dry adiabat specified by thm. Temp(nlev) and thm are
C in deg C, press(nlev) is in mB. Returned variables are the ending 
C level (where slab area changes sign) and the area (in m**2/s**).
C badT is bad temp, so do not use ... 12 july 88
      dimension P(nlev),hgt(nlev),T(nlev) 
C 
      grav=9.81 !m/s**2 
      rocp=0.28571428 
      area=0
      iend = istart 
      iprev = istart
      do i=istart+1,nlev
       if(T(i).ne.badT)then 
        delP=(P(iprev)-P(i))*0.5
        Tmean=(T(iprev)+T(i))*0.5+273.16
        Tlift=(thd+273.16)*((P(i) + delP)*0.001)**rocp
        slab=grav*(hgt(i)-hgt(iprev))*(Tlift-Tmean)/(Tmean) 
        if(slab*area.lt.0)then ! sign change so stop integration
              iend=i
              return
        end if
        area=area+slab
        iprev=i 
       end if 
      end do
      return
      end 
C *********************** 
      subroutine vAPE_moist(P,hgt,T,RH,nlev,istart,thm,iend,badT,area)
C compute energy area, considering a parcel lifted from P(istart) 
C along the moist adiabat specified by thm. T(nlev) and thm are 
C in deg C, press(nlev) is in mB. Returned variables are the ending 
C level (where slab area changes sign) and the area (in m**2/s**).
C version of APE_moist that uses virtual Temps. 
C badT is missing temp value..check istart=iend  for all bad
      dimension P(nlev),hgt(nlev),T(nlev),RH(nlev)
C 
      grav=9.81 !m/s**2 
      area=0.0
      iend=istart 
      iprev=istart
      do i=istart+1,nlev
       if(T(i).ne.badT)then 
        delP=(P(iprev)-P(i))*0.5
        Tmean=(T(iprev)+T(i))*0.5 
        pmean=(P(iprev)+P(i))*0.5
        Tlift=satlft(thm,P(i) + delP) ! lift to middle of layer 
        es=vapfw(Tlift) 
        Tvl=(Tlift+273.16)/(1.0-(es/pmean)*0.378) 
        if((Rh(i).le.100.).and.(rh(iprev).le.100))then
                e=(Rh(iprev)+Rh(i))*0.5*es*0.01 
                Tve=(Tmean+273.16)/(1.0-(e/pmean)*0.378)
        else
              Tve=tmean+273.16
        end if
        slab=grav*(hgt(i)-hgt(iprev))*(Tvl-Tve)/Tve 
C     write(1,*)'T,Tlift:',Tmean,Tlift,'CAPE slab:',slab,' at i =',i
        if(slab*area.lt.0)then ! sign change so stop integration
              iend=i
              return
        end if
        area=area+slab
        iprev=i 
       end if 
      end do
      return
      end 
C **********************
      subroutine prec_water(isfc, itop, nlev, press, temp, rh, w) 
C compute total q in terms of liquid water from press(isfc) 
C to itop, return as depth in cm
C calls subroutines in thermosubs, where this also resides. 
      parameter( grav = 9.81 )
      real w, press(nlev), rh(nlev), temp(nlev) ! mb, %, deg C. 
C 
      w = 0.0 
      del_press = ( press(itop-1) - press(itop) ) * 100.0 ! pascals 
      do i = isfc, itop 
         if(rh(i).ge.0.0)then 
                  es = vapfw(temp(i)) ! sat vap press in mb 
                  e = rh(i) * 0.01 * es    ! vap press
                  q = 0.622 * e / (press(i) -0.378*e) 
                  w = w + del_press * q / grav
         end if 
      end do
      w = w * 1.0e-3 ! (kg/m2) / (1.0e3 kg/m3) for depth
      w = w * 100 ! m to cm 
      return
      end 













