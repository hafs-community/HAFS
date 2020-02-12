#if defined(ROW_LAND)
#define SEA_P .true.
#define SEA_U .true.
#define SEA_V .true.
#define SEA_Q .true.
#elif defined(ROW_ALLSEA)
#define SEA_P allip(j).or.ip(i,j).ne.0
#define SEA_U alliu(j).or.iu(i,j).ne.0
#define SEA_V alliv(j).or.iv(i,j).ne.0
#define SEA_Q alliq(j).or.iq(i,j).ne.0
#else
#define SEA_P ip(i,j).ne.0
#define SEA_U iu(i,j).ne.0
#define SEA_V iv(i,j).ne.0
#define SEA_Q iq(i,j).ne.0
#endif
      module mod_momtum
      use mod_xc    ! HYCOM communication interface
      implicit none
c
c --- module for momtum and related routines
c
      private !! default is private
      public  :: momtum_hs, momtum, momtum4
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     &  stress,stresx,stresy,dpmx,thkbop,oneta,onetai

      contains

      subroutine momtum_hs(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_pipe       ! HYCOM debugging interface
      use mod_tides      ! HYCOM tides
#if defined(STOKES)
      use mod_stokes     ! HYCOM Stokes drift
#endif
      implicit none
c
      integer m,n
c
c --- -----------------------------------------
c --- hydrostatic equation (and surface stress)
c --- -----------------------------------------
c
      logical, parameter :: lpipe_momtum=.false.      !usually .false.


      real,    parameter :: dragw_rho=0.01072d0*1026.0d0  !ice-ocean drag from CICE
      real,    parameter ::     pairc=1013.0d0*100.0d0    !air pressure (Pa)
      real,    parameter ::      rgas=287.1d0           !gas constant (j/kg/k)
      real,    parameter ::     tzero=273.16d0          !celsius to kelvin offset
c
      character text*12
c
      real    dpdn,dpup,q,samo,simo,uimo,vimo,dpsur,psur,usur,vsur,
     &        airt,vpmx,wndx,wndy,wind,cdw,pair,rair,
     &        sumdp,sumth,thksur
      integer i,j,k,kl,l,margin,mbdy,hlstep
c     &,iffstep
c	  data iffstep/0/
c      save iffstep
c
*     real*8    wtime
*     external  wtime
*     real*8    wtime1(10),wtime2(20,kdm),wtimes
c
      include 'stmt_fns.h'
c
#if defined(RELO)
      if     (.not.allocated(stress)) then
        allocate(
     &          stress(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          stresx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          stresy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            dpmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          thkbop(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           oneta(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          onetai(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 7*(idm+2*nbdy)*(jdm+2*nbdy) )
                stress = r_init
                stresx = r_init
                stresy = r_init
                  dpmx = r_init
                thkbop = r_init
                 oneta = r_init
                onetai = r_init
      endif
#endif
c
      mbdy = 6
c
c --- dp has up to date halos from cnuity or mod_hycom
      call xctilr(dpmixl(1-nbdy,1-nbdy,  m),1,   1, 6,6, halo_ps)
      call xctilr(pbavg( 1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_ps)
      call xctilr(saln(  1-nbdy,1-nbdy,1,m),1,  kk, 6,6, halo_ps)
      call xctilr(temp(  1-nbdy,1-nbdy,1,m),1,  kk, 6,6, halo_ps)
      call xctilr(th3d(  1-nbdy,1-nbdy,1,m),1,  kk, 6,6, halo_ps)
      if     (windf .and. iceflg.eq.2) then
        kl=max(nsigma,1)
        call xctilr(u(      1-nbdy,1-nbdy,1,n),1,kl, 1,1, halo_uv)
        call xctilr(ubavg(  1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_uv)
        call xctilr(v(      1-nbdy,1-nbdy,1,n),1,kl, 1,1, halo_vv)
        call xctilr(vbavg(  1-nbdy,1-nbdy,  n),1, 1, 1,1, halo_vv)
      endif
c
c --- tidal forcing
c
      if(tidflg.eq.2 .or. tidflg.eq.3) then
         hlstep=0
         call tides_force(hlstep)
      endif
c
c --- hydrostatic equation (and surface stress)
c
*        wtime1( 1) = wtime()
c
c --- rhs: th3d.m, temp.m, saln.m, p, pbavg.m
c --- lhs: thstar, p, oneta, montg
c
      margin = mbdy
c
!$OMP PARALLEL DO PRIVATE(j,k,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            sumdp = 0.0
            sumth = 0.0
            do k=1,kk
              if (kapref.ne.0) then  !thermobaric
c
c ---   sigma-star is virtual potential density, as defined in 
c ---   Sun et.al. (1999), 'Inclusion of thermobaricity in 
c ---   isopycnic-coordinate ocean models', JPO 29 pp 2719-2729.
c
c ---   use upper interface pressure in converting sigma to sigma-star.
c ---   to avoid density variations in layers intersected by bottom
c
                if     (kapref.gt.0) then
                  thstar(i,j,k,1)=th3d(i,j,k,m)
     &                             +kappaf(temp(i,j,k,m),
     &                                     saln(i,j,k,m),
     &                                     th3d(i,j,k,m)+thbase,
     &                                        p(i,j,k),
     &                                     kapref)
                else
                  thstar(i,j,k,1)=th3d(i,j,k,m)
     &                             +kappaf(temp(i,j,k,m),
     &                                     saln(i,j,k,m),
     &                                     th3d(i,j,k,m)+thbase,
     &                                        p(i,j,k),
     &                                     2)
                  thstar(i,j,k,2)=th3d(i,j,k,m)
     &                             +kappaf(temp(i,j,k,m),
     &                                     saln(i,j,k,m),
     &                                     th3d(i,j,k,m)+thbase,
     &                                        p(i,j,k),
     &                                     kapi(i,j))
                endif !kapref
              else  !non-thermobaric
                thstar(i,j,k,1)=th3d(i,j,k,m)
              endif  !thermobaric:else
c
              p(i,j,k+1)=p(i,j,k)+dp(i,j,k,m)
c
              if     (sshflg.ne.0) then
                sumth = sumth + dp(i,j,k,m)*th3d(i,j,k,m)
                sumdp = sumdp + dp(i,j,k,m)
              endif !sshflg
            enddo !k
c
            if     (sshflg.ne.0) then
              sumth = sumth / max( sumdp, onemm )  !vertical mean of th3d
              sumdp = sumdp*qonem * g              !depth(m) * g
              steric(i,j) =  sshgmn(i,j) +
     &                      (sshgmn(i,j) + sumdp) *
     &                      (thmean(i,j) - sumth) /
     &                      (1000.0+thbase+sumth)
            endif !sshflg
c
c ---       store (1+eta) (= p_total/p_prime) in -oneta-
            oneta(i,j)=1.+pbavg(i,j,m)/p(i,j,kk+1)
            onetai(i,j)=oneta(i,j)-pbavg(i,j,m)/pbot(i,j)
c
c ---       m_prime in lowest layer:
            montg(i,j,kk,1)=psikk(i,j,1)+
     &        ( p(i,j,kk+1)*(thkk(i,j,1)-thstar(i,j,kk,1))
     &          -pbavg(i,j,m)*thstar(i,j,kk,1) )*thref**2
            if     (kapref.eq.-1) then
              montg(i,j,kk,2)=psikk(i,j,2)+
     &          ( p(i,j,kk+1)*(thkk(i,j,2)-thstar(i,j,kk,2))
     &            -pbavg(i,j,m)*thstar(i,j,kk,2) )*thref**2
            endif !kapref.eq.-1
c
c ---       m_prime in remaining layers:
            do k=kk-1,1,-1
              montg(i,j,k,1)=montg(i,j,k+1,1)+p(i,j,k+1)*oneta(i,j)
     &            *(thstar(i,j,k+1,1)-thstar(i,j,k,1))*thref**2
              if     (kapref.eq.-1) then
                montg(i,j,k,2)=montg(i,j,k+1,2)+p(i,j,k+1)*oneta(i,j)
     &              *(thstar(i,j,k+1,2)-thstar(i,j,k,2))*thref**2
              endif !kapref.eq.-1
            enddo !k
c
c ---       srfhgt (used diagnostically, in mxmyaij and for tidal SAL).
            if     (kapref.ne.-1) then
              montg1(i,j) =                 montg(i,j,1,1)
            else
              montg1(i,j) =      skap(i,j) *montg(i,j,1,1) +
     &                      (1.0-skap(i,j))*montg(i,j,1,2)
            endif !kapref
            srfhgt(i,j) = montg1(i,j) + thref*pbavg(i,j,m)
c
cdiag       if     (sshflg.ne.0) then
cdiag              if     (itest.gt.0 .and. jtest.gt.0) then
cdiag                write (lp,'(i9,2i5,3x,a,2f12.6,f12.2)')
cdiag     .            nstep,itest+i0,jtest+j0,
cdiag     .              'momtum-l224: ssh =',
cdiag     .              steric(i,j),sshgmn(i,j),sumdp
cdiag           write (lp,'(i9,2i5,3x,a,3f12.6)')
cdiag.            nstep,itest+i0,jtest+j0,
cdiag.              'thmn =',
cdiag.              sumth,thmean(i,j),1000.0+thbase+sumth
cdiag           write (lp,'(i9,2i5,3x,a,3f12.6)')
cdiag.            nstep,itest+i0,jtest+j0,
cdiag.              'ssh  =',
cdiag.              srfhgt(i,j),steric(i,j),srfhgt(i,j)-steric(i,j)
cdiag         endif !test
cdiag            endif !sshflg
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_momtum .and. mslprf) then
c ---   compare two model runs.
        util4(1:ii,1:jj) =  mslprs(1:ii,1:jj,l0)
        write (text,'(a9,i3)') 'mslprs l=',l0
        call pipe_compare_sym1(util4, ip,text)
        util4(1:ii,1:jj) =  mslprs(1:ii,1:jj,l1)
        write (text,'(a9,i3)') 'mslprs l=',l1
        call pipe_compare_sym1(util4, ip,text)
c
        util4(1:ii,1:jj) =  mslprs(1:ii,1:jj,l0)-mslprs(1:ii,0:jj-1,l0)
        write (text,'(a9,i3)') 'mslprY l=',l0
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) =  mslprs(1:ii,1:jj,l1)-mslprs(1:ii,0:jj-1,l1)
        write (text,'(a9,i3)') 'mslprY l=',l1
        call pipe_compare_sym1(util4, iv,text)
c
        util4(1:ii,1:jj) =  mslprs(1:ii,1:jj,l0)-mslprs(0:ii-1,1:jj,l0)
        write (text,'(a9,i3)') 'mslprX l=',l0
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) =  mslprs(1:ii,1:jj,l1)-mslprs(0:ii-1,1:jj,l1)
        write (text,'(a9,i3)') 'mslprX l=',l1
        call pipe_compare_sym1(util4, iu,text)
      endif
c
c      call dpudpv(dpu(1-nbdy,1-nbdy,1,m),
c     &            dpv(1-nbdy,1-nbdy,1,m),
c     &            p,depthu,depthv, max(0,margin-1))
cCL/RB MPI bug correction 2011-01
c      call xctilr(dpu(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_us)
c      call xctilr(dpv(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vs)

c
c --- account for temporal smoothing of mid-time dpmixl. calculate the vertical
c --- excursions of the coordinates immediately above and below the mixed
c --- layer base, then vertically interpolate this motion to dpmixl(i,j,m)
c
      if(hybrid .and. mxlkta) then
c
c ---   rhs: dp, dpmixl.m
c ---   lhs: util1, util2
c
        margin = mbdy
c
!$OMP   PARALLEL DO PRIVATE(j,i,k,dpup,dpdn,q)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
              util1(i,j)=0.0
              util2(i,j)=0.0
              do k=1,kk
                util1(i,j)=util2(i,j)
                util2(i,j)=util2(i,j)+dpo(i,j,k,m)
                if     (util2(i,j).ge.dpmixl(i,j,m).and.
     &                  util1(i,j).lt.dpmixl(i,j,m)     ) then
                  dpup=p(i,j,k  )-util1(i,j)
                  dpdn=p(i,j,k+1)-util2(i,j)
                  q=(util2(i,j)-dpmixl(i,j,m))/max(onemm,dpo(i,j,k,m))
                  dpmixl(i,j,m)=dpmixl(i,j,m)+(dpdn+q*(dpup-dpdn))
                endif
              enddo !k
            endif !ip
          enddo !l
        enddo !i
!$OMP   END PARALLEL DO
      endif
c
c --- --------------
c --- surface stress
c --- --------------
c
      if     (windf) then
        margin =0
c
!$OMP   PARALLEL DO PRIVATE(j,i,k,
!$OMP&                      dpsur,psur,usur,vsur,thksur,
!$OMP&                      airt,vpmx,wndx,wndy,wind,cdw,rair,
!$OMP&                      uimo,vimo,simo,samo)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_P) then
              if     (wndflg.eq.4 .or. wndflg.eq.5 .or.
     &                (iceflg.eq.2 .and. si_c(i,j).gt.0.0)) then
c ---           average currents over top thkcdw meters
                thksur = onem*min( thkcdw, depths(i,j) )
                usur   = 0.0
                vsur   = 0.0
                psur   = 0.0
                do k= 1,kk
                  dpsur = min( dpo(i,j,k,n), max( 0.0, thksur-psur ) )
                  usur  = usur + dpsur*(u(i,j,k,n)+u(i+1,j,k,n))
                  vsur  = vsur + dpsur*(v(i,j,k,n)+v(i,j+1,k,n))
                  psur  = psur + dpsur
                  if     (psur.ge.thksur) then
                    exit
                  endif
                enddo !k
                usur  = 0.5*( usur/psur + ubavg(i,  j,n) +
     &                                    ubavg(i+1,j,n)  )
                
                vsur  = 0.5*( vsur/psur + vbavg(i,j,  n) +
     &                                    vbavg(i,j+1,n)  )
              endif !usur,vsur
c
c<-hsk NCEP-coupler
# if defined(USE_SCOUPLER) 
                if(taux_(i,j).ne.cflag)taux(i,j,:)=taux_(i,j)
                if(tauy_(i,j).ne.cflag)tauy(i,j,:)=tauy_(i,j)
#endif
              
              if     (wndflg.eq.2 .or. wndflg.eq.3) then ! tau on p grid
                if     (natm.eq.2) then
                  surtx(i,j) =   taux(i,j,l0)*w0+taux(i,j,l1)*w1
                  surty(i,j) =   tauy(i,j,l0)*w0+tauy(i,j,l1)*w1
                else
                  surtx(i,j) =   taux(i,j,l0)*w0+taux(i,j,l1)*w1
     &                          +taux(i,j,l2)*w2+taux(i,j,l3)*w3 
                  surty(i,j) =   tauy(i,j,l0)*w0+tauy(i,j,l1)*w1
     &                          +tauy(i,j,l2)*w2+tauy(i,j,l3)*w3 
                endif !natm
              elseif (wndflg.eq.1) then ! tau on u&v grids - NOT RECOMMEDED
                if     (natm.eq.2) then
                  surtx(i,j) = ( (taux(i,j,l0)+taux(i+1,j,l0))*w0
     &                          +(taux(i,j,l1)+taux(i+1,j,l1))*w1)*0.5
                  surty(i,j) = ( (tauy(i,j,l0)+tauy(i,j+1,l0))*w0
     &                          +(tauy(i,j,l1)+tauy(i,j+1,l1))*w1)*0.5
                else
                  surtx(i,j) = ( (taux(i,j,l0)+taux(i+1,j,l0))*w0
     &                          +(taux(i,j,l1)+taux(i+1,j,l1))*w1
     &                          +(taux(i,j,l2)+taux(i+1,j,l2))*w2
     &                          +(taux(i,j,l3)+taux(i+1,j,l3))*w3)*0.5
                  surty(i,j) = ( (tauy(i,j,l0)+tauy(i,j+1,l0))*w0
     &                          +(tauy(i,j,l1)+tauy(i,j+1,l1))*w1
     &                          +(tauy(i,j,l2)+tauy(i,j+1,l2))*w2
     &                          +(tauy(i,j,l3)+tauy(i,j+1,l3))*w3)*0.5
                endif !natm
              else !wndflg.eq.4,5,6
c ---           calculate stress from 10m winds using cd_coare or cd_core2
                if     (natm.eq.2) then
                  airt = airtmp(i,j,l0)*w0+airtmp(i,j,l1)*w1
                  vpmx = vapmix(i,j,l0)*w0+vapmix(i,j,l1)*w1
                  wndx =   taux(i,j,l0)*w0+  taux(i,j,l1)*w1
                  wndy =   tauy(i,j,l0)*w0+  tauy(i,j,l1)*w1
                else
                  airt = airtmp(i,j,l0)*w0+airtmp(i,j,l1)*w1
     &                  +airtmp(i,j,l2)*w2+airtmp(i,j,l3)*w3
                  vpmx = vapmix(i,j,l0)*w0+vapmix(i,j,l1)*w1
     &                  +vapmix(i,j,l2)*w2+vapmix(i,j,l3)*w3
                  wndx =   taux(i,j,l0)*w0+  taux(i,j,l1)*w1
     &                    +taux(i,j,l2)*w2+  taux(i,j,l3)*w3
                  wndy =   tauy(i,j,l0)*w0+  tauy(i,j,l1)*w1
     &                    +tauy(i,j,l2)*w2+  tauy(i,j,l3)*w3
                endif !natm
                wind = sqrt( wndx**2 + wndy**2 )
                if     (mslprf .or. flxflg.eq.6) then
                  if     (natm.eq.2) then
                    pair = mslprs(i,j,l0)*w0+mslprs(i,j,l1)*w1
     &                    +prsbas
                  else
                    pair = mslprs(i,j,l0)*w0+mslprs(i,j,l1)*w1
     &                    +mslprs(i,j,l2)*w2+mslprs(i,j,l3)*w3
     &                    +prsbas
                  endif !natm
c<-hsk NCEP-coupler
#if defined(USE_SCOUPLER)
                if(mslprs_(i,j).ne.cflag)pair=mslprs_(i,j)+prsbas
#endif
c-> hsk
                else
                  pair = pairc
                endif
                if     (flxflg.eq.6) then !use virtual temperature
                  rair = pair / (rgas * (tzero+airt) * (1.0+0.608*vpmx))
                else
                  rair = pair / (rgas * (tzero+airt))
                endif
c
                if     (wndflg.eq.4 .and. flxflg.eq.6) then
c ---             use wind-current in place of wind for everything
                  samo  = sqrt( (wndx-usur)**2 + (wndy-vsur)**2 )
                  cdw   = 1.0e-3*cd_coarep(samo,vpmx,airt,pair,
     &                                     temp(i,j,1,n))
                  surtx( i,j) = rair*cdw*samo*(wndx-usur)
                  surty( i,j) = rair*cdw*samo*(wndy-vsur)
                  wndocn(i,j) = samo  !save for thermf
                elseif (wndflg.eq.4) then
                  cdw  = 1.0e-3*cd_coare(wind,vpmx,airt,
     &                                    temp(i,j,1,n))
c ---             use wind-current magnitude and direction for stress 
                  samo = sqrt( (wndx-usur)**2 + (wndy-vsur)**2 )
                  surtx(i,j) = rair*cdw*samo*(wndx-usur)
                  surty(i,j) = rair*cdw*samo*(wndy-vsur)
                else  ! wndflg.eq.5
                  cdw  = 1.0e-3*cd_core2(wind,vpmx,airt,
     &                                    temp(i,j,1,n))
c ---             use wind-current magnitude and direction for stress 
                  samo = sqrt( (wndx-usur)**2 + (wndy-vsur)**2 )
                  surtx(i,j) = rair*cdw*samo*(wndx-usur)
                  surty(i,j) = rair*cdw*samo*(wndy-vsur)
                endif
              endif !wndflg
c
              surtx(i,j) = surtx(i,j) + oftaux(i,j)
              surty(i,j) = surty(i,j) + oftauy(i,j)
c
              if     (ishlf(i,j).eq.0) then !under an ice shelf
                surtx(i,j) = 0.0
                surty(i,j) = 0.0
              elseif (iceflg.eq.2 .and. si_c(i,j).gt.0.0) then
c ---           allow for ice-ocean stress
                uimo = si_u(i,j) - usur
                vimo = si_v(i,j) - vsur
                simo = sqrt( uimo**2 + vimo**2 )
                surtx(i,j) = (1.0-si_c(i,j))*surtx(i,j) +
     &                            si_c(i,j) *dragw_rho*simo*uimo
                surty(i,j) = (1.0-si_c(i,j))*surty(i,j) +
     &                            si_c(i,j) *dragw_rho*simo*vimo
              endif !ice-ocean stress
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        call xctilr(surtx,1,1, 6,6, halo_pv)
        call xctilr(surty,1,1, 6,6, halo_pv)
      endif !windf
c
      return
      end subroutine momtum_hs
c
      real function cd_coare(wind,vpmx,airt,sst)
      implicit none
c
      real    wind,vpmx,airt,sst
c
c --- Wind stress drag coefficient * 10^3 from an approximation
c --- to the COARE 3.0 bulk algorithm (Fairall et al. 2003).
c
c --- wind = wind speed (m/s)
c --- vpmx = water vapor mixing ratio (kg/kg)
c --- airt = air temperature (C)
c --- sst  = sea temperature (C)
c
c ---              Ta-Ts
c ---           ==============
c ---   STABLE:  7    to  0.75 degC
c ---  NEUTRAL:  0.75 to -0.75 degC
c --- UNSTABLE: -0.75 to -8    degC
c
c ---              Va
c ---           ==============
c ---   Low:     1    to   5   m/s
c ---   High:    5    to  34   m/s
c
c --- vamax of 34 m/s from Sraj et al, 2013 (MWR-D-12-00228.1).
c
      real    tamts,q,qva,va
c
      real, parameter :: vamin=  1.0,  vamax=34.0
      real, parameter :: tdmin= -8.0,  tdmax= 7.0
      real, parameter :: tzero=273.16
c
      real, parameter ::
     &  as0_00=-0.06695,   as0_10= 0.09966,  as0_20=-0.02477,
     &  as0_01= 0.3133,    as0_11=-2.116,    as0_21= 0.2726,
     &  as0_02=-0.001473,  as0_12= 4.626,    as0_22=-0.5558,
     &  as0_03=-0.004056,  as0_13=-2.680,    as0_23= 0.3139

      real, parameter ::
     &  as5_00= 0.55815,   as5_10=-0.005593, as5_20= 0.0006024,
     &  as5_01= 0.08174,   as5_11= 0.2096,   as5_21=-0.02629,
     &  as5_02=-0.0004472, as5_12=-8.634,    as5_22= 0.2121,
     &  as5_03= 2.666e-6,  as5_13= 18.63,    as5_23= 0.7755

      real, parameter ::
     &  au0_00= 1.891,     au0_10=-0.006304, au0_20= 0.0004406,
     &  au0_01=-0.7182,    au0_11=-0.3028,   au0_21=-0.01769,
     &  au0_02= 0.1975,    au0_12= 0.3120,   au0_22= 0.01303,
     &  au0_03=-0.01790,   au0_13=-0.1210,   au0_23=-0.003394

      real, parameter ::
     &  au5_00= 0.6497,    au5_10= 0.003827, au5_20=-4.83e-5,
     &  au5_01= 0.06993,   au5_11=-0.2756,   au5_21= 0.007710,
     &  au5_02= 3.541e-5,  au5_12=-1.091,    au5_22=-0.2555,
     &  au5_03=-3.428e-6,  au5_13= 4.946,    au5_23= 0.7654

      real, parameter ::
     &  an0_00= 1.057,     an5_00= 0.6825,
     &  an0_01=-0.06949,   an5_01= 0.06945,
     &  an0_02= 0.01271,   an5_02=-0.0001029

      real, parameter ::
     &  ap0_10= as0_00 + as0_10*0.75 + as0_20*0.75**2,
     &  ap0_11=          as0_11*0.75 + as0_21*0.75**2,
     &  ap0_12=          as0_12*0.75 + as0_22*0.75**2,
     &  ap0_13=          as0_13*0.75 + as0_23*0.75**2

      real, parameter ::
     &  ap5_10= as5_00 + as5_10*0.75 + as5_20*0.75**2,
     &  ap5_11=          as5_11*0.75 + as5_21*0.75**2,
     &  ap5_12=          as5_12*0.75 + as5_22*0.75**2,
     &  ap5_13=          as5_13*0.75 + as5_23*0.75**2

      real, parameter ::
     &  am0_10= au0_00 - au0_10*0.75 + au0_20*0.75**2,
     &  am0_11=        - au0_11*0.75 + au0_21*0.75**2,
     &  am0_12=        - au0_12*0.75 + au0_22*0.75**2,
     &  am0_13=        - au0_13*0.75 + au0_23*0.75**2

      real, parameter ::
     &  am5_10= au5_00 - au5_10*0.75 + au5_20*0.75**2,
     &  am5_11=        - au5_11*0.75 + au5_21*0.75**2,
     &  am5_12=        - au5_12*0.75 + au5_22*0.75**2,
     &  am5_13=        - au5_13*0.75 + au5_23*0.75**2
c
c --- saturation specific humidity (lowe, j.appl.met., 16, 100-103, 1976)
      real qsatur,t
      qsatur(t)=.622e-3*(6.107799961e+00+t*(4.436518521e-01
     &               +t*(1.428945805e-02+t*(2.650648471e-04
     &               +t*(3.031240396e-06+t*(2.034080948e-08
     &               +t* 6.136820929e-11))))))
c
c ---     correct tamts to 100% humidity
          tamts = airt-sst - 0.61*(airt+tzero)*(qsatur(airt)-vpmx)
          tamts = min( tdmax, max( tdmin, tamts ) )
          va    = max( vamin, min( vamax, wind  ) )
          qva   = 1.0/va
          if     (va.le.5.0) then
            if     (tamts.ge. 0.75) then
              cd_coare = 
     &           (as0_00 + as0_01* va + as0_02* va**2 + as0_03* va**3)
     &         + (as0_10 + as0_11*qva + as0_12*qva**2 + as0_13*qva**3)
     &           *tamts
     &         + (as0_20 + as0_21*qva + as0_22*qva**2 + as0_23*qva**3)
     &           *tamts**2 
            elseif (tamts.le.-0.75) then
              cd_coare = 
     &           (au0_00 + au0_01* va + au0_02* va**2 + au0_03* va**3)
     &         + (au0_10 + au0_11*qva + au0_12*qva**2 + au0_13*qva**3)
     &           *tamts
     &         + (au0_20 + au0_21*qva + au0_22*qva**2 + au0_23*qva**3)
     &           *tamts**2 
            elseif (tamts.ge. -0.098)  then
              q =  (tamts+0.098)/0.848  !linear between  0.75 and -0.098
              cd_coare = q*
     &        (  (         as0_01* va + as0_02* va**2 + as0_03* va**3)
     &         + (ap0_10 + ap0_11*qva + ap0_12*qva**2 + ap0_13*qva**3)
     &        ) + (1.0-q)*
     &           (an0_00 + an0_01* va + an0_02* va**2)
            else
              q = (-tamts-0.098)/0.652  !linear between -0.75 and -0.098
              cd_coare = q*
     &        (  (         au0_01* va + au0_02* va**2 + au0_03* va**3)
     &         + (am0_10 + am0_11*qva + am0_12*qva**2 + am0_13*qva**3)
     &        ) + (1.0-q)*
     &           (an0_00 + an0_01* va + an0_02* va**2)
            endif !tamts
          else !va>5
            if     (tamts.ge. 0.75) then
              cd_coare = 
     &           (as5_00 + as5_01* va + as5_02* va**2 + as5_03* va**3)
     &         + (as5_10 + as5_11*qva + as5_12*qva**2 + as5_13*qva**3)
     &           *tamts
     &         + (as5_20 + as5_21*qva + as5_22*qva**2 + as5_23*qva**3)
     &           *tamts**2 
            elseif (tamts.le.-0.75) then
              cd_coare = 
     &           (au5_00 + au5_01* va + au5_02* va**2 + au5_03* va**3)
     &         + (au5_10 + au5_11*qva + au5_12*qva**2 + au5_13*qva**3)
     &           *tamts
     &         + (au5_20 + au5_21*qva + au5_22*qva**2 + au5_23*qva**3)
     &           *tamts**2 
            elseif (tamts.ge. -0.098)  then
              q =  (tamts+0.098)/0.848  !linear between  0.75 and -0.098
              cd_coare = q*
     &        (  (         as5_01* va + as5_02* va**2 + as5_03* va**3)
     &         + (ap5_10 + ap5_11*qva + ap5_12*qva**2 + ap5_13*qva**3)
     &        ) + (1.0-q)*
     &           (an5_00 + an5_01* va + an5_02* va**2)
            else
              q = (-tamts-0.098)/0.652  !linear between -0.75 and -0.098
              cd_coare = q*
     &        (  (         au5_01* va + au5_02* va**2 + au5_03* va**3)
     &         + (am5_10 + am5_11*qva + am5_12*qva**2 + am5_13*qva**3)
     &        ) + (1.0-q)*
     &           (an5_00 + an5_01* va + an5_02* va**2)
            endif !tamts
          endif !va
c
      end function cd_coare
c
      real function cd_coarep(samo,vpmx,airt,pair,sst)
      implicit none
c
      real    samo,vpmx,airt,pair,sst
c
c --- Wind stress drag coefficient * 10^3 from an approximation
c --- to the COARE 3.0 bulk algorithm (Fairall et al. 2003).
c
c --- samo = wind-ocean speed (m/s)
c --- vpmx = water vapor mixing ratio (kg/kg)
c --- airt = air temperature (C)
c --- pair = air pressure (Pa)
c --- sst  = sea temperature (C)
c
c ---              Ta-Ts
c ---           ==============
c ---   STABLE:  7    to  0.75 degC
c ---  NEUTRAL:  0.75 to -0.75 degC
c --- UNSTABLE: -0.75 to -8    degC
c
c ---              Va
c ---           ==============
c ---   Low:     1    to   5   m/s
c ---   High:    5    to  34   m/s
c
c --- vamax of 34 m/s from Sraj et al, 2013 (MWR-D-12-00228.1).
c
      real    tamts,q,qva,va
c
      real, parameter :: vamin=  1.0,  vamax=34.0
      real, parameter :: tdmin= -8.0,  tdmax= 7.0
      real, parameter :: tzero=273.16
c
      real, parameter ::
     &  as0_00=-0.06695,   as0_10= 0.09966,  as0_20=-0.02477,
     &  as0_01= 0.3133,    as0_11=-2.116,    as0_21= 0.2726,
     &  as0_02=-0.001473,  as0_12= 4.626,    as0_22=-0.5558,
     &  as0_03=-0.004056,  as0_13=-2.680,    as0_23= 0.3139

      real, parameter ::
     &  as5_00= 0.55815,   as5_10=-0.005593, as5_20= 0.0006024,
     &  as5_01= 0.08174,   as5_11= 0.2096,   as5_21=-0.02629,
     &  as5_02=-0.0004472, as5_12=-8.634,    as5_22= 0.2121,
     &  as5_03= 2.666e-6,  as5_13= 18.63,    as5_23= 0.7755

      real, parameter ::
     &  au0_00= 1.891,     au0_10=-0.006304, au0_20= 0.0004406,
     &  au0_01=-0.7182,    au0_11=-0.3028,   au0_21=-0.01769,
     &  au0_02= 0.1975,    au0_12= 0.3120,   au0_22= 0.01303,
     &  au0_03=-0.01790,   au0_13=-0.1210,   au0_23=-0.003394

      real, parameter ::
     &  au5_00= 0.6497,    au5_10= 0.003827, au5_20=-4.83e-5,
     &  au5_01= 0.06993,   au5_11=-0.2756,   au5_21= 0.007710,
     &  au5_02= 3.541e-5,  au5_12=-1.091,    au5_22=-0.2555,
     &  au5_03=-3.428e-6,  au5_13= 4.946,    au5_23= 0.7654

      real, parameter ::
     &  an0_00= 1.057,     an5_00= 0.6825,
     &  an0_01=-0.06949,   an5_01= 0.06945,
     &  an0_02= 0.01271,   an5_02=-0.0001029

      real, parameter ::
     &  ap0_10= as0_00 + as0_10*0.75 + as0_20*0.75**2,
     &  ap0_11=          as0_11*0.75 + as0_21*0.75**2,
     &  ap0_12=          as0_12*0.75 + as0_22*0.75**2,
     &  ap0_13=          as0_13*0.75 + as0_23*0.75**2

      real, parameter ::
     &  ap5_10= as5_00 + as5_10*0.75 + as5_20*0.75**2,
     &  ap5_11=          as5_11*0.75 + as5_21*0.75**2,
     &  ap5_12=          as5_12*0.75 + as5_22*0.75**2,
     &  ap5_13=          as5_13*0.75 + as5_23*0.75**2

      real, parameter ::
     &  am0_10= au0_00 - au0_10*0.75 + au0_20*0.75**2,
     &  am0_11=        - au0_11*0.75 + au0_21*0.75**2,
     &  am0_12=        - au0_12*0.75 + au0_22*0.75**2,
     &  am0_13=        - au0_13*0.75 + au0_23*0.75**2

      real, parameter ::
     &  am5_10= au5_00 - au5_10*0.75 + au5_20*0.75**2,
     &  am5_11=        - au5_11*0.75 + au5_21*0.75**2,
     &  am5_12=        - au5_12*0.75 + au5_22*0.75**2,
     &  am5_13=        - au5_13*0.75 + au5_23*0.75**2
c
      real satvpr,qsaturp,t,t6,p6
c
c --- saturation vapor pressure (Pa),
c --- from a polynominal approximation (lowe, j.appl.met., 16, 100-103, 1976)
      satvpr(t)=  100.0*(6.107799961e+00+t*(4.436518521e-01
     &               +t*(1.428945805e-02+t*(2.650648471e-04
     &               +t*(3.031240396e-06+t*(2.034080948e-08
     &               +t* 6.136820929e-11))))))
c
c --- pressure dependent saturation mixing ratio (kg/kg)
c --- p6 is pressure in Pa
      qsaturp(t6,p6)=0.622*(satvpr(t6)/(p6-satvpr(t6)))
c
c ---     correct tamts to 100% humidity
          tamts = airt-sst -
     &            0.608*(airt+tzero)*(qsaturp(airt,pair)-vpmx)
          tamts = min( tdmax, max( tdmin, tamts ) )
          va    = max( vamin, min( vamax, samo  ) )
          qva   = 1.0/va
          if     (va.le.5.0) then
            if     (tamts.ge. 0.75) then
              cd_coarep =
     &           (as0_00 + as0_01* va + as0_02* va**2 + as0_03* va**3)
     &         + (as0_10 + as0_11*qva + as0_12*qva**2 + as0_13*qva**3)
     &           *tamts
     &         + (as0_20 + as0_21*qva + as0_22*qva**2 + as0_23*qva**3)
     &           *tamts**2 
            elseif (tamts.le.-0.75) then
              cd_coarep =
     &           (au0_00 + au0_01* va + au0_02* va**2 + au0_03* va**3)
     &         + (au0_10 + au0_11*qva + au0_12*qva**2 + au0_13*qva**3)
     &           *tamts
     &         + (au0_20 + au0_21*qva + au0_22*qva**2 + au0_23*qva**3)
     &           *tamts**2 
            elseif (tamts.ge. -0.098)  then
              q =  (tamts+0.098)/0.848  !linear between  0.75 and -0.098
              cd_coarep = q*
     &        (  (         as0_01* va + as0_02* va**2 + as0_03* va**3)
     &         + (ap0_10 + ap0_11*qva + ap0_12*qva**2 + ap0_13*qva**3)
     &        ) + (1.0-q)*
     &           (an0_00 + an0_01* va + an0_02* va**2)
            else
              q = (-tamts-0.098)/0.652  !linear between -0.75 and -0.098
              cd_coarep = q*
     &        (  (         au0_01* va + au0_02* va**2 + au0_03* va**3)
     &         + (am0_10 + am0_11*qva + am0_12*qva**2 + am0_13*qva**3)
     &        ) + (1.0-q)*
     &           (an0_00 + an0_01* va + an0_02* va**2)
            endif !tamts
          else !va>5
            if     (tamts.ge. 0.75) then
              cd_coarep = 
     &           (as5_00 + as5_01* va + as5_02* va**2 + as5_03* va**3)
     &         + (as5_10 + as5_11*qva + as5_12*qva**2 + as5_13*qva**3)
     &           *tamts
     &         + (as5_20 + as5_21*qva + as5_22*qva**2 + as5_23*qva**3)
     &           *tamts**2 
            elseif (tamts.le.-0.75) then
              cd_coarep = 
     &           (au5_00 + au5_01* va + au5_02* va**2 + au5_03* va**3)
     &         + (au5_10 + au5_11*qva + au5_12*qva**2 + au5_13*qva**3)
     &           *tamts
     &         + (au5_20 + au5_21*qva + au5_22*qva**2 + au5_23*qva**3)
     &           *tamts**2 
            elseif (tamts.ge. -0.098)  then
              q =  (tamts+0.098)/0.848  !linear between  0.75 and -0.098
              cd_coarep = q*
     &        (  (         as5_01* va + as5_02* va**2 + as5_03* va**3)
     &         + (ap5_10 + ap5_11*qva + ap5_12*qva**2 + ap5_13*qva**3)
     &        ) + (1.0-q)*
     &           (an5_00 + an5_01* va + an5_02* va**2)
            else
              q = (-tamts-0.098)/0.652  !linear between -0.75 and -0.098
              cd_coarep = q*
     &        (  (         au5_01* va + au5_02* va**2 + au5_03* va**3)
     &         + (am5_10 + am5_11*qva + am5_12*qva**2 + am5_13*qva**3)
     &        ) + (1.0-q)*
     &           (an5_00 + an5_01* va + an5_02* va**2)
            endif !tamts
          endif !va
c
      end function cd_coarep
c
      real function cd_core2(wind,vpmx,airt,sst)
      implicit none
c
      real    wind,vpmx,airt,sst
c
c --- Wind stress drag coefficient * 10^3 from
c --- the CORE v2 bulk algorithm (Large and Yeager, 2009).
c
c --- wind = wind speed (m/s)
c --- vpmx = water vapor mixing ratio (kg/kg)
c --- airt = air temperature (C)
c --- sst  = sea temperature (C)
c
c --- Added to HYCOM by Alexandra Bozec, FSU.
c
      integer it_a
      real    u10,v10,uw10,uw
      real    cd_n10,cd_n10_rt,ce_n10,ch_n10,cd_rt,stab,
     &        tv,tstar,qstar,bstar,zeta,x2,x,xx,
     &        psi_m,psi_h,z0,rair,qrair,zi
      real    cd10,ce10,ch10,ustar
c
      real, parameter :: vonkar=  0.4        !Von Karmann constant
      real, parameter ::  tzero=273.16       !celsius to kelvin offset
      real, parameter ::      g=  9.806      !same as HYCOM's g
c
c --- saturation specific humidity
      real qsatur5,t,qra
      qsatur5(t,qra)= 0.98*qra*6.40380e5*exp(-5107.4/(t+tzero))
c
c --- CORE v2 Large and Yeager 2009 Clim. Dyn.: The global climatology
c ---  of an interannually varying air-sea flux dataset.
c --- The bulk formulae effectively transform the problem of specifying
c --- the turbulent surface fluxes (at zi=10m) into one of describing 
c --- the near surface atmospheric state (wind, temperature and humidity).
c
*     write(6,'(a,1p4g14.5)')
*    &   '   wind,vpmx,airt,sst =',wind,vpmx,airt,sst
*
      rair = 1.22
      rair = 1.0/rair

      zi = 10.0
      tv = (airt+tzero)*(1.0+0.608*vpmx)  !in Kelvin
      uw = max(wind, 0.5)  !0.5 m/s floor on wind (undocumented NCAR)
      uw10 = uw            !first guess 10m wind

      cd_n10 = (2.7/uw10+0.142+0.0764*uw10)*1.0e-3         !L-Y eqn. 6a
*     write(6,'(a,1p4g14.5)')
*    &   'cd_n10 =',cd_n10,
*    &     (2.7/uw10)*1.0e-3,0.142*1.0e-3,(0.0764*uw10)*1.0e-3
      cd_n10_rt = sqrt(cd_n10)
      ce_n10 =  34.6 *cd_n10_rt*1.0e-3                     !L-Y eqn. 6b
      stab   = 0.5 + sign(0.5,airt-sst)
      ch_n10 = (18.0*stab+32.7*(1-stab))*cd_n10_rt*1.0e-3  !L-Y eqn. 6c

      cd10 = cd_n10  !first guess for exchange coeff's at z
      ch10 = ch_n10
      ce10 = ce_n10
*     write(6,'(a,1p4g14.5)')
*    &   'uw10,psi_m,cd_n10,cd10=',uw10,0.0,cd_n10,cd10
*
      do it_a= 1,2  !Monin-Obukhov iteration
        cd_rt = sqrt(cd10)
        ustar = cd_rt*uw                              !L-Y eqn. 7a
        tstar = (ch10/cd_rt)*(airt-sst)               !L-Y eqn. 7b
        qstar = (ce10/cd_rt)*
     &          (vpmx-qsatur5(sst,qrair))             !L-Y eqn. 7c
        bstar = g*(tstar/tv+qstar/(vpmx+1.0/0.608))
        zeta  = vonkar*bstar*zi/(ustar*ustar)         !L-Y eqn. 8a
        zeta  = sign( min(abs(zeta),10.0), zeta )     !undocumented NCAR
        x2 = sqrt(abs(1.0-16.0*zeta))                 !L-Y eqn. 8b
        x2 = max(x2, 1.0)                             !undocumented NCAR
        x  = sqrt(x2)

        if (zeta > 0.0) then
            psi_m = -5.0*zeta                         !L-Y eqn. 8c
            psi_h = -5.0*zeta                         !L-Y eqn. 8c
        else
            psi_m = log((1.0+2.0*x+x2)*(1.0+x2)/8.0)
     &            - 2.0*(atan(x)-atan(1.0))           !L-Y eqn. 8d
            psi_h = 2.0*log((1.0+x2)/2.0)             !L-Y eqn. 8e
        end if

        uw10 = uw/(1.0+cd_n10_rt*(log(zi/10.0)-psi_m) !L-Y eqn. 9
     &           /vonkar)
        cd_n10 = (2.7/uw10+0.142+0.0764*uw10)*1.0e-3  !L-Y eqn. 6a again
        cd_n10_rt = sqrt(cd_n10)
        ce_n10 = 34.6*cd_n10_rt*1.0e-3                !L-Y eqn. 6b again
        stab   = 0.5 + sign(0.5,zeta)
        ch_n10 =(18.0*stab+32.7*(1.0-stab))*cd_n10_rt*1.0e-3  !L-Y eqn. 6c again
        z0     = 10.0*exp(-vonkar/cd_n10_rt)          !diagnostic
c

        xx   = (log(zi/10.0)-psi_m)/vonkar
        cd10 = cd_n10/(1.0+cd_n10_rt*xx)**2           !L-Y 10a
        xx   = (log(zi/10.0)-psi_h)/vonkar
        ch10 = ch_n10/(1.0+ch_n10*xx/cd_n10_rt) *
     &                 sqrt(cd10/cd_n10)              !L-Y 10b
        ce10 = ce_n10/(1.0+ce_n10*xx/cd_n10_rt) *
     &                 sqrt(cd10/cd_n10)              !L-Y 10c
*       write(6,'(a,1p4g14.5)')
*    &     'uw10,psi_m,cd_n10,cd10=',uw10,psi_m,cd_n10,cd10
      enddo
c
      cd_core2 = cd10*1.0e3
      return
      end function cd_core2
c
      subroutine momtum(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_pipe       ! HYCOM debugging interface
      use mod_tides      ! HYCOM tides
#if defined(STOKES)
      use mod_stokes     ! Stokes Drift Velocity Module
#endif
      implicit none
c
      integer m,n
c
c --- ---------------------------------------------------------
c --- momentum equations (2nd order version)
c
c --- Enstrophy conserving advection scheme (Sadourney, 1975)
c
c --- diffusion is Laplacian and/or biharmonic, both with
c --- "constant" and deformation dependent coefficients.
c
c --- hydrostatic equation and surface stress via momtum_hs
c --- ---------------------------------------------------------
c --- on entry:
c ---  saln(:,:,:,m) = time step t   with RA time smoothing
c ---  saln(:,:,:,n) = time step t+1
c
c ---    dp(:,:,:,m) = time step t   with RA time smoothing
c ---    dp(:,:,:,n) = time step t+1
c
c ---   dpo(:,:,:,n) = time step t-1
c ---   dpo(:,:,:,m) = time step t
c
c ---   dpv(:,:,:,n) = time step t-1
c ---   dpv(:,:,:,m) = time step t
c ---   dpu(:,:,:,n) = time step t-1
c ---   dpu(:,:,:,m) = time step t
c
c ---     v(:,:,:,n) = time step t-1
c ---     v(:,:,:,m) = time step t
c ---     u(:,:,:,n) = time step t-1
c ---     u(:,:,:,m) = time step t
c
c --- on exit:
c ---   dpv(:,:,:,m) = time step t   with RA time smoothing
c ---   dpv(:,:,:,n) = time step t+1
c ---   dpu(:,:,:,m) = time step t   with RA time smoothing
c ---   dpu(:,:,:,n) = time step t+1
c
c ---     v(:,:,:,m) = time step t   with RA time smoothing
c ---     v(:,:,:,n) = time step t+1
c ---     u(:,:,:,m) = time step t   with RA time smoothing
c ---     u(:,:,:,n) = time step t+1
c
c --- vtotn(:,:)     = time step t-1 to t+1 barotropic tendency
c --- utotn(:,:)     = time step t-1 to t+1 barotropic tendency
c --- ---------------------------------------------------------
c
      logical, parameter :: lpipe_momtum=.false.  !usually .false.
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
     &                 vis2u,vis4u,vis2v,vis4v,vort,
     &                 wgtia,wgtib,wgtja,wgtjb,
     &                 dl2u,dl2uja,dl2ujb,dl2v,dl2via,dl2vib,
     &                 pnk0,pnkp,stresl
#else
      real, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &                 vis2u,vis4u,vis2v,vis4v,vort,
     &                 wgtia,wgtib,wgtja,wgtjb,
     &                 dl2u,dl2uja,dl2ujb,dl2v,dl2via,dl2vib,
     &                 pnk0,pnkp,stresl
      common/momtumr4/ 
     &                 vis2u,vis4u,vis2v,vis4v,vort,
     &                 wgtia,wgtib,wgtja,wgtjb,
     &                 dl2u,dl2uja,dl2ujb,dl2v,dl2via,dl2vib,
     &                 pnk0,pnkp,stresl
      save  /momtumr4/
#endif
c
#if defined(STOKES)
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     &                 usdflux,vsdflux
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk) ::
#endif
     &            dvdzu,dudzu,dvdzv,dudzv,dzdx,dzdy
c

      real    uatvk0,uatvkm,uatvkp,usd_v,vatuk0,vatukm,vatukp,vsd_u,
     &        zbot,ztop

#endif /* STOKES */
      integer, save :: ifirst
c
      real dpia,dpib,dpja,dpjb,vis2a,vis4a,vis2b,vis4b,
     &     scuya,scuyb,scvxa,scvxb,vmag,dall,adrlim,
     &     dpxy,ptopl,pbotl,cutoff,qcutoff,h1,q,deform,aspy2,aspx2,
     &     dt1inv,phi,plo,pbop,pthkbl,ubot,vbot,pstres,
     &     dmontg,dthstr,dragu,dragv,qdpu,qdpv,dpthin,
     &     dpun,uhm,uh0,uhp,dpvn,vhm,vh0,vhp,sum_m,sum_n
      real dp12,dp23,dp123,dp3m1,ql1,ql2,ql3
      integer i,ia,ib,j,ja,jb,k,ka,l,mbdy,ktop,kmid,kbot,margin
c
*     real*8    wtime
*     external  wtime
*     real*8    wtime1(10),wtime2(20,kdm),wtimes
c
      character text*12
      integer, save, allocatable, dimension(:,:) ::
     &  mask
c
      real hfharm,a,b
      include 'stmt_fns.h'
c
c --- harmonic mean divided by 2
      hfharm(a,b)=a*b/(a+b)
c
      data ifirst / 0 /
c
#if defined(RELO)
      if     (.not.allocated(vis2u)) then
        allocate(
     &           vis2u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           vis4u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           vis2v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           vis4v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            vort(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           wgtia(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           wgtib(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           wgtja(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           wgtjb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            dl2u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dl2uja(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dl2ujb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            dl2v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dl2via(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dl2vib(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            pnk0(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            pnkp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          stresl(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 18*(idm+2*nbdy)*(jdm+2*nbdy) )
                 vis2u = 0.0  !r_init
                 vis4u = 0.0  !r_init
                 vis2v = 0.0  !r_init
                 vis4v = 0.0  !r_init
                  vort = 0.0  !r_init
                 wgtia = 0.0  !r_init
                 wgtib = 0.0  !r_init
                 wgtja = 0.0  !r_init
                 wgtjb = 0.0  !r_init
                  dl2u = 0.0  !r_init
                dl2uja = 0.0  !r_init
                dl2ujb = 0.0  !r_init
                  dl2v = 0.0  !r_init
                dl2via = 0.0  !r_init
                dl2vib = 0.0  !r_init
                  pnk0 = 0.0  !r_init
                  pnkp = 0.0  !r_init
                stresl = 0.0  !r_init
      endif !vis2u
#if defined(STOKES)
      if     (.not.allocated(usdflux)) then
        allocate(
     &             usdflux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             vsdflux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
                   usdflux = 0.0 !r_init
                   vsdflux = 0.0 !r_init
c
        allocate(
     &                dvdzu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk),
     &                dudzu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk),
     &                dvdzv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk),
     &                dudzv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk),
     &                 dzdx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk),
     &                 dzdy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,1:kk) )
        call mem_stat_add( 6*(idm+2*nbdy)*(jdm+2*nbdy)*kk )
                      dvdzu = 0.0 !r_init
                      dudzu = 0.0 !r_init
                      dvdzv = 0.0 !r_init
                      dudzv = 0.0 !r_init
                       dzdx = 0.0 !r_init
                       dzdy = 0.0 !r_init
      endif !usdflux
#endif /* STOKES */
#endif /* RELO */
c
      mbdy = 6
c
c --- dp and dpo have up to date halos from cnuity
      call xctilr(dpu(  1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_us)
      call xctilr(dpv(  1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vs)
      call xctilr(u(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_uv)
      call xctilr(v(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vv)
      call xctilr(ubavg(1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_uv)
      call xctilr(vbavg(1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_vv)
      call xctilr(uflx( 1-nbdy,1-nbdy,1  ),1,  kk, 6,6, halo_uv)
      call xctilr(vflx( 1-nbdy,1-nbdy,1  ),1,  kk, 6,6, halo_vv)
c
      if     (ifirst.eq.0) then
        ifirst=1
c ---   setup zero fill.
        margin = mbdy
c
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            vis2u(i,j)=0.0
            vis4u(i,j)=0.0
            vis2v(i,j)=0.0
            vis4v(i,j)=0.0
            dl2u( i,j)=0.0
            dl2v( i,j)=0.0
          enddo !i
        enddo !j
      endif
c
      dpthin  = 0.001*onemm
      h1      =       tenm  !used in lateral weighting of hor.pres.grad.
      cutoff  =   0.5*onem
      qcutoff = 1.0/cutoff
c
c --- ---------------------------------------
c --- hydrostatic equation and surface stress
c --- ---------------------------------------
c
      call momtum_hs(m,n)
c
c +++ ++++++++++++++++++
c +++ momentum equations
c +++ ++++++++++++++++++
c
#if defined(STOKES)
*        wtime1( 4) = wtime()
      margin = mbdy
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            usdflux(i,j)=0.
          endif !iu
          if (SEA_V) then
            vsdflux(i,j)=0.
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- Calculate dudzu,dvdzu using a Lagrange interpolation.
c
      margin = mbdy-1
c
!$OMP PARALLEL DO PRIVATE(j,i,k,
!$OMP&                    dp12,dp23,dp123,dp3m1,ql1,ql2,ql3,
!$OMP&                    vatuk0,vatukm,vatukp,
!$OMP&                    uatvk0,uatvkm,uatvkp)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        k=1
          do i=1-margin,ii+margin
            if (SEA_U) then
              dp12   = max(cutoff,dpu(i,j,1,m))+
     &                 max(cutoff,dpu(i,j,2,m))
              dp23   = max(cutoff,dpu(i,j,2,m))+
     &                 max(cutoff,dpu(i,j,3,m))
              dp123  = dp12+dp23
              ql1    = -2.0*(dp12+dp123)/(dp12 *dp123)
              ql2    =  2.0*      dp123 /(dp12 *dp23)
              ql3    = -2.0* dp12       /(dp123*dp23)
              vatukm = 0.25*(v(i,  j,  1,m)+
     &                       v(i-1,j,  1,m)+
     &                       v(i,  j+1,1,m)+
     &                       v(i-1,j+1,1,m) )
              vatuk0 = 0.25*(v(i,  j,  2,  m)+
     &                       v(i-1,j,  2,  m)+
     &                       v(i,  j+1,2,  m)+
     &                       v(i-1,j+1,2,  m) )
              vatukp = 0.25*(v(i,  j,  3,m)+
     &                       v(i-1,j,  3,m)+
     &                       v(i,  j+1,3,m)+
     &                       v(i-1,j+1,3,m) )
              dudzu(i,j,1) = onem*(u(i,j,1,m)*ql1+
     &                             u(i,j,2,m)*ql2+
     &                             u(i,j,3,m)*ql3 )
              dvdzu(i,j,k) = onem*(vatukm    *ql1+
     &                             vatuk0    *ql2+
     &                             vatukp    *ql3 )
            endif !iu
            if (SEA_V) then
              dp12   = max(cutoff,dpv(i,j,1,m))+
     &                 max(cutoff,dpv(i,j,2,m))
              dp23   = max(cutoff,dpv(i,j,2,m))+
     &                 max(cutoff,dpv(i,j,3,m))
              dp123  = dp12+dp23
              ql1    = -2.0*(dp12+dp123)/(dp12 *dp123)
              ql2    =  2.0*      dp123 /(dp12 *dp23)
              ql3    = -2.0* dp12       /(dp123*dp23)
              uatvkm = 0.25*(u(i,  j,  1,m)+
     &                       u(i-1,j,  1,m)+
     &                       u(i,  j+1,1,m)+
     &                       u(i-1,j+1,1,m) )
              uatvk0 = 0.25*(u(i,  j,  2,  m)+
     &                       u(i-1,j,  2,  m)+
     &                       u(i,  j+1,2,  m)+
     &                       u(i-1,j+1,2,  m) )
              uatvkp = 0.25*(u(i,  j,  3,m)+
     &                       u(i-1,j,  3,m)+
     &                       u(i,  j+1,3,m)+
     &                       u(i-1,j+1,3,m) )
              dvdzv(i,j,1) = onem*(v(i,j,1,m)*ql1+
     &                             v(i,j,2,m)*ql2+
     &                             v(i,j,3,m)*ql3 )
              dudzv(i,j,k) = onem*(uatvkm    *ql1+
     &                             uatvk0    *ql2+
     &                             uatvkp    *ql3 )
            endif !iv
          enddo !i
!       k=1
        do k= 2,kk-1
          do i=1-margin,ii+margin
            if (SEA_U) then
              dp12   = max(cutoff,dpu(i,j,k-1,m))+
     &                 max(cutoff,dpu(i,j,k,  m))
              dp23   = max(cutoff,dpu(i,j,k,  m))+
     &                 max(cutoff,dpu(i,j,k+1,m))
              dp123  = dp12+dp23
              dp3m1  = dp23-dp12
              ql1    = -2.0*dp23/(dp12*dp123)
              ql2    =  2.0*dp3m1/(dp12*dp23)
              ql3    =  2.0*dp12/(dp123*dp23)
              vatukm = 0.25*(v(i,  j,  k-1,m)+
     &                       v(i-1,j,  k-1,m)+
     &                       v(i,  j+1,k-1,m)+
     &                       v(i-1,j+1,k-1,m) )
              vatuk0 = 0.25*(v(i,  j,  k,  m)+
     &                       v(i-1,j,  k,  m)+
     &                       v(i,  j+1,k,  m)+
     &                       v(i-1,j+1,k,  m) )
              vatukp = 0.25*(v(i,  j,  k+1,m)+
     &                       v(i-1,j,  k+1,m)+
     &                       v(i,  j+1,k+1,m)+
     &                       v(i-1,j+1,k+1,m) )
              dudzu(i,j,k) = onem*(u(i,j,k-1,m)*ql1+
     &                             u(i,j,k,  m)*ql2+
     &                             u(i,j,k+1,m)*ql3 )
              dvdzu(i,j,k) = onem*(vatukm*      ql1+
     &                             vatuk0      *ql2+
     &                             vatukp      *ql3 )
cdiag         if (max(abs(i-itest),abs(j-jtest)).eq.0) then
cdiag           write(lp,'(a,i3,3g20.10)')
cdiag.            'dp,k =',k,dp12,dp23,ql1+ql2+ql3
cdiag           write(lp,'(a,i3,3g20.10)')
cdiag.            'ql,k =',k,ql1,ql2,ql3
cdiag           write(lp,'(a,i3,2g20.10)')
cdiag.            'dz,k =',k,dudzu(i,j,k),dvdzu(i,j,k)
cdiag         endif !test
            endif !iu
            if (SEA_V) then
              dp12   = max(cutoff,dpv(i,j,k-1,m))+
     &                 max(cutoff,dpv(i,j,k,  m))
              dp23   = max(cutoff,dpv(i,j,k,  m))+
     &                 max(cutoff,dpv(i,j,k+1,m))
              dp123  = dp12+dp23
              dp3m1  = dp23-dp12
              ql1    = -2.0*dp23/(dp12*dp123)
              ql2    =  2.0*dp3m1/(dp12*dp23)
              ql3    =  2.0*dp12/(dp123*dp23)
              uatvkm = 0.25*(u(i,  j,  k-1,m)+
     &                       u(i-1,j,  k-1,m)+
     &                       u(i,  j+1,k-1,m)+
     &                       u(i-1,j+1,k-1,m) )
              uatvk0 = 0.25*(u(i,  j,  k,  m)+
     &                       u(i-1,j,  k,  m)+
     &                       u(i,  j+1,k,  m)+
     &                       u(i-1,j+1,k,  m) )
              uatvkp = 0.25*(u(i,  j,  k+1,m)+
     &                       u(i-1,j,  k+1,m)+
     &                       u(i,  j+1,k+1,m)+
     &                       u(i-1,j+1,k+1,m) )
              dvdzv(i,j,k) = onem*(v(i,j,k-1,m)*ql1+
     &                             v(i,j,k  ,m)*ql2+
     &                             v(i,j,k+1,m)*ql3 )
              dudzv(i,j,k) = onem*(uatvkm      *ql1+
     &                             uatvk0      *ql2+
     &                             uatvkp      *ql3 )
            endif !iv
          enddo !i
        enddo !k
        k=kk
          do i=1-margin,ii+margin
            if (SEA_U) then
              dp12   = max(cutoff,dpu(i,j,kk-2,m))+
     &                 max(cutoff,dpu(i,j,kk-1,m))
              dp23   = max(cutoff,dpu(i,j,kk-1,m))+
     &                 max(cutoff,dpu(i,j,kk,  m))
              dp123  = dp12+dp23
              ql1    =  2.0*dp23/(dp12*dp123)
              ql2    = -2.0*dp123/(dp12*dp23)
              ql3    =  2.0*(dp12+dp123)/(dp123*dp23)
              vatukm = 0.25*(v(i,  j  ,kk-2,m)+
     &                       v(i-1,j  ,kk-2,m)+
     &                       v(i,  j+1,kk-2,m)+
     &                       v(i-1,j+1,kk-2,m) )
              vatuk0 = 0.25*(v(i,  j  ,kk-1,  m)+
     &                       v(i-1,j  ,kk-1,  m)+
     &                       v(i,  j+1,kk-1,  m)+
     &                       v(i-1,j+1,kk-1,  m) )
              vatukp = 0.25*(v(i,  j,  kk,m)+
     &                       v(i-1,j,  kk,m)+
     &                       v(i,  j+1,kk,m)+
     &                       v(i-1,j+1,kk,m) )
              dudzu(i,j,kk) = onem*(u(i,j,kk-2,m)*ql1+
     &                              u(i,j,kk-1,m)*ql2+
     &                              u(i,j,kk,  m)*ql3 )
              dvdzu(i,j,kk) = onem*(vatukm       *ql1+
     &                              vatuk0       *ql2+
     &                              vatukp       *ql3 )
            endif !iu
            if (SEA_V) then
              dp12   = max(cutoff,dpv(i,j,kk-2,m))+
     &                 max(cutoff,dpv(i,j,kk-1,m))
              dp23   = max(cutoff,dpv(i,j,kk-1,m))+
     &                 max(cutoff,dpv(i,j,kk,  m))
              dp123  = dp12+dp23
              ql1    =  2.0*dp23/(dp12*dp123)
              ql2    = -2.0*dp123/(dp12*dp23)
              ql3    =  2.0*(dp12+dp123)/(dp123*dp23)
              uatvkm = 0.25*(u(i,  j  ,kk-2,m)+
     &                       u(i-1,j  ,kk-2,m)+
     &                       u(i,  j+1,kk-2,m)+
     &                       u(i-1,j+1,kk-2,m) )
              uatvk0 = 0.25*(u(i,  j  ,kk-1,m)+
     &                       u(i-1,j  ,kk-1,m)+
     &                       u(i,  j+1,kk-1,m)+
     &                       u(i-1,j+1,kk-1,m) )
              uatvkp = 0.25*(u(i,  j,  kk,  m)+
     &                       u(i-1,j,  kk,  m)+
     &                       u(i,  j+1,kk,  m)+
     &                       u(i-1,j+1,kk,  m) )
              dvdzv(i,j,kk) = onem*(v(i,j,kk-2,m)*ql1+
     &                              v(i,j,kk-1,m)*ql2+
     &                              v(i,j,kk,  m)*ql3 )
              dudzv(i,j,kk) = onem*(uatvkm       *ql1+
     &                              uatvk0       *ql2+
     &                              uatvkp       *ql3 )
            endif !iv
c<-hsk
c        if((i.eq.itest).and.(j.eq.jtest)) then
c          write(lp,'(a,i9,2i6,i5,6e13.5)')
c     &    'm1368: nstep,i,j,k,u(i,j,1,m),v(i,j,1,m) ',
c     &    u(i,j,k,m),v(i,j,k,m),u(i,j,1,m),v(i,j,1,m),
c     &    u(i,j,kk,m),v(i,j,kk,m)
c        endif
c->hsk
          enddo !i
!       k=kk
      enddo !j
c
c                              Calculation of dzdx,dzdy needed in momtum -u-v-
c                              -----------------------------------------
c
      margin = 0
c
!$OMP PARALLEL DO PRIVATE(j,i,k,zbot,ztop)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j = 1-margin, jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then  
            ztop = -(depths(i,j)-depths(i-1,j))
            do k = kk, 1, -1
              zbot        = ztop
              ztop        = zbot +
     &                      qonem*(dp(i,  j,k,m)*oneta(i,  j)-
     &                             dp(i-1,j,k,m)*oneta(i-1,j) )
              dzdx(i,j,k) = 0.5*(ztop+zbot)
            enddo !k
          endif  ! iu
c
          if (SEA_V) then
            ztop = -(depths(i,j)-depths(i,j-1))
            do k = kk, 1, -1
              zbot        = ztop
              ztop        = zbot +
     &                      qonem*(dp(i,j,  k,m)*oneta(i,  j)-
     &                             dp(i,j-1,k,m)*oneta(i,j-1) )
              dzdy(i,j,k) = 0.5*(ztop+zbot)
            enddo !k
          endif  ! iv
        enddo  !i
      enddo !j
      call xctilr(dzdx(1-nbdy,1-nbdy,1),1, kk, nbdy,nbdy, halo_uv)
      call xctilr(dzdy(1-nbdy,1-nbdy,1),1, kk, nbdy,nbdy, halo_vv)
#endif /* STOKES */
c
c --- rhs: p, u.n+, v.n+, ubavg.n+, vbavg.n+, depthv+, pvtrop+
c --- rhs: dpmixl.m+, taux+, dpu, depthu+, dpv, tauy+
c --- lhs: util1, util2, drag, ubrhs, stresx, vbrhs, stresy
c
*-----disp_count  = disp_count + 1
      stresl(:,:) = 0.0
c
      if     (drglim.gt.0.0) then
        adrlim = drglim
      else
        adrlim = 0.125
      endif
      dt1inv = 1./delt1
c
      margin = mbdy - 1
c
!$OMP PARALLEL DO PRIVATE(j,l,i,k,
!$OMP&                    phi,plo,pbop,ubot,vbot,vmag,dall,pstres)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
c
        do i=1-margin,ii+margin
          if (SEA_P) then
c
c ---       bottom drag (standard bulk formula)
c ---       bottom stress is applied over thickness thkbot (or the
c ---        total depth is this is less)
c
            thkbop(i,j)=min( thkbot*onem, p(i,j,kk+1) )
c
c ---       the bottom stress term is estimated using velocity averaged
c ---       over the bottom boundary layer. this thickness is dpbbl for
c ---       the kpp boundary layer; otherwise, it is thkbop
c
            ubot=0.0
            vbot=0.0
            if (mxlkpp .and. bblkpp) then
              pthkbl=max(dpbbl(i,j),thkbop(i,j))  !thknss of bot. b.l.
            else
              pthkbl=thkbop(i,j)                  !thknss of bot. b.l.
            endif
            pbop=p(i,j,kk+1)-pthkbl               !top of bot. b.l.
            phi =max(p(i,j,1),pbop)
            do k=1,kk
              plo =phi  ! max(p(i,j,k),pbop)
              phi =max(p(i,j,k+1),pbop)
              ubot=ubot + (u(i,j,k,n)+u(i+1,j,k,n))*(phi-plo)
              vbot=vbot + (v(i,j,k,n)+v(i,j+1,k,n))*(phi-plo)
*RBc Modif RB pour friction avec cl ouvertes
*RBccc              ubot=ubot + (u(i,j,k,n)+u(i+1,j,k,n))*(phi-plo)
*RBccc              vbot=vbot + (v(i,j,k,n)+v(i,j+1,k,n))*(phi-plo)
*RB              if (iuopn(i,j).ge.1) then
*RB                 ubot=ubot + (unest(i,j,k,1)+u(i+1,j,k,n))*(phi-plo)
*RB              elseif (iuopn(i+1,j).ge.1) then
*RB                 ubot=ubot + (u(i,j,k,n)+unest(i+1,j,k,1))*(phi-plo)
*RB              else
*RB                 ubot=ubot + (u(i,j,k,n)+u(i+1,j,k,n))*(phi-plo)
*RB              endif
*RB              if (ivopn(i,j).ge.1) then
*RB                 vbot=vbot + (vnest(i,j,k,1)+v(i,j+1,k,n))*(phi-plo)
*RB              elseif (ivopn(i,j+1).ge.1) then
*RB                 vbot=vbot + (v(i,j,k,n)+vnest(i,j+1,k,1))*(phi-plo)
*RB              else
*RB                 vbot=vbot + (v(i,j,k,n)+v(i,j+1,k,n))*(phi-plo)
*RB              endif
            enddo !k
            ubot=ubot/min(pthkbl,p(i,j,kk+1))
     &            + (ubavg(i,j,n)+ubavg(i+1,j,n))
            vbot=vbot/min(pthkbl,p(i,j,kk+1))
     &            + (vbavg(i,j,n)+vbavg(i,j+1,n))
c
c ---       drag = Cb * (|v| + c.bar)
c ---       include 1/thkbop for the fraction of layer calculation
c ---        and onem for conversion from 1/dp to 1/h
c
            vmag=0.5*sqrt(ubot**2+vbot**2)
            dall=cbp(i,j)*(vmag+cbarp(i,j))  !no tidal drag
            drag(i,j)=dall*onem/thkbop(i,j)
            util1(i,j) = ubot
            util2(i,j) = vbot
            if (mxlkpp .and. bblkpp) then
              ustarb(i,j)=sqrt(dall*vmag)
            endif
c
c ---       tidal bottom drag, drgten.1.1 is drgscl*rh
c
            util6(i,j)=max(0.1,thkdrg)*onem  !drag applied over this thknss
            util5(i,j)=drgten(1,1,i,j)/min(util6(i,j)*qonem,depths(i,j))
          endif !ip
        enddo !i
c
c ---   store r.h.s. of barotropic u/v eqn. in -ubrhs,vbrhs-
c ---   time-interpolate wind stress
c
        do i=1-margin,ii+margin
          if (SEA_U) then
            ubrhs(i,j)=
     &        (vbavg(i  ,j,  m)*depthv(i  ,j)
     &        +vbavg(i  ,j+1,m)*depthv(i  ,j+1)
     &        +vbavg(i-1,j,  m)*depthv(i-1,j)
     &        +vbavg(i-1,j+1,m)*depthv(i-1,j+1))
     &        *(pvtrop(i,j)+pvtrop(i,j+1))*.125
c
            if     (windf) then
              if(hybrid .and. mxlkrt) then
                pstres=0.5*(dpmixl(i,j,m)+dpmixl(i-1,j,m))
              else
                pstres=dpu(i,j,1,m)
              endif
c ---         units of surtx are N/m^2 (i.e. Pa)
              stresx(i,j)=(surtx(i,j)+surtx(i-1,j))*0.5*g
     &                    /(pstres*thref)
            else  ! no taux
              stresx(i,j)=0.
            endif !windf:else
          endif !iu
c
          if (SEA_V) then
            vbrhs(i,j)=
     &      -(ubavg(i,  j  ,m)*depthu(i,j  )
     &       +ubavg(i+1,j  ,m)*depthu(i+1,j  )
     &       +ubavg(i,  j-1,m)*depthu(i,j-1)
     &       +ubavg(i+1,j-1,m)*depthu(i+1,j-1))
     &       *(pvtrop(i,j)+pvtrop(i+1,j))*.125
c
            if     (windf) then
              if(hybrid .and. mxlkrt) then
                pstres=0.5*(dpmixl(i,j,m)+dpmixl(i,j-1,m))
              else
                pstres=dpv(i,j,1,m)
              endif 
c ---         units of surty are N/m^2 (i.e. Pa)
              stresy(i,j)=(surty(i,j)+surty(i,j-1))*0.5*g
     &                    /(pstres*thref)
            else  ! no tauy
              stresy(i,j)=0.
            endif !windf:else
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'uba.n  n=',n
        call pipe_compare_sym1(ubavg(1-nbdy,1-nbdy,n),iu,text)
        write (text,'(a9,i3)') 'vba.n  n=',n
        call pipe_compare_sym1(vbavg(1-nbdy,1-nbdy,n),iv,text)
        write (text,'(a9,i3)') 'cbarp  k=',0
        call pipe_compare_sym1(cbarp,ip,text)
        write (text,'(a9,i3)') 'cbp    k=',0
        call pipe_compare_sym1(cbp  ,ip,text)
        write (text,'(a9,i3)') 'thkbop k=',0
        call pipe_compare_sym1(thkbop,ip,text)
        write (text,'(a9,i3)') 'ubot_p k=',0
        call pipe_compare_sym1(util1 ,ip,text)
        write (text,'(a9,i3)') 'vbot_p k=',0
        call pipe_compare_sym1(util2 ,ip,text)
        write (text,'(a9,i3)') 'drag   k=',0
        call pipe_compare_sym1(drag,ip,text)
      endif
c
c --- the old  momeq2.f  starts here
c
*        wtime1( 5) = wtime()
c
c --- rhs: 0.0
c --- lhs: util1, util2
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
c ---     spatial weighting function for pressure gradient calculation:
          util1(i,j)=0.0
          util2(i,j)=0.0
c ---     p.1 
          pnkp( i,j)=0.0
        enddo !i
        enddo !j
c
      do 9 k=1,kk
c
c --- store total (barotropic plus baroclinic) flow at old and mid time in
c --- -utotn,vtotn- and -utotm,vtotm- respectively. store minimum thickness
c --- values for use in pot.vort. calculation in -dpmx-.
c --- store p.k and p.k+1 for time level t+1 in pnk0,pnkp
c
*         wtime2( 1,k) = wtime()
c
c --- rhs: dpmx, dp.m+
c --- lhs: dpmx
c
      margin = mbdy - 2
c
      do i=1-margin,ii+margin
        dpmx(i,1-margin)=2.*cutoff
      enddo !i
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          dpmx(i,j+1)=2.*cutoff
        enddo !i
        do i=1-margin,ii+margin
          if (SEA_U) then
            dpmx(i,j+1)=max(dpmx(i,j+1),dpo(i,j,k,m)+dpo(i-1,j,k,m))
          endif !iu
          if (SEA_P) then
            pnk0(i,j)=pnkp(i,j)
            pnkp(i,j)=pnk0(i,j)+dp(i,j,k,n)
          endif !ip
        enddo !i
      enddo !j
c
*         wtime2( 2,k) = wtime()
c
c --- rhs: ubavg.m, ubavg.n, dp.m+, dpu
c --- lhs: utotm, utotn, uflux, dpmx, pu
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,l,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do l=1,isu(j) !ok
c
          i=ifu(j,l)-1
          if     (i.ge.1-margin .and. i.le.ii+margin) then
            if     (iuopn(i,j).ne.0) then
              utotm(i,j)=u(i+1,j,k,m)+ubavg(i,j,m)
              utotn(i,j)=u(i+1,j,k,n)+ubavg(i,j,n)
              uflux(i,j)=utotm(i,j)*max(dpo(i,j,k,m),cutoff)
#if defined(STOKES)
            usdflux(i,j)=usd(i+1,j,k)*
     &                              max(dpo(i,j,k,m),cutoff)
#endif
            endif
          endif
          i=ilu(j,l)+1
          if     (i.ge.1-margin .and. i.le.ii+margin) then
            if     (iuopn(i,j).ne.0) then
              utotm(i,j)=u(i-1,j,k,m)+ubavg(i,j,m)
              utotn(i,j)=u(i-1,j,k,n)+ubavg(i,j,n)
              uflux(i,j)=utotm(i,j)*max(dpo(i-1,j,k,m),cutoff)
#if defined(STOKES)
            usdflux(i,j)=usd(i-1,j,k)*
     &                              max(dpo(i-1,j,k,m),cutoff)
#endif
            endif
          endif  
c
          do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
            dpmx(i,j)=max(dpmx(i,j),dpo(i,j,k,m)+dpo(i-1,j,k,m))
            utotm(i,j)=u(i,j,k,m)+ubavg(i,j,m)
            utotn(i,j)=u(i,j,k,n)+ubavg(i,j,n)
            uflux(i,j)=utotm(i,j)*max(dpu(i,j,k,m),cutoff)
#if defined(STOKES)
          usdflux(i,j)=usd(i,j,k)*max(dpu(i,j,k,m),cutoff)  !usd is total drift
#endif
            pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,m)
c
          enddo !i
        enddo !l
      enddo !j
!$OMP END PARALLEL DO
c
*         wtime2( 3,k) = wtime()
c
c --- rhs: vbavg.m, vbavg.n, dp.m+, dpv
c --- lhs: vtotm, vtotn, vflux, dpmx, pv
c
      margin = mbdy - 2
c
      do i=1-margin,ii+margin
        do l=1,jsv(i) !ok
          j=jfv(i,l)-1
          if     (j.ge.1-margin .and. j.le.jj+margin) then
            if      (ivopn(i,j).ne.0) then
              vtotm(i,j)=v(i,j+1,k,m)+vbavg(i,j,m)
              vtotn(i,j)=v(i,j+1,k,n)+vbavg(i,j,n)
              vflux(i,j)=vtotm(i,j)*max(dpo(i,j,k,m),cutoff)
#if defined(STOKES)
            vsdflux(i,j)=vsd(i,j+1,k)*
     &                              max(dpo(i,j,k,m),cutoff)
#endif
            endif
          endif  
          j=jlv(i,l)+1
          if     (j.ge.1-margin .and. j.le.jj+margin) then
            if      (ivopn(i,j).gt.0) then
              vtotm(i,j)=v(i,j-1,k,m)+vbavg(i,j,m)
              vtotn(i,j)=v(i,j-1,k,n)+vbavg(i,j,n)
              vflux(i,j)=vtotm(i,j)*max(dpo(i,j-1,k,m),cutoff)
#if defined(STOKES)
            vsdflux(i,j)=vsd(i,j-1,k)*
     &                              max(dpo(i,j-1,k,m),cutoff)
#endif
            endif
          endif  
        enddo !l
      enddo !i
c
*         wtime2( 4,k) = wtime()
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            dpmx(i  ,j)=max(dpmx(i  ,j),dpo(i,j,k,m)+dpo(i,j-1,k,m))
            dpmx(i+1,j)=max(dpmx(i+1,j),dpo(i,j,k,m)+dpo(i,j-1,k,m))
            vtotm(i,j)=v(i,j,k,m)+vbavg(i,j,m)
            vtotn(i,j)=v(i,j,k,n)+vbavg(i,j,n)
            vflux(i,j)=vtotm(i,j)*max(dpv(i,j,k,m),cutoff)
#if defined(STOKES)
          vsdflux(i,j)=vsd(i,j,k)*max(dpv(i,j,k,m),cutoff)  !vsd is total drift
#endif
            pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,m)
          endif !iv
        enddo !i
      enddo !j
c
c --- define auxiliary velocity fields (via,vib,uja,ujb) to implement
c --- sidewall friction along near-vertical bottom slopes. wgtja,wgtjb,wgtia,
c --- wgtib indicate the extent to which a sidewall is present.
c
*         wtime2( 5,k) = wtime()
c
c --- rhs: pu, depthu+, utotn+, wgtja
c --- lhs: wgtja, wgtjb, uja, ujb, dl2u
c --- rhs: pv, depthv+, vtotn+, wgtia
c --- lhs: wgtia, wgtib, via, vib, dl2v
c --- rhs: vtotm, vort+, corio+, dp.m+, dpmx+, vtotn
c --- lhs: vort, potvor, defor2
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,ja,jb,l,i,ia,ib,aspy2,aspx2)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
c ---   assume margin<nblk
        ja=j-1
        jb=j+1
        do i=1-margin,ii+margin
          if (SEA_U) then
            wgtja(i,j)=max(0.,min(1.,(pu(i,j,k+1)-depthu(i,ja))
     &                /max(pu(i,j,k+1)-pu(i,j,k),epsil)))
            wgtjb(i,j)=max(0.,min(1.,(pu(i,j,k+1)-depthu(i,jb))
     &                /max(pu(i,j,k+1)-pu(i,j,k),epsil)))
            uja(i,j)=(1.-wgtja(i,j))*utotn(i,ja)+
     &               wgtja(i,j)*slip*utotn(i,j)
            ujb(i,j)=(1.-wgtjb(i,j))*utotn(i,jb)+
     &               wgtjb(i,j)*slip*utotn(i,j)
*RBcRB
*RB            if (i.ge.1-margin.and.i.le.ii+margin) then
*RB               if (ivopn(i-1,j).ne.0.and.ivopn(i,j).ne.0) then
*RB                  uja(i,j)=utotn(i,j)
*RB               endif
*RB               if (j.ge.1-margin.and.j.le.jj+margin) then
*RB                  if (ivopn(i-1,j+1).ne.0.and.ivopn(i,j+1).ne.0) then
*RB                     ujb(i,j)=utotn(i,j)
*RB                  endif
*RB               endif
*RB            endif
c
c ---       Laplacian of utotn scaled by -0.25*max(scux,scuy)**2
            aspx2 = aspux(i,j)**2
            aspy2 = aspuy(i,j)**2
            dl2u(i,j)=0.5*(aspx2+aspy2)*utotn(i,j)
     &                   -0.25*aspx2*(utotn(i+1,j)+utotn(i-1,j))
     &                   -0.25*aspy2*(  uja(i,  j)+  ujb(i,  j))
c
          endif !iu
c
          if (SEA_V) then
c ---       assume margin<nblk
            ia=i-1
            ib=i+1
            wgtia(i,j)=max(0.,min(1.,(pv(i,j,k+1)-depthv(ia,j))
     &                /max(pv(i,j,k+1)-pv(i,j,k),epsil)))
            wgtib(i,j)=max(0.,min(1.,(pv(i,j,k+1)-depthv(ib,j))
     &                /max(pv(i,j,k+1)-pv(i,j,k),epsil)))
            via(i,j)=(1.-wgtia(i,j))*vtotn(ia,j)+
     &                wgtia(i,j)*slip*vtotn(i,j)
            vib(i,j)=(1.-wgtib(i,j))*vtotn(ib,j)+
     &                wgtib(i,j)*slip*vtotn(i,j)
*RBcRB
*RB            if (j.ge.1-margin.and.j.le.jj+margin) then
*RB               if (iuopn(i,j-1).ne.0.and.iuopn(i,j).ne.0) then
*RB                  via(i,j)=vtotn(i,j)
*RB               endif
*RB               if (i.ge.1-margin.and.i.le.ii+margin) then
*RB                  if (iuopn(i+1,j-1).ne.0.and.iuopn(i+1,j).ne.0) then
*RB                     vib(i,j)=vtotn(i,j)
*RB                  endif
*RB               endif
*RB            endif
c
c ---       Laplacian of vtotn scaled by -0.25*max(scvx,scvy)**2
            aspx2 = aspvx(i,j)**2
            aspy2 = aspvy(i,j)**2
            dl2v(i,j)=0.5*(aspx2+aspy2)*vtotn(i,j)
     &                 -0.25*aspy2*(vtotn(i,j+1)+vtotn(i,j-1))
     &                 -0.25*aspx2*(  via(i,j  )+  vib(i,j  ))
          endif !iv
        enddo !i
c
c --- vorticity, pot.vort., defor. at lateral boundary points
        do l=1,isv(j) !ok
          i=ifv(j,l)
          if     (i.ge.1-margin .and. i.le.ii+margin) then
            vort(i  ,j)= vtotm(i,j)*(1.-slip)*scvy(i,j)*scq2i(i  ,j)
            potvor(i  ,j)=(vort(i  ,j)+corio(i  ,j)) * 8.
     &                     /max(8.*cutoff,
     &                          4.*(dpo(i,j,k,m)+dpo(i,ja ,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i+1,j))
            defor2(i  ,j)=(vtotn(i,j)*(1.-slip)*scvy(i,j))**2*
     &                      scq2i(i  ,j)
          endif
          i=ilv(j,l)
          if     (i.ge.1-margin .and. i.le.ii+margin) then
            vort(i+1,j)=-vtotm(i,j)*(1.-slip)*scvy(i,j)*scq2i(i+1,j)
            potvor(i+1,j)=(vort(i+1,j)+corio(i+1,j)) * 8.
     &                     /max(8.*cutoff,
     &                          4.*(dpo(i,j,k,m)+dpo(i,ja ,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i+1,j))
            defor2(i+1,j)=(vtotn(i,j)*(1.-slip)*scvy(i,j))**2*
     &                      scq2i(i+1,j)
          endif
        enddo !i
      enddo !l
!$OMP END PARALLEL DO
c
*         wtime2( 6,k) = wtime()
c
c --- vorticity, pot.vort., defor. at lateral boundary points
c
c --- rhs: utotm, vort+, corio+, dp.m+, dpmx+, utotn
c --- lhs: vort, potvor, defor2
c
      margin = mbdy - 2
c
      do i=1-margin,ii+margin
c ---   assume margin<nblk
        ia=i-1
        do l=1,jsu(i) !ok
          j=jfu(i,l)
          if     (j.ge.1-margin .and. j.le.jj+margin) then
            vort(i,j  )=-utotm(i,j)*(1.-slip)*scux(i,j)*scq2i(i,j  )
            potvor(i,j  )=(vort(i,j  )+corio(i,j  )) * 8.
     &                     /max(8.*cutoff,
     &                          4.*(dpo(i,j,k,m)+dpo(ia ,j,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i,j+1))
            defor2(i,j  )=(utotn(i,j)*(1.-slip)*scux(i,j))**2*
     &                      scq2i(i,j  )
          endif
          j=jlu(i,l)
          if     (j.ge.1-margin .and. j.le.jj+margin) then
            vort(i,j+1)= utotm(i,j)*(1.-slip)*scux(i,j)*scq2i(i,j+1)
            potvor(i,j+1)=(vort(i,j+1)+corio(i,j+1)) * 8.
     &                     /max(8.*cutoff,
     &                          4.*(dpo(i,j,k,m)+dpo(ia ,j,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i,j+1))
           defor2(i,j+1)=(utotn(i,j)*(1.-slip)*scux(i,j))**2*
     &                      scq2i(i,j+1)
          endif
        enddo !l
      enddo !i
c
*         wtime2( 7,k) = wtime()
c
c --- rhs: p, utotn+, vtotn+
c --- lhs: defor1
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            defor1(i,j)=((utotn(i+1,j)*scuy(i+1,j)
     &                   -utotn(i,  j)*scuy(i,  j))
     &                  -(vtotn(i,j+1)*scvx(i,j+1)
     &                   -vtotn(i,j  )*scvx(i,j  )))**2
     &                  *scp2i(i,j)
          endif !ip
        enddo !i
      enddo !j
c
c --- vorticity, pot.vort., defor. at interior points (incl. promontories)
*         wtime2( 8,k) = wtime()
c
c --- rhs: vtotm+, utotm+, vort, dp.m+, dpmx+, vib+, via, ujb+, uja
c --- lhs: vort, potvor, defor2
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_Q) then
            vort(i,j)=(vtotm(i,j)*scvy(i,j)-vtotm(i-1,j)*scvy(i-1,j)
     &                -utotm(i,j)*scux(i,j)+utotm(i,j-1)*scux(i,j-1))
     &                *scq2i(i,j)
            potvor(i,j)=(vort(i,j)+corio(i,j)) * 8.
     &         /max(8.*cutoff,
     &              2.*(dpo(i,j  ,k,m)+dpo(i-1,j  ,k,m)+
     &                  dpo(i,j-1,k,m)+dpo(i-1,j-1,k,m)),
     &              dpmx(i,j),dpmx(i-1,j),dpmx(i+1,j),
     &                        dpmx(i,j-1),dpmx(i,j+1))
            defor2(i,j)=(vib(i-1,j)*scvy(i,j)-via(i,j)*scvy(i-1,j)
     &                  +ujb(i,j-1)*scux(i,j)-uja(i,j)*scux(i,j-1))**2
     &                  *scq2i(i,j)
c
          endif !iq
        enddo !i
      enddo !j
c
c --- define auxiliary del2 fields (dl2via,dl2vib,dl2uja,dl2ujb) to imple-
c --- ment biharmonic sidewall friction along near-vertical bottom slopes.
c
*         wtime2( 9,k) = wtime()
c
c --- rhs: wgtja, wgtjb, dlu2+, wgtia, wgtib, dlv2+
c --- lhs: dl2uja, dl2ujb, dl2via, dl2vib
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,ja,jb,i,ia,ib)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        ja=j-1
        jb=j+1
        do i=1-margin,ii+margin
          if (SEA_U) then
            dl2uja(i,j)=(1.-wgtja(i,j))*dl2u(i,ja)+
     &                  wgtja(i,j)*slip*dl2u(i,j)
            dl2ujb(i,j)=(1.-wgtjb(i,j))*dl2u(i,jb)+
     &                  wgtjb(i,j)*slip*dl2u(i,j)
*RBcRB
*RB            if (ivopn(i-1,j  ).ne.0.and.ivopn(i,j  ).ne.0) then
*RB               dl2uja(i,j) =dl2u(i,j)
*RB            endif
*RB            if (ivopn(i-1,j+1).ne.0.and.ivopn(i,j+1).ne.0) then
*RB               dl2ujb(i,j) =dl2u(i,j)
*RB            endif
          endif !iu
c
          if (SEA_V) then
            ia=i-1
            ib=i+1
            dl2via(i,j)=(1.-wgtia(i,j))*dl2v(ia,j)+
     &                  wgtia(i,j)*slip*dl2v(i, j)
            dl2vib(i,j)=(1.-wgtib(i,j))*dl2v(ib,j)+
     &                  wgtib(i,j)*slip*dl2v(i, j)
*RBcRB
*RB            if (iuopn(i,  j-1).ne.0.and.iuopn(i,  j).ne.0) then
*RB               dl2via(i,j) =dl2v(i,j)
*RB            endif
*RB            if (iuopn(i+1,j-1).ne.0.and.iuopn(i+1,j).ne.0) then
*RB               dl2vib(i,j) =dl2v(i,j)
*RB            endif
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
*         wtime2(10,k) = wtime()
c
      if     (lpipe .and. lpipe_momtum .and. k.eq.1) then
c ---   compare two model runs.
        if     (.not.allocated(mask)) then
          allocate(mask(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))
          call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)/2 )  !integer array
        endif
        do j=1,jj
          do i=1,ii
            mask(i,j)=min(1,iq(i,j)+iu(i,j  )+iv(i,j  )
     &                             +iu(i,j-1)+iv(i-1,j))
          enddo !i
        enddo !j
        write (text,'(a9,i3)') 'potvor k=',k
        call pipe_compare_sym1(potvor,mask,text)
      endif
c
c --- ----------
c --- u equation
c --- ----------
c
c --- deformation-dependent eddy viscosity coefficient
c
*         wtime2(11,k) = wtime()
c
c --- rhs: defor1+, defor2+
c --- lhs: vis2u,vis4u
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,deform)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            deform = sqrt(0.5*(defor1(i,j)+defor1(i-1,j)+
     &                         defor2(i,j)+defor2(i,j+1) ))
c ---       viscosity terms are not additive, use the maximum
            vis2u(i,j)=max(veldf2u(i,j),visco2*deform)
            vis4u(i,j)=max(veldf4u(i,j),visco4*deform)
c
          endif !iu
        enddo !i
      enddo !j
c
*         wtime2(12,k) = wtime()
c
c --- rhs: vis2u+, vis4u+, dl2u+, dpu.m+, dl2uja, wgtja,, dl2ujb, wgtjb
c --- lhs: vis2u, vis4u, uflux1, uflux2, uflux3
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,ja,jb,l,i,
!$OMP&                    dpxy,dpja,dpjb,
!$OMP&                    vis2a,vis4a,vis2b,vis4b,scuya,scuyb)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        ja=j-1
        jb=j+1
        do l=1,isu(j) !ok
          if     (ifu(j,l).gt. 1-nbdy) then
            vis2u(ifu(j,l)-1,j)=vis2u(ifu(j,l),j)
            vis4u(ifu(j,l)-1,j)=vis4u(ifu(j,l),j)
          endif
          if     (ilu(j,l).lt.ii+nbdy) then
            vis2u(ilu(j,l)+1,j)=vis2u(ilu(j,l),j)
            vis4u(ilu(j,l)+1,j)=vis4u(ilu(j,l),j)
          endif
c
c ---     longitudinal turb. momentum flux (at mass points)
c ---     note that hfharm is 0.5*harmonic mean
c
          do i=max(1-margin,ifu(j,l)-1),min(ii+margin,ilu(j,l))
            uflux1(i,j)=
     &        ((vis2u(i,j)+vis2u(i+1,j))*(utotn(i,j)-utotn(i+1,j))+
     &         (vis4u(i,j)+vis4u(i+1,j))*(dl2u( i,j)-dl2u( i+1,j)) )
     &                   *hfharm(max(dpu(i  ,j,k,m),onemm),
     &                           max(dpu(i+1,j,k,m),onemm))
     &                   *scp2(i,j)*2./(scux(i,j)+scux(i+1,j))
c

          enddo !i
c
c --- lateral turb. momentum flux (at vorticity points)
c --- (left and right fluxes are evaluated separately because of sidewalls)
c
          do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
            dpxy=max(dpu(i,j ,k,m),onemm)
            dpja=max(dpu(i,ja,k,m),onemm)
            dpjb=max(dpu(i,jb,k,m),onemm)
c
c --- check whether variables along coast have been initialized correctly
c
            if (iu(i,ja).eq.0) then
              vis2a=vis2u(i,j )
              vis4a=vis4u(i,j )
              scuya=scuy( i,j )
            else
              vis2a=vis2u(i,ja)
              vis4a=vis4u(i,ja)
              scuya=scuy( i,ja)
            endif
            if (iu(i,jb).eq.0) then
              vis2b=vis2u(i,j )
              vis4b=vis4u(i,j )
              scuyb=scuy( i,j )
            else
              vis2b=vis2u(i,jb)
              vis4b=vis4u(i,jb)
              scuyb=scuy( i,jb)
            endif
            uflux2(i,j)=((vis2u(i,j)+vis2a)*(uja(   i,j)-utotn(i,j))+
     &                   (vis4u(i,j)+vis4a)*(dl2uja(i,j)-dl2u( i,j)) )
     &                  *hfharm(dpja+wgtja(i,j)*(dpxy-dpja),dpxy)
     &                  *scq2(i,j )*2./(scuy(i,j)+scuya)
            uflux3(i,j)=((vis2u(i,j)+vis2b)*(utotn(i,j)-ujb(   i,j))+
     &                   (vis4u(i,j)+vis4b)*(dl2u( i,j)-dl2ujb(i,j)) )
     &                  *hfharm(dpjb+wgtjb(i,j)*(dpxy-dpjb),dpxy)
     &                  *scq2(i,jb)*2./(scuy(i,j)+scuyb)
          enddo !i
        enddo !l
      enddo !j
!$OMP END PARALLEL DO
c
c --- pressure force in x direction
c --- ('scheme 2' from appendix -a- in bleck-smith paper)
c
*         wtime2(13,k) = wtime()
c
c --- rhs: depthu, pu, montg+, thstar+, p+, dp.m+
c --- lhs: util1, pgfx
c
      margin = mbdy - 2
c
#if defined(STOKES)
!$OMP PARALLEL DO PRIVATE(j,i,dmontg,dthstr,q,vsd_u)
!$OMP&         SCHEDULE(STATIC,jblk)
#else
!$OMP PARALLEL DO PRIVATE(j,i,dmontg,dthstr,q)
!$OMP&         SCHEDULE(STATIC,jblk)
#endif
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            util1(i,j)=max(0.,min(depthu(i,j)-pu(i,j,k),h1))
            if     (kapref.ne.-1) then
              dmontg=montg( i,j,k,1)-montg( i-1,j,k,1)
              dthstr=thstar(i,j,k,1)-thstar(i-1,j,k,1)
            else !2 thermobaric reference states
              q=0.5*(skap(i,j)+skap(i-1,j))
              dmontg=     q *(montg( i,j,k,1)-montg( i-1,j,k,1))+
     &               (1.0-q)*(montg( i,j,k,2)-montg( i-1,j,k,2))
              dthstr=     q *(thstar(i,j,k,1)-thstar(i-1,j,k,1))+
     &               (1.0-q)*(thstar(i,j,k,2)-thstar(i-1,j,k,2))
            endif
            pgfx(i,j)=util1(i,j)*
     &          ( dmontg+
     &            dthstr*(p(i,j,k+1)*p(i-1,j,k+1)-
     &                    p(i,j,k  )*p(i-1,j,k  ))*thref**2
     &                      /(dp(i,j,k,m)+dp(i-1,j,k,m)+epsil) )
#if defined(STOKES)
            vsd_u = 0.25*(vsd(i,  j,  k)+
     &                    vsd(i-1,j,  k)+
     &                    vsd(i,  j+1,k)+
     &                    vsd(i-1,j+1,k) )
            pgfx(i,j)=pgfx(i,j)+util1(i,j)*
     &          ( dzdx(i,j,k)* 
     &             (usd(i,j,k)*dudzu(i,j,k)+
     &              vsd_u     *dvdzu(i,j,k) ) )
            util3(i,j)=util1(i,j)*
     &          ( dzdx(i,j,k)* 
     &             (usd(i,j,k)*dudzu(i,j,k)+
     &              vsd_u     *dvdzu(i,j,k) ) )
#endif            
          endif !iu
        enddo !i
      enddo !j
c
*     if     (lpipe .and. lpipe_momtum .and. k.eq.1) then
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'pu     k=',k
        call pipe_compare_sym1(pu(1-nbdy,1-nbdy,k),iu,text)
        write (text,'(a9,i3)') 'depthu k=',k
        call pipe_compare_sym1(depthu,iu,text)
        write (text,'(a9,i3)') 'util1  k=',k
        call pipe_compare_sym1(util1, iu,text)
        util4(1:ii,1:jj) = montg( 1:ii,1:jj,k,1)-montg( 0:ii-1,1:jj,k,1)
        write (text,'(a9,i3)') 'montgX k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = thstar(1:ii,1:jj,k,1)-thstar(0:ii-1,1:jj,k,1)
        write (text,'(a9,i3)') 'thstaX k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k)*p(0:ii-1,1:jj,k)
        write (text,'(a9,i3)') 'pSQ    k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k+1)*p(0:ii-1,1:jj,k+1)
        write (text,'(a9,i3)') 'pSQ    k=',k+1
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = dpo(1:ii,1:jj,k,m)+dpo(0:ii-1,1:jj,k,m)+epsil
        write (text,'(a9,i3)') 'dp.m+  k=',k
        call pipe_compare_sym1(util4, iu,text)
        write (text,'(a9,i3)') 'pgfx   k=',k
        call pipe_compare_sym1( pgfx,  iu,text)
#if defined(STOKES)
        write (text,'(a9,i3)') 'pgfxST k=',k
        call pipe_compare_sym1( util3, iu,text)
        write (text,'(a9,i3)') 'dpu.m  k=',k
        call pipe_compare_sym1( dpu(1-nbdy,1-nbdy,k,m),iu,text)
        write (text,'(a9,i3)') 'dzdx   k=',k
        call pipe_compare_sym1( dzdx(1-nbdy,1-nbdy,k),iu,text)
        write (text,'(a9,i3)') 'dudzu  k=',k
        call pipe_compare_sym1( dudzu(1-nbdy,1-nbdy,k),iu,text)
        write (text,'(a9,i3)') 'dvdzu  k=',k
        call pipe_compare_sym1( dvdzu(1-nbdy,1-nbdy,k),iu,text)
#endif            
      endif
c
*         wtime2(14,k) = wtime()
c
c --- rhs: pgfx+, util1+, p+, dpmixl.m+, dpu.m, utotn, drag+
c --- rhs: stresx, u.m, u.n, dpu.n, gradx, utotm+, vtotm+,
c --- rhs: vflux+, potvor+, uflux1+, uflux2, uflux3
c --- lhs: gradx, stress, u.m, u.n
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,ja,jb,i,
!$OMP&                    q,dpun,uhm,uh0,uhp,
!$OMP&                    ptopl,pbotl,pstres,pbop,qdpu,dragu)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        ja=j-1
        jb=j+1
        do i=1-margin,ii+margin
          if (SEA_U) then
c
c --- check whether variables along coast have been initialized correctly
c
            gradx(i,j)=(pgfx(i,j)+(h1-util1(i,j))*
     &        (pgfx (i-1,j)+pgfx (i+1,j)+pgfx (i,ja)+pgfx (i,jb))/
     &        (util1(i-1,j)+util1(i+1,j)+util1(i,ja)+util1(i,jb)+
     &          epsil))/h1
c
            ptopl=min(depthu(i,j),0.5*(p(i,j,k  )+p(i-1,j,k  )))
            pbotl=min(depthu(i,j),0.5*(p(i,j,k+1)+p(i-1,j,k+1)))
            if(hybrid .and. mxlkrt) then
              pstres=0.5*(dpmixl(i,j,m)+dpmixl(i-1,j,m))
            else
              pstres=dpu(i,j,1,m)
            endif
c
c ---       top and bottom boundary layer stress
c ---       drag term is FRAC * Cb * (|v| + c.bar) * onem/dp
c ---        where FRAC is the fraction of the layer to apply stress too
c
            pbop=depthu(i,j)-0.5*(thkbop(i,j)+thkbop(i-1,j)) !top of bot. b.l.
            qdpu=1.0/max(dpu(i,j,k,m),onemm)
            dragu=0.5*(drag(i,j)+drag(i-1,j))*qdpu*
     &                ( max(pbop,    pbotl)
     &                 -max(pbop,min(ptopl,pbotl-onemm)))
            if     (drglim.gt.0.0) then
c             explicit drag: u.t+1 - u.t-1 = - 2dt*dragu*u.t-1
c                   limiter: 2dt*dragu <= drglim
              stress(i,j)=-utotn(i,j)*min(dragu,drglim*dt1inv) +
     &             (stresx(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpu
            else
c             implicit drag: u.t+1 - u.t-1 = - 2dt*dragu*u.t+1
c                                          = - 2dt*dragu / (1+2dt*dragu) * u.t+1
              stress(i,j)=-utotn(i,j)*dragu/(1.0+delt1*dragu) +
     &             (stresx(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpu
            endif
c
            if     (tidflg.gt.0 .and. drgscl.ne.0.0
     &                          .and. thkdrg.gt.0.0) then
              pbop=depthu(i,j)-0.5*(util6(i,j)+util6(i-1,j)) !top of bot. b.l.
              dragu=0.5*(util5(i,j)+util5(i-1,j))*
     &                  ( max(pbop,    pbotl)
     &                   -max(pbop,min(ptopl,pbotl-onemm)))*qdpu
              if     (drglim.gt.0.0) then
                stress(i,j)=stress(i,j)
     &                      -utotn(i,j)*min(dragu,drglim*dt1inv)
                stresl(i,j)=-utotn(i,j)*min(dragu,drglim*dt1inv)
              else
                stress(i,j)=stress(i,j)
     &                      -utotn(i,j)*dragu/(1.0+delt1*dragu)
                stresl(i,j)=-utotn(i,j)*dragu/(1.0+delt1*dragu)
              endif
c ---         !anti-drag on detided velocity (always explicit)
              stress(i,j)=stress(i,j)
     &                      +untide(i,j)*min(dragu,adrlim*dt1inv)
              stresl(i,j)=stresl(i,j)
     &                      +untide(i,j)*min(dragu,adrlim*dt1inv)
            endif !tidflg
c
cdiag       util4(i,j) = u(i,j,k,n)

            u(i,j,k,n) = utotn(i,j) +
     &        delt1*(-scuxi(i,j)*(gradx(i,j)
     &                            +0.25*(utotm(i+1,j  )**2-
     &                                   utotm(i-1,j  )**2+
     &                                   vtotm(i  ,j  )**2+
     &                                   vtotm(i  ,j+1)**2-
     &                                   vtotm(i-1,j  )**2-
     &                                   vtotm(i-1,j+1)**2 ))
     &               +0.125*(vflux(i  ,j)+vflux(i  ,j+1)+
     &                       vflux(i-1,j)+vflux(i-1,j+1) )
     &                     *(potvor(i,j)+potvor(i,j+1))
     &               -ubrhs(i,j)
     &               +stress(i,j)
     &               -(uflux1(i,j)-uflux1(i-1,j)+
     &                 uflux3(i,j)-uflux2(i  ,j) )*qdpu/scu2(i,j) )
#if defined(STOKES)
            u(i,j,k,n) = u(i,j,k,n) +
     &        delt1*(+0.125*(vsdflux(i  ,j)+vsdflux(i  ,j+1)+
     &                       vsdflux(i-1,j)+vsdflux(i-1,j+1) )
     &                     *(potvor(i,j)+potvor(i,j+1))
     &              )
#endif
c
c
c ---       Robert-Asselin time filter of -u- field
c ---       Note that this is smoothing dp * utot,
c ---        but the filter is not conservative over 3 time levels.
            dpun = max(0.0,
     &                 min(depthu(i,j),0.5*(pnkp(i,j)+pnkp(i-1,j)))-
     &                 min(depthu(i,j),0.5*(pnk0(i,j)+pnk0(i-1,j))) )
            uhm  = utotn(i,j)*max(dpu(i,j,k,n),dpthin)
            uh0  = utotm(i,j)*max(dpu(i,j,k,m),dpthin)
            uhp  = u(i,j,k,n)*max(dpun,        dpthin)
            q    = 0.5*ra2fac*(uhm + uhp - 2.0*uh0)
            u(i,j,k,m) = uh0 + q
            u(i,j,k,n) = uhp

          endif !iu
        enddo !i
      enddo !j
c
c --- dissipation per m^2 on p-grid
c
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            if     (tidflg.gt.0 .and. drgscl.ne.0.0
     &                          .and. thkdrg.gt.0.0) then
              displd_mn(i,j) = displd_mn(i,j) + 
     &          qthref*0.5*qonem*dpo(i,j,k,m)*
     &          (utotn(i+1,j)*stresl(i+1,j)+ 
     &           utotn(i,  j)*stresl(i,  j) )
              dispqd_mn(i,j) = dispqd_mn(i,j) + 
     &          qthref*0.5*qonem*dpo(i,j,k,m)*
     &            (utotn(i+1,j)*(stress(i+1,j)-stresl(i+1,j))+ 
     &             utotn(i,  j)*(stress(i,  j)-stresl(i,  j)) )
            else
              dispqd_mn(i,j) = dispqd_mn(i,j) + 
     &          qthref*0.5*qonem*dpo(i,j,k,m)*
     &            (utotn(i+1,j)*stress(i+1,j)+ 
     &             utotn(i,  j)*stress(i,  j) )
            endif
          endif !ip
        enddo !i
      enddo !j
c
      if     (lpipe .and. lpipe_momtum .and. k.eq.1) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'vtotm  k=',k
        call pipe_compare_sym1(vtotm, iv,text)
        write (text,'(a9,i3)') 'utotm  k=',k
        call pipe_compare_sym1(utotm, iu,text)
        write (text,'(a9,i3)') 'gradx  k=',k
        call pipe_compare_sym1(gradx, iu,text)
        write (text,'(a9,i3)') 'ubrhs  k=',k
        call pipe_compare_sym1(ubrhs, iu,text)
        write (text,'(a9,i3)') 'stress k=',k
        call pipe_compare_sym1(stress,iu,text)
        util4(1:ii,1:jj) =  vflux(1:ii,  1:jj  )
     &                     +vflux(0:ii-1,1:jj  )
     &                     +vflux(1:ii,  2:jj+1)
     &                     +vflux(0:ii-1,2:jj+1)
        write (text,'(a9,i3)') 'vflux  k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) =  potvor(1:ii,  1:jj  )
     &                     +potvor(0:ii-1,2:jj+1)
        write (text,'(a9,i3)') 'potvor k=',k
        call pipe_compare_sym1(util4, iu,text)
#if defined(STOKES)
        util4(1:ii,1:jj) =  vsdflux(1:ii,  1:jj  )
     &                     +vsdflux(0:ii-1,1:jj  )
     &                     +vsdflux(1:ii,  2:jj+1)
     &                     +vsdflux(0:ii-1,2:jj+1)
        write (text,'(a9,i3)') 'vsdflx k=',k
        call pipe_compare_sym1(util4, iu,text)
        write (text,'(a9,i3)') 'dvdzu  k=',k
        call pipe_compare_sym1(dvdzu(1-nbdy,1-nbdy,k), iu,text)
        write (text,'(a9,i3)') 'dudzu  k=',k
        call pipe_compare_sym1(dudzu(1-nbdy,1-nbdy,k), iu,text)
#endif /* STOKES */
        util4(1:ii,1:jj) =  uflux1(1:ii,  1:jj)
     &                     -uflux1(0:ii-1,0:jj)
     &                     +uflux3(1:ii,  1:jj)
     &                     -uflux2(1:ii,  1:jj)
        write (text,'(a9,i3)') 'uflux  k=',k
        call pipe_compare_sym1(util4, iu,text)
        write (text,'(a9,i3)') 'u.n    k=',k
        call pipe_compare_sym1(u(1-nbdy,1-nbdy,k,n),iu,text)
      endif
c
 100    format(i9,8x,'uold    unew   gradp  nonlin   corio',
     &3x,'ubrhs  stress    fric')
c
c --- ----------
c --- v equation
c --- ----------
c
c --- deformation-dependent eddy viscosity coefficient
c
*         wtime2(15,k) = wtime()
!$OMP PARALLEL DO PRIVATE(j,i,deform)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            deform = sqrt(0.5*(defor1(i,j)+defor1(i,j-1)+
     &                         defor2(i,j)+defor2(i+1,j) ))
c ---       viscosity terms are not additive, use the maximum
            vis2v(i,j)=max(veldf2v(i,j),visco2*deform)
            vis4v(i,j)=max(veldf4v(i,j),visco4*deform)
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
*         wtime2(16,k) = wtime()
      do i=1-margin,ii+margin
        do l=1,jsv(i) !ok
          if     (jfv(i,l).gt. 1-nbdy) then
            vis2v(i,jfv(i,l)-1)=vis2v(i,jfv(i,l))
            vis4v(i,jfv(i,l)-1)=vis4v(i,jfv(i,l))
          endif
          if     (jlv(i,l).lt.jj+nbdy) then
            vis2v(i,jlv(i,l)+1)=vis2v(i,jlv(i,l))
            vis4v(i,jlv(i,l)+1)=vis4v(i,jlv(i,l))
          endif
        enddo !l
      enddo !i
c
!$OMP PARALLEL DO PRIVATE(j,i,ia,ib,
!$OMP&                    dpxy,dpia,dpib,
!$OMP&                    vis2a,vis4a,vis2b,vis4b,scvxa,scvxb)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
c
c ---   longitudinal turb. momentum flux (at mass points)
c ---   note that hfharm is 0.5*harmonic mean
c
        do i=1-margin,ii+margin
          if (SEA_P) then
            if     (iv(i,j)+iv(i,j+1).gt.0) then
            vflux1(i,j)=
     &        ((vis2v(i,j)+vis2v(i,j+1))*(vtotn(i,j)-vtotn(i,j+1))+
     &         (vis4v(i,j)+vis4v(i,j+1))*(dl2v( i,j)-dl2v( i,j+1)) )
     &                   *hfharm(max(dpv(i,j  ,k,m),onemm),
     &                           max(dpv(i,j+1,k,m),onemm))
     &                   *scp2(i,j)*2./(scvy(i,j)+scvy(i,j+1))
            endif
          endif !ip
        enddo !i
c
c ---   lateral turb. momentum flux (at vorticity points)
c ---   (left and right fluxes are evaluated separately because of sidewalls)
c
        do i=1-margin,ii+margin
          if (SEA_V) then
            ia=i-1
            ib=i+1
            dpxy=max(dpv(i ,j,k,m),onemm)
            dpia=max(dpv(ia,j,k,m),onemm)
            dpib=max(dpv(ib,j,k,m),onemm)
c
c --- check whether variables along coast have been initialized correctly
cdiag       if (k.eq.kk) then
cdiag         if (iv(ia,j).eq.0 .and. dpv(ia,j,k,m).ne.0.) then
cdiag!$OMP CRITICAL
cdiag           write(lp,'(i9,2i5,a,1p,2e9.1)') nstep,i,j,
cdiag     &       '  error - nonzero dpv(ia):',dpv(ia,j,k,m)
cdiag!$OMP END CRITICAL
cdiag         endif
cdiag         if (iv(ib,j).eq.0 .and. dpv(ib,j,k,m).ne.0.) then
cdiag!$OMP CRITICAL
cdiag           write(lp,'(i9,2i5,a,1p,2e9.1)') nstep,i,j,
cdiag     &       '  error - nonzero dpv(ib):',dpv(ib,j,k,m)
cdiag!$OMP END CRITICAL
cdiag         endif
cdiag       endif
            if (iv(ia,j).eq.0) then
              vis2a=vis2v(i ,j)
              vis4a=vis4v(i ,j)
              scvxa=scvx( i, j)
            else
              vis2a=vis2v(ia,j)
              vis4a=vis4v(ia,j)
              scvxa=scvx( ia,j)
            endif
            if (iv(ib,j).eq.0) then
              vis2b=vis2v(i ,j)
              vis4b=vis4v(i ,j)
              scvxb=scvx( i, j)
            else
              vis2b=vis2v(ib,j)
              vis4b=vis4v(ib,j)
              scvxb=scvx( ib,j)
            endif
            vflux2(i,j)=((vis2v(i,j)+vis2a)*(via(   i,j)-vtotn(i,j))+
     &                   (vis4v(i,j)+vis4a)*(dl2via(i,j)-dl2v( i,j)) )
     &                  *hfharm(dpia+wgtia(i,j)*(dpxy-dpia),dpxy)
     &                  *scq2(i ,j)*2./(scvx(i,j)+scvxa)
            vflux3(i,j)=((vis2v(i,j)+vis2b)*(vtotn(i,j)-vib(   i,j))+
     &                   (vis4v(i,j)+vis4b)*(dl2v( i,j)-dl2vib(i,j)) )
     &                  *hfharm(dpib+wgtib(i,j)*(dpxy-dpib),dpxy)
     &                  *scq2(ib,j)*2./(scvx(i,j)+scvxb)
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- pressure force in y direction
c --- ('scheme 2' from appendix -a- in bleck-smith paper)
c
*         wtime2(17,k) = wtime()
#if defined(STOKES)
!$OMP PARALLEL DO PRIVATE(j,i,dmontg,dthstr,q,usd_v)
!$OMP&         SCHEDULE(STATIC,jblk)
#else
!$OMP PARALLEL DO PRIVATE(j,i,dmontg,dthstr,q)
!$OMP&         SCHEDULE(STATIC,jblk)
#endif
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            util2(i,j)=max(0.,min(depthv(i,j)-pv(i,j,k),h1))
            if     (kapref.ne.-1) then
              dmontg=montg( i,j,k,1)-montg( i,j-1,k,1)
              dthstr=thstar(i,j,k,1)-thstar(i,j-1,k,1)
            else !2 thermobaric reference states
              q=0.5*(skap(i,j)+skap(i,j-1))
              dmontg=     q *(montg( i,j,k,1)-montg( i,j-1,k,1))+
     &               (1.0-q)*(montg( i,j,k,2)-montg( i,j-1,k,2))
              dthstr=     q *(thstar(i,j,k,1)-thstar(i,j-1,k,1))+
     &               (1.0-q)*(thstar(i,j,k,2)-thstar(i,j-1,k,2))
            endif
            pgfy(i,j)=util2(i,j)*
     &          ( dmontg+
     &            dthstr*(p(i,j,k+1)*p(i,j-1,k+1)-
     &                    p(i,j,k  )*p(i,j-1,k  ))*thref**2
     &                      /(dp(i,j,k,m)+dp(i,j-1,k,m)+epsil) )
#if defined(STOKES)
            usd_v = 0.25*(usd(i,  j,  k)+
     &                    usd(i,  j-1,k)+
     &                    usd(i-1,j,  k)+
     &                    usd(i-1,j-1,k) )
            pgfy(i,j)=pgfy(i,j)+util2(i,j)*
     &          ( dzdy(i,j,k)* 
     &              (vsd(i,j,k)*dvdzv(i,j,k)+
     &               usd_v     *dudzv(i,j,k) ) )
            util3(i,j)=util2(i,j)*
     &          ( dzdy(i,j,k)* 
     &              (vsd(i,j,k)*dvdzv(i,j,k)+
     &               usd_v     *dudzv(i,j,k) ) )
#endif            
          endif !iv
        enddo !i
      enddo !j
c
*     if     (lpipe .and. lpipe_momtum .and. k.eq.1) then
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'pv     k=',k
        call pipe_compare_sym1(pv(1-nbdy,1-nbdy,k),iv,text)
        write (text,'(a9,i3)') 'depthv k=',k
        call pipe_compare_sym1(depthv,iv,text)
        write (text,'(a9,i3)') 'util2  k=',k
        call pipe_compare_sym1(util2, iv,text)
        util4(1:ii,1:jj) = montg( 1:ii,1:jj,k,1)-montg( 1:ii,0:jj-1,k,1)
        write (text,'(a9,i3)') 'montgY k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = thstar(1:ii,1:jj,k,1)-thstar(1:ii,0:jj-1,k,1)
        write (text,'(a9,i3)') 'thstaY k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k)  *p(1:ii,0:jj-1,k)
        write (text,'(a9,i3)') 'pSQ    k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k+1)*p(1:ii,0:jj-1,k+1)
        write (text,'(a9,i3)') 'pSQ    k=',k+1
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = dp(1:ii,1:jj,k,m)+dp(1:ii,0:jj-1,k,m)+epsil
        write (text,'(a9,i3)') 'dp.m+  k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'pgfy   k=',k
        call pipe_compare_sym1(pgfy,  iv,text)
#if defined(STOKES)
        write (text,'(a9,i3)') 'pgfyST k=',k
        call pipe_compare_sym1(util3, iv,text)

        write (text,'(a9,i3)') 'dpv.m  k=',k
        call pipe_compare_sym1( dpv(1-nbdy,1-nbdy,k,m),iv,text)
        write (text,'(a9,i3)') 'dzdy   k=',k
        call pipe_compare_sym1( dzdy(1-nbdy,1-nbdy,k),iu,text)
        write (text,'(a9,i3)') 'dvdzv  k=',k
        call pipe_compare_sym1( dvdzv(1-nbdy,1-nbdy,k),iu,text)
        write (text,'(a9,i3)') 'dudzv  k=',k
        call pipe_compare_sym1( dudzv(1-nbdy,1-nbdy,k),iu,text)
#endif            
      endif
c
*         wtime2(18,k) = wtime()
!$OMP PARALLEL DO PRIVATE(j,i,ia,ib)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            ia=i-1
            ib=i+1
c
c --- check whether variables along coast have been initialized correctly
c
            grady(i,j)=(pgfy(i,j)+(h1-util2(i,j))*
     &        (pgfy (ia ,j)+pgfy (ib ,j)
     &        +pgfy (i,j-1)+pgfy (i,j+1))/
     &        (util2(ia ,j)+util2(ib ,j)
     &        +util2(i,j-1)+util2(i,j+1)+epsil))/h1
          endif !iv
        enddo !i
      enddo !j
c
*         wtime2(19,k) = wtime()
!$OMP PARALLEL DO PRIVATE(j,i,
!$OMP&                    q,dpvn,vhm,vh0,vhp,
!$OMP&                    ptopl,pbotl,pstres,pbop,qdpv,dragv)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            ia=i-1
            ib=i+1
            ptopl=min(depthv(i,j),0.5*(p(i,j,k  )+p(i,j-1,k  )))
            pbotl=min(depthv(i,j),0.5*(p(i,j,k+1)+p(i,j-1,k+1)))
            if(hybrid .and. mxlkrt) then
              pstres=0.5*(dpmixl(i,j,m)+dpmixl(i,j-1,m))
            else
              pstres=dpv(i,j,1,m)
            endif
c
c ---       top and bottom boundary layer stress
c ---       bottom drag term is FRAC * Cb * (|v| + c.bar) * onem/dp
c ---        where FRAC is the fraction of the layer to apply drag too
c
            pbop=depthv(i,j)-0.5*(thkbop(i,j)+thkbop(i,j-1))  !top of bot. b.l.
            qdpv=1.0/max(dpv(i,j,k,m),onemm)
            dragv=0.5*(drag(i,j)+drag(i,j-1))*qdpv*
     &                ( max(pbop,    pbotl)
     &                 -max(pbop,min(ptopl,pbotl-onemm)))
            if     (drglim.gt.0.0) then
              stress(i,j)=-vtotn(i,j)*min(dragv,drglim*dt1inv) +
     &             (stresy(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpv
            else
              stress(i,j)=-vtotn(i,j)*dragv/(1.0+delt1*dragv) +
     &             (stresy(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpv
            endif
c
            if     (tidflg.gt.0 .and. drgscl.ne.0.0
     &                          .and. thkdrg.gt.0.0) then
              pbop=depthv(i,j)-0.5*(util6(i,j)+util6(i,j-1))  !top of bot. b.l.
              dragv=0.5*(util5(i,j)+util5(i,j-1))*
     &                  ( max(pbop,    pbotl)
     &                   -max(pbop,min(ptopl,pbotl-onemm)))*qdpv
              if     (drglim.gt.0.0) then
                stress(i,j)=stress(i,j)
     &                      -vtotn(i,j)*min(dragv,drglim*dt1inv)
                stresl(i,j)=-vtotn(i,j)*min(dragv,drglim*dt1inv)
              else
                stress(i,j)=stress(i,j)
     &                      -vtotn(i,j)*dragv/(1.0+delt1*dragv)
                stresl(i,j)=-vtotn(i,j)*dragv/(1.0+delt1*dragv)
              endif
c ---         !anti-drag on detided velocity (always explicit)
              stress(i,j)=stress(i,j)
     &                      +vntide(i,j)*min(dragv,adrlim*dt1inv)
              stresl(i,j)=stresl(i,j)
     &                      +vntide(i,j)*min(dragv,adrlim*dt1inv)
      endif !tidflg
c
cdiag       util4(i,j) = v(i,j,k,n)
            v(i,j,k,n) = vtotn(i,j) +
     &        delt1*(-scvyi(i,j)*(grady(i,j)
     &                            +0.25*(vtotm(i,  j+1)**2-
     &                                   vtotm(i,  j-1)**2+
     &                                   utotm(i,  j  )**2+
     &                                   utotm(i+1,j  )**2-
     &                                   utotm(i,  j-1)**2-
     &                                   utotm(i+1,j-1)**2 ))
     &               -0.125*(uflux(i,  j  )+uflux(i+1,j  )+
     &                       uflux(i,  j-1)+uflux(i+1,j-1) )
     &                     *(potvor(i,j)+potvor(i+1,j))
     &               -vbrhs(i,j)
     &               +stress(i,j)
     &               -(vflux1(i,j)-vflux1(i,j-1)+
     &                 vflux3(i,j)-vflux2(i,j  ) )*qdpv/scv2(i,j) )
#if defined(STOKES)
            v(i,j,k,n) = v(i,j,k,n) +
     &        delt1*(-0.125*(usdflux(i,  j  )+usdflux(i+1,j  )+
     &                       usdflux(i,  j-1)+usdflux(i+1,j-1) )
     &                     *(potvor(i,j)+potvor(i+1,j))
     &              )
#endif
c
c ---       Robert-Asselin time filter of -v- field
c ---       Note that this is smoothing dp * vtot,
c ---        but the filter is not conservative over 3 time levels.
            dpvn = max(0.0,
     &                 min(depthv(i,j),0.5*(pnkp(i,j)+pnkp(i,j-1)))-
     &                 min(depthv(i,j),0.5*(pnk0(i,j)+pnk0(i,j-1))) )
            vhm  = vtotn(i,j)*max(dpv(i,j,k,n),dpthin)
            vh0  = vtotm(i,j)*max(dpv(i,j,k,m),dpthin)
            vhp  = v(i,j,k,n)*max(dpvn,        dpthin)
            q    = 0.5*ra2fac*(vhm + vhp - 2.0*vh0)
            v(i,j,k,m) = vh0 + q
            v(i,j,k,n) = vhp
          endif !iv
        enddo !i
      enddo !j
c
c --- dissipation per m^2 on p-grid
c
      do j=1,jj
        do i=1,ii
          if (SEA_P) then
            if     (tidflg.gt.0 .and. drgscl.ne.0.0
     &                          .and. thkdrg.gt.0.0) then
              displd_mn(i,j) = displd_mn(i,j) + 
     &          qthref*0.5*qonem*dpo(i,j,k,m)*
     &          (vtotn(i,j+1)*stresl(i,j+1)+ 
     &           vtotn(i,j)  *stresl(i,j)   )
              dispqd_mn(i,j) = dispqd_mn(i,j) + 
     &          qthref*0.5*qonem*dpo(i,j,k,m)*
     &            (vtotn(i,j+1)*(stress(i,j+1)-stresl(i,j+1))+ 
     &             vtotn(i,j)  *(stress(i,j)  -stresl(i,j)  ) )
            else
              dispqd_mn(i,j) = dispqd_mn(i,j) + 
     &          qthref*0.5*qonem*dpo(i,j,k,m)*
     &            (vtotn(i,j+1)*stress(i,j+1)+ 
     &             vtotn(i,j)  *stress(i,j)   )
            endif
          endif !ip
        enddo !i
      enddo !j
c
*     if     (lpipe .and. lpipe_momtum .and. k.eq.1) then
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        util4(1:ii,1:jj) =  vtotm(1:ii,  2:jj+1)**2
     &                     +utotm(1:ii,  1:jj  )**2
     &                     +utotm(2:ii+1,1:jj  )**2
     &                     -vtotm(1:ii,  0:jj-1)**2
     &                     -utotm(1:ii,  0:jj-1)**2
     &                     -utotm(2:ii+1,0:jj-1)**2
        write (text,'(a9,i3)') 'totm   k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'grady  k=',k
        call pipe_compare_sym1(grady, iv,text)
        write (text,'(a9,i3)') 'vbrhs  k=',k
        call pipe_compare_sym1(vbrhs, iv,text)
        if     (k.eq.kk) then
          write (text,'(a9,i3)') 'p      k=',k-1
          call pipe_compare_sym1(p(1-nbdy,1-nbdy,k-1),ip,text)
          write (text,'(a9,i3)') 'p      k=',k
          call pipe_compare_sym1(p(1-nbdy,1-nbdy,k)  ,ip,text)
          write (text,'(a9,i3)') 'drag   k=',k
          call pipe_compare_sym1(drag,ip,text)
        endif
        write (text,'(a9,i3)') 'stress k=',k
        call pipe_compare_sym1(stress,iv,text)
        util4(1:ii,1:jj) =  uflux(1:ii,  1:jj  )
     &                     +uflux(2:ii+1,1:jj  )
     &                     +uflux(1:ii,  0:jj-1)
     &                     +uflux(2:ii+1,0:jj-1)
        write (text,'(a9,i3)') 'uflux  k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) =  potvor(1:ii,  1:jj  )
     &                     +potvor(2:ii+1,1:jj  )
        write (text,'(a9,i3)') 'potvor k=',k
        call pipe_compare_sym1(util4, iv,text)
#if defined(STOKES)
        util4(1:ii,1:jj) =  usdflux(1:ii,  1:jj  )
     &                     +usdflux(2:ii+1,1:jj  )
     &                     +usdflux(1:ii,  0:jj-1)
     &                     +usdflux(2:ii+1,0:jj-1)
        write (text,'(a9,i3)') 'usdflx k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'dvdzv  k=',k
        call pipe_compare_sym1(dvdzv(1-nbdy,1-nbdy,k), iv,text)
        write (text,'(a9,i3)') 'dudzv  k=',k
        call pipe_compare_sym1(dudzv(1-nbdy,1-nbdy,k), iv,text)
#endif /* STOKES */
        util4(1:ii,1:jj) =  vflux1(1:ii,  1:jj  )
     &                     -vflux1(1:ii,  0:jj-1)
     &                     +vflux3(1:ii,  1:jj  )
     &                     -vflux2(1:ii,  1:jj  )
        write (text,'(a9,i3)') 'vflux  k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'v.n    k=',k
        call pipe_compare_sym1(v(1-nbdy,1-nbdy,k,n),iv,text)
      endif
c
 101    format(i9,8x,'vold    vnew   gradp  nonlin   corio',
     &3x,'vbrhs  stress    fric')
c
*         wtime2(20,k) = wtime()
 9    continue  ! k=1,kk
c
c --- update dpu.m,dpv.m to time level t   with RA time smoother
c --- update dpu.n,dpv.n to time level t+1
c
      margin = 1
c
*        wtime1( 7) = wtime()
c --- p is from dp.m, see momtum_hs
      call dpudpv(dpu(1-nbdy,1-nbdy,1,m),
     &            dpv(1-nbdy,1-nbdy,1,m),
     &            p,depthu,depthv, margin,max(0,margin-1))
c
!$OMP PARALLEL DO PRIVATE(j,i,k)
!$OMP&           SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            do k=1,kk
              p(i,j,k+1) = p(i,j,k) + dp(i,j,k,n)
            enddo !k
          endif !ip
        enddo !i
      enddo !j
c
      call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &            dpv(1-nbdy,1-nbdy,1,n),
     &            p,depthu,depthv, margin,max(0,margin-1))
c
c --- convert from dpv*vtot to vtot to v
c --- substitute depth-weighted averages at massless grid points.
c --- extract barotropic velocities generated during most recent 
c --- baroclinic time step and use them to force barotropic flow field.
c
      margin = 0
c
!$OMP PARALLEL DO PRIVATE(j,i,k,q,sum_m,sum_n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            k=1
              u(i,j,k,m) = u(i,j,k,m)/max(dpu(i,j,k,m),dpthin)
              u(i,j,k,n) = u(i,j,k,n)/max(dpu(i,j,k,n),dpthin)
              sum_m      = u(i,j,k,m)*    dpu(i,j,k,m)
              sum_n      = u(i,j,k,n)*    dpu(i,j,k,n)
            do k=2,kk
              u(i,j,k,m) = u(i,j,k,m)/max(dpu(i,j,k,m),dpthin)
              q          = min(dpu(i,j,k,m),cutoff)
              u(i,j,k,m) = (u(i,j,k,m)*q+u(i,j,k-1,m)*(cutoff-q))*
     &                     qcutoff
              u(i,j,k,n) = u(i,j,k,n)/max(dpu(i,j,k,n),dpthin)
              q          = min(dpu(i,j,k,n),cutoff)
              u(i,j,k,n) = (u(i,j,k,n)*q+u(i,j,k-1,n)*(cutoff-q))*
     &                     qcutoff
              sum_m      = sum_m + u(i,j,k,m)*dpu(i,j,k,m)
              sum_n      = sum_n + u(i,j,k,n)*dpu(i,j,k,n)
            enddo !k
c
            sum_m = sum_m/depthu(i,j)
            sum_n = sum_n/depthu(i,j)
            do k=1,kk
              u(i,j,k,m) = u(i,j,k,m) - sum_m
              u(i,j,k,n) = u(i,j,k,n) - sum_n
            enddo !k
c
            utotn(i,j) = dt1inv*(sum_n - ubavg(i,j,n))  !for barotp
          endif !iu
c
          if (SEA_V) then
            k=1
              v(i,j,k,m) = v(i,j,k,m)/max(dpv(i,j,k,m),dpthin)
              v(i,j,k,n) = v(i,j,k,n)/max(dpv(i,j,k,n),dpthin)
              sum_m      = v(i,j,1,m)*    dpv(i,j,1,m)
              sum_n      = v(i,j,1,n)*    dpv(i,j,1,n)
            do k=2,kk
              v(i,j,k,m) = v(i,j,k,m)/max(dpv(i,j,k,m),dpthin)
              q          = min(dpv(i,j,k,m),cutoff)
              v(i,j,k,m) = (v(i,j,k,m)*q+v(i,j,k-1,m)*(cutoff-q))*
     &                     qcutoff
              v(i,j,k,n) = v(i,j,k,n)/max(dpv(i,j,k,n),dpthin)
              q          = min(dpv(i,j,k,n),cutoff)
              v(i,j,k,n) = (v(i,j,k,n)*q+v(i,j,k-1,n)*(cutoff-q))*
     &                     qcutoff
              sum_m      = sum_m + v(i,j,k,m)*dpv(i,j,k,m)
              sum_n      = sum_n + v(i,j,k,n)*dpv(i,j,k,n)
            enddo !k
c
            sum_m = sum_m/depthv(i,j)
            sum_n = sum_n/depthv(i,j)
            do k=1,kk
              v(i,j,k,m) = v(i,j,k,m) - sum_m
              v(i,j,k,n) = v(i,j,k,n) - sum_n
            enddo !k
c
            vtotn(i,j) = dt1inv*(sum_n - vbavg(i,j,n))  !for barotp
          endif !iv
        enddo !i
      enddo !j - do 30 loop
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        do k= 1,kk
          write (text,'(a9,i3)') 'dpv.m  k=',k
          call pipe_compare_sym1(dpv(1-nbdy,1-nbdy,k,m),iv,text)
          write (text,'(a9,i3)') 'dpv.n  k=',k
          call pipe_compare_sym1(dpv(1-nbdy,1-nbdy,k,n),iv,text)
          write (text,'(a9,i3)') 'v.n(2) k=',k
          call pipe_compare_sym1(  v(1-nbdy,1-nbdy,k,n),iv,text)
        enddo !k
        write (text,'(a9,i3)') 'vtotn  k=',kk
        call pipe_compare_sym1(vtotn, iv,text)
      endif
*
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'v.n(3) k=',1
        call pipe_compare_sym1(v(1-nbdy,1-nbdy,1,n),iv,text)
      endif
c
      return
      end subroutine momtum
c
      subroutine momtum4(m,n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_pipe       ! HYCOM debugging interface
      use mod_tides      ! HYCOM tides
      implicit none
c
      integer m,n
c
c --- ---------------------------------------------------------
c --- momentum equations (4th order version)
c
c --- Winther, N.G., Y.G. Morel, G. Evensen (2007)
c --- Efficiency of high order numerical schemes
c --- for momentum advection.
c --- Journal of Marine Systems 67 (2007) 31-46
c
c --- diffusion is biharmonic with speed dependent and
c --- deformation dependent coefficients.
c
c --- hydrostatic equation and surface stress via momtum_hs
c --- ---------------------------------------------------------
c --- on entry:
c ---  saln(:,:,:,m) = time step t   with RA time smoothing
c ---  saln(:,:,:,n) = time step t+1
c
c ---    dp(:,:,:,m) = time step t   with RA time smoothing
c ---    dp(:,:,:,n) = time step t+1
c
c ---   dpo(:,:,:,n) = time step t-1
c ---   dpo(:,:,:,m) = time step t
c
c ---   dpv(:,:,:,n) = time step t-1
c ---   dpv(:,:,:,m) = time step t
c ---   dpu(:,:,:,n) = time step t-1
c ---   dpu(:,:,:,m) = time step t
c
c ---     v(:,:,:,n) = time step t-1
c ---     v(:,:,:,m) = time step t
c ---     u(:,:,:,n) = time step t-1
c ---     u(:,:,:,m) = time step t
c
c --- on exit:
c ---   dpv(:,:,:,m) = time step t   with RA time smoothing
c ---   dpv(:,:,:,n) = time step t+1
c ---   dpu(:,:,:,m) = time step t   with RA time smoothing
c ---   dpu(:,:,:,n) = time step t+1
c
c ---     v(:,:,:,m) = time step t   with RA time smoothing
c ---     v(:,:,:,n) = time step t+1
c ---     u(:,:,:,m) = time step t   with RA time smoothing
c ---     u(:,:,:,n) = time step t+1
c
c --- vtotn(:,:)     = time step t-1 to t+1 barotropic tendency
c --- utotn(:,:)     = time step t-1 to t+1 barotropic tendency
c --- ---------------------------------------------------------
c
      logical, parameter :: lpipe_momtum=.false.  !usually .false.
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
     &                 scuyi,scvxi,cflp,viscp,viscq,
     &                 advu,diffu,advv,diffv,
     &                 pnk0,pnkp,stresl
#else
      real, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &                 scuyi,scvxi,cflp,viscp,viscq,
     &                 advu,diffu,advv,diffv,
     &                 dl2u,dl2uja,dl2ujb,dl2v,dl2via,dl2vib,  !not used
     &                 pnk0,pnkp,stresl
      common/momtumr4/ 
     &                 scuyi,scvxi,cflp,viscp,viscq,
     &                 advu,diffu,advv,diffv,
     &                 dl2u,dl2uja,dl2ujb,dl2v,dl2via,dl2vib,  !not used
     &                 pnk0,pnkp,stresl
      save  /momtumr4/
#endif
c
      real, dimension(1-nbdy:idm+jdm+nbdy) ::  !want max(idm,jdm)
     &       uwim,uwin,vwim,vwin, !not saved, OMP private
     &       dun,dum,dvn,dvm      !not saved, OMP private
      real, dimension(-2:2) ::
     &       uwjm,uwjn,vwjm,vwjn  !not saved, OMP private
c
      integer i,ia,ib,im1,ip1,
     &        j,ja,jb,jm1,jp1,k,ka,l,mbdy,margin
c
      real    vmag,dall,adrlim,ptopl,pbotl,cutoff,qcutoff,h1,q,
     &        dt1inv,phi,plo,pbop,pthkbl,ubot,vbot,pstres,
     &        dmontg,dthstr,dragu,dragv,qdpu,qdpv,dpthin,
     &        dpun,uhm,uh0,uhp,dpvn,vhm,vh0,vhp,sum_m,sum_n,
     &        scpmin,scpasp
c
      real    utotja,utotjb,vtotia,vtotib,wia,wib,wja,wjb
      real    clpn,clmn,crpn,crmn,clpm,crmm
      real    upij,vpij,velmodul,defortot
      real    ulimmax,hmindiff
      real    q12,s00,s01,s10,s11
c
*     real*8    wtime
*     external  wtime
*     real*8    wtime1(10),wtime2(20,kdm),wtimes
c
      character text*12
      integer, save, allocatable, dimension(:,:) ::
     &  mask
c
      integer ifirst
      save    ifirst
c
*     include 'stmt_fns.h'
c
      data ifirst / 0 /
c

#if defined(STOKES)
      write(6,*)'Subroutine momtum4 does not incorporate Stokes Drift'
      call xcstop('(momtum4)')
#endif
#if defined(RELO)
      if     (.not.allocated(stress)) then
        allocate(
     &          stress(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          stresx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          stresy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            dpmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          thkbop(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           oneta(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 6*(idm+2*nbdy)*(jdm+2*nbdy) )
                stress = r_init
                stresx = r_init
                stresy = r_init
                  dpmx = r_init
                thkbop = r_init
                 oneta = r_init
      endif !stress
      if     (.not.allocated(scuyi)) then
        allocate(
     &            scuyi(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            scvxi(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             cflp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            viscp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            viscq(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             advu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            diffu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             advv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            diffv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             pnk0(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &             pnkp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           stresl(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 12*(idm+2*nbdy)*(jdm+2*nbdy) )
                  scuyi = 0.0  !r_init
                  scvxi = 0.0  !r_init
                   cflp = 0.0  !r_init
                  viscp = 0.0  !r_init
                  viscq = 0.0  !r_init
                   advu = 0.0  !r_init
                  diffu = 0.0  !r_init
                   advv = 0.0  !r_init
                  diffv = 0.0  !r_init
                   pnk0 = 0.0  !r_init
                   pnkp = 0.0  !r_init
                 stresl = 0.0  !r_init
      endif !scuyi
#endif
c
      mbdy = 6
c
      call xctilr(dpu(  1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_us)
      call xctilr(dpv(  1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vs)
      call xctilr(u(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_uv)
      call xctilr(v(    1-nbdy,1-nbdy,1,1),1,2*kk, 6,6, halo_vv)
      call xctilr(ubavg(1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_uv)
      call xctilr(vbavg(1-nbdy,1-nbdy,1  ),1,   2, 6,6, halo_vv)
c
      if     (ifirst.eq.0) then
        ifirst=1
        margin = mbdy
c
!$OMP PARALLEL DO PRIVATE(j,i,scpmin,scpasp)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            pu(i,j,1) = 0.0
            pv(i,j,1) = 0.0
c
            defor1(i,j) = 0.0
            defor2(i,j) = 0.0
            viscp(i,j) = 0.0
            viscq(i,j) = 0.0
c
            scuyi(i,j) = 1.0/scuy(i,j)
            scvxi(i,j) = 1.0/scvx(i,j)
c
c ---       CFL constraint on 2D explicit biharmonic viscosity, on p-grid
c ---       CFL limit on B is DX^4/32DT, use 75% of this value
c ---       where DX=min(dx,dy) and max(dx,dy) is used for calculating B
            scpmin = min(scpx(i,j),scpy(i,j))
            scpasp = scpmin / max(scpx(i,j),scpy(i,j),epsil)  !<=1
            cflp(i,j) = (0.75/(32.0*delt1)) * scpmin * scpasp**3
          enddo !i
        enddo !j
      endif !ifirst
c
c ---------------------------------
c     First initialize fields
c ---------------------------------
c
      dpthin  = 0.001*onemm
      hmindiff=       tenm
      h1      =       tenm  !used in lateral weighting of hor.pres.grad.
      cutoff  =   0.5*onem
      qcutoff = 1.0/cutoff
      q12     = 1.0/12.0
c
c --- ---------------------------------------
c --- hydrostatic equation and surface stress
c --- ---------------------------------------
c
      call momtum_hs(m,n)
c
c +++ ++++++++++++++++++
c +++ momentum equations
c +++ ++++++++++++++++++
c
*        wtime1( 4) = wtime()
c
c --- rhs: p, u.n+, v.n+, ubavg.n+, vbavg.n+, depthv+, pvtrop+
c --- rhs: dpmixl.m+, taux+, dpu, depthu+, dpv, tauy+
c --- lhs: util1, util2, drag, ubrhs, stresx, vbrhs, stresy
c
      if     (drglim.gt.0.0) then
        adrlim = drglim
      else
        adrlim = 0.125
      endif
      dt1inv = 1./delt1
c
      margin = mbdy - 1
c
!$OMP PARALLEL DO PRIVATE(j,i,k,
!$OMP&                    phi,plo,pbop,ubot,vbot,vmag,dall,pstres)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
c     
        do i=1-margin,ii+margin
          if (SEA_P) then
c
c ---       bottom drag (standard bulk formula)
c ---       bottom stress is applied over thickness thkbot (or the
c ---        total depth is this is less)
c
            thkbop(i,j)=min( thkbot*onem, p(i,j,kk+1) )
c
c ---       the bottom stress term is estimated using velocity averaged
c ---       over the bottom boundary layer. this thickness is dpbbl for
c ---       the kpp boundary layer; otherwise, it is thkbop
c
            ubot=0.0
            vbot=0.0
            if (mxlkpp .and. bblkpp) then
              pthkbl=max(dpbbl(i,j),thkbop(i,j))  !thknss of bot. b.l.
            else
              pthkbl=thkbop(i,j)                  !thknss of bot. b.l.
            endif
            pbop=p(i,j,kk+1)-pthkbl               !top of bot. b.l.
            phi =max(p(i,j,1),pbop)
            do k=1,kk
              plo =phi  ! max(p(i,j,k),pbop)
              phi =max(p(i,j,k+1),pbop)
              ubot=ubot + (u(i,j,k,n)+u(i+1,j,k,n))*(phi-plo)
              vbot=vbot + (v(i,j,k,n)+v(i,j+1,k,n))*(phi-plo)
            enddo !k
            ubot=ubot/min(pthkbl,p(i,j,kk+1))
     &            + (ubavg(i,j,n)+ubavg(i+1,j,n))
            vbot=vbot/min(pthkbl,p(i,j,kk+1))
     &            + (vbavg(i,j,n)+vbavg(i,j+1,n))
c
c ---       drag = Cb * (|v| + c.bar)
c ---       include 1/thkbop for the fraction of layer calculation
c ---        and onem for conversion from 1/dp to 1/h
c
            vmag=0.5*sqrt(ubot**2+vbot**2)
            dall=cbp(i,j)*(vmag+cbarp(i,j))  !no tidal drag
            drag(i,j)=dall*onem/thkbop(i,j)
            if (mxlkpp .and. bblkpp) then
              ustarb(i,j)=sqrt(dall*vmag)
            endif
c
c ---       tidal bottom drag, drgten.1.1 is drgscl*rh
c
            util6(i,j)=max(0.1,thkdrg)*onem  !drag applied over this thknss
            util5(i,j)=drgten(1,1,i,j)/min(util6(i,j)*qonem,depths(i,j))
          endif !ip
        enddo !i
c         
c ---   store r.h.s. of barotropic u/v eqn. in -ubrhs,vbrhs-
c ---   time-interpolate wind stress
c
        do i=1-margin,ii+margin
          if (SEA_U) then
            ubrhs(i,j)=
     &        (vbavg(i  ,j,  m)*depthv(i  ,j)
     &        +vbavg(i  ,j+1,m)*depthv(i  ,j+1)
     &        +vbavg(i-1,j,  m)*depthv(i-1,j)
     &        +vbavg(i-1,j+1,m)*depthv(i-1,j+1))
     &        *(pvtrop(i,j)+pvtrop(i,j+1))*.125
c    
            if     (windf) then
              if(hybrid .and. mxlkrt) then
                pstres=0.5*(dpmixl(i,j,m)+dpmixl(i-1,j,m))
              else
                pstres=dpu(i,j,1,m)
              endif
c ---         units of surtx are N/m^2 (i.e. Pa)
              stresx(i,j)=(surtx(i,j)+surtx(i-1,j))*0.5*g
     &                    /(pstres*thref)
            else  ! no taux
              stresx(i,j)=0.
            endif !windf:else
          endif !iu
        enddo !i
        do i=1-margin,ii+margin
          if (SEA_V) then
            vbrhs(i,j)=
     &      -(ubavg(i,  j  ,m)*depthu(i,j  )
     &       +ubavg(i+1,j  ,m)*depthu(i+1,j  )
     &       +ubavg(i,  j-1,m)*depthu(i,j-1)
     &       +ubavg(i+1,j-1,m)*depthu(i+1,j-1))
     &       *(pvtrop(i,j)+pvtrop(i+1,j))*.125
c    
            if     (windf) then
              if(hybrid .and. mxlkrt) then
                pstres=0.5*(dpmixl(i,j,m)+dpmixl(i,j-1,m))
              else
                pstres=dpv(i,j,1,m)
              endif
c ---         units of surty are N/m^2 (i.e. Pa)
              stresy(i,j)=(surty(i,j)+surty(i,j-1))*0.5*g
     &                    /(pstres*thref)
            else  ! no tauy
              stresy(i,j)=0.
            endif !windf:else
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'uba.n  n=',n
        call pipe_compare_sym1(ubavg(1-nbdy,1-nbdy,n),iu,text)
        write (text,'(a9,i3)') 'vba.n  n=',n
        call pipe_compare_sym1(vbavg(1-nbdy,1-nbdy,n),iv,text)
        write (text,'(a9,i3)') 'drag   k=',0
        call pipe_compare_sym1(drag,ip,text)
      endif
c
c --- the old  momeq2.f  starts here
c
c --- rhs: 0.0
c --- lhs: util1, util2
c
      margin = mbdy
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
c ---   spatial weighting function for pressure gradient calculation:
          util1(i,j)=0.0
          util2(i,j)=0.0
c ---     p.1
          pnkp( i,j)=0.0
c
          utotn(i,j)=0.0
          utotm(i,j)=0.0
          vtotn(i,j)=0.0
          vtotm(i,j)=0.0
        enddo !i
      enddo !j
c
      do 9 k=1,kk
c
c --- store total (barotropic plus baroclinic) flow at old and mid time in
c --- -utotn,vtotn- and -utotm,vtotm- respectively. store minimum thickness
c --- values for use in pot.vort. calculation in -dpmx-.
c --- store p.k and p.k+1 for time level t+1 in pnk0,pnkp
c
*         wtime2( 1,k) = wtime()
c
c --- rhs: dpmx, dp.m+
c --- lhs: dpmx
c
      margin = mbdy - 1
c
      do i=1-margin,ii+margin
        dpmx(i,1-margin)=2.*cutoff
      enddo !i
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          dpmx(i,j+1)=2.*cutoff
        enddo !i
        do i=1-margin,ii+margin
          if (SEA_U) then
            dpmx(i,j+1)=max(dpmx(i,j+1),dpo(i,j,k,m)+dpo(i-1,j,k,m))
          endif !iu
          if (SEA_P) then
            pnk0(i,j)=pnkp(i,j)
            pnkp(i,j)=pnk0(i,j)+dp(i,j,k,n)
          endif !ip
        enddo !i
      enddo !j
c       
*         wtime2( 2,k) = wtime()
c
c --- rhs: ubavg.m, ubavg.n, dp.m+, dpu
c --- lhs: utotm, utotn, uflux, dpmx, pu
c
      margin = mbdy - 1
c
!$OMP PARALLEL DO PRIVATE(j,l,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do l=1,isu(j) !ok
c       
          i=ifu(j,l)-1
          if     (i.ge.1-margin .and. i.le.ii+margin) then
            if     (iuopn(i,j).ne.0) then !open boundary
              utotm(i,j)=u(i+1,j,k,m)+ubavg(i,j,m)
              utotn(i,j)=u(i+1,j,k,n)+ubavg(i,j,n)
              uflux(i,j)=utotm(i,j)*max(dpo(i,j,k,m),cutoff)
            endif
          endif
          i=ilu(j,l)+1
          if     (i.ge.1-margin .and. i.le.ii+margin) then
            if     (iuopn(i,j).ne.0) then !open boundary
              utotm(i,j)=u(i-1,j,k,m)+ubavg(i,j,m)
              utotn(i,j)=u(i-1,j,k,n)+ubavg(i,j,n)
              uflux(i,j)=utotm(i,j)*max(dpo(i-1,j,k,m),cutoff)
            endif
          endif
c           
          do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
            dpmx(i,j)=max(dpmx(i,j),dpo(i,j,k,m)+dpo(i-1,j,k,m))
            utotm(i,j)=u(i,j,k,m)+ubavg(i,j,m)
            utotn(i,j)=u(i,j,k,n)+ubavg(i,j,n)
            uflux(i,j)=utotm(i,j)*max(dpu(i,j,k,m),cutoff)
            pu(i,j,k+1)=pu(i,j,k)+dpu(i,j,k,m)
          enddo !i
        enddo !l
      enddo !j
!$OMP END PARALLEL DO
c     
*         wtime2( 3,k) = wtime()
c
c --- rhs: vbavg.m, vbavg.n, dp.m+, dpv
c --- lhs: vtotm, vtotn, vflux, dpmx, pv
c
      margin = mbdy - 1
c
      do i=1-margin,ii+margin
        do l=1,jsv(i) !ok
          j=jfv(i,l)-1
          if     (j.ge.1-margin .and. j.le.jj+margin) then
            if      (ivopn(i,j).ne.0) then !open boundary
              vtotm(i,j)=v(i,j+1,k,m)+vbavg(i,j,m)
              vtotn(i,j)=v(i,j+1,k,n)+vbavg(i,j,n)
              vflux(i,j)=vtotm(i,j)*max(dpo(i,j,k,m),cutoff)
            endif
          endif
          j=jlv(i,l)+1
          if     (j.ge.1-margin .and. j.le.jj+margin) then
            if      (ivopn(i,j).ne.0) then !open boundary
              vtotm(i,j)=v(i,j-1,k,m)+vbavg(i,j,m)
              vtotn(i,j)=v(i,j-1,k,n)+vbavg(i,j,n)
              vflux(i,j)=vtotm(i,j)*max(dpo(i,j-1,k,m),cutoff)
            endif
          endif
        enddo !l
      enddo !i
c       
*         wtime2( 4,k) = wtime()
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            dpmx(i  ,j)=max(dpmx(i  ,j),dpo(i,j,k,m)+dpo(i,j-1,k,m))
            dpmx(i+1,j)=max(dpmx(i+1,j),dpo(i,j,k,m)+dpo(i,j-1,k,m))
            vtotm(i,j)=v(i,j,k,m)+vbavg(i,j,m)
            vtotn(i,j)=v(i,j,k,n)+vbavg(i,j,n)
            vflux(i,j)=vtotm(i,j)*max(dpv(i,j,k,m),cutoff)
            pv(i,j,k+1)=pv(i,j,k)+dpv(i,j,k,m)
          endif !iv
        enddo !i
      enddo !j
c
c --- rhs: corio+, dp.m+, dpmx+
c --- lhs: potvor
c
      margin = mbdy - 1
c
!$OMP PARALLEL DO PRIVATE(j,ja,jb,l,i,ia,ib)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
c ---   assume margin<nblk
        ja=j-1
        jb=j+1
c
c --- pot.vort., at lateral boundary points
        do l=1,isv(j) !ok
          i=ifv(j,l)
          if     (i.ge.1-margin .and. i.le.ii+margin) then
c           input of vorticity open boundary - a test - western border
            potvor(i  ,j)=corio(i  ,j) * 8.0
     &                     /max(8.0*cutoff,
     &                          4.0*(dpo(i,j,k,m)+dpo(i,ja ,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i+1,j))
          endif
          i=ilv(j,l)
          if     (i.ge.1-margin .and. i.le.ii+margin) then
c           input of vorticity open boundary - a test - eastern border
            potvor(i+1,j)=corio(i+1,j) * 8.0
     &                     /max(8.0*cutoff,
     &                          4.0*(dpo(i,j,k,m)+dpo(i,ja ,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i+1,j))
          endif
        enddo !l
      enddo !j
!$OMP END PARALLEL DO
c
c
c --- pot.vort., at lateral boundary points
c
c --- rhs: corio+, dp.m+, dpmx+
c --- lhs: potvor
c
      margin = mbdy - 1
c
      do i=1-margin,ii+margin
c ---   assume margin<nblk
        ia=i-1
        do l=1,jsu(i) !ok
          j=jfu(i,l)
          if     (j.ge.1-margin .and. j.le.jj+margin) then
c           input of vorticity open boundary - a test - southern border
            potvor(i,j  )=corio(i,j  ) * 8.0
     &                     /max(8.0*cutoff,
     &                          4.0*(dpo(i,j,k,m)+dpo(ia ,j,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i,j+1))
          endif
c
          j=jlu(i,l)
          if     (j.ge.1-margin .and. j.le.jj+margin) then
c           input of vorticity open boundary - a test - northern border
            potvor(i,j+1)=corio(i,j+1) * 8.0
     &                     /max(8.0*cutoff,
     &                          4.0*(dpo(i,j,k,m)+dpo(ia ,j,k,m)),
     &                          dpmx(i,j),
     &                          dpmx(i,j+1))
          endif
        enddo !l
      enddo !i
c
c --- pot.vort., at interior points (incl. promontories)
c
c --- rhs: corio, dp.m+, dpmx+
c --- lhs: potvor
c
      margin = mbdy - 1
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_Q) then
            potvor(i,j)=corio(i,j) * 8.0
     &         /max(8.0*cutoff,
     &              2.0*(dpo(i,j  ,k,m)+dpo(i-1,j  ,k,m)+
     &                   dpo(i,j-1,k,m)+dpo(i-1,j-1,k,m) ),
     &              dpmx(i,j),dpmx(i-1,j),dpmx(i+1,j),
     &                        dpmx(i,j-1),dpmx(i,j+1)    )
          endif !iq
        enddo !i
      enddo !j
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        if     (.not.allocated(mask)) then
          allocate(mask(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))
          call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)/2 ) !integer array
        endif
        do j=1,jj
          do i=1,ii
            mask(i,j)=min(1,iq(i,j)+iu(i,j  )+iv(i,j  )
     &                             +iu(i,j-1)+iv(i-1,j))
          enddo
        enddo
        write (text,'(a9,i3)') 'potvor k=',k
        call pipe_compare_sym1(potvor,mask,text)
      endif
c
c ----------------------------------------
c --- initialize fields for QUICK SCHEME
c ----------------------------------------
c
      margin = mbdy
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          advu( i,j)=0.0
          advv( i,j)=0.0
          diffu(i,j)=0.0
          diffv(i,j)=0.0
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c ------------------------------------------------------
c --- deformation-dependent eddy viscosity coefficient
c --- defor. at interior points (incl. promontories)
c --- defor takes into account nested and closed boundaries
c ------------------------------------------------------
c
      margin = mbdy - 1
c
!$OMP PARALLEL DO PRIVATE(j,i,
!$OMP&                    utotja,utotjb,vtotia,vtotib,wia,wib,wja,wjb)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            defor1(i,j)=((utotn(i+1,j)-utotn(i,j))
     &                  -(vtotn(i,j+1)-vtotn(i,j)))**2
          endif !ip
        enddo !i
        do i=1-margin,ii+margin
          if (SEA_Q) then
            if     (iu(i,j).ne.0 .and. iu(i,j-1).ne.0) then
c ---         wja is 1 for sidewall at j-1 w.r.t. j
              wja = max(0.0,min(1.0,
     &                          (pu(i,j,k+1)-depthu(i,j-1))
     &                            /max(pu(i,j,k+1)-pu(i,j,k),
     &                                 epsil)))
              utotja = (1.0-wja)*utotn(i,j-1)
c ---         wjb is 1 for sidewall at j w.r.t. j-1, i.e. at (j-1)+1
              wjb = max(0.0,min(1.0,
     &                          (pu(i,j-1,k+1)-depthu(i,j))
     &                            /max(pu(i,j-1,k+1)-pu(i,j-1,k),
     &                                 epsil)))
              utotjb = (1.0-wjb)*utotn(i,j) 
            else  !disable u difference
              utotja = utotn(i,j)
              utotjb = utotn(i,j)
            endif
            if     (iv(i,j).ne.0 .and. iv(i-1,j).ne.0) then
c ---         wia is 1 for sidewall at i-1 w.r.t. i
              wia = max(0.0,min(1.0,
     &                          (pv(i,j,k+1)-depthv(i-1,j))
     &                            /max(pv(i,j,k+1)-pv(i,j,k),
     &                                 epsil)))
              vtotia = (1.0-wia)*vtotn(i-1,j)
c ---         wib is 1 for sidewall at i w.r.t. i-1, i.e. at (i-1)+1
              wib = max(0.0,min(1.0,
     &                          (pv(i-1,j,k+1)-depthv(i,j))
     &                            /max(pv(i-1,j,k+1)-pv(i-1,j,k),
     &                                 epsil)))
              vtotib = (1.0-wib)*vtotn(i,j)
            else  !disable v difference
              vtotia = vtotn(i,j)
              vtotib = vtotn(i,j)
            endif
            defor2(i,j)=(vtotib-vtotia + utotjb-utotja)**2
          endif !iq
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- eddy viscosity coefficient on p-grid
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,upij,vpij,velmodul,defortot)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
c           use boundary values (zero or open) for p-grid velocities
            upij =  (utotn(i+1,j  ) +
     &               utotn(i,  j  )  )*0.5
            vpij =  (vtotn(i,  j+1) +
     &               vtotn(i,  j  )  )*0.5
            velmodul=sqrt( upij**2 + vpij**2 )
c
            defortot=sqrt(  defor1(i,  j) +
     &                     (defor2(i,  j)  *iq(i,  j)  +
     &                      defor2(i+1,j)  *iq(i+1,j)  +
     &                      defor2(i,  j+1)*iq(i,  j+1)+
     &                      defor2(i+1,j+1)*iq(i+1,j+1) )
     &           /max(1,iq(i,j)+iq(i+1,j)+iq(i,j+1)+iq(i+1,j+1))
     &                   )
c
c ---       viscosity terms are not additive, use the maximum
c ---       note that actual viscosity is scp{xy}**3 times viscp
            viscp(i,j)=max(0.25*(veldf4u(i,j)+veldf4u(i+1,j)+
     &                           veldf4v(i,j)+veldf4v(i,j+1) ),
     &                     velmodul*facdf4,
     &                     visco4*defortot)
c ---        CFL constraint on 2D explicit biharmonic viscosity
             viscp(i,j)=min(cflp(i,j), viscp(i,j) )
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- eddy viscosity coefficient on q-grid
c
!$OMP PARALLEL DO PRIVATE(j,i,s00,s01,s10,s11,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin+1,jj+margin
        do i=1-margin+1,ii+margin
          s00 = ip(i,  j)
          s10 = ip(i-1,j)
          s01 = ip(i,  j-1)
          s11 = ip(i-1,j-1)
          q   = 1.0/max(1.0,s00+s01+s10+s11)
          viscq(i,j) = (viscp(i,  j)  *s00 +
     &                  viscp(i-1,j)  *s10 +
     &                  viscp(i,  j-1)*s01 +
     &                  viscp(i-1,j-1)*s11  )*q
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- ----------
c --- u equation
c --- ----------
c
c --------------------------------------------------------
c --- START QUICK ADVECTION SCHEME HERE
c --------------------------------------------------------
c
c -------------------------------------
c First compute advection along X
c -------------------------------------
c
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,ja,jm1,jp1,l,i,
!$OMP&                    uwim,uwin,uwjm,uwjn,dum,dun,
!$OMP&                    clpm,crmm,clpn,clmn,crpn,crmn)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-nbdy,ii+nbdy
          uwim(i)=utotm(i,j)
          uwin(i)=utotn(i,j)
        enddo !i
c
        do l=1,isu(j) !ok
c
c ---     modify uwi for boundary conditions : begin
          i=ifu(j,l)
          if     (i-2.ge. 1-nbdy) then
            if (iuopn(i-1,j).ne.0) then !open boundary
              uwin(i-2)= utotn(i-1,j)
              uwim(i-2)= utotm(i-1,j)
            else
              uwin(i-2)=-utotn(i,  j)  !uwin(i-1)==0.0
              uwim(i-2)=-utotm(i,  j)  !uwim(i-1)==0.0
            endif
          endif
          i=ilu(j,l)
          if     (i+2.le.ii+nbdy) then
            if (iuopn(i+1,j).ne.0) then !open boundary
              uwin(i+2)= utotn(i+1,j)
              uwim(i+2)= utotm(i+1,j)
            else
              uwin(i+2)=-utotn(i,  j)  !uwin(i+1)==0.0
              uwim(i+2)=-utotm(i,  j)  !uwim(i+1)==0.0
            endif
          endif
c ---     modify uwi for boundary conditions : end
c
           do i=max(1-nbdy+1,ifu(j,l)-1),min(ii+nbdy,ilu(j,l)+2)
             dum(i)=uwim(i)-uwim(i-1)
             dun(i)=uwin(i)-uwin(i-1)
           enddo !i
c
           do i=max(1-margin,ifu(j,l)),min(ii+margin,ilu(j,l))
             clpm=dum(i)  -dum(i-1)
             crmm=dum(i+2)-dum(i+1)
c
             clpn=dun(i)  -dun(i-1)
             clmn=dun(i+1)-dun(i)
             crpn=clmn
             crmn=dun(i+2)-dun(i+1)
c
c ---        calculating U dxU and biharmonic diffusion
             advu(i,j)=uwim(i)*(0.5*(uwim(i+1)-uwim(i-1)) -
     &                            q12*(crmm-clpm)            )*
     &                 scuxi(i,j)
c
             diffu(i,j)=
     &        (viscp(i,  j)*(crpn-crmn)*max(hmindiff,dpo(i,  j,k,m)) -
     &         viscp(i-1,j)*(clpn-clmn)*max(hmindiff,dpo(i-1,j,k,m))  )
     &           *scuxi(i,j)/max(hmindiff,dpu(i,j,k,m))
c
cdiag        if (max(abs(i-itest),abs(j-jtest)).le.1.and.k.eq.1) then
cdiag          write(lp,'(a,2i5,i3,4f12.5)')
cdiag.           'advu-x =',
cdiag.           i+i0,j+j0,k,
cdiag.           advu(i,j),
cdiag.           uwim(i)*0.5*(uwim(i+1)-uwim(i-1))*scuxi(i,j),
cdiag.          -uwim(i)*q12*(crmm-clpm)*scuxi(i,j),
cdiag.           uwim(i)
cdiag        endif !test
           enddo !i
c
c ---     restore uwi from boundary conditions : begin
          i=ifu(j,l)
          if     (i-2.ge. 1-nbdy) then
            uwin(i-2)=utotn(i-2,j)
            uwim(i-2)=utotm(i-2,j)
          endif
          i=ilu(j,l)
          if     (i+2.le.ii+nbdy) then
            uwin(i+2)=utotn(i+2,j)
            uwim(i+2)=utotm(i+2,j)
          endif
c ---     restore uwi from boundary conditions : end
        enddo !l
c
c ---   -------------------------------------
c ---   Now compute advection along Y
c ---   -------------------------------------
c
        do i=1-margin,ii+margin
          if (SEA_U) then
            do ja= -2,2
              uwjm(ja) = utotm(i,j+ja)
              uwjn(ja) = utotn(i,j+ja)
            enddo !ja
c
            if     (iu(i,j-1).eq.0) then
              if     (iuopn(i,j-1).ne.0) then  !open boundary
                uwjn(-2)= utotn(i,j-1)
                uwjm(-2)= utotm(i,j-1)
              else
                uwjn(-2)=-utotn(i,j)  !uwjn(-1)==0.0
                uwjm(-2)=-utotm(i,j)  !uwjm(-1)==0.0
              endif
            endif
            if     (iu(i,j+1).eq.0) then
              if     (iuopn(i,j+1).ne.0) then  !open boundary
                uwjn( 2)= utotn(i,j+1)
                uwjm( 2)= utotm(i,j+1)
              else
                uwjn( 2)=-utotn(i,j)  !uwjn( 1)==0.0
                uwjm( 2)=-utotm(i,j)  !uwjm( 1)==0.0
              endif
            endif
c
            do ja= -1,2
              dun(j+ja)=uwjn(ja)-uwjn(ja-1)
              dum(j+ja)=uwjm(ja)-uwjm(ja-1)
            enddo
c
            clpm=dum(j)  -dum(j-1)
            crmm=dum(j+2)-dum(j+1)
c
            clpn=dun(j)  -dun(j-1)
            crmn=dun(j+2)-dun(j+1)
            clmn=dun(j+1)-dun(j)
            crpn=clmn
c
            advu(i,j)=advu(i,j)+
     &      0.25*(vtotm(i,j)  +vtotm(i-1,j)  +
     &            vtotm(i,j+1)+vtotm(i-1,j+1) )*
     &            (0.5*(uwjm(+1)-uwjm(-1))-
     &             q12*(crmm-clpm)         )*scuyi(i,j)
c
            jm1=j-1
            if(iu(i,j-1).eq.0) then
              jm1=j
            endif
            jp1=j+1
            if(iu(i,j+1).eq.0) then
              jp1=j
            endif
            diffu(i,j)=diffu(i,j)+
     &       ( viscq(i,j+1)*(crpn-crmn)*
     &         max(hmindiff,0.5*(dpu(i,jp1,k,m)+dpu(i,j,  k,m)))
     &        -viscq(i,j)  *(clpn-clmn)*
     &         max(hmindiff,0.5*(dpu(i,j,  k,m)+dpu(i,jm1,k,m)))
     &       )*scuyi(i,j)/max(hmindiff,dpu(i,j,k,m))
c
c
cdiag       if (max(abs(i-itest),abs(j-jtest)).le.1.and.k.eq.1) then
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'advu-y =',
cdiag.          i+i0,j+j0,k,
cdiag.          advu(i,j),
cdiag.          0.25*(vtotm(i,j)  +vtotm(i-1,j)  +
cdiag.                vtotm(i,j+1)+vtotm(i-1,j+1) )*scuyi(i,j)*
cdiag.          0.5*(uwjm(+1)-uwjm(-1)),
cdiag.         -0.25*(vtotm(i,j)  +vtotm(i-1,j)  +
cdiag.                vtotm(i,j+1)+vtotm(i-1,j+1) )*scuyi(i,j)*
cdiag.          q12*(crmm-clpm),
cdiag.          0.25*(vtotm(i,j)  +vtotm(i-1,j)  +
cdiag.                vtotm(i,j+1)+vtotm(i-1,j+1) )
cdiag       endif !test
          endif !ip
        enddo !i
      enddo !j - do 16
!$OMP END PARALLEL DO
c
c ------------------------------------------------------
c --- ----------
c --- v equation
c --- ----------
c ------------------------------------------------------
c
c --------------------------------------------------------
c --- START QUICK ADVECTION SCHEME HERE
c --------------------------------------------------------
c
c -------------------------------------
c First compute advection along X
c -------------------------------------
c
      margin = mbdy - 2
c
!$OMP PARALLEL DO PRIVATE(j,ja,l,i,im1,ip1,
!$OMP&             dvm,dvn,vwim,vwin,vwjm,vwjn,
!$OMP&             clpm,crmm,clpn,clmn,crpn,crmn)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-nbdy,ii+nbdy
          vwim(i)=vtotm(i,j)
          vwin(i)=vtotn(i,j)
        enddo !i
c
        do l=1,isv(j) !ok
c
c ---     modify vwi for boundary conditions : begin
          i=ifv(j,l)
          if     (ivopn(i-1,j).ne.0) then  !open boundary
            if     (i-2.gt.1-nbdy) then
              vwin(i-2)= vtotn(i-1,j)
              vwim(i-2)= vtotm(i-1,j)
            endif 
          else
            if     (i-2.gt.1-nbdy) then
              vwin(i-2)=-vtotn(i,  j)  !vwin(i-1)==0.0
              vwim(i-2)=-vtotm(i,  j)  !vwim(i-1)==0.0
            endif 
          endif 
c
          i=ilv(j,l)
          if     (ivopn(i+1,j).ne.0) then  !open boundary
            if     (i+2.le.ii+nbdy) then
              vwin(i+2)= vtotn(i+1,j)
              vwim(i+2)= vtotm(i+1,j)
            endif 
          else
            if     (i+2.le.ii+nbdy) then
              vwin(i+2)=-vtotn(i,j)  !vwin(i+1==0.0
              vwim(i+2)=-vtotm(i,j)  !vwim(i+1==0.0
            endif 
          endif 
c ---     modify vwi for boundary conditions : end
c
          do i=max(1-nbdy+1,ifv(j,l)-1),min(ii+nbdy,ilv(j,l)+2)
            dvm(i)=vwim(i)-vwim(i-1)
            dvn(i)=vwin(i)-vwin(i-1)
          enddo !i
c
          do i=max(1-margin,ifv(j,l)),min(ii+margin,ilv(j,l))
            clpm=dvm(i)  -dvm(i-1)
            crmm=dvm(i+2)-dvm(i+1)
c
            clpn=dvn(i)  -dvn(i-1)
            crmn=dvn(i+2)-dvn(i+1)
            clmn=dvn(i+1)-dvn(i)
            crpn=clmn
c
c ---       calculating U dxV and biharmonic diffusion
            advv(i,j)=
     &      0.25*(utotm(i,j)  +utotm(i+1,j)  +
     &            utotm(i,j-1)+utotm(i+1,j-1) )*
     &            (0.5*(vwim(i+1)-vwim(i-1))-
     &             q12*(crmm-clpm)               )*scvxi(i,j)
c
            im1=i-1
            if(iv(i-1,j).eq.0) then
              im1=i
            endif
            ip1=i+1
            if(iv(i+1,j).eq.0) then
              ip1=i
            endif
            diffv(i,j)=
     &      ( viscq(i+1,j)*(crpn-crmn)*
     &        max(hmindiff,0.5*(dpv(ip1,j,k,m)+dpv(i,  j,k,m))) -
     &        viscq(i,  j)*(clpn-clmn)*
     &        max(hmindiff,0.5*(dpv(i,  j,k,m)+dpv(im1,j,k,m)))  )
     &         *scvxi(i,j)/max(hmindiff,dpv(i,j,k,m))
c
cdiag       if (max(abs(i-itest),abs(j-jtest)).le.1.and.k.eq.1) then
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'advv-x =',
cdiag.          i+i0,j+j0,k,
cdiag.          advv(i,j),
cdiag.          0.25*(utotm(i,j)  +utotm(i+1,j)  +
cdiag.                utotm(i,j-1)+utotm(i+1,j-1) )*scvxi(i,j)*
cdiag.          0.5*(vwim(i+1)-vwim(i-1)),
cdiag.         -0.25*(utotm(i,j)  +utotm(i+1,j)  +
cdiag.                utotm(i,j-1)+utotm(i+1,j-1) )*scvxi(i,j)*
cdiag.          q12*(crmm-clpm),
cdiag.          0.25*(utotm(i,j)  +utotm(i+1,j)  +
cdiag.                utotm(i,j-1)+utotm(i+1,j-1) )
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'advv-v =',
cdiag.          i+i0,j+j0,k,
cdiag.          vwim(i-2),
cdiag.          vwim(i-1),
cdiag.          vwim(i+1),
cdiag.          vwim(i+2)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'difv-v =',
cdiag.          i+i0,j+j0,k,
cdiag.          vwin(i-2),
cdiag.          vwin(i-1),
cdiag.          vwin(i+1),
cdiag.          vwin(i+2)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'difv-x =',
cdiag.          i+i0,j+j0,k,
cdiag.          diffv(i,j),
cdiag.          viscq(i,j),
cdiag.          viscq(i+1,j),
cdiag.          vwin(i)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'difv-c =',
cdiag.          i+i0,j+j0,k,
cdiag.          crpn,
cdiag.          crpn-crmn,
cdiag.          clpn,
cdiag.          clpn-clmn
cdiag       endif !test
          enddo !i
c
c ---     restore vwi from boundary conditions : begin
          i=ifv(j,l)
          if     (i-1.gt.1-nbdy) then
            vwin(i-1)=vtotn(i-1,j)
            vwim(i-1)=vtotm(i-1,j)
          endif
          if     (i-2.gt.1-nbdy) then
            vwin(i-2)=vtotn(i-2,j)
            vwim(i-2)=vtotm(i-2,j)
          endif
          i=ilv(j,l)
          if     (i+1.lt.ii+nbdy) then
            vwin(i+1)=vtotn(i+1,j)
            vwim(i+1)=vtotm(i+1,j)
          endif
          if     (i+2.lt.ii+nbdy) then
            vwin(i+2)=vtotn(i+2,j)
            vwim(i+2)=vtotm(i+2,j)
          endif
c ---     restore vwi from boundary conditions : end
        enddo !l
c
c ---  -------------------------------------
c ---  Now compute advection along Y
c ---  -------------------------------------
c
        do i=1-margin,ii+margin
          if (SEA_V) then
            do ja= -2,2
              vwjm(ja) = vtotm(i,j+ja)
              vwjn(ja) = vtotn(i,j+ja)
            enddo !ja
c
            if     (iv(i,j-1).eq.0) then
              if     (ivopn(i,j-1).ne.0) then  !open boundary
                vwjn(-2)= vtotn(i,j-1)
                vwjm(-2)= vtotm(i,j-1)
              else
                vwjn(-2)=-vtotn(i,j)  !vwjn(-1)==0.0
                vwjm(-2)=-vtotm(i,j)  !vwjm(-1)==0.0
              endif
            endif
            if     (iv(i,j+1).eq.0) then
              if     (ivopn(i,j+1).ne.0) then  !open boundary
                vwjn( 2)= vtotn(i,j+1)
                vwjm( 2)= vtotm(i,j+1)
              else
                vwjn( 2)=-vtotn(i,j)  !vwjn( 1)==0.0
                vwjm( 2)=-vtotm(i,j)  !vwjm( 1)==0.0
              endif
            endif
c
            do ja= -1,2
              dvn(j+ja)=vwjn(ja)-vwjn(ja-1)
              dvm(j+ja)=vwjm(ja)-vwjm(ja-1)
            enddo !ja
c
            clpm=dvm(j)  -dvm(j-1)
            crmm=dvm(j+2)-dvm(j+1)
c
            clpn=dvn(j)  -dvn(j-1)
            clmn=dvn(j+1)-dvn(j)
            crpn=clmn
            crmn=dvn(j+2)-dvn(j+1)
c
            advv(i,j)=advv(i,j)+
     &                vwjm(0)*(0.5*(vwjm(+1)-vwjm(-1)) -
     &                         q12*(crmm-clpm)           )*
     &                scvyi(i,j)
c
             diffv(i,j)=diffv(i,j)+
     &        (viscp(i,j)  *(crpn-crmn)*max(hmindiff,dpo(i,j,  k,m)) -
     &         viscp(i,j-1)*(clpn-clmn)*max(hmindiff,dpo(i,j-1,k,m))  )
     &           *scvyi(i,j)/max(hmindiff,dpv(i,j,k,m))
c
cdiag       if (max(abs(i-itest),abs(j-jtest)).le.1.and.k.eq.1) then
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'advv-y =',
cdiag.          i+i0,j+j0,k,
cdiag.          advv(i,j),
cdiag.          vwjm(0)*0.5*(vwjm(+1)-vwjm(-1))*scvyi(i,j),
cdiag.         -vwjm(0)*(q12*(crmm-clpm))*scvyi(i,j),
cdiag.          vwjm(0)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'advv-v =',
cdiag.          i+i0,j+j0,k,
cdiag.          vwjm(-2),
cdiag.          vwjm(-1),
cdiag.          vwjm( 1),
cdiag.          vwjm( 2)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'difv-v =',
cdiag.          i+i0,j+j0,k,
cdiag.          vwjn(-2),
cdiag.          vwjn(-1),
cdiag.          vwjn( 1),
cdiag.          vwjn( 2)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'difv-y =',
cdiag.          i+i0,j+j0,k,
cdiag.          diffv(i,j),
cdiag.          viscp(i,j),
cdiag.          viscp(i,j-1),
cdiag.          vwjn(0)
cdiag         write(lp,'(a,2i5,i3,4f12.5)')
cdiag.          'difv-c =',
cdiag.          i+i0,j+j0,k,
cdiag.          crpn,
cdiag.          crpn-crmn,
cdiag.          clpn,
cdiag.          clpn-clmn
cdiag       endif !test
          endif !iv
        enddo !i
      enddo !j - do 17
!$OMP END PARALLEL DO
c
c --- ---------------------------------------
c --- ---------------------------------------
c --- ----------
c --- u equation
c --- ----------
c --- ---------------------------------------
c --- ---------------------------------------
c
c --- pressure force in x direction
c --- ('scheme 2' from appendix -a- in bleck-smith paper)
c
c --- rhs: depthu, pu, montg+, thstar+, p+, dp.m+
c --- lhs: util1, pgfx
c
      margin = mbdy - 4
c
!$OMP PARALLEL DO PRIVATE(j,i,dmontg,dthstr,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            util1(i,j)=max(0.0,min(depthu(i,j)-pu(i,j,k),h1))
c
            if     (kapref.ne.-1) then
              dmontg=montg( i,j,k,1)-montg( i-1,j,k,1)
              dthstr=thstar(i,j,k,1)-thstar(i-1,j,k,1)
            else !2 thermobaric reference states
              q=0.5*(skap(i,j)+skap(i-1,j))
              dmontg=     q *(montg( i,j,k,1)-montg( i-1,j,k,1))+
     &               (1.0-q)*(montg( i,j,k,2)-montg( i-1,j,k,2))
              dthstr=     q *(thstar(i,j,k,1)-thstar(i-1,j,k,1))+
     &               (1.0-q)*(thstar(i,j,k,2)-thstar(i-1,j,k,2))
            endif
            pgfx(i,j)=util1(i,j)*
     &        (scuxi(i,j)*(dmontg+
     &                     dthstr*(p(i,j,k+1)*p(i-1,j,k+1)-
     &                             p(i,j,k  )*p(i-1,j,k  ))*thref**2
     &                        /(dp(i,j,k,m)+dp(i-1,j,k,m)+epsil)    )
     &        )
          endif !iu
        enddo !i
      enddo !j
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'pu     k=',k
        call pipe_compare_sym1(pu(1-nbdy,1-nbdy,k),iu,text)
        write (text,'(a9,i3)') 'depthu k=',k
        call pipe_compare_sym1(depthu,iu,text)
        write (text,'(a9,i3)') 'util1  k=',k
        call pipe_compare_sym1(util1, iu,text)
        util4(1:ii,1:jj) = montg( 1:ii,1:jj,k,1)-montg( 0:ii-1,1:jj,k,1)
        write (text,'(a9,i3)') 'montgX k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = thstar(1:ii,1:jj,k,1)-thstar(0:ii-1,1:jj,k,1)
        write (text,'(a9,i3)') 'thstaX k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k)*p(0:ii-1,1:jj,k)
        write (text,'(a9,i3)') 'pSQ    k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k+1)*p(0:ii-1,1:jj,k+1)
        write (text,'(a9,i3)') 'pSQ    k=',k+1
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = dp(1:ii,1:jj,k,m)+dp(0:ii-1,1:jj,k,m)+epsil
        write (text,'(a9,i3)') 'dp.m+  k=',k
        call pipe_compare_sym1(util4, iu,text)
        write (text,'(a9,i3)') 'advu   k=',k
        call pipe_compare_sym1(advu,  iu,text)
        write (text,'(a9,i3)') 'diffu  k=',k
        call pipe_compare_sym1(diffu, iu,text)
        util4(1:ii,1:jj) = vflux(1:ii,  1:jj)+vflux(1:ii,  2:jj+1)+
     &                     vflux(0:ii-1,1:jj)+vflux(0:ii-1,2:jj+1)
        write (text,'(a9,i3)') 'vflux  k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = potvor(1:ii,1:jj)+potvor(1:ii,2:jj+1)
        write (text,'(a9,i3)') 'potvor k=',k
        call pipe_compare_sym1(util4, iu,text)
        write (text,'(a9,i3)') 'pgfx   k=',k
        call pipe_compare_sym1(pgfx,  iu,text)
      endif
c
c
c --- rhs: pgfx+, util1+, p+, dpmixl.m+, dpu.m, utotn, drag+
c --- rhs: stresx, u.m, u.n, dpu.n, gradx, utotm+, vtotm+,
c --- rhs: vflux+, potvor+, uflux1+, uflux2, uflux3
c --- lhs: gradx, stress, u.m, u.n
c
      margin = mbdy - 5
c
!$OMP PARALLEL DO PRIVATE(j,ja,jb,i,
!$OMP&                    q,dpun,uhm,uh0,uhp,
!$OMP&                    ptopl,pbotl,pstres,pbop,qdpu,dragu)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        ja=j-1
        jb=j+1
        do i=1-margin,ii+margin
          if (SEA_U) then
            gradx(i,j)=(pgfx(i,j)+(h1-util1(i,j))*
     &        (pgfx (i-1,j)+pgfx (i+1,j)+pgfx (i,ja)+pgfx (i,jb))/
     &        (util1(i-1,j)+util1(i+1,j)+util1(i,ja)+util1(i,jb)+
     &          epsil))/h1
c
            ptopl=min(depthu(i,j),0.5*(p(i,j,k  )+p(i-1,j,k  )))
            pbotl=min(depthu(i,j),0.5*(p(i,j,k+1)+p(i-1,j,k+1)))
            if(hybrid .and. mxlkrt) then
              pstres=0.5*(dpmixl(i,j,m)+dpmixl(i-1,j,m))
            else
              pstres=dpu(i,j,1,m)
            endif
c             
c ---       top and bottom boundary layer stress
c ---       drag term is FRAC * Cb * (|v| + c.bar) * onem/dp
c ---        where FRAC is the fraction of the layer to apply stress too
c
            pbop=depthu(i,j)-0.5*(thkbop(i,j)+thkbop(i-1,j)) !top of bot. b.l.
            qdpu=1.0/max(dpu(i,j,k,m),onemm)
            dragu=0.5*(drag(i,j)+drag(i-1,j))*qdpu*
     &                ( max(pbop,    pbotl)
     &                 -max(pbop,min(ptopl,pbotl-onemm)))
            if     (drglim.gt.0.0) then
c             explicit drag: u.t+1 - u.t-1 = - 2dt*dragu*u.t-1
c                   limiter: 2dt*dragu <= drglim
              stress(i,j)=-utotn(i,j)*min(dragu,drglim*dt1inv) +
     &             (stresx(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpu
            else
c             implicit drag: u.t+1 - u.t-1 = - 2dt*dragu*u.t+1
c                                          = - 2dt*dragu / (1+2dt*dragu) * u.t+1
              stress(i,j)=-utotn(i,j)*dragu/(1.0+delt1*dragu) +
     &             (stresx(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpu
            endif
c
            if     (tidflg.gt.0 .and. drgscl.ne.0.0
     &                          .and. thkdrg.gt.0.0) then
              pbop=depthu(i,j)-0.5*(util6(i,j)+util6(i-1,j)) !top of bot. b.l.
              dragu=0.5*(util5(i,j)+util5(i-1,j))*
     &                  ( max(pbop,    pbotl)
     &                   -max(pbop,min(ptopl,pbotl-onemm)))*qdpu
              if     (drglim.gt.0.0) then
                stress(i,j)=stress(i,j)
     &                      -utotn(i,j)*min(dragu,drglim*dt1inv)
              else
                stress(i,j)=stress(i,j)
     &                      -utotn(i,j)*dragu/(1.0+delt1*dragu)
              endif
c ---         !anti-drag on detided velocity (always explicit)
              stress(i,j)=stress(i,j)
     &                      +untide(i,j)*min(dragu,adrlim*dt1inv)
            endif !tidflg
c
cdiag       util4(i,j) = u(i,j,k,n)
            u(i,j,k,n) = utotn(i,j) + delt1*(
     &       - gradx(i,j)
     &       - ubrhs(i,j)
     &       +stress(i,j)
     &       -  advu(i,j)
     &       + diffu(i,j)
     &       + 0.125*(vflux(i  ,j)  +
     &                vflux(i  ,j+1)+
     &                vflux(i-1,j)  +
     &                vflux(i-1,j+1) )*(potvor(i,j)+potvor(i,j+1)) )
c
c ---       Robert-Asselin time filter of -u- field
c ---       Note that this is smoothing dp * utot,
c ---        but the filter is not conservative over 3 time levels.
            dpun = max(0.0,
     &                 min(depthu(i,j),0.5*(pnkp(i,j)+pnkp(i-1,j)))-
     &                 min(depthu(i,j),0.5*(pnk0(i,j)+pnk0(i-1,j))) )
            uhm  = utotn(i,j)*max(dpu(i,j,k,n),dpthin)
            uh0  = utotm(i,j)*max(dpu(i,j,k,m),dpthin)
            uhp  = u(i,j,k,n)*max(dpun,        dpthin)
            q    = 0.5*ra2fac*(uhm + uhp - 2.0*uh0)
            u(i,j,k,m) = uh0 + q
            u(i,j,k,n) = uhp
          endif !iu
        enddo !i
      enddo !j
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'vtotm  k=',k
        call pipe_compare_sym1(vtotm, iv,text)
        write (text,'(a9,i3)') 'utotm  k=',k
        call pipe_compare_sym1(utotm, iu,text)
        util4(1:ii,1:jj) = advu(0:ii-1,1:jj)  +
     &                     advu(2:ii+1,1:jj)  +
     &                     advu(1:ii,  0:jj-1)+
     &                     advu(1:ii,  2:jj+1)
        write (text,'(a9,i3)') 'advu+  k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = diffu(0:ii-1,1:jj)  +
     &                     diffu(2:ii+1,1:jj)  +
     &                     diffu(1:ii,  0:jj-1)+
     &                     diffu(1:ii,  2:jj+1)
        write (text,'(a9,i3)') 'diffu+ k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = pgfx(0:ii-1,1:jj)  +
     &                     pgfx(2:ii+1,1:jj)  +
     &                     pgfx(1:ii,  0:jj-1)+
     &                     pgfx(1:ii,  2:jj+1)
        write (text,'(a9,i3)') 'pgfx+  k=',k
        call pipe_compare_sym1(util4, iu,text)
        util4(1:ii,1:jj) = util1(0:ii-1,1:jj)  +
     &                     util1(2:ii+1,1:jj)  +
     &                     util1(1:ii,  0:jj-1)+
     &                     util1(1:ii,  2:jj+1)+epsil
        write (text,'(a9,i3)') 'util1+ k=',k
        call pipe_compare_sym1(util4, iu,text)
        write (text,'(a9,i3)') 'gradx  k=',k
        call pipe_compare_sym1(gradx, iu,text)
        write (text,'(a9,i3)') 'ubrhs  k=',k
        call pipe_compare_sym1(ubrhs, iu,text)
        write (text,'(a9,i3)') 'stress k=',k
        call pipe_compare_sym1(stress,iu,text)
        write (text,'(a9,i3)') 'u.n    k=',k
        call pipe_compare_sym1(u(1-nbdy,1-nbdy,k,n),iu,text)
      endif
c
cdiag if (k.eq.1) then
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag write (lp,100) nstep
cdiag do j=jtest-1,jtest+1
cdiag do i=itest-1,itest+1
cdiag if     (iu(i,j).ne.1) then
cdiag write (lp,'(2i5,i3,2f8.3)') i+i0,j+j0,k,
cdiag.  0.0,0.0
cdiag else
cdiag write (lp,'(2i5,i3,8f8.3)') i+i0,j+j0,k,
cdiag.  util4(i,j),u(i,j,k,n),
cdiag.  -delt1*gradx(i,j),
cdiag.  -delt1*advu(i,j),
cdiag.   delt1*
cdiag.   .125*(vflux(i  ,j)+vflux(i  ,j+1)+vflux(i-1,j)+vflux(i-1,j+1))
cdiag.       *(potvor(i,j)+potvor(i,j+1)),
cdiag.  -delt1*ubrhs(i,j),
cdiag.   delt1*stress(i,j),
cdiag.   delt1*diffu(i,j)
cdiag endif
cdiag enddo !i
cdiag enddo !j
cdiag endif !test
cdiag endif !k==1
 100    format(i9,8x,'uold    unew   gradp  nonlin   corio',
     &3x,'ubrhs  stress    fric')
c
c --- ----------
c --- v equation
c --- ----------
c
c --- pressure force in y direction
c --- ('scheme 2' from appendix -a- in bleck-smith paper)
c
      margin = mbdy - 4
c
!$OMP PARALLEL DO PRIVATE(j,i,dmontg,dthstr,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            util2(i,j)=max(0.,min(depthv(i,j)-pv(i,j,k),h1))
            if     (kapref.ne.-1) then
              dmontg=montg( i,j,k,1)-montg( i,j-1,k,1)
              dthstr=thstar(i,j,k,1)-thstar(i,j-1,k,1)
            else !2 thermobaric reference states
              q=0.5*(skap(i,j)+skap(i,j-1))
              dmontg=     q *(montg( i,j,k,1)-montg( i,j-1,k,1))+
     &               (1.0-q)*(montg( i,j,k,2)-montg( i,j-1,k,2))
              dthstr=     q *(thstar(i,j,k,1)-thstar(i,j-1,k,1))+
     &               (1.0-q)*(thstar(i,j,k,2)-thstar(i,j-1,k,2))
            endif
            pgfy(i,j)=util2(i,j)*
     &        (scvyi(i,j)*(dmontg+
     &                     dthstr*(p(i,j,k+1)*p(i,j-1,k+1)-
     &                             p(i,j,k  )*p(i,j-1,k  ))*thref**2
     &                      /(dp(i,j,k,m)+dp(i,j-1,k,m)+epsil)      )
     &        )
          endif !iv
        enddo !i
      enddo !j
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'pv     k=',k
        call pipe_compare_sym1(pv(1-nbdy,1-nbdy,k),iv,text)
        write (text,'(a9,i3)') 'depthv k=',k
        call pipe_compare_sym1(depthv,iv,text)
        write (text,'(a9,i3)') 'util2  k=',k
        call pipe_compare_sym1(util2, iv,text)
        util4(1:ii,1:jj) = montg( 1:ii,1:jj,k,1)-montg( 1:ii,0:jj-1,k,1)
        write (text,'(a9,i3)') 'montgY k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = thstar(1:ii,1:jj,k,1)-thstar(1:ii,0:jj-1,k,1)
        write (text,'(a9,i3)') 'thstaY k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k)  *p(1:ii,0:jj-1,k)
        write (text,'(a9,i3)') 'pSQ    k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = p(1:ii,1:jj,k+1)*p(1:ii,0:jj-1,k+1)
        write (text,'(a9,i3)') 'pSQ    k=',k+1
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) = dp(1:ii,1:jj,k,m)+dp(1:ii,0:jj-1,k,m)+epsil
        write (text,'(a9,i3)') 'dp.m+  k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'pgfy   k=',k
        call pipe_compare_sym1(pgfy,  iv,text)
      endif
c
      margin = mbdy - 5
c
!$OMP PARALLEL DO PRIVATE(j,i,ia,ib)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            ia=i-1
            ib=i+1
c
            grady(i,j)=(pgfy(i,j)+(h1-util2(i,j))*
     &        (pgfy (ia ,j)+pgfy (ib ,j)
     &        +pgfy (i,j-1)+pgfy (i,j+1))/
     &        (util2(ia ,j)+util2(ib ,j)
     &        +util2(i,j-1)+util2(i,j+1)+epsil))/h1
          endif !iv
        enddo !i
      enddo !j
c
      margin = mbdy - 5
c
!$OMP PARALLEL DO PRIVATE(j,i,
!$OMP&                    q,dpvn,vhm,vh0,vhp,
!$OMP&                    ptopl,pbotl,pstres,pbop,qdpv,dragv)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_V) then
            ptopl=min(depthv(i,j),0.5*(p(i,j,k  )+p(i,j-1,k  )))
            pbotl=min(depthv(i,j),0.5*(p(i,j,k+1)+p(i,j-1,k+1)))
            if(hybrid .and. mxlkrt) then
              pstres=0.5*(dpmixl(i,j,m)+dpmixl(i,j-1,m))
            else
              pstres=dpv(i,j,1,m)
            endif
c
c ---       top and bottom boundary layer stress
c ---       drag term is FRAC * Cb * (|v| + c.bar) * onem/dp
c ---        where FRAC is the fraction of the layer to apply stress too
            pbop=depthv(i,j)-0.5*(thkbop(i,j)+thkbop(i,j-1))  !top of bot. b.l.
            qdpv=1.0/max(dpv(i,j,k,m),onemm)
            dragv=0.5*(drag(i,j)+drag(i,j-1))*qdpv*
     &                ( max(pbop,    pbotl)
     &                 -max(pbop,min(ptopl,pbotl-onemm)))
            if     (drglim.gt.0.0) then
              stress(i,j)=-vtotn(i,j)*min(dragv,drglim*dt1inv) +
     &             (stresy(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpv
            else
              stress(i,j)=-vtotn(i,j)*dragv/(1.0+delt1*dragv) +
     &             (stresy(i,j)*thref*(min(pstres,pbotl+onemm)
     &                                -min(pstres,ptopl      )))*qdpv
            endif
c
            if     (tidflg.gt.0 .and. drgscl.ne.0.0
     &                          .and. thkdrg.gt.0.0) then
              pbop=depthv(i,j)-0.5*(util6(i,j)+util6(i,j-1))  !top of bot. b.l.
              dragv=0.5*(util5(i,j)+util5(i,j-1))*
     &                  ( max(pbop,    pbotl)
     &                   -max(pbop,min(ptopl,pbotl-onemm)))*qdpv
              if     (drglim.gt.0.0) then
                stress(i,j)=stress(i,j)
     &                      -vtotn(i,j)*min(dragv,drglim*dt1inv)
              else
                stress(i,j)=stress(i,j)
     &                      -vtotn(i,j)*dragv/(1.0+delt1*dragv)
              endif
c ---         !anti-drag on detided velocity (always explicit)
              stress(i,j)=stress(i,j)
     &                      +vntide(i,j)*min(dragv,adrlim*dt1inv)
            endif !tidflg
c
cdiag       util4(i,j) = v(i,j,k,n)
            v(i,j,k,n) = vtotn(i,j) + delt1*(
     &       - grady(i,j)
     &       - vbrhs(i,j)
     &       +stress(i,j)
     &       -  advv(i,j)
     &       + diffv(i,j)
     &       - 0.125*(uflux(i,  j)  +
     &                uflux(i+1,j)  +
     &                uflux(i,  j-1)+
     &                uflux(i+1,j-1) )*(potvor(i,j)+potvor(i+1,j)) )
c
c ---       Robert-Asselin time filter of -v- field
c ---       Note that this is smoothing dp * vtot,
c ---        but the filter is not conservative over 3 time levels.
            dpvn = max(0.0,
     &                 min(depthv(i,j),0.5*(pnkp(i,j)+pnkp(i,j-1)))-
     &                 min(depthv(i,j),0.5*(pnk0(i,j)+pnk0(i,j-1))) )
            vhm  = vtotn(i,j)*max(dpv(i,j,k,n),dpthin)
            vh0  = vtotm(i,j)*max(dpv(i,j,k,m),dpthin)
            vhp  = v(i,j,k,n)*max(dpvn,        dpthin)
            q    = 0.5*ra2fac*(vhm + vhp - 2.0*vh0)
            v(i,j,k,m) = vh0 + q
            v(i,j,k,n) = vhp
          endif !iv
        enddo !i
      enddo !j
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        util4(1:ii,1:jj) =  vtotm(1:ii,  2:jj+1)**2
     &                     +utotm(1:ii,  1:jj  )**2
     &                     +utotm(2:ii+1,1:jj  )**2
     &                     -vtotm(1:ii,  0:jj-1)**2
     &                     -utotm(1:ii,  0:jj-1)**2
     &                     -utotm(2:ii+1,0:jj-1)**2
        write (text,'(a9,i3)') 'totm   k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'grady  k=',k
        call pipe_compare_sym1(grady, iv,text)
        write (text,'(a9,i3)') 'vbrhs  k=',k
        call pipe_compare_sym1(vbrhs, iv,text)
        if     (k.eq.kk) then
          write (text,'(a9,i3)') 'p      k=',k-1
          call pipe_compare_sym1(p(1-nbdy,1-nbdy,k-1),ip,text)
          write (text,'(a9,i3)') 'p      k=',k
          call pipe_compare_sym1(p(1-nbdy,1-nbdy,k)  ,ip,text)
          write (text,'(a9,i3)') 'drag   k=',k
          call pipe_compare_sym1(drag,ip,text)
        endif
        write (text,'(a9,i3)') 'stress k=',k
        call pipe_compare_sym1(stress,iv,text)
        util4(1:ii,1:jj) =  uflux(1:ii,  1:jj  )
     &                     +uflux(2:ii+1,1:jj  )
     &                     +uflux(1:ii,  0:jj-1)
     &                     +uflux(2:ii+1,0:jj-1)
        write (text,'(a9,i3)') 'uflux  k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) =  potvor(1:ii,  1:jj  )
     &                     +potvor(2:ii+1,1:jj  )
        write (text,'(a9,i3)') 'potvor k=',k
        call pipe_compare_sym1(util4, iv,text)
        util4(1:ii,1:jj) =  vflux1(1:ii,  1:jj  )
     &                     -vflux1(1:ii,  0:jj-1)
     &                     +vflux3(1:ii,  1:jj  )
     &                     -vflux2(1:ii,  1:jj  )
        write (text,'(a9,i3)') 'vflux  k=',k
        call pipe_compare_sym1(util4, iv,text)
        write (text,'(a9,i3)') 'v.n    k=',k
        call pipe_compare_sym1(v(1-nbdy,1-nbdy,k,n),iv,text)
      endif
c
cdiag if (k.eq.1) then
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag write (lp,101) nstep
cdiag do j=jtest-1,jtest+1
cdiag do i=itest-1,itest+1
cdiag if     (iv(i,j).ne.1) then
cdiag write (lp,'(2i5,i3,2f8.3)') i+i0,j+j0,k,
cdiag.  0.0,0.0
cdiag else
cdiag write (lp,'(2i5,i3,8f8.3)') i+i0,j+j0,k,
cdiag.  util4(i,j),v(i,j,k,n),
cdiag.  -delt1*grady(i,j),
cdiag.  -delt1*advv(i,j),
cdiag.  -delt1*
cdiag.   .125*(uflux(i,j  )+uflux(i+1,j  )+uflux(i,j-1)+uflux(i+1,j-1))
cdiag.       *(potvor(i,j)+potvor(i+1,j)),
cdiag.  -delt1*vbrhs(i,j),
cdiag.   delt1*stress(i,j),
cdiag.   delt1*diffv(i,j)
cdiag endif
cdiag enddo !i
cdiag enddo !j
cdiag endif !test
cdiag endif !k==1
 101    format(i9,8x,'vold    vnew   gradp  nonlin   corio',
     &3x,'vbrhs  stress    fric')
c
 9    continue  ! k=1,kk
c
c --- update dpu.m,dpv.m to time level t   with RA time smoother
c --- update dpu.n,dpv.n to time level t+1
c
      margin = 1
c
*        wtime1( 7) = wtime()
c --- p is from dp.m, see momtum_hs
      call dpudpv(dpu(1-nbdy,1-nbdy,1,m),
     &            dpv(1-nbdy,1-nbdy,1,m),
     &            p,depthu,depthv, margin,max(0,margin-1))
c
!$OMP PARALLEL DO PRIVATE(j,i,k)
!$OMP&           SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            do k=1,kk
              p(i,j,k+1) = p(i,j,k) + dp(i,j,k,n)
            enddo !k
          endif !ip
        enddo !i
      enddo !j
c
      call dpudpv(dpu(1-nbdy,1-nbdy,1,n),
     &            dpv(1-nbdy,1-nbdy,1,n),
     &            p,depthu,depthv, margin,max(0,margin-1))
c    
c --- convert from dpv*vtot to vtot to v
c --- substitute depth-weighted averages at massless grid points.
c --- extract barotropic velocities generated during most recent
c --- baroclinic time step and use them to force barotropic flow field.
c
      margin = 0
c
!$OMP PARALLEL DO PRIVATE(j,i,k,q,sum_m,sum_n)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            k=1
              u(i,j,k,m) = u(i,j,k,m)/max(dpu(i,j,k,m),dpthin)
              u(i,j,k,n) = u(i,j,k,n)/max(dpu(i,j,k,n),dpthin)
              sum_m      = u(i,j,k,m)*    dpu(i,j,k,m)
              sum_n      = u(i,j,k,n)*    dpu(i,j,k,n)
            do k=2,kk
              u(i,j,k,m) = u(i,j,k,m)/max(dpu(i,j,k,m),dpthin)
              q          = min(dpu(i,j,k,m),cutoff)
              u(i,j,k,m) = (u(i,j,k,m)*q+u(i,j,k-1,m)*(cutoff-q))*
     &                     qcutoff
              u(i,j,k,n) = u(i,j,k,n)/max(dpu(i,j,k,n),dpthin)
              q          = min(dpu(i,j,k,n),cutoff)
              u(i,j,k,n) = (u(i,j,k,n)*q+u(i,j,k-1,n)*(cutoff-q))*
     &                     qcutoff
              sum_m      = sum_m + u(i,j,k,m)*dpu(i,j,k,m)
              sum_n      = sum_n + u(i,j,k,n)*dpu(i,j,k,n)
            enddo !k
c
            sum_m = sum_m/depthu(i,j)
            sum_n = sum_n/depthu(i,j)
            do k=1,kk
              u(i,j,k,m) = u(i,j,k,m) - sum_m
              u(i,j,k,n) = u(i,j,k,n) - sum_n
            enddo !k
c
            utotn(i,j) = dt1inv*(sum_n - ubavg(i,j,n))  !for barotp
          endif !iu
        enddo !i
c
        do i=1-margin,ii+margin
          if (SEA_V) then
            k=1
              v(i,j,k,m) = v(i,j,k,m)/max(dpv(i,j,k,m),dpthin)
              v(i,j,k,n) = v(i,j,k,n)/max(dpv(i,j,k,n),dpthin)
              sum_m      = v(i,j,1,m)*    dpv(i,j,1,m)
              sum_n      = v(i,j,1,n)*    dpv(i,j,1,n)
            do k=2,kk
              v(i,j,k,m) = v(i,j,k,m)/max(dpv(i,j,k,m),dpthin)
              q          = min(dpv(i,j,k,m),cutoff)
              v(i,j,k,m) = (v(i,j,k,m)*q+v(i,j,k-1,m)*(cutoff-q))*
     &                     qcutoff
              v(i,j,k,n) = v(i,j,k,n)/max(dpv(i,j,k,n),dpthin)
              q          = min(dpv(i,j,k,n),cutoff)
              v(i,j,k,n) = (v(i,j,k,n)*q+v(i,j,k-1,n)*(cutoff-q))*
     &                     qcutoff
              sum_m      = sum_m + v(i,j,k,m)*dpv(i,j,k,m)
              sum_n      = sum_n + v(i,j,k,n)*dpv(i,j,k,n)
            enddo !k
c
            sum_m = sum_m/depthv(i,j)
            sum_n = sum_n/depthv(i,j)
            do k=1,kk
              v(i,j,k,m) = v(i,j,k,m) - sum_m
              v(i,j,k,n) = v(i,j,k,n) - sum_n
            enddo !k
c
            vtotn(i,j) = dt1inv*(sum_n - vbavg(i,j,n))  !for barotp
          endif !iv
        enddo !i
      enddo !j - do 30 loop
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        do k= 1,kk
          write (text,'(a9,i3)') 'dpu.m  k=',k
          call pipe_compare_sym1(dpu(1-nbdy,1-nbdy,k,m),iu,text)
          write (text,'(a9,i3)') 'dpu.n  k=',k
          call pipe_compare_sym1(dpu(1-nbdy,1-nbdy,k,n),iu,text)
          write (text,'(a9,i3)') 'u.n(2) k=',k
          call pipe_compare_sym1(  u(1-nbdy,1-nbdy,k,n),iu,text)
          write (text,'(a9,i3)') 'dpv.m  k=',k
          call pipe_compare_sym1(dpv(1-nbdy,1-nbdy,k,m),iv,text)
          write (text,'(a9,i3)') 'dpv.n  k=',k
          call pipe_compare_sym1(dpv(1-nbdy,1-nbdy,k,n),iv,text)
          write (text,'(a9,i3)') 'v.n(2) k=',k
          call pipe_compare_sym1(  v(1-nbdy,1-nbdy,k,n),iv,text)
        enddo !k
        write (text,'(a9,i3)') 'utotn  k=',kk
        call pipe_compare_sym1(utotn, iu,text)
        write (text,'(a9,i3)') 'vtotn  k=',kk
        call pipe_compare_sym1(vtotn, iv,text)
      endif
c
*        wtime1(10) = wtime()
*        if     (mod(nstep,100).eq.0) then
* ---      timer printout.
*          do i= 2,10
*            write(lp,'(4x,a,i3,a,f9.5,a)')
*    &        'momtum   section',i,' time =',
*    &        wtime1(i) - wtime1(i-1),' seconds'
*          enddo
*          do i= 2,20
*            wtimes=0.0
*            do k=1,kk
*              wtimes = wtimes + (wtime2(i,k)-wtime2(i-1,k))
*            enddo
*            write(lp,'(4x,a,i3,a,f9.5,a)')
*    &        'momtum k section',i,' time =',
*    &        wtimes,' seconds'
*          enddo
*          call flush(lp)
*        endif
c
      if     (lpipe .and. lpipe_momtum) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'u.n(3) k=',1
        call pipe_compare_sym1(u(1-nbdy,1-nbdy,1,n),iu,text)
        write (text,'(a9,i3)') 'v.n(3) k=',1
        call pipe_compare_sym1(v(1-nbdy,1-nbdy,1,n),iv,text)
      endif
c
      return
      end subroutine momtum4

      end module mod_momtum
c
c
c> Revision history
c>
c> Mar. 1995 - changed min.depth in pot.vort. calculation from 1 mm to 1 m
c>             (loops 812,802,803)
c> July 1997 - transferred -visc- and -vort- from common to local
c> July 1997 - eliminated 3-D arrays -uold,vold- (used in time smoothing)
c> Aug. 1997 - added some loops that used to be in barotp.f
c> Aug. 1997 - transferred -wgtia,wgtib,wgtja,wgtjb- from common to local
c> Oct. 1999 - ekman depth calculation added in loops 66-70 - based on ustar
c>             which is calculated from the stress
c> Oct. 1999 - surface stress terms handled differently depending on m.l.
c>             model
c> Dec. 1999 - switched to biharmonic friction
c> Jan. 2000 - added biharm to select between laplacian and biharmonic
c> Jan. 2000 - ekman depth calculation removed - surface momentum flux
c>             distributed within layer one for all mixed layer models
c> Jan. 2000 - added r. bleck's re-formulation of -pgfx- and -pgfy-
c>             to properly commute pressure torque from layer to layer
c> Jul. 2000 - loop reordering and logic changes for OpenMP
c> Aug. 2000 - loop 117 executed only when hybrid vertical coordinate is used
c> Oct. 2001 - replaced biharm with veldf[24] and visco[24]
c> Sep. 2004 - kapref selects one of three thermobaric reference states
c> Jun. 2006 - split out momtum_hs, for initial ssh calculation
c> Nov. 2006 - inserted volume-force for tide
c> Apr. 2007 - added drglim, implicit or CFL-limited explicit bottom drag
c> Apr. 2007 - added dragrh, linear tidal drag based on bottom roughness
c> Apr. 2007 - btrlfr: moved [uvp]bavg assignment to barotp
c> Jun. 2007 - added momtum4
c> Mar  2009 - more accurate kappaf, with potential density
c> Now  2009 - several bugfixes to momtum4
c> Mar  2010 - remove   anti-drag on detided velocity
c> Apr  2010 - put back anti-drag on detided velocity
c> Apr  2011 -  cbarp(i,j) in place of cbar
c> Jul  2011 - salfac(i,j) in place of tidsal
c> Jul  2011 - modified momtum4 based on NERSC 2.1.03_MPI momtum_quick
c> Jul  2011 - modified momtum4 to use viscp and viscq
c> Jul  2011 - modified momtum4 to use local arrays for y advection
c> Jul  2011 - modified momtum4 to always use v 0 -v for extrapolation
c> Aug  2011 - use ra2fac for wuv1 and wuv2 (so now wuv==wts)
c> Aug  2011 - replaced dpold,dpoldm with dpo
c> Aug  2011 - reworked Robert-Asselin time filter
c> Sep  2011 -    cbp(i,j) in place of cb
c> Jan  2012 - added thkcdw
c> July 2012 - bugfix for bottom drag when depth < thkbot
c> July 2012 - thkbop is now always based on thkbot (even for bblkpp)
c> Nov. 2012 - surtx,y are halo_pv (not halo_ps)
c> Nov. 2012 - wndflg=4 for calculating wind stress using cd_coare
c> Nov. 2012 - oftaux,oftauy for wind stress offset
c> Jan. 2013 - added thkdrg=0.0 for tidal drag in barotp.f
c> Jan. 2013 - replaced dragrh with drgten.1.1
c> Jan. 2013 - added gtide for tidal body forcing
c> May  2013 - removed thbase from montg.kk
c> July 2013 - vamax set to 34 m/s
c> Sep. 2013 - optionally added Stokes drift 
c> Nov. 2013 - wndflg=5 for calculating wind stress using cd_core2
c> Jan. 2014 - tv is in Kelvin (cd_core2)
c> Jan. 2014 - added gslpr for atmospheric pressure forcing
c> Jan. 2014 - added natm
c> Apr. 2014 - added pair for atmospheric pressure forcing
c> Apr. 2014 - added ice shelf logic (ishlf)
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Oct  2014 - added cd_coarep for flxflg=6
c> Oct  2014 - flxflg=6   uses samo in place of wind everywhere
c> Oct  2014 - flxflg=4,5 uses wind-current magnitude and direction for stress
c> Oct. 2042 - dragw_rho appropriate for thkcdw=3.0, 2x default
c> Apr. 2015 - moved atmospheric pressure forcing to barotp
c> Apr. 2015 - moved tidal body forcing to barotp
c> Sep. 2015 - calculate dudzu,dvdzu using a Lagrange interpolation
