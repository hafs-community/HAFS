      module mod_cb_arrays
      use mod_dimensions
      implicit none
      public ! everything is public
c
c     wrapper for common_blocks.h
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2) ::
#endif
     & u,v,           ! velocity components
     & dp,dpo,        ! layer thickness on p-grid
     & dpu,dpv,       ! layer thickness on u-grid and v-grid
     & temp,          ! temperature
     & saln,          ! salinity
     & th3d,          ! potential density
     & thstar,        ! virtual potential density
     & montg          ! montgomery potential

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:,:) ::
#else
      real, save,
     &      dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2,mxtrcr) ::
#endif
     & tracer         ! inert tracers

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1) ::
#endif
     & p,pu,pv        ! interface pressure

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) ::
#endif
     & theta,         ! isopycnal layer target densties - thbase
     & diaflx         ! time integral of diapyc.flux

#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & corio,         ! coriolis parameter
     & potvor,        ! potential vorticity
     & srfhgt,        ! sea surface height, g*ssh(m)
     & steric,        ! steric sea surface height, g*sssh(m)
     & sshgmn,        !   mean sea surface height, g*mssh(m)
     & thmean,        !   mean depth averaged density
     & montg1,        ! layer 1 montgomery potential
     & skap           ! thermobaric scale factor between reference states

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) ::
#endif
     & psikk,         ! montg.pot. in bottom layer
     & thkk           ! virtual potential density in bottom layer
c                                                                   
#if defined(RELO)
      integer, save, allocatable, dimension(:,:) ::
#else
      integer, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & kapi           ! thermobaric reference state index (1 or 3)
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) ::
#endif
     & uflx,vflx,     ! mass fluxes
     & uflxav,vflxav, ! average fluxes
     & dpav           ! average fluxes

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,3) ::
#endif
     & ubavg,vbavg,   ! barotropic velocity
     & pbavg          ! barotropic pressure

#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & defor1,defor2, ! deformation components
     & ubrhs, vbrhs,  ! rhs of barotropic u,v eqns.
     & utotm, vtotm,  ! total (barotrop.+baroclin.)..
     & utotn, vtotn,  ! ..velocities at 2 time levels
     & uflux, vflux,  ! horizontal mass fluxes
     & uflux1,vflux1, ! more mass fluxes
     & uflux2,vflux2, ! more mass fluxes
     & uflux3,vflux3  ! more mass fluxes
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & util1,util2,   ! arrays for temporary storage
     & util3,util4,   ! arrays for temporary storage
     & util5,util6,   ! arrays for temporary storage
     & plon, plat,    ! lon,lat at p pts
     & ulon, ulat,    ! lon,lat at u pts
     & vlon, vlat,    ! lon,lat at v pts
     & pang,          ! angle between xwards and ewards
     & scux, scuy,    ! mesh size at u pts in x,y dir.
     & scvx, scvy,    ! mesh size at v pts in x,y dir.
     & scpx, scpy,    ! mesh size at p pts in x,y dir.
     & scqx, scqy,    ! mesh size at q pts in x,y dir.
     & scu2, scv2,    ! grid box area at u,v pts
     & scp2, scq2,    ! grid box area at p,q pts
     & scp2i,scq2i,   ! inverses of scp2,scq2
     & scuxi,scvyi,   ! inverses of scux,scvy
     & aspux,aspuy,   ! u-grid aspect ratios for diffusion
     & aspvx,aspvy,   ! v-grid aspect ratios for diffusion
     & veldf2u,       ! u-grid laplacian  diffusion coefficient
     & veldf2v,       ! v-grid laplacian  diffusion coefficient
     & veldf4u,       ! u-grid biharmonic diffusion coefficient
     & veldf4v,       ! v-grid biharmonic diffusion coefficient
     & thkdf4u,       ! u-grid biharmonic diffusion coefficient
     & thkdf4v,       ! v-grid biharmonic diffusion coefficient
     & cbp,           ! p-grid quadratic bottom friction coefficient
     & cbarp,         ! p-grid rms flow speed for linear bottom friction
     & pgfx, pgfy,    ! horiz. presssure gradient
     & gradx,grady,   ! horiz. presssure gradient
     & depthu,depthv, ! bottom pres. at u,v points
     & pvtrop,        ! pot.vort. of barotropic flow
     & depths,        ! water depth
     & drag,          ! bottom drag
     & salfac,        ! spatialy varying "scalar" SAL factor
     & topiso,        ! shallowest depth for isopycnal layers (pressure units)
     & diws,          ! spacially varying background/internal wave diffusivity
     & diwm,          ! spacially varying background/internal wave viscosity
     & diwbot,        ! background/internal wave diffusivity at the bottom
     & diwqh0,        ! background/internal wave diffusivity vertical scale
     & sssrmx,        ! maximum SSS difference for relaxation (psu)
     & tidepg_mn,     ! tidal pressure gradient forcing, time mean
     & displd_mn,     ! dissipation from linear    drag, time mean
     & dispqd_mn      ! dissipation from quadratic drag, time mean

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:) ::
#else
      real, save, dimension(1:2,1:2,1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & drgten         ! tidal bottom drag tensor
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & uja,   ujb,    ! velocities at lateral ..
     & via,   vib,    !       .. neighbor points
     & pbot,          ! bottom pressure at t=0
     & sgain,         ! salin.changes from diapyc.mix.
     & surtx,         ! surface net x-stress on p-grid
     & surty,         ! surface net y-stress on p-grid
     & surflx,        ! surface net thermal energy flux
     & sswflx,        ! surface swv thermal energy flux
     & mixflx,        ! mixed layer thermal energy flux
     & sstflx,        ! surface thermal  flux from sst relax
     & sssflx,        ! surface salinity flux from sss relax
     & rivflx,        ! surface salinity flux from rivers
     & salflx,        ! surface salinity flux
     & buoflx,        ! mixed layer buoyancy flux
     & bhtflx,        ! mixed layer buoyancy flux from heat
     & wndocn,        ! magnitude of 10m wind minus ocean current
     & ustar,         ! friction velocity
     & ustarb,        ! bottom friction velocity
     & turgen,        ! turb.kin.energ. generation
     & thkice,        ! grid-cell avg. ice thknss (m)
     & covice,        ! ice coverage (rel.units)
     & temice,        ! ice surface temperature
     & flxice,        ! heat flux under ice
     & fswice,        ! swv  flux under ice
     & sflice,        ! salt flux under ice
     &   si_c,        ! ice concentration   on p-grid from coupler
     &   si_h,        ! ice thickness       on p-grid from coupler
     &   si_t,        ! ice temperature     on p-grid from coupler
     &   si_u,        ! ice u-velocity      on p-grid from coupler
     &   si_v,        ! ice v-velocity      on p-grid from coupler
     &  si_tx,        ! x-stress  under ice on p-grid from coupler
     &  si_ty         ! y-stesss  under ice on p-grid from coupler
c
#if defined(RELO)
      integer, save, allocatable, dimension(:,:) ::
#else
      integer, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & klist,         ! k-index
     & jerlov         ! jerlov water type 1-5, 0 for kpar, -1 for chl
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) ::
#endif
     & dpmixl,        ! mixed layer depth
     & t1sav,         ! upper sublayer temperature
     & s1sav,         ! upper sublayer salinity
     & tmlb,          ! temp in lyr. containing mlb.
     & smlb           ! saln in lyr. containing mlb

#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & hekman,        ! ekman layer thickness
     & hmonob,        ! monin-obukhov length
     & dpbl,          ! turbulent boundary layer depth
     & dpbbl,         ! bottom turbulent boundary layer depth
     & dpmold,        ! mixed layer depth at old time step
     & tmix,          ! mixed layer temperature
     & smix,          ! mixed layer salinity
     & thmix,         ! mixed layer potential density
     & umix,  vmix    ! mixed layer velocity

#if defined(RELO)
      real, save, allocatable, dimension(:) ::
#else
      real, save, dimension(kdm) ::
#endif
     & dp0k,          ! minimum deep    z-layer separation
     & ds0k           ! minimum shallow z-layer separation

      real, save ::
     & dpns,          ! depth to start terrain following
     & dsns           ! depth to stop  terrain following
c
#if defined(RELO)
      integer, save, allocatable, dimension(:,:,:) ::
#else
      integer, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) ::
#endif
     & nmlb           ! layer containing mlb.
c
c ---  s w i t c h e s    (if set to .true., then...)
c --- btrlfr      leapfrog barotropic time step
c --- btrmas      barotropic is mass conserving
c --- diagno      output model fields and diagnostic messages
c --- thermo      use thermodynamic forcing (flxflg>0)
c --- windf       use wind stress   forcing (wndflg>0)
c --- pcipf       use evap-precip surface salinity flux
c --- epmass      treat evap-precip as a mass exchange
c --- mslprf      use msl presssure forcing
c --- priver      use river precip bogas
c --- rivera      annual-only river precip bogas
c --- kparan      annual-only kpar or chl
c --- relax       activate lateral boundary T/S/p  climatological nudging
c --- srelax      activate surface salinity        climatological nudging
c ---              (sssflg==1 or -1)
c --- trelax      activate surface temperature     climatological nudging
c ---              (sstflg==1)
c --- trcrlx      activate lateral boundary tracer climatological nudging
c --- relaxf      input T/S/p   relaxation fields
c --- relaxs      input surface relaxation fields only
c --- relaxt      input tracer  relaxation fields
c --- locsig      use locally-referenced potential density for stability
c --- vsigma      use spacially varying target densities
c --- hybrid      use hybrid vertical coordinates
c --- isopyc      use isopycnic vertical coordinates (MICOM mode)
c --- icegln      use energy loan ice model (iceflg==1)
c --- hybraf      HYBGEN: apply Robert-Asselin filter during hybgen
c --- isopcm      HYBGEN: use PCM to remap isopycnal layers
c --- mxl_no      NO:  deactivate             mixed layer model (mlflag==0)
c --- mxlkta      KT:    activate    original mixed layer model (mlflag==2)
c --- mxlktb      KT:    activate alternative mixed layer model (mlflag==3)
c --- mxlkrt      KT:    activate MICOM or HYCOM Kraus-Turner (mlflag==2,3)
c --- pensol      KT:    activate penetrating solar radiation
c --- mxlkpp      KPP:   activate mixed layer model (mlflag==1)
c --- bblkpp      KPP:   activate bottom boundary layer
c --- shinst      KPP:   activate shear instability mixing
c --- dbdiff      KPP:   activate double diffusion  mixing
c --- nonloc      KPP:   activate nonlocal b. layer mixing
c --- botdiw      KPROF: activate bot.enhan.int.wav mixing
c --- difout      KPROF: output visc/diff coeffs in archive
c --- mxlmy       MY2.5: activate mixed layer model (mlflag==5)
c --- mxlpwp      PWP:   activate mixed layer model (mlflag==4)
c --- mxlgiss     GISS:  activate mixed layer model (mlflag==6)
c --- stroff      add a net wind stress offset
c --- flxoff      add a net heat flux offset
c --- flxsmo      activate smoothing of surface fluxes
c --- trcrin      initialize tracer from restart file
c --- trcout      advect tracer and save results in history/restart file
c --- dsur1p      single point only surface diagnostics
c --- arcend      always write a 3-d archive at the end of the run
c --- frcflg      distort atmospheric fluxes for some dubious purpose
c
      logical, save ::
     &              btrlfr,btrmas,diagno,thermo,windf,mslprf,
     &              pcipf,epmass,priver,rivera,kparan,
     &              relax,srelax,trelax,trcrlx,relaxf,relaxs,relaxt,
     &              locsig,vsigma,hybrid,isopyc,icegln,hybraf,isopcm,
     &              mxl_no,mxlkta,mxlktb,mxlkrt,pensol,
     &              mxlkpp,bblkpp,shinst,dbdiff,nonloc,
     &              botdiw,difout,
     &              mxlmy,mxlpwp,mxlgiss,
     &              stroff,flxoff,flxsmo,trcrin,trcout,
     &              dsur1p,arcend
     &              ,frcflg
c
c ---  t e x t
c ---  ctitle     four lines describing the simulation
c
      character*80, save, dimension(4) :: ctitle
c
c --- atmospheric forcing fields, natm is 2 or 4
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm) ::
#endif
     & taux,          ! wind stress in x direction
     & tauy,          ! wind stress in y direction
     & wndspd,        ! wind speed
     & ustara,        ! ustar (tke source)
     & mslprs,        ! mean sea level pressure (anomaly)
     & airtmp,        ! air temperature
     & vapmix,        ! atmosph. vapor mixing ratio
     & precip,        ! precipitation
     & radflx,        ! net solar radiation
     & swflx,         ! net shortwave radiation
     & surtmp,        ! surface temp. used to calculate input lw radiation
     & seatmp         ! best available SST from observations

c<-hsk
#if defined(USE_SCOUPLER)
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & seatmp_,       ! another best available SST from observations
     & taux_,         ! wind stress in x direction
     & tauy_,         ! wind stress in y direction
     & precip_,       ! precipitation (m/s) (>0 into the ocean)
     & radflx_,       ! net radiative flux  (>0 into the ocean)
     & swflx_,        ! net shortwave radiation (>0 into the ocean)
     & mslprs_,       ! atmospheric pressure (>0 into the ocean): raw input=total atm pressure
                      ! in HYCOM_Init, it is used as anomalies, by mslprs_=mslprs_ - prsbas.
     & snsibl_,       ! sensible heat flux (>0 into the atmosphere)
     & latent_,         ! latent heat flux   (>0 into the atmosphere)
     & taux_g,          ! wind stress in x direction
     & tauy_g,          ! wind stress in y direction
     & precip_g,        ! precipitation (m/s) (>0 into the ocean)
     & radflx_g,        ! net radiative flux  (>0 into the ocean)
     & swflx_g,         ! net shortwave radiation (>0 into the ocean)
     & mslprs_g       ! atmospheric pressure (>0 into the ocean)

      real           :: cflag,tau_,momfac,thefac,atmfac,radfac
      integer        :: nwhich
#endif
c
c--- STOKES drift through WW3 coupling
#if defined(WW3CPL)
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & dtaux_,       ! x-direction wind stress modified by WW3
     & dtauy_,       ! y-direction wind stress modigied by WW3
     & stkwl_,       ! mean depth (DPM in cpl3.f and lamda in ww.comm.f) for the surface Stokes Drift can reach from WW3
     & kpphg_,       ! bogus variable: KPP mixing for WW3 (employed only for POM)
     & wbcond_,      ! bogus variable: vertical velocity at the wave layer, coming from WW3
     & usdc_,       ! x-direction surface Stokes drift from WW3 on u-grids 
     & vsdc_        ! y-direction surface Stokes drift from WW3 on v-grids
#endif /* WW3CPL */
c->hsk
c
c --- monthly atmospheric forcing fields
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,4) ::
#endif
     & akpar,         ! photosynthetically available radiation coefficent
                      ! or chlorophyll-a (jerlov=-1)
     & rivers         ! river inflow bogused to surface precipitation
 
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & oftaux,        ! wind stress offset in x direction
     & oftauy,        ! wind stress offset in y direction
     & offlux         ! net heat flux offset

#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(0:24,-91:91) ::
#endif
     & diurnl         ! hourly vs latitude shortwave scale factor table
c
c --- surface and sidewall and nestwall boundary fields
c ---  (kkwall and kknest are either kdm or, if inactive, 1).
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:) ::
#else
      real, save,
     &      dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kkwall,4) ::
#endif
     & pwall,         ! pressure b.c. at sidewalls
     & swall,         ! salinity b.c. at sidewalls
     & twall          ! temp.    b.c. at sidewalls

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:,:) ::
#else
      real, save, 
     &      dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kkwall,4,
     &                                                   mxtrcr) ::
#endif
     & trwall         ! tracer   b.c. at sidewalls

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:) ::
#else
      real, save,
     &      dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kknest,2) ::
#endif
     & pnest,         ! pressure b.c. at nestwalls
     & snest,         ! salinity b.c. at nestwalls
     & tnest,         ! temp.    b.c. at nestwalls
     & unest,         ! u-vel.   b.c. at nestwalls
     & vnest          ! v-vel.   b.c. at nestwalls
 
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) ::
#endif
     & ubnest,        ! barotropic u-velocity at nestwalls
     & vbnest,        ! barotropic v-velocity at nestwalls
     & ubpnst,        ! barotropic u-velocity at nestwalls on p-grid
     & vbpnst,        ! barotropic v-velocity at nestwalls on p-grid
     & pbnest         ! barotropic pressure   at nestwalls

#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & rmu,           ! weights for   s.w.b.c. relax
     & rmunp,         ! weights for p.n.w.b.c. relax
     & rmunv,         ! weights for v.n.w.b.c. relax
     & rmutra         ! weights for tracr.b.c. relax (maximum of all tracers)

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,mxtrcr) ::
#endif
     & rmutr          ! weights for tracr.b.c. relax

#if defined(RELO)
      integer, save, allocatable, dimension(:,:) ::
#else
      integer, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     & maskbc         ! mask for nested barotropic boundary condition
c
c --- pwp variables
      real, save ::
     & rigc           ! PWP: critical gradient richardson number
     &,ribc           ! PWP: critical bulk richardson number
c
c --- m-y 2.5 variables
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:,:) ::
#else
      real, save,
     &      dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1,2) ::
#endif
     & q2             !  tke
     &,q2l            !  tke * turbulent length scale

#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save,
     &      dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1) ::
#endif
     & difqmy         !  tke diffusivity
     &,vctymy         !  viscosity on mellor-yamada vertical grid
     &,diftmy         !  temperature diffusivity on mellor-yamada vertical grid

      real, save ::
     & ghc            !  constant for calculating tke production
     &,sef            !  constant for calculating tke production
     &,smll           !  constant for calculating tke
     &,const1         !  coefficient for estimating surface and bottom bc's
     &,coef4          !  coefficient for calculating  viscosity/diffusivity
     &,coef5          !  coefficient for calculating  viscosity/diffusivity
     &,a1my           !  coefficient for calculating  viscosity/diffusivity
     &,b1my           !  coefficient for calculating  viscosity/diffusivity
     &,a2my           !  coefficient for calculating  viscosity/diffusivity
     &,b2my           !  coefficient for calculating  viscosity/diffusivity
     &,c1my           !  coefficient for calculating  viscosity/diffusivity
     &,e1my           !  coefficient for calculating  viscosity/diffusivity
     &,e2my           !  coefficient for calculating  viscosity/diffusivity
     &,e3my           !  coefficient for calculating  viscosity/diffusivity
c
c --- kpp variables
#if defined(RELO)
      real, save, allocatable, dimension(:,:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1) ::
#endif
     & zgrid          !  grid levels in meters
     &,vcty           !  vert. viscosity coefficient
     &,difs           !  vert. scalar diffusivity
     &,dift           !  vert. temperature diffusivity
     &,ghats          !  nonlocal transport

      real, save ::
     & vonk           !  von karman constant
     &,zmin,zmax      !  zehat limits for table
     &,umin,umax      !  ustar limits for table
     &,epsilon        ! vertical coordinate scale factor
     &,cmonob         ! KPP:    scale factor for Monin-Obukov length
     &,cekman         ! KPP:    scale factor for Ekman depth
     &,qrinfy         ! KPP:    1/max grad.rich.no. for shear instability
     &,difm0          ! KPP:    max viscosity    due to shear instability
     &,difs0          ! KPP:    max diffusivity  due to shear instability
     &,difmiw         ! KPP/MY: background/internal wave viscosity   (m^2/s)
     &,difsiw         ! KPP/MY: background/internal wave diffusivity (m^2/s)
     &,dsfmax         ! KPP:    salt fingering diffusivity factor    (m^2/s)
     &,rrho0          ! KPP:    salt fingering rp=(alpha*delT)/(beta*delS)
     &,ricr           ! KPP:    critical bulk richardson number
     &,cs             ! KPP:    value for nonlocal flux term
     &,cstar          ! KPP:    value for nonlocal flux term
     &,cv             ! KPP:    buoyancy frequency ratio (0.0 to use a fn. of N)
     &,c11            ! KPP:    value for turb velocity scale
     &,deltaz         ! delta zehat in table
     &,deltau         ! delta ustar in table
     &,vtc            ! constant for estimating background shear in rib calc.
     &,cg             ! constant for estimating nonlocal flux term of diff. eq.
     &,dp0enh         ! dist. for tapering diff. enhancement at interface nbl-1

      integer, save ::
     & niter          ! KPP: iterations for semi-implicit soln. (2 recomended)
     &,hblflg         ! KPP: b.layer interpolation flag (0=con.1=lin.,2=quad.)
c
c --- nasa giss variables
c
      integer, save ::
     &        nextrtbl0,ifexpabstable,nextrtbl1,
     &        nextrtbl,nposapprox,mt0,mt,ntbl,
     &        mt_ra_r,n_theta_r_oct,nbig
c
      real, save ::
     &        deltheta_r,pidbl,rri
c
      integer, save ::
     & irimax(-762:762)
     &,nb
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(-762:762,-nlgiss:nlgiss) ::
#endif
     & slq2b
     &,smb
     &,shb
     &,ssb
c
      real, save ::
     & ribtbl(-762:762)
     &,ridb(  -762:762)
     &,dri
     &,back_ra_r(-39:117)
     &,sisamax(  -39:117)
     &,ra_rmax(  -39:117)
     &,c_y_r0(   -39:117)
     &,sm_r1(    -39:117)
     &,sh_r1(    -39:117)
     &,ss_r1(    -39:117)
     &,slq2_r1(  -39:117)
     &,b1,visc_cbu_limit,diff_cbt_limit
     &,theta_rcrp,theta_rcrn
c
      integer, save ::
     &        ifback,ifsali,ifepson2,ifrafgmax,
     &        ifsalback,ifchengcon,ifunreal,idefmld,
     &        ifpolartablewrite,ifbg_theta_interp
c
      real, save ::
     &       back_ph_0,adjust_gargett,back_k_0,back_del_0,back_s2,
     &       ri0,ebase,epson2_ref,
     &       eps_bot0,scale_bot,      !for bottom-enhanced
     &       eplatidepmin,wave_30,    !and latitude dependent mixing
     &       deltemld,delrhmld,
     &       back_sm2,v_back0,t_back0,
     &       s_back0,ri_internal,backfrac,backfact,ako,tpvot0,sgmt,
     &       tptot0,tpcot0,ttot0,tcot0,tctot0,tpvot,tptot,tpcot,
     &       ttot,tcot,tctot,back_l_0
c
      real*8, save :: area,avgbot,watcum,empcum,wndrep
c
      real, save ::
     &               time,delt1,dlt,
     &                w0, w1, w2, w3,  ! wind  interp. scale factors
     &                wk0,wk1,wk2,wk3, ! kpar  interp. scale factors
     &                wr0,wr1,wr2,wr3, ! river interp. scale factors
     &                wc0,wc1,wc2,wc3, ! clim. interp. scale factors
     &                wn0,wn1,         ! nest  interp. scale factors
     &                wb0,wb1          ! baro. interp. scale factors
c
      integer, save ::
     &                nstep,nstep1,nstep2,lstep,
     &                l0, l1, l2, l3,  ! wind  indexes
     &                lk0,lk1,lk2,lk3, ! kpar  indexes
     &                lr0,lr1,lr2,lr3, ! river indexes
     &                lc0,lc1,lc2,lc3, ! clim. indexes
     &                ln0,ln1,         ! nest  indexes
     &                lb0,lb1          ! baro. indexes
c
c --- 'sigma ' = isopyncnal layer target densities (sigma units)
c --- 'thbase' = reference density (sigma units)
c --- 'saln0'  = initial salinity value
c --- 'baclin' = baroclinic time step
c --- 'batrop' = barotropic time step
c ---'qhybrlx' = HYBGEN: relaxation coefficient (inverse baroclinic time steps)
c --- 'hybiso' = HYBGEN: Use PCM if layer is within hybiso of target density
c --- 'visco2' = deformation-dependent Laplacian  viscosity factor
c --- 'visco4' = deformation-dependent biharmonic viscosity factor
c --- 'facdf4' =       speed-dependent biharmonic viscosity factor
c --- 'veldf2' = diffusion velocity (m/s) for Laplacian  momentum dissipation
c --- 'veldf4' = diffusion velocity (m/s) for biharmonic momentum dissipation
c --- 'temdf2' = diffusion velocity (m/s) for Laplacian  temp/saln diffusion
c --- 'temdfc' = temp diffusion conservation (0.0 all density, 1.0 all temp)
c --- 'thkdf2' = diffusion velocity (m/s) for Laplacian  thickness diffusion
c --- 'thkdf4' = diffusion velocity (m/s) for biharmonic thickness diffusion
c --- 'vertmx' = diffusion velocity (m/s) for mom.mixing across mix.layr.base
c --- 'tofset' = temperature anti-drift offset (degC/century)
c --- 'sofset' = salnity     anti-drift offset  (psu/century)
c --- 'diapyc' = KT: diapycnal diffusivity x buoyancy freq. (m**2/s**2)
c --- 'dtrate' = KT: maximum permitted m.l. detrainment rate (m/day)
c --- 'slip'   = +1 for free-slip, -1  for non-slip boundary conditions
c --- 'cb'     = coefficient of quadratic bottom friction
c --- 'cbar'   = rms flow speed (m/s) for linear bottom friction law
c --- 'drglim' = limiter for explicit friction (1.0 no limiter, 0.0 implicit)
c --- 'drgscl' = scale factor for tidal drag   (0.0 for no tidal drag)
c --- 'thkdrg' = thickness of bottom boundary layer for tidal drag (m)
c --- 'dsurfq' = number of days between model diagnostics at the surface
c --- 'diagfq' = number of days between model diagnostics
c --- 'proffq' = number of days between model diagnostics at some locations
c --- 'tilefq' = number of days between model diagnostics on some tiles
c --- 'meanfq' = number of days between model diagnostics (time averaged)
c --- 'rstrfq' = number of days between model restart output
c --- 'bnstfq' = number of days between baro. nesting archive input
c --- 'nestfq' = number of days between 3-d   nesting archive input
c --- 'ra2fac' = weight for Robert-Asselin time filter
c --- 'wbaro'  = weight for time smoothing of barotropic u,v,p field
c --- 'thkmls' = reference mixed-layer thickness for SSS relaxation (m)
c --- 'thkmlt' = reference mixed-layer thickness for SST relaxation (m)
c --- 'thkriv' = nominal thickness of river inflow (m)
c --- 'thkcdw' = thickness for near-surface currents in ice-ocean stress
c --- 'thkfrz' = maximum thickness of near surface freezing zone (m)
c --- 'tfrz_0' = ENLN: ice melting point (degC) at S=0psu
c --- 'tfrz_s' = ENLN: gradient of ice melting point (degC/psu)
c --- 'ticegr' = ENLN: vertical temperature gradient inside ice (deg/m)
c ---                   (0.0 to get ice surface temp. from atmos. surtmp)
c --- 'hicemn' = ENLN: minimum ice thickness (m)
c --- 'hicemx' = ENLN: maximum ice thickness (m)
c --- 'thkmin' = KT/PWP: minimum mixed-layer thickness (m)
c --- 'bldmin' = KPP:    minimum surface boundary layer thickness (m)
c --- 'bldmax' = KPP:    maximum surface boundary layer thickness (m)
c --- 'thkbot' = thickness of bottom boundary layer (m)
c --- 'sigjmp' = minimum density jump across interfaces   (theta units)
c --- 'tmljmp' = equivalent temperature jump across the mixed layer (degC)
c --- 'prsbas' = msl pressure is input field + prsbas (Pa)
c --- 'salmin' = minimum salinity allowed in an isopycnic layer (psu)
c --- 'dp00'   = deep    z-level spacing minimum thickness (m)
c --- 'dp00x'  = deep    z-level spacing maximum thickness (m)
c --- 'dp00f'  = deep    z-level spacing stretching factor (1.0=const.z)
c --- 'ds00'   = shallow z-level spacing minimum thickness (m)
c --- 'ds00x'  = shallow z-level spacing maximum thickness (m)
c --- 'ds00f'  = shallow z-level spacing stretching factor (1.0=const.z)
c --- 'dp00i'  = deep iso-pycnal spacing minimum thickness (m)
c --- 'isotop' = shallowest depth for isopycnal layers     (m), <0 from file
c --- 'nhybrd' = number of hybrid levels (0=all isopycnal)
c --- 'nsigma' = number of sigma  levels (nhybrd-nsigma z-levels)
c --- 'hybmap' = HYBGEN:  remapper  flag (0=PCM,1=PLM,2=PPM,-ve:isoPCM)
c --- 'hybflg' = HYBGEN:  generator flag (0=T&S,1=th&S,2=th&T)
c --- 'advflg' = thermal  advection flag (0=T&S,1=th&S,2=th&T)
c --- 'advtyp' = scalar   advection type (0=PCM,1=MPDATA,2=FCT2,4=FCT4)
c --- 'momtyp' = momentum advection type (2=2nd order, 4=4th order)
c --- 'kapref' = thermobaric reference state (-1=input,0=none,1,2,3=constant)
c --- 'kapnum' = number of thermobaric reference states (1 or 2)
c --- 'tsofrq' = number of time steps between anti-drift offset calcs
c --- 'mixfrq' = KT: number of time steps between diapycnal mixing calcs
c --- 'icefrq' = relax to tfrz with e-folding time of icefrq time steps
c --- 'icpfrq' = number of time steps between sea-ice updates
c --- 'ntracr' = number of tracers (<=mxtrcr)
c --- 'trcflg' = tracer type flag (one per tracer)
c --- 'clmflg' = climatology frequency flag (6=bimonthly,12=monthly)
c --- 'dypflg' = KT: diapycnal mixing flag (0=none,1=KPP,2=explicit)
c --- 'iniflg' = initial state flag (0=level,1=zonal,2=climatology)
c --- 'lbflag' = lateral baro. bndy flag (0=none;nest:2=B-K,4=Flather,6=clamped)
c ---             (port: 1=Browning-Kreiss,3=Flather)
c --- 'mapflg' = map flag (0=mercator,2=uniform,3=beta-plane,4=input)
c --- 'yrflag' = days in year flag   (0=360,1=366,2=366Jan1,3=actual)
c --- 'sshflg' = diagnostic SSH flag (0=SSH,1=SSH&stericSSH)
c --- 'iversn' = hycom version number x10
c --- 'iexpt'  = experiment number x10
c --- 'jerlv0' = initial jerlov water type (1 to 5; 0 for kpar, -1 for chl)
c --- 'iceflg' = sea ice model flag   (0=none,1=energy loan,2=coupled/esmf)
c --- 'ishelf' = ice shelf flag       (0=none,1=ice shelf over ocean)
c --- 'wndflg' = wind str. input flag (0=none,1=on u/v grid,2,3=on p grid)
c --- 'ustflg' = ustar forcing flag          (3=input,1=wndspd,2=stress)
c --- 'flxflg' = thermal forcing flag (0=none,3=net-flux,1,2,4=sst-based)
c --- 'empflg' = E-P     forcing flag (0=none,3=net_E-P, 1,2,4=sst-based_E)
c --- 'dswflg' = diurnal shortwv flag (0=none,1=daily to diurnal correction)
c --- 'albflg' = ocean albedo    flag (0=none,1=.06,2=L&Y)
c --- 'sssflg' = SSS relaxation  flag (0=none,1=clim,-1=clim+rmx)
c --- 'lwflag' = longwave corr.  flag (0=none,1=clim,2=atmos), sst-based
c --- 'sstflg' = SST relaxation  flag (0=none,1=clim,2=atmos,3=obs)
c --- 'icmflg' = ice mask        flag (0=none,1=clim,2=atmos,3=obs)
c --- 'difsmo' = KPROF: number of layers with horiz smooth diff coeffs
c
#if defined(RELO)
      real, save, allocatable, dimension(:) ::
#else
      real, save, dimension(kdm) ::
#endif
     &  sigma,
     &  salmin
c
      real, save ::
     &               thbase,saln0,baclin,batrop,
     &               qhybrlx,hybiso,
     &               visco2,visco4,veldf2,veldf4,facdf4,
     &               temdf2,temdfc,thkdf2,thkdf4,vertmx,diapyc,
     &               tofset,sofset,dtrate,slip,cb,cbar,
     &               drglim,drgscl,thkdrg,
     &               dsurfq,diagfq,proffq,tilefq,meanfq,
     &               rstrfq,bnstfq,nestfq,
     &               ra2fac,wbaro,
     &               thkmls,thkmlt,thkriv,thkmin,bldmin,bldmax,thkbot,
     &               thkcdw,thkfrz,tfrz_0,tfrz_s,ticegr,hicemn,hicemx,
     &               dp00,dp00f,dp00x,ds00,ds00f,ds00x,dp00i,isotop,
     &               sigjmp,tmljmp,prsbas
c
      integer, save ::
     &               tsofrq,mixfrq,icefrq,icpfrq,nhybrd,nsigma,
     &               hybmap,hybflg,advflg,advtyp,momtyp,
     &               kapref,kapnum,
     &               ntracr,trcflg(mxtrcr),
     &               clmflg,dypflg,iniflg,lbflag,mapflg,yrflag,sshflg,
     &               iversn,iexpt,jerlv0,
     &               iceflg,ishelf,icmflg,wndflg,ustflg,
     &               flxflg,empflg,dswflg,albflg,lwflag,sstflg,sssflg,
     &               difsmo,disp_count
c
      real,    parameter ::
     &  g     =   9.806,      !gravitational acceleration (m/s**2)
     &  thref =   1.0d-3,     !reference value of specific volume (m**3/kg)
     &  onem   = g/thref,     !one meter in pressure units (Pa)
     &  tenm   = onem*10.0,
     &  tencm  = onem* 0.1,
     &  onecm  = onem* 0.01,
     &  onemm  = onem* 0.001,
     &  spcifh = 3990.0,      !specific heat of sea water (j/kg/deg)
     &  epsil  = 1.0d-11,     !small nonzero to prevent division by zero
     &  hugel  = 2.0**100,    !large number used to indicate land points
     &  pi     = 3.14159265358979323846d0,
     &  radian = pi/180.0,
     &  qonem  = 1.0/onem,
     &  qthref = 1.0/thref
c
c --- grid point where detailed diagnostics are desired:
      integer, save :: itest,jtest,ittest,jttest
c
c --- filenames.
      character*80, save ::
     &              flnmdep,flnmgrd,flnmshlf,
     &              flnmrsi,flnmrso, flnmflx,
     &              flnmarc,flnmovr,flnmfor,flnmforw,flnminp,
     &              flnmarcm,
     &              flnmarcs,
     &              flnmarct
c
c --- CCSM3 variables (should be in CCSM3 modules)
      logical, save :: dosstbud,doovtn,chk_ovtn,dodump,dohist,dorestart
c
      integer, save :: istrt_mo,icurrent_mo,istrt_yr,icurrent_yr
c
c --- diurnal cycle factor for short wave heat flux
      integer, save :: nsteps_per_day,nsteps_today
c
c<-hsk @10/27/2016
#if defined(WW3CPL)
      real, save :: sfcu,sfcv,dpcu,dpcv,stkx_,stky_,wdpth
#endif
c-> hsk


      contains

      subroutine cb_allocate
c
c --- Allocate saved arrays
c
      call set_r_init
c
#if defined(RELO)
      call gindex_allocate   !from mod_dimensions
c
      allocate(
     &              u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &              v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &             dp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &            dpo(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &            dpu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &            dpv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &           temp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &           saln(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &           th3d(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &         thstar(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2),
     &          montg(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2) )
      call mem_stat_add( 11*(idm+2*nbdy)*(jdm+2*nbdy)*kdm*2 )
#endif
                    u = r_init
                    v = r_init
                   dp = r_init
                  dpo = r_init
                  dpu = r_init
                  dpv = r_init
                 temp = r_init
                 saln = r_init
                 th3d = r_init
               thstar = r_init
                montg = r_init
c
      if     (ntracr.gt.0) then
#if defined(RELO)
        allocate(
     &        tracer(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,2,ntracr) )
        call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)*kdm*2*ntracr )
#endif
              tracer = r_init
      endif
c
#if defined(RELO)
      allocate(
     &              p(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &             pu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &             pv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1) )
      call mem_stat_add( 3*(idm+2*nbdy)*(jdm+2*nbdy)*(kdm+1) )
#endif
                    p = r_init
                   pu = r_init
                   pv = r_init
c
#if defined(RELO)
      allocate(
     &         theta(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &        diaflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
#endif
               theta = r_init
              diaflx = r_init
c
#if defined(RELO)
      allocate(
     &        corio(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       potvor(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       srfhgt(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       steric(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       sshgmn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       thmean(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       montg1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         skap(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 8*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
              corio = r_init
             potvor = r_init
             srfhgt = r_init
             steric = r_init
             sshgmn = r_init
             thmean = r_init
             montg1 = r_init
               skap = r_init
c
#if defined(RELO)
      allocate(
     &         psikk(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &          thkk(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*2 )
#endif
               psikk = r_init
                thkk = r_init
c
#if defined(RELO)
      allocate(
     &         kapi(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)/2 )  !real=2*int
#endif
               kapi = -99
c
#if defined(RELO)
      allocate(
     &          uflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &          vflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &        uflxav(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &        vflxav(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm),
     &          dpav(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm) )
      call mem_stat_add( 5*(idm+2*nbdy)*(jdm+2*nbdy)*kdm )
#endif
                uflx = r_init
                vflx = r_init
              uflxav = r_init
              vflxav = r_init
                dpav = r_init
c
#if defined(RELO)
      allocate(
     &         ubavg(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,3),
     &         vbavg(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,3),
     &         pbavg(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,3) )
      call mem_stat_add( 3*(idm+2*nbdy)*(jdm+2*nbdy)*3 )
#endif
               ubavg = r_init
               vbavg = r_init
               pbavg = r_init
c
#if defined(RELO)
      allocate(
     &        defor1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        defor2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         ubrhs(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         vbrhs(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         utotm(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         vtotm(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         utotn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         vtotn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ,
     &         uflux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         vflux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        uflux1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        vflux1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        uflux2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        vflux2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        uflux3(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        vflux3(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 16*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
              defor1 = r_init
              defor2 = r_init
               ubrhs = r_init
               vbrhs = r_init
               utotm = r_init
               vtotm = r_init
               utotn = r_init
               vtotn = r_init
               uflux = r_init
               vflux = r_init
              uflux1 = r_init
              vflux1 = r_init
              uflux2 = r_init
              vflux2 = r_init
              uflux3 = r_init
              vflux3 = r_init
c
#if defined(RELO)
      allocate(
     &         util1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         util2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         util3(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         util4(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         util5(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         util6(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          plon(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          plat(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          ulon(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          ulat(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          vlon(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          vlat(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          pang(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scuy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scvx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scvy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scpx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scpy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scqx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scqy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scu2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scv2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scp2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          scq2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         scp2i(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         scq2i(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         scuxi(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         scvyi(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         aspux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         aspuy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         aspvx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         aspvy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       veldf2u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       veldf2v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       veldf4u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       veldf4v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       thkdf4u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       thkdf4v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           cbp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         cbarp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          pgfx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          pgfy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         gradx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         grady(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        depthu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        depthv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        pvtrop(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        depths(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          drag(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        salfac(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        topiso(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          diws(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          diwm(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        diwbot(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        diwqh0(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        sssrmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &     tidepg_mn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &     displd_mn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &     dispqd_mn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 60*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
               util1 = r_init
               util2 = r_init
               util3 = r_init
               util4 = r_init
               util5 = r_init
               util6 = r_init
                plon = r_init
                plat = r_init
                ulon = r_init
                ulat = r_init
                vlon = r_init
                vlat = r_init
                pang = r_init
                scux = r_init
                scuy = r_init
                scvx = r_init
                scvy = r_init
                scpx = r_init
                scpy = r_init
                scqx = r_init
                scqy = r_init
                scu2 = r_init
                scv2 = r_init
                scp2 = r_init
                scq2 = r_init
               scp2i = r_init
               scq2i = r_init
               scuxi = r_init
               scvyi = r_init
                aspux = r_init
               aspuy = r_init
               aspvx = r_init
               aspvy = r_init
             veldf2u = r_init
             veldf2v = r_init
             veldf4u = r_init
             veldf4v = r_init
             thkdf4u = r_init
             thkdf4v = r_init
                 cbp = r_init
               cbarp = r_init
                pgfx = r_init
                pgfy = r_init
               gradx = r_init
               grady = r_init
              depthu = r_init
              depthv = r_init
              pvtrop = r_init
              depths = r_init
                drag = r_init
              salfac = r_init
              topiso = r_init
                diws = r_init
                diwm = r_init
              diwbot = r_init
              diwqh0 = r_init
              sssrmx = r_init
           tidepg_mn = r_init
           displd_mn = r_init
           dispqd_mn = r_init
c
#if defined(RELO)
      allocate(
     &        drgten(1:2,1:2,1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 4*(idm+2*nbdy)*(jdm+2*nbdy)*2 )
#endif
              drgten = r_init
c
#if defined(RELO)
      allocate(
     &           uja(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           ujb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           via(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           vib(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          pbot(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         sgain(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         surtx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         surty(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        surflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        sswflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        mixflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        sstflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        sssflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        rivflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        salflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        buoflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        bhtflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         ustar(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ustarb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        turgen(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        thkice(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        covice(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        temice(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        flxice(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        fswice(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        sflice(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          si_c(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          si_h(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          si_t(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          si_u(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          si_v(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         si_tx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         si_ty(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 33*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
                 uja = r_init
                 ujb = r_init
                 via = r_init
                 vib = r_init
                pbot = r_init
               sgain = r_init
               surtx = r_init
               surty = r_init
              surflx = r_init
              sswflx = r_init
              mixflx = r_init
              sstflx = r_init
              sssflx = r_init
              rivflx = r_init
              salflx = r_init
              buoflx = r_init
              bhtflx = r_init
               ustar = r_init
              ustarb = r_init
              turgen = r_init
              thkice = r_init
              covice = r_init
              temice = r_init
              flxice = r_init
              fswice = r_init
              sflice = r_init
                si_c = r_init
                si_h = r_init
                si_t = r_init
                si_u = r_init
                si_v = r_init
               si_tx = r_init
               si_ty = r_init
c
      if     (flxflg.eq.6) then
#if defined(RELO)
        allocate(wndocn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy))
#endif
        wndocn = r_init
      endif !flxflg.eq.6
c
#if defined(RELO)
      allocate(
     &        klist(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       jerlov(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy) ) !real=2*int
#endif
              klist = -99
             jerlov = -99
c
#if defined(RELO)
      allocate(
     &        dpmixl(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &         t1sav(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &         s1sav(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &          tmlb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &          smlb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) )
      call mem_stat_add( 5*(idm+2*nbdy)*(jdm+2*nbdy)*2 )
#endif
              dpmixl = r_init
               t1sav = r_init
               s1sav = r_init
                tmlb = r_init
                smlb = r_init
c
#if defined(RELO)
      allocate(
     &        hekman(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        hmonob(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          dpbl(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         dpbbl(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        dpmold(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          tmix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          smix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         thmix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          umix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          vmix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 19*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
              hekman = r_init
              hmonob = r_init
                dpbl = r_init
               dpbbl = r_init
              dpmold = r_init
                tmix = r_init
                smix = r_init
               thmix = r_init
                umix = r_init
                vmix = r_init
c
#if defined(RELO)
      allocate(
     &        nmlb(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) )
      call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy) ) !real=2*int
#endif
              nmlb = -99
c
#if defined(RELO)
      allocate(
     &           taux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &           tauy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         wndspd(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         ustara(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         mslprs(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         airtmp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         vapmix(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         precip(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         radflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &          swflx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         surtmp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm),
     &         seatmp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,natm) )
      call mem_stat_add( 12*(idm+2*nbdy)*(jdm+2*nbdy)*natm )
      allocate(
     &          akpar(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,4),
     &         rivers(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,4) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*4 )
#endif
                 taux = r_init
                 tauy = r_init
               wndspd = r_init
               ustara = r_init
               mslprs = r_init
               airtmp = r_init
               vapmix = r_init
               precip = r_init
               radflx = r_init
                swflx = r_init
               surtmp = r_init
               seatmp = r_init
                akpar = r_init
               rivers = r_init
c
c<-hsk: 
#if defined(USE_SCOUPLER)
#if defined(RELO)
c  for WRF
       allocate(
     &           taux_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           tauy_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         mslprs_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         precip_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         radflx_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          swflx_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         snsibl_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         latent_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 8*(idm+2*nbdy)*(jdm+2*nbdy) )
c for GFS
      allocate(
     &           taux_g(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           tauy_g(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         mslprs_g(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         precip_g(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         radflx_g(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          swflx_g(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ) 
      call mem_stat_add( 6*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif 
           taux_ = r_init
           tauy_ = r_init
         mslprs_ = r_init
         precip_ = r_init
         radflx_ = r_init
          swflx_ = r_init
         snsibl_ = r_init
         latent_ = r_init

          taux_g = r_init
          tauy_g = r_init
        mslprs_g = r_init
        precip_g = r_init
        radflx_g = r_init
         swflx_g = r_init
#endif	
c
#if defined(WW3CPL)
#if defined(RELO)
c  for WW3
       allocate(
     &           dtaux_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           dtauy_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
           dtaux_ = r_init
           dtauy_ = r_init
#endif
c
#if defined(WW3CPL)
#if defined(RELO)
c  for WW3
       allocate(stkwl_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         wbcond_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          kpphg_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           usdc_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           vsdc_(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
           kpphg_ = 0.
          wbcond_ = 0.
           stkwl_ = r_init
           usdc_  = r_init
           vsdc_  = r_init
#endif
c
c
#if defined(RELO)
      allocate(
     &     oftaux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &     oftauy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &     offlux(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 3*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
           offlux = r_init
           oftaux = r_init
           oftauy = r_init
c
#if defined(RELO)
      allocate(
     &         swall(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kkwall,4),
     &         twall(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kkwall,4) )
      call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*kkwall*4 )
#endif
               swall = r_init
               twall = r_init
c
#if defined(RELO)
      allocate(
     &         diurnl(0:24,-91:91) )
      call mem_stat_add( 25*193 )
#endif
               diurnl = r_init
c
      if     (relaxt .or. kkwall.eq.kdm) then
#if defined(RELO)
        allocate(
     &         pwall(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm,4) )
        call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)*kdm*4 )
#endif
               pwall = r_init
      endif
c
      if     (ntracr.gt.0) then
#if defined(RELO)
        allocate(
     &        trwall(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kkwall,4,ntracr) )
        call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)*kkwall*4*ntracr )
#endif
              trwall = r_init
      endif
c
#if defined(RELO)
      allocate(
     &         pnest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kknest,2),
     &         snest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kknest,2),
     &         tnest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kknest,2),
     &         unest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kknest,2),
     &         vnest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kknest,2) )
      call mem_stat_add( 5*(idm+2*nbdy)*(jdm+2*nbdy)*kknest*2 )
#endif
               pnest = r_init
               snest = r_init
               tnest = r_init
               unest = r_init
               vnest = r_init
c
#if defined(RELO)
      allocate(
     &        ubnest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &        vbnest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &        ubpnst(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &        vbpnst(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2),
     &        pbnest(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,2) )
      call mem_stat_add( 5*(idm+2*nbdy)*(jdm+2*nbdy)*2 )
#endif
              ubnest = r_init
              vbnest = r_init
              ubpnst = r_init
              vbpnst = r_init
              pbnest = r_init
c
#if defined(RELO)
      allocate(
     &          rmu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        rmunp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        rmunv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       rmutra(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 5*(idm+2*nbdy)*(jdm+2*nbdy) )
#endif
                rmu = r_init
              rmunp = r_init
              rmunv = r_init
             rmutra = r_init
c
      if     (ntracr.gt.0) then
#if defined(RELO)
        allocate(
     &         rmutr(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ntracr) )
        call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)*ntracr )
#endif
               rmutr = r_init
      endif
c
#if defined(RELO)
      allocate(
     &     maskbc(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)/2 ) !real=2*int
#endif
           maskbc = -99
c
      if (mxlmy) then
#if defined(RELO)
        kkmy25 = kk
        allocate(
     &               q2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1,2),
     &              q2l(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1,2) )
        call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy)*(kkmy25+2)*2 )
c
        allocate(
     &           difqmy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1),
     &           vctymy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1),
     &           diftmy(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,0:kkmy25+1) )
        call mem_stat_add( 3*(idm+2*nbdy)*(jdm+2*nbdy)*(kkmy25+2) )
#endif
                     q2 = r_init
                    q2l = r_init
                 difqmy = r_init
                 vctymy = r_init
                 diftmy = r_init
#if defined(RELO)
      else
        kkmy25 = -1
#endif
      endif !mxlmy
c
#if defined(RELO)
      allocate(
     &          zgrid(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &           vcty(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &           difs(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &           dift(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1),
     &          ghats(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,kdm+1) )
      call mem_stat_add( 5*(idm+2*nbdy)*(jdm+2*nbdy)*(kdm+1) )
#endif
                zgrid = r_init
                 vcty = r_init
                 difs = r_init
                 dift = r_init
                ghats = r_init
c
      if     (mxlgiss) then
#if defined(RELO)
        allocate(
     &    slq2b(-762:762,-762:762),
     &      smb(-762:762,-762:762),
     &      shb(-762:762,-762:762),
     &      ssb(-762:762,-762:762) )
        call mem_stat_add( 4*1525*1525 )
#endif
          slq2b = r_init
            smb = r_init
            shb = r_init
            ssb = r_init
      endif !mxlgiss
c
      end subroutine cb_allocate

      end module mod_cb_arrays
c
c> Revision history:
c>
c> Feb. 2001 - added halo and converted to f90 declarations
c> Aug. 2001 - added bnstfq,nestfq
c> Jan. 2002 - added curvilinear grid arrays and deleted /pivot/
c> May  2002 - added d[ps]00*
c> May  2002 - added PWP and MY2.5 mixed layer models
c> Aug  2002 - added ntracr and trcflg
c> Nov  2002 - added thkmls and thkmlt
c> Apr  2003 - added dp00i, vsigma, and priver
c> May  2003 - added bldmin, bldmax, flxsmo, and icmflg
c> Jun  2003 - added locsig
c> Nov  2003 - added advtyp
c> Jan  2004 - added latdiw
c> Jan  2004 - added bblkpp
c> Jan  2004 - added hblflg
c> Feb  2004 - added botdiw
c> Feb  2004 - added temdfc
c> Mar  2004 - added thkriv and epmass
c> Mar  2004 - added isotop
c> Mar  2005 - added tfrz_0, tfrz_s, ticegr, hicemn, and hicemx
c> Mar  2005 - added tsofrq, tofset, and sofset
c> Mar  2005 - added empflg
c> May  2005 - added kapref and kapnum
c> Jun  2006 - added surtx,surty
c> Jun  2006 - added icefrq,txice,tyice,uice,vice,flxice,fswice,sflice
c> Jun  2006 - added thkfrz
c> Jan  2007 - added si_t; renamed si_[chuv] and si_t[xy]
c> Feb  2007 - added CCSM3-only variables
c> Apr  2007 - added dragrh,drglim,drgscl
c> Apr  2007 - added srfhgt,montg1
c> Apr  2007 - added btrlfr and btrmas
c> Jun  2007 - added momtyp and facdf4.
c> Sep  2007 - added hybmap, hybiso and isopcm.
c> Feb  2008 - added thkdrg.
c> Feb  2008 - added sshflg and steric,sshgmn,thmean.
c> Jun  2008 - added tilefq.
c> Mar  2008 - added dswflg and diurnl.
c> Dec  2008 - difsmo is now an integer number of layers.
c> Jan  2009 - added arcend.
c> Jun  2009 - added sssrmx.
c> Mar  2010 - added diwbot.
c> Apr  2010 - changed sssrmx to an array.
c> Apr  2010 - added diwqh0; removed diwlat.
c> Apr  2010 - added proffq.
c> Nov  2010 - added wndrep.
c> Apr  2011 - added flnmarcs.
c> Apr  2011 - added cbarp.
c> Apr  2011 - replaced huge with hugel, to avoid clash with intrinsic huge.
c> Jul  2011 - added salfac.
c> Aug  2011 - replaced dpold and dpoldm with dpo
c> Aug  2011 - added ra2fac, removed wts[12] and wuv[12]
c> Aug  2011 - added hybraf
c> Sep  2011 - added cbp.
c> Nov  2011 - added icpfrq
c> Jan  2012 - added thkcdw
c> Mar  2012 - replaced dssk with dpns and dsns
c> Nov  2012 - added stroff and oftaux,oftauy
c> Jan  2013 - replaced dragrh with drgten
c> Apr  2013 - added displd_mn and dispqd_mn and tidepg_mn
c> July 2013 - added diws and diwm
c> Oct  2013 - jerlov constants now in swfrac_ij (thermf.f)
c> Nov  2013 - added rivflx
c> Nov  2013 - added albflg
c> Jan  2014 - added prsbas, mslprf and mslprs
c> Jan  2014 - /consts/ replaced by parameters
c> Jan  2014 - /dicycr/ deleted
c> Jan  2014 - added natm
c> Jan  2014 - replaced common_blocks.h with mod_cb_arrays.F
c> Jan  2014 - macro /* RELO */ for relocatable (region independent) version
c> Apr  2014 - added ishelf and flnmshlf
c> Oct  2014 - added wndocn
c> Feb  2015 - added pang for coupled cases
c> Aug  2015 - added mxl_no
