      program hycom_cice
c
c --- ESMF driver for HYCOM ocean model and CICE sea-ice model
c
      use ESMF_Mod
      use mod_hycom, only :
     &      OCN_put_export  => put_export,
     &      OCN_get_import  => get_import,
     &      OCN_end_of_run  => end_of_run,
     &      OCN_nts_day     => nts_day,
     &      OCN_nts_cpl     => nts_ice,
     &      OCN_SetServices => HYCOM_SetServices
      use ice_kinds_mod
      use CICE_ComponentMod, only :
     &      ICE_SetServices => CICE_SetServices
      use CICE_InitMod, only :
     &      ICE_nts_day     => nts_day
      use CICE_RunMod, only :
     &      ICE_put_export  => put_export,
     &      ICE_get_import  => get_import,
     &      ICE_end_of_run  => end_of_run
      use mod_OICPL, only :
     &      CPL_i2o         => ice2ocn_phase,
     &      CPL_o2i         => ocn2ice_phase,
     &      CPL_SetServices => OICPL_SetServices
c
      implicit none
c
c --- Local variables
c
c --- Gridded Components
      type(ESMF_GridComp) :: ocnGridComp,     !HYCOM as an ESMF component
     &                       iceGridComp      ! CICE as an ESMF component
c
c --- Coupler Components
      type(ESMF_CplComp)  :: o2iCplComp
c
c --- States, Virtual Machines, and Layouts
      type(ESMF_State)    :: ocnImpState,     ! HYCOM import state
     &                       ocnExpState,     ! HYCOM export state
     &                       iceImpState,     ! CICE  import state
     &                       iceExpState,     ! CICE  export state
     &                       cplImpState,     ! OICPL import state
     &                       cplExpState      ! OICPL export state
c
      type(ESMF_VM) :: worldVM
      integer :: petCount, localPet, split
c
c --- Calendars and clocks
      type(ESMF_Clock) :: worldClock
      type(ESMF_Clock) :: ocnClock
      type(ESMF_Clock) :: iceClock
c
c --- Return codes for error checks
      integer :: rc,rc2
c
c --- ICE coupling frequency
      integer :: ice_nts_cpl,ocn_cpl_day
c
c --- Miscellaneous
      integer :: i,its,its_ocn,its_ice,icpl,iday
c
c-------------------------------------------------------------------------------
c  Initialize the ESMF Framework
c-------------------------------------------------------------------------------
c
c --- Set default calendar and log type; get world VM
      rc = ESMF_Success
      call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN,
     &                      defaultLogType=ESMF_LOG_MULTI,
     &                                  vm=worldVM,
     &                                  rc=rc)
      if (rc .ne. ESMF_SUCCESS) stop 99
c
c --- Get VM info
      call ESMF_VMGet(worldVM, petCount=petCount, localPET=localPet,
     &                rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "ESMF_VMGet failed", rcToReturn=rc2))
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Create section
c-------------------------------------------------------------------------------
c
c --- Create the OCEAN gridded component
      ocnGridComp = ESMF_GridCompCreate(
     &                                name="OCEAN Gridded Component",
     &                        gridCompType=ESMF_OCEAN,
     &                                  rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OCEAN  GridCompCreate failed", rcToReturn=rc2))
     &   goto 10
c
c --- Create empty OCEAN  import/export states
      ocnImpState = ESMF_StateCreate(stateName="OCEAN Import",
     &                               stateType=ESMF_STATE_IMPORT,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OCEAN  ImpState Create failed", rcToReturn=rc2))
     &   goto 10

      ocnExpState = ESMF_StateCreate(stateName="OCEAN Export",
     &                               stateType=ESMF_STATE_EXPORT,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OCEAN  ExpState Create failed", rcToReturn=rc2))
     &   goto 10
c
c --- Create the SEAICE gridded component
      iceGridComp = ESMF_GridCompCreate(
     &                                name='SEAICE Component',
     &                        gridcomptype=ESMF_SEAICE,
     &                                  rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "SEAICE GridCompCreate failed", rcToReturn=rc2))
     &   goto 10
c
c --- Create empty SEAICE import/export states
      iceImpState = ESMF_StateCreate(stateName="SEAICE Import",
     &                               stateType=ESMF_STATE_IMPORT,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "SEAICE ImpState Create failed", rcToReturn=rc2))
     &   goto 10

      iceExpState = ESMF_StateCreate(stateName="SEAICE Export",
     &                               stateType=ESMF_STATE_EXPORT,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "SEAICE ExpState Create failed", rcToReturn=rc2))
     &   goto 10
c
c --- Create the OICPL coupler component
      o2iCplComp = ESMF_CplCompCreate(
     &             name="OICPL Coupler Component",
     &             rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPLE CplCompCreate failed", rcToReturn=rc2))
     &   goto 10
c
c --- Create empty OICPL import/export states
      cplImpState = ESMF_StateCreate(stateName="OICPL Import",
     &                               stateType=ESMF_STATE_IMPORT,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL ImpState Create failed", rcToReturn=rc2))
     &   goto 10

      cplExpState = ESMF_StateCreate(stateName="OICPL Export",
     &                               stateType=ESMF_STATE_EXPORT,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL ExpState Create failed", rcToReturn=rc2))
     &   goto 10
c
c --- Add OCEAN and SEAICE states to OICPL states
      CALL ESMF_StateAdd(cplImpState, ocnImpState, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL: Add OCEAN  impState failed", rcToReturn=rc2))
     &   goto 10

      CALL ESMF_StateAdd(cplImpState, iceImpState, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL: Add SEAICE impState failed", rcToReturn=rc2))
     &   goto 10

      CALL ESMF_StateAdd(cplExpState, ocnExpState, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL: Add OCEAN  expState failed", rcToReturn=rc2))
     &   goto 10

      CALL ESMF_StateAdd(cplExpState, iceExpState, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL: Add SEAICE impState failed", rcToReturn=rc2))
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Register section
c-------------------------------------------------------------------------------
c
c --- Register the OCEAN  gridded component
      call ESMF_GridCompSetServices(ocnGridComp,
     &                              OCN_SetServices, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OCEAN  Registration failed", rcToReturn=rc2))
     &   goto 10
c
c --- Register the SEAICE gridded component
      call ESMF_GridCompSetServices(iceGridComp,
     &                              ICE_SetServices, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "SEAICE Registration failed", rcToReturn=rc2))
     &   goto 10
c
c --- Register the OICPL coupler component
      call ESMF_CplCompSetServices(o2iCplComp,
     &                             CPL_SetServices,rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL  Registration failed", rcToReturn=rc2))
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Initalize Section
c-------------------------------------------------------------------------------
c
c --- Initialize OCEAN  gridded component
      call ESMF_GridCompInitialize(    gridComp=ocnGridComp,
     &                              importState=ocnImpState,
     &                              exportState=ocnExpState,
     &                                    phase=ESMF_SINGLEPHASE,
     &                             blockingflag=ESMF_NONBLOCKING,
     &                                       rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OCEAN Initialize failed", rcToReturn=rc2))
     &   goto 10
c
c --- Initialize SEAICE gridded component
      call ESMF_GridCompInitialize(    gridComp=iceGridComp,
     &                              importState=iceImpState,
     &                              exportState=iceExpState,
     &                                    phase=ESMF_SINGLEPHASE,
     &                             blockingflag=ESMF_NONBLOCKING,
     &                                       rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "SEAICE Initialize failed", rcToReturn=rc2))
     &   goto 10
c
      ocn_cpl_day = OCN_nts_day/OCN_nts_cpl
      ice_nts_cpl = ICE_nts_day/ocn_cpl_day
      if     (localPet.eq.0) then !master
        write(6,'(a,i5)') 'OCN_nts_day = ',OCN_nts_day
        write(6,'(a,i5)') 'ICE_nts_day = ',ICE_nts_day
        write(6,'(a,i5)') 'OCN_nts_cpl = ',OCN_nts_cpl
        write(6,'(a,i5)') 'ice_nts_cpl = ',ice_nts_cpl
      endif
      if     (OCN_nts_day.ne.ocn_cpl_day*OCN_nts_cpl) then
        if     (localPet.eq.0) then !master
          write(6,*) 'ERROR OCN_nts_cpl not a divisor of OCN_nts_day'
        endif
        goto 10
      endif
      if     (ICE_nts_day.ne.ocn_cpl_day*ice_nts_cpl) then
        if     (localPet.eq.0) then !master
          write(6,*) 'ERROR ice_nts_cpl not a divisor of ICE_nts_day'
        endif
        goto 10
      endif
c
c --- Initialize OICPL coupler component
      call ESMF_CplCompInitialize(     cplComp=o2iCplComp,
     &                             importState=cplImpState,
     &                             exportState=cplExpState,
     &                                   phase=ESMF_SINGLEPHASE,
     &                            blockingflag=ESMF_BLOCKING,
     &                                      rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL  Initialize failed", rcToReturn=rc2))
     &   goto 10
c
c --- Couple SEAICE to OCEAN
      call ESMF_CplCompRun(     cplComp=o2iCplComp,
     &                      importState=cplImpState,
     &                      exportState=cplExpState,
     &                            phase=CPL_i2o,
     &                     blockingflag=ESMF_BLOCKING,
     &                               rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL I2O Run failed", rcToReturn=rc2))
     &   goto 10
c
c --- Couple OCEAN to SEAICE
      call ESMF_CplCompRun(     cplComp=o2iCplComp,
     &                      importState=cplImpState,
     &                      exportState=cplExpState,
     &                            phase=CPL_o2i,
     &                     blockingflag=ESMF_BLOCKING,
     &                               rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL I2O Run failed", rcToReturn=rc2))
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Run Section
c-------------------------------------------------------------------------------
c
c --- Run Ocean and SeaIce in lockstep, both looking backwards for imports
      do icpl=1,huge(iday)/2 !until end of run

c ---   OCEAN
        do its= 1,OCN_nts_cpl !couple period, OCEAN
          if     (mod(its,OCN_nts_cpl).le.1 .and.
     &            localPet.eq.0) then !master
            write(6,'(a,i4,i4)') ' OCEAN run - icpl,its = ',icpl,its
          endif
          OCN_get_import = its.eq.1           !import at start of period
          OCN_put_export = its.eq.OCN_nts_cpl !export at   end of period
          call ESMF_GridCompRun(    gridComp=ocnGridComp,
     &                           importState=ocnImpState,
     &                           exportState=ocnExpState,
     &                                 phase=ESMF_SINGLEPHASE,
     &                          blockingflag=ESMF_NONBLOCKING,
     &                                    rc=rc)
          if (ESMF_LogMsgFoundError(rc,
     &        "OCEAN Run failed", rcToReturn=rc2))
     &        goto 10
        enddo !its; OCEAN
 
c ---   SEAICE
        do its= 1,ice_nts_cpl !couple period, SEAICE
          if     (mod(its,ice_nts_cpl).le.1 .and.
     &            localPet.eq.0) then !master
            write(6,'(a,i4,i4)') 'SEAICE run - icpl,its = ',icpl,its
          endif
          ICE_get_import = its.eq.1           !import at start of period
          ICE_put_export = its.eq.ice_nts_cpl !export at   end of period
!         ICE_put_export = .false.      !don't export at end   of period
          call ESMF_GridCompRun(    gridComp=iceGridComp,
     &                           importState=iceImpState,
     &                           exportState=iceExpState,
     &                                 phase=ESMF_SINGLEPHASE,
     &                          blockingflag=ESMF_NONBLOCKING,
     &                                    rc=rc)
          if (ESMF_LogMsgFoundError(rc,
     &        "SEAICE Run failed (last half day)", rcToReturn=rc2))
     &         goto 10
        enddo !its; SEAICE

c ---   use end_of_run, rather than a ESMF Clock
        if     (OCN_end_of_run) then
           exit !icpl
        endif !end_of_run

c ---   Couple SEAICE to OCEAN
        call ESMF_CplCompRun(     cplComp=o2iCplComp,
     &                        importState=cplImpState,
     &                        exportState=cplExpState,
     &                              phase=CPL_i2o,
     &                       blockingflag=ESMF_BLOCKING,
     &                                 rc=rc)
        if (ESMF_LogMsgFoundError(rc,
     &      "OICPL I2O Run failed", rcToReturn=rc2))
     &     goto 10

c ---   Couple OCEAN to SEAICE
        call ESMF_CplCompRun(     cplComp=o2iCplComp,
     &                        importState=cplImpState,
     &                        exportState=cplExpState,
     &                              phase=CPL_o2i,
     &                       blockingflag=ESMF_BLOCKING,
     &                                 rc=rc)
        if (ESMF_LogMsgFoundError(rc,
     &      "OICPL I2O Run failed", rcToReturn=rc2))
     &     goto 10

      enddo !icpl
c
      call ESMF_VMBarrier(worldVM)
c
c-------------------------------------------------------------------------------
c  Finalize Section
c-------------------------------------------------------------------------------
c
c --- Finalize OCEAN gridded component
      call ESMF_GridCompFinalize(    gridComp=ocnGridComp,
     &                            importState=ocnImpState,
     &                            exportState=ocnExpState,
     &                                  phase=ESMF_SINGLEPHASE,
     &                           blockingflag=ESMF_NONBLOCKING,
     &                                     rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OCEAN  Finalize failed", rcToReturn=rc2))
     &   goto 10
c
c --- Finalize SEAICE gridded component
      call ESMF_GridCompFinalize(    gridComp=iceGridComp,
     &                            importState=iceImpState,
     &                            exportState=iceExpState,
     &                                  phase=ESMF_SINGLEPHASE,
     &                           blockingflag=ESMF_NONBLOCKING,
     &                                     rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "SEAICE Finalize failed", rcToReturn=rc2))
     &   goto 10
c
c --- Finalize OACPL coupler component
      call ESMF_CplCompFinalize(     cplComp=o2iCplComp,
     &                           importState=cplImpState,
     &                           exportState=cplExpState,
     &                                 phase=ESMF_SINGLEPHASE,
     &                          blockingflag=ESMF_BLOCKING,
     &                                    rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &    "OICPL  Finalize failed", rcToReturn=rc2))
     &   goto 10

c
10    continue
      call ESMF_VMBarrier(worldVM)
      call ESMF_Finalize(rc=rc)
c
      stop
      end program hycom_cice
