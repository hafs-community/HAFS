#if defined(USE_ESMF)
      program hycom
c
c --- ESMF driver for stand-alone HYCOM ocean model
c
      use ESMF_Mod
*     use mod_hycom, only : OCEAN_SetServices => HYCOM_SetServices
      use mod_hycom, only : end_of_run,
     &                      OCEAN_SetServices => HYCOM_SetServices
c
      implicit none
c
c --- Local variables
c
c --- Gridded Components
      type(ESMF_GridComp) :: oceanGridComp
c
c --- States, Virtual Machines, and Layouts
      type(ESMF_VM) :: worldVM
      type(ESMF_State) :: oceanImpState, oceanExpState
      integer :: petCount, localPet, split
c
c --- Calendars and clocks
      type(ESMF_Clock) :: worldClock
      type(ESMF_Clock) :: oceanClock
c
c --- Return codes for error checks
      integer :: rc
c
c --- Miscellaneous
      integer :: i
c
c-------------------------------------------------------------------------------
c  Initialize the ESMF Framework
c-------------------------------------------------------------------------------
c
c --- Set default calendar and log type; get world VM
      call ESMF_Initialize(defaultCalendar=ESMF_CAL_GREGORIAN,
     &                     defaultLogType=ESMF_LOG_SINGLE,
     &                     vm=worldVM, rc=rc)
      if (rc .ne. ESMF_SUCCESS) stop 99
c
c --- Get VM info
      call ESMF_VMGet(worldVM, petCount=petCount, localPET=localPet,
     &                rc=rc)
      if (ESMF_LogMsgFoundError(rc, "ESMF_VMGet failed", rc)) 
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Create section
c-------------------------------------------------------------------------------
c
c --- Create the OCEAN gridded component
      oceanGridComp = ESMF_GridCompCreate(vm=worldVM,
     &                name="OCEAN Gridded Component",
     &                gridCompType=ESMF_OCEAN,
     &                rc=rc)
      if (ESMF_LogMsgFoundError(rc, "OCEAN GridCompCreate failed", rc))
     &   goto 10
c
c --- Create empty OCEAN import/export states
      oceanImpState = ESMF_StateCreate(stateName="OCEAN Import State",
     &                                 stateType=ESMF_STATE_IMPORT,
     &                                 rc=rc)
      if (ESMF_LogMsgFoundError(rc, "OCEAN ImpState Create failed", rc))
     &   goto 10
      oceanExpState = ESMF_StateCreate(stateName="OCEAN Export State",
     &                                 stateType=ESMF_STATE_EXPORT,
     &                                 rc=rc)
      if (ESMF_LogMsgFoundError(rc, "OCEAN ExpState Create failed", rc))
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Register section
c-------------------------------------------------------------------------------
c
c --- Register the OCEAN gridded component
      call ESMF_GridCompSetServices(oceanGridComp,
     &                              OCEAN_SetServices, rc)
      if (ESMF_LogMsgFoundError(rc, "OCEAN Registration failed", rc))
     &   goto 10
c
c-------------------------------------------------------------------------------
c --- Initalize Section
c-------------------------------------------------------------------------------
c
c --- Initialize OCEAN gridded component
      call ESMF_GridCompInitialize(gridComp=oceanGridComp,
     &                             importState=oceanImpState,
     &                             exportState=oceanExpState,
     &                             clock=worldClock,
     &                             phase=ESMF_SINGLEPHASE,
     &                             blockingflag=ESMF_NONBLOCKING,
     &                             rc=rc)
      if (ESMF_LogMsgFoundError(rc, "OCEAN Initialize failed", rc))
     &   goto 10
*c
*c --- Get copy of OCEAN clock
*      call ESMF_GridCompGet(oceanGridComp, clock=oceanClock, rc=rc)
*c
*c --- Initialize WORLD clock using OCEAN clock
*      worldClock = ESMF_ClockCreate(clock=oceanClock, rc=rc)
c
c-------------------------------------------------------------------------------
c --- Run Section
c-------------------------------------------------------------------------------
c
      do !until end of run
        call ESMF_GridCompRun(gridComp=oceanGridComp,
     &                        importState=oceanImpState,
     &                        exportState=oceanExpState,
     &                        clock=worldClock,
     &                        phase=ESMF_SINGLEPHASE,
     &                        blockingflag=ESMF_NONBLOCKING,
     &                        rc=rc)
c
c ---   use end_of_run, rather than a ESMF Clock
        if     (end_of_run) then
          exit
        endif
      enddo
c
c-------------------------------------------------------------------------------
c  Finalize Section
c-------------------------------------------------------------------------------
c
c --- Finalize OCEAN gridded component
      call ESMF_GridCompFinalize(gridComp=oceanGridComp,
     &                           importState=oceanImpState,
     &                           exportState=oceanExpState,
     &                           clock=worldClock,
     &                           phase=ESMF_SINGLEPHASE,
     &                           blockingflag=ESMF_NONBLOCKING,
     &                           rc=rc)
      if (ESMF_LogMsgFoundError(rc, "OCEAN Finalize failed", rc)) 
     &   goto 10
c
10    continue
      call ESMF_VMBarrier(worldVM)
      call ESMF_Finalize(rc=rc)
c
      stop
      end program hycom
#else
c
      program hycom
c
c --- Non-ESMF driver for stand-alone HYCOM ocean model
c
      use mod_hycom, only : end_of_run,
     &                      HYCOM_Init,
     &                      HYCOM_Run,
     &                      HYCOM_Final
c
      implicit none
c
c --- Initialize HYCOM.
      call HYCOM_Init

c --- Run HYCOM.
      do !until end of run
        call HYCOM_Run
        if     (end_of_run) then
          exit
        endif
      enddo
c
c --- Finalize HYCOM.
      call HYCOM_Final
c
      stop
      end program hycom
#endif
