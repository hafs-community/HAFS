      module mod_OICPL  !ocean-ice coupler
c
c --- ESMF Framework module
      use ESMF_Mod
c
      implicit none
      private
c
      public OICPL_SetServices
c
c --- phase info
      integer, parameter, public :: ice2ocn_phase = 1
      integer, parameter, public :: ocn2ice_phase = 2
c
c --- VM and PET info
      type(ESMF_VM), save :: vm
      integer,       save :: petCount,localPet
c
c --- Route handles for regridding and/or redistribution
      type(ESMF_RouteHandle), save ::
     &    i2oRouteHandle, o2iRouteHandle

      contains

      subroutine OICPL_SetServices(cplComp, rc)
c
      type(ESMF_CplComp)   :: cplComp
      integer, intent(out) :: rc
c
      call ESMF_CplCompSetEntryPoint(
     &     cplComp,
     &     ESMF_SETINIT,
     &     OICPL_Init,
     &     ESMF_SINGLEPHASE,
     &     rc=rc)
      call ESMF_CplCompSetEntryPoint(
     &     cplComp,
     &     ESMF_SETRUN,
     &     OICPL_Run_I2O,
     &     ice2ocn_phase,
     &     rc=rc)
      call ESMF_CplCompSetEntryPoint(
     &     cplComp,
     &     ESMF_SETRUN,
     &     OICPL_Run_O2I,
     &     ocn2ice_phase,
     &     rc=rc)
      call ESMF_CplCompSetEntryPoint(
     &     cplComp,
     &     ESMF_SETFINAL,
     &     OICPL_Final,
     &     ESMF_SINGLEPHASE,
     &     rc=rc)
c
      end subroutine OICPL_SetServices


      subroutine OICPL_Init(cplComp, impState, expState, extClock, rc)
c
c --- Calling parameters
      type(ESMF_CplComp)   :: cplComp
      type(ESMF_State)     :: impState
      type(ESMF_State)     :: expState
      type(ESMF_Clock)     :: extClock
      integer, intent(out) :: rc
c
c --- Locals
      integer :: rc2
      type(ESMF_State)       :: oiState, oeState, iiState, ieState
      type(ESMF_FieldBundle) :: ocnBundle,        iceBundle
c
c --- Report
      call ESMF_LogWrite("OICPL initialize routine called",
     &                   ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
c
c --- Get VM
      call ESMF_CplCompGet(cplComp, vm=vm, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get VM failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get PET info
      call ESMF_VMGet(vm, petCount=petCount, localPET=localPet, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get VM info failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get OCEAN and SEAICE import states
      call ESMF_StateGet(impState, "OCEAN Import", oiState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get OCEAN impState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
      call ESMF_StateGet(impState, "SEAICE Import", iiState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get SEAICE impState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get OCEAN and SEAICE export states
      call ESMF_StateGet(expState, "OCEAN Export", oeState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get OCEAN expState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
      call ESMF_StateGet(expState, "SEAICE Export", ieState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get SEAICE expState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Initialize I2O
c
c --- Get bundle for ocn
      call ESMF_StateGet(oiState, "HYCOM Import", ocnBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get HYCOM Import failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get bundle for ice
      call ESMF_StateGet(ieState, "CICE Export",  iceBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get CICE Export failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Transfer fields from ice state to ocn state
      call ESMF_FieldBundleRedistStore(iceBundle, ocnBundle,
     &                                 i2oRouteHandle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, 
     &   "FieldBundleRedistStore i2o failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c  Initialize O2I
c
c --- Get bundle for ice
      call ESMF_StateGet(iiState, "CICE Import", iceBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get CICE Import failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get bundle for ocn
      call ESMF_StateGet(oeState, "HYCOM Export", ocnBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get HYCOM Export failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Setup O2I route handle
      call ESMF_FieldBundleRedistStore(ocnBundle, iceBundle,
     &                                 o2iRouteHandle, rc=rc)
      if (ESMF_LogMsgFoundError(rc,
     &   "FieldBundleRedistStore o2i failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
      return
      end subroutine OICPL_Init

      subroutine OICPL_Run_I2O(cplComp, impState, expState, extClock,
     &                         rc)
c
c --- Calling parameters
      type(ESMF_CplComp)   :: cplComp
      type(ESMF_State)     :: impState
      type(ESMF_State)     :: expState
      type(ESMF_Clock)     :: extClock
      integer, intent(out) :: rc
c
c --- Locals
      integer :: rc2
      type(ESMF_State)       :: oiState,   ieState
      type(ESMF_FieldBundle) :: ocnBundle, iceBundle
c
c --- Report
      call ESMF_LogWrite("OICPL I2O run routine called",
     &                   ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
c
c --- Get OCEAN import state
      call ESMF_StateGet(impState, "OCEAN Import", oiState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get OCEAN impState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get SEAICE export state
      call ESMF_StateGet(expState, "SEAICE Export", ieState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get SEAICE expState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get bundle for ocn
      call ESMF_StateGet(oiState, "HYCOM Import", ocnBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get HYCOM Import failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get bundle for ice
      call ESMF_StateGet(ieState, "CICE Export",  iceBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get CICE Export failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Transfer fields from ice state to ocn state
      call ESMF_FieldBundleRedist(iceBundle, ocnBundle,
     &                            i2oRouteHandle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "FieldBundleRedist i2o failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
      return
      end subroutine OICPL_Run_I2O

      subroutine OICPL_Run_O2I(cplComp, impState, expState, extClock,
     &                         rc)
c
c --- Calling parameters
      type(ESMF_CplComp)   :: cplComp
      type(ESMF_State)     :: impState
      type(ESMF_State)     :: expState
      type(ESMF_Clock)     :: extClock
      integer, intent(out) :: rc
c
c --- Locals
      integer :: rc2
      type(ESMF_State)       :: oeState,   iiState
      type(ESMF_FieldBundle) :: ocnBundle, iceBundle
c
c --- Report
      call ESMF_LogWrite( "OICPL O2I run routine called",
     &                   ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
c
c --- Get SEAICE import state
      call ESMF_StateGet(impState, "SEAICE Import", iiState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get SEAICE impState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get OCEAN export state
      call ESMF_StateGet(expState, "OCEAN Export", oeState, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get OCEAN expState failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get bundle for ice
      call ESMF_StateGet(iiState, "CICE Import", iceBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get CICE Import failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Get bundle for ocn
      call ESMF_StateGet(oeState, "HYCOM Export", ocnBundle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "Get HYCOM Export failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
c --- Transfer fields from ocn state to ice state
      call ESMF_FieldBundleRedist(ocnBundle, iceBundle,
     &                            o2iRouteHandle, rc=rc)
      if (ESMF_LogMsgFoundError(rc, "FieldBundleRedist o2i failed",
     &   rcToReturn=rc2)) call ESMF_Finalize(rc=rc)
c
      return
      end subroutine OICPL_Run_O2I

      subroutine OICPL_Final(cplComp, impState, expState, extClock, rc)
c
c --- Calling parameters
      type(ESMF_CplComp)   :: cplComp
      type(ESMF_State)     :: impState
      type(ESMF_State)     :: expState
      type(ESMF_Clock)     :: extClock
      integer, intent(out) :: rc
c
c --- Locals
c
c --- Report
      call ESMF_LogWrite("OICPL finalize routine called", 
     &                   ESMF_LOG_INFO, rc=rc)
      call ESMF_LogFlush(rc=rc)
c
c --- Release i2o regrid/redist route handle
      call ESMF_FieldBundleRedistRelease(i2oRouteHandle, rc=rc)
c
c --- Release o2i regrid/redist route handle
      call ESMF_FieldBundleRedistRelease(o2iRouteHandle, rc=rc)
c
      return
      end subroutine OICPL_Final

      end module mod_OICPL
