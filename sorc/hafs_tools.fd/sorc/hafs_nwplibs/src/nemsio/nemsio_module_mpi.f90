!----------------------------------------------------------------------------
module nemsio_module_mpi
!$$$ module document block
!
! module:   nemsio_module      API for NEMS input/output 
!
! Abstract: This module handles NEMS input/output
!
! Program history log
!    2006-11-10    Jun Wang  for gfsio
!    2008-02-29    Jun Wang
!    2010-09-06    Jun Wang change in/out attributes for densewrt
!    2011-10-31    Jun Wang remove bacio, write header from mpiio
!
! Public Variables
! Public Defined Types
!   nemsio_gfile
!     private
!        gtype:   character(nemsio_charkind*2)  NEMSIO file identifier
!        gdatatype:character(nemsio_charkind) data format
!        modelname:character(nemsio_charkind) modelname
!        version: integer(nemsio_intkind)   verion number
!        nmeta:   integer(nemsio_intkind)   number of metadata rec
!        lmeta:   integer(nemsio_intkind)   length of metadata rec 2 for model paramodels
!        nrec:    integer(nemsio_intkind)   number of data rec
!        idate(1:7):integer(nemsio_intkind) initial date (yyyy/mm/dd/hh/mm/ssn/ssd)
!        nfday:   integer(nemsio_intkind)   forecast day
!        nfhour:  integer(nemsio_intkind)   forecast hour
!        nfminute:integer(nemsio_intkind)   forecast minutes
!        nfsecondn:integer(nemsio_intkind)  numerator of forecast second fraction
!        nfsecondd:integer(nemsio_intkind)  denominator of forecast second fraction
!        dimy:    integer(nemsio_intkind)   dimension in latitude
!        dimx:    integer(nemsio_intkind)   dimension in Longitude
!        dimz:    integer(nemsio_intkind)   number of levels
!        nframe:  integer(nemsio_intkind)   dimension of halo
!        nsoil:    integer(nemsio_intkind)  number of soil layers
!        ntrac:    integer(nemsio_intkind)  number of tracers
!        jcap:    integer(nemsio_intkind)   spectral truncation
!        ncldt:   integer(nemsio_intkind)   number of cloud types
!        idsl:    integer(nemsio_intkind)   semi-lagrangian id
!        idvc:    integer(nemsio_intkind)   vertical coordinate id
!        idvm:    integer(nemsio_intkind)   mass variable id
!        idrt:    integer(nemsio_intkind)   grid identifier
!                 (idrt=4 for gaussian grid,
!                  idrt=0 for equally-spaced grid including poles,
!                  idrt=256 for equally-spaced grid excluding poles)
!        rlon_min:real(nemsio_realkind)     minimal longtitude of regional domain (global:set to 0)
!        rlon_max:real(nemsio_realkind)     maximal longtitude of regional domain (global:set to 360.)
!        rlat_min:real(nemsio_realkind)     minimal longtitude of regional domain (global:set to -90)
!        rlat_max:real(nemsio_realkind)     maximal longtitude of regional domain (global:set to 90)
!        extrameta:logical(nemsio_logickind)extra meta data flag 
!        nmetavari:integer(nemsio_intkind)  number of extra meta data integer variables
!        nmetavarr:integer(nemsio_intkind)  number of extra meta data real variables
!        nmetavarl:integer(nemsio_intkind)  number of extra meta data logical variables
!        nmetavarc:integer(nemsio_intkind)  number of extra meta data character variables
!        nmetaaryi:integer(nemsio_intkind)  number of extra meta data integer arrays
!        nmetaaryr:integer(nemsio_intkind)  number of extra meta data real arrays
!        nmetaaryl:integer(nemsio_intkind)  number of extra meta data logical arrays
!        nmetaaryc:integer(nemsio_intkind)  number of extra meta data character arrays
!
!        recname: character(nemsio_charkind),allocatable    recname(:)
!        reclevtyp: character(nemsio_charkind*2),allocatable    reclevtyp(:)
!        reclev:  integer(nemsio_intkind),allocatable       reclev(:)
!        vcoord:  real(nemsio_realkind),allocatable         vcoord(:,:,:)
!        lat:  real(nemsio_realkind),allocatable         lat(:) lat for mess point
!        lon:  real(nemsio_realkind),allocatable         lon(:) lon for mess point
!        gvlat1d: real(nemsio_realkind),allocatable         gvlat1d(:) lat for wind point
!        gvlon1d: real(nemsio_realkind),allocatable         gvlon1d(:) lon for wind point
!        Cpi:     real(nemsio_realkind),allocatable         cpi(:)
!        Ri:      real(nemsio_realkind),allocatable         ri(:)
!
!        variname:character(nemsio_charkind)  names of extra meta data integer variables
!        varrname:character(nemsio_charkind)  names of extra meta data real variables
!        varlname:character(nemsio_charkind)  names of extra meta data logical variables
!        varcname:character(nemsio_charkind)  names of extra meta data character variables
!        varival: integer(nemsio_intkind)     values of extra meta data integer variables
!        varrval: real(nemsio_realkind)       values of extra meta data integer variables
!        varlval: logical(nemsio_logickind)   values of extra meta data integer variables
!        varcval: character(nemsio_charkind)  values of extra meta data integer variables
!        aryiname:character(nemsio_charkind)  names of extra meta data integer arrays
!        aryrname:character(nemsio_charkind)  names of extra meta data real arrays
!        arylname:character(nemsio_charkind)  names of extra meta data logical arrays
!        arycname:character(nemsio_charkind)  names of extra meta data character arrays
!        aryilen: integer(nemsio_intkind)     lengths of extra meta data integer arrays
!        aryilen: integer(nemsio_intkind)     number of extra meta data integer arrays
!        aryilen: integer(nemsio_intkind)     number of extra meta data integer arrays

!!--- file handler
!        gfname:  character(255)  file name
!        gaction: character(nemsio_charkind)  read/write
!        flunit:  integer(nemsio_intkind)  unit number  
!
! Public method
!   nemsio_init
!   nemsio_finalize
!   nemsio_open
!   nemsio_writerec
!   nemsio_readirec
!   nemsio_writerecv
!   nemsio_readirecv
!   nemsio_writerecw34
!   nemsio_readirecw34
!   nemsio_writerecvw34
!   nemsio_readirecvw34
!   nemsio_close
!   nemsio_getfilehead
! Possible return code
!          0   Successful call
!         -1   Open or close I/O error
!         -2   array size
!         -3   Meta data I/O error (possible EOF)
!         -4   GETGB/PUTGB error
!         -5   Search record and set GRIB message info error
!         -6   allocate/deallocate error
!         -7   set grib table
!         -8   file meta data initialization (default:1152*576)
!         -9   NOT nemsio type file
!         -10  get/close file unit
!         -11  read/write bin data
!         -12  read/write NMM B grid lat lon
!         -13  read/write NMM sfc var
!         -15  read/write gsi 
!         -17  get var from file header
!         -20  nemsio init
!
!$$$ end module document block
!
!  use mpi
!
  implicit none
  private
  include "mpif.h"
!------------------------------------------------------------------------------
! private variables and type needed by nemsio_gfile
  integer,parameter:: nemsio_lmeta1=48,nemsio_lmeta3=40
  integer,parameter:: nemsio_intkind=4,nemsio_intkind8=8
  integer,parameter:: nemsio_realkind=4,nemsio_dblekind=8
  integer,parameter:: nemsio_charkind=16,nemsio_charkind8=8, nemsio_charkind4=4
  integer,parameter:: nemsio_logickind=4
  integer,parameter:: nemsio_maxint=2147483647
  real(nemsio_intkind),parameter     :: nemsio_intfill=-9999_nemsio_intkind
  integer(nemsio_intkind8),parameter    :: nemsio_intfill8=-9999_nemsio_intkind8
  logical(nemsio_logickind),parameter:: nemsio_logicfill=.false.
  real(nemsio_intkind),parameter     :: nemsio_kpds_intfill=-1_nemsio_intkind
  real(nemsio_realkind),parameter    :: nemsio_realfill=-9999._nemsio_realkind
  real(nemsio_dblekind),parameter    :: nemsio_dblefill=-9999._nemsio_dblekind
!
!------------------------------------------------------------------------------
!---  public types
  type,public :: nemsio_gfile
    private
    character(nemsio_charkind8) :: gtype=' '
    integer(nemsio_intkind):: version=nemsio_intfill
    character(nemsio_charkind8):: gdatatype=' '
    character(nemsio_charkind8):: modelname=' '
    integer(nemsio_intkind):: nmeta=nemsio_intfill
    integer(nemsio_intkind):: lmeta=nemsio_intfill
    integer(nemsio_intkind):: nrec=nemsio_intfill
!
    integer(nemsio_intkind):: idate(7)=nemsio_intfill
    integer(nemsio_intkind):: nfday=nemsio_intfill
    integer(nemsio_intkind):: nfhour=nemsio_intfill
    integer(nemsio_intkind):: nfminute=nemsio_intfill
    integer(nemsio_intkind):: nfsecondn=nemsio_intfill
    integer(nemsio_intkind):: nfsecondd=nemsio_intfill
!    integer(nemsio_intkind):: ifdate(7)=nemsio_intfill
!
    integer(nemsio_intkind):: dimx=nemsio_intfill
    integer(nemsio_intkind):: dimy=nemsio_intfill
    integer(nemsio_intkind):: dimz=nemsio_intfill
    integer(nemsio_intkind):: nframe=nemsio_intfill
    integer(nemsio_intkind):: nsoil=nemsio_intfill
    integer(nemsio_intkind):: ntrac=nemsio_intfill
!
    integer(nemsio_intkind) :: jcap=nemsio_intfill
    integer(nemsio_intkind) :: ncldt=nemsio_intfill
    integer(nemsio_intkind) :: idvc=nemsio_intfill
    integer(nemsio_intkind) :: idsl=nemsio_intfill
    integer(nemsio_intkind) :: idvm=nemsio_intfill
    integer(nemsio_intkind) :: idrt=nemsio_intfill
    real(nemsio_realkind) :: rlon_min=nemsio_realfill
    real(nemsio_realkind) :: rlon_max=nemsio_realfill
    real(nemsio_realkind) :: rlat_min=nemsio_realfill
    real(nemsio_realkind) :: rlat_max=nemsio_realfill
    logical(nemsio_logickind) :: extrameta=nemsio_logicfill
!
    integer(nemsio_intkind):: nmetavari=nemsio_intfill
    integer(nemsio_intkind):: nmetavarr=nemsio_intfill
    integer(nemsio_intkind):: nmetavarl=nemsio_intfill
    integer(nemsio_intkind):: nmetavarc=nemsio_intfill
    integer(nemsio_intkind):: nmetavarr8=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryi=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryr=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryl=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryc=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryr8=nemsio_intfill
!
    character(nemsio_charkind),allocatable :: recname(:)
    character(nemsio_charkind),allocatable :: reclevtyp(:)
    integer(nemsio_intkind),allocatable    :: reclev(:)
!
    real(nemsio_realkind),allocatable      :: vcoord(:,:,:)
    real(nemsio_realkind),allocatable      :: lat(:)
    real(nemsio_realkind),allocatable      :: lon(:)
    real(nemsio_realkind),allocatable      :: dx(:)
    real(nemsio_realkind),allocatable      :: dy(:)
!
    real(nemsio_realkind),allocatable      :: Cpi(:)
    real(nemsio_realkind),allocatable      :: Ri(:)
!
    character(nemsio_charkind),allocatable :: variname(:)
    integer(nemsio_intkind),allocatable    :: varival(:)
    character(nemsio_charkind),allocatable :: varrname(:)
    real(nemsio_realkind),allocatable      :: varrval(:)
    character(nemsio_charkind),allocatable :: varr8name(:)
    real(nemsio_dblekind),allocatable      :: varr8val(:)
    character(nemsio_charkind),allocatable :: varlname(:)
    logical(nemsio_logickind),allocatable  :: varlval(:)
    character(nemsio_charkind),allocatable :: varcname(:)
    character(nemsio_charkind),allocatable :: varcval(:)
!
    character(nemsio_charkind),allocatable :: aryiname(:)
    integer(nemsio_intkind),allocatable    :: aryilen(:)
    integer(nemsio_intkind),allocatable    :: aryival(:,:)
    character(nemsio_charkind),allocatable :: aryrname(:)
    integer(nemsio_intkind),allocatable    :: aryrlen(:)
    real(nemsio_realkind),allocatable      :: aryrval(:,:)
    character(nemsio_charkind),allocatable :: arylname(:)
    integer(nemsio_intkind),allocatable    :: aryllen(:)
    logical(nemsio_logickind),allocatable  :: arylval(:,:)
    character(nemsio_charkind),allocatable :: arycname(:)
    integer(nemsio_intkind),allocatable    :: aryclen(:)
    character(nemsio_charkind),allocatable :: arycval(:,:)
    character(nemsio_charkind),allocatable :: aryr8name(:)
    integer(nemsio_intkind),allocatable    :: aryr8len(:)
    real(nemsio_dblekind),allocatable      :: aryr8val(:,:)
!  
    character(255) :: gfname
    character(nemsio_charkind8) :: gaction
    integer(nemsio_intkind8)    :: tlmeta=nemsio_intfill
    integer(nemsio_intkind)    :: fieldsize=nemsio_intfill
    integer(nemsio_intkind)    :: flunit=nemsio_intfill
    integer(nemsio_intkind)    :: headvarinum=nemsio_intfill
    integer(nemsio_intkind)    :: headvarrnum=nemsio_intfill
    integer(nemsio_intkind)    :: headvarcnum=nemsio_intfill
    integer(nemsio_intkind)    :: headvarlnum=nemsio_intfill
    integer(nemsio_intkind)    :: headaryinum=nemsio_intfill
    integer(nemsio_intkind)    :: headaryrnum=nemsio_intfill
    integer(nemsio_intkind)    :: headarycnum=nemsio_intfill
    character(nemsio_charkind),allocatable :: headvarcname(:)
    character(nemsio_charkind),allocatable :: headvariname(:)
    character(nemsio_charkind),allocatable :: headvarrname(:)
    character(nemsio_charkind),allocatable :: headvarlname(:)
    character(nemsio_charkind),allocatable :: headaryiname(:)
    character(nemsio_charkind),allocatable :: headaryrname(:)
    character(nemsio_charkind),allocatable :: headarycname(:)
    integer(nemsio_intkind),allocatable    :: headvarival(:)
    real(nemsio_realkind),allocatable      :: headvarrval(:)
    character(nemsio_charkind),allocatable :: headvarcval(:)
    logical(nemsio_logickind),allocatable  :: headvarlval(:)
    integer(nemsio_intkind),allocatable    :: headaryival(:,:)
    real(nemsio_realkind),allocatable      :: headaryrval(:,:)
    character(nemsio_charkind),allocatable :: headarycval(:,:)
    character,allocatable      :: cbuf(:)
    integer(nemsio_intkind):: mbuf=0,nlen,nnum,mnum
    integer(nemsio_intkind8)    :: tlmetalat=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetalon=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetadx=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetady=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetavarival=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetaaryival=nemsio_intfill
    character(16)               :: file_endian=''
    logical                     :: do_byteswap=.false.
!-- for MPI I/O
    integer(nemsio_intkind)     :: mpi_comm=nemsio_intfill
    integer(nemsio_intkind)     :: lead_task=nemsio_intfill
    integer(nemsio_intkind)     :: mype=nemsio_intfill
    integer(nemsio_intkind)     :: npes=nemsio_intfill
    integer(nemsio_intkind)     :: fh=nemsio_intfill
    real(nemsio_realkind)       :: fieldsize_real4=nemsio_realfill
    real(nemsio_dblekind)       :: fieldsize_real8=nemsio_realfill
  end type nemsio_gfile
!
!------------------------------------------------------------------------------
!--- private types
!
  type :: nemsio_meta1
    sequence
     character(nemsio_charkind8) :: gtype
     character(nemsio_charkind8) :: modelname
     character(nemsio_charkind8) :: gdatatype
     integer(nemsio_intkind) :: version,nmeta,lmeta
     integer(nemsio_intkind) :: reserve(3)
  end type nemsio_meta1
!
  type :: nemsio_meta2
    sequence
    integer(nemsio_intkind) :: nrec 
    integer(nemsio_intkind) :: idate(1:7),nfday,nfhour,nfminute,nfsecondn, &
                               nfsecondd,dimx,dimy,dimz,nframe,nsoil,ntrac,&
                               jcap,ncldt,idvc,idsl,idvm,idrt
    real(nemsio_realkind)   :: rlon_min,rlon_max,rlat_min,rlat_max 
    logical(nemsio_logickind) :: extrameta
  end type nemsio_meta2
!
  type :: nemsio_meta3
    integer(nemsio_intkind) :: nmetavari,nmetavarr,nmetavarl,nmetavarc, &
                               nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc, &
                               nmetavarr8,nmetaaryr8
  end type nemsio_meta3
!
  character(16) :: machine_endian='big_endian'
!
!*** for mpi
  integer(nemsio_intkind)   :: itypemeta1,itypemeta2
!
  type  :: nemsio_grbmeta
    integer(nemsio_intkind)   :: jf=nemsio_intfill
    integer(nemsio_intkind)   :: j=nemsio_kpds_intfill
    logical*1,allocatable     :: lbms(:)
    integer(nemsio_intkind)   :: jpds(200)=nemsio_kpds_intfill
    integer(nemsio_intkind)   :: jgds(200)=nemsio_kpds_intfill
  end type nemsio_grbmeta
!
!----- interface
  interface nemsio_getheadvar
    module procedure nemsio_getfheadvari
    module procedure nemsio_getfheadvarr
    module procedure nemsio_getfheadvarl
    module procedure nemsio_getfheadvarc
    module procedure nemsio_getfheadvarr8
    module procedure nemsio_getfheadaryi
    module procedure nemsio_getfheadaryr
    module procedure nemsio_getfheadaryr8
    module procedure nemsio_getfheadaryl
    module procedure nemsio_getfheadaryc
  end interface nemsio_getheadvar
!
  interface nemsio_denseread
    module procedure nemsio_denseread4
    module procedure nemsio_denseread8
  end interface nemsio_denseread
!
  interface nemsio_densewrite
    module procedure nemsio_densewrite4
    module procedure nemsio_densewrite8
  end interface nemsio_densewrite
!
!--- file unit for putgb/getgb ----
  integer(nemsio_intkind),save   :: fileunit(600:699)=0
!------------------------------------------------------------------------------
!public mehtods
  public nemsio_intkind,nemsio_intkind8,nemsio_realkind,nemsio_dblekind
  public nemsio_charkind,nemsio_charkind8,nemsio_logickind
  public nemsio_init,nemsio_finalize,nemsio_open,nemsio_close
  public nemsio_denseread,nemsio_densewrite
  public nemsio_getfilehead,nemsio_getheadvar,nemsio_getrechead
!
contains
!-------------------------------------------------------------------------------
  subroutine nemsio_init(iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! initialization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    integer(nemsio_intkind),optional,intent(out):: iret
!-- local vars
    integer :: meta1_type(2),meta1_block(2),meta1_disp(2)
    integer :: meta2_type(3),meta2_block(3),meta2_disp(3)
    integer :: ios
!
    if (present(iret))iret=1
!------------------------------------------------------------
! MPI set meta data type
!------------------------------------------------------------
! 1. meta1
    meta1_type(1)=MPI_CHARACTER
    meta1_type(2)=MPI_INTEGER
    meta1_block(1)=24
    meta1_block(2)=6
    meta1_disp(1)=0
    meta1_disp(2)=meta1_disp(1)+meta1_block(1)*1
    call mpi_type_struct(2,meta1_block,meta1_disp,meta1_type,            &
           itypemeta1,ios)
    call mpi_type_commit(itypemeta1,ios)
    if ( ios.ne.0 ) then
      return
    endif
!
! 2. meta2
    meta2_type(1)=MPI_INTEGER
    meta2_type(2)=MPI_REAL
    meta2_type(3)=MPI_LOGICAL
    meta2_block(1)=25
    meta2_block(2)=4
    meta2_block(3)=1
    meta2_disp(1)=0
    meta2_disp(2)=meta2_block(1)*4+meta2_disp(1)
    meta2_disp(3)=meta2_block(2)*4+meta2_disp(2)
    call mpi_type_struct(3,meta2_block,meta2_disp,meta2_type,            &
         itypemeta2,ios)
    call mpi_type_commit(itypemeta2,ios)
    if ( ios.ne.0 ) then
      return
    endif
!
!------------------------------------------------------------
! check machine endian
!------------------------------------------------------------
    call chk_endianc(machine_endian)
    if(trim(machine_endian)=='mixed_endian') then
      call nemsio_stop('You are in mixed endian computer,stop!!!')
    endif
!
    if(present(iret)) iret=0
!
  end subroutine nemsio_init
!------------------------------------------------------------------------------
  subroutine nemsio_finalize()
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: finalization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
!--
  end subroutine nemsio_finalize
!------------------------------------------------------------------------------
  subroutine nemsio_open(gfile,gfname,gaction,mpi_comm,                     &
             iret,gdatatype,version,mype,npes,                              &
      nmeta,lmeta,modelname,nrec,idate,nfday,nfhour,nfminute,nfsecondn,     &
      nfsecondd, &
      dimx,dimy,dimz,nframe,nsoil,ntrac,jcap,ncldt,idvc,idsl,idvm,idrt,     &
      rlon_min,rlon_max,rlat_min,rlat_max,extrameta,           &
      nmetavari,nmetavarr,nmetavarl,nmetavarc,                              &
      nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,                              &
      nmetavarr8,nmetaaryr8,                                                &
      recname,reclevtyp,reclev,vcoord,lat,lon,dx,dy,cpi,ri,                 &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval,  &
      varr8name,varr8val,                                                   &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,                    &
      arylname,aryllen,arylval,arycname,aryclen,arycval,                    &
      aryr8name,aryr8len,aryr8val  )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: open nemsio file, and read/write the meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)    :: gfile
    character*(*),intent(in)            :: gfname
    character*(*),intent(in)            :: gaction
    integer,intent(in)                  :: mpi_comm
!-------------------------------------------------------------------------------
! optional variables
!-------------------------------------------------------------------------------
    integer(nemsio_intkind),optional,intent(out) :: iret
    character*(*),optional,intent(in)            :: gdatatype,modelname
    integer(nemsio_intkind),optional,intent(in)  :: version,nmeta,lmeta,nrec
    integer,optional,intent(in)                  :: mype,npes
    integer(nemsio_intkind),optional,intent(in)  :: idate(7),nfday,nfhour,    &
            nfminute, nfsecondn,nfsecondd
    integer(nemsio_logickind),optional,intent(in):: dimx,dimy,dimz,nframe,    &
            nsoil,ntrac
    integer(nemsio_logickind),optional,intent(in):: jcap,ncldt,idvc,idsl,     &
            idvm,idrt
    real(nemsio_realkind),optional,intent(in)    :: rlat_min,rlat_max,   &
             rlon_min,rlon_max
    logical(nemsio_logickind),optional,intent(in):: extrameta
    integer(nemsio_intkind),optional,intent(in)  :: nmetavari,nmetavarr, &   
            nmetavarl,nmetavarc,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc, &
            nmetavarr8,nmetaaryr8
!
    character*(*),optional,intent(in)            :: recname(:),reclevtyp(:)
    integer(nemsio_intkind),optional,intent(in)  :: reclev(:)
    real(nemsio_realkind),optional,intent(in)    :: vcoord(:,:,:)
    real(nemsio_realkind),optional,intent(in)    :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(in)    :: dx(:),dy(:)
    real(nemsio_realkind),optional,intent(in)    :: Cpi(:),Ri(:)
!
    character*(*),optional,intent(in)            :: variname(:),varrname(:),&
          varlname(:),varcname(:),varr8name(:),aryiname(:),aryrname(:),     &
          arylname(:),arycname(:),aryr8name(:)
    integer(nemsio_intkind),optional,intent(in)  :: aryilen(:),aryrlen(:),  &
          aryllen(:),aryclen(:),aryr8len(:)
    integer(nemsio_intkind),optional,intent(in)  :: varival(:),aryival(:,:)
    real(nemsio_realkind),optional,intent(in)    :: varrval(:),aryrval(:,:)
    real(nemsio_dblekind),optional,intent(in)    :: varr8val(:),aryr8val(:,:)
    logical(nemsio_logickind),optional,intent(in):: varlval(:),arylval(:,:)
    character(*),optional,intent(in)             :: varcval(:),arycval(:,:)
!
    integer :: ios
!------------------------------------------------------------
!### for MPI IO, just need this part for read header ###### 
!    assign a unit number 
!------------------------------------------------------------
    if (present(iret)) iret=-1
!
    gfile%gfname=gfname
    gfile%gaction=gaction
    gfile%mpi_comm=mpi_comm
    gfile%lead_task=0
!
    if(present(mype)) then
      gfile%mype=mype
    else
      call mpi_comm_rank(mpi_comm,gfile%mype,ios)
      if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
      endif
    endif
    if(present(npes)) then
      gfile%npes=npes
    else
      call mpi_comm_size(mpi_comm,gfile%npes,ios)
      if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
      endif
    endif
!
!------------------------------------------------------------
! open and read meta data for READ
!------------------------------------------------------------
    if ( equal_str_nocase(trim(gaction),"read").or.          &
      equal_str_nocase(trim(gaction),"rdwr") )then
!
!-read 2D field using MPI I/O
!
       if(equal_str_nocase(trim(gaction),"read")) then
        call mpi_file_open(mpi_comm,gfname,MPI_MODE_RDONLY,MPI_INFO_NULL,gfile%fh,ios)
       else if(equal_str_nocase(trim(gaction),"rdwt")) then
        call mpi_file_open(mpi_comm,gfname,MPI_MODE_RDWR,MPI_INFO_NULL,gfile%fh,ios)
       endif
       if ( ios.ne.0) then
        if ( present(iret))  then
          return
        else
          call nemsio_stop
        endif
       endif
!
!-read  meta data for gfile, use non-mpi read for header
!
       call nemsio_rcreate(gfile,ios)
!       write(0,*)'after nemsio_rcreate'
       if ( ios.ne.0) then
        if ( present(iret))  then
          iret=ios
          return
        else
          call nemsio_stop
        endif
       endif
!------------------------------------------------------------
! open and write meta data for WRITE
!------------------------------------------------------------
    elseif (equal_str_nocase(trim(gaction),"write")) then
!
!-write 2D field using MPI I/O
!
      call mpi_file_open(mpi_comm,gfname,MPI_MODE_CREATE+MPI_MODE_WRONLY,    &
           MPI_INFO_NULL,gfile%fh,ios)
!      print *,'mype=',gfile%mype,'after mpi_file_open,ios=',ios
      if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
      endif
!
!-write  meta data for gfile, use non-mpi write for header
!
      call nemsio_wcreate(gfile,ios,gdatatype=gdatatype, &
        version=version, nmeta=nmeta,lmeta=lmeta,modelname=modelname,  &
        nrec=nrec,idate=idate,nfday=nfday,nfhour=nfhour,nfminute=nfminute,&
        nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
        dimx=dimx,dimy=dimy,dimz=dimz,nframe=nframe,nsoil=nsoil,   &
        ntrac=ntrac,jcap=jcap,ncldt=ncldt,idvc=idvc,idsl=idsl,    &
        idvm=idvm,idrt=idrt,                          &
        rlon_min=rlon_min,rlon_max=rlon_max,rlat_min=rlat_min, &
        rlat_max=rlat_max,extrameta=extrameta, &
        nmetavari=nmetavari,nmetavarr=nmetavarr,nmetavarr8=nmetavarr8,&
        nmetavarl=nmetavarl,nmetavarc=nmetavarc, &
        nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,nmetaaryr8=nmetaaryr8,&
        nmetaaryl=nmetaaryl,nmetaaryc=nmetaaryc, &
        recname=recname,reclevtyp=reclevtyp,    &
        reclev=reclev,vcoord=vcoord,lat=lat,lon=lon,dx=dx,dy=dy,    &
        cpi=cpi,ri=ri,variname=variname,varival=varival,varrname=varrname,&
        varrval=varrval,varlname=varlname,varlval=varlval, &
        varcname=varcname,varcval=varcval, &
        varr8name=varr8name,varr8val=varr8val, &
        aryiname=aryiname,aryilen=aryilen,aryival=aryival, &
        aryrname=aryrname,aryrlen=aryrlen,aryrval=aryrval, &
        aryr8name=aryr8name,aryr8len=aryr8len,aryr8val=aryr8val, &
        arylname=arylname,aryllen=aryllen,arylval=arylval, &
        arycname=arycname,aryclen=aryclen,arycval=arycval  )
      if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
     endif
!
!------------------------------------------------------------
! if gaction is wrong
!------------------------------------------------------------
    else
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! set default header
!------------------------------------------------------------
    if(.not.allocated(gfile%headvariname).or. &
       .not.allocated(gfile%headvarrname).or. &
       .not.allocated(gfile%headvarcname).or. &
       .not.allocated(gfile%headvarlname).or. &
       .not.allocated(gfile%headaryiname).or. &
       .not.allocated(gfile%headaryrname) ) then
      call nemsio_setfhead(gfile,ios)
      if ( present(iret)) iret=ios
      if ( ios.ne.0) then
        if (present(iret)) return
        call nemsio_stop
      endif
    endif
!
    iret=0
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_open
!-------------------------------------------------------------------------------
  subroutine nemsio_close(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: close gfile including closing the file, returning unit number, 
!           setting file meta data empty
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind)      :: ios
!------------------------------------------------------------
! close the file
!------------------------------------------------------------
    if ( present(iret) ) iret=-1
    call mpi_file_close(gfile%fh,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! empty gfile meta data
!------------------------------------------------------------
    call nemsio_axmeta(gfile,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  end subroutine nemsio_close
!------------------------------------------------------------------------------
  subroutine nemsio_rcreate(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
!local variables
    integer(nemsio_intkind)      :: ios,nmeta,tlmeta4
    integer(nemsio_intkind)     :: iread
    integer (kind=mpi_offset_kind) ::idisp
    integer :: status(mpi_status_size)
    type(nemsio_meta1)           :: meta1
    type(nemsio_meta2)           :: meta2
    type(nemsio_meta3)           :: meta3
    integer(nemsio_intkind) :: i,nummeta
    character(nemsio_charkind8),allocatable :: char8var(:)
!------------------------------------------------------------
! open gfile for read header
!------------------------------------------------------------
    iret=-3
!------------------------------------------------------------
! read first meta data record
!------------------------------------------------------------
    idisp=4
    call mpi_file_read_at(gfile%fh,idisp,meta1,1,itypemeta1,status,ios)
!    print *,'1 PE',gfile%mype,'ios=',ios,' gtype=',meta1%gtype, meta1%modelname, &
!         meta1%gdatatype,meta1%version,meta1%nmeta,meta1%lmeta
!
    gfile%do_byteswap=.false.
! check byteswap
    if(meta1%lmeta/=120) then
      gfile%do_byteswap=.true.
      if(gfile%do_byteswap) call byteswap(meta1%version,nemsio_intkind,6)
    endif
    gfile%gtype=meta1%gtype
    gfile%gdatatype=meta1%gdatatype
    gfile%modelname=meta1%modelname
    gfile%version=meta1%version
    gfile%nmeta=meta1%nmeta
    gfile%lmeta=meta1%lmeta
    gfile%tlmeta=nemsio_lmeta1+8
    if ( trim(gfile%gdatatype(1:3)).ne."bin"               &
         .and. trim(gfile%gdatatype(1:4)).ne."grib" ) then
      gfile%gdatatype="grib"
    endif
!     print *,'aft meta1,gtype=',trim(gfile%gtype),'version=',gfile%version, &
!      'nmeta=',gfile%nmeta,'gfile%lmeta=',gfile%lmeta,'gfile%gdatatype=',   &
!      gfile%gdatatype,'modelname=',gfile%modelname
    if ( gfile%gtype(1:6) .ne. 'NEMSIO' ) then
      iret=-9
      return
    endif
!------------------------------------------------------------
! read second meta data record
!------------------------------------------------------------
    idisp=gfile%tlmeta+4
    call mpi_file_read_at(gfile%fh,idisp,meta2,1,itypemeta2,status,ios)
    if(gfile%do_byteswap) then
      call byteswap(meta2%nrec,nemsio_intkind,25)
      call byteswap(meta2%rlon_min,nemsio_realkind,4)
      call byteswap(meta2%extrameta,nemsio_logickind,1)
    endif
    gfile%tlmeta=gfile%tlmeta+nemsio_intkind*25+nemsio_realkind*4+nemsio_logickind+8
!
    gfile%nrec=meta2%nrec
    gfile%idate(1:7)=meta2%idate(1:7)
    gfile%nfday=meta2%nfday
    gfile%nfhour=meta2%nfhour
    gfile%nfminute=meta2%nfminute
    gfile%nfsecondn=meta2%nfsecondn
    gfile%nfsecondd=meta2%nfsecondd
    gfile%dimx=meta2%dimx
    gfile%dimy=meta2%dimy
    gfile%dimz=meta2%dimz
    gfile%nframe=meta2%nframe
    gfile%nsoil=meta2%nsoil
    gfile%ntrac=meta2%ntrac
    gfile%jcap=meta2%jcap
    gfile%ncldt=meta2%ncldt
    gfile%idvc=meta2%idvc
    gfile%idsl=meta2%idsl
    gfile%idvm=meta2%idvm
    gfile%idrt=meta2%idrt
    gfile%rlon_min=meta2%rlon_min
    gfile%rlon_max=meta2%rlon_max
    gfile%rlat_min=meta2%rlat_min
    gfile%rlat_max=meta2%rlat_max
    gfile%extrameta=meta2%extrameta
    gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)
!    print *,'meta2,nrec=',gfile%nrec,gfile%idate(1:7),gfile%nfday,  &
!      gfile%nfhour,gfile%nfminute,gfile%nfsecondn,gfile%nfsecondd,  &
!      gfile%dimx,gfile%dimy,gfile%dimz,gfile%nframe,gfile%nsoil,    &
!      gfile%ntrac,gfile%jcap,gfile%ncldt,gfile%idvc,gfile%idsl,     &
!      gfile%idvm,gfile%idrt,gfile%rlon_min,gfile%rlon_max,          &
!      gfile%rlat_min,gfile%rlat_max,gfile%extrameta

    nummeta=gfile%nmeta
!------------------------------------------------------------
! set up gfile required meata arrays
!------------------------------------------------------------
    call nemsio_almeta(gfile,ios)
    if ( ios .ne. 0 ) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! read gfile meta data array (meta rec 3:13)
!------------------------------------------------------------
!meta3:recname
    if(nummeta>2) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%recname)*size(gfile%recname)
      call mpi_file_read_at(gfile%fh,idisp,gfile%recname,iread,MPI_CHARACTER,status,ios)
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+iread+8
    endif
!meta4:reclevtyp
    if(nummeta>3) then
      idisp=gfile%tlmeta+4
      call mpi_file_read_at(gfile%fh,idisp,gfile%reclevtyp,iread,MPI_CHARACTER,status,ios)
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+iread+8
    endif
!meta5:reclev
    if(nummeta>4) then
      idisp=gfile%tlmeta+4
      iread=size(gfile%reclev)
      call mpi_file_read_at(gfile%fh,idisp,gfile%reclev,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%reclev,nemsio_intkind,size(gfile%reclev))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%reclev)*iread+8
    endif
!meta6:vcoord
    if(nummeta>5) then
      idisp=gfile%tlmeta+4
      iread=size(gfile%vcoord)
      call mpi_file_read_at(gfile%fh,idisp,gfile%vcoord,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%vcoord,nemsio_realkind,size(gfile%vcoord))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%vcoord)*iread+8
    endif
!meta7:lat
    if(nummeta>6) then
      idisp=gfile%tlmeta+4
      iread=size(gfile%lat)
      call mpi_file_read_at(gfile%fh,idisp,gfile%lat,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%lat,nemsio_realkind,size(gfile%lat))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%lat)*iread+8
    endif
!meta8:lon
    if(nummeta>7) then
      idisp=gfile%tlmeta+4
      call mpi_file_read_at(gfile%fh,idisp,gfile%lon,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%lon,nemsio_realkind,size(gfile%lon))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%lon)*iread+8
    endif
!meta9:dx
    if(nummeta>8) then
      idisp=gfile%tlmeta+4
      call mpi_file_read_at(gfile%fh,idisp,gfile%dx,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%dx,nemsio_realkind,size(gfile%dx))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%dx)*iread+8
    endif
!meta10:dy
    if(nummeta>9) then
      idisp=gfile%tlmeta+4
      call mpi_file_read_at(gfile%fh,idisp,gfile%dy,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%dy,nemsio_realkind,size(gfile%dy))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%dy)*iread+8
    endif
!meta11:cpi
    if(nummeta>10) then
      idisp=gfile%tlmeta+4
      iread=size(gfile%cpi)
      call mpi_file_read_at(gfile%fh,idisp,gfile%cpi,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%cpi,nemsio_realkind,size(gfile%cpi))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%cpi)*iread+8
    endif
!Ri
    if(nummeta>11) then
      idisp=gfile%tlmeta+4
      call mpi_file_read_at(gfile%fh,idisp,gfile%ri,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap) call byteswap(gfile%ri,nemsio_realkind,size(gfile%ri))
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+kind(gfile%ri)*iread+8
    endif
!
    extrameta_if: if(gfile%extrameta) then
!------------------------------------------------------------
! read out extra meta data
!------------------------------------------------------------
    idisp=gfile%tlmeta
    call mpi_file_read_at(gfile%fh,idisp,iread,1,MPI_INTEGER,status,ios)
    if(gfile%do_byteswap) call byteswap(iread,nemsio_intkind,1)
    idisp=gfile%tlmeta+4
    if(iread/4==10) then
      call mpi_file_read_at(gfile%fh,idisp,meta3,10,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap) call byteswap(meta3,nemsio_intkind,10)
      gfile%nmetavarr8=meta3%nmetavarr8
      gfile%nmetaaryr8=meta3%nmetaaryr8
      gfile%tlmeta=gfile%tlmeta+nemsio_lmeta3+8
    elseif(iread/4==8) then
      call mpi_file_read_at(gfile%fh,idisp,meta3,8,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap) call byteswap(meta3,nemsio_intkind,8)
      gfile%tlmeta=gfile%tlmeta+nemsio_lmeta3
    endif
    gfile%nmetavari=meta3%nmetavari
    gfile%nmetavarr=meta3%nmetavarr
    gfile%nmetavarl=meta3%nmetavarl
    gfile%nmetavarc=meta3%nmetavarc
    gfile%nmetaaryi=meta3%nmetaaryi
    gfile%nmetaaryr=meta3%nmetaaryr
    gfile%nmetaaryl=meta3%nmetaaryl
    gfile%nmetaaryc=meta3%nmetaaryc
!
!      print *,'after meta3,nmetavari=',gfile%nmetavari,'nvarr=',gfile%nmetavarr, &
!     'varl=',gfile%nmetavarl,'varc=',gfile%nmetavarc, gfile%nmetavarr8,'naryi=',&
!     gfile%nmetaaryi,gfile%nmetaaryr,gfile%nmetaaryl,gfile%nmetaaryc,gfile%nmetaaryr8,&
!     'iread=',iread
   
    call nemsio_alextrameta(gfile,ios)
    if ( ios .ne. 0 ) then
      iret=ios
      return
    endif

!meta var integer
    if (gfile%nmetavari.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%variname)*gfile%nmetavari
      call mpi_file_read_at(gfile%fh,idisp,gfile%variname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetavari
      call mpi_file_read_at(gfile%fh,idisp,gfile%varival,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
        call byteswap(gfile%varival,nemsio_intkind,iread)
      gfile%tlmetavarival=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
    endif
!
!meta var real
    if (gfile%nmetavarr.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%varrname)*gfile%nmetavarr
      call mpi_file_read_at(gfile%fh,idisp,gfile%varrname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetavarr
      call mpi_file_read_at(gfile%fh,idisp,gfile%varrval,iread,MPI_REAL,status,ios)
      if(gfile%do_byteswap)    &
        call byteswap(gfile%varrval,nemsio_realkind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_realkind+8
    endif
!
!meta var logical
    if (gfile%nmetavarl.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%varlname)*gfile%nmetavarl
      call mpi_file_read_at(gfile%fh,idisp,gfile%varlname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetavarl
      call mpi_file_read_at(gfile%fh,idisp,gfile%varlval,iread,MPI_LOGICAL,status,ios)
      if(gfile%do_byteswap)    &
        call byteswap(gfile%varlval,nemsio_logickind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_logickind+8
    endif
!
!meta var character
    if (gfile%nmetavarc.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%varcname)*gfile%nmetavarc
      call mpi_file_read_at(gfile%fh,idisp,gfile%varcname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=len(gfile%varcname)*gfile%nmetavarc
      call mpi_file_read_at(gfile%fh,idisp,gfile%varcval,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
    endif
!meta var real 8
    if (gfile%nmetavarr8.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%varr8name)*gfile%nmetavarr8
      call mpi_file_read_at(gfile%fh,idisp,gfile%varr8name,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetavarr8
      call mpi_file_read_at(gfile%fh,idisp,gfile%varr8val,iread,MPI_REAL8,status,ios)
      if(gfile%do_byteswap)    &
          call byteswap(gfile%varr8val,nemsio_dblekind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_dblekind+8
    endif
!
!meta arr integer
    if (gfile%nmetaaryi.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%aryiname)*gfile%nmetaaryi
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryiname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetaaryi
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryilen,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
          call byteswap(gfile%aryilen,nemsio_intkind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
!
      allocate(gfile%aryival(maxval(gfile%aryilen),gfile%nmetaaryi))
      do i=1,gfile%nmetaaryi
        idisp=gfile%tlmeta+4
        iread=gfile%aryilen(i)
        call mpi_file_read_at(gfile%fh,idisp,gfile%aryival(1:iread,i),iread,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryival(:,i),nemsio_intkind,iread)
        gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
      enddo
    endif
!meta arr real
    if (gfile%nmetaaryr.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%aryrname)*gfile%nmetaaryr
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryrname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetaaryr
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryrlen,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
          call byteswap(gfile%aryrlen,nemsio_intkind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
!
      allocate(gfile%aryrval(maxval(gfile%aryrlen),gfile%nmetaaryr))
      do i=1,gfile%nmetaaryr
        idisp=gfile%tlmeta+4
        iread=gfile%aryrlen(i)
        call mpi_file_read_at(gfile%fh,idisp,gfile%aryrval(1:iread,i),iread,MPI_REAL,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryrval(1:iread,i),nemsio_realkind,iread)
        gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
      enddo
    endif
!meta arr logical
    if (gfile%nmetaaryl.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%arylname)*gfile%nmetaaryl
      call mpi_file_read_at(gfile%fh,idisp,gfile%arylname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetaaryl
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryllen,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
          call byteswap(gfile%aryllen,nemsio_intkind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
!
      allocate(gfile%arylval(maxval(gfile%aryllen),gfile%nmetaaryl))
      do i=1,gfile%nmetaaryl
        idisp=gfile%tlmeta+4
        iread=gfile%aryllen(i)
        call mpi_file_read_at(gfile%fh,idisp,gfile%arylval(1:iread,i),iread,MPI_LOGICAL,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%arylval(1:iread,i),nemsio_logickind,iread)
        gfile%tlmeta=gfile%tlmeta+iread*nemsio_logickind+8
      enddo
    endif
!meta arr char
    if (gfile%nmetaaryc.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%arycname)*gfile%nmetaaryc
      call mpi_file_read_at(gfile%fh,idisp,gfile%arycname,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetaaryc
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryclen,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
          call byteswap(gfile%aryclen,nemsio_intkind,gfile%nmetaaryc)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
!
      allocate(gfile%arycval(maxval(gfile%aryclen),gfile%nmetaaryc))
      do i=1,gfile%nmetaaryc
        idisp=gfile%tlmeta+4
        iread=gfile%aryclen(i)*len(gfile%arycval)
        call mpi_file_read_at(gfile%fh,idisp,gfile%arycval(1:iread,i),iread,MPI_CHARACTER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iread+8
      enddo
    endif
!meta arr real8
    if (gfile%nmetaaryr8.gt.0) then
      idisp=gfile%tlmeta+4
      iread=len(gfile%aryr8name)*gfile%nmetaaryr8
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryr8name,iread,MPI_CHARACTER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iread+8
!
      idisp=gfile%tlmeta+4
      iread=gfile%nmetaaryr8
      call mpi_file_read_at(gfile%fh,idisp,gfile%aryr8len,iread,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
          call byteswap(gfile%aryr8len,nemsio_intkind,iread)
      gfile%tlmeta=gfile%tlmeta+iread*nemsio_intkind+8
!
      allocate(gfile%aryr8val(maxval(gfile%aryr8len),gfile%nmetaaryr8))
      do i=1,gfile%nmetaaryr8
        idisp=gfile%tlmeta+4
        iread=gfile%aryr8len(i)
        call mpi_file_read_at(gfile%fh,idisp,gfile%aryr8val(1:iread,i),iread,MPI_REAL,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryr8val(1:iread,i),nemsio_dblekind,iread)
        gfile%tlmeta=gfile%tlmeta+iread*nemsio_dblekind+8
      enddo
    endif
!
!end if extrameta
    endif extrameta_if
!
    call MPI_Barrier(gfile%mpi_comm, ios)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
   iret=0
  end subroutine nemsio_rcreate
!------------------------------------------------------------------------------
  subroutine nemsio_wcreate(gfile,iret,gdatatype,version,  &
      nmeta,lmeta,modelname,nrec,idate,nfday,nfhour,nfminute,nfsecondn,     &
      nfsecondd, &
      dimx,dimy,dimz,nframe,nsoil,ntrac,jcap,ncldt,idvc,idsl,idvm,idrt,     &
      rlon_min,rlon_max,rlat_min,rlat_max,extrameta,                        &
      nmetavari,nmetavarr,nmetavarl,nmetavarc,nmetavarr8,                   &
      nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,nmetaaryr8,                   &
      recname,reclevtyp,reclev,vcoord,lat,lon,dx,dy,cpi,ri,                 &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval,  &
      varr8name,varr8val,                                                   &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,                    &
      arylname,aryllen,arylval,arycname,aryclen,arycval,                    &
      aryr8name,aryr8len,aryr8val )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)             :: gfile
    integer(nemsio_intkind),intent(out)          :: iret
!optional variables
    character*(*),optional,intent(in)            :: gdatatype,modelname
    integer(nemsio_intkind),optional,intent(in)  :: version,nmeta,lmeta,nrec
    integer(nemsio_intkind),optional,intent(in)  :: idate(7),nfday,nfhour,  &
            nfminute,nfsecondn,nfsecondd
    integer(nemsio_logickind),optional,intent(in):: dimx,dimy,dimz,nframe,    &
            nsoil,ntrac
    integer(nemsio_logickind),optional,intent(in):: jcap,ncldt,idvc,idsl,     &
            idvm,idrt
    real(nemsio_realkind),optional,intent(in)    :: rlat_min,rlat_max,   &
             rlon_min,rlon_max
    logical(nemsio_logickind),optional,intent(in):: extrameta
    integer(nemsio_intkind),optional,intent(in)  :: nmetavari,nmetavarr, &
            nmetavarl,nmetavarc,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc, &
            nmetavarr8,nmetaaryr8
!
    character*(*),optional,intent(in)            :: recname(:),reclevtyp(:)
    integer(nemsio_intkind),optional,intent(in)  :: reclev(:)
    real(nemsio_realkind),optional,intent(in)    :: vcoord(:,:,:)
    real(nemsio_realkind),optional,intent(in)    :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(in)    :: dx(:),dy(:)
    real(nemsio_realkind),optional,intent(in)    :: Cpi(:),Ri(:)
!
    character*(*),optional,intent(in)            :: variname(:),varrname(:),&
          varlname(:),varcname(:),varr8name(:),aryiname(:),aryrname(:),     &
          arylname(:),arycname(:),aryr8name(:)
    integer(nemsio_intkind),optional,intent(in)  :: aryilen(:),aryrlen(:),  &
          aryllen(:),aryclen(:),aryr8len(:)
    integer(nemsio_intkind),optional,intent(in)  :: varival(:),aryival(:,:)
    real(nemsio_realkind),optional,intent(in)    :: varrval(:),aryrval(:,:)
    real(nemsio_dblekind),optional,intent(in)    :: varr8val(:),aryr8val(:,:)
    logical(nemsio_logickind),optional,intent(in):: varlval(:),arylval(:,:)
    character(*),optional,intent(in)             :: varcval(:),arycval(:,:)
!
!---  local variables
!
    real(nemsio_realkind) :: radi
    integer(nemsio_intkind) :: iwrite,nwrite
    type(nemsio_meta1)      :: meta1
    type(nemsio_meta2)      :: meta2
    type(nemsio_meta3)      :: meta3
    integer(nemsio_intkind) :: i,n,ios,nummeta
    integer :: status(MPI_STATUS_SIZE) 
    integer (kind=mpi_offset_kind) :: idisp
    logical :: linit
!------------------------------------------------------------
! set gfile meta data to operational model (default) if it's empty
!------------------------------------------------------------
    iret=-3
    gfile%gtype="NEMSIO"
    gfile%do_byteswap=.false.
    if(present(gdatatype)) then
      if ( trim(gdatatype).ne.'grib'.and.gdatatype(1:3).ne.'bin'.and. &
           trim(gdatatype).ne.'') return
      gfile%gdatatype=gdatatype
      if(trim(gdatatype)=='') gfile%gdatatype='grib'
      if(index(gfile%gdatatype,'_be')>0) then
        gfile%file_endian='big_endian'
      elseif(index(gfile%gdatatype,'_le')>0) then
        gfile%file_endian='little_endian'
      else
        gfile%file_endian=machine_endian
      endif
      if(trim(machine_endian)/=trim(gfile%file_endian)) gfile%do_byteswap=.true.
    elseif(trim(gfile%gdatatype).eq.'') then
      gfile%gdatatype='grib'
    endif
!    print *,'in wcreate,mype=',gfile%mype,'file_endian=',gfile%file_endian, &
!      'machine_endian=',machine_endian

    if(present(modelname)) then 
      gfile%modelname=modelname
    else
      gfile%modelname="GFS"
    endif
!    print *,'NEMSIO file,datatype,model is ',gfile%gtype, &
!        gfile%gdatatype,gfile%modelname,idate(1:7),'machine_endian=', &
!        machine_endian,'gfile%file_endian=',gfile%file_endian,'gfile%do_byteswap=',gfile%do_byteswap
    if(present(version)) gfile%version=version
    if(present(dimx)) gfile%dimx=dimx
    if(present(dimy)) gfile%dimy=dimy
    if(present(dimz)) gfile%dimz=dimz
    if(present(nrec)) gfile%nrec=nrec
    if(present(nmeta)) gfile%nmeta=nmeta
    if(gfile%nmeta==nemsio_intfill) gfile%nmeta=12
    if(present(lmeta)) gfile%lmeta=lmeta
    if(gfile%lmeta==nemsio_intfill)   &
      gfile%lmeta=25*nemsio_intkind+4*nemsio_realkind+nemsio_logickind
    if(present(nsoil)) gfile%nsoil=nsoil
    if(gfile%nsoil.eq.nemsio_intfill) gfile%nsoil=4
    if(present(nframe)) gfile%nframe=nframe
    if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=0
    if(trim(gfile%modelname)=='GFS')gfile%nframe=0
    if(present(idate)) gfile%idate=idate
    if ( gfile%idate(1) .lt. 50) then
        gfile%idate(1)=2000+gfile%idate(1)
    else if (gfile%idate(1) .lt. 100) then
        gfile%idate(1)=1999+gfile%idate(1)
    endif
    if ( gfile%idate(1).eq.nemsio_intfill) then
      print *,'idate=',gfile%idate,' WRONG: please provide idate(1:7)(yyyy/mm/dd/hh/min/secn/secd)!!!'
      call nemsio_stop()
    endif
!
    if ( gfile%gtype(1:6).eq."NEMSIO" ) then
      call nemsio_gfinit(gfile,ios,recname=recname,reclevtyp=reclevtyp,reclev=reclev)
      if (ios .ne.0 ) then
        iret=ios
        return
      endif
    endif
!
!------------------------------------------------------------
! set up basic gfile meta data variables from outsides to 
! define meta data array
!------------------------------------------------------------
    if(present(nfday)) gfile%nfday=nfday
    if(present(nfhour)) gfile%nfhour=nfhour
    if(present(nfminute)) gfile%nfminute=nfminute
    if(present(nfsecondn)) gfile%nfsecondn=nfsecondn
    if(present(nfsecondd)) gfile%nfsecondd=nfsecondd
    if(present(ntrac)) gfile%ntrac=ntrac
    if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=0
    if(present(ncldt)) gfile%ncldt=ncldt
    if(present(jcap)) gfile%jcap=jcap
    if(present(idvc)) gfile%idvc=idvc
    if(present(idsl)) gfile%idsl=idsl
    if(present(idvm)) gfile%idvm=idvm
    if(present(idrt)) gfile%idrt=idrt
    if(present(rlon_min)) gfile%rlon_min=rlon_min
    if(present(rlon_max)) gfile%rlon_max=rlon_max
    if(present(rlat_min)) gfile%rlat_min=rlat_min
    if(present(rlat_max)) gfile%rlat_max=rlat_max
    if(present(extrameta)) gfile%extrameta=extrameta
    if(gfile%fieldsize.eq.nemsio_intfill) &
       gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)
    if(gfile%mype.eq.gfile%lead_task) then
     if(gfile%gdatatype(1:4).eq.'bin4') then
      call mpi_send(gfile%fieldsize*nemsio_realkind,1,MPI_integer,0,99,gfile%mpi_comm,ios)
      call mpi_recv(gfile%fieldsize_real4,1,MPI_real4,0,99,gfile%mpi_comm,status,ios)
     elseif(gfile%gdatatype(1:4).eq.'bin8') then
      call mpi_send(gfile%fieldsize*nemsio_dblekind,1,MPI_integer,0,99,gfile%mpi_comm,ios)
      call mpi_recv(gfile%fieldsize_real8,1,MPI_real8,0,99,gfile%mpi_comm,status,ios)
     endif
    endif
!
!---------------------------------------------------------------------
!*** for lead write task
!---------------------------------------------------------------------
!
    if(gfile%mype.eq.gfile%lead_task) then
!
    if( gfile%extrameta )then
      if(present(nmetavari).and.present(variname).and.present(varival)) then
        if(nmetavari.gt.0 .and.size(variname).eq.nmetavari .and. &
          size(varival).eq.nmetavari) then
           gfile%nmetavari=nmetavari
           if(allocated(gfile%variname)) deallocate(gfile%variname)
           if(allocated(gfile%varival)) deallocate(gfile%varival)
           allocate(gfile%variname(nmetavari),gfile%varival(nmetavari))
           gfile%variname=variname
           gfile%varival=varival
        endif
      endif
      if(present(nmetavarr).and.present(varrname).and.present(varrval)) then
        if( nmetavarr.gt.0.and.size(varrname).eq.nmetavarr .and. &
          size(varrval).eq.nmetavarr) then
           gfile%nmetavarr=nmetavarr
           if(allocated(gfile%varrname)) deallocate(gfile%varrname)
           if(allocated(gfile%varrval)) deallocate(gfile%varrval)
           allocate(gfile%varrname(nmetavarr),gfile%varrval(nmetavarr))
           gfile%varrname=varrname
           gfile%varrval=varrval
        endif
      endif
      if(present(nmetavarl).and.present(varlname).and.present(varlval)) then
        if( nmetavarl.gt.0.and.size(varlname).eq.nmetavarl .and. &
          size(varlval).eq.nmetavarl) then
           gfile%nmetavarl=nmetavarl
           if(allocated(gfile%varlname)) deallocate(gfile%varlname)
           if(allocated(gfile%varlval)) deallocate(gfile%varlval)
           allocate(gfile%varlname(nmetavarl),gfile%varlval(nmetavarl))
           gfile%varlname=varlname
           gfile%varlval=varlval
        endif
      endif
      if(present(nmetavarc).and.present(varcname).and.present(varcval)) then
        if( nmetavarc.gt.0.and.size(varcname).eq.nmetavarc .and. &
          size(varcval).eq.nmetavarc) then
           gfile%nmetavarc=nmetavarc
           if(allocated(gfile%varcname)) deallocate(gfile%varcname)
           if(allocated(gfile%varcval)) deallocate(gfile%varcval)
           allocate(gfile%varcname(nmetavarc),gfile%varcval(nmetavarc))
           gfile%varcname=varcname
           gfile%varcval=varcval
        endif
      endif
      if(present(nmetavarr8).and.present(varr8name).and.present(varr8val)) then
        if( nmetavarr8.gt.0.and.size(varr8name).eq.nmetavarr8 .and. &
          size(varr8val).eq.nmetavarr8) then
            gfile%nmetavarr8=nmetavarr8
            if(allocated(gfile%varr8name)) deallocate(gfile%varr8name)
            if(allocated(gfile%varr8val)) deallocate(gfile%varr8val)
            allocate(gfile%varr8name(nmetavarr8),gfile%varr8val(nmetavarr8))
            gfile%varr8name=varr8name
            gfile%varr8val=varr8val
        endif
      endif
      if(present(nmetaaryi).and.present(aryiname).and.present(aryilen)) then
        if( nmetaaryi.gt.0.and.size(aryiname).eq.nmetaaryi .and. &
          size(aryilen).eq.nmetaaryi) then
           gfile%nmetaaryi=nmetaaryi
           if(allocated(gfile%aryiname)) deallocate(gfile%aryiname)
           if(allocated(gfile%aryilen)) deallocate(gfile%aryilen)
           allocate(gfile%aryiname(nmetaaryi),gfile%aryilen(nmetaaryi))
           gfile%aryiname=aryiname
           gfile%aryilen=aryilen
           if(present(aryival)) then
             if(size(aryival).eq.nmetaaryi*maxval(gfile%aryilen) ) then
               if(allocated(gfile%aryival)) deallocate(gfile%aryival)
               allocate(gfile%aryival(maxval(gfile%aryilen),nmetaaryi))
               gfile%aryival=aryival
             endif
           endif
        endif
      endif
      if(present(nmetaaryr).and.present(aryrname).and.present(aryrlen)) then
        if( nmetaaryr.gt.0.and.size(aryrname).eq.nmetaaryr .and. &
          size(aryrlen).eq.nmetaaryr) then
           gfile%nmetaaryr=nmetaaryr
           if(allocated(gfile%aryrname)) deallocate(gfile%aryrname)
           if(allocated(gfile%aryrlen)) deallocate(gfile%aryrlen)
           allocate(gfile%aryrname(nmetaaryr),gfile%aryrlen(nmetaaryr))
           gfile%aryrname=aryrname
           gfile%aryrlen=aryrlen
           if(present(aryrval) ) then
              if(size(aryrval).eq.nmetaaryr*maxval(gfile%aryrlen)) then
                if(allocated(gfile%aryrval)) deallocate(gfile%aryrval)
                allocate(gfile%aryrval(maxval(gfile%aryrlen),nmetaaryr))
                gfile%aryrval=aryrval
              endif
            endif
        endif
      endif
      if(present(nmetaaryl).and.present(arylname).and.present(aryllen)) then
        if( nmetaaryl.gt.0 .and.size(arylname).eq.nmetaaryl .and. &
          size(aryllen).eq.nmetaaryl) then
           gfile%nmetaaryl=nmetaaryl
           if(allocated(gfile%arylname)) deallocate(gfile%arylname)
           if(allocated(gfile%aryllen)) deallocate(gfile%aryllen)
           allocate(gfile%arylname(nmetaaryl),gfile%aryllen(nmetaaryl))
           gfile%arylname=arylname
           gfile%aryllen=aryllen
           if(present(arylval)) then
              if(size(arylval).eq.nmetaaryl*maxval(gfile%aryllen)) then
                if(allocated(gfile%arylval)) deallocate(gfile%arylval)
                allocate(gfile%arylval(maxval(gfile%aryllen),nmetaaryl))
                gfile%arylval=arylval
             endif
           endif
        endif
      endif
      if(present(nmetaaryc).and.present(arycname).and.present(aryclen)) then
        if( nmetaaryc.gt.0 .and.size(arycname).eq.nmetaaryc .and. &
          size(aryclen).eq.nmetaaryc) then
           gfile%nmetaaryc=nmetaaryc
           if(allocated(gfile%arycname)) deallocate(gfile%arycname)
           if(allocated(gfile%aryclen)) deallocate(gfile%aryclen)
           allocate(gfile%arycname(nmetaaryc),gfile%aryclen(nmetaaryc))
           gfile%arycname=arycname
           gfile%aryclen=aryclen
           if(present(arycval)) then
              if(size(arycval).eq.nmetaaryc*maxval(gfile%aryclen)) then
                if(allocated(gfile%arycval)) deallocate(gfile%arycval)
                allocate(gfile%arycval(maxval(gfile%aryclen),nmetaaryc))
                gfile%arycval=arycval
              endif
           endif
        endif
      endif
      if(present(nmetaaryr8).and.present(aryr8name).and.present(aryr8len)) then
        if( nmetaaryr8.gt.0.and.size(aryr8name).eq.nmetaaryr8 .and. &
          size(aryr8len).eq.nmetaaryr8) then
            gfile%nmetaaryr8=nmetaaryr8
            if(allocated(gfile%aryr8name)) deallocate(gfile%aryr8name)
            if(allocated(gfile%aryr8len)) deallocate(gfile%aryr8len)
            allocate(gfile%aryr8name(nmetaaryr8),gfile%aryr8len(nmetaaryr8))
            gfile%aryr8name=aryr8name
            gfile%aryr8len=aryr8len
            if(present(aryr8val) ) then
              if(size(aryr8val).eq.nmetaaryr8*maxval(gfile%aryr8len)) then
                if(allocated(gfile%aryr8val)) deallocate(gfile%aryr8val)
                allocate(gfile%aryr8val(maxval(gfile%aryr8len),nmetaaryr8))
                gfile%aryr8val=aryr8val
              endif
            endif
        endif
      endif
      if (gfile%nmetavari+gfile%nmetavarr+gfile%nmetavarl+gfile%nmetavarc+ &
          gfile%nmetaaryi+gfile%nmetaaryr+gfile%nmetaaryl+gfile%nmetaaryc+ &
          gfile%nmetavarr8+gfile%nmetaaryr8 .lt.10*nemsio_intfill )then
           print *,'WRONG: gfile%extrameta is not compatiable with input extra meta!'
           return
      endif
    endif 
!
!------------------------------------------------------------
! check gfile meta data array size
!------------------------------------------------------------
    call nemsio_chkgfary(gfile,ios)
    if (ios.ne. 0) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! continue to set gfile meta data variables tnd arrays
!------------------------------------------------------------
!set gfile data type to bin/grb, default set to grb
!recname
    nummeta=2
    if(present(recname) ) then
       if (gfile%nrec.eq.size(recname)) then
         gfile%recname=recname
       else
         print *,'WRONG: the size of recname is not equal to the total number of the fields in the file!'
         return
       endif
       nummeta=nummeta+1
    endif
!reclevtyp
    if(present(reclevtyp)) then
       if (gfile%nrec.eq.size(reclevtyp)) then
         gfile%reclevtyp=reclevtyp
       else
         print *,'WRONG: the size of reclevtyp is not equal to the total number of the fields in the file!'
         return
       endif
       nummeta=nummeta+1
    endif
!reclev
    if(present(reclev) ) then
       if (gfile%nrec.eq.size(reclev)) then
         gfile%reclev=reclev
       else
         print *,'WRONG: the size of reclev is not equal to the total number of the fields in the file!'
         return
       endif
       nummeta=nummeta+1
    endif
!vcoord vcoord(levs+1
    if(present(vcoord) ) then
       if ((gfile%dimz+1)*3*2.eq.size(vcoord)) then
         gfile%vcoord=vcoord
       else
         print *,'WRONG: the size of vcoord is not (lm+1,3,2) !'
         return
       endif
       nummeta=nummeta+1
    endif
!lat
    if(present(lat) ) then
       if (gfile%fieldsize.eq.size(lat)) then
         if(.not.(all(lat==0.))) gfile%lat=lat
       else
         print *,'WRONG: the input size(lat) ',size(lat),' is not equal to: ',gfile%fieldsize
         return
       endif
       nummeta=nummeta+1
    endif
    if(allocated(gfile%lat)) then
       gfile%rlat_max=maxval(gfile%lat)
       gfile%rlat_min=minval(gfile%lat)
    endif
!lon
    if(present(lon) ) then
       if (gfile%fieldsize.eq.size(lon)) then
         if(.not.(all(lon==0.)) ) gfile%lon=lon
       else
         print *,'WRONG: the input size(lon) ',size(lon),' is not equal to: ',gfile%fieldsize
         return
       endif
       nummeta=nummeta+1
    endif
    if(allocated(gfile%lon)) then
       gfile%rlon_max=maxval(gfile%lon)
       gfile%rlon_min=minval(gfile%lon)
    endif
!dx
    if(present(dx) ) then
       if (gfile%fieldsize.eq.size(dx)) then
         if(.not.(all(dx==0.)) ) gfile%dx=dx
       else
         print *,'WRONG: the input size(dx) ',size(dx),' is not equal to: ',gfile%fieldsize
         return
       endif
       nummeta=nummeta+1
    endif
!dy
    if(present(dy) ) then
       if (gfile%fieldsize.eq.size(dy)) then
         if(.not.(all(dy==0.)) ) gfile%dy=dy
       else
         print *,'WRONG: the input size(dy) ',size(dy),' is not equal to: ',gfile%fieldsize
         return
       endif
       nummeta=nummeta+1
    endif
!Cpi
    if( present(Cpi) ) then
       if (gfile%ntrac+1.eq.size(gfile%Cpi)) then
         if(.not.(all(cpi==0.))) gfile%Cpi = Cpi
       else
         print *,'WRONG: the input size(cpi) ',size(cpi),' is not equal to: ',gfile%ntrac+1
         return
       endif
       nummeta=nummeta+1
    endif
!Ri
    if( present(Ri) ) then
       if (gfile%ntrac+1.eq.size(gfile%Ri)) then
         if(.not.(all(ri==0.))) gfile%Ri = Ri
       else
         print *,'WRONG: the input size(ri) ',size(ri),' is not equal to: ',gfile%ntrac+1
         return
       endif
       nummeta=nummeta+1
    endif
    if(gfile%nmeta==nemsio_intfill) gfile%nmeta=nummeta
!------------------------------------------------------------
! write out the header by lead_task
!------------------------------------------------------------
!------------------------------------------------------------
! write out first meta data record
!------------------------------------------------------------
    meta1%gtype=gfile%gtype
    meta1%gdatatype=gfile%gdatatype
    meta1%modelname=gfile%modelname
    meta1%version=gfile%version
    meta1%nmeta=gfile%nmeta
    meta1%lmeta=gfile%lmeta
    meta1%reserve=0
    idisp=0
    iwrite=nemsio_lmeta1
    if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
    call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
    idisp=4
    if(gfile%do_byteswap) call byteswap(meta1%version,nemsio_intkind,6)
    call mpi_file_write_at(gfile%fh,idisp,meta1,1,itypemeta1,status,ios)
    if(gfile%do_byteswap) call byteswap(meta1%version,nemsio_intkind,6)
    idisp=4+nemsio_lmeta1
    call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
    if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
    gfile%tlmeta=nemsio_lmeta1+8
!------------------------------------------------------------
! write out second meta data record
!------------------------------------------------------------
      meta2%nrec=gfile%nrec
      meta2%idate(1:7)=gfile%idate(1:7)
      meta2%nfday=gfile%nfday
      meta2%nfhour=gfile%nfhour
      meta2%nfminute=gfile%nfminute
      meta2%nfsecondn=gfile%nfsecondn
      meta2%nfsecondd=gfile%nfsecondd
      meta2%dimx=gfile%dimx
      meta2%dimy=gfile%dimy
      meta2%dimz=gfile%dimz
      meta2%nframe=gfile%nframe
      meta2%nsoil=gfile%nsoil
      meta2%ntrac=gfile%ntrac
      meta2%jcap=gfile%jcap
      meta2%ncldt=gfile%ncldt
      meta2%idvc=gfile%idvc
      meta2%idsl=gfile%idsl
      meta2%idvm=gfile%idvm
      meta2%idrt=gfile%idrt
      meta2%rlon_min=gfile%rlon_min
      meta2%rlon_max=gfile%rlon_max
      meta2%rlat_min=gfile%rlat_min
      meta2%rlat_max=gfile%rlat_max
      meta2%extrameta=gfile%extrameta
      idisp=gfile%tlmeta
      iwrite=gfile%lmeta
      if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
      call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap) then
        call byteswap(meta2%nrec,nemsio_intkind,25)
        call byteswap(meta2%rlon_min,nemsio_realkind,4)
        call byteswap(meta2%extrameta,nemsio_logickind,1)
      endif
      idisp=idisp+4
      call mpi_file_write_at(gfile%fh,idisp,meta2,1,itypemeta2,status,ios)
      if(gfile%do_byteswap) then
        call byteswap(meta2%nrec,nemsio_intkind,25)
        call byteswap(meta2%rlon_min,nemsio_realkind,4)
        call byteswap(meta2%extrameta,nemsio_logickind,1)
      endif
      idisp=idisp+gfile%lmeta
      call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
      gfile%tlmeta=gfile%tlmeta+gfile%lmeta+8
!     print *,'tlmet2 =',gfile%tlmeta,'nwrite=',nwrite,'meta2=', &
!      meta2%dimx,meta2%dimy,meta2%dimz,meta2%nframe,meta2%nsoil, &
!      meta2%ntrac,meta2%jcap,meta2%ncldt,meta2%idvc,meta2%idsl,  &
!      meta2%idvm,meta2%idrt,meta2%rlon_min,meta2%rlon_max,       &
!      meta2%rlat_min,meta2%rlat_max,meta2%extrameta
      nummeta=gfile%nmeta
!------------------------------------------------------------
! write out 3rd-13th meta data record (arrays)
!------------------------------------------------------------
!recname
      if( nummeta>2) then
        idisp=gfile%tlmeta
        iwrite=nemsio_charkind*size(gfile%recname)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%recname,iwrite,MPI_CHARACTER,status,ios)
        if(ios<0) return
        idisp=idisp+nemsio_charkind*size(gfile%recname)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_charkind*size(gfile%recname)+8
      endif
!reclevtyp
      if( nummeta>3) then
        idisp=gfile%tlmeta
        iwrite=nemsio_charkind*size(gfile%reclevtyp)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%reclevtyp,iwrite,MPI_CHARACTER,status,ios)
        if(ios<0) return
        idisp=idisp+nemsio_charkind*size(gfile%recname)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_charkind*size(gfile%reclevtyp)+8
      endif
!reclev
      if( nummeta>4) then
        idisp=gfile%tlmeta
        iwrite=nemsio_intkind*size(gfile%reclev)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%reclev,nemsio_intkind,size(gfile%reclev))
        call mpi_file_write_at(gfile%fh,idisp,gfile%reclev,size(gfile%reclev),MPI_INTEGER,status,ios)
        if(ios<0) return
        idisp=idisp+nemsio_intkind*size(gfile%reclev)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_intkind*size(gfile%reclev)+8
      endif
!vcoord
      if ( nummeta>5 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%vcoord)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%vcoord,nemsio_realkind,size(gfile%vcoord))
        call mpi_file_write_at(gfile%fh,idisp,gfile%vcoord,size(gfile%vcoord),MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%vcoord,nemsio_realkind,size(gfile%vcoord))
        idisp=idisp+nemsio_realkind*size(gfile%vcoord)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%vcoord)+8
      endif
!lat
      if ( nummeta>6 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%lat)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%lat,nemsio_realkind,size(gfile%lat))
        call mpi_file_write_at(gfile%fh,idisp,gfile%lat,gfile%fieldsize,MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%lat,nemsio_realkind,size(gfile%lat))
        idisp=idisp+nemsio_realkind*size(gfile%lat)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%lat)+8
      endif
!lon
      if ( nummeta>7 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%lon)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%lon,nemsio_realkind,size(gfile%lon))
        call mpi_file_write_at(gfile%fh,idisp,gfile%lon,gfile%fieldsize,MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%lon,nemsio_realkind,size(gfile%lon))
        idisp=idisp+nemsio_realkind*size(gfile%lon)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%lat)+8
!      print *,'tlmetreclon=',gfile%tlmeta,'nwrite=',nwrite
      endif
!dx
      if ( nummeta>8 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%dx)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%dx,nemsio_realkind,size(gfile%dx))
        call mpi_file_write_at(gfile%fh,idisp,gfile%dx,gfile%fieldsize,MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%dx,nemsio_realkind,size(gfile%dx))
        idisp=idisp+nemsio_realkind*size(gfile%dx)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%dx)+8
      endif
!dy
      if ( nummeta>9 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%dy)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%dy,nemsio_realkind,size(gfile%dy))
        call mpi_file_write_at(gfile%fh,idisp,gfile%dy,gfile%fieldsize,MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%dy,nemsio_realkind,size(gfile%dy))
        idisp=idisp+nemsio_realkind*size(gfile%dy)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%dy)+8
!      print *,'tlmetrecdy=',gfile%tlmeta,'nwrite=',nwrite
      endif
!Cpi
      if ( nummeta>10 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%cpi)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%cpi,nemsio_realkind,size(gfile%cpi))
        call mpi_file_write_at(gfile%fh,idisp,gfile%cpi,size(gfile%cpi),MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%cpi,nemsio_realkind,size(gfile%cpi))
        idisp=idisp+nemsio_realkind*size(gfile%cpi)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%cpi)+8
      endif
!Ri
      if ( nummeta>11 ) then
        idisp=gfile%tlmeta
        iwrite=nemsio_realkind*size(gfile%ri)
        if(gfile%do_byteswap) call byteswap(iwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap) call byteswap(gfile%ri,nemsio_realkind,size(gfile%ri))
        call mpi_file_write_at(gfile%fh,idisp,gfile%ri,size(gfile%ri),MPI_REAL,status,ios)
        if(ios<0) return
        if(gfile%do_byteswap) call byteswap(gfile%ri,nemsio_realkind,size(gfile%ri))
        idisp=idisp+nemsio_realkind*size(gfile%ri)
        call mpi_file_write_at(gfile%fh,idisp,iwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+nemsio_realkind*size(gfile%ri)+8
      endif
!------------------------------------------------------------
! write out extra meta data record 
!------------------------------------------------------------
    if(gfile%extrameta) then
      meta3%nmetavari=gfile%nmetavari
      meta3%nmetavarr=gfile%nmetavarr
      meta3%nmetavarl=gfile%nmetavarl
      meta3%nmetavarc=gfile%nmetavarc
      meta3%nmetaaryi=gfile%nmetaaryi
      meta3%nmetaaryr=gfile%nmetaaryr
      meta3%nmetaaryl=gfile%nmetaaryl
      meta3%nmetaaryc=gfile%nmetaaryc
      meta3%nmetavarr8=gfile%nmetavarr8
      meta3%nmetaaryr8=gfile%nmetaaryr8
      idisp=gfile%tlmeta
!!!!!####!!now iwritewill be nemsio_lmeta3
      if(gfile%nmetavarr8>0.or.gfile%nmetaaryr8>0) then
        iwrite=nemsio_lmeta3
      else
        iwrite=nemsio_lmeta3-8
      endif
      nwrite=iwrite
      if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
      call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
      idisp=idisp+4
      if(gfile%do_byteswap)    &
        call byteswap(meta3%nmetavari,nemsio_intkind,iwrite/4)
      call mpi_file_write_at(gfile%fh,idisp,meta3,iwrite/4,MPI_INTEGER,status,ios)
      if(gfile%do_byteswap)    &
        call byteswap(meta3%nmetavari,nemsio_intkind,iwrite/4)
      idisp=idisp+iwrite
      call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
      gfile%tlmeta=gfile%tlmeta+iwrite+8
!      print *,'tlmetameta3=',gfile%tlmeta,'nmetavari=',gfile%nmetavari,gfile%nmetavarr, &
!       gfile%nmetavarl,gfile%nmetavarc,gfile%nmetavarr8,'nmetaaryi=',gfile%nmetaaryi, &
!       gfile%nmetaaryr,gfile%nmetaaryl,gfile%nmetaaryc,gfile%nmetaaryr8,'iwrite=',iwrite
!
!-- write meta var integer
      if (gfile%nmetavari.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%variname)*gfile%nmetavari
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%variname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8
!
        idisp=gfile%tlmeta
        iwrite=gfile%nmetavari
        nwrite=iwrite*kind(gfile%varival)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varival,nemsio_intkind,size(gfile%varival))
        call mpi_file_write_at(gfile%fh,idisp,gfile%varival,iwrite,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varival,nemsio_intkind,size(gfile%varival))
        idisp=idisp+iwrite*kind(gfile%varival)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%varival)+8
      endif
!var real4
      if (gfile%nmetavarr.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%varrname)*gfile%nmetavarr
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%varrname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetavarr
        nwrite=iwrite*kind(gfile%varrval)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varrval,nemsio_realkind,size(gfile%varrval))
        call mpi_file_write_at(gfile%fh,idisp,gfile%varrval,iwrite,MPI_REAL,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varrval,nemsio_realkind,size(gfile%varrval))
        idisp=idisp+iwrite*kind(gfile%varrval)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%varrval)+8

      endif
!var logical
      if (gfile%nmetavarl.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%varlname)*gfile%nmetavarl
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%varlname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetavarl
        nwrite=iwrite*kind(gfile%varlval)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varlval,nemsio_logickind,size(gfile%varlval))
        call mpi_file_write_at(gfile%fh,idisp,gfile%varlval,iwrite,MPI_LOGICAL,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varlval,nemsio_logickind,size(gfile%varlval))
        idisp=idisp+iwrite*kind(gfile%varlval)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%varlval)+8
      endif

!var character
      if (gfile%nmetavarc.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%varcname)*gfile%nmetavarc
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%varcname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8


        idisp=gfile%tlmeta
        iwrite=gfile%nmetavarc*len(gfile%varcval)
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%varcval,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8
      endif

!var real8
      if (gfile%nmetavarr8.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%varr8name)*gfile%nmetavarr8
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%varr8name,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetavarr8
        nwrite=iwrite*kind(gfile%varr8val)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varr8val,nemsio_dblekind,size(gfile%varr8val))
        call mpi_file_write_at(gfile%fh,idisp,gfile%varr8val,iwrite,MPI_REAL8,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%varr8val,nemsio_dblekind,size(gfile%varr8val))
        idisp=idisp+iwrite*kind(gfile%varr8val)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%varr8val)+8
      endif

!meta arr integer
      if (gfile%nmetaaryi.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%aryiname)*gfile%nmetaaryi
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryiname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetaaryi
        nwrite=iwrite*kind(gfile%aryilen)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryilen,nemsio_intkind,size(gfile%aryilen))
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryilen,iwrite,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryilen,nemsio_intkind,size(gfile%aryilen))
        idisp=idisp+iwrite*kind(gfile%aryilen)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryilen)+8
!
        do i=1,gfile%nmetaaryi
          idisp=gfile%tlmeta
          iwrite=gfile%aryilen(i)
          nwrite=iwrite*kind(gfile%aryival)
          if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          idisp=idisp+4
          if(gfile%do_byteswap)    &
            call byteswap(gfile%aryival(1:iwrite,i),nemsio_intkind,gfile%aryilen(i))
          call mpi_file_write_at(gfile%fh,idisp,gfile%aryival(1:iwrite,i),iwrite,MPI_INTEGER,status,ios)
          if(gfile%do_byteswap)    &
          call byteswap(gfile%aryival(1:iwrite,i),nemsio_intkind,gfile%aryilen(i))
          idisp=idisp+iwrite*kind(gfile%aryival)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryival)+8
        enddo
      endif
!meta arr real
      if (gfile%nmetaaryr.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%aryrname)*gfile%nmetaaryr
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryrname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetaaryr
        nwrite=iwrite*kind(gfile%aryrlen)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryrlen,nemsio_intkind,size(gfile%aryrlen))
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryrlen,iwrite,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryrlen,nemsio_intkind,size(gfile%aryrlen))
        idisp=idisp+iwrite*kind(gfile%aryrlen)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryrlen)+8
!
        do i=1,gfile%nmetaaryr
          idisp=gfile%tlmeta
          iwrite=gfile%aryrlen(i)
          nwrite=iwrite*kind(gfile%aryrval)
          if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          idisp=idisp+4
          if(gfile%do_byteswap)    &
            call byteswap(gfile%aryrval(1:iwrite,i),nemsio_realkind,gfile%aryrlen(i))
          call mpi_file_write_at(gfile%fh,idisp,gfile%aryrval(1:iwrite,i),iwrite,MPI_REAL,status,ios)
          if(gfile%do_byteswap)    &
          call byteswap(gfile%aryrval(1:iwrite,i),nemsio_realkind,gfile%aryrlen(i))
          idisp=idisp+iwrite*kind(gfile%aryrval)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryrval)+8
        enddo
      endif
!meta arr logical
      if (gfile%nmetaaryl.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%arylname)*gfile%nmetaaryl
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%arylname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetaaryl
        nwrite=iwrite*kind(gfile%aryllen)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryllen,nemsio_intkind,size(gfile%aryllen))
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryllen,iwrite,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryllen,nemsio_intkind,size(gfile%aryllen))
        idisp=idisp+iwrite*kind(gfile%aryllen)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryllen)+8

        do i=1,gfile%nmetaaryl
          idisp=gfile%tlmeta
          iwrite=gfile%aryllen(i)
          nwrite=iwrite*kind(gfile%arylval)
          if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          idisp=idisp+4
          if(gfile%do_byteswap)    &
            call byteswap(gfile%arylval(1:iwrite,i),nemsio_logickind,gfile%aryllen(i))
          call mpi_file_write_at(gfile%fh,idisp,gfile%arylval(1:iwrite,i),iwrite,MPI_LOGICAL,status,ios)
          if(gfile%do_byteswap)    &
          call byteswap(gfile%arylval(1:iwrite,i),nemsio_logickind,gfile%aryllen(i))
          idisp=idisp+iwrite*kind(gfile%arylval)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%arylval)+8
        enddo
      endif
!meta arr char
      if (gfile%nmetaaryc.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%arycname)*gfile%nmetaaryc
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%arycname,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetaaryc
        nwrite=iwrite*kind(gfile%aryclen)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryclen,nemsio_intkind,size(gfile%aryclen))
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryclen,iwrite,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryclen,nemsio_intkind,size(gfile%aryclen))
        idisp=idisp+iwrite*kind(gfile%aryclen)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryclen)+8

        do i=1,gfile%nmetaaryc
          idisp=gfile%tlmeta
          iwrite=gfile%aryclen(i)*len(gfile%arycval)
          nwrite=iwrite
          if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          idisp=idisp+4
          call mpi_file_write_at(gfile%fh,idisp,gfile%arycval(1:iwrite,i),iwrite,MPI_CHARACTER,status,ios)
          idisp=idisp+iwrite
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          gfile%tlmeta=gfile%tlmeta+iwrite+8
        enddo
      endif
!meta arr real8
      if (gfile%nmetaaryr8.gt.0) then
        idisp=gfile%tlmeta
        iwrite=len(gfile%aryr8name)*gfile%nmetaaryr8
        nwrite=iwrite
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryr8name,iwrite,MPI_CHARACTER,status,ios)
        idisp=idisp+iwrite
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite+8

        idisp=gfile%tlmeta
        iwrite=gfile%nmetaaryr8
        nwrite=iwrite*kind(gfile%aryr8len)
        if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        idisp=idisp+4
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryr8len,nemsio_intkind,size(gfile%aryr8len))
        call mpi_file_write_at(gfile%fh,idisp,gfile%aryr8len,iwrite,MPI_INTEGER,status,ios)
        if(gfile%do_byteswap)    &
          call byteswap(gfile%aryr8len,nemsio_intkind,size(gfile%aryr8len))
        idisp=idisp+iwrite*kind(gfile%aryr8len)
        call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
        gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryr8len)+8

        do i=1,gfile%nmetaaryr8
          idisp=gfile%tlmeta
          iwrite=gfile%aryr8len(i)
          nwrite=iwrite*kind(gfile%aryr8val)
          if(gfile%do_byteswap) call byteswap(nwrite,nemsio_intkind,1)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          idisp=idisp+4
          if(gfile%do_byteswap)    &
            call byteswap(gfile%aryr8val(1:iwrite,i),nemsio_dblekind,gfile%aryr8len(i))
          call mpi_file_write_at(gfile%fh,idisp,gfile%aryr8val(1:iwrite,i),iwrite,MPI_REAL8,status,ios)
          if(gfile%do_byteswap)    &
          call byteswap(gfile%aryr8val(1:iwrite,i),nemsio_dblekind,gfile%aryr8len(i))
          idisp=idisp+iwrite*kind(gfile%aryr8val)
          call mpi_file_write_at(gfile%fh,idisp,nwrite,1,MPI_INTEGER,status,ios)
          gfile%tlmeta=gfile%tlmeta+iwrite*kind(gfile%aryr8val)+8
        enddo
      endif

    endif     !end of gfile%extrameta
!
   endif      !end of lead_task
!mpi
    call MPI_Barrier(gfile%mpi_comm, ios)
    call mpi_bcast(gfile%tlmeta,1,MPI_INTEGER8,gfile%lead_task,gfile%mpi_comm,ios)
!    write(0,*)'after mpi_bcasttlmeta,',gfile%tlmeta, 'end of wcreate,ios=',ios
!
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_wcreate
!------------------------------------------------------------------------------
  subroutine nemsio_getfilehead(gfile,iret,gtype,gdatatype,gfname,gaction, &
      modelname,version,nmeta,lmeta,nrec,idate,nfday,nfhour,nfminute,  &
      nfsecondn,nfsecondd,dimx,dimy,dimz,nframe,nsoil,ntrac,ncldt,jcap,&
      idvc,idsl,idvm,idrt, rlon_min,rlon_max,rlat_min,rlat_max,tlmeta, &
      file_endian,do_byteswap,                                         &
      extrameta,nmetavari,nmetavarr,nmetavarl,nmetavarc,nmetavarr8,    &
      nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,nmetaaryr8,    &
      recname,reclevtyp,reclev,vcoord,lon,lat,dx,dy,cpi,ri,  &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval, &
      varr8name,varr8val,                                   &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,    &
      arylname,aryllen,arylval,arycname,aryclen,arycval,    &
      aryr8name,aryr8len,aryr8val    )

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get nemsio meta data information from outside
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    integer(nemsio_intkind),optional,intent(out) :: iret
    character*(*),optional,intent(out)           :: gtype,gdatatype,gfname, &
                                                    gaction,modelname
    integer(nemsio_intkind),optional,intent(out) :: version,nmeta,lmeta,tlmeta
    integer(nemsio_realkind),optional,intent(out):: nrec,idate(7),nfday,nfhour, &
                                                    nfminute,nfsecondn,nfsecondd
    integer(nemsio_realkind),optional,intent(out):: dimx,dimy,dimz,nframe, &
                                                    nsoil,ntrac
    integer(nemsio_realkind),optional,intent(out):: ncldt,jcap,idvc,idsl,idvm,idrt
    real(nemsio_realkind),optional,intent(out)   :: rlon_min,rlon_max,rlat_min, &
                                                    rlat_max
    character*(*),optional,intent(out)           :: file_endian
    logical(nemsio_logickind),optional,intent(out):: do_byteswap
    logical(nemsio_logickind),optional,intent(out):: extrameta
    integer(nemsio_realkind),optional,intent(out):: nmetavari,nmetavarr, &
                                                    nmetavarl,nmetavarc,nmetaaryi, &
                                                    nmetaaryr,nmetaaryl,nmetaaryc, &
                                                    nmetavarr8,nmetaaryr8
    character(*),optional,intent(out)           :: recname(:)
    character(*),optional,intent(out)           :: reclevtyp(:)
    integer(nemsio_intkind),optional,intent(out) :: reclev(:)
    real(nemsio_realkind),optional,intent(out)   :: vcoord(:,:,:)
    real(nemsio_realkind),optional,intent(out)   :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(out)   :: dx(:),dy(:)
    real(nemsio_realkind),optional,intent(out)   :: Cpi(:),Ri(:)
    character(*),optional,intent(out)            :: variname(:),varrname(:)
    character(*),optional,intent(out)            :: varlname(:),varcname(:)
    character(*),optional,intent(out)            :: varr8name(:)
    character(*),optional,intent(out)            :: aryiname(:),aryrname(:)
    character(*),optional,intent(out)            :: arylname(:),arycname(:)
    character(*),optional,intent(out)            :: aryr8name(:)
    integer(nemsio_intkind),optional,intent(out) :: aryilen(:),aryrlen(:)
    integer(nemsio_intkind),optional,intent(out) :: aryllen(:),aryclen(:)
    integer(nemsio_intkind),optional,intent(out) :: aryr8len(:)
    integer(nemsio_intkind),optional,intent(out) :: varival(:),aryival(:,:)
    real(nemsio_realkind),optional,intent(out)   :: varrval(:),aryrval(:,:)
    real(nemsio_dblekind),optional,intent(out)   :: varr8val(:),aryr8val(:,:)
    logical(nemsio_logickind),optional,intent(out):: varlval(:),arylval(:,:)
    character(*),optional,intent(out)             :: varcval(:),arycval(:,:)
    integer ios,i
!------------------------------------------------------------
    if (present(iret)) iret=-3
    if(present(gtype)) gtype=gfile%gtype
    if(present(gdatatype)) gdatatype=gfile%gdatatype
    if(present(gfname)) gfname=trim(gfile%gfname)
    if(present(gaction)) gaction=gfile%gaction
    if(present(modelname)) modelname=gfile%modelname
    if(present(version)) version=gfile%version
    if(present(nmeta)) nmeta=gfile%nmeta
    if(present(lmeta)) lmeta=gfile%lmeta
    if(present(nrec)) nrec=gfile%nrec
    if(present(nfday)) nfday=gfile%nfday
    if(present(nfhour)) nfhour=gfile%nfhour
    if(present(nfminute)) nfminute=gfile%nfminute
    if(present(nfsecondn)) nfsecondn=gfile%nfsecondn
    if(present(nfsecondd)) nfsecondd=gfile%nfsecondd
    if(present(idate)) idate(1:7)=gfile%idate(1:7)
    if(present(dimx)) dimx=gfile%dimx
    if(present(dimy)) dimy=gfile%dimy
    if(present(dimz)) dimz=gfile%dimz
    if(present(nframe)) nframe=gfile%nframe
    if(present(nsoil)) nsoil=gfile%nsoil
    if(present(ntrac)) ntrac=gfile%ntrac
    if(present(jcap)) jcap=gfile%jcap
    if(present(ncldt)) ncldt=gfile%ncldt
    if(present(idvc)) idvc=gfile%idvc
    if(present(idsl)) idsl=gfile%idsl
    if(present(idvm)) idvm=gfile%idvm
    if(present(idrt)) idrt=gfile%idrt
    if(present(rlon_min)) rlon_min=gfile%rlon_min
    if(present(rlon_max)) rlon_max=gfile%rlon_max
    if(present(rlat_min)) rlat_min=gfile%rlat_min
    if(present(rlat_max)) rlat_max=gfile%rlat_max
    if(present(rlat_max)) rlat_max=gfile%rlat_max
    if(present(tlmeta)) tlmeta=gfile%tlmeta
    if(present(file_endian)) file_endian=gfile%file_endian
    if(present(do_byteswap)) do_byteswap=gfile%do_byteswap
    if(present(extrameta)) extrameta=gfile%extrameta
!
!    print *,'in getfilehead, 1extrameta=',gfile%extrameta,        &
!     'nrec=',gfile%nrec,'size(recname)=',size(recname),           &
!     size(reclevtyp),size(reclev)
!--- rec
    if(present(recname) ) then
       if (gfile%nrec.ne.size(recname)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         recname=gfile%recname
       endif
    endif
    if(present(reclevtyp)) then
       if (gfile%nrec.ne.size(reclevtyp)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         reclevtyp=gfile%reclevtyp
       endif
    endif
    if(present(reclev) ) then
       if (gfile%nrec.ne.size(reclev)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         reclev=gfile%reclev
       endif
    endif
!--- vcoord
    if(present(vcoord)) then
       if (size(vcoord) .ne. (gfile%dimz+1)*2*3 ) then
         if ( present(iret))  return
         call nemsio_stop
       else
         vcoord=gfile%vcoord
       endif
    endif
!--- lat
    if(present(lat) ) then
       if (size(lat).ne.gfile%fieldsize) then
         print *,'WRONG: size(lat)=',size(lat),' is not equal to ',gfile%fieldsize
         if ( present(iret))  return
         call nemsio_stop
       else
         lat=gfile%lat
       endif
    endif
!--- lon
    if(present(lon) ) then
       if (size(lon).ne.gfile%fieldsize) then
         print *,'WRONG: size(lon)=',size(lon),' is not equal to ',gfile%fieldsize
         if ( present(iret)) return
         call nemsio_stop
       else
         lon=gfile%lon
       endif
    endif
!--- dx
    if(present(dx) ) then
!       print *,'getfilehead, size(dx)=',size(dx),gfile%fieldsize,  &
!          maxval(gfile%dx),minval(gfile%dx)
       if (size(dx).ne.gfile%fieldsize) then
         print *,'WRONG: size(dX)=',size(dx),' is not equal to ',gfile%fieldsize
         if ( present(iret))  return
         call nemsio_stop
       else
         dx=gfile%dx
       endif
    endif
    if(present(dy) ) then
       if (size(dy).ne.gfile%fieldsize) then
         print *,'WRONG: size(dy)=',size(dy),' is not equal to ',gfile%fieldsize
         if ( present(iret)) return
         call nemsio_stop
       else
         dy=gfile%dy
       endif
    endif
!--- Cpi
    if(present(Cpi) ) then
       if (gfile%ntrac+1.ne.size(Cpi)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         Cpi=gfile%Cpi
       endif
    endif
!Ri
    if(present(Ri) ) then 
       if (gfile%ntrac+1.ne.size(Ri)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         Ri=gfile%Ri
       endif
    endif
!------------------------------------------------------------------------------
!*** for extra meta field
!------------------------------------------------------------------------------
!extrameta
    if (present(nmetavari) ) nmetavari=gfile%nmetavari
    if (present(nmetavarr) ) nmetavarr=gfile%nmetavarr
    if (present(nmetavarl) ) nmetavarl=gfile%nmetavarl
    if (present(nmetavarc) ) nmetavarc=gfile%nmetavarc
    if (present(nmetavarr8) ) nmetavarr8=gfile%nmetavarr8
    if (present(nmetaaryi) ) nmetaaryi=gfile%nmetaaryi
    if (present(nmetaaryr) ) nmetaaryr=gfile%nmetaaryr
    if (present(nmetaaryl) ) nmetaaryl=gfile%nmetaaryl
    if (present(nmetaaryc) ) nmetaaryc=gfile%nmetaaryc
    if (present(nmetaaryr8) ) nmetaaryr8=gfile%nmetaaryr8
    if ( gfile%nmetavari.gt.0 ) then
       if (present(variname)) then
         if(size(variname).eq.nmetavari) then
           do i=1,nmetavari
             variname(i)=gfile%variname(i)
           enddo
         endif
       endif
       if (present(varival)) then
         if(size(varival).eq.nmetavari) varival(1:nmetavari)=gfile%varival(1:nmetavari)
       endif
    endif
    if ( gfile%nmetavarr.gt.0 ) then
       if (present(varrname)) then
         if(size(varrname).eq.nmetavarr) varrname=gfile%varrname
       endif
       if (present(varrval)) then
         if(size(varrval).eq.nmetavarr) varrval=gfile%varrval
       endif
    endif
    if ( gfile%nmetavarl.gt.0 ) then
       if (present(varlname)) then
         if(size(varlname).eq.nmetavarl) varlname=gfile%varlname
       endif
       if (present(varlval)) then
         if(size(varlval).eq.nmetavarl) varlval=gfile%varlval
       endif
    endif
    if ( gfile%nmetavarc.gt.0 ) then
       if (present(varcname)) then
         if(size(varcname).eq.gfile%nmetavarc)  varcname=gfile%varcname
       endif
       if (present(varcval)) then
         if(size(varcval).eq.gfile%nmetavarc)  varcval=gfile%varcval
       endif
    endif
    if ( gfile%nmetavarr8.gt.0 ) then
       if (present(varr8name)) then
         if(size(varr8name).eq.gfile%nmetavarr8) varr8name=gfile%varr8name
       endif
       if (present(varr8val)) then
         if(size(varr8val).eq.gfile%nmetavarr8)  varr8val=gfile%varr8val
       endif
    endif
    if ( gfile%nmetaaryi.gt.0 ) then
       if (present(aryiname)) then
         if(size(aryiname).eq.nmetaaryi) aryiname=gfile%aryiname
       endif
       if (present(aryilen)) then
         if(size(aryilen).eq.nmetaaryi) aryilen=gfile%aryilen
       endif
       if (present(aryival)) then
         if(size(aryival).eq.nmetaaryi*maxval(gfile%aryilen) ) &
           aryival=gfile%aryival
       endif
    endif
    if ( gfile%nmetaaryr.gt.0 ) then
       if (present(aryrname)) then
         if(size(aryrname).eq.nmetaaryr) aryrname=gfile%aryrname
       endif
       if (present(aryrlen)) then
         if(size(aryrlen).eq.nmetaaryr) aryrlen=gfile%aryrlen
       endif
       if (present(aryrval)) then
         if(size(aryrval).eq.nmetaaryr*maxval(gfile%aryrlen) ) &
           aryrval=gfile%aryrval
       endif
    endif
    if ( gfile%nmetaaryl.gt.0 ) then
       if (present(arylname)) then
         if(size(arylname).eq.nmetaaryl)  arylname=gfile%arylname
       endif
       if (present(aryllen)) then
         if(size(aryllen).eq.nmetaaryl) aryllen=gfile%aryllen
       endif
       if (present(arylval) ) then
         if(size(arylval).eq.nmetaaryl*maxval(gfile%aryllen) ) &
           arylval=gfile%arylval
       endif
    endif
    if ( gfile%nmetaaryc.gt.0 ) then
       if (present(arycname)) then
         if(size(arycname).eq.gfile%nmetaaryc)  arycname=gfile%arycname
       endif
       if (present(aryclen)) then
         if(size(aryclen).eq.gfile%nmetaaryc)  aryclen=gfile%aryclen
       endif
       if (present(arycval)) then
         if(size(arycval).eq.gfile%nmetaaryc*maxval(gfile%aryclen) ) &
           arycval=gfile%arycval
       endif
    endif
    if ( gfile%nmetaaryr8.gt.0 ) then
       if (present(aryr8name)) then
         if( size(aryr8name).eq.gfile%nmetaaryr8)  aryr8name=gfile%aryr8name
       endif
       if (present(aryr8len)) then
         if(size(aryr8len).eq.gfile%nmetaaryr8)  aryr8len=gfile%aryr8len
       endif
       if (present(aryr8val)) then
         if(size(aryr8val).eq.gfile%nmetaaryr8*maxval(gfile%aryr8len) ) &
           aryr8val=gfile%aryr8val
       endif
    endif

    call mpi_barrier(gfile%mpi_comm,ios)
    if ( present(iret)) iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_getfilehead
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvari(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    integer(nemsio_intkind),intent(out)           :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarinum
      if(equal_str_nocase(trim(varname),trim(gfile%headvariname(i))) ) then
           varval=gfile%headvarival(i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetavari.gt.0) then
      do i=1,gfile%nmetavari
        if(equal_str_nocase(trim(varname),trim(gfile%variname(i))) ) then
           varval=gfile%varival(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---    
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvari
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarr(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    real(nemsio_realkind),intent(out)             :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarrnum
      if(equal_str_nocase(trim(varname),trim(gfile%headvarrname(i))) ) then
           varval=gfile%headvarrval(i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetavarr.gt.0) then
      do i=1,gfile%nmetavarr
        if(equal_str_nocase(trim(varname),trim(gfile%varrname(i))) ) then
           varval=gfile%varrval(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarr
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarl(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    logical(nemsio_logickind),intent(out)         :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetavarl.gt.0) then
      do i=1,gfile%nmetavarl
        if(equal_str_nocase(trim(varname),trim(gfile%varlname(i))) ) then
           varval=gfile%varlval(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarl
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarc(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    character(*),intent(out)                      :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarcnum
      if(equal_str_nocase(trim(varname),trim(gfile%headvarcname(i))) ) then
           varval=gfile%headvarcval(i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetavarc.gt.0) then
      do i=1,gfile%nmetavarc
        if(equal_str_nocase(trim(varname),trim(gfile%varcname(i))) ) then
           varval=gfile%varcval(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarc
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarr8(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                 :: varname
    real(nemsio_dblekind),intent(out)             :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
!---
    if(gfile%nmetavarr8.gt.0) then
      do i=1,gfile%nmetavarr8
        if(equal_str_nocase(trim(varname),trim(gfile%varr8name(i))) ) then
           varval=gfile%varr8val(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif

    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarr8
!------------------------------------------------------------------------------
  subroutine nemsio_getfheadaryi(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    integer(nemsio_intkind),intent(out)           :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ios
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headaryinum
      if(equal_str_nocase(trim(varname),trim(gfile%headaryiname(i))) ) then
           varval(:)=gfile%headaryival(1:gfile%aryilen(i),i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetaaryi.gt.0) then
      do i=1,gfile%nmetaaryi
        if(equal_str_nocase(trim(varname),trim(gfile%aryiname(i))) ) then
           varval(:)=gfile%aryival(1:gfile%aryilen(i),i)
           if(present(iret) ) iret=0
           ios=0
           return
        endif
      enddo
    endif
!---    
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryi
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadaryr(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    real(nemsio_realkind),intent(out)             :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ios
!---
    if(present(iret) ) iret=-17
    if(gfile%headaryrnum>0) then
     do i=1,gfile%headaryrnum
      if(equal_str_nocase(trim(varname),trim(gfile%headaryrname(i))) ) then
           varval(:)=gfile%headaryrval(1:gfile%aryrlen(i),i)
           if(present(iret) ) iret=0
           return
      endif
     enddo
    endif
!---
    if(gfile%nmetaaryr.gt.0) then
      do i=1,gfile%nmetaaryr
        if(equal_str_nocase(trim(varname),trim(gfile%aryrname(i)))) then
           varval(:)=gfile%aryrval(1:gfile%aryrlen(i),i)
           if(present(iret) ) iret=0
           ios=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryr
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadaryl(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    logical(nemsio_logickind),intent(out)         :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ios
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetaaryl.gt.0) then
      do i=1,gfile%nmetaaryl
        if(equal_str_nocase(trim(varname),trim(gfile%arylname(i)))) then
           varval(:)=gfile%arylval(1:gfile%aryllen(i),i)
           if(present(iret) ) iret=0
           ios=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryl
!------------------------------------------------------------------------------
  subroutine nemsio_getfheadaryc(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    character(*),intent(out)                      :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ios
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetaaryc.gt.0) then
      do i=1,gfile%nmetaaryc
       if(equal_str_nocase(trim(varname),trim(gfile%headarycname(i))) ) then
           varval(:)=gfile%headarycval(1:gfile%aryclen(i),i)
           if(present(iret) ) iret=0
           return
       endif
      enddo
    endif
!---
    if(gfile%nmetaaryc.gt.0) then
      do i=1,gfile%nmetaaryc
        if(equal_str_nocase(trim(varname),trim(gfile%arycname(i)))) then
           varval(:)=gfile%arycval(1:gfile%aryclen(i),i)
           if(present(iret) ) iret=0
           ios=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryc
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadaryr8(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    real(nemsio_dblekind),intent(out)             :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ios
!---
    if(present(iret) ) iret=-17
!---
    if(gfile%nmetaaryr8.gt.0) then
      do i=1,gfile%nmetaaryr8
        if(equal_str_nocase(trim(varname),trim(gfile%aryr8name(i)))) then
           varval(:)=gfile%aryr8val(1:gfile%aryr8len(i),i)
           if(present(iret) ) iret=0
           ios=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryr8
!------------------------------------------------------------------------------

!*****************   read bin data set :  ********************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_searchrecv(gfile,jrec,name,levtyp,lev,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: search rec number giving rec name, levtyp and lev
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(out)           :: jrec
    character(*),intent(in)                      :: name, levtyp
    integer(nemsio_intkind),intent(in)            :: lev
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i, nsize

    iret=-11
    jrec=0
    do i=1,gfile%nrec
      if ( trim(name) .eq. trim(gfile%recname(i)) .and.  &
        trim(levtyp) .eq. trim(gfile%reclevtyp(i)) .and.  &
        lev .eq. gfile%reclev(i) ) then
           jrec=i
           exit
      endif
    enddo
    if ( jrec .ne.0 ) iret=0
!
    return
  end subroutine nemsio_searchrecv
!------------------------------------------------------------------------------
!
!*****************  no read grb1 data set :  **********************************
!
!------------------------------------------------------------------------------
!##############################################################################
!
!*****************   write data set :  ********************************
!
!##############################################################################
!------------------------------------------------------------------------------

!*****************   write out bin data set :  ********************************

!------------------------------------------------------------------------------
!
!***************** no write out grb data set :  ********************************
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
  subroutine nemsio_chkgfary(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: check if arrays in gfile is allocated and with right size
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)         :: gfile
    integer(nemsio_intkind),intent(out)   :: iret
    integer   :: ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    if ( gfile%dimx .eq. nemsio_intfill .or. gfile%dimy .eq. nemsio_intfill &
        .or. gfile%dimz .eq. nemsio_intfill .or. gfile%nrec .eq. nemsio_intfill &
        .or. gfile%idate(1) .eq.nemsio_intfill .or. gfile%ntrac .eq.nemsio_intfill ) then
        return
    endif
    if (.not. allocated(gfile%vcoord) .or. size(gfile%vcoord).ne. &
       (gfile%dimz+1)*3*2 ) then
       call nemsio_almeta1(gfile,ios)
       if (ios .ne. 0) return
    endif
    if (.not.allocated(gfile%lat) .or. size(gfile%lat).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%lon) .or. size(gfile%lon).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%dx) .or. size(gfile%dx).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%dy) .or. size(gfile%dy).ne.gfile%fieldsize) then
        call nemsio_almeta2(gfile,ios)
        if (ios .ne. 0) return
    endif
    if (.not.allocated(gfile%Cpi) .or. size(gfile%Cpi).ne.gfile%ntrac+1 .or. &
        .not.allocated(gfile%Ri) .or. size(gfile%Ri).ne.gfile%ntrac+1 ) then
        call nemsio_almeta3(gfile,ios)
        if (ios .ne. 0) return
    endif

    if (allocated(gfile%recname) .and. size(gfile%recname).eq.gfile%nrec)&
    then
        if (allocated(gfile%reclevtyp) .and. size(gfile%reclevtyp) &
        .eq.gfile%nrec) then
           if (allocated(gfile%reclev) .and. size(gfile%reclev).eq. &
             gfile%nrec) then
               iret=0
               return
           endif
         endif
   endif
   call  nemsio_almeta4(gfile,ios)
   if (ios .ne. 0) return
   iret=0
  end subroutine nemsio_chkgfary
!------------------------------------------------------------------------------
  subroutine nemsio_almeta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate all the arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile 
    integer(nemsio_intkind),intent(out)  :: iret
    integer ::dimvcoord1,dimvcoord2,dimnmmlev
    integer ::dimrecname,dimreclevtyp,dimreclev
    integer ::dimfield
    integer ::dimcpr
    integer ::iret1,iret2,iret3,iret4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimvcoord1=gfile%dimz+1
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    dimfield=gfile%fieldsize
    dimcpr=gfile%ntrac+1
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
    allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), &
             stat=iret1)
    allocate(gfile%vcoord(dimvcoord1,3,2) ,stat=iret2) 
    allocate(gfile%lat(dimfield), gfile%lon(dimfield), &
             gfile%dx(dimfield), gfile%dy(dimfield) ,stat=iret3)
    allocate(gfile%Cpi(dimcpr), gfile%Ri(dimcpr), stat=iret4)

    iret=abs(iret1)+abs(iret2)+abs(iret3)+abs(iret4)
    if(iret.eq.0) then
      gfile%reclev=nemsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
      gfile%vcoord=nemsio_realfill
      gfile%lat=nemsio_realfill
      gfile%lon=nemsio_realfill
      gfile%dx=nemsio_realfill
      gfile%dy=nemsio_realfill
      gfile%Cpi=nemsio_realfill
      gfile%Ri=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta
!------------------------------------------------------------------------------
  subroutine nemsio_alextrameta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate all the arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer ::iret1,iret2,iret3,iret4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    if(gfile%extrameta) then
!      print *,'nmetavari=',gfile%nmetavari,'nmetavarr=',gfile%nmetavarr, &
!              'nmetavarl=',gfile%nmetavarl,'nmetavarc=',gfile%nmetavarc, &
!              'nmetaaryi=',gfile%nmetaaryi,'nmetaaryr=',gfile%nmetaaryi, &
!              'nmetaaryl=',gfile%nmetaaryl,'nmetaaryc=',gfile%nmetaaryc
      if(gfile%nmetavari.gt.0) then
         if(allocated(gfile%variname)) deallocate(gfile%variname)
         if(allocated(gfile%varival)) deallocate(gfile%varival)
         allocate(gfile%variname(gfile%nmetavari), &
                  gfile%varival(gfile%nmetavari), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarr.gt.0) then
         if(allocated(gfile%varrname)) deallocate(gfile%varrname)
         if(allocated(gfile%varrval)) deallocate(gfile%varrval)
         allocate(gfile%varrname(gfile%nmetavarr), &
                  gfile%varrval(gfile%nmetavarr), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarl.gt.0) then
         if(allocated(gfile%varlname)) deallocate(gfile%varlname)
         if(allocated(gfile%varlval)) deallocate(gfile%varlval)
         allocate(gfile%varlname(gfile%nmetavarl), &
                  gfile%varlval(gfile%nmetavarl), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarc.gt.0) then
         if(allocated(gfile%varcname)) deallocate(gfile%varcname)
         if(allocated(gfile%varcval)) deallocate(gfile%varcval)
         allocate(gfile%varcname(gfile%nmetavarc), &
                  gfile%varcval(gfile%nmetavarc), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarr8.gt.0) then
         if(allocated(gfile%varr8name)) deallocate(gfile%varr8name)
         if(allocated(gfile%varr8val)) deallocate(gfile%varr8val)
         allocate(gfile%varr8name(gfile%nmetavarr8), &
                  gfile%varr8val(gfile%nmetavarr8), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryi.gt.0) then
         if(allocated(gfile%aryiname)) deallocate(gfile%aryiname)
         if(allocated(gfile%aryilen)) deallocate(gfile%aryilen)
         if(allocated(gfile%aryival)) deallocate(gfile%aryival)
         allocate(gfile%aryiname(gfile%nmetaaryi), &
                  gfile%aryilen(gfile%nmetaaryi), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryr.gt.0) then
         if(allocated(gfile%aryrname)) deallocate(gfile%aryrname)
         if(allocated(gfile%aryrlen)) deallocate(gfile%aryrlen)
         if(allocated(gfile%aryrval)) deallocate(gfile%aryrval)
         allocate(gfile%aryrname(gfile%nmetaaryr), &
                  gfile%aryrlen(gfile%nmetaaryr), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryl.gt.0) then
         if(allocated(gfile%arylname)) deallocate(gfile%arylname)
         if(allocated(gfile%aryllen)) deallocate(gfile%aryllen)
         if(allocated(gfile%arylval)) deallocate(gfile%arylval)
         allocate(gfile%arylname(gfile%nmetaaryl), &
                  gfile%aryllen(gfile%nmetaaryl), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryc.gt.0) then
         if(allocated(gfile%arycname)) deallocate(gfile%arycname)
         if(allocated(gfile%aryclen)) deallocate(gfile%aryclen)
         if(allocated(gfile%arycval)) deallocate(gfile%arycval)
         allocate(gfile%arycname(gfile%nmetaaryc), &
                  gfile%aryclen(gfile%nmetaaryc), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryr8.gt.0) then
         if(allocated(gfile%aryr8name)) deallocate(gfile%aryr8name)
         if(allocated(gfile%aryr8len)) deallocate(gfile%aryr8len)
         if(allocated(gfile%aryr8val)) deallocate(gfile%aryr8val)
         allocate(gfile%aryr8name(gfile%nmetaaryr8), &
                  gfile%aryr8len(gfile%nmetaaryr8), stat=iret1 )
         if(iret1.ne.0) return
      endif
    endif

    iret=0
  end subroutine nemsio_alextrameta
!------------------------------------------------------------------------------
  subroutine nemsio_almeta1(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate vcoord in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dimvcoord1,dimnmmlev,dimnmmnsoil
    integer :: dimgsilev
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimvcoord1=gfile%dimz+1
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    allocate(gfile%vcoord(dimvcoord1,3,2), stat=iret)
    if(iret.eq.0) then
      gfile%vcoord=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta1
!------------------------------------------------------------------------------
  subroutine nemsio_almeta2(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lat1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dimlat
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimlat=gfile%fieldsize
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    allocate(gfile%lat(dimlat),gfile%lon(dimlat), &
             gfile%dx(dimlat),gfile%dy(dimlat), stat=iret)
    if(iret.eq.0) then
      gfile%lat=nemsio_realfill
      gfile%lon=nemsio_realfill
      gfile%dx=nemsio_realfill
      gfile%dy=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta2
!------------------------------------------------------------------------------
  subroutine nemsio_almeta3(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lon1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dim1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1d=gfile%ntrac+1
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
    allocate(gfile%Cpi(dim1d),gfile%Ri(dim1d),stat=iret)
    if(iret.eq.0) then
       gfile%Cpi=nemsio_realfill
       gfile%Ri=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta3
!------------------------------------------------------------------------------
  subroutine nemsio_almeta4(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate recnam, reclvevtyp, and reclev in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dimrecname,dimreclevtyp,dimreclev
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), stat=iret)
    if(iret.eq.0) then
      gfile%reclev=nemsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta4
!------------------------------------------------------------------------------
  subroutine nemsio_axmeta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: empty gfile variables and decallocate arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)      :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    gfile%gtype=' '
    gfile%gdatatype=' '
    gfile%modelname=' '
    gfile%version=nemsio_intfill
    gfile%nmeta=nemsio_intfill
    gfile%lmeta=nemsio_intfill
    gfile%nrec=nemsio_intfill
    gfile%idate(1:7)=nemsio_intfill
    gfile%nfday=nemsio_intfill
    gfile%nfhour=nemsio_intfill
    gfile%nfminute=nemsio_intfill
    gfile%nfsecondn=nemsio_intfill
    gfile%nfsecondd=nemsio_intfill
    gfile%dimx=nemsio_intfill
    gfile%dimy=nemsio_intfill
    gfile%dimz=nemsio_intfill
    gfile%nframe=nemsio_intfill
    gfile%nsoil=nemsio_intfill
    gfile%ntrac=nemsio_intfill
    gfile%jcap=nemsio_intfill
    gfile%ncldt=nemsio_intfill
    gfile%idvc=nemsio_intfill
    gfile%idsl=nemsio_intfill
    gfile%idvm=nemsio_intfill
    gfile%idrt=nemsio_intfill
    gfile%rlon_min=nemsio_realfill
    gfile%rlon_max=nemsio_realfill
    gfile%rlat_min=nemsio_realfill
    gfile%rlat_max=nemsio_realfill
    gfile%extrameta=nemsio_logicfill
    gfile%nmetavari=nemsio_intfill
    gfile%nmetavarr=nemsio_intfill
    gfile%nmetavarl=nemsio_intfill
    gfile%nmetavarc=nemsio_intfill
    gfile%nmetaaryi=nemsio_intfill
    gfile%nmetaaryr=nemsio_intfill
    gfile%nmetaaryl=nemsio_intfill
    gfile%nmetaaryc=nemsio_intfill

    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
!
    gfile%mbuf=0
    gfile%nnum=0
    gfile%nlen=0
    gfile%mnum=0
    if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
    if(allocated(gfile%headvariname)) deallocate(gfile%headvariname)
    if(allocated(gfile%headvarrname)) deallocate(gfile%headvarrname)
    if(allocated(gfile%headvarlname)) deallocate(gfile%headvarlname)
    if(allocated(gfile%headvarcname)) deallocate(gfile%headvarcname)
    if(allocated(gfile%headvarival)) deallocate(gfile%headvarival)
    if(allocated(gfile%headvarrval)) deallocate(gfile%headvarrval)
    if(allocated(gfile%headvarlval)) deallocate(gfile%headvarlval)
    if(allocated(gfile%headvarcval)) deallocate(gfile%headvarcval)
    if(allocated(gfile%headaryiname)) deallocate(gfile%headaryiname)
    if(allocated(gfile%headaryrname)) deallocate(gfile%headaryrname)
    if(allocated(gfile%headarycname)) deallocate(gfile%headarycname)
    if(allocated(gfile%headaryival)) deallocate(gfile%headaryival)
    if(allocated(gfile%headaryrval)) deallocate(gfile%headaryrval)
    if(allocated(gfile%headarycval)) deallocate(gfile%headarycval)
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_axmeta
!------------------------------------------------------------------------------
  subroutine nemsio_setfhead(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: required file header (default)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer(nemsio_intkind) i,j,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-17
    gfile%headvarinum=29
    gfile%headvarrnum=4
    gfile%headvarlnum=1
    gfile%headvarcnum=3
    gfile%headaryinum=2
    gfile%headaryrnum=7
    gfile%headarycnum=2
!
    allocate(gfile%headvariname(gfile%headvarinum),gfile%headvarival(gfile%headvarinum) )
    gfile%headvariname(1)='version'
    gfile%headvarival(1)=gfile%version
    gfile%headvariname(2)='nmeta'
    gfile%headvarival(2)=gfile%nmeta
    gfile%headvariname(3)='lmeta'
    gfile%headvarival(3)=gfile%lmeta
    gfile%headvariname(4)='nrec'
    gfile%headvarival(4)=gfile%nrec
    gfile%headvariname(5)='nfday'
    gfile%headvarival(5)=gfile%nfday
    gfile%headvariname(6)='nfhour'
    gfile%headvarival(6)=gfile%nfhour
    gfile%headvariname(7)='nfminute'
    gfile%headvarival(7)=gfile%nfminute
    gfile%headvariname(8)='nfsecondn'
    gfile%headvarival(8)=gfile%nfsecondn
    gfile%headvariname(9)='nfsecondd'
    gfile%headvarival(9)=gfile%nfsecondd
    gfile%headvariname(10)='dimx'
    gfile%headvarival(10)=gfile%dimx
    gfile%headvariname(11)='dimy'
    gfile%headvarival(11)=gfile%dimy
    gfile%headvariname(12)='dimz'
    gfile%headvarival(12)=gfile%dimz
    gfile%headvariname(13)='nframe'
    gfile%headvarival(13)=gfile%nframe
    gfile%headvariname(14)='nsoil'
    gfile%headvarival(14)=gfile%nsoil
    gfile%headvariname(15)='ntrac'
    gfile%headvarival(15)=gfile%ntrac
    gfile%headvariname(16)='jcap'
    gfile%headvarival(16)=gfile%jcap
    gfile%headvariname(17)='ncldt'
    gfile%headvarival(17)=gfile%ncldt
    gfile%headvariname(18)='idvc'
    gfile%headvarival(18)=gfile%idvc
    gfile%headvariname(19)='idsl'
    gfile%headvarival(19)=gfile%idsl
    gfile%headvariname(20)='idvm'
    gfile%headvarival(20)=gfile%idvm
    gfile%headvariname(21)='idrt'
    gfile%headvarival(21)=gfile%idrt
    gfile%headvariname(22)='nmetavari'
    gfile%headvarival(22)=gfile%nmetavari
    gfile%headvariname(23)='nmetavarr'
    gfile%headvarival(23)=gfile%nmetavarr
    gfile%headvariname(24)='nmetavarl'
    gfile%headvarival(24)=gfile%nmetavarl
    gfile%headvariname(25)='nmetavarc'
    gfile%headvarival(25)=gfile%nmetavarc
    gfile%headvariname(26)='nmetaaryi'
    gfile%headvarival(26)=gfile%nmetaaryi
    gfile%headvariname(27)='nmetaaryr'
    gfile%headvarival(27)=gfile%nmetaaryr
    gfile%headvariname(28)='nmetaaryl'
    gfile%headvarival(28)=gfile%nmetaaryl
    gfile%headvariname(29)='nmetaaryc'
    gfile%headvarival(29)=gfile%nmetaaryc
!
    allocate(gfile%headvarrname(gfile%headvarrnum),gfile%headvarrval(gfile%headvarrnum) )
    gfile%headvarrname(1)='rlon_min'
    gfile%headvarrval(1)=gfile%rlon_min
    gfile%headvarrname(2)='rlon_max'
    gfile%headvarrval(2)=gfile%rlon_max
    gfile%headvarrname(3)='rlat_min'
    gfile%headvarrval(3)=gfile%rlat_min
    gfile%headvarrname(4)='rlat_min'
    gfile%headvarrval(4)=gfile%rlat_min
!
    allocate(gfile%headvarcname(gfile%headvarcnum),gfile%headvarcval(gfile%headvarcnum) )
    gfile%headvarcname(1)='gtype'
    gfile%headvarcval(1)=gfile%gtype
    gfile%headvarcname(2)='modelname'
    gfile%headvarcval(2)=gfile%modelname
    gfile%headvarcname(3)='gdatatype'
    gfile%headvarcval(3)=gfile%gdatatype
!head logic var
!    write(0,*)'before setfhead, headvarl,nrec=',gfile%nrec 
    allocate(gfile%headvarlname(gfile%headvarlnum),gfile%headvarlval(gfile%headvarlnum) )
    gfile%headvarlname(1)='extrameta'
    gfile%headvarlval(1)=gfile%extrameta
!
!--- gfile%head int ary
!    write(0,*)'before setfhead, headaryi,nrec=',gfile%nrec,gfile%headaryinum
    allocate(gfile%headaryiname(gfile%headaryinum) )
    allocate(gfile%headaryival(max(size(gfile%reclev),7),gfile%headaryinum))
    gfile%headaryiname(1)='idate'
    gfile%headaryival(1:7,1)=gfile%idate(1:7)
    gfile%headaryiname(2)='reclev'
    if(allocated(gfile%reclev)) gfile%headaryival(:,2)=gfile%reclev(:)
!
!--- gfile%head real ary
!    write(0,*)'before setfhead, headaryr,',gfile%headaryrnum ,gfile%fieldsize
    allocate(gfile%headaryrname(gfile%headaryrnum) )
    allocate(gfile%headaryrval(max(gfile%fieldsize,(gfile%dimz+1)*6),gfile%headaryrnum))
    gfile%headaryrname(1)='vcoord'
    if(allocated(gfile%vcoord)) then
    do j=1,2
     do i=1,3
      do k=1,gfile%dimz+1
       gfile%headaryrval(k+((j-1)*3+i-1)*(gfile%dimz+1),1)=gfile%vcoord(k,i,j)
      enddo
     enddo
    enddo
    endif
    gfile%headaryrname(2)='lat'
    if(allocated(gfile%lat)) gfile%headaryrval(:,2)=gfile%lat
    gfile%headaryrname(3)='lon'
    if(allocated(gfile%lon)) gfile%headaryrval(:,3)=gfile%lon
    gfile%headaryrname(4)='dx'
    if(allocated(gfile%dx)) gfile%headaryrval(:,4)=gfile%dx
    gfile%headaryrname(5)='dy'
    if(allocated(gfile%dy)) gfile%headaryrval(:,5)=gfile%dy
    gfile%headaryrname(6)='cpi'
    if(allocated(gfile%cpi)) gfile%headaryrval(1:size(gfile%cpi),6)=gfile%cpi(:)
    gfile%headaryrname(7)='ri'
    if(allocated(gfile%ri)) gfile%headaryrval(1:size(gfile%ri),7)=gfile%ri(:)
!
!--- gfile%head char var
!    write(0,*)'before setfhead, headaryc,nrec=',gfile%nrec,gfile%headarycnum
    allocate(gfile%headarycname(gfile%headarycnum) )
    if(size(gfile%recname)>0) then
      allocate(gfile%headarycval(size(gfile%recname),gfile%headarycnum))
      gfile%headarycname(1)='recname'
      if(allocated(gfile%recname)) gfile%headarycval(:,1)=gfile%recname
      gfile%headarycname(2)='reclevtyp'
      if(allocated(gfile%reclevtyp)) gfile%headarycval(:,2)=gfile%reclevtyp
    endif
!
!    write(0,*)'end ef nemsio_setfhead'
    iret=0
  end subroutine nemsio_setfhead
!------------------------------------------------------------------------------
  subroutine nemsio_getrechead(gfile,jrec,name,levtyp,lev,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: given record number, return users record name, lev typ, and levs
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    integer(nemsio_intkind),intent(in)           :: jrec
    character*(*),intent(out)                   :: name,levtyp
    integer(nemsio_intkind),intent(out)          :: lev
    integer(nemsio_intkind),optional,intent(out) :: iret
    integer :: ios
! - - - - - - - - - - - - - -  - - - - - - - -  - - - - - - - - - - - - - - - -
    if( present(iret)) iret=-6
    if ( jrec.gt.0 .or. jrec.le.gfile%nrec) then
      name=gfile%recname(jrec)
      levtyp=gfile%reclevtyp(jrec)
      lev=gfile%reclev(jrec)
      if(present(iret)) iret=0
      return
    else
      if ( present(iret))  then
       return
      else
        call nemsio_stop
      endif
    endif
  end subroutine nemsio_getrechead
!------------------------------------------------------------------------------
  subroutine nemsio_gfinit(gfile,iret,recname,reclevtyp,reclev)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set gfile variables to operational model output
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    character(nemsio_charkind),optional,intent(in)  :: recname(:)
    character(nemsio_charkind*2),optional,intent(in):: reclevtyp(:)
    integer(nemsio_intkind),optional,intent(in)     :: reclev(:)
    integer  :: i,j,rec,rec3dopt
    real(nemsio_dblekind),allocatable :: slat(:),wlat(:)
    real(nemsio_dblekind),allocatable :: dx(:)
    real(nemsio_dblekind)             :: radi
    logical ::linit
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! set operational format
!
    iret=-8
    gfile%version=200809
    gfile%nfday=0
    gfile%nfhour=0
    gfile%nfminute=0
    gfile%nfsecondn=0
    gfile%nfsecondd=100
    gfile%extrameta=.false.
    gfile%nmetavari=0
    gfile%nmetavarr=0
    gfile%nmetavarl=0
    gfile%nmetavarc=0
    gfile%nmetaaryi=0
    gfile%nmetaaryr=0
    gfile%nmetaaryl=0
    gfile%nmetaaryc=0
!
   iret=0
  end subroutine nemsio_gfinit
!------------------------------------------------------------------------------
  subroutine nemsio_stop(message)
    implicit none
    character(*),optional,intent(in) :: message
    integer ::ios
!---
     if ( present(message) ) print *,'message'
     call mpi_finalize(ios)
     stop
!
  end subroutine nemsio_stop
!------------------------------------------------------------------------------
!
    subroutine nemsio_denseread4(gfile,ista,iend,jsta,jend,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: ista,iend,jsta,jend
     real(nemsio_realkind),intent(out)   :: data(:)
     integer,optional,intent(out)        :: iret
!--- local vars
     integer                  :: status(MPI_STATUS_SIZE)
     integer                  :: fieldmapsize,nfld,nfldloop,mfldrmd
     integer,allocatable      :: fieldmap(:)
     integer ios,i,j,nfldsize,fieldmapsize1,k,nstt,nend
     integer(8) idispstt
     real(nemsio_dblekind),allocatable :: tmp(:)
!---
     iret=-25
!
     if(size(data)/=(iend-ista+1)*(jend-jsta+1)*gfile%nrec) then
       print *,'WRONG: data size ',size(data),' doesn"t match total subdomain data size', &
         (iend-ista+1)*(jend-jsta+1)*gfile%nrec
       return
     endif
!--- set nfld
    if(gfile%gdatatype(1:4).eq.'bin4') then
        nfldsize=gfile%fieldsize+2
     elseif (gfile%gdatatype(1:4).eq.'bin8') then
        nfldsize=gfile%fieldsize+1
     endif
     nfld=min(gfile%nrec,nemsio_maxint/nfldsize)
     nfldloop=(gfile%nrec-1)/nfld+1
     mfldrmd=mod(gfile%nrec,nfld)
!     print *,'in dense read,nfld=',nfld,'nfldloop=',nfldloop, &
!       'mfldrmd=',mfldrmd
!--- set file map
     fieldmapsize=(iend-ista+1)*(jend-jsta+1)*nfld
     allocate(fieldmap(fieldmapsize) )
     call set_mpimap_read(gfile,ista,iend,jsta,jend,fieldmap,ios)
     if(ios.ne.0) return
!---
     do k=1,nfldloop
!
       if(k<nfldloop.or.mfldrmd==0) then
         nstt=(k-1)*fieldmapsize+1
         nend=k*fieldmapsize
       elseif(mfldrmd/=0) then
         nstt=(k-1)*fieldmapsize+1
         nend=gfile%nrec*(iend-ista+1)*(jend-jsta+1)
         deallocate(fieldmap)
         fieldmapsize=(iend-ista+1)*(jend-jsta+1)*mfldrmd
         allocate(fieldmap(fieldmapsize) )
!          print *,'bf set_mpa_read,size=',fieldmapsize,'mfldrmd=',mfldrmd
         call set_mpimap_read(gfile,ista,iend,jsta,jend,fieldmap,ios)
         if(ios.ne.0) return
       endif

       if(gfile%gdatatype(1:4)=='bin4') then
         idispstt=int(k-1,8)*int(nfld,8)*int(gfile%fieldsize*4+8,8)
         call readmpi4(gfile,fieldmapsize,fieldmap,data(nstt:nend),ios,idispstt)
       else if (gfile%gdatatype(1:4)=='bin8') then
         allocate(tmp(size(data)))
         idispstt=int(k-1,8)*int(nfld,8)*int(gfile%fieldsize*8+8,8)
         call readmpi8(gfile,fieldmapsize,fieldmap,tmp(nstt:nend),ios,idispstt)
         data=tmp
         deallocate(tmp)
       endif
       if(ios.ne.0) return
!
     enddo
     if(allocated(fieldmap))deallocate(fieldmap)
!
     iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    end subroutine nemsio_denseread4
!------------------------------------------------------------------------------
    subroutine nemsio_denseread8(gfile,ista,iend,jsta,jend,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read all the fields out in real 8 MPI
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: ista,iend,jsta,jend
     real(nemsio_dblekind),intent(out)  :: data(:)
     integer,optional,intent(out)        :: iret
!--- local vars
     integer                  :: fieldmapsize
     integer,allocatable      :: fieldmap(:)
     integer ios,i,j,nfldsize,nfld,nfldloop,mfldrmd,k,nstt,nend
     integer(8) idispstt
     real(nemsio_realkind),allocatable :: tmp(:)
!---
     iret=-25
!
     if(size(data)/=(iend-ista+1)*(jend-jsta+1)*gfile%nrec) then
       print *,'WRONG: data size ',size(data),' doesn"t match total subdomain data size', &
         (iend-ista+1)*(jend-jsta+1)*gfile%nrec
       return
     endif
!--- set nfld
    if(gfile%gdatatype(1:4).eq.'bin4') then
        nfldsize=gfile%fieldsize+2
     elseif (gfile%gdatatype(1:4).eq.'bin8') then
        nfldsize=gfile%fieldsize+1
     endif
     nfld=min(gfile%nrec,nemsio_maxint/nfldsize)
     nfldloop=(gfile%nrec-1)/nfld+1
     mfldrmd=mod(gfile%nrec,nfld)
!     write(0,*)'in dense read,nfld=',nfld,'nfldloop=',nfldloop, &
!       'mfldrmd=',mfldrmd
!--- set file map
     fieldmapsize=(iend-ista+1)*(jend-jsta+1)*nfld
     allocate(fieldmap(fieldmapsize) )
     call set_mpimap_read(gfile,ista,iend,jsta,jend,fieldmap,ios)
     if(ios.ne.0) return
!---
     do k=1,nfldloop
!
       if(k<nfldloop.or.mfldrmd==0) then
         nstt=(k-1)*fieldmapsize+1
         nend=k*fieldmapsize
       elseif(mfldrmd/=0) then
         nstt=(k-1)*fieldmapsize+1
         nend=gfile%nrec*(iend-ista+1)*(jend-jsta+1)
         deallocate(fieldmap)
         fieldmapsize=(iend-ista+1)*(jend-jsta+1)*mfldrmd
         allocate(fieldmap(fieldmapsize) )
         call set_mpimap_read(gfile,ista,iend,jsta,jend,fieldmap,ios)
         if(ios.ne.0) return
       endif
!
!---
       if(gfile%gdatatype(1:4)=='bin4') then
         allocate(tmp(size(data)))
         idispstt=int(k-1,8)*int(nfld,8)*int(gfile%fieldsize*4+8,8)
         call readmpi4(gfile,fieldmapsize,fieldmap,tmp,ios,idispstt)
         data=tmp
         deallocate(tmp)
       elseif(gfile%gdatatype(1:4)=='bin8') then
         idispstt=int(k-1,8)*int(nfld,8)*int(gfile%fieldsize*8+8,8)
         call readmpi8(gfile,fieldmapsize,fieldmap,data,ios,idispstt)
       endif
       if(ios.ne.0) return
     enddo
     if(allocated(fieldmap))deallocate(fieldmap)
!
     iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine nemsio_denseread8
!------------------------------------------------------------------------------
   subroutine readmpi4(gfile,fieldmapsize,fieldmap,data,iret,idispstt)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read real 4 data out using MPI
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: fieldmapsize
     integer,intent(in)                  :: fieldmap(fieldmapsize)
     real(nemsio_realkind),intent(out)   :: data(fieldmapsize)
     integer,optional,intent(out)        :: iret
     integer(8),optional,intent(in)        :: idispstt
!local vars
     integer(MPI_OFFSET_KIND) :: idisp
     integer                  :: filetype,ios
     integer                  :: status(MPI_STATUS_SIZE)
!
!--- set file type
     call mpi_type_create_indexed_block(fieldmapsize,1,fieldmap, &
            MPI_REAL,filetype,ios)

     call MPI_TYPE_COMMIT(filetype,iret)
     if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop('stop at MPI set field map!')
       endif
     endif
!
!--- file set view, and read
     if(present(idispstt)) then
       idisp=gfile%tlmeta+4+idispstt
     else
       idisp=gfile%tlmeta+4
     endif
     call mpi_file_set_view(gfile%fh,idisp,MPI_REAL4,filetype,'native', &
       MPI_INFO_NULL,ios)
     call MPI_FILE_READ_ALL(gfile%fh,data,fieldmapsize,MPI_REAL4,  &
       status,ios)
     if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop('stop at MPI read file all for bin4!')
       endif
     endif
     if(gfile%do_byteswap) call byteswap(data,nemsio_realkind,size(data))
!
      iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine readmpi4
!------------------------------------------------------------------------------
   subroutine readmpi8(gfile,fieldmapsize,fieldmap,data,iret,idispstt)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: fieldmapsize
     integer,intent(in)                  :: fieldmap(fieldmapsize)
     real(nemsio_dblekind),intent(out)   :: data(fieldmapsize)
     integer,optional,intent(out)        :: iret
     integer(8),optional,intent(in)        :: idispstt
!local vars
     integer(MPI_OFFSET_KIND) :: idisp
     integer                  :: filetype,ios
     integer                  :: status(MPI_STATUS_SIZE)
!---
     iret=-25
!
!--- set file type
     call mpi_type_create_indexed_block(fieldmapsize,1,fieldmap, &
          MPI_REAL8,filetype,ios)
     call MPI_TYPE_COMMIT(filetype,iret)
     if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop('stop at MPI set field map!')
       endif
     endif
!
!--- file set view, and read
     if(present(idispstt)) then
       idisp=gfile%tlmeta+4+idispstt
     else
       idisp=gfile%tlmeta+4
     endif
     call mpi_file_set_view(gfile%fh,idisp,MPI_REAL8,filetype,'native', &
       MPI_INFO_NULL,ios)
     call MPI_FILE_READ_ALL(gfile%fh,data,fieldmapsize,MPI_REAL8,  &
       status,ios)
     if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop('stop at MPI read file all for bin8!')
       endif
     endif
!
      iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    end subroutine readmpi8
!------------------------------------------------------------------------------
    subroutine set_mpimap_read(gfile,ista,iend,jsta,jend,fieldmap,iret,jrec)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(in)     :: gfile
     integer,intent(in)                :: ista,iend,jsta,jend
     integer,intent(out)            :: fieldmap(:)
     integer,intent(out)               :: iret
     integer,optional,intent(in)       :: jrec
!-- local vars
     integer i,j,k,m,jm,km,nfieldsize,nfld,krec,kstart
!---
     iret=-20
!---
     if(gfile%gdatatype(1:4).eq.'bin4') then
        nfieldsize=gfile%fieldsize+2
     elseif (gfile%gdatatype(1:4).eq.'bin8') then
        nfieldsize=gfile%fieldsize+1
     endif
!---
     if(present(jrec)) then
       krec=jrec
       nfld=1
     else
       krec=1
       nfld=size(fieldmap)/((iend-ista+1)*(jend-jsta+1))
     endif
!--- set file map
     kstart=(krec-1)*nfieldsize
!     write(0,*)'in set_mpimap, kstart=',kstart,' tlmeta=',gfile%tlmeta,  &
!      ' nfieldsize=',nfieldsize,'krec=',krec,'nfld=',nfld,'fldsize=',gfile%fieldsize, &
!      'dimx=',gfile%dimx,'dimy=',gfile%dimy,'nfrmae=',gfile%nframe
!
     if (gfile%nframe.eq.0) then
       m=0
       do k=1,nfld
         km=(k-1)*nfieldsize+kstart-1
         do j=jsta,jend
           jm=(j-1)*gfile%dimx
           do i=ista,iend
             m=m+1
             fieldmap(m)=i+jm+km
           enddo
         enddo
       enddo
     else if(gfile%nframe.gt.0) then
       m=0
       do k=1,nfld
         km=(k-1)*nfieldsize+kstart-1
         do j=jsta,jend
           jm=(j-1)*(gfile%dimx+2*gfile%nframe)
           do i=ista,iend
             m=m+1
             fieldmap(m)=i+jm+km
           enddo
         enddo
       enddo
     endif
!
     iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine set_mpimap_read
!------------------------------------------------------------------------------
   subroutine set_mpimap_wrt(gfile,ista,iend,jsta,jend,fieldmap,iret,jrec)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(in)     :: gfile
     integer,intent(in)                :: ista,iend,jsta,jend
     integer,intent(out)               :: fieldmap(:)
     integer,intent(out)               :: iret
     integer,optional,intent(in)       :: jrec
!-- local vars
     integer i,j,k,m,jm,km,nfieldsize,nfld,krec,kstart,inum
!---
     iret=-20
!---
     if(present(jrec)) then
       krec=jrec
       nfld=1
     else
       krec=1
       if(gfile%mype==gfile%lead_task) then
         nfld=size(fieldmap)/((iend-ista+1)*(jend-jsta+1)+2)
       else
         nfld=size(fieldmap)/((iend-ista+1)*(jend-jsta+1))
       endif
     endif
!--- set file map
     nfieldsize=gfile%fieldsize+2
     kstart=(krec-1)*nfieldsize
!

     if (gfile%nframe.eq.0) then
       inum=gfile%dimx
     elseif(gfile%nframe.gt.0) then
       inum=gfile%dimx+2*gfile%nframe
     endif
!
     m=0
     do k=1,nfld
         km=(k-1)*nfieldsize+kstart
         if(gfile%mype.eq.gfile%lead_task) then
           m=m+1
           fieldmap(m)=km
         endif
         do j=jsta,jend
             jm=(j-1)*inum
             do i=ista,iend
               m=m+1
               fieldmap(m)=i+jm+km
             enddo
         enddo
         if(gfile%mype.eq.gfile%lead_task) then
           m=m+1
           fieldmap(m)=km+nfieldsize-1
         endif
     enddo
!
     iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine set_mpimap_wrt
!------------------------------------------------------------------------------
   subroutine nemsio_densewrite4(gfile,ista,iend,jsta,jend,data,jrecs,jrece,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: ista,iend,jsta,jend
     real(nemsio_realkind),intent(in)    :: data(:)
     integer,optional,intent(in)         :: jrecs,jrece
     integer,optional,intent(out)        :: iret
!
     real(nemsio_dblekind),allocatable   :: data8(:)
!
     if(gfile%gdatatype(1:4)=='bin4') then
      call  mpi_densewrite4(gfile,ista,iend,jsta,jend,data,jrecs,jrece,iret)
     else if (gfile%gdatatype(1:4)=='bin8') then
      allocate(data8(size(data)))
      data8=data
      call  mpi_densewrite8(gfile,ista,iend,jsta,jend,data8,jrecs,jrece,iret)
      deallocate(data8)
     endif
!
   end subroutine nemsio_densewrite4
!------------------------------------------------------------------------------
   subroutine nemsio_densewrite8(gfile,ista,iend,jsta,jend,data,jrecs,jrece,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: ista,iend,jsta,jend
     real(nemsio_dblekind),intent(in)    :: data(:)
     integer,optional,intent(in)         :: jrecs,jrece
     integer,optional,intent(out)        :: iret
!
     real(nemsio_realkind),allocatable   :: data4(:)
!
     if(gfile%gdatatype(1:4)=='bin4') then
      allocate(data4(size(data)))
      data4=data
      call  mpi_densewrite4(gfile,ista,iend,jsta,jend,data4,jrecs,jrece,iret)
      deallocate(data4)
     else if (gfile%gdatatype(1:4)=='bin8') then
      call  mpi_densewrite8(gfile,ista,iend,jsta,jend,data,jrecs,jrece,iret)
     endif
!
   end subroutine nemsio_densewrite8
!
!------------------------------------------------------------------------------
   subroutine mpi_densewrite4(gfile,ista,iend,jsta,jend,data,jrecs,jrece,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: ista,iend,jsta,jend
     real(nemsio_realkind),intent(in)    :: data(:)
     integer,optional,intent(in)         :: jrecs,jrece
     integer,optional,intent(out)        :: iret
!--- local vars
     integer                  :: i,ios,nfldsize,nfld,nfldloop,mfldrmd,k
     integer               :: fieldmapsize,fldmapsize1,fldmapsize
     integer,allocatable      :: fieldmap(:)
     real(nemsio_realkind),allocatable      :: datatmp(:)
     integer irecs,irece,nfldlp,mrec,mrecs,filetype
     integer(8) idispstt,fielddatasize
!---
     iret=-25
!
!--- set nfld
     if(present(jrecs).and.present(jrece)) then
       mrec=jrece-jrecs+1
       mrecs=jrecs
     else
       mrec=gfile%nrec
       mrecs=1
     endif
     nfldsize=gfile%fieldsize+2
     nfld=min(mrec,nemsio_maxint/(nfldsize*2))
     nfldloop=(mrec-1)/nfld+1
     mfldrmd=mod(mrec,nfld)
!
!--- set file map
     if(gfile%mype==gfile%lead_task) then
      fieldmapsize=((iend-ista+1)*(jend-jsta+1)+2)*nfld
      fldmapsize=(iend-ista+1)*(jend-jsta+1)+2
      fldmapsize1=(iend-ista+1)*(jend-jsta+1)
     else
      fldmapsize=(iend-ista+1)*(jend-jsta+1)
      fieldmapsize=(iend-ista+1)*(jend-jsta+1)*nfld
     endif
     allocate(datatmp(fieldmapsize))
     allocate(fieldmap(fieldmapsize) )
     call set_mpimap_wrt(gfile,ista,iend,jsta,jend,fieldmap,ios)
     if(ios.ne.0) return
!
!--- set file type
     call mpi_type_create_indexed_block(fieldmapsize,1,fieldmap, &
          MPI_REAL,filetype,ios)
     call MPI_TYPE_COMMIT(filetype,ios)
     if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop('stop: at write set type indexed block')
       endif
     endif
!
!---
     do k=1,nfldloop
!
       irecs=(k-1)*nfld
       if(k<nfldloop.or.mfldrmd==0) then
         irece=k*nfld
         nfldlp=nfld
       elseif(mfldrmd/=0) then
         deallocate(fieldmap,datatmp)
         fieldmapsize=fldmapsize*mfldrmd
         allocate(fieldmap(fieldmapsize) )
         allocate(datatmp(fieldmapsize) )
         call set_mpimap_wrt(gfile,ista,iend,jsta,jend,fieldmap,ios)
         if(ios.ne.0) return
!
!--- set file type
         call mpi_type_create_indexed_block(fieldmapsize,1,fieldmap, &
           MPI_REAL,filetype,ios)
         call MPI_TYPE_COMMIT(filetype,ios)
         if ( ios.ne.0 ) then
          if ( present(iret))  then
            iret=ios
            return
          else
            call nemsio_stop('stop: at write set type indexed block')
          endif
         endif

         irece=mrec
         nfldlp=mfldrmd
       endif
!
!--- prepare data
       do i=1,nfldlp
        if(gfile%mype.eq.gfile%lead_task) then
          datatmp((i-1)*fldmapsize+1)=gfile%fieldsize_real4
          datatmp(i*fldmapsize)=datatmp(1)
          datatmp((i-1)*fldmapsize+2:i*fldmapsize-1)=data((irecs+i-1)*fldmapsize1+1:(irecs+i)*fldmapsize1)
        else
          datatmp((i-1)*fldmapsize+1:i*fldmapsize)=data((irecs+i-1)*fldmapsize+1:(irecs+i)*fldmapsize)
        endif
       enddo
!
       idispstt=(int(k-1,8)*int(nfld,8)+int(mrecs-1,8))*int(nfldsize*4,8)

       call writempi4(gfile,fieldmapsize,filetype,datatmp,iret=iret, &
         idispstt=idispstt)
       if (iret.ne.0) return
!
     enddo
     deallocate(fieldmap,datatmp)
!
     iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine mpi_densewrite4
!------------------------------------------------------------------------------
   subroutine mpi_densewrite8(gfile,ista,iend,jsta,jend,data,jrecs,jrece,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: ista,iend,jsta,jend
     real(nemsio_dblekind),intent(in)    :: data(:)
     integer,optional,intent(in)         :: jrecs,jrece
     integer,optional,intent(out)        :: iret
!--- local vars
     integer                  :: i,ios,nfldsize,nfld,nfldloop,mfldrmd,k
     integer               :: fieldmapsize,fldmapsize,fldmapsize1,fielddatasize
     integer,allocatable      :: fieldmap(:)
     real(nemsio_dblekind),allocatable   :: datatmp(:)
     integer irecs,irece,nfldlp,mrec,mrecs
     integer(8) idispstt
     integer filetype
!---
     iret=-25
!
!--- set nfld
     if(present(jrecs).and.present(jrece)) then
       mrec=jrece-jrecs+1
       mrecs=jrecs
     else
       mrec=gfile%nrec
       mrecs=1
     endif
     nfldsize=gfile%fieldsize+2
     nfld=min(mrec,nemsio_maxint/(nfldsize*2))
     nfldloop=(mrec-1)/nfld+1
     mfldrmd=mod(mrec,nfld)
!     write(0,*)'in dense read,nfld=',nfld,'nfldloop=',nfldloop, &
!       'mfldrmd=',mfldrmd
!
!--- set file map
    if(gfile%mype==gfile%lead_task) then
      fieldmapsize=((iend-ista+1)*(jend-jsta+1)+2)*nfld
      fldmapsize=(iend-ista+1)*(jend-jsta+1)+2
      fldmapsize1=(iend-ista+1)*(jend-jsta+1)
    else
      fldmapsize=(iend-ista+1)*(jend-jsta+1)
      fieldmapsize=(iend-ista+1)*(jend-jsta+1)*nfld
    endif
    allocate(datatmp(fieldmapsize))
    allocate(fieldmap(fieldmapsize) )
    call set_mpimap_wrt(gfile,ista,iend,jsta,jend,fieldmap,ios)
    if(ios.ne.0) return
!
!--- set file type
     call mpi_type_create_indexed_block(fieldmapsize,1,fieldmap, &
          MPI_REAL8,filetype,ios)
     call MPI_TYPE_COMMIT(filetype,ios)
     if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop('stop: at write set type indexed block')
       endif
     endif
!
!---
     do k=1,nfldloop
!
       irecs=(k-1)*nfld+1 
       if(k<nfldloop.or.mfldrmd==0) then
         nfldlp=nfld
         irece=k*nfld 
       elseif(mfldrmd/=0) then
         deallocate(fieldmap,datatmp)
         fieldmapsize=fldmapsize*mfldrmd
         allocate(fieldmap(fieldmapsize) )
         allocate(datatmp(fieldmapsize) )
         call set_mpimap_wrt(gfile,ista,iend,jsta,jend,fieldmap,ios)
         if(ios.ne.0) return
!
!--- set file type
         call mpi_type_create_indexed_block(fieldmapsize,1,fieldmap, &
          MPI_REAL8,filetype,ios)
         call MPI_TYPE_COMMIT(filetype,ios)
         if ( ios.ne.0 ) then
          if ( present(iret))  then
           iret=ios
           return
          else
           call nemsio_stop('stop: at write set type indexed block')
          endif
         endif

         nfldlp=mfldrmd
         irece=mrec
       endif
!
!--- prepare data
       do i=1,nfldlp
        if(gfile%mype.eq.gfile%lead_task) then
          datatmp((i-1)*fldmapsize+1)=gfile%fieldsize_real8
          datatmp(i*fldmapsize+2)=datatmp(1)
          datatmp((i-1)*fldmapsize+2:i*fldmapsize+1)=data((irecs+i-1)*fldmapsize1+1:(irecs+i)*fldmapsize1)
        else
          datatmp((i-1)*fldmapsize+1:i*fldmapsize)=data((irecs+i-1)*fldmapsize+1:(irecs+i)*fldmapsize)
        endif
       enddo

       idispstt=(int(k-1,8)*int(nfld,8)+int(mrecs-1,8))*int(gfile%fieldsize*8+8,8)
!
       call writempi8(gfile,fieldmapsize,filetype,datatmp,iret=iret,idispstt=idispstt)
         if(iret/=0) return
     enddo
     deallocate(fieldmap,datatmp)
!
     iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine mpi_densewrite8
!------------------------------------------------------------------------------
   subroutine writempi4(gfile,fieldmapsize,filetype,data,iret,idispstt) 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write out real 4 data using MPI
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: fieldmapsize
     integer,intent(in)                  :: filetype
     real(nemsio_realkind),intent(in)    :: data(:)
     integer,optional,intent(out)        :: iret
     integer(8),optional,intent(in)      :: idispstt
!--- local vars
     integer(MPI_OFFSET_KIND) :: idisp
     integer                  :: status(MPI_STATUS_SIZE)
     integer ios
!
!--- file set view, and read
!
       if(present(idispstt))  then
         idisp=gfile%tlmeta+idispstt
       else
         idisp=gfile%tlmeta
       endif
       call mpi_file_set_view(gfile%fh,idisp,MPI_REAL4,filetype,'native', &
         MPI_INFO_NULL,ios)
       if(gfile%do_byteswap) call byteswap(data,nemsio_realkind,size(data))
       call MPI_FILE_WRITE_ALL(gfile%fh,data,fieldmapsize,MPI_REAL4,  &
        status,ios)
       if(gfile%do_byteswap) call byteswap(data,nemsio_realkind,size(data))
       if ( ios.ne.0 ) then
         if ( present(iret))  then
           iret=ios
           return
         else
           call nemsio_stop('stop: at MPI write all for bin4')
         endif
       endif
!
      iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   end subroutine writempi4
!------------------------------------------------------------------------------
  subroutine writempi8(gfile,fieldmapsize,filetype,data,iret,idispstt)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write out real 8 data using MPI
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     type(nemsio_gfile),intent(inout)    :: gfile
     integer,intent(in)                  :: fieldmapsize
     integer,intent(in)                  :: filetype
     real(nemsio_dblekind),intent(in)    :: data(:)
     integer,optional,intent(out)        :: iret
     integer(8),optional,intent(in)         :: idispstt
!--- local vars
     integer(MPI_OFFSET_KIND) :: idisp
     integer                  :: status(MPI_STATUS_SIZE)
     integer ios
!---
     iret=-25
!
!--- file set view, and read
!
       if(present(idispstt))  then
         idisp=gfile%tlmeta+idispstt
       else
         idisp=gfile%tlmeta
       endif
       call mpi_file_set_view(gfile%fh,idisp,MPI_REAL8,filetype,'native', &
         MPI_INFO_NULL,ios)
       if(gfile%do_byteswap) call byteswap(data,nemsio_dblekind,size(data))
       call MPI_FILE_WRITE_ALL(gfile%fh,data,fieldmapsize,MPI_REAL8,  &
         status,ios)
       if ( ios.ne.0 ) then
         if ( present(iret))  then
           iret=ios
           return
         else
           call nemsio_stop('stop: at MPI write all for bin8')
         endif
       endif
       if(gfile%do_byteswap) call byteswap(data,nemsio_dblekind,size(data))
!
      iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    end subroutine writempi8
!
!------------------------------------------------------------------------------
!
     elemental function equal_str_nocase(str1,str2)
!
!-----------------------------------------------------------------------
!
! convert a word to lower case
!
      logical              :: equal_str_nocase
      Character (len=*) , intent(in) :: str1
      Character (len=*) , intent(in) :: str2
      integer :: i,ic1,ic2,nlen
      nlen = len(str2)
!
      if(len(str1)/=nlen)  then
        equal_str_nocase=.false.
        return
      endif
      equal_str_nocase=.false.
      do i=1,nlen
        ic1 = ichar(str1(i:i))
        if (ic1 >= 65 .and. ic1 < 91) ic1 = ic1+32
        ic2 = ichar(str2(i:i))
        if (ic2 >= 65 .and. ic2 < 91) ic2 = ic2+32
        if(ic1/=ic2) then
           equal_str_nocase=.false.
           return
        endif
      end do
      equal_str_nocase=.true.
!
!-----------------------------------------------------------------------
!
      end function equal_str_nocase
!
!-----------------------------------------------------------------------
!
      subroutine chk_endianc(endian)
!
        implicit none
!
        character(16),intent(out)    :: endian
!     ------------------------------------------------------------------
        INTEGER,PARAMETER :: ASCII_0 = 48,ASCII_1 = 49,ASCII_2 = 50,    &
                           ASCII_3 = 51
        INTEGER(4)        :: I
        common// I
!     ------------------------------------------------------------------
!***** code start
!     ------------------------------------------------------------------
        I = ASCII_0 + ASCII_1*256 + ASCII_2*(256**2) + ASCII_3*(256**3)
        call sub(endian)
!
!     ------------------------------------------------------------------
!
      end subroutine chk_endianc
!
!-----------------------------------------------------------------------
!
      subroutine sub(endian)
!
        implicit none
!
        character(16),intent(out)        :: endian
!        character,intent(inout) :: i*4
        character               :: i*4
        common//  i
!     ------------------------------------------------------------------
        if(i .eq. '0123') then
!          WRITE(*,*) ' Machine is Little-Endian '
          endian='little_endian'
          return
        elseif (i .eq. '3210') then
!          WRITE(*,*) ' Machine is Big-Endian '
          endian='big_endian'
          return
        else
!          WRITE(*,*) ' Mixed endianity machine ... '
          endian='mixed_endian'
          return
        endif
!
!     ------------------------------------------------------------------
!
      end subroutine sub
!
!------------------------------------------------------------------------------
  end module nemsio_module_mpi
