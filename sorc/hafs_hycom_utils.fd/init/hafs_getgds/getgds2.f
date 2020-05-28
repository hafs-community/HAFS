      program ofs_getgds2
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  ofs_getgds2
C   PRGMMR: DIredell          ORG: SIB         DATE: 2014-09-18
C
C ABSTRACT: This program reads GRIB2 file and prints Grid specification
C           for input to copygb2 (-g option). 
C           This program reduced version of degrib2.
C
C PROGRAM HISTORY LOG:
C 2014-09-16  Diredell    
C
C USAGE:
C COMMAND LINE:
C     ofs_getgds2  grib2_file
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     6        - STANDARD FORTRAN PRINT FILE
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     LIBRARY:
C       G2LIB    - GB_INFO, GT_GETFLD, PRLEVEL, PRVTIME
C       W3LIB    - GBYTE, SKGB
C       BACIO    - BAOPENR, BAREAD, BACLOSE
C       SYSTEM   - IARGC   FUNCTION RETURNS NUMBER OF ARGUMENT ON
C                          COMMAND LINE
C                - GETARG  ROUTINE RETURNS COMMAND LINE ARGUMENT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: COMMAND LINE CAN HAVE ONE FILE NAME.
C     
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM RS/6000
C
      use grib_mod
      use params
      parameter(msk1=32000,msk2=4000)
      CHARACTER(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
!      integer :: igds(5),igdstmpl(200),ipdstmpl(200),idrstmpl(200)
!      integer :: ideflist(500)
      character(len=250) :: gfile1
      INTEGER(4) NARG,IARGC,temparg
      integer :: currlen=0
      logical :: unpack,expand
      type(gribfield) :: gfld
      call start()
      unpack=.true.
      expand=.false.
      
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET ARGUMENTS
      NARG=IARGC()
      IF(NARG.NE.1) THEN
        CALL ERRMSG('ofd_getgds2:  Incorrect usage')
        CALL ERRMSG('Usage: ofd_getgds2 grib2file')
        CALL ERREXIT(2)
      ENDIF

      IFL1=10
      temparg=1
      CALL GETARG(temparg,gfile1)
      NCGB=LEN_TRIM(gfile1)
      CALL BAOPENR(ifl1,gfile1(1:NCGB),IOS)

      itot=0
      icount=0
      iseek=0
C     do
         call skgb(ifl1,iseek,msk1,lskip,lgrib)
         if (lgrib.eq.0) stop 10
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            currlen=lgrib
         endif
         call baread(ifl1,lskip,lgrib,lengrib,cgrib)
         if (lgrib.ne.lengrib) then
            write(6,*)' ofd_getgds2: IO Error.'
            call errexit(9)
         endif
         iseek=lskip+lgrib
         icount=icount+1

! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1,
     &                numfields,numlocal,maxlocal,ierr)
         if (ierr.ne.0) then
           write(6,'(A,I0)') ' ERROR extracting field = ',ierr
           stop 10
         endif
         itot=itot+numfields
C        do n=1,numfields
           n=1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(6,'(A,I0)') ' ERROR extracting field = ',ierr
C            cycle
           endif

           write(6,'(100(I0,1x))')
     &         gfld%igdtnum, (gfld%igdtmpl(j),j=1,gfld%igdtlen)
C
          call gf_free(gfld)
C        enddo

C     enddo
      stop
      end
