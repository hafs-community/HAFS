PROGRAM timeinterp_forcing
  !
  ! add interpolated/extrapolted fields to forcing file 
  !    ./temp/forcing.OFIELD.[ab] for times
  ! extracted from forcing file in 
  !    ./temp/forcing.IFIELD.b
  ! write expanded file to
  !    ./forcing.OFIELD.[ab] 
  ! 
  !
  USE mod_za,ONLY : xcspmd,zaiost,zaiopf,zaiowr,zaiocl,zaiord,idm,jdm
  !
  IMPLICIT NONE

  REAL :: fldmin,fldmax,time,time_old,time_star
  REAL, DIMENSION(:,:), ALLOCATABLE :: T_a_star,T_a,T_s
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: imsk,ip
  INTEGER, PARAMETER :: lua=77,lub=lua+1001,luaa=15,luas=16
  INTEGER :: m,n,ihead,num_records,level1,num,iheada,iheads
  CHARACTER(LEN=79), DIMENSION(:), ALLOCATABLE :: preambl
  CHARACTER(LEN=10) :: labela,labels
  CHARACTER(LEN=16) :: ifile='forcing.123456.b',ofile='forcing.123456.b'
  character(len=14)::oname
  character(len=6)::ifield,ofield
  integer ::num_times,mark
  real , dimension(:), allocatable ::g_time
  real :: wei
  real ,parameter :: time_out=-1.
  !
  ! Read input parameters
  read(5,'(a)')ifield
  READ(5,*) ihead
  READ(5,*) num_times
  allocate(g_time(num_times),preambl(ihead))
  ifile(9:9+6-1)=ifield

  write(*,*)'ifile=',TRIM(ifile)

  OPEN (unit=10,file='./temp/'//TRIM(ifile),status='old' ,action='read')
  READ (10,'(a79)') preambl
  do mark=1,num_times
     read(10,'(28x,f11.2)') g_time(mark)
  enddo
  write(*,*)'Get times to interpolate/extrapolate from ',ifield
  write(*,*)'First target time ',g_time(1)
  write(*,*)'Last  target time ',g_time(num_times)
  close(10)
  deallocate(preambl)

  read(5,'(a)')ofield
  READ(5,*) ihead
  READ(5,*) num_records
  READ(5,'(a)') labela
  ofile(9:9+6-1)=ofield
  write(*,*)'Construct interpolation/interpolation for ',trim(ofile)
  WRITE(*,*) 'ihead,num_records=',ihead,num_records

  ! 
  !
  ! Get grid parameters and allocate arrays.
  CALL xcspmd  !input idm,jdm by use association
  m=idm ; n=jdm
  ALLOCATE ( T_a_star(idm,jdm),imsk(idm,jdm),ip(idm,jdm) &
       ,T_a(idm,jdm),T_s(idm,jdm) ,preambl(ihead) )
  !
  ! Initialize I/O uints.
  CALL zaiost ! initialize units
  !
  ! Read HYCOM mask from regional.mask.[ab]
  ! CALL mask_hycom_2(imsk)
  WRITE(*,*) 'imsk min, max = ',MINVAL(imsk),MAXVAL(imsk)
  !
  ! Open output files and write header in 'forcing.airtmp.b file.
  oname='forcing.'// ofield
  CALL zaiopf(oname//'.a','new' , lua)
  OPEN (unit=lub,file=oname//'.b',status='new' ,action='write')
  OPEN (unit=10,file='./temp/'//TRIM(ofile),status='old' ,action='read')
  READ (10,'(a79)') preambl
  CLOSE(10)
  ! NOTE: header will have the date of creation of the original file
  ! no mention that the field was corrected
  WRITE(lub,'(a79)') preambl
  ! write(*,'(A79)') preambl
  mark=1
  time_old=time_out
  time_star=time_old
  level1=0
  iheada=ihead ; iheads=ihead
  !
  ! Loop over records
  records: DO num=1,num_records
     !
     ! Read ofile
     CALL get_abfld1(luaa,ip,T_a,m,n,labela, &
          level1,'./temp/'//ofile,iheada,time) 
     PRINT *,'./temp/'//ofield,' num=',num

     IF (num==num_records) THEN 
        CALL zaiocl(luaa)
     ENDIF
     !

     if(mark <= num_times ) then

      if(g_time(mark)< time .and. time_old==time_out) then
         time_star=g_time(mark)
         T_a_star=T_a
         mark=mark+1
      elseif(g_time(mark)< time .and. g_time(mark)>time_old) then
         time_star=g_time(mark)
         wei=(g_time(mark)-time_old)/(time-time_old)
         T_a_star=T_s+wei*(T_a-T_s)
         mark=mark+1
      endif
!      print *,time_star,mark,'time_star,mark'
     !
     ! Write extrapolated/interpolated field
      if(mark.gt.1) then
         CALL zaiowr(T_a_star,imsk,.FALSE., fldmin,fldmax, lua,.FALSE.)
         WRITE(lub,'(A,'' date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
              &       labela(1:10),time_star,fldmin,fldmax
      endif
     endif
     if(time_star<time) then
        time_star=time
        T_a_star=T_a
        CALL zaiowr(T_a_star,imsk,.FALSE., fldmin,fldmax, lua,.FALSE.)
       WRITE(lub,'(A,'' date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
             &       labela(1:10),time_star,fldmin,fldmax
     endif

     !
     T_s=T_a
     time_old=time
  ENDDO records

    do while (mark <= num_times ) 
     if(g_time(mark)> time ) then
        time_star=g_time(mark)
        T_a_star=T_a
        mark=mark+1
      print *,time_star,mark,'time_star,mark, at end'     
     !
     ! Write extrapolated field at the end
        CALL zaiowr(T_a_star,imsk,.FALSE., fldmin,fldmax, lua,.FALSE.)
        WRITE(lub,'(A,'' date,span,range = '',F10.2,'' 0 '',1P2E16.7)') &
          &       labela(1:10),time_star,fldmin,fldmax
     endif
    enddo
 

  !
  CLOSE(lub)
  CALL zaiocl(lua)
  !
  DEALLOCATE (T_a)
  DEALLOCATE (T_s)
  DEALLOCATE (T_a_star)
  DEALLOCATE(imsk)
  DEALLOCATE(ip)
  DEALLOCATE(preambl)
  !
  !
CONTAINS
  !
  !========================================================================
  !
  SUBROUTINE mask_hycom_2(imsk)
    !
    !   Read HYCOM mask and calculate HYCOM land/sea mask (land=0,sea=1)
    !
    INTEGER, DIMENSION(:,:), INTENT(out) :: imsk
    REAL, DIMENSION(:,:), ALLOCATABLE :: dc
    REAL :: dmin,dmax
    INTEGER :: i,j,nxhycom,nyhycom
    nxhycom=SIZE(imsk,1)
    nyhycom=SIZE(imsk,2)
    ALLOCATE (dc(1:nxhycom,1:nyhycom))
    imsk=0
!!!!    call zaiost 
    CALL zaiopf('regional.mask.a','old', 61)
    CALL zaiord(dc,imsk,.FALSE.,dmin,dmax, 61)
    CALL zaiocl(61)
    DO j=1,nyhycom
       DO i=1,nxhycom
          IF(dc(i,j)>10.) THEN
             imsk(i,j)=1  
          ELSE 
             imsk(i,j)=0 
          ENDIF
!         imsk(i,j)=1  
       END DO
    END DO
  END SUBROUTINE mask_hycom_2
END PROGRAM timeinterp_forcing
