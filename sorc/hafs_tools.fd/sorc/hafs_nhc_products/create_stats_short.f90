  program hafs_nhc_products

!===============================================================
!! read atcf file, and generate a short version of stats
!!       IN - 'storm_info' file
!!              fort.19  atcf of  previous cycle (6h earlier)
!!              fort.20  atcf of  current cycle
!!       OUT  - fort.41   stat.short
!!              fort.51   afos
!!              fort.61   tpc
!================================================================
  implicit none
  
  !---parameter define
  character*80 atcf,storm_name, skip, lower_case, lower, upper
  character yyyy*4,mm*2,dd*2,hh*2,storm_id*3

  integer nmax
  !---local variables
  parameter(nmax=200)
  integer :: i,j,k
  real, dimension(0:nmax) :: fhr,wind,press,rlat,rlon, &
                             direct,speed                   ! storm heading and speed
  character clat(0:nmax)*1 , clon(0:nmax)*1                 ! save W,E or N,S  
  integer :: tmp1,tmp2,tmp3,tmp4,tmp5
  character s1*1 ,s2*1

  real dlat1,dlat2,dlon1,dlon2,dis,dt, dlat,dlon

  real distsp

  !---message head
  character line*80,head1*72,head2*72,head3*72,     &
            head4*72,head5*72,head6*72,stmtyp(3)*19,mon(12)*3,monz*3, &
            date*10,slsh*1,headcoup*72,headnocoup*72

  integer iscolt(3),iecolt(3), iocean, istt, imm

  data head1/'ATTENTION...NATIONAL HURRICANE CENTER'/,             &
       head2/'NCEP HURRICANE MODEL..HAFS MMM...FORECAST MADE FOR'/,&
       head3/'INITIAL TIME        '/,                              &
       head4/'FORECAST STORM POSITION '/,                          &
       head5/'HOUR        LATITUDE        LONGITUDE        HEADING/&
             SPEED(KT)'/, &
       head6/'STORM DISSIPATED AT     HRS AT THE ABOVE PSN.      '/
  data headcoup/'NCEP COUPLED HAFS HURRICANE MODEL FORECAST MADE FOR'/
  data headnocoup/'NCEP HAFS HURRICANE MODEL FORECAST MADE FOR  '/
  data stmtyp/'TROPICAL DEPRESSION','TROPICAL STORM','HURRICANE'/
  data iscolt/1,1,1/,iecolt/19,14,9/
  data mon/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG','SEP',  &
           'OCT','NOV','DEC'/
  data slsh/'/'/

  !------------------------------------------------------------------
  !---read storname and stormid from storm1.holdvars.txt
  open(10,file='storm1.holdvars.txt',status='old',err=999)
  do i=1, 91
    read(10,*)
  enddo
  read(10,'(12x,A4,A2,A2,A2)')yyyy,mm,dd,hh
  read(10,*)
  read(10,*)
  read(10,*)
  read(10,'(13x,A10)')storm_name
  read(10,'(15x,A3)')storm_id
  close(10)

  !---construct atcf file name
  atcf=lower(trim(storm_name)//trim(storm_id)//'.'  &
  &        //yyyy//mm//dd//hh//'.hafs.trak.atcfunix')
  write(*,*)'atcf=',trim(atcf)

  !---example: AL, 14, 2016100706, 03, HAFS, 000, 277N,  797W, 103,  938, XX,  34, NEQ, 023 
  do i=0,nmax
    fhr(i)=-999.0
  enddo
          
  k=0
  do i=1,nmax
    read(20,'(29x,I4,1x,I4,A1,1x,I5,A1,1x,I4,1x,I5)',end=101,err=9991) &
             tmp1,tmp2, s1,tmp3,s2,tmp4,tmp5
    if( i == 1 .or. ( i .gt. 1 .and. float(tmp1) /= fhr(k) ) ) then
      k=k+1
      fhr(k)=float(tmp1)
      rlat(k)=float(tmp2)/10
      clat(k)=s1
      if ( trim(s1) == 'S' )rlat(k)=-1.0*rlat(k)
        rlon(k)=float(tmp3)/10
        clon(k)=s2
        if ( trim(s2) == 'W' )rlon(k)=-1.0*rlon(k)
          wind(k)=float(tmp4)
          press(k)=float(tmp5)
  !---output a simple file for track/intensity
          write(21,'(F6.1,4F9.2)')fhr(k),rlat(k),rlon(k),wind(k),press(k)   
    endif
  enddo
  9991       continue
  if (k .lt. 2) then
    write(6,*)'atcf is not found or bad, stop'
    stop
  else
    write(6,*)'continue though bad line is found in atcf'
  endif
  101        close(20)


  !!!---write stats.shrot, unit=41
  !example: HOUR:126.0  LONG:  -79.25  LAT:  16.51  MIN PRESS (hPa):  940.02   MAX SURF WIND (KNOTS):103.50
  do i=1,k
    write(41,'(A5,F5.1,A7,F8.2,A6,F7.2,A18,F8.2,A25,F6.2)')&
             'HOUR:',fhr(i),'  LONG:',rlon(i),&
             'LAT:',rlat(i),'  MIN PRESS (hPa):',press(i),&
             '   MAX SURF WIND (KNOTS):',wind(i)
  enddo
          
  !!!---outout head for afos file, unit=51
  line=' '
  line=head1
  write(6,9)  line
  write(51,9)  line
  9 format(a)
  line=' '
  write(6,9)  line
  write(51,9)  line

  iocean=1   ! may read from a file
  if (iocean == 1) then
    line=headcoup
  else
    line=headnocoup
  endif

  write(6,9)  line
  write(51,9)  line

  if(wind(1) .le. 34.0)  then
    istt=1
  else if(wind(1) .le. 66.0)  then
    istt=2
  else
    istt=3
  endif
  line=' '
  write(6,9)  line
  write(51,9)  line
  line(iscolt(istt):iecolt(istt))=stmtyp(istt)
  line(iecolt(istt)+2:iecolt(istt)+10)=upper(trim(storm_name))
  line(iecolt(istt)+12:iecolt(istt)+14)=upper(trim(storm_id))
  write(6,9)  line
  write(51,9)  line

  !---date/time
  date='          '
  date(1:2)=trim(hh)
  date(9:10)=trim(dd)
  read(mm,'(I2)')imm
  monz=mon( imm )
  date(3:3)='Z'
  date(5:7)=monz
  write(6,25)  date
  25 format('  ...date=',a)
  line=' '
  write(6,9)  line
  write(51,9)  line
  line=' '
  line=head3
  line(15:24)=date
  write(6,9)  line
  write(51,9)  line


  write(51,9)' '
  write(51,9)'DISCLAIMER ...  THIS INFORMATION IS PROVIDED AS GUIDANCE.  IT'
  write(51,9)'REQUIRES INTERPRETATION BY HURRICANE SPECIALISTS AND SHOULD'
  write(51,9)'NOT BE CONSIDERED AS A FINAL PRODUCT.  PLEASE SEE THE TPC/NHC'
  write(51,9)'OFFICIAL FORECAST.'      
  write(51,9)' '

  line=' '
  write(6,9)  line
  write(51,9)  line
  line=head4
  write(6,9)  line
  write(51,9)  line
  line=' '
  write(6,9)  line
  write(51,9)  line

  !---column header
  line=' '
  line=head5
  write(6,9)  line
  write(51,9)  line
  line=' '
  write(6,9)  line
  write(51,9)  line

  !---compute heading direction and speed
  !---need previous location, unit=19        
  j=1  
  read(19,'(29x,I4,1x,I4,A1,1x,I5,A1,1x,I4,1x,I5)',      &
       err=910)  tmp1,tmp2, s1,tmp3,s2,tmp4,tmp5
  j=0
  fhr(0)=-6.0  !float(tmp1)
  rlat(0)=float(tmp2)/10
  clat(0)=s1
  if ( trim(s1) == 'S' )rlat(0)=-1.0*rlat(0)
    rlon(0)=float(tmp3)/10
    clon(0)=s2
  if ( trim(s2) == 'W' )rlon(0)=-1.0*rlon(0)
    wind(0)=float(tmp4)
    press(0)=float(tmp5)
             
  910         continue        
  if (j == 1) then  ! if previous atcf does not exist
    write(*,*)'previous atcf file does not exist'
    fhr(0)=-6.0
    rlat(0)=rlat(1)
    rlon(0)=rlon(1)
  endif

  !---calculate heading and speed
  do i=j+1,k
    dlat1=rlat(i-1)
    dlat2=rlat(i)
    dlon1=rlon(i-1)
    dlon2=rlon(i)
    dis=DISTSP(DLAT1,DLON1,DLAT2,DLON2)
    dt=(fhr(i)-fhr(i-1))*3600.0
    speed(i)=1.94*dis/dt
    dlon=rlon(i)-rlon(i-1)
    dlat=rlat(i)-rlat(i-1)
    direct(i)=atan2d(dlon,dlat)
    if(direct(i) .lt. 0.0) direct(i)=direct(i)+360.0

  !---in case we do not have previous atcf, assume storm heading/speed same as i=2 
    if (j == 1 .and. i ==2 ) &
      write(51,4050)ifix(fhr(1)),rlat(1),-rlon(1),direct(2),'/',speed(2) 

  !---write(*,*)'dlat1,dlat2,dlon1,dlon2,distance=',dlat1,dlat2,dlon1,dlon2,dis,direct(i),speed(i)
    write(51,4050) ifix(fhr(i)),rlat(i),-rlon(i),direct(i),'/',speed(i)
    4050      format(1x,i3,9X,F7.1,10X,F7.1,10x,f5.0,a,f4.1)

  enddo

  close(51)

  !!!---write tpc file, read first 8 line from fort.51, then write to unit=61
  do i=1,8
    read(51,'(A)')skip
    write(*,*)skip
    write(61,9)skip
  enddo

  write(61,9)'FORECAST POSITIONS (FROM STATS.SHORT FILE...)'
  write(61,9)' '
  write(61,9)'HOUR     LATITUDE    LONGITUDE    MIN PRESS (hPa)     MAX SFC WIND(KTS)'
  write(61,9)' '

  !---example:   0       13.55       -61.17         1003                  39
  do i=1,k
    write(61,'(I4,F12.2,1x,F12.2,I13,I20)')ifix(fhr(i)),rlat(i),rlon(i),ifix(press(i)),ifix(wind(i))
  enddo

  !---write some message
  if (fhr(k) == 126.0 ) write(61,'(a)')'    FORECAST RAN COUPLED TO HOUR: 126'
  if(fhr(k) .lt. 6.0)write(61,'(a)')'    STORM DISSIPATED BEFORE 6 HOURS....'
  if(fhr(k) .lt.126.0 .and. fhr(k) .ge. 6.0) then
    line='    STORM DISSIPATED AT XXX HOURS AT ABOVE POSITION....'
    write(line(25:27),'(I3)')ifix(fhr(k))         
    write(61,'(a)')line
  endif

  write(61,9)' '
  write(61,9)'DISCLAIMER ...  THIS INFORMATION IS PROVIDED AS GUIDANCE.  IT'
  write(61,9)'REQUIRES INTERPRETATION BY HURRICANE SPECIALISTS AND SHOULD'
  write(61,9)'NOT BE CONSIDERED AS A FINAL PRODUCT.  PLEASE SEE THE TPC/NHC'
  write(61,9)'OFFICIAL FORECAST.'      
  write(61,9)' '

  write(6,*)'ALL DONE'
  write(6,*)' fort.41   stat.short'
  write(6,*)' fort.51   afos'
  write(6,*)' fort.61   tpc'
  stop
 
  999      continue
  write(6,*)'storm_info file or atcf is not found or bad, stop'
  write(6,*)'cannot process stat.short,afos,tpc files'
  stop

  end 
  
  !!!-------------------------------------------------------------------------------

  function lower(word)
  implicit none 
  character(len=*) word,lower
  character lower_all*26 , upper_all*26
  character(len=len( word) ) out
  integer :: i,n

  lower_all='abcdefghijklmnopqrstuvwxyz'
  upper_all='ABCDEFGHIJKLMNOPQRSTUVWXYZ'

  do i=1,len( word )
    n=index(upper_all,word(i:i) )
    out(i:i)=word(i:i) 
    if (n /= 0) out(i:i)=lower_all(n:n) 
  enddo

  lower=out

  end function lower
 
  !!!---------------------------------------------------------------------------------

  function upper(word)
  implicit none
  character(len=*) word,upper
  character lower_all*26 , upper_all*26
  character(len=len( word) ) out
  integer :: i,n

  lower_all='abcdefghijklmnopqrstuvwxyz'
  upper_all='ABCDEFGHIJKLMNOPQRSTUVWXYZ'

  do i=1,len( word )
    n=index(lower_all,word(i:i) )
    out(i:i)=word(i:i) 
    if (n /= 0) out(i:i)=upper_all(n:n) 
  enddo

  upper=out

  end function upper

  !--------------------------------------------------------------------------------

  function DISTSP(DLAT1,DLON1,DLAT2,DLON2)
  implicit none
  real :: DLAT1, DLON1, DLAT2, DLON2, DISTSP
  real :: XXD, XXM
  real :: REARTH
  
  REARTH=6.37E6
 
  XXD=COSD(DLON1-DLON2)*COSD(DLAT1)*COSD(DLAT2)+SIND(DLAT1)*SIND(DLAT2)
  XXM=AMIN1(1.0,AMAX1(-1.0,XXD))
  DISTSP=ACOS(XXM)*REARTH

  RETURN

  end function DISTSP 
