

        CHARACTER PART1*2,storm_id*3,NS*1,EW*1,NUM*2,PART2*3
        INTEGER LATHR(7),LONHR(7)


        READ(5,*)storm_id,ih

        READ(11,13)PART2,IDAT,IHOUR,LAT,NS,LON,EW

        if(ih.ne.IHOUR)then
           print*,'wrong tc vitals',IDAT,IHOUR
           stop 111
        end if

        rewind 11

        LATHR=9999
        LONHR=9999

        DO I=1,100
          READ(12,65,end=104)PART1,NUM,IDAT,IHOUR,IFH,LAT,NS,LON,EW
          print*,"zhang=",PART1,NUM,IDAT,IHOUR,IFH,LAT,NS,LON,EW
            if(NS.eq.'S')LAT=-LAT
            if(EW.eq.'E')LON=3600-LON
            print*,"tong=",IFH
            if(IFH.ge.3.and.IFH.le.9)THEN
               LATHR(IFH-2)=LAT
               LONHR(IFH-2)=LON
               print*,"tong=",IFH-2,LATHR(IFH-2),LONHR(IFH-2)
            end if
          if(IFH.eq.9) go to 105
        END DO

!        print*,LAT0,LON0,LAT3,LON3,LAT6,LON6,LAT9,LON9

 65     FORMAT(A2,2x,A2,4x,I6,I2,12x,I3,2x,I3,A1,2x,I4,A1)

 104    CONTINUE

        IF(LATHR(4).eq.9999.or.LONHR(4).eq.9999)then
	  print*,'there is no 6 hour storm position'
	  stop 999
        ELSE IF(LATHR(4).EQ.0.and.LATHR(4).eq.0)THEN
          print*,'there is no 6 hour storm position'
          stop 999
        END IF
        GO TO 105

        READ(11,13)PART2,IDAT,IHOUR,LAT,NS,LON,EW
 13     FORMAT(5x,A3,13x,I6,1x,I2,3x,I3,A1,1x,I4,A1)
        if(NS.eq.'S')LAT=-LAT
        if(EW.eq.'E')LON=3600-LON

        LATHR=LAT
        LONHR=LON
        IF(PART2.ne.storm_id)then
          print*,'can not find storm center in model guess, stop'
          stop
        end if

 105    CONTINUE

        print*,PART1,NUM,IDAT,IFH,LAT,NS,LON,EW,storm_id


        WRITE(30,15)IDAT,IHOUR,(LATHR(J),LONHR(J),J=1,7),storm_id
 15     FORMAT('72HDAS',I6,I2,14I4,'   0   0   0   0   0   0',1x,3A)

        END
