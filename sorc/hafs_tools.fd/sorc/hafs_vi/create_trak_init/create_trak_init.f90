

        CHARACTER PART1*2,storm_id*3,basin*2,NS*1,EW*1,NUM*2,PART2*3

        READ(5,*)storm_id,IYEAR

        IF(storm_id(3:3).eq.'L') basin='AL'
        IF(storm_id(3:3).eq.'W') basin='WP'
        IF(storm_id(3:3).eq.'E') basin='EP'
        IF(storm_id(3:3).eq.'C') basin='CP'
!zhang: for india ocean:
        IF(storm_id(3:3).eq.'A') basin='AA'
        IF(storm_id(3:3).eq.'B') basin='BB'
!Chanh: for Southern hemisphere and Southern India Ocean
        IF(storm_id(3:3).eq.'P') basin='SP'
        IF(storm_id(3:3).eq.'S') basin='SI'

        REWIND(12)

        IFH=-1

        IF(IYEAR.LE.2005)THEN

        DO I=1,1000
          READ(12,65,end=104)PART1,NUM,IDAT,IHOUR,IFH,LAT,NS,LON,EW
          IF(PART1.eq.basin.and.NUM.eq.storm_id(1:2).and.IFH.eq.0)then
            if(abs(LAT).LT.0.01.or.abs(LON).lt.0.01)go to 104
            if(NS.eq.'S')LAT=-LAT
            if(EW.eq.'E')LON=3600-LON
            go to 105
          END IF
        END DO

 65     FORMAT(A2,2x,A2,4x,I6,I2,12x,I3,2x,I3,A1,2x,I4,A1)

        ELSE

        DO I=1,1000
!          READ(12,75,end=104)PART1,NUM,IDAT,IHOUR,IFH,LAT,NS,LON,EW
          READ(12,65,end=104)PART1,NUM,IDAT,IHOUR,IFH,LAT,NS,LON,EW
          IF(PART1.eq.basin.and.NUM.eq.storm_id(1:2).and.IFH.eq.0)then
            if(abs(LAT).LT.0.01.or.abs(LON).lt.0.01)go to 104
            if(NS.eq.'S')LAT=-LAT
            if(EW.eq.'E')LON=3600-LON
            go to 105
          END IF
        END DO

 75     FORMAT(A2,1x,A2,1x,4x,I6,I2,12x,I3,2x,I3,A1,2x,I4,A1)

        ENDIF

 104    CONTINUE

        READ(11,13)PART2,IDAT,IHOUR,LAT,NS,LON,EW
 13     FORMAT(5x,A3,13x,I6,1x,I2,3x,I3,A1,1x,I4,A1)
        if(NS.eq.'S')LAT=-LAT
        if(EW.eq.'E')LON=3600-LON

        IF(PART2.ne.storm_id)then
          print*,'can not find storm center in model guess, stop'
          stop
        end if

 105    CONTINUE

!CWH        print*,PART1,NUM,IDAT,IFH,LAT,NS,LON,EW
        print*,PART1,NUM,IDAT,LAT,NS,LON,EW


        WRITE(30,15)IDAT,IHOUR,LAT,LON,LAT,LON,LAT,LON,LAT,LON, &
                    LAT,LON,LAT,LON,LAT,LON,storm_id
 15     FORMAT('72HDAS',I6,I2,14I4,'   0   0   0   0   0   0',1x,3A)

        END
