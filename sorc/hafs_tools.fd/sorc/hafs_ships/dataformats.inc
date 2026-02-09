c
c        dataformats.inc
c
c*************************************************
c           a and b deck data records
c*************************************************/

      integer  AidTauMax
      PARAMETER  (AidTauMax = 136)

      type ATCF_RECORD
         SEQUENCE
         character      basin*2
         character      cyNum*2
         character      DTG*10
         character      technum*2
         character      tech*4
         character      tau*3
         character      latns*4
         character      lonew*5
         character      vmax*3
         character      mslp*4
         character      ty*2
         character      rad*3
         character      windcode*3
         character*4    radii(4)
         character      radp*4
         character      rrp*4
         character      mrd*3
         character      gusts*3
         character      eye*3
         character      unused*3
         character      maxseas*3
         character      initials*3
         character      dir*3
         character      speed*3
         character      stormname*10
         character      depth*1
         character      seas*2
         character      seascode*3
         character*3    seasrad(4)
      end type ATCF_RECORD

c
c   Data structure to hold best track records (b-deck)
c
      type B_RECORD
         SEQUENCE
         character      basin*2 
         integer        cyNum 
         character      DTG*10
         character      tech*4
         integer        tau
         real           lat
         character      NS*1
         real           lon
         character      EW*1
         integer        vmax
         integer        mslp
         character      ty*2
      end type B_RECORD

c
c   Data structure to hold objective aids records (a-deck)
c
      type A_RECORD
         SEQUENCE
         character      basin*2
         integer        cyNum
         character      DTG*10
         integer        technum
         character      tech*4
         integer        tau
         real           lat
         character      NS*1
         real           lon
         character      EW*1
         integer        vmax
         integer        mslp
         character      ty*2
         integer        rad
         character      windcode*3
         integer        radii(4)
         integer        radp
         integer        rrp
         integer        mrd
         integer        gusts
         integer        eye
         integer        maxseas
         character      initials*3
         integer        dir
         integer        speed
         character      stormname*10
         character      depth*1
         integer        seas
         character      seascode*3
         integer        seasrad(4)
      end type A_RECORD

c
c   Data structure to hold all objective aid info for a single technique
c   and a single DTG
c
      type AID_DATA
         SEQUENCE
         integer        numrcrds
         type (A_RECORD)    aRecord(AidTauMax*4)
         type (ATCF_RECORD) atcfRcd(AidTauMax*4)
      end type AID_DATA

c
c   Data structure to hold all objective aid info for a single DTG
c
      type BIG_AID_DATA
         SEQUENCE
         integer        numrcrds
         type (A_RECORD)    aRecord(100*AidTauMax*4)
         type (ATCF_RECORD) atcfRcd(100*AidTauMax*4)
      end type BIG_AID_DATA

c******************************************
c   OBJECTIVE AIDS STRUCTURES
c******************************************
c
c   Information for one tau of objective aid data.
c

      type PNT
         SEQUENCE
         integer*2      v 
         integer*2      h 
      end type PNT

      type OBJECTIVE_AIDS_DATAPOINT
         SEQUENCE
         logical*2      IsValid
         logical*2      IsEmpty
         real           Latitude
         logical*2      North
         real           Longitude
         logical*2      East
         type (PNT)     pos
         integer*2      Wind
      end type OBJECTIVE_AIDS_DATAPOINT

c
c   A collection of 5 taus where 0=12hrs, 1=24hrs, 2=36hrs, 3=48hrs, 
c   4=72hrs.
c
      type OBJECTIVE_AIDS_DATAPOINTS
         SEQUENCE
         type (OBJECTIVE_AIDS_DATAPOINT) Point(5)
      end type OBJECTIVE_AIDS_DATAPOINTS

c
c   structure to contain all the data for one DTG of one aid technique
c   NOTE! make sure to zero the entire contents before ANY use.
c
      type OBJECTIVE_AIDS_RCRD
         SEQUENCE
         character      StormID*32
         character      Technique*16
         character      DTG*9
         type (OBJECTIVE_AIDS_DATAPOINTS) Data
      end type OBJECTIVE_AIDS_RCRD

c
c   Info for an objective aid data record.
c
      type OBJAID_RECORD
         SEQUENCE
         character      techname*5
         character      dtg*9
         integer        lat(5)
         integer        lon(5)
         integer        vmax(5)
         character      stormID*6
      end type OBJAID_RECORD
