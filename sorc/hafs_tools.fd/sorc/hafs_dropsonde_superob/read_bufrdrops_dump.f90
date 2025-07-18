PROGRAM DECODEBUFR

use bufrdrops

implicit none

!Used in reading bufr
integer             :: nlevs,numlevs,nmsg,lev,ilev,numfiles,maxobs,nargs
integer             :: unit_bfin,unit_tblin,unit_log,unit_list
integer             :: endoffile,iost, noerr,nbad,nbadtime,nbadsubset
integer             :: nbadac,nbaddrift,dtmax,ifile,syntimesecs
integer             :: iyr, imnth, iday, ihr, imn, isec, itime
integer             :: isynyr, isynmnth, isynday, isynhr
integer,parameter   :: mxmn=8,mxlv=2000
real *8             :: hdr(5),qcvals(6), ervals(6),lauvals(3),drvals(6)
real*8,parameter    :: badval=100000000000.0D0, missvalr=-999.0
real,parameter      :: grav=9.80665
integer*8           :: idate
integer             :: iret,ireadsb,ireadmg,typ,ityp,t29,i
real*8              :: lat,lon,dtime,acidr
real*8              :: drinfo(5,mxlv),obsarraybf(4,mxlv),rseqnum
real*8              :: reltinfo(7),rellocinfo(3),rlat, rlon, lastlat, lastlon
character(len=3)    :: cint
character(len=2)    :: ltype
character(len=5)    :: acid,seqnum
character(len=8)    :: subset, subsetbf
character(len=6)    :: sid
character(len=10)   :: syntime
character(len=16)   :: infile
character(len=80)   :: acstr='ACID'
character(len=80)   :: seqstr='SEQNUM'
character(len=80)   :: launchstr='CLATH CLONH HEIT'
character(len=80)   :: reltime='TSIG YEAR MNTH DAYS HOUR MINU SECO'
character(len=80)   :: obstr='TMDB TMDP WDIR WSPD'
character(len=80)   :: drst='LTDS PRLC GP10 LONDH LATDH'
character(len=80)   :: filelist(200)
!Used in writing prepbufr
real                :: u, v, t, q,rsecs
real                :: wdir, wspd, temp, tempd, gpht, press, rsyntime,tdiff
real                :: latnow, lonnow,rht,error,qnow,hgtnow
real                :: pnow,unow,vnow,tnow,tdnow,tdnow2
real                :: ptop, pbottom, htop, hbottom,sdev(5)
real,allocatable    :: levarray(:)
real*8,allocatable  :: obsarray(:,:),locarray(:,:)
real*8              :: metadatam(9),metadataw(9),header(9)
real*8              :: obsdatam(9),obsqcm(7),obserrm(7),outdrinfo(3)
real*8              :: obsdataw(9),obsqcw(7),obserrw(7)
real*8              :: qcmark,qerror,perror,terror,uverror
integer             :: unit_bfout,unit_tblout,lint
integer             :: obsusem,obsusew
character(len=80)   :: outobsstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
character(len=80)   :: outqcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
character(len=80)   :: outerrstr='POE QOE TOE ZOE WOE NUL PWE     '
character(len=80)   :: outheadstr='SID XOB YOB DHR TYP ELV T29 TSB ITP'
character(len=8)    :: outsubset
character(len=18)   :: outfilebf='bufrdrops.prepbufr'
logical             :: found
EQUIVALENCE(acidr,sid)

call getarg(1,syntime)
call getarg(2,ltype)

if(ltype/="P".and.ltype/="H".and.ltype/="HY".and.ltype/="h".and.ltype/="p".and.ltype/="hy") then
   print *, ltype," is not and accepted level type"
   print *, "Enter P for pressure H for height or HY for hybrid"
   stop
endif

nargs=COMMAND_ARGUMENT_COUNT()
if(nargs.eq.3)then
   call getarg(3,cint)
   read(cint,'(I3)') lint

endif

read(syntime(1:4),'(I4)') isynyr
read(syntime(5:6),'(I2)') isynmnth
read(syntime(7:8),'(I2)') isynday
read(syntime(9:10),'(I2)') isynhr
read(syntime(3:10),'(I8)') itime
rsyntime=float(itime)

dtmax=10800

subsetbf="NC002104"
outsubset="ADPUPA"
unit_bfin=20
unit_bfout=21
unit_tblin=22
unit_tblout=23
unit_log=24
unit_list=26

open(unit_log,file='bufr_dump.log',form='formatted',status='new')

numfiles=0
endoffile=0
noerr=0
iost=0

open(unit_list,file='bufrdrops.filelist',form='formatted')
do while(endoffile==0)
   read(unit_list,'(A)',iostat=iost) infile
   if(iost.ne.noerr) then
      if(iost.lt.0) then
         endoffile=1
      else
         write(unit_log,*) 'error reading in dropsonde file list'
         exit
      endif
   else
      numfiles=numfiles+1
      filelist(numfiles)=infile
   endif
enddo

close(unit_list)

i=0
nbadtime=0
nbaddrift=0
nbadac=0
nbadsubset=0

open(unit=unit_tblout,file='prepobs_prep.bufrtable',action='read')
open(unit=unit_bfout,file=outfilebf,status='new',action='write',form='unformatted')
call openbf(unit_bfout,'OUT',unit_tblout)

do ifile = 1,numfiles

   infile=filelist(ifile)
   print *,'infile ',infile
   open(unit_bfin,file=trim(infile),action='read',form='unformatted')
   open(unit_tblin,file='bufrdrops.bufrtable')
   call openbf(unit_bfin,'IN',unit_bfin)
   call dxdump(unit_bfin,unit_tblin)
   call datelen(10)
   nmsg=0
   nlevs=0
   maxobs=0
!open file to count total number of messages

   msg_report: do while(ireadmg(unit_bfin,subset,idate) >= 0)
      if(subset==subsetbf)then
         nmsg=nmsg+1
         sb_report: do while(ireadsb(unit_bfin) == 0)
            maxobs = maxobs + 1
         enddo sb_report
      endif
   enddo msg_report

   write(unit_log,*) 'total number of bufr obs ',maxobs
   write(unit_log,*) 'total number of messages ', nmsg

   call closbf(unit_bfin)

   write(unit_log,*) 'reading in file #',ifile,infile
   open(unit_bfin,file=trim(infile),form='unformatted')
   call datelen(10)
   call openbf(unit_bfin,'IN',unit_bfin)

msg_report2: do while(ireadmg(unit_bfin,subset,idate) >= 0)
   i=i+1
   if(subset==subsetbf) then !This is a dropsonde
      drinfo=badval
      obsarraybf=badval
      call readns(unit_bfin,subset,idate,iret)
      call ufbint(unit_bfin,reltinfo,7,1,iret,reltime)

      iyr=int(reltinfo(2))
      imnth=int(reltinfo(3))
      iday=int(reltinfo(4))
      ihr=int(reltinfo(5))
      imn=int(reltinfo(6))
      isec=int(reltinfo(7))

      idate=(iyr*100 + imnth)*100 + iday
      if(iyr.gt.2000)then
         iyr=mod(iyr,2000)
      else
         iyr=mod(iyr,1900)
      endif

      tdiff=syntime2dt(isynhr,isynday,iday,ihr,imn,isec)

      if (abs(tdiff).le.dtmax)then !Drop is within time window
         write(unit_log,*) 'syntime ',isynhr,' droptime ',ihr,imn,isec
         write(unit_log,*) 'idate ',idate,' tdiff ',tdiff/3600
         call ufbint(unit_bfin,lauvals,3,1,iret,launchstr)
         rlat=lauvals(1)
         rlon=lauvals(2)
         rht=lauvals(3)
         lastlat=lat
         lastlon=lon

         call ufbint(unit_bfin,acid,1,1,iret,acstr)
         call ufbint(unit_bfin,rseqnum,1,1,iret,seqstr)

         if(acid(1:2)=="NO")then !Is a NOAA drop
            sid(1:2)="AA"
            sid(3:3)=acid(5:5)

            if(rseqnum.ge.10)then
               write(sid(4:5),'(I2)') INT(rseqnum)
            else
               sid(4:4)="0"
               write(sid(5:5),'(I1)') INT(rseqnum)
            endif
            sid(6:6)="A"

            call ufbint(unit_bfin,drinfo,5,mxlv,iret,drst)
            call ufbint(unit_bfin,obsarraybf,4,mxlv,iret,obstr)

            nlevs=drinfo(1,1) 
            write(unit_log,*) 'Drop #',i,' nlevs ',nlevs
            write(unit_log,*) 'Launch location ',rlat,rlon

            if(nlevs.gt.0.and.nlevs.ne.badval) then !Has drift info
               allocate(obsarray(5,nlevs),locarray(4,nlevs))
               obsarray=badval
               locarray=badval
               nbad=0
           
               ilev=0
               do lev=1,nlevs
                  if(drinfo(2,lev).ne.badval.and.drinfo(3,lev).ne.badval.and.drinfo(4,lev).ne.badval &
                     .and.drinfo(5,lev).ne.badval)then

                     ilev=ilev+1
                     obsarray(1,ilev)=drinfo(2,lev)/100.
                     if(obsarraybf(1,lev).ne.badval)then
                       obsarray(2,ilev)=obsarraybf(1,lev)
                     endif
                     if(obsarraybf(2,lev).ne.badval)then
                        obsarray(3,ilev)=obsarraybf(2,lev)
                     endif
                     if(obsarraybf(3,lev).ne.badval.and.obsarraybf(4,lev).ne.badval)then
                        !convert spd dir to u v wind components     
                        wdir=obsarraybf(3,lev)
                        wspd=obsarraybf(4,lev)
                        call ds2uv(wspd,wdir,u,v)
                        obsarray(4,ilev)=u
                        obsarray(5,ilev)=v
                     else
                        obsarray(4,ilev)=badval
                        obsarray(5,ilev)=badval
                     endif
                     if (drinfo(4,lev).ne.badval)then
                        lastlon=drinfo(4,lev) + rlon
                        locarray(2,ilev)=lastlon
                     else
                        locarray(2,ilev)=lastlon
                     endif
                     if(drinfo(5,ilev).ne.badval)then
                        lastlat=drinfo(5,lev) + rlat
                        locarray(3,ilev)=lastlat
                     else
                        locarray(3,ilev)=lastlat
                     endif
                     locarray(1,ilev)=drinfo(1,lev) + tdiff 
                     locarray(4,ilev)=drinfo(3,lev)/grav 
               else 
                  nbad=nbad+1 
            endif 
         enddo
   
         ptop=obsarray(1,ilev) 
         pbottom=obsarray(1,1) 
         htop=locarray(4,ilev) 
         hbottom=locarray(4,1)

         write(unit_log,'(A6,I3,A3,I4,A20,I3)') 'drop #',i,' : ',ilev,' good levels out of ',nlevs
         write(unit_log,'(A13,F7.2)') 'top pressure ',ptop 
         write(unit_log,'(A16,F7.2)') 'bottom pressure ',pbottom
         write(unit_log,'(A11,F10.2)') 'top height ',htop 
         write(unit_log,'(A14,F10.2)') 'bottom height ',hbottom

         if(trim(ltype)=="P".or.trim(ltype)=="p")then 
            numlevs=levcount(ltype,lint,ptop,pbottom) 
            allocate(levarray(numlevs))
            levarray=0.0 
            write(unit_log,*) numlevs,ltype,' levels' 
            call getlevs(ltype,lint,numlevs,ptop,pbottom,levarray)
         endif 
         if(trim(ltype)=="H".or.ltype=="HY".or.trim(ltype)=="h".or.trim(ltype)=="hy")then
            numlevs=levcount(ltype,lint,hbottom,htop) 
            write(unit_log,*) numlevs,ltype,' levels' 
            allocate(levarray(numlevs))
            levarray=0.0 
            call getlevs(ltype,lint,numlevs,hbottom,htop,levarray) 
         endif

         write(unit_log,*) numlevs,ltype,' levels' 
         write(unit_log,*) 'Level list:',levarray

         metadataw=badval 
         metadatam=badval 
         qcmark=2.0 
         qerror=2.0 
         perror=2.0

         metadatam(1)=acidr 
         metadataw(1)=acidr
         !standard observation type
         metadatam(5)=137 
         metadataw(5)=237 
         metadatam(6)=0 
         metadataw(6)=0 
         metadatam(7)=31 
         metadataw(7)=31 
         metadatam(8)=2
         metadataw(8)=2 
         metadatam(9)=96 
         metadataw(9)=96

         call openmb(unit_bfout,outsubset,isyntime)
         !Intepolate to pressure intervals specified by pint
         if(ltype=="P")then
            do i = 1,numlevs  
               pnow=levarray(i) 
               obsdatam=badval 
               obsdataw=badval 
               obsqcm=badval 
               obsqcw=badval
               obserrm=badval 
               obserrw=badval

               call getplev(obsarray,locarray,sdev,nlevs,ilev,pnow,found,rsecs,& 
                    latnow,lonnow,tnow,tdnow,hgtnow,unow,vnow)
               if(found)then
                  dtime=rsecs/3600     
                  obsdatam(1)=pnow
                  obsdataw(1)=pnow
                  !single level observation
                  obsdatam(8)=6.0 
                  obsdataw(8)=6.0 
                  obserrm(1)= sdev(1) 
                  obserrw(1)= sdev(1) 
                  metadatam(2)=lonnow
                  metadataw(2)=lonnow 
                  metadatam(3)=latnow 
                  metadataw(3)=latnow 
                  metadatam(4)=dtime
                  metadataw(4)=dtime

                  !Calculate specific humidity and relative humidity from Td
                  if(tnow.ne.missvalr.and.tdnow.ne.missvalr)then                      
                     qnow=td2q(tnow,tdnow,pnow)
                     obsdatam(2)=qnow*1000000.
                     obsqcm(2)=qcmark
                     obsusem=1
                     qnow=sdev(2)
                     obserrm(2)=qnow*100000.
                  else
                     obsqcm(2)=3.0
                     obsdatam(2)=badval
                     obserrm(2)=badval !missval?
               endif

               if(tnow.ne.missvalr)then
                  obsdatam(3)=tnow - 273.16
                  obsusem=1
                  call geterror(pnow,"t",error)
                     obserrm(3)=sdev(3) !terror
                     obsqcm(3)=qcmark
                     obsqcm(1)=qcmark
               else
                  obsusem=0
               endif

               if(hgtnow.ne.missvalr)then
                  obsdataw(4)=hgtnow
                  obsdatam(4)=hgtnow
                  obsqcm(4)=qcmark
                  obsqcw(4)=qcmark
               endif

               if(unow.ne.missvalr.and.vnow.ne.missvalr)then
                  obsdataw(5)=unow
                  obsdataw(6)=vnow
                  obsqcw(5)=qcmark
                  obsqcw(1)=qcmark
                  obsusew=1
                  call geterror(pnow,"u",error)
                  obserrw(4)=sdev(4) !use zerr location for u error
                  obserrw(5)=sdev(5) !v
               else
                  obsusew=0
               endif

               if(obsusem.eq.1)then
                  CALL UFBINT(unit_bfout,metadatam(:),9,1,iret,outheadstr)
                  CALL UFBINT(unit_bfout,obsdatam(:),9,1,iret,outobsstr)
                  CALL UFBINT(unit_bfout,obsqcm(:),7,1,iret,outqcstr)
                  CALL UFBINT(unit_bfout,obserrm(:),7,1,iret,outerrstr)
                  CALL WRITSB(unit_bfout)
               endif

               if(obsusew.eq.1)then
                  CALL UFBINT(unit_bfout,metadataw(:),9,1,iret,outheadstr)
                  CALL UFBINT(unit_bfout,obsdataw(:),9,1,iret,outobsstr)
                  CALL UFBINT(unit_bfout,obsqcw(:),7,1,iret,outqcstr)
                  CALL UFBINT(unit_bfout,obserrw(:),7,1,iret,outerrstr)
                  CALL WRITSB(unit_bfout)
               endif
            endif
         enddo
      endif

      if(ltype=="H".or.ltype=="HY")then
         do i = 1,numlevs
            hgtnow=levarray(i)
            obsdatam=badval
            obsdataw=badval
            obsqcm=badval
            obsqcw=badval
            obserrm=badval
            obserrw=badval

            call gethlev(obsarray,locarray,sdev,nlevs,ilev,hgtnow,found,rsecs,latnow,lonnow,tnow,tdnow,pnow,unow,vnow)
            if(found)then
               dtime= rsecs/3600;

               if(pnow.ne.missvalr)then
                  obsdatam(1)=pnow
                  obsdataw(1)=pnow
               else
                  obsusem=0
                  obsusew=0
               endif

               !single level observation
               obsdatam(8)=6.0
               obsdataw(8)=6.0
               obserrm(1)=sdev(1)
               obserrw(1)=sdev(1)
               metadatam(2)=lonnow
               metadataw(2)=lonnow
               metadatam(3)=latnow
               metadataw(3)=latnow
               metadatam(4)=dtime
               metadataw(4)=dtime
               !Calculate specific humidity and relative humidity from Td
               if(tnow.ne.missvalr.and.tdnow.ne.missvalr.and.pnow.ne.missvalr)then
               qnow=td2q(tnow,tdnow,pnow)
               obsdatam(2)=qnow*1000000.
               obsqcm(2)=qcmark
               obsusem=1
               qnow=sdev(2)
               obserrm(2)=qnow*1000000.
            else
               obsqcm(2)=3.0
               obsdatam(2)=badval
               obserrm(2)=badval
            endif

            if(tnow.ne.missvalr)then
               obsdatam(3)=tnow - 273.16
               obsusem=1
               call geterror(pnow,"t",error)
               obserrm(3)=sdev(3)!terror
               obsqcm(3)=qcmark
               obsqcm(1)=qcmark
            else
               obsusem=0
            endif

            obsdataw(4)=hgtnow
            obsdatam(4)=hgtnow
            obsqcm(4)=qcmark
            obsqcw(4)=qcmark

            if(unow.ne.missvalr.and.vnow.ne.missvalr)then
               obsdataw(5)=unow
               obsdataw(6)=vnow
               obsqcw(5)=qcmark
               obsqcw(1)=qcmark
               obsusew=1
               call geterror(pnow,"u",error)
               obserrw(4)=sdev(4)!use zerr location for u error
               obserrw(5)=sdev(5)!v
            else
               obsusew=0
            endif

            if(pnow.ne.missvalr) then
               obsdataw(1)=pnow
            else
               obsusem=0
               obsusew=0
            endif

            if(obsusem.eq.1)then
               CALL UFBINT(unit_bfout,metadatam(:),9,1,iret,outheadstr)
               CALL UFBINT(unit_bfout,obsdatam(:),9,1,iret,outobsstr)
               CALL UFBINT(unit_bfout,obsqcm(:),7,1,iret,outqcstr)
               CALL UFBINT(unit_bfout,obserrm(:),7,1,iret,outerrstr)
               CALL WRITSB(unit_bfout)
            endif

            if(obsusew.eq.1)then
               CALL UFBINT(unit_bfout,metadataw(:),9,1,iret,outheadstr)
               CALL UFBINT(unit_bfout,obsdataw(:),9,1,iret,outobsstr)
               CALL UFBINT(unit_bfout,obsqcw(:),7,1,iret,outqcstr)
               CALL UFBINT(unit_bfout,obserrw(:),7,1,iret,outerrstr)
               CALL WRITSB(unit_bfout)
            endif
         endif
      enddo
   endif

   deallocate(obsarray,locarray,levarray)
   call closmg(unit_bfout)

               !!!!!!!!end processing of good dropsonde
            else
               write(unit_log,*) 'Drop #',i,' No drift info acid = ',acid,' nlevs = ',nlevs
               nbaddrift=nbaddrift+1
            endif ! Drift info check
         else
            write(unit_log,*) 'Drop #',i,' Not a NOAA dropsonde acid = ',acid
            nbadac=nbadac+1
         endif ! NOAA drops check
      else
         write(unit_log,*) 'Drop #',i,' Out of time window '
         nbadtime=nbadtime+1
      endif ! Time window check
   else
      write(unit_log,*) 'Drop #', i,' Not a Dropsonde subset = ',subset
      nbadsubset=nbadsubset+1
   endif    ! Dropsonde check
enddo msg_report2

!open(unit=unit_tblout,file='bufr_table_out',action='read')
!open(unit=unit_bfout,file=outfilebf,status='old',action='write',form='unformatted')
!call openbf(unit_bfout,'APN',unit_tblout)
call closbf(unit_bfin)
close(unit_bfin)           
write(unit_log,*) nbadsubset,' Subsets rejected'
write(unit_log,*) nbadac,' non-NOAA dropsondes'
write(unit_log,*) nbadtime,' drops out of time window'
write(unit_log,*) nbaddrift,' drops with no levels count'

enddo ! loop over bufr files


close(unit_tblout)
call closbf(unit_bfout)

ENDPROGRAM

