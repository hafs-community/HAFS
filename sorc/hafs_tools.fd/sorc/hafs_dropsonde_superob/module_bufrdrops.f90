module bufrdrops

contains
!_______________________________________________________
!FUNCTIONS
!_______________________________________________________

integer function levcount(ltype,lint,lmin,lmax)
!number of superob levels given range and level type

implicit none

character(len=2),intent(in)   :: ltype
integer,intent(in)            :: lint
integer                       :: nlevs,i
real,intent(in)               :: lmin,lmax
real                          :: hybridlevs(88)

data hybridlevs /0.,50.,100.,150., 200., 250.,300.,350.,400.,450.,500.,600.,700.,800.,900.,1000.,&
   1150.,1300.,1450.,1600.,1750.,1900.,2050.,2200.,2350.,2500.,2650.,2800.,3000.,3200.,3400.,&
   3600.,3800.,4000.,4200.,4400.,4600.,4800.,5000.,5200.,5400.,5600.,5800.,6000.,6200.,6400.,&
   6600.,6800.,7050.,7300.,7550.,7800.,8050.,8300.,8550.,8800.,9050.,9300.,9550.,9800.,10100.,&
   10400.,10700.,11000.,11300.,11600.,11900.,12200.,12500.,12800.,13100.,13400.,13700.,14000.,&
   14300.,14600.,14900.,15200.,15500.,15800.,16100.,16400.,16700.,18000.,18300.,18600.,18900.,&
      19100. /

select case(ltype)
   case("HY")
      do i = 1,88
         if(lmax.lt.hybridlevs(i))then
            nlevs=i
         endif
         nlevs=nlevs+1
      enddo
   case("P")
      nlevs=int((lmax - lmin)/lint)
      nlevs=nlevs+2
   case("H")
      nlevs=floor((lmax - lmin)/lint)
      nlevs=nlevs+2
end select

levcount=nlevs

end function levcount


integer function isyntime(imnth,iday,ihr)
!
implicit none

integer,intent(in)           :: imnth,iday,ihr
integer                      :: day, mnth
character(len=2)             :: syntime


!Find appropriate 6 hour synoptic time
select case(ihr)
   case(0:2)
      isyntime=0
      syntime="00"
      day=iday
      mnth=imnth
   case(21:23) 
      isyntime=0
      syntime="00"
      day=iday + 1
      mnth=imnth
      if(day.gt.30) then
         if(imnth.eq.6.or.imnth.eq.9.or.imnth.eq.11)then
            mnth = imnth +1
            day=iday - 30
         endif
      endif
      if(iday.gt.31)then
         mnth=imnth + 1
         day=iday - 31
         mnth=imnth
         if(imnth.eq.13) mnth = 1
      endif
   case(3:8) 
      isyntime=6
      syntime="06"
      day=iday
      mnth=imnth
   case(9:14) 
      isyntime=12
      syntime="12"
      day=iday
      mnth=imnth

   case(15:20) 
      isyntime=18
      syntime="18"
      day=iday
      mnth=imnth
end select

end function isyntime

real function syntime2dt(isyntime,synday,iday,ihr,imn,isec)

implicit none

integer,intent(in)  :: isyntime,synday,iday,ihr,imn,isec
integer             :: itimesecs,isyntimesecs
real                :: dtsecs

isyntimesecs=isyntime*3600
itimesecs=(ihr*3600)+(imn*60)+isec

if(isyntime.ne.0)then
   dtsecs=float(itimesecs - isyntimesecs)
else
   if(synday.ne.iday)then
      dtsecs=float(itimesecs) - (24*3600)
   else
      dtsecs=float(itimesecs - isyntimesecs)
   endif
endif


syntime2dt=dtsecs

end function syntime2dt

real function td2q(temp,tempd,press)

implicit none

real,intent(in)  :: temp,tempd,press
real             :: e, es,rh
real             :: q

es=6.112*exp(17.67*(temp-273.16)/(temp-29.66))
e=6.112*exp((17.67*(tempd-273.16))/(tempd-29.66))
rh=e/es
q=(0.622*e)/(press - (0.378*e))

td2q=q

end function td2q

!_____________________________________________________________
!SUBROUTINES
!_____________________________________________________________
subroutine getlevs(ltype,lint,nlevs,lmin,lmax,levarray)
implicit none

character(len=2),intent(in)   :: ltype
integer,intent(in)            :: lint, nlevs
integer                       :: nplevs,nhlevs,i
real                          :: pbottom,htop
real,intent(in)               :: lmin,lmax
real                          :: hybridlevs(88)
real,intent(inout)              :: levarray(nlevs)

data hybridlevs /0.,50.,100.,150.,200.,250.,300.,350.,400.,450.,500.,600.,700.,800.,900.,1000.,&
   1150.,1300.,1450.,1600.,1750.,1900.,2050.,2200.,2350.,2500.,2650.,2800.,3000.,3200.,3400.,&
   3600.,3800.,4000.,4200.,4400.,4600.,4800.,5000.,5200.,5400.,5600.,5800.,6000.,6200.,6400.,&
   6600.,6800.,7050.,7300.,7550.,7800.,8050.,8300.,8550.,8800.,9050.,9300.,9550.,9800.,10100.,&
   10400.,10700.,11000.,11300.,11600.,11900.,12200.,12500.,12800.,13100.,13400.,13700.,14000.,&
   14300.,14600.,14900.,15200.,15500.,15800.,16100.,16400.,16700.,18000.,18300.,18600.,18900.,&
   19100./

select case(trim(ltype))
   
   case("HY")
      levarray(1:nlevs)=hybridlevs(1:nlevs)
   case("P")
      pbottom=ceiling(lmax)
      do i = 1,nlevs
         levarray(i)= pbottom - (i-1)*lint
      enddo
   case("H")
      htop=float((nlevs - 1 ) * lint)
      do i = 1,nlevs
         levarray(i)=htop - (i-1)*lint
      enddo   
end select

end subroutine getlevs

subroutine ds2uv(wspd,wdir,u,v)
!extract u and v components from wind speed and "from" direction
implicit none
real,intent(in)    :: wspd, wdir
real               :: rdir, deg2rad, pi2, pi32,u,v
real,parameter     :: pi=3.14152

deg2rad=pi/180.
pi32=(3.0*pi)/2.0
pi2=2.0*pi

rdir=wdir*deg2rad
rdir=pi32-rdir
if(rdir.lt.0) then
   rdir=rdir+pi2
endif
u=wspd*cos(rdir)
v=wspd*sin(rdir)

end subroutine ds2uv

subroutine getplev(obs,locs,sdev,nlevs,ilev,lev,found,rsecs,lat,lon,temp,rh,gphgt,u,v)
!returns observation data for a given pressure level

implicit none
integer, intent(in)  :: ilev,nlevs
real, intent(in)     :: lev
logical, intent(out) :: found
real*8, intent(in)   :: obs(5,nlevs),locs(4,nlevs)
real, intent(out)    :: sdev(5),rsecs,lat,lon,temp,rh,gphgt,u,v
real*8               :: badval
real                 :: tdnow, tnow,pnow, e, es, q, qave
real                 :: missval,levnow,minp,maxp,obstemp(7,50)
integer              :: nsecs,nu,nrh,nt,nlat,nht,idate
integer              :: itime,i

found=.FALSE.
minp=lev-5
maxp=lev+5
badval=100000000000.0D0
missval=-999.0

u=0.0
v=0.0
temp=0.0
rh=0.0
lat=0.0
lon=0.0
gphgt=0.0
nt=0
nrh=0
nu=0
nht=0
nlat=0
nsecs=0
rsecs=0
obstemp=0.0
sdev=0.0

do i = 1,ilev
  
   levnow=obs(1,i)
   if(levnow.ge.minp.and.levnow.le.maxp)then
      found=.TRUE.
      if(obs(2,i).ne.badval)then
        temp=temp+obs(2,i)
        nt=nt+1
        obstemp(1,nt)=obs(1,i)
        obstemp(3,nt)=obs(2,i)
      endif
      if(obs(3,i).ne.badval)then
         rh=rh+obs(3,i)
         nrh=nrh+1
         obstemp(2,nrh)=obs(3,i)
         obstemp(6,nrh)=obs(2,i)
         obstemp(7,nrh)=obs(1,i)
      endif
      if(obs(4,i).ne.badval.and.obs(5,i).ne.badval)then
         u=u+obs(4,i)
         v=v+obs(5,i)
         nu=nu+1
         obstemp(4,nu)=obs(4,i)
         obstemp(5,nu)=obs(5,i)
      endif

      if(locs(4,i).ne.badval)then
         gphgt=gphgt+locs(4,i)
         nht=nht+1
      endif

      rsecs=rsecs+locs(1,i)
      nsecs=nsecs+1

      if(locs(2,i).ne.badval.and.locs(3,i).ne.badval)then
         lat=lat+locs(3,i)
         lon=lon+locs(2,i)
         nlat=nlat+1
         found=.TRUE.
      endif
   endif
enddo

if(nu.gt.0)then
    u=u/nu
    v=v/nu
    do i = 1, nu
       sdev(4)=sdev(4)+((obstemp(4,i)-u)**2)
       sdev(5)=sdev(5)+((obstemp(5,i)-v)**2)
    enddo
    sdev(4)=sqrt(sdev(4)/nu)
    sdev(5)=sqrt(sdev(5)/nu)
else
   u=missval
   v=missval
   sdev(4)=badval
   sdev(5)=badval
endif

if(nt.gt.0)then
   temp=temp/nt
   do i = 1,nt
      sdev(1)=sdev(1)+((obstemp(1,i)-lev)**2)
      sdev(3)=sdev(3)+((obstemp(3,i)-temp)**2)
   enddo
   sdev(1)=sqrt(sdev(1)/nt)
   sdev(3)=sqrt(sdev(3)/nt)
else
   temp=missval
   sdev(1)=badval
   sdev(3)=badval
endif

if(nrh.gt.0)then
   rh=rh/nrh
   es=6.112*exp(17.67*(temp-273.16)/(temp-29.66))
   e=6.112*exp(17.67*(rh-273.16)/(rh-29.66))
   qave=(0.622*e)/(lev - (0.378*3))
   do i = 1,nrh
      tnow=obstemp(6,i)
      tdnow=obstemp(2,i)
      pnow=obstemp(7,i)
      es=6.112*exp(17.67*(tnow-273.16)/(tnow-29.66))
      e=6.112*exp(17.67*(tdnow-273.16)/(tdnow-29.66))
      q=(0.622*e)/(pnow - (0.378*e))
      sdev(2)=sdev(2)+((q-qave)**2)
   enddo
   sdev(2)=sqrt(sdev(2)/nrh)
else
   rh=missval
   sdev(2)=badval
endif

if(nht.gt.0)then
   gphgt=gphgt/nht
endif

if(nlat.gt.0)then
   lon=lon/nlat
   lat=lat/nlat
endif

if(nsecs.gt.0)then
   rsecs=rsecs/nsecs
endif

if(nu.eq.0.and.nt.eq.0.and.nrh.eq.0) found=.FALSE.

end subroutine getplev

subroutine gethlev(obs,locs,sdev,nlevs,ilev,lev,found,rsecs,lat,lon,temp,rh,press,u,v)
!returns observation data for a given height level

implicit none
integer, intent(in)  :: ilev,nlevs
real, intent(in)     :: lev
logical, intent(out) :: found
real*8, intent(in)   :: obs(5,nlevs),locs(4,nlevs)
real, intent(out)    :: sdev(5),rsecs,lat,lon,temp,rh,press,u,v
real*8               :: badval
real                 :: obstemp(7,50),missval,levnow,minh,maxh
real                 :: tnow,tdnow,pnow,e,es,q,qave
integer              :: nsecs,nu,nrh,nt,nlat,np,idate
integer              :: itime,i

found=.FALSE.
minh=lev-15
maxh=lev+15
badval=100000000000.0D0
missval=-999.0

u=0.0
v=0.0
temp=0.0
rh=0.0
lat=0.0
lon=0.0
press=0.0
np=0
nt=0
nrh=0
nu=0
nlat=0
nsecs=0
rsecs=0
obstemp=0.0
sdev=0.0

do i = 1,ilev

   levnow=locs(4,i)
   if(levnow.ge.minh.and.levnow.le.maxh)then
      found=.TRUE.
      if(obs(2,i).ne.badval)then
        temp=temp+obs(2,i)
        nt=nt+1
        obstemp(3,nt)=obs(2,i)
      endif
      if(obs(3,i).ne.badval)then
         rh=rh+obs(3,i)
         nrh=nrh+1
         obstemp(2,nrh)=obs(3,i)
         obstemp(6,nrh)=obs(2,i)
         obstemp(7,nrh)=obs(1,i)
      endif
      if(obs(4,i).ne.badval.and.obs(5,i).ne.badval)then
         u=u+obs(4,i)
         v=v+obs(5,i)
         nu=nu+1
         obstemp(4,nu)=obs(4,i)
         obstemp(5,nu)=obs(5,i)
      endif

      if(obs(1,i).ne.badval)then
         press=press+obs(1,i)
         np=np+1
      endif

      rsecs=rsecs+locs(1,i)
      nsecs=nsecs+1

      if(locs(2,i).ne.badval.and.locs(3,i).ne.badval)then
         lat=lat+locs(3,i)
         lon=lon+locs(2,i)
         nlat=nlat+1
         found=.TRUE.
      endif
   endif
enddo

if(nu.gt.0)then
   u=u/nu
   v=v/nu
   do i = 1,nu
      sdev(4)=sdev(4)+((obstemp(4,i)-u)**2)
      sdev(5)=sdev(5)+((obstemp(5,i)-v)**2)
   enddo
   sdev(4)=sqrt(sdev(4)/nu)
   sdev(5)=sqrt(sdev(5)/nu)
else
   u=missval
   v=missval
   sdev(4)=badval
   sdev(5)=badval
endif

if(nt.gt.0)then
   temp=temp/nt
   do i = 1,nt
      sdev(3)=sdev(3)+((obstemp(3,i)-temp)**2)
   enddo
   sdev(3)=sqrt(sdev(3)/nt)
else
   temp=missval
   sdev(3)=badval
endif

if(np.gt.0)then
   press=press/np
   do i = 1,np
      sdev(1)=sdev(1)+((obstemp(1,i)-press)**2)
   enddo
   sdev(1)=sqrt(sdev(1)/np)
else
   press=missval
   sdev(1)=badval
endif 

if(nrh.gt.0)then
   rh=rh/nrh
   es=6.112*exp(17.67*(temp-273.16)/(temp-29.66))
   e=6.112*exp(17.67*(rh-273.16)/(rh-29.66))
   qave=(0.622*e)/(press - (0.378*3))
   do i = 1,nrh
      tnow=obstemp(6,i)
      tdnow=obstemp(2,i)
      pnow=obstemp(7,i)
      es=6.112*exp(17.67*(tnow-273.16)/(tnow-29.66))
      e=6.112*exp(17.67*(tdnow-273.16)/(tdnow-29.66))
      q=(0.622*e)/(pnow - (0.378*e))
      sdev(2)=sdev(2)+((q-qave)**2)
   enddo
   sdev(2)=sqrt(sdev(2)/nrh)
else
   rh=missval
   sdev(2)=missval
endif

if(nlat.gt.0)then
   lon=lon/nlat
   lat=lat/nlat
endif

if(nsecs.gt.0)then
   rsecs=rsecs/nsecs
endif

if(nu.eq.0.and.nt.eq.0.and.nrh.eq.0) found=.FALSE.
if(np.eq.0) found=.FALSE.

end subroutine gethlev

subroutine geterror(pressure,obstype,error)
!returns default error from GSI error tables

real,intent(in)              :: pressure
real,intent(out)             :: error
real                         :: uverror(33),plevs(33),terror(33)
integer                      :: ilev
character(len=1),intent(in)  :: obstype
data plevs/1100,1050,1000,950,900,850,800,750,700,650,600,550,500,450,400,350,&
           300,250,200,150,100,75,50,40,30,20,10,5,4,3,2,1,0/
!FROM GLOBAL GSI TABLE
data terror/1.2, 1.2, 1.2, 1.1, 0.9, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8,&
            0.8, 0.8, 0.8, 0.9, 1.2, 1.2, 1.0, 0.8, 0.8, 0.9, 0.95, 1.0, 1.25,&
            1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5/
data uverror/2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.5, 2.6, 2.7, 2.8,&
             2.95, 3.1, 3.25, 3.4, 3.175, 2.95, 2.725, 2.5, 2.6, 2.7, 2.7, 2.7,&
             2.7, 2.7, 2.7,2.7, 2.7, 2.7, 2.7, 2.7/


if(obstype=="U".or.obstype=="V".or.obstype=="u".or.obstype=="v")then
   do i = 1,32
      if(pressure.le.plevs(i).and.pressure.gt.plevs(i+1))then
         error=uverror(i)
         exit
      endif
   enddo
endif

if(obstype=="T".or.obstype=="t")then
   do i = 1,33
      if(pressure.le.plevs(i).and.pressure.gt.plevs(i+1))then
         error=terror(i)
         exit
      endif
   enddo
endif
end subroutine geterror

end module bufrdrops
