      program rtofs_obs_read

!     This program calls 5 subroutines: read_sst, sst_qc, read_ssh,
!     read_profile (doing qc also), read_sss, sss_qc 

      real      latmin,latmax,lonmin,lonmax
      character argv*12

      call getarg(1, argv)


!      latmin=1.0
!      latmax=50.0
!      lonmin=-100.0
!      lonmax=-7.0

      open(1,file='domain.txt',status='old')
      read(1,*) latmin, latmax, lonmin, lonmax
      close(1)
      print*,latmin, latmax, lonmin, lonmax

      if(argv.eq.'read_sst') then
         print*, 'read_sst'
         call read_sst(latmin,latmax,lonmin,lonmax)
      endif

      if(argv.eq.'sst_qc') then
         print*, 'sst_qc'
         call sst_qc
      endif

      if(argv.eq.'read_ssh') then
         print*, 'read_ssh'
         call read_ssh(latmin,latmax,lonmin,lonmax)
      endif

      if(argv.eq.'read_profile') then
         print*, 'read_profile'
         call read_profile(latmin,latmax,lonmin,lonmax)
      endif

      if(argv.eq.'read_sss') then
         print*, 'read_sss'
         call read_sss(latmin,latmax,lonmin,lonmax)
      endif

      if(argv.eq.'sss_qc') then
         print*, 'sss_qc'
         call sss_qc
      endif

      stop
      end 

!     ###### read RTOFS SST binary ######

      subroutine read_sst(latmin,latmax,lonmin,lonmax)

      real      latmin,latmax,lonmin,lonmax
      character, allocatable:: ob_dtg(:) *12
      REAL, ALLOCATABLE:: ob_age(:),ob_bias(:),ob_err(:)
      REAL, ALLOCATABLE:: ob_flg(:),ob_lat(:),ob_lon(:),ob_qc(:)
      REAL, ALLOCATABLE:: ob_sst(:)
      INTEGER, ALLOCATABLE:: ob_typ(:),ob_wm(:)

      character winstart *12, winend *12
      integer n_read
      integer vrsn
      integer i

      open(10,file='sst.bin',form='unformatted')

      n_sst = 0
      write(*,*) 'reading SST'

      read (10) n_read, n_chn, vrsn
        
      if (n_read .gt. 0) then

      allocate (ob_age(n_read))
      allocate (ob_bias(n_read))
      allocate (ob_dtg(n_read))
      allocate (ob_err(n_read))
      allocate (ob_flg(n_read))
      allocate (ob_lat(n_read))
      allocate (ob_lon(n_read))
      allocate (ob_qc(n_read))
      allocate (ob_sst(n_read))
      allocate (ob_typ(n_read))
      allocate (ob_wm(n_read))

      read (10) ob_age(1:n_read)
      read (10) ob_bias(1:n_read)
      read (10) ob_dtg(1:n_read)
      read (10) ob_err(1:n_read)
      read (10) ob_flg(1:n_read)
      read (10) ob_lat(1:n_read)
      read (10) ob_lon(1:n_read)
      read (10) ob_qc(1:n_read)
      read (10) ob_sst(1:n_read)
      read (10) ob_typ(1:n_read)
      read (10) ob_wm(1:n_read)

      open(21,file='window.txt',status='old')
      read(21,'(a12)') winstart
      read(21,'(a12)') winend
      close(21)

      open(20,file='sst.txt',status='unknown')

      do i=1, n_read

!     HAT10
      if(ob_lat(i).ge.latmin.and.ob_lat(i).le.latmax.and.
     6   ob_lon(i).ge.lonmin.and.ob_lon(i).le.lonmax) then

!     time window
      if(ob_dtg(i).ge.winstart.and.ob_dtg(i).le.winend) then

      if(ob_dtg(i)(11:12).gt."59") ob_dtg(i)(11:12)="59"

!       Missing ob_qc
      if(ob_qc(i).ge.0.0.and.ob_qc(i).lt.10.0) then

      write(20,25) ob_dtg(i),ob_lat(i),ob_lon(i),
     6               ob_sst(i),ob_err(i),ob_qc(i)

      endif ! End of missing ob_qc
      endif ! End of time window 
      endif ! End of HAT10

      enddo

25    format(a12,1x,2(f8.3,1x),3(1x,f6.3))
      close(20)

      endif

      return
      end

!     ###### read RTOFS SSH binary ######

      subroutine read_ssh(latmin,latmax,lonmin,lonmax)
        
      real      latmin,latmax,lonmin,lonmax
      integer   n_lvl
      integer   n_read
      integer   vrsn

      real,     allocatable :: ob_age (:)
      integer,  allocatable :: ob_cyc (:)
      character,allocatable :: ob_dtg (:) * 14
      real,     allocatable :: ob_lat (:)
      real,     allocatable :: ob_lon (:)
      real,     allocatable :: ob_qc (:)
      integer,  allocatable :: ob_ltc (:)
      character,allocatable :: ob_rcpt (:) * 14
      integer,  allocatable :: ob_sat (:)
      integer,  allocatable :: ob_smpl (:)
      real,     allocatable :: ob_ssh (:)
      integer,  allocatable :: ob_trck (:) 
        
      character winstart *12, winend *12

      open(10,file='ssh.bin',form='unformatted')

      write(*,*) 'reading SSH'

      read (10) n_read, n_lvl, vrsn

      if (n_read .gt. 0) then

      allocate (ob_age(n_read))
      allocate (ob_cyc(n_read))
      allocate (ob_lat(n_read))
      allocate (ob_lon(n_read))
      allocate (ob_qc(n_read))
      allocate (ob_sat(n_read))
      allocate (ob_smpl(n_read))
      allocate (ob_ssh(n_read))
      allocate (ob_trck(n_read))
      allocate (ob_ltc(n_read))
      allocate (ob_dtg(n_read))
      allocate (ob_rcpt(n_read))

      read (10) ob_age(1:n_read)
      read (10) ob_cyc(1:n_read)
      read (10) ob_lat(1:n_read)
      read (10) ob_lon(1:n_read)
      read (10) ob_qc(1:n_read)
      read (10) ob_sat(1:n_read)
      read (10) ob_smpl(1:n_read)
      read (10) ob_ssh(1:n_read)
      read (10) ob_trck(1:n_read)
      read (10) ob_ltc(1:n_read)
      read (10) ob_dtg(1:n_read)
      read (10) ob_rcpt(1:n_read)

      open(21,file='window.txt',status='old')
      read(21,'(a12)') winstart
      read(21,'(a12)') winend
      close(21)

      open(20,file='ssh.txt',status='unknown')
        
      do i=1, n_read

!     HAT10
      if(ob_lat(i).ge.latmin.and.ob_lat(i).le.latmax.and.
     6     ob_lon(i).ge.lonmin.and.ob_lon(i).le.lonmax) then
!     HAT10

!     time window
      if(ob_dtg(i)(1:12).ge.winstart.and.ob_dtg(i)(1:12).le.
     6   winend) then

      if(ob_dtg(i)(13:14).gt."59") ob_dtg(i)(13:14)="59"
      if(ob_dtg(i)(11:12).gt."59") ob_dtg(i)(11:12)="59"

      write(20,25) ob_dtg(i)(1:12),ob_lat(i),ob_lon(i),
     6             ob_ssh(i),0.1,ob_qc(i)

      endif ! End of time window
      endif ! End of HAT10

      enddo

25    format(a12,1x,2(f8.3,1x),3(1x,f6.3))
      close(20)

      endif

      return
      end

!     ###### read RTOFS PROFILE binary ######

      subroutine read_profile(latmin,latmax,lonmin,lonmax)
c     implicit none
c
c     ..define maximum number daily files
c
      real      latmin,latmax,lonmin,lonmax,dx,dy,ds,lat,lon
      real      tqc,sqc,ohc,ohc_max,dss
      integer   i, n,ns,k,kk,z,zz,m,mm
      character winstart *12, winend *12,dtg*12,sgn*7
c
      integer    UNIT
      parameter (UNIT = 20)

c     Binary data
      logical   exist
      integer   n_prf,len,len_data,mx_depth,mx_obs,n_dup,n_files
      integer   n_in,n_lev,n_out,n_rpl,new_file,old_vrsn,total,vrsn
      real      qc_lmt

      real,     allocatable :: prf_btm (:)
      real,     allocatable :: prf_lat (:), latt(:)
      real,     allocatable :: prf_lon (:), lonn(:)
      integer,  allocatable :: prf_ls (:)
      integer,  allocatable :: prf_lt (:)
      real,     allocatable :: prf_sal_typ (:)
      real,     allocatable :: prf_sqc (:)
      integer,  allocatable :: prf_tmp_typ (:)
      real,     allocatable :: prf_tqc (:)

      real,     allocatable :: prf_lvl (:,:)
      real,     allocatable :: prf_sal (:, :)
      real,     allocatable :: prf_sal_err (:, :)
      real,     allocatable :: prf_sprb (:, :)
      real,     allocatable :: prf_tmp (:, :)
      real,     allocatable :: prf_tmp_err (:, :)
      real,     allocatable :: prf_tprb (:, :)
      real,     allocatable :: prf_cssd (:, :)
      real,     allocatable :: prf_ctsd (:, :)
      integer,  allocatable :: ob_flg (:, :)

      character,allocatable :: prf_dtg (:) * 12
      real,     allocatable :: prf_rct (:)
      character,allocatable :: prf_sgn (:) * 7

c     superobbing using average
      REAL, ALLOCATABLE :: nsum(:)
      REAL, ALLOCATABLE :: hsum(:), hAve(:), z_l(:), z_i(:)
      REAL, ALLOCATABLE :: tsum(:), terrsum(:), tAve(:), terrAve(:)
      REAL, ALLOCATABLE :: ssum(:), serrsum(:), sAve(:), serrAve(:)

c     RTOFS binary
      open (UNIT, file='profile.bin', status='old',form='unformatted')

      read (UNIT) n_in, n_lev, old_vrsn
      write(*,*) 'LOCATION LEVEL version',n_in,n_lev,old_vrsn

      if (n_in .gt. 0) then
      write(*,*) 'starting allocate ......'
      allocate (prf_btm (n_in))
      allocate (prf_lat (n_in))
      allocate (prf_lon (n_in))
      allocate (prf_ls (n_in))
      allocate (prf_lt (n_in))
      allocate (prf_sal_typ (n_in))
      allocate (prf_sqc (n_in))
      allocate (prf_tmp_typ (n_in))
      allocate (prf_tqc (n_in))

      allocate (prf_lvl (n_lev,n_in))
      allocate (prf_sal (n_lev, n_in))
      allocate (prf_sal_err (n_lev, n_in))
      allocate (prf_sprb (n_lev, n_in))
      allocate (prf_tmp (n_lev, n_in))
      allocate (prf_tmp_err (n_lev, n_in))
      allocate (prf_tprb (n_lev, n_in))
      allocate (prf_cssd (n_lev, n_in))
      allocate (prf_ctsd (n_lev, n_in))
      allocate (ob_flg (n_lev, n_in))

      allocate (prf_dtg (n_in))
      allocate (prf_rct (n_in))
      allocate (prf_sgn (n_in))

      allocate (latt(n_in))
      allocate (lonn(n_in))

!     READING
      write(*,*) 'starting read ......'
      read (unit) prf_btm(1:n_in)
      read (unit) prf_lat(1:n_in)
      read (unit) prf_lon(1:n_in)
      read (unit) prf_ls(1:n_in)
      read (unit) prf_lt(1:n_in)
      read (unit) prf_sal_typ(1:n_in)
      read (unit) prf_sqc(1:n_in)
      read (unit) prf_tmp_typ(1:n_in)
      read (unit) prf_tqc(1:n_in)
     
      do n = 1, n_in
      read (unit) prf_lvl(1:prf_lt(n),n)
      read (unit) prf_sal(1:prf_lt(n),n)
      read (unit) prf_sal_err(1:prf_lt(n),n)
      read (unit) prf_sprb(1:prf_lt(n),n)
      read (unit) prf_tmp(1:prf_lt(n),n)
      read (unit) prf_tmp_err(1:prf_lt(n),n)
      read (unit) prf_tprb(1:prf_lt(n),n)
      read (UNIT) ob_clm_sal
      read (unit) prf_cssd(1:prf_lt(n),n)
      read (UNIT) ob_clm_tmp
      read (unit) prf_ctsd(1:prf_lt(n),n)
      read (unit) ob_flg(1:prf_lt(n),n)
      enddo

      read (unit) prf_dtg(1:n_in)
      read (unit) prf_rct(1:n_in)
      read (unit) prf_sgn(1:n_in)
     
      close (unit)
      endif
      write(*,*) 'finish read profile.bin ......'

!      do n=1, n_in
!      write(*,*) prf_sgn(n)
!      enddo
!      stop

!     time window
      open(21,file='window.txt',status='old')
      read(21,'(a12)') winstart
      read(21,'(a12)') winend
      close(21)

      open(30,file='profile.txt',status='unknown')

!     #################### PROFILE LOCATIONS ##########
      ns=0
      do n=1, n_in

      lat=prf_lat(n)
      lon=prf_lon(n) 
      dtg=prf_dtg(n)
      tqc=prf_tqc(n)
      sqc=prf_sqc(n)
      kk=prf_lt(n)
      sgn=prf_sgn(n)

!     HAT10/NHC domain. cycle from PROFILE LOCATIONS
      if(lat.le.latmin.or.lat.ge.latmax.or.
     6   lon.le.lonmin.or.lon.ge.lonmax) cycle
!     time window. cycle from PROFILE LOCATIONS
      if(dtg.lt.winstart.or.dtg.gt.winend) cycle

      if(tqc.gt.99.0) tqc=99.9 
      if(sqc.gt.99.0) sqc=99.9 

!     ############## THINNING locations, rejected if ds<0.2 degree
      dss=0.5
!     GoM, Puerto Rico
      if(lat.lt.35.0.and.lat.gt.15.0.and.
     6   lon.lt.-60.0.and.lon.gt.-100.0) then
      dss=2.0
!      print*,'SIGN',sgn
      endif

!     print*,lat,lon,dss,ns
      n_dup=0
      do i=1,ns
      dx=lonn(i)-lon
      dy=latt(i)-lat
      ds=sqrt(dx*dx+dy*dy)
      if(ds.lt.dss) then
!      if(dss.gt.0.3) write(*,'(a7,1x,6(f7.3,1x))') sgn,lonn(i),lon,
!     6               latt(i),lat,ds,dss
!      print*,'N_DUP',sgn 
      n_dup=n_dup+1
      cycle                ! cycle from neighbor search
      endif
      enddo
      if(n_dup.gt.0) cycle ! cycle from PROFILE LOCATIONS

!     BLOBS
!      if(lat.lt.34.0.and.lat.gt.30.0.and.
!     6   lon.lt.-72.0.and.lon.gt.-78.0) then
!      write(*,*) 'blobs',lat,lon
!      do k=1, 5 !kk !  profile depth level
!      write(*,'(5(f9.4,1x))') lat,lon,prf_tmp(k,n),prf_sal(k,n),
!     6                        prf_lvl(k,n)
!      end do
!      cycle
!      endif

!     ###################### vertical SUPEROBBING 
      zz=(prf_lvl(kk,n)-prf_lvl(1,n))/2+2 ! box, grid size 2 m

      if(zz.lt.10) cycle  ! few levels. cycle from PROFILE LOCATIONS
      allocate (hsum(zz), hAve(zz), nsum(zz), z_i(zz), z_l(zz))
      allocate (tsum(zz), terrsum(zz), ssum(zz), serrsum(zz))
      allocate (tAve(zz), terrAve(zz), sAve(zz), serrAve(zz))

      hsum(1:zz)=0
      tsum(1:zz)=0
      terrsum(1:zz)=0
      ssum(1:zz)=0
      serrsum(1:zz)=0
      nsum(1:zz)=0

      do k=1, kk !  PROFILE DEPTH LEVELS
!     Salinity QC. cycle from profile depth level
      if(prf_sal(k,n).lt.10.0.or.prf_sal(k,n).gt.40.0) cycle !from lvl

!     assign to box based on height, same k for 6,7.999
      z=int((prf_lvl(k,n)-prf_lvl(1,n))/2)+1
      hsum(z)=hsum(z)+prf_lvl(k,n)
      tsum(z)=tsum(z)+prf_tmp(k,n)
      terrsum(z)=terrsum(z)+prf_tmp_err(k,n)
      ssum(z)=ssum(z)+prf_sal(k,n)
      if(prf_sal_err(k,n).eq.-999.0) prf_sal_err(k,n)=1.0
      serrsum(z)=serrsum(z)+prf_sal_err(k,n)
      nsum(z)=nsum(z)+1
      enddo            ! PROFILE DEPTH LEVELS

!     +++++++++FOR superobbing, mm box IS NO EMPTY 
      mm=0
      do z=1,zz        ! BOX
!     nsum(k) can be zero (empty box)
!     Average of a box if obs available
      if(nsum(z).gt.0) then
      mm=mm+1
      tAve(mm)=tsum(z)/nsum(z)
      terrAve(mm)=terrsum(z)/nsum(z)
      sAve(mm)=ssum(z)/nsum(z)
      serrAve(mm)=serrsum(z)/nsum(z)
      hAve(mm)=hsum(z)/nsum(z)
      z_l(mm)=hAve(mm)
      endif
      enddo            ! BOX

!     ############## OHC check
!     interface depth
      z_i(1)=0.0
      do m=1,mm-1
      z_i(m)=(z_l(m)+z_l(m+1))/2.0
      enddo
!     check OHC
!     rho = 1025       # sea water density kg/m-3
!     cp  = 3850       # sea water specific heat J/(kg K)
!     1025*3850*10^-7 = 0.394625

      ohc=0
      do m=1,mm-1
      if(tAve(m).gt.26.0) then
      ohc=ohc+0.394625*(tAve(m)-26.0)*(z_i(m+1)-z_i(m))      
      endif
      enddo

      if(lon.gt.-100.0) then
      ohc_max=150.0
      else 
      ohc_max=250.0
      endif

      if(ohc.gt.ohc_max) then
      write(*,50) dtg,sgn,'LON',lon,'LAT',lat,'OHC',ohc
50    format(a12,1x,a7,1x,a3,1x,f6.1,1x,a3,1x,f5.1,1x,a3,1x,f7.1)

!      do k=1,kk
!      write(*,'(5(f9.4,1x))') lat,lon,prf_tmp(k,n),prf_sal(k,n),
!     6                        prf_lvl(k,n)
!      enddo

!      do m=1,mm
!      write(*,*) tAve(m),hAve(m)
!      enddo

      deallocate (hsum, tsum, terrsum, ssum, serrsum, nsum)
      deallocate (hAve, tAve, terrAve, sAve, serrAve)
      deallocate (z_i, z_l)
      cycle          ! PROFILE LOCATIONS
      endif

!     ############## write text for ioda NETCDF
      do m=1,mm
      write(30,25) dtg,lat,lon,tAve(m),terrAve(m),tqc,
     6             sAve(m),serrAve(m),sqc,hAve(m)
      enddo

      deallocate (hsum, tsum, terrsum, ssum, serrsum, nsum)
      deallocate (hAve, tAve, terrAve, sAve, serrAve)
      deallocate (z_i, z_l)
      
      ns=ns+1
      latt(ns)=lat
      lonn(ns)=lon

      enddo ! i=1, n_in

      print*,'TOTAL PROFILE ns=',ns

25    format(a12,1x,2(f8.3,1x),6(1x,f6.3),1x,f6.1)
      close(30)
!     ############## PROFILE LOCATIONS

      return
      end

!     ################################################################
!     ###################### read RTOFS SSS binary ###################
!     ################################################################

      subroutine read_sss(latmin,latmax,lonmin,lonmax)

      real      latmin,latmax,lonmin,lonmax
      real,     allocatable :: ob_age (:)
      character,allocatable :: ob_dtg (:) * 12
      real,     allocatable :: ob_err (:)
      integer,  allocatable :: ob_flg (:)
      real,     allocatable :: ob_lat (:)
      real,     allocatable :: ob_lon (:)
      real,     allocatable :: ob_qc (:)
      character,allocatable :: ob_rcp (:) * 12
      real,     allocatable :: ob_sss (:)
      real,     allocatable :: ob_sst (:)
      integer,  allocatable :: ob_typ (:)
c
      character winstart *12, winend *12
      integer n_read, n_lvl, vrsn, i
c
      open(10,file='sss.bin',form='unformatted')

      write(*,*) 'reading RTOFS binary SSS'

      read (10) n_read, n_lvl, vrsn
      if (n_read .gt. 0) then
c
      allocate (ob_age (n_read))
      allocate (ob_dtg (n_read))
      allocate (ob_err (n_read))
      allocate (ob_flg (n_read))
      allocate (ob_lat (n_read))
      allocate (ob_lon (n_read))
      allocate (ob_qc (n_read))
      allocate (ob_rcp (n_read))
      allocate (ob_sss (n_read))
      allocate (ob_sst (n_read))
      allocate (ob_typ (n_read))

      read (10) ob_age(1:n_read)
      read (10) ob_err(1:n_read)
      read (10) ob_flg(1:n_read)
      read (10) ob_lat(1:n_read)
      read (10) ob_lon(1:n_read)
      read (10) ob_qc(1:n_read)
      read (10) ob_typ(1:n_read)
      read (10) ob_sss(1:n_read)
      read (10) ob_sst(1:n_read)
      read (10) ob_dtg(1:n_read)

      if (vrsn .eq. 2) then
         read (10) ob_rcp(1:n_read)
      else
         do i = 1, n_read
         ob_rcp(i) = ob_dtg(i)
         enddo
      endif

      open(21,file='window.txt',status='old')
      read(21,'(a12)') winstart
      read(21,'(a12)') winend
      close(21)

      open(20,file='sss.txt',status='new')

      do i=1, n_read

!     HAT10
      if(ob_lat(i).ge.latmin.and.ob_lat(i).le.latmax.and.
     6     ob_lon(i).ge.lonmin.and.ob_lon(i).le.lonmax) then

!     time window
      if(ob_dtg(i).ge.winstart.and.ob_dtg(i).le.winend) then

      if(ob_dtg(i)(11:12).gt."59") ob_dtg(i)(11:12)="59"

!     Missing ob_qc
      if(ob_qc(i).gt.99.0) ob_qc(i)=99.9
      if(ob_err(i).gt.9.9) ob_err(i)=9.9

      write(20,25) ob_dtg(i),ob_lat(i),ob_lon(i),
     6             ob_sss(i),ob_err(i),ob_qc(i)

      endif ! time window 

      endif ! HAT10

      enddo

25    format(a12,1x,2(f8.3,1x),3(1x,f6.3))
      close(20)

      endif

      return
      end


!      program sst_buddy_check_superobbing

!      character argv*12

!      call getarg(1, argv)

!      if(argv.eq.'sst_qc') then
!         print*, 'sst_qc'
!         call sst_qc
!      endif

!      stop
!      end 

!       ###### thin RTOFS SST ######

!     program sst_buddy_check_superobbing
      subroutine sst_qc()

      parameter(kk=100)
       
      character ob_dtg *12
      REAL ob_lat,ob_lon,ob_sst,ob_err,ob_qc

      character, ALLOCATABLE:: dtg(:,:) *12
      integer,   ALLOCATABLE:: bk(:,:)
      real,      ALLOCATABLE:: lat(:,:),lon(:,:),sst(:,:),err(:,:),
     6                          qc(:,:)
      real,      ALLOCATABLE:: latk(:,:,:),lonk(:,:,:),sstk(:,:,:),
     6                         errk(:,:,:),qck(:,:,:)

!     hat10/NHC domain south lat, west lon 0.08X0.08 box
!      latn = 47.0            ! 47.0N
!      lats = -23.0           ! 23.0S
!      lonw = -178.0          ! 178.0W
!      lone = 15.0            ! 15.0E

      open(1,file='domain.txt',status='old')
      read(1,*) latmin, latmax, lonmin, lonmax
      close(1)
      print*,latmin, latmax, lonmin, lonmax

      ds   = 0.08

      ii=int((lonmax-lonmin)/ds)+2
      jj=int((latmax-latmin)/ds)+2
      print*,ii,jj
      allocate (dtg(jj,ii),bk(jj,ii))
      allocate (lat(jj,ii),lon(jj,ii),sst(jj,ii),err(jj,ii),qc(jj,ii))
      allocate (latk(kk,jj,ii),lonk(kk,jj,ii),sstk(kk,jj,ii),
     6          errk(kk,jj,ii),qck(kk,jj,ii))
!      stop

      do j = 1, jj
      do i = 1, ii
        lat(j,i) = 0
        lon(j,i) = 0
        sst(j,i) = 0
        err(j,i) = 0
        qc(j,i) = 0
        bk(j,i) = 0
      enddo
      enddo

      open(10,file='sst.txt',status='old')
      open(20,file='sst_qc.txt',status='unknown')

99    continue
      read(10,25,end=88) ob_dtg,ob_lat,ob_lon,ob_sst,ob_err,ob_qc
      if(ob_err.lt.0.05) write(*,*) 'SMALL OBS ERROR.....',ob_err

!     index for obs to be in a box
      jy = int((ob_lat-latmin)/ds)+1
      ix = int((ob_lon-lonmin)/ds)+1

!     hat10/NHC domain filter
      if(ix.ge.1.and.ix.le.ii.and.jy.ge.1.and.jy.le.jj) then

!     time string
      dtg(jy,ix)=ob_dtg

!     add for box average later on
      lat(jy,ix)=lat(jy,ix) + ob_lat
      lon(jy,ix)=lon(jy,ix) + ob_lon
      sst(jy,ix)=sst(jy,ix) + ob_sst
      err(jy,ix)=err(jy,ix) + ob_err
       qc(jy,ix)= qc(jy,ix) + ob_qc
       bk(jy,ix)= bk(jy,ix) + 1

!     individual in a box
      k=bk(jy,ix)
      latk(k,jy,ix)=ob_lat
      lonk(k,jy,ix)=ob_lon
      sstk(k,jy,ix)=ob_sst
      errk(k,jy,ix)=ob_err
       qck(k,jy,ix)=ob_qc

      !if(jy.eq.213.and.ix.eq.144) then
      !write(*,*) k,sstk(k,jy,ix)
      !endif

      endif  ! hat10/NHC

      goto 99  ! All stations

88    continue
      write(*,*) 'finishing reading'

      !write(*,*) 'kk',(sstk(k,213,144),k=1,bk(213,144))
      !stop

!     Average of each box
      errmin=10.0
      errmax=0.0
      do j = 1, jj
      do i = 1, ii

      if(bk(j,i).ge.1) then
      lat(j,i)=lat(j,i)/bk(j,i)
      lon(j,i)=lon(j,i)/bk(j,i)
      sst(j,i)=sst(j,i)/bk(j,i)
      err(j,i)=err(j,i)/bk(j,i)
      qc(j,i)=qc(j,i)/bk(j,i)

      if(err(j,i).gt.errmax) errmax=err(j,i)
      if(err(j,i).lt.errmin) errmin=err(j,i)

      endif

      enddo
      enddo

      write(*,*)'errmax errmin ...',errmax,errmin

      write(*,*) 'buddy check against box average ..........'
      isobs = 0
      nnn=0

!     loop every box
      do j = 2, jj-1
      do i = 2, ii-1

!    isolated obs in a box
      if(bk(j,i).eq.1) then
      bk(j,i)=0
      isobs=isobs+1
      endif

!     obs data #>1 in a box
      if(bk(j,i).gt.1) then
      nn=bk(j,i)

      mmm=0
77    continue
!     loop every obs in a box
      do n=1,nn

!     each obs and average
      ti=sstk(n,j,i)
      ta=sst(j,i)

!     Buddy check against average
      if(abs(ti-ta).gt.2.0) then
      nnn=nnn+1
      mmm=mmm+1
      write(*,*) '+++++++++++++++ This 0.08X0.08 deg Box +++++++++'
      write(*,'(2(a5,f7.2))') 'lat= ',lat(j,i),' lon= ',lon(j,i)
      write(*,'(2(a14,i3))') 'total outlier=',mmm,' outlier ID ', n
      write(*,*) 'outlier sst ...',ti,'prior mean...',ta
      write(*,'(a5,1x,i2,1x,a3,1x,10(f6.3,1x))') 'total', nn,'SST ',
     6         (sstk(m,j,i),m=1,nn) 
      sstk(n,j,i)=ta

!     new average
      tv=0
      do m=1,nn
      tv=tv+sstk(m,j,i)
      enddo
      sst(j,i)=tv/bk(j,i)
      write(*,*) 'post mean....',sst(j,i)
      goto 77
      endif  ! abs(ti-ta).gt.2.0 outlier
!     End Buddy check against average

      enddo
!     loop every obs in a box

      endif  ! bk(j,i).gt.1 obs data #>1 in a box

      enddo
      enddo
!     loop every box

      write(*,*) 'isolated obs = ....      ', isobs
      write(*,*) 'buddy check obs = ....', nnn

!     write for ioda
      do j = 1, jj
      do i = 1, ii

      if(bk(j,i).ge.1.0) then
      write(20,25) dtg(j,i),lat(j,i),lon(j,i),sst(j,i),err(j,i),
     6             qc(j,i)
      endif

      enddo
      enddo

25    format(a12,1x,2(f8.3,1x),3(1x,f6.3))
      close(10)

      return
      end

!      program sss_buddy_check
!
!      character argv*12

!      call getarg(1, argv)

!      if(argv.eq.'sss_qc') then
!         print*, 'sss_qc'
!         call sss_qc
!      endif

!      stop
!      end 

!     ###### RTOFS SSS qc ######

!     program sss_buddy_check
      subroutine sss_qc()

      parameter(kk=40)  
       
      character ob_dtg *12
      REAL ob_lat,ob_lon,ob_sss,ob_err,ob_qc
      real latmin, latmax, lonmin, lonmax, median

      character, allocatable:: dtg(:,:,:) *12
      integer,   ALLOCATABLE:: nobs(:,:)
      real,      ALLOCATABLE:: lat(:,:,:),lon(:,:,:),
     6           sss(:,:,:),err(:,:,:),qc(:,:,:),arr(:)

!     hat10/NHC domain lat/lon 0.4degree X 0.4degree box
!      latn = 47.0            ! 47.0N
!      lats = -23.0           ! 23.0S
!      lonw = -178.0          ! 178.0W
!      lone = 15.0            ! 15.0E

      open(1,file='domain.txt',status='old')
      read(1,*) latmin, latmax, lonmin, lonmax
      close(1)
      print*,latmin, latmax, lonmin, lonmax

      ds   = 0.4

      ii=int((lonmax-lonmin)/ds)+2
      jj=int((latmax-latmin)/ds)+2
      print*,ii,jj
      allocate (dtg(kk,jj,ii),nobs(jj,ii))
      allocate (lat(kk,jj,ii),lon(kk,jj,ii),sss(kk,jj,ii))
      allocate (err(kk,jj,ii),qc(kk,jj,ii))
!      stop

      open(10,file='sss.txt',status='old')
      open(20,file='sss_qc.txt',status='unknown')

!     read all stations
99    continue
      read(10,25,end=88) ob_dtg,ob_lat,ob_lon,ob_sss,ob_err,ob_qc
      if(ob_err.lt.0.05) write(*,*) 'SMALL OBS ERROR.....',ob_err

!     index for obs to be in a box
      j = int((ob_lat-latmin)/ds)+1
      i = int((ob_lon-lonmin)/ds)+1
!      print*,ob_lat,ob_lon,j,i
!      stop

!     hat10/NHC domain. put obs in a box
      if(i.ge.1.and.i.le.ii.and.j.ge.1.and.j.le.jj) then
      nobs(j,i)= nobs(j,i) + 1
      n=nobs(j,i)
      dtg(n,j,i)=ob_dtg
      lat(n,j,i)=ob_lat
      lon(n,j,i)=ob_lon
      sss(n,j,i)=ob_sss
      err(n,j,i)=ob_err
       qc(n,j,i)=ob_qc
      endif

      goto 99  ! read all stations
88    continue
      write(*,*) 'finishing reading'

      write(*,*) 'buddy median check ..........'
      isobs = 0
      nnn=0

      write(*,*) 'starting QC ..................................'
!     loop every box
      do j = 2, jj-1
      do i = 2, ii-1

!     isolated 2 obs in a box
      if(nobs(j,i).le.2) then
      isobs=isobs+1
      endif

!     obs data >2 in a box for buddy check
      if(nobs(j,i).ge.2) then

      nn=nobs(j,i)
      allocate (arr(nn))
      do n=1,nn
      arr(n)=sss(n,j,i)
!      write(*,*) 'arr ..............',arr(n)
      enddo

!     sorting to arrange data from small to large
      call sort(arr,nn)

!     median
      if (mod(nn, 2) == 0) then
        median = (arr(nn/2) + arr(nn/2 + 1)) / 2.0
      else
        median = arr(nn/2 + 1)
      end if

!     buddy check agaist median
      do n=1,nn
      if (abs(sss(n,j,i)-median) > 2.0 ) then
      nnn=nnn+1
      write(*,*) '+++++++++++ This 0.4X0.4 deg Box +++++++++'
      write(*,'(i2,1x,a3,1x,10(f6.3,1x))') nn,'obs',arr
      write(*,'(a7,1x,f6.3)') "median:",median
      write(*,'(a18,1x,f6.3)') 'outlier sss(n,j,i)',sss(n,j,i)
      sss(n,j,i) = median
      write(*,'(a18,1x,f6.3)') 'new     sss(n,j,i)',sss(n,j,i)
      end if
      enddo
      deallocate (arr)

      endif
!     obs data >2 in a box

      enddo
      enddo
!     loop every box

      write(*,*) 'isolated obs = .... ', isobs
      write(*,*) 'buddy check = ....', nnn
      
!     write out for ioda. Every box
      do j = 1, jj
      do i = 1, ii

!     Every obs
      if(nobs(j,i).ge.1) then
      nn=nobs(j,i)
      do n=1,nn
      write(20,25) dtg(n,j,i),lat(n,j,i),lon(n,j,i),sss(n,j,i),
     6             err(n,j,i),qc(n,j,i)
      enddo
      endif

      enddo
      enddo

25    format(a12,1x,2(f8.3,1x),3(1x,f6.3))
      close(10)

      return
      end

      subroutine sort(a, n)
      implicit none
      integer :: n, i, j
      real :: a(n), temp
   
      do i = 1, n-1
      do j = i+1, n
        if (a(i) > a(j)) then
         temp = a(i)
         a(i) = a(j)
         a(j) = temp
        end if
      enddo
      enddo
      end subroutine sort

