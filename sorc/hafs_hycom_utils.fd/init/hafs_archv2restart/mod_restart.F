      module mod_restart
      implicit none

c     integer, save :: kapnum = 1  !default is one thermobaric reference state
      integer, save :: kapnum   !default is one thermobaric reference state
      real,    save :: thbase

      real, save, allocatable, dimension (:,:)   :: 
     & pbot    ! bottom pressure at t=0

      real, save, allocatable, dimension (:,:,:) :: 
     & psikk,  ! montg.pot.     in bottom layer
     & thkk    ! virt.pot.dens. in bottom layer

      contains

      subroutine restart_in(flnmrsi, icegln, n, dtime)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character*120 flnmrsi  !restart filename
      logical       icegln   !input ice
      integer       n        !time slot number (1 or 2)
      real*8        dtime    !model time (on exit)
c
c     read in one timestep from a restart file on unit 11.
c     no ice and no tracers
c
      character cline*80
      integer   i,k
      logical   lold
c
      allocate(  pbot(ii,jj) )
      allocate( psikk(ii,jj,kapnum) )
      allocate(  thkk(ii,jj,kapnum) )
c
      open (unit=11,file=flnmrsi(1:len_trim(flnmrsi)-2)//'.b',
     &      status='old',action='read',form='formatted')
      call zaiopf(flnmrsi(1:len_trim(flnmrsi)-2)//'.a','old', 11)
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      endif !1st tile
      if     (cline(1:9).eq.'RESTART: ') then
        lold   = .true.
        sigver = 0
        read( 11,'(a)') cline
        if     (mnproc.eq.1) then
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        endif !1st tile
        i = index(cline,'=')
        read(cline(i+1:),*) nstep,dtime
      elseif (cline(1:9).eq.'RESTART2:') then
        lold = .false.
        i = index(cline,'=')
        read(cline(i+1:),*) k,k,k,sigver  !only want sigver
        read( 11,'(a)') cline
        if     (mnproc.eq.1) then
        write(lp,'(a)') cline(1:len_trim(cline))
        call flush(lp)
        endif !1st tile
        i = index(cline,'=')
        read(cline(i+1:),*) nstep,dtime,thbase
      else
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)') 'error in hycom - unknown restart type'
        endif !1st tile
        call xcstop('(restart_in)')
               stop '(restart_in)'
      endif
c
      if     (n.eq.1) then !1st time step
        call restart_in3d(u,     kk,1,1, ip, 'u       ')
        call restart_sk3d(       kk,         'u       ')
        call restart_in3d(v,     kk,1,1, ip, 'v       ')
        call restart_sk3d(       kk,         'v       ')
        call restart_in3d(dp,    kk,1,1, ip, 'dp      ')
        call restart_sk3d(       kk,         'dp      ')
        call restart_in3d(temp,  kk,1,1, ip, 'temp    ')
        call restart_sk3d(       kk,         'temp    ')
        call restart_in3d(saln,  kk,1,1, ip, 'saln    ')
        call restart_sk3d(       kk,         'saln    ')
        if     (lold) then
          call restart_in3d(th3d,  kk,1,1, ip, 'th3d    ')
          call restart_sk3d(       kk,         'th3d    ')
        else
          do k= 1,kk
            call th3d_p(temp(1,1,k),saln(1,1,k),
     &              th3d(1,1,k),ii,jj, sigver,thbase)
          enddo !k
        endif
c
        call restart_in3d(ubaro, 1, 1,1, ip, 'ubavg   ')
        call restart_sk3d(       1,          'ubavg   ')
        call restart_sk3d(       1,          'ubavg   ')
        call restart_in3d(vbaro, 1, 1,1, ip, 'vbavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_in3d(pbaro, 1, 1,1, ip, 'pbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
c
        call restart_in3d(pbot,  1,      1,1, ip, 'pbot    ')
        call restart_in3d(psikk, kapnum, 1,1, ip, 'psikk   ')
        call restart_in3d(thkk,  kapnum, 1,1, ip, 'thkk    ')
c
        call restart_in3d(dpmixl,1, 1,1, ip, 'dpmixl  ')
        call restart_sk3d(       1,          'dpmixl  ')
      else !2nd time step
        call restart_sk3d(       kk,         'u       ')
        call restart_in3d(u,     kk,1,1, ip, 'u       ')
        call restart_sk3d(       kk,         'v       ')
        call restart_in3d(v,     kk,1,1, ip, 'v       ')
        call restart_sk3d(       kk,         'dp      ')
        call restart_in3d(dp,    kk,1,1, ip, 'dp      ')
        call restart_sk3d(       kk,         'temp    ')
        call restart_in3d(temp,  kk,1,1, ip, 'temp    ')
        call restart_sk3d(       kk,         'saln    ')
        call restart_in3d(saln,  kk,1,1, ip, 'saln    ')
        if     (lold) then
          call restart_sk3d(       kk,         'th3d    ')
          call restart_in3d(th3d,  kk,1,1, ip, 'th3d    ')
        else
          th3d(:,:,1:kk) = 0.0
        endif
c
        call restart_sk3d(       1,          'ubavg   ')
        call restart_in3d(ubaro, 1, 1,1, ip, 'ubavg   ')
        call restart_sk3d(       1,          'ubavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_in3d(vbaro, 1, 1,1, ip, 'vbavg   ')
        call restart_sk3d(       1,          'vbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
        call restart_in3d(pbaro, 1, 1,1, ip, 'pbavg   ')
        call restart_sk3d(       1,          'pbavg   ')
c
        call restart_in3d(pbot,  1,      1,1, ip, 'pbot    ')
        call restart_in3d(psikk, kapnum, 1,1, ip, 'psikk   ')
        call restart_in3d(thkk,  kapnum, 1,1, ip, 'thkk    ')
c
        call restart_sk3d(       1,          'dpmixl  ')
        call restart_in3d(dpmixl,1, 1,1, ip, 'dpmixl  ')
      endif
c
      if    (icegln) then
        call restart_in3d(temice,1, 1,1, ip, 'temice  ')
        call restart_in3d(covice,1, 1,1, ip, 'covice  ')
        call restart_in3d(thkice,1, 1,1, ip, 'thkice  ')
      endif
c
      close (unit=11)
      call zaiocl(11)
c
      call getdepth(dpbl) !dpbl is workspace
      return
      end subroutine restart_in

      subroutine restart_in_pbot(flnmrsi)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character*120 flnmrsi
c
c     read in a small part of a a restart file on unit 11.
c
      character cline*80
      integer   i,k
      logical   lold
c
      allocate(  pbot(ii,jj) )
      allocate( psikk(ii,jj,kapnum) )
      allocate(  thkk(ii,jj,kapnum) )
c
      open (unit=11,file=flnmrsi(1:len_trim(flnmrsi)-2)//'.b',
     &      status='old',action='read',form='formatted')
      call zaiopf(flnmrsi(1:len_trim(flnmrsi)-2)//'.a','old', 11)
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      endif !1st tile
      if     (cline(1:9).eq.'RESTART: ') then
        lold   = .true.  !don't set sigver
      elseif (cline(1:9).eq.'RESTART2:') then
        lold = .false.
        i = index(cline,'=')
        read(cline(i+1:),*) k,k,k,sigver  !reset sigver
      else
        if     (mnproc.eq.1) then
        write(lp,'(/ a /)') 'error in hycom - unknown restart type'
        endif !1st tile
        call xcstop('(restart_in)')
               stop '(restart_in)'
      endif
      read( 11,'(a)') cline
      if     (mnproc.eq.1) then
      write(lp,'(a)') cline(1:len_trim(cline))
      call flush(lp)
      endif !1st tile
c
      call restart_sk3d(2*kk, 'u       ')
      call restart_sk3d(2*kk, 'v       ')
      call restart_sk3d(2*kk, 'dp      ')
      call restart_sk3d(2*kk, 'temp    ')
      call restart_sk3d(2*kk, 'saln    ')
      if     (lold) then
        call restart_sk3d(2*kk, 'th3d    ')
      endif
c
      call restart_sk3d(3,    'ubavg   ')
      call restart_sk3d(3,    'vbavg   ')
      call restart_sk3d(3,    'pbavg   ')
c
      call restart_in3d(pbot,  1,      1,1, ip, 'pbot    ')
      call restart_in3d(psikk, kapnum, 1,1, ip, 'psikk   ')
      call restart_in3d(thkk,  kapnum, 1,1, ip, 'thkk    ')
c
      close (unit=11)
      call zaiocl(11)
      return
      end subroutine restart_in_pbot

      subroutine restart_in3d(field3d,l, k1,ki, mask, cfield)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      integer   l,k1,ki
      real,    dimension (ii,jj,*) ::
     & field3d
      integer, dimension (ii,jj) ::
     & mask
      character cfield*8
c
c --- read a single restart 3-d array field from unit 11.
c --- file input is 1:l, field3d output is k1:k1+ki*(l-1):ki.
c
      integer   i,layer,level,k,kout
      real      hmina,hminb,hmaxa,hmaxb
      character cline*80
c
      if     (mnproc.eq.1) then
      write(lp,'(a,i3,2x,a)') 'restart_in3d - l,cfield = ',l,cfield
      call flush(lp)
      endif !1st tile
c
      kout = k1
      do k= 1,l
        read ( 11,'(a)')  cline
        if     (mnproc.eq.1) then
        write (lp,'(a)')  cline(1:len_trim(cline))
        endif !1st tile
        if     (cline(1:8).ne.cfield) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cline(1:len_trim(cline)),
     &           'error in restart_in3d - expected ',cfield
          endif !1st tile
          call xcstop('(restart_in3d)')
                 stop '(restart_in3d)'
        endif
        i = index(cline,'=')
        read (cline(i+1:),*) layer,level,hminb,hmaxb
        call zaiord(field3d(1,1,kout),
     &              mask,.false., hmina,hmaxa, 11)
        if     (abs(hmina-hminb).gt.abs(hminb)*1.e-4 .or.
     &          abs(hmaxa-hmaxb).gt.abs(hmaxb)*1.e-4     ) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,3i3 / a / a,1p3e14.6 / a,1p3e14.6 /)')
     &      'error - .a and .b files not consistent:',
     &      'iunit,k,l = ',11,k,l,
     &      cline,
     &      '.a,.b min = ',hmina,hminb,hmina-hminb,
     &      '.a,.b max = ',hmaxa,hmaxb,hmaxa-hmaxb
          endif !1st tile
          call xcstop('(restart_in3d)')
                 stop '(restart_in3d)'
        endif
*       write(6,*) 'kout, field3d = ',kout,field3d(20,20,kout)
        kout = kout + ki
      enddo !k
c
      return
      end subroutine restart_in3d

      subroutine restart_sk3d(l, cfield)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      integer   l
      character cfield*8
c
c --- skip a single restart 3-d array field from unit 11.
c
      integer   k
      character cline*80
c
*     if     (mnproc.eq.1) then
*     write(lp,'(a,i3,2x,a)') 'restart_sk3d - l,cfield = ',l,cfield
*     call flush(lp)
*     endif !1st tile
c
      do k= 1,l
        call zaiosk(11)
        read ( 11,'(a)')  cline
*       if     (mnproc.eq.1) then
*       write (lp,'(a)')  cline(1:len_trim(cline))
*       endif !1st tile
        if     (cline(1:8).ne.cfield) then
          if     (mnproc.eq.1) then
          write(lp,'(/ a / a,a /)') cline(1:len_trim(cline)),
     &           'error in restart_sk3d - expected ',cfield
          endif !1st tile
          call xcstop('(restart_in3d)')
                 stop '(restart_in3d)'
        endif
      enddo
c
      return
      end subroutine restart_sk3d

      subroutine restart_out(flnmrso, nstepx, dtimex,
     &                       iexpt,iversn,yrflag, icegln,trcout)
      use mod_plot  ! HYCOM plot array interface
      use mod_za    ! HYCOM array I/O interface
      implicit none
c
      character*120 flnmrso
      logical       icegln,trcout
      integer       nstepx,iexpt,iversn,yrflag
      real*8        dtimex
c
c     write out in a restart file on unit 12
c
      logical   lopen
      integer   i,j,k,l
      real      xmin(2*kk),xmax(2*kk)
      character cline*80
c
      call zaiopf(flnmrso(1:len_trim(flnmrso)-2)//'.a','new', 12)
      if     (mnproc.eq.1) then
        open (unit=12,file=flnmrso(1:len_trim(flnmrso)-2)//'.b',
     &        status='new',action='write',form='formatted')
        write(lp,'(a)') ' creating a new restart file'
      endif !1st tile
c
      if     (sigver.eq.0) then
        if     (mnproc.eq.1) then
        write(12,'(a,3i6)') 'RESTART: iexpt,iversn,yrflag = ',
     &                                iexpt,iversn,yrflag
        write(cline,*)                nstepx,dtimex
        write(12,'(a,a)')   'RESTART: nstep,dtime = ',
     &                                trim(cline)
        endif !1st tile
      else
        if     (mnproc.eq.1) then
        write(12,'(a,4i6)') 'RESTART2: iexpt,iversn,yrflag,sigver = ',
     &                             iexpt,iversn,yrflag,sigver
        write(cline,*)                nstepx,dtimex,thbase
        write(12,'(a,a)')   'RESTART2: nstep,dtime,thbase = ',
     &                             trim(cline)
        call flush(12)
        endif !1st tile
      endif !sigver
c
      call zaiowr3(u,      kk, iu,.false., xmin,xmax, 12, .true.)
      call zaiowr3(u,      kk, iu,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'u       ',k,l+1,xmin(k),xmax(k)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(v,      kk, iv,.false., xmin,xmax, 12, .true.)
      call zaiowr3(v,      kk, iv,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'v       ',k,l+1,xmin(k),xmax(k)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(dp,     kk, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(dp,     kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'dp      ',k,l+1,xmin(k),xmax(k)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(temp,   kk, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(temp,   kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'temp    ',k,l+1,xmin(k),xmax(k)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(saln,   kk, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(saln,   kk, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 0,1
        do k= 1,kk
          write(12,4100) 'saln    ',k,l+1,xmin(k),xmax(k)
        enddo
      enddo
      endif !1st tile
      if     (sigver.eq.0) then
        call zaiowr3(th3d,   kk, ip,.false., xmin,xmax, 12, .true.)
        call zaiowr3(th3d,   kk, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 0,1
          do k= 1,kk
            write(12,4100) 'th3d    ',k,l+1,xmin(k),xmax(k)
          enddo
        enddo
        endif !1st tile
      endif !sigver==0
      call zaiowr3(ubaro,      1, iu,.false., xmin,xmax, 12, .true.)
      call zaiowr3(ubaro,      1, iu,.false., xmin,xmax, 12, .true.)
      call zaiowr3(ubaro,      1, iu,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,3
        do k= 0,0
          write(12,4100) 'ubavg   ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(vbaro,      1, iv,.false., xmin,xmax, 12, .true.)
      call zaiowr3(vbaro,      1, iv,.false., xmin,xmax, 12, .true.)
      call zaiowr3(vbaro,      1, iv,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,3
        do k= 0,0
          write(12,4100) 'vbavg   ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(pbaro,      1, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(pbaro,      1, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(pbaro,      1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,3
        do k= 0,0
          write(12,4100) 'pbavg   ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(pbot,       1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,1
        do k= 0,0
          write(12,4100) 'pbot    ',k,l,  xmin(l),xmax(l)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(psikk, kapnum, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,kapnum
        do k= 0,0
          write(12,4100) 'psikk   ',k,l,  xmin(l),xmax(l)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(thkk,  kapnum, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,kapnum
        do k= 0,0
          write(12,4100) 'thkk    ',k,l,  xmin(l),xmax(l)
        enddo
      enddo
      endif !1st tile
      call zaiowr3(dpmixl,      1, ip,.false., xmin,xmax, 12, .true.)
      call zaiowr3(dpmixl,      1, ip,.false., xmin,xmax, 12, .true.)
      if     (mnproc.eq.1) then
      do l= 1,2
        do k= 0,0
          write(12,4100) 'dpmixl  ',k,l,  xmin(1),xmax(1)
        enddo
      enddo
      endif !1st tile
      if (icegln) then
        call zaiowr3(temice,     1, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 1,1
          do k= 0,0
            write(12,4100) 'temice  ',k,l,  xmin(l),xmax(l)
          enddo
        enddo
        endif !1st tile
        call zaiowr3(covice,     1, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 1,1
          do k= 0,0
            write(12,4100) 'covice  ',k,l,  xmin(l),xmax(l)
          enddo
        enddo
        endif !1st tile
        call zaiowr3(thkice,     1, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 1,1
          do k= 0,0
            write(12,4100) 'thkice  ',k,l,  xmin(l),xmax(l)
          enddo
        enddo
        endif !1st tile
      endif
      if (trcout) then
        call zaiowr3(tracer, kk, ip,.false., xmin,xmax, 12, .true.)
        call zaiowr3(tracer, kk, ip,.false., xmin,xmax, 12, .true.)
        if     (mnproc.eq.1) then
        do l= 0,1
          do k= 1,kk
            write(12,4100) 'tracer  ',k,l+1,xmin(k),xmax(k)
          enddo
        enddo
        endif !1st tile
      endif
      call zaiofl(12)
      if     (mnproc.eq.1) then
      write(lp,'(a,f11.2)') ' restart created at model day',dtimex
      endif !1st tile
      call xcsync(flush_lp)
c
      return
 4100 format(a,': layer,tlevel,range = ',i3,i3,2x,1p2e16.7)
      end subroutine restart_out

      end module mod_restart
