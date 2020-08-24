      program archv2restart
      use mod_plot     ! HYCOM plot array interface
      use mod_za       ! HYCOM array I/O interface
      use mod_restart  ! see above
c
c --- hycom/micom archive to hycom restart file.
c
      common/conrng/ amn,amx
c
      character*120    flnmarch,flnmrsi,flnmrso
      logical          ltheta,smooth,lsteric,icegln
c
      integer          artype,iexpt,iversn,kkin,yrflag,kapref
      double precision time3(3)
      real*8           time
c
      real, parameter :: flag = 2.0**100
c
c --- 'lhycom' -- hycom (vs micom) input file
c --- 'trcout' -- tracer input
      logical   lhycom,trcout
      data      lhycom/.true. /, trcout/.false./
c
      call xcspmd
      call zaiost
      lp=6
c
c --- read model data
c ---   'flnmarch' = name of  input archive file
c ---   'flnmrsi'  = name of  input restart file
c ---   'flnmrso'  = name of output restart file
c ---   'iexpt '   = experiment number x10  (000=from archive file)
c ---   'yrflag'   = days in year flag (0=360J16,1=366J16,2=366J01,3=actual)
c ---   'idm   '   = longitudinal array size
c ---   'jdm   '   = latitudinal  array size
c ---   'kapref'   = thermobaric reference state (-1 to 3, optional, default 0)
c ---   'kdm   '   = number of layers
        read (*,'(a)')    flnmarch
        write (lp,'(2a)') ' input archive file: ',
     &                    flnmarch(1:len_trim(flnmarch))
        call flush(lp)
        read (*,'(a)')    flnmrsi 
        write (lp,'(2a)') ' input restart file: ',
     &                    flnmrsi(1:len_trim(flnmrsi))
        call flush(lp)
        read (*,'(a)')    flnmrso 
        write (lp,'(2a)') 'output restart file: ',
     &                    flnmrso(1:len_trim(flnmrso))
        call flush(lp)
        call blkini(iexpt, 'iexpt ')
        call blkini(yrflag,'yrflag')
        call blkini(ii,    'idm   ')
        call blkini(jj,    'jdm   ')
        call blkini2(i,j,  'kapref','kdm   ')  !read kapref or kdm
        if (j.eq.1) then
          if (i.lt.0) then  !convert kapref to kapnum
            kapnum = 2  !declared in mod_restart
          else
            kapnum = 1  !declared in mod_restart
          endif
          call blkini(kk,  'kdm   ')
        else
          kk     = i
          kapnum = 1  !declared in mod_restart
        endif
c---hsk
        kapnum = 2
c       write(*,*) 'kapnum in archv: ',kapnum
c---hsk
        if     (ii.ne.idm .or. jj.ne.jdm) then
          write(lp,*)
          write(lp,*) 'error - wrong idm or jdm (should be:',
     &                                           idm,jdm,')'
          write(lp,*)
          call flush(lp)
          stop
        endif
        iorign = 1
        jorign = 1
c
c ---   'thbase' = reference density (sigma units)
c ---   'baclin' = baroclinic time step (seconds), int. divisor of 86400
        call blkinr(thbase,
     &             'thbase','("blkinr: ",a6," =",f11.4," sig")')
        call blkinr(baclin,
     &             'baclin','("blkinr: ",a6," =",f11.4," sec")')
        write(lp,*)
        call flush(lp)
c
c --- array allocation
c
      call plot_alloc
*
*     write (lp,'(a,2i4)') ' plot_alloc, kkmax,kk:',kkmax,kk
c
      dpthfil = 'regional.depth'
c
      do j=1,jj
        do i=1,ii
          p(i,j,1)=0.
        enddo
      enddo
c
c --- read the archive file.
c
      if (lhycom) then
        call getdat(flnmarch,time3,artype,initl,lsteric,icegln,trcout,
     &              iexpt,iversn,yrflag,kkin)       ! hycom input
        time = time3(3)
        if (kkin.ne.kk) then
          write(lp,*)
          write(lp,*) 'error - kkin must be kdm'
          write(lp,*)
          stop
        endif
      else
        call getdtm(flnmarch,time,initl, thbase)    ! micom input
        artype = 1
        iversn = 10
      endif
c
c --- partial read of the input restart file
c
      call restart_in_pbot(flnmrsi)
c
      if     (yrflag.eq.0) then
        year  = 360.0d0
      elseif (yrflag.lt.3) then
        year  = 366.0d0
      else
        year  = 365.25d0
      endif
c
c --- define grid scale
c
      call bigrid(depths)
c
c --- srfht=montg+thref*pbaro
      pbaro(:,:) = (srfht(:,:) - montg(:,:))*1.e3
c
      nstep = nint(time/(baclin/86400.0d0))
      call restart_out(flnmrso, nstep, time,
     &                 iexpt,iversn,yrflag, icegln,trcout)
c
      stop '(normal)'
      end
