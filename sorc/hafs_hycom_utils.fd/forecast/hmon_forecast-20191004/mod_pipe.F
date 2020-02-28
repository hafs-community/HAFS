      module mod_pipe
      use mod_xc  ! HYCOM communication interface
c
c --- HYCOM (named pipe based) debugging interface
c
      logical, save, public  :: lpipe
c
      integer, save, private :: ipunit,lpunit,ishift,jshift,nsym,
     &                          nstep_start
      logical, save, private :: ldebug,ldebugssh,
     &                          lmaster,lpipeio,lshift,lslave,
     &                          lsym,lnan,
     &                          ltracer,ltracernan,ltracermax,
     &                          lpipe_fatal,lpipe_anyfailed,
     &                          lnan_anyfailed

      real,    save, private :: trcmax(mxtrcr)
c
      real,    allocatable, dimension(:,:),
     &         save, private :: field1,field2,tmask,vmask,amask

      contains
c
c --- this set of routines facilitates output comparison from two HYCOM
c --- versions running side by side. one model, the 'slave', writes its
c --- output into a named pipe. the other model, the 'master', reads
c --- from the pipe and compares.
c --- differences are recorded in 'PIPE_base.out'.
c
c --- call 'pipe_fatal_on'  to exit     on differences (this is the default)
c --- call 'pipe_fatal_off' to continue on differences
c
c --- call 'pipe_init' at start of main program.
c
c ---   if the file 'PIPE_MASTER' exists then this is the master,
c ---   if the file 'PIPE_SLAVE'  exists then this is the slave,
c ---   if the file 'PIPE_SYM'    exists then this is master and slave,
c ---   if the file 'PIPE_NAN'    exists then this is master and slave,
c ---   if the file 'PIPE_TRACER' exists then this is master and slave,
c ---   otherwise there is no comparison made.
c
c ---   if the file 'PIPE_SHIFT' exists for the slave, then it
c ---   is a single-line plain text file containing two integers
c ---   specifiying how much to periodically shift the slave arrays
c ---   before sending them to the master.  it is an error for
c ---   'PIPE_SHIFT' to exist (a) on the master and (b) when not
c ---   making a comparison.
c
c ---   if the file 'PIPE_SYM' exists, there is no slave and the
c ---   master compares its own fields for various symmetries.
c ---   it is a single-line plain text file containing an integer
c ---   specifiying what kind of symmetries to test for (0=constant,
c ---   1=transpose, 2=constant-in-j, -2=arctic, 4=4-way, 8=8-way).  
c ---   it is an error for 'PIPE_SYM' to exist when making a 
c ---   master/slave comparison.
c
c ---   if the file 'PIPE_NAN' exists, there is no slave and the
c ---   master checks that all appropriate fields are free of
c ---   NaNs and Infs.
c
c ---   if the file 'PIPE_TRACER' exists, there is no slave and the
c ---   master checks that all appropriate tracers are non-negative
c ---   and compares temperature to any "temperature" tracer.
c
c ---   if the file 'PIPE_DEBUG' exists then debugging printout
c ---   is produced for point itest,jtest (>0, see blkdat.f).  
c ---   if itest=-1 the min/max/iospycnal of th3d are printed.
c ---   if jtest=-1 the basin-wide means are printed.
c ---   this works with or without a pipe for comparison.
c
c ---   if the file 'PIPE_DEBUG_SSH' exists then debugging printout
c ---   is produced for SSH at point itest,jtest.  in addition, SSH
c ---   is checked everywhere for NaN and/or Infs.
c ---   this works with or without a pipe for comparison.
c
c ---   the 'PIPE_MASTER' and 'PIPE_SLAVE' files contain the location
c ---   of an existing named-pipe.  the 'PIPE_SHIFT' file contains the
c ---   periodic shift to apply on the slave.  the 'PIPE_SYM' file
c ---   contains the kind of symmetries to test for.  the contents of
c ---   the 'PIPE_DEBUG' and 'PIPE_DEBUG_SSH' files are ignored.
c
c --- call 'pipe_compare'      (from master and slave) anywhere in the code
c ---   to check whether data stored in a single array are identical
c
c --- call 'pipe_compare_sym1' anywhere in the code to check whether
c ---   data stored in a single p-grid array are symmetrical.
c ---   note that this can be used in place of 'pipe_compare', since
c ---   it will call the latter in master/slave mode.
c
c --- call 'pipe_compare_sym2' anywhere in the code to check whether
c ---   data stored in vector u and v grid arrays are symmetrical.
c ---   note that this can be used in place of 'pipe_compare', since
c ---   it will call the latter twice (for u and v) in master/slave mode.
c
c --- call 'pipe_comparall'   (from master and slave) after major routines
c ---   to check whether data stored in all major arrays are identical or
c ---   symmetric.
c
      subroutine pipe_fatal_on
      implicit none
      lpipe_fatal = .true.
      return
      end subroutine pipe_fatal_on
c
      subroutine pipe_fatal_off
      implicit none
      lpipe_fatal = .false.
      return
      end subroutine pipe_fatal_off
c
      subroutine pipe_init
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      character*256 cpipe
c
      character*12  cinfo
      integer       irecl
      logical       ltstep
c
#if defined(OCEANS2)
c --- master and slave in the same mpi executable
      lmaster = nocean.eq.1
      lslave  = nocean.eq.2
c
      lshift     = .false.
      lsym       = .false.
      lnan       = .false.
      ltracer    = .false.
      ltracernan = .false.
      ltracermax = .false.
      ldebug     = .false.
      ldebugssh  = .false.
#else
      inquire(file=trim(flnminp)//'PIPE_MASTER',    exist=lmaster)
      inquire(file=trim(flnminp)//'PIPE_SLAVE',     exist=lslave)
      inquire(file=trim(flnminp)//'PIPE_SHIFT',     exist=lshift)
      inquire(file=trim(flnminp)//'PIPE_SYM',       exist=lsym)
      inquire(file=trim(flnminp)//'PIPE_NAN',       exist=lnan)
      inquire(file=trim(flnminp)//'PIPE_TRACER',    exist=ltracer)
      inquire(file=trim(flnminp)//'PIPE_TRACERNAN', exist=ltracernan)
      inquire(file=trim(flnminp)//'PIPE_TRACERMAX', exist=ltracermax)
      inquire(file=trim(flnminp)//'PIPE_DEBUG',     exist=ldebug)
      inquire(file=trim(flnminp)//'PIPE_DEBUG_SSH', exist=ldebugssh)
#endif
c
      if     (lmaster .and. lslave) then
        call xchalt('pipe_init: (master/slave ambiguity)')
               stop 'pipe_init: (master/slave ambiguity)'
      endif
      if     (lsym .and. (lmaster .or. lslave)) then
        call xchalt('pipe_init: (sym/master/slave ambiguity)')
               stop 'pipe_init: (sym/master/slave ambiguity)'
      endif
      if     (lnan .and. lsym) then
        call xchalt('pipe_init: (nan/sym ambiguity)')
               stop 'pipe_init: (nan/sym ambiguity)'
      endif
      if     (lnan .and. (lmaster .or. lslave)) then
        call xchalt('pipe_init: (nan/master/slave ambiguity)')
               stop 'pipe_init: (nan/master/slave ambiguity)'
      endif
      lpipe   = lmaster .or. lslave .or. lsym .or. lnan .or. ldebug
      lpipeio = lmaster .or. lslave
      if     (lshift .and. .not.lslave) then
        call xchalt('pipe_init: (shift ambiguity)')
               stop 'pipe_init: (shift ambiguity)'
      endif
c
      lpipe_fatal     = .true.   !by default, exit on detecting differences
      lpipe_anyfailed = .false.  !no differences detected yet
      lnan_anyfailed  = .false.  !no NaN's detected yet
c
      if     (ltracermax) then
        ltracer = .true.
        open(unit=uoff+99,file=trim(flnminp)//'PIPE_TRACERMAX')  !on all nodes
        read(uoff+99,*) trcmax(1:ntracr)
        close(unit=uoff+99) !file='PIPE_TRACERMAX'
      else
        trcmax(1:ntracr) = huge(trcmax(1))
      endif
c
      if     (lpipeio) then
        inquire(file=trim(flnminp)//'PIPE_START',   exist=ltstep)
        if     (ltstep) then
          open(unit=uoff+99,file=trim(flnminp)//'PIPE_START')  !on all nodes
          read(uoff+99,*) nstep_start
          close(unit=uoff+99) !file='PIPE_START'
        else
          nstep_start = 0
        endif
      endif
c
      if     (lpipe .or. ltracer) then
c
c ---   allocate arrays for comparison
c
        allocate( field1(itdm,jtdm) )
        allocate( field2(itdm,jtdm) )
        call mem_stat_add( 2*itdm*jtdm )
        if     (.not.lslave) then
          allocate( tmask( itdm,jtdm) )
          call mem_stat_add( itdm*jtdm )
          allocate( amask( 1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
          call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy) )
        endif
        if     (lsym .or. lnan) then
          allocate( vmask( itdm,jtdm) )
          call mem_stat_add( itdm*jtdm )
        endif
      endif  ! pipe
c
#if defined(OCEANS2)
      if     (lpipeio) then
c
c ---   open some output files
c
        lpunit=19
c
        if     (mnproc.eq.1) then
          if     (lmaster) then
            open (unit=lpunit,file=trim(flnminp)//'PIPE_base.out',
     &            status='unknown')
          else  ! slave
            open (unit=lpunit,file=trim(flnminp)//'PIPE_test.out',
     &            status='unknown')
          endif ! master/slave
        endif  !1st tile only.
        call xcsync(flush_lp)
      endif  ! pipeio
#else
      if     (lpipeio) then
c
c ---   open the pipe and some output files
c ---   note that named pipe buffers are small, typically 64KB
c
        ipunit=18
        lpunit=19
c
        if     (mnproc.eq.1) then
          if     (lmaster) then
            open (unit=17,file=trim(flnminp)//'PIPE_MASTER',
     &            status='old',form='formatted')
            read (     17,'(a)') cpipe
            close(unit=17)
            write(lp,'(a,a)') 'master opening pipe for reading: ',
     &                        cpipe(1:len_trim(cpipe))
            call flush(lp)
#if defined(ALPHA)
c ---       work-around a compiler bug by skipping irecl
            open (unit=ipunit,file=cpipe,status='old',
     &            action='read',
     &            form='unformatted')
#else
            cinfo=' '  !removes spurious compiler warning message
            inquire( iolength=irecl ) cinfo,field1(:,1)
            open (unit=ipunit,file=cpipe,status='old',
     &            action='read',recl=irecl,
     &            form='unformatted')
#endif
            open (unit=lpunit,file=trim(flnminp)//'PIPE_base.out',
     &            status='unknown')
          else  ! slave
            open (unit=17,file=trim(flnminp)//'PIPE_SLAVE',
     &            status='old',form='formatted')
            read (     17,'(a)') cpipe
            close(unit=17)
            write(lp,'(a,a)') 'slave opening pipe for writing: ',
     &                        cpipe(1:len_trim(cpipe))
            call flush(lp)
#if defined(ALPHA)
c ---       work-around a compiler bug by skipping irecl
            open (unit=ipunit,file=cpipe,status='old',
     &            action='write',
     &            form='unformatted')
#else
            cinfo=' '  !removes spurious compiler warning message
            inquire( iolength=irecl ) cinfo,field1(:,1)
            open (unit=ipunit,file=cpipe,status='old',
     &            action='write',recl=irecl,
     &            form='unformatted')
#endif
            open (unit=lpunit,file=trim(flnminp)//'PIPE_test.out',
     &            status='unknown')
c
            if     (lshift) then
              open (unit=17,file=trim(flnminp)//'PIPE_SHIFT',
     &              status='old',form='formatted')
              read (     17,*) ishift,jshift
              close(unit=17)
              write(lp,'(a,2i5)') 'slave periodic shift is:',
     &                            ishift,jshift
              call flush(lp)
            endif ! shift
          endif ! master/slave
        endif  !1st tile only.
        call xcsync(flush_lp)
      endif  ! pipeio
#endif /* OCEANS2:else */
c
      if     (lsym) then
        open (unit=17,file=trim(flnminp)//'PIPE_SYM',
     &        status='old',form='formatted')
        read (     17,*) nsym
        close(unit=17)
        if     (mnproc.eq.1) then
        lpunit=19
        open (unit=lpunit,file=trim(flnminp)//'PIPE_base.out',
     &        status='unknown')
        write(lpunit,'(a,i2)') 'symmetry type is:',nsym
        write(lp,    '(a,i2)') 'symmetry type is:',nsym
        call flush(lpunit)
        endif
        if     (nsym.ne. 0 .and.
     &          nsym.ne. 1 .and.
     &          nsym.ne. 2 .and.
     &          nsym.ne.-2      ) then
          if     (mnproc.eq.1) then
          write(lp,'(a)') 'symmetry type is not supported'
          endif
          call xcstop('(pipe_init)')
                 stop '(pipe_init)'
        endif
        call xcsync(flush_lp)
      endif ! sym
c
      return
      end subroutine pipe_init
c
#if defined(OCEANS2)
      subroutine pipe_compare(field,mask,what)
      use mod_cb_arrays  ! HYCOM saved arrays, for nstep
      implicit none
c
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: field
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: mask
      character*12,
     &         intent(in) :: what
c
c --- call this routine from anywhere in the code (from both versions, of
c --- course) to check whether data stored in 'field' are identical
c --- master and slave in same executable, uses xcpipe to exchange fields
c
      integer      i,i1,j,j1
      logical      fail
      character*12 which
      real         fnan  !target for huge
c
      if (lpipeio .and. nstep.ge.nstep_start) then
        if (lmaster) then
          do j=1,jj
            do i=1,ii
              amask(i,j) = mask(i,j)
            enddo
          enddo
          call xcaget(tmask,  amask, 1)
        endif !master
        call xcaget(field2, field, 1)
c
        if     (mnproc.eq.1) then
          if (lslave) then
            write (lpunit,'(2a)') 'writing for comparison: ',what
            call flush(lpunit)
            call xcpipe(field1,which, field2,what)
          else  ! master
            call xcpipe(field1,which, field2,what)
            write (lpunit,'(2a)') 'reading for comparison: ',which
            call flush(lpunit)
            if (what.ne.which) then
              write (lpunit,'(4a)') 'out of sync -- trying to compare ',
     &           what,'  to  ',which
              call xchalt('(pipe_compare)')
                     stop '(pipe_compare)'
            endif
c
            fail=.false.
            do j=1,jtdm
              do i=1,itdm
                if (tmask(i,j).gt.0.0 .and.
     &              field2(i,j).ne.field1(i,j)) then
c-----------------if     (hycom_isnaninf(field2(i,j))) then
                  if     (.not.
     &                    (field2(i,j).ge.-huge(fnan) .and.
     &                     field2(i,j).le. huge(fnan)      )) then
                    write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &                'i,j=',i,j,
     &                '  master:',field2(i,j),
     &                '  slave:', field1(i,j),what
                  else
                    write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &                'i,j=',i,j,
     &                '  master:',field2(i,j),
     &                '  error:', field2(i,j)-field1(i,j),what
                  endif
                  fail=.true.
                endif
              enddo
            enddo
            lpipe_anyfailed = lpipe_anyfailed .or. fail
            if (lpipe_fatal .and. fail) then  ! exit
              call xchalt('(pipe_compare)')
                     stop '(pipe_compare)'
            endif
          endif !slave:master
        endif !1st tile
        call xcsync(no_flush) ! wait for 1st tile
      endif !lpipeio
      return
      end subroutine pipe_compare
#else
      subroutine pipe_compare(field,mask,what)
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: field
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: mask
      character*12,
     &         intent(in) :: what
c
c --- call this routine from anywhere in the code (from both versions, of
c --- course) to check whether data stored in 'field' are identical
c --- uses short reads and writes to a named pipe
c
      integer      i,i1,j,j1
      logical      fail
      character*12 which
      real         fnan  !target for huge
c
      if (lpipeio .and. nstep.ge.nstep_start) then
        if (lmaster) then
          do j=1,jj
            do i=1,ii
              amask(i,j) = mask(i,j)
            enddo
          enddo
          call xcaget(tmask,  amask, 1)
        endif !master
        call xcaget(field2, field, 1)
c
        if     (mnproc.eq.1) then
          if (lslave) then
            if (.not.lshift) then
              write (lpunit,'(2a)') 'writing for comparison: ',what
              call flush(lpunit)
              do j= 1,jtdm
                write (ipunit) what, field2(:,j)
              enddo !j
            else  ! shift slave array by ishift,jshift
              do j=1,jtdm
                j1 = mod( j-1+jshift+jtdm, jtdm ) + 1
                do i=1,itdm
                  i1 = mod( i-1+ishift+itdm, itdm ) + 1
                  field1(i1,j1) = field2(i,j)
                enddo
              enddo
              write (lpunit,'(2a)') 'writing for comparison: ',what
              call flush(lpunit)
              do j= 1,jtdm
                write (ipunit) what, field1(:,j)
              enddo !j
            endif
          else  ! master
            do j= 1,jtdm
              read  (ipunit) which,field1(:,j)
            enddo !j
            write (lpunit,'(2a)') 'reading for comparison: ',which
            call flush(lpunit)
            if (what.ne.which) then
              write (lpunit,'(4a)') 'out of sync -- trying to compare ',
     &           what,'  to  ',which
              call xchalt('(pipe_compare)')
                     stop '(pipe_compare)'
            endif
c
            fail=.false.
            do j=1,jtdm
              do i=1,itdm
                if (tmask(i,j).gt.0.0 .and.
     &              field2(i,j).ne.field1(i,j)) then
c-----------------if     (hycom_isnaninf(field2(i,j))) then
                  if     (.not.
     &                    (field2(i,j).ge.-huge(fnan) .and.
     &                     field2(i,j).le. huge(fnan)      )) then
                    write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &                'i,j=',i,j,
     &                '  master:',field2(i,j),
     &                '  slave:', field1(i,j),what
                  else
                    write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &                'i,j=',i,j,
     &                '  master:',field2(i,j),
     &                '  error:', field2(i,j)-field1(i,j),what
                  endif
                  fail=.true.
                endif
              enddo
            enddo
            lpipe_anyfailed = lpipe_anyfailed .or. fail
            if (lpipe_fatal .and. fail) then  ! exit
              call xchalt('(pipe_compare)')
                     stop '(pipe_compare)'
            endif
          endif !slave:master
        endif !1st tile
        call xcsync(no_flush) ! wait for 1st tile
      endif !lpipeio
      if     (ldebug .and. ittest.ne.-1 .and. jttest.ne.-1) then
        if     (i0.lt.ittest .and. i0+ii.ge.ittest .and.
     &          j0.lt.jttest .and. j0+jj.ge.jttest      ) then
          write (lp,'(a,2i5,2x,a,a,1pg24.10)')
     &      'i,j=',itest+i0,jtest+j0,
     &      what,': ',
     &      field(itest,jtest)
        endif
      endif
      return
      end subroutine pipe_compare
#endif /* OCEANS2:else */

      subroutine pipe_compare_sym1(field,mask,what)
      implicit none
c
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: field
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: mask
      character*12,
     &         intent(in) :: what
c
c --- call this routine from anywhere in the code 
c --- to check whether data stored in 'field' is symmetric or
c --- contains NaNs or Infs.
c
c --- pass through to pipe_compare when in master/slave mode.
c
      integer      i,io,j,jo
      logical      fail
      real         fnan  !target for huge
c
      if     (lpipeio .or. ldebug) then
        call pipe_compare(field,mask,what)
      elseif (lsym) then
        do j=1,jj
          do i=1,ii
            amask(i,j) = mask(i,j)
          enddo
        enddo
        call xcaget(tmask,  amask, 1)
        call xcaget(field1, field, 1)
        if     (mnproc.eq.1) then
          write (lpunit,'(2a)') 'comparing: ',what
          call flush(lpunit)
          fail=.false.
          if     (nsym.eq.-2) then  !arctic
            j  = jtdm
            jo = jtdm-1
            do i=1,itdm
              io = itdm-mod(i-1,itdm)
              if (tmask(i,j).gt.0.0 .and.
     &            field1(i,j).ne.field1(io,jo)) then
                write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &            'i,j=',i,j,
     &            '  orig :',field1(i,j),
     &            '  error:',field1(i,j)-field1(io,jo),what
                fail=.true.
              endif
            enddo !i
          else  !standard symteries
            do j=1,jtdm
            do i=1,itdm
              if (tmask(i,j).gt.0.0) then
              if     (nsym.eq.0) then  ! constant field
                if (field1(i,j).ne.field1(1,1)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j)-field1(1,1),what
                  fail=.true.
                endif
              elseif (nsym.eq.2) then  ! constant field in j direction
                if (field1(i,j).ne.field1(i,1)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j)-field1(i,1),what
                  fail=.true.
                endif
              elseif (nsym.eq.1) then  ! p=p.transpose
                if (tmask(i,j).gt.0.0 .and.
     &              field1(i,j).ne.field1(j,i)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j)-field1(j,i),what
                  fail=.true.
                endif
              endif !nsym
              endif !mask
            enddo !i
            enddo !j
          endif !nsym==-2:else
          lpipe_anyfailed = lpipe_anyfailed .or. fail
          if (lpipe_fatal .and. fail) then  ! exit
            call flush(lpunit)
            call xchalt('(pipe_compare_sym1)')
                   stop '(pipe_compare_sym1)'
          endif
        endif !1st tile
        call xcsync(no_flush) ! wait for 1st tile
      elseif (lnan) then
        if     (mnproc.eq.1) then
          write (lpunit,'(2a)') 'checking: ',what
          call flush(lpunit)
        endif !1st tile
        call xcsync(flush_lp)
c ---   do the NaN checking on the local tile
        do j=1,jj
          do i=1,ii
            if     (mask(i,j).gt.0) then
c-------------if     (hycom_isnaninf(field(i,j))) then
              if     (.not.
     &                (field(i,j).ge.-huge(fnan) .and.
     &                 field(i,j).le. huge(fnan)      )) then
                write (lpunit,'(a,a,2i5)')
     &            what,' (NaN): i,j=',i+i0,j+j0
                lnan_anyfailed = .true.  !local fail
              endif
            endif
          enddo !i
        enddo !j
        call xcsync(flush_lp)
      endif !lpipeio:sym:nan
      return
      end subroutine pipe_compare_sym1

      subroutine pipe_compare_sym2(field_u,mask_u,what_u,
     &                             field_v,mask_v,what_v)
      implicit none
c
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: field_u,field_v
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: mask_u,mask_v
      character*12,
     &         intent(in) :: what_u,what_v
c
c --- call this routine from anywhere in the code 
c --- to check whether data stored in 'field_[uv]' is symmetric or
c --- contains NaNs or Infs.
c
c --- pass through to pipe_compare when in master/slave mode.
c
      integer      i,io,j,jo
      logical      fail
c
      if     (lpipeio .or. ldebug) then
        call pipe_compare(field_u,mask_u,what_u)
        call pipe_compare(field_v,mask_v,what_v)
      elseif (lnan) then
        call pipe_compare_sym1(field_u,mask_u,what_u)
        call pipe_compare_sym1(field_v,mask_v,what_v)
      elseif (lsym) then
        do j=1,jj
          do i=1,ii
            amask(i,j) = mask_u(i,j)
          enddo
        enddo
        call xcaget(tmask,  amask,   1)
        call xcaget(field1, field_u, 1)
        call xcaget(field2, field_v, 1)
        if     (nsym.eq.-2) then  !arctic
          do j=1,jj
            do i=1,ii
              amask(i,j) = mask_v(i,j)
            enddo
          enddo
          call xcaget(vmask,  amask,   1)
        endif
        if     (mnproc.eq.1) then
          write (lpunit,'(4a)') 'comparing: ',what_u,' and ',what_v
          call flush(lpunit)
          fail=.false.
          if     (nsym.eq.-2) then  !arctic
            j  = jtdm
            jo = jtdm-1
            do i=1,itdm
              io = mod(itdm-(i-1),itdm)+1
              if (tmask(i,j).gt.0.0 .and.
     &            field1(i,j).ne.-field1(io,jo)) then
                write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &            'i,j=',i,j,
     &            '  orig :',field1(i,j),
     &            '  error:',field1(i,j)+field1(io,jo),what_u
                fail=.true.
              endif
            enddo !i
            j  = jtdm
            jo = jtdm
            do i=1,itdm
              io = itdm-mod(i-1,itdm)
              if (vmask(i,j).gt.0.0 .and.
     &            field2(i,j).ne.-field2(io,jo)) then
                write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &            'i,j=',i,j,
     &            '  orig :',field2(i,j),
     &            '  error:',field2(i,j)+field2(io,jo),what_v
                fail=.true.
              endif
            enddo !i
          else  !standard symteries
            do j=1,jtdm
            do i=1,itdm
              if     (nsym.eq.0) then  ! constant field
                if (field1(i,j).ne.field1(1,1)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j)-field1(1,1),what_u
                  fail=.true.
                endif
                if (field2(i,j).ne.field2(1,1)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field2(i,j),
     &              '  error:',field2(i,j)-field2(1,1),what_v
                  fail=.true.
                endif
              elseif (nsym.eq.2) then  ! constant field in j direction
                if (field1(i,j).ne.field1(i,1)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j)-field1(i,1),what_u
                  fail=.true.
                endif
                if (field2(i,j).ne.field2(i,1)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field2(i,j),
     &              '  error:',field2(i,j)-field2(i,1),what_v
                  fail=.true.
                endif
              elseif (nsym.eq.1) then  ! u==v.transpose
                if (tmask(i,j).gt.0.0 .and.
     &              field1(i,j).ne.field2(j,i)) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  uvel :',field1(i,j),
     &              '  error:',field1(i,j)-field2(j,i),what_u
                  fail=.true.
                endif
              endif
            enddo !i
            enddo !j
          endif
          lpipe_anyfailed = lpipe_anyfailed .or. fail
          if (lpipe_fatal .and. fail) then  ! exit
            call xchalt('(pipe_compare_sym2)')
                   stop '(pipe_compare_sym2)'
          endif
        endif !1st tile
        call xcsync(no_flush) ! wait for 1st tile
      endif !lpipeio:nan:sym
      return
      end subroutine pipe_compare_sym2

      subroutine pipe_compare_same(fielda,fieldb,mask,what)
      implicit none
c
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: fielda,fieldb
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: mask
      character*12,
     &         intent(in) :: what
c
c --- call this routine from anywhere in the code 
c --- to check whether data stored in 'fielda' and 'fieldb'
c --- are identical.
c
c --- only active in PIPE_TRACER mode.
c --- typically fielda is temp and fieldb is a "temperature" tracer.
c
      integer      i,j
      logical      fail
c
      if     (ltracer) then
        do j=1,jj
          do i=1,ii
            amask(i,j) = mask(i,j)
          enddo
        enddo
        call xcaget(tmask,  amask,  1)
        call xcaget(field1, fielda, 1)
        call xcaget(field2, fieldb, 1)
        if     (mnproc.eq.1) then
          write (lpunit,'(2a)') 'comparing: ',what
          call flush(lpunit)
          fail=.false.
          do j=1,jtdm
            do i=1,itdm
              if (tmask(i,j).gt.0.0 .and.
     &            field1(i,j).ne.field2(i,j)) then
                write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &            'i,j=',i,j,
     &            '  orig :',field1(i,j),
     &            '  error:',field1(i,j)-field2(i,j),what
                fail=.true.
              endif
            enddo
          enddo
          lpipe_anyfailed = lpipe_anyfailed .or. fail
          if (lpipe_fatal .and. fail) then  ! exit
            call xchalt('(pipe_compare_same)')
                   stop '(pipe_compare_same)'
          endif
        endif !1st tile
        call xcsync(no_flush) ! wait for 1st tile
      endif !ltracer
      return
      end subroutine pipe_compare_same

      subroutine pipe_compare_notneg(field,mask,what,field_max)
      implicit none
c
      real,    dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: field
      integer, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in) :: mask
      character*12,
     &         intent(in) :: what
      real,    
     &         intent(in) :: field_max
c
c --- call this routine from anywhere in the code 
c --- to check whether data stored in 'field' is non-negative.
c
c --- only active in PIPE_TRACER mode.
c --- typically field is a tracer.
c
      integer      i,j
      logical      fail
      real         fnan  !target for huge
c
      if     (ltracernan) then
        if     (mnproc.eq.1) then
          write (lpunit,'(2a)') 'checking: ',what
          call flush(lpunit)
        endif !1st tile
        call xcsync(flush_lp)
c ---   do the NaN checking on the local tile
        fail=.false.
        do j=1,jj
          do i=1,ii
            if     (mask(i,j).gt.0) then
c-------------if     (hycom_isnaninf(field(i,j))) then
              if     (.not.
     &                (field(i,j).ge.-huge(fnan) .and.
     &                 field(i,j).le. huge(fnan)      )) then
                write (lpunit,'(a,a,2i5)')
     &            what,' (NaN): i,j=',i+i0,j+j0
                fail=.true.
              endif
            endif
          enddo !i
        enddo !j
        call xcsync(flush_lp)
        lpipe_anyfailed = lpipe_anyfailed .or. fail
        if (lpipe_fatal .and. fail) then  ! exit
          call xchalt('(pipe_compare_notneg)')
                 stop '(pipe_compare_notneg)'
        endif
      endif
c
      if     (ltracer) then
        do j=1,jj
          do i=1,ii
            amask(i,j) = mask(i,j)
          enddo
        enddo
        call xcaget(tmask,  amask,  1)
        call xcaget(field1, field, 1)
        if     (mnproc.eq.1) then
          write (lpunit,'(2a)') 'comparing: ',what
          call flush(lpunit)
          fail=.false.
          do j=1,jtdm
            do i=1,itdm
              if (tmask(i,j).gt.0.0) then
                if (field1(i,j).lt.0.0) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j),what
                  fail=.true.
                elseif (field1(i,j).gt.field_max) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j)-field_max,what
                  fail=.true.
c---------------elseif (hycom_isnaninf(field1(i,j))) then
                elseif (.not.
     &                  (field1(i,j).ge.-huge(fnan) .and.
     &                   field1(i,j).le. huge(fnan)      )) then
                  write (lpunit,'(a,2i5,1p,2(a,e12.5),4x,a)')
     &              'i,j=',i,j,
     &              '  orig :',field1(i,j),
     &              '  error:',field1(i,j),what
                  fail=.true.
                endif !errors
              endif !tmask.gt.0
            enddo
          enddo
          lpipe_anyfailed = lpipe_anyfailed .or. fail
          if (lpipe_fatal .and. fail) then  ! exit
            call xchalt('(pipe_compare_notneg)')
                   stop '(pipe_compare_notneg)'
          endif
        endif !1st tile
        call xcsync(no_flush) ! wait for 1st tile
      endif !ltracer
      return
      end subroutine pipe_compare_notneg

      subroutine pipe_comparall(m,n, cinfo)
      use mod_cb_arrays  ! HYCOM saved arrays
#if defined(STOKES)
      use mod_stokes     ! HYCOM Stokes drift
#endif
      implicit none
c
      integer, intent(in) :: m,n
      character*12,
     &         intent(in) :: cinfo
c
c --- write out a standard menu of arrays for testing
c
      logical hycom_isnaninf  !function to detect NaN and Inf
c
      character*99 cformat
      character*12 txt1,txt2
      integer      i,imax,imin,j,jmax,jmin,k,ktr,l,mnp
      real         diso,dmax,dmin,damax,damin
      real*8       tmean,smean,pmean,rmean
      real*8       d1,d2,d3,d4
      real         r1,r2,r3,r4
      real         fnan  !target for huge
c
      real*8       tmean0,smean0,rmean0,
     &             tmean1,smean1,rmean1
      save         tmean0,smean0,rmean0,
     &             tmean1,smean1,rmean1
      data         tmean0,smean0,rmean0 / 3*0.0d0 /
c
cdiag if     (mnproc.eq.1) then
cdiag write(lp,'(a,i10)') cinfo,nstep
cdiag call flush(lp)
cdiag endif
c
      if     (ldebugssh) then
c
c ---   printout SSH in cm at point itest,jtest.
c
        if     (min(ittest,jttest).le.0) then
          call xcstop('(pipe_comparall: debug_ssh ambiguity)')
                 stop '(pipe_comparall: debug_ssh ambiguity)'
        endif
        if     (i0.lt.ittest .and. i0+ii.ge.ittest .and.
     &          j0.lt.jttest .and. j0+jj.ge.jttest      ) then
c         ssh,montg,thref*pbavg (cm)
          write (lp,"(i8,i5,i5,1x,a,a,3f15.8)")
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),':',
     &       (100.0/g)*srfhgt(itest,jtest),
     &       (100.0/g)*montg1(itest,jtest),
     &       (100.0/g)*srfhgt(itest,jtest)-
     &       (100.0/g)*montg1(itest,jtest)
        endif  ! ittest,jttest tile
        call xcsync(flush_lp)
c
c ---   check SSH for NaN, but continue even if NaN detected
c
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0) then
c-------------if     (hycom_isnaninf(srfhgt(i,j))) then
              if     (.not.
     &                (srfhgt(i,j).ge.-huge(fnan) .and.
     &                 srfhgt(i,j).le. huge(fnan)      )) then
                write (lp,"(i8,i5,i5,1x,a,a,3f15.8)")
     &             nstep,i+i0,j+j0,cinfo(1:6),':   NaN'
              endif !hycom_isnaninf
            endif !ip
          enddo !i
        enddo !j
        call xcsync(flush_lp)
      endif !ldebugssh
c
      if     (ldebug .and. ittest.ne.-1 .and. jttest.ne.-1) then
c
c ---   printout at point itest,jtest.
c
        if     (min(ittest,jttest).le.0) then
          call xcstop('(pipe_comparall: debug ambiguity)')
                 stop '(pipe_comparall: debug ambiguity)'
        endif
        if     (i0.lt.ittest .and. i0+ii.ge.ittest .and.
     &          j0.lt.jttest .and. j0+jj.ge.jttest      ) then
        if     (cinfo(1:6).eq.'momtum') then
          write(cformat,'(a,a)')
     &      '(i8,i5,i5,1x,a,a/',
     &      '(i8,5x,i5,1x,a,a,1p4e12.4))'
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':       surtx       surty      srfhgt      montg1',
     &       nstep,0,          cinfo(1:6),':',
     &        surtx(itest,jtest),
     &        surty(itest,jtest),
     &       srfhgt(itest,jtest),
     &       montg1(itest,jtest)
        endif  !'momtum'
        if     (ntracr.eq.0) then
          write(cformat,'(a,a)')
     &      '(i8,i5,i5,1x,a,a/',
     &      '(i8,5x,i5,1x,a,a,2f7.3,2f7.3,f8.4,f9.3,f10.3))'
        else
          write(cformat,'(a,i2,a,a,i2,a)')
     &      '(i8,i5,i5,1x,a,a,',ntracr,'a / ',
     &      '(i8,5x,i5,1x,a,a,2f7.3,2f7.3,f8.4,f9.3,f10.3,',
     &      ntracr,'f8.4))'
        endif
*       write(lp,'(3a)') '"',trim(cformat),'"'
        if     (.not.mxlkrt) then
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':  ubaro  vbaro ub.i+1 vb.i+1  ub.j+1   vb.j+1      dpth',
     &      ('    zero',ktr=1,ntracr),
     &       nstep,0,          cinfo(1:6),'>',
     &       ubavg(itest,jtest,  m),
     &       vbavg(itest,jtest,  m),
     &       ubavg(itest+1,jtest,m),
     &       vbavg(itest+1,jtest,m),
     &       ubavg(itest,jtest+1,m),
     &       vbavg(itest,jtest+1,m),
     &           p(itest,jtest,kk+1)/onem,
     &       (0.0,ktr=1,ntracr),
     &       nstep,0,          cinfo(1:6),':',
     &       ubavg(itest,  jtest,  n),
     &       vbavg(itest,  jtest,  n),
     &       ubavg(itest+1,jtest,  n),
     &       vbavg(itest+1,jtest,  n),
     &       ubavg(itest,  jtest+1,n),
     &       vbavg(itest,  jtest+1,n),
     &           p(itest,  jtest,kk+1)/onem,
     &       (0.0,ktr=1,ntracr)
#if defined(STOKES)
          if     (allocated(usdbavg)) then
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':UsdbaroVsdbaro ub.i+1 vb.i+1  ub.j+1   vb.j+1      dpth',
     &       nstep,0,          cinfo(1:6),'#',
     &       usdbavg(itest,  jtest),
     &       vsdbavg(itest,  jtest),
     &       usdbavg(itest+1,jtest),
     &       vsdbavg(itest+1,jtest),
     &       usdbavg(itest,  jtest+1),
     &       vsdbavg(itest,  jtest+1),
     &             p(itest,  jtest,kk+1)/onem
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ': usdtot vsdtot   temp   saln    dens    thkns      dpth',
     &      (nstep,k,          cinfo(1:6),'#',
     &        usd(itest,jtest,k),
     &        vsd(itest,jtest,k),
     &       temp(itest,jtest,k,m),
     &       saln(itest,jtest,k,m),
     &       th3d(itest,jtest,k,m)+thbase,
     &         dp(itest,jtest,k,m)/onem,  !RA       time t
     &        dpo(itest,jtest,k,m)/onem,  !original time t
     &       k=1,2)
          endif !allocated
#endif
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':   utot   vtot   temp   saln    dens    thkns      dpth',
     &      ('  tracer',ktr=1,ntracr),
     &      (nstep,k,          cinfo(1:6),'>',
     &          u(itest,jtest,k,m)+ubavg(itest,jtest,m),
     &          v(itest,jtest,k,m)+vbavg(itest,jtest,m),
     &       temp(itest,jtest,k,m),
     &       saln(itest,jtest,k,m),
     &       th3d(itest,jtest,k,m)+thbase,
     &         dp(itest,jtest,k,m)/onem,  !RA       time t
     &        dpo(itest,jtest,k,m)/onem,  !original time t
     &       (tracer(itest,jtest,k,m,ktr),ktr=1,ntracr),
     &       nstep,k,          cinfo(1:6),':',
     &          u(itest,jtest,k,n)+ubavg(itest,jtest,n),
     &          v(itest,jtest,k,n)+vbavg(itest,jtest,n),
     &       temp(itest,jtest,k,n),
     &       saln(itest,jtest,k,n),
     &       th3d(itest,jtest,k,n)+thbase,
     &         dp(itest,jtest,k,n)/onem,
     &          p(itest,jtest,k+1)/onem,
     &       (tracer(itest,jtest,k,n,ktr),ktr=1,ntracr),
     &       k=1,kk)
        else
c ---     include KT mixed layer values.
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':   utot   vtot   temp   saln    dens    thkns      dpth',
     &      ('  tracer',ktr=1,ntracr),
     &       nstep,0,          cinfo(1:6),':',
     &          0.0,
     &          0.0,
     &       tmix(itest,jtest),     
     &       smix(itest,jtest),     
     &      thmix(itest,jtest)+thbase,
     &     dpmixl(itest,jtest,n)/onem,
     &     dpmixl(itest,jtest,n)/onem,
     &       (0.0,ktr=1,ntracr),
     &      (nstep,k,          cinfo(1:6),':',
     &          u(itest,jtest,k,n)+ubavg(itest,jtest,n),
     &          v(itest,jtest,k,n)+vbavg(itest,jtest,n),
     &       temp(itest,jtest,k,n),
     &       saln(itest,jtest,k,n),
     &       th3d(itest,jtest,k,n)+thbase,
     &         dp(itest,jtest,k,n)/onem,
     &          p(itest,jtest,k+1)/onem,
     &       (tracer(itest,jtest,k,n,ktr),ktr=1,ntracr),
     &       k=1,kk)
        endif
        if     (mxlmy) then
          write(cformat,'(a,a)')
     &      '(i8,i5,i5,1x,a,a/',
     &      '(i8,5x,i5,1x,a,a,g15.5,g15.5,f9.3,f9.2))'
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':             q2            q2l    thkns     dpth',
     &      (nstep,k,          cinfo(1:6),':',
     &         q2(itest,jtest,k,n),
     &        q2l(itest,jtest,k,n),
     &         dp(itest,jtest,k,n)/onem,
     &          p(itest,jtest,k+1)/onem,
     &       k=1,kk)
        endif  !'mxlmy'
        if     (cinfo(1:6).eq.'mxkprf' .and. .not.mxlkrt) then
          write(cformat,'(a,a)')
     &      '(i8,i5,i5,1x,a,a/',
     &      '(i8,5x,i5,1x,a,a,f7.3,f8.2,f7.3,f8.2,f9.3,f9.2))'
          write (lp,cformat) 
     &       nstep,itest+i0,jtest+j0,cinfo(1:6),
     &       ':   temp  t-diff   saln  s-diff    thkns     dpth',
     &      (nstep,k,          cinfo(1:6),':',
     &       temp(itest,jtest,k,n),
     &       dift(itest,jtest,k+1)*1.e4,
     &       saln(itest,jtest,k,n),
     &       difs(itest,jtest,k+1)*1.e4,
     &         dp(itest,jtest,k,n)/onem,
     &          p(itest,jtest,k+1)/onem,
     &       k=1,klist(itest,jtest))
        endif  !'mxkprf'
        endif  ! ittest,jttest tile
        call xcsync(flush_lp)
      endif
c
      if     (ldebug .and. ittest.eq.-1) then
c
c ---   printout min/max/iospycnal th3d
c
 104    format (i8,a3,1x,a,a)
 105    format (i8,i3,1x,a,a,2i5,f9.5,f7.3,f9.5,2i5,i7)
        if     (mnproc.eq.1) then
        write(lp,104)
     &       nstep,'  k',cinfo(1:6),
     &       ': imin jmin  denamin deniso  denamax imax jmax mnproc'
        endif
        call xcsync(flush_lp)
        do k= 1,kk
          diso=sigma(k)-thbase
          dmin= huge(dmin)
          dmax=-huge(dmax)
          do j= 1,jj
            do i= 1,ii
              if     (ip(i,j).eq.1) then
                if     (th3d(i,j,k,n).lt.dmin) then
                  dmin=th3d(i,j,k,n)
                  imin=i
                  jmin=j
                endif
                if     (th3d(i,j,k,n).gt.dmax) then
                  dmax=th3d(i,j,k,n)
                  imax=i
                  jmax=j
                endif
              endif
            enddo
          enddo
          damin=dmin
          call xcminr(damin)
          damax=dmax
          call xcmaxr(damax)
          do mnp= 1,ijpr
            if     (mnp.eq.mnproc) then
              if     (dmin.eq.damin .or. dmax.eq.damax) then
                write (lp,105) 
     &            nstep,k,cinfo(1:6),
     &            ':',imin,jmin,dmin-diso,
     &                          diso+thbase,
     &                          dmax-diso,imax,jmax,mnproc
              endif
            endif
            call xcsync(flush_lp)
          enddo
        enddo
        call flush(lp)
      endif
c
      if     (ldebug .and. jttest.eq.-1) then
c
c ---   printout basin-wide means.
c
!$OMP   PARALLEL DO PRIVATE(j,k,l,i)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (ip(i,j).ne.0) then
                util5(i,j)=              dp(i,j,1,n)*scp2(i,j)
                util6(i,j)=temp(i,j,1,n)*dp(i,j,1,n)*scp2(i,j)
                util3(i,j)=saln(i,j,1,n)*dp(i,j,1,n)*scp2(i,j)
                util4(i,j)=th3d(i,j,1,n)*dp(i,j,1,n)*scp2(i,j)
              do k=2,kk
                util5(i,j)=util5(i,j)+
     &                                   dp(i,j,k,n)*scp2(i,j)
                util6(i,j)=util6(i,j)+
     &                     temp(i,j,k,n)*dp(i,j,k,n)*scp2(i,j)
                util3(i,j)=util3(i,j)+
     &                     saln(i,j,k,n)*dp(i,j,k,n)*scp2(i,j)
                util4(i,j)=util4(i,j)+
     &                     th3d(i,j,k,n)*dp(i,j,k,n)*scp2(i,j)
              enddo !k
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
        call xcsum(d1, util5,ipa)
        call xcsum(d2, util6,ipa)
        call xcsum(d3, util3,ipa)
        call xcsum(d4, util4,ipa)
        pmean=d1
        tmean=d2/pmean
        smean=d3/pmean
        rmean=d4/pmean
c
 106    format (i8,3x,1x,a,a,3f8.4,1p3e10.2)
        if     (mnproc.eq.1) then
        write (lp,106)
     &    nstep,cinfo(1:6),
     &    ': t,s,th',
     &    tmean,smean,rmean+thbase,
     &    tmean-tmean0,smean-smean0,rmean-rmean0
        call flush(lp)
        endif
c
c ---   NaN detection.
        r1 = d1
        r2 = d2
        r3 = d3
        r4 = d4
        if     (hycom_isnaninf(r1) .or.
     &          hycom_isnaninf(r2) .or.
     &          hycom_isnaninf(r3) .or.
     &          hycom_isnaninf(r4)     ) then
          if (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error - NaN or Inf detected'
          write(lp,*)
          call flush(lp)
          endif !1st tile
        endif !NaN
!$OMP   PARALLEL DO PRIVATE(j,i,k)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if     (iu(i,j).eq.1) then
              util5(i,j)=u(i,j,1,n)
              do k=2,kk
                util5(i,j)=util5(i,j)+u(i,j,k,n)
              enddo
            endif !iu
            if     (iv(i,j).eq.1) then
              util6(i,j)=v(i,j,1,n)
              do k=2,kk
                util6(i,j)=util6(i,j)+v(i,j,k,n)
              enddo
            endif !iv
          enddo
        enddo
!$OMP   END PARALLEL DO
        call xcsum(d1, util5,iu)
        call xcsum(d2, util6,iv)
        r1 = d1
        r2 = d2
        if     (hycom_isnaninf(r1) .or.
     &          hycom_isnaninf(r2)     ) then
          if (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) 'error - u or v NaN or Inf detected'
          write(lp,*)
          call flush(lp)
          endif !1st tile
        endif !NaN
c
        if     (cinfo(1:6).eq.'ENTER ') then
          tmean1=tmean
          smean1=smean
          rmean1=rmean
        elseif (cinfo(1:6).eq.'tsadvc') then
          if     (mnproc.eq.1) then
          write (lp,106)
     &      nstep,'cn+tsa',
     &      ': t,s,th',
     &      tmean,smean,rmean+thbase,
     &      tmean-tmean1,smean-smean1,rmean-rmean1
          call flush(lp)
          endif
        elseif (cinfo(1:6).eq.'hybgen') then
          if     (mnproc.eq.1) then
          write (lp,106)
     &      nstep,'EXIT  ',
     &      ': t,s,th',
     &      tmean,smean,rmean+thbase,
     &      tmean-tmean1,smean-smean1,rmean-rmean1
          call flush(lp)
          endif
        endif
c
        tmean0=tmean
        smean0=smean
        rmean0=rmean
      endif
c
      if     ((ltracer .or. ltracernan) .and. cinfo(1:1).ne.'i') then
        do ktr= 1,ntracr
          if     (mnproc.eq.1) then
          write (lpunit,'(a,i10)') cinfo,nstep
          endif
          call xcsync(flush_lp)
          if     (trcflg(ktr).eq.2) then
c
c ---       compare temp and this temperature tracer.
c
            do k=1,kk
              write (txt1,'(a9,i3)') 'temp(kn) ',k
              call pipe_compare_same(  temp(1-nbdy,1-nbdy,k,n),
     &                               tracer(1-nbdy,1-nbdy,k,n,ktr),
     &                               ip,txt1)
            enddo
          else
c
c ---       check that tracer is non-negative.
c
            do k=1,kk
              write (txt1,'(a6,i3,i3)') 'tracer',ktr,k
              call pipe_compare_notneg(tracer(1-nbdy,1-nbdy,k,n,ktr),
     &                                 ip,txt1,trcmax(ktr))
            enddo
          endif
        enddo !ktr
        if     (mnproc.eq.1) then
        write (lpunit,'(a,i10,a)') cinfo,nstep,' -- OK'
        endif
        call xcsync(flush_lp)
      endif !ltracer
c
      if     (lpipe) then
c
c ---   pipe_compare_sym[12] works for both lsym and lpipeio.
c ---   exit at the end on differences
c
        call pipe_fatal_off
c
        if     (mnproc.eq.1) then
        write (lpunit,'(a,i10)') cinfo,nstep
        endif
        call xcsync(flush_lp)
        txt1='ubavg(n)    '
        txt2='vbavg(n)    '
        call pipe_compare_sym2(ubavg(1-nbdy,1-nbdy,n),iu,txt1,
     &                         vbavg(1-nbdy,1-nbdy,n),iv,txt2)
        txt1='pbavg(n)    '
        call pipe_compare_sym1(pbavg(1-nbdy,1-nbdy,n),ip,txt1)
        txt1='montg(1)    '
        call pipe_compare_sym1(montg1(1-nbdy,1-nbdy),ip,txt1)
        if     (cinfo(1:6).eq.'icloan' .or.
     &          cinfo(1:6).eq.'icecpl'     ) then  !ice fields
          txt1='covice      '
          call pipe_compare_sym1(covice,ip,txt1)
          txt1='flxice      '
          call pipe_compare_sym1(flxice,ip,txt1)
          txt1='fswice      '
          call pipe_compare_sym1(fswice,ip,txt1)
          txt1='sflice      '
          call pipe_compare_sym1(sflice,ip,txt1)
        endif
        if     (cinfo(1:6).eq.'icloan' .or.
     &          cinfo(1:6).eq.'icecpl' .or.
     &          cinfo(1:6).eq.'thermf'     ) then  !surface fields
          txt1='surflx      '
          call pipe_compare_sym1(surflx,ip,txt1)
          txt1='sswflx      '
          call pipe_compare_sym1(sswflx,ip,txt1)
          txt1='salflx      '
          call pipe_compare_sym1(salflx,ip,txt1)
        endif
        do k=1,kk
          write(txt1(10:12),'(i3)') k
          write(txt2(10:12),'(i3)') k
c
          txt1(1:9) = '...u(kn) '
          txt2(1:9) = '...v(kn) '
          call pipe_compare_sym2(   u(1-nbdy,1-nbdy,k,n),iu,txt1,
     &                              v(1-nbdy,1-nbdy,k,n),iv,txt2)
          txt1(1:9) = '.dpu(kn) '
          txt2(1:9) = '.dpv(kn) '
          call pipe_compare_sym2( dpu(1-nbdy,1-nbdy,k,n),iu,txt1,
     &                            dpv(1-nbdy,1-nbdy,k,n),iv,txt2)
          txt1(1:9) = '..dp(kn) '
          call pipe_compare_sym1(  dp(1-nbdy,1-nbdy,k,n),ip,txt1)
          txt1(1:9) = 'temp(kn) '
          call pipe_compare_sym1(temp(1-nbdy,1-nbdy,k,n),ip,txt1)
          txt1(1:9) = 'saln(kn) '
          call pipe_compare_sym1(saln(1-nbdy,1-nbdy,k,n),ip,txt1)
          txt1(1:9) = 'th3d(kn) '
          call pipe_compare_sym1(th3d(1-nbdy,1-nbdy,k,n),ip,txt1)
*         write(lpunit,'(a,a12,a,i3)') '***',txt1,'*** k=',k
        enddo
c
        if     (lpipe_anyfailed .or. lnan_anyfailed) then  !exit on failure
          call xchalt('(pipe_compare_all)')
                 stop '(pipe_compare_all)'
        else
          if     (mnproc.eq.1) then
          write (lpunit,'(a,i10,a)') cinfo,nstep,' -- OK'
          endif
        endif
        call xcsync(flush_lp)
c
        call pipe_fatal_on
      endif !lpipe
c
      return
      end subroutine pipe_comparall
c
      end module mod_pipe
c
c
c> Revision history:
c>
c> Oct  2000 - added PIPE_DEBUG     for debugging printout
c> Aug  2001 - added PIPE_SHIFT     for shifted   comparision
c> Aug  2001 - added PIPE_SYM       for symmetric comparision
c> Feb  2004 - added PIPE_SYM       arctic option for arctic bipolar patch
c> Jun  2005 - added PIPE_DEBUG_SSH for debugging SSH printout
c> Jan  2012 - added PIPE_NAN       for NaN checking
c> Aug  2012 - added PIPE_TRACERNAN for tracer NaN checking
c> Aug  2012 - added PIPE_TRACERMAX for tracer maximum checking
c> Sep  2012 - pipe files in directory flnminp
c> Feb  2014 - added lpipe_fatal and lpipe_anyfailed
c> Apr  2014 - replace ip with ipa for mass sums
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> Jul  2015 - added lnan_anyfailed
