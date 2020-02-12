#if defined(ROW_LAND)
#define SEA_P .true.
#define SEA_U .true.
#define SEA_V .true.
#elif defined(ROW_ALLSEA)
#define SEA_P allip(j).or.ip(i,j).ne.0
#define SEA_U alliu(j).or.iu(i,j).ne.0
#define SEA_V alliv(j).or.iv(i,j).ne.0
#else
#define SEA_P ip(i,j).ne.0
#define SEA_U iu(i,j).ne.0
#define SEA_V iv(i,j).ne.0
#endif
      module mod_tsadvc
      use mod_xc    ! HYCOM communication interface
      use mod_pipe  ! HYCOM debugging interface
      implicit none
c
c --- module for tsadvc and related routines
c
      private !! default is private
      public  :: tsadvc
c
      integer, parameter, dimension (0:4) ::
     &  mbdy_advtyp = (/ 2,    !PCM
     &                   5,    !MPDATA
     &                   5,    !FCT2
     &                   0,    !N/A
     &                   5 /)  !FCT4
c
      logical, parameter :: lpipe_advem=.false.  !extra checking (when pipe on)
      logical, parameter :: lconserve  =.false.  !explicitly conserve the field
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) ::
#else
      real, save, dimension(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
#endif
     &   fmx,fmn      ! local max,min
     &  ,flx,fly      ! fluxes
     &  ,fldlo        ! lo order solution
     &  ,fmxlo,fmnlo  ! local min
     &  ,fax,fay      ! fluxes
     &  ,rp,rm        ! FCT/MPDATA terms
     &  ,flxdiv       ! flux divergence
     &  ,tx1,ty1      ! MPDATA terms
     &  ,fldao,fldan  ! total field quantity (old/center, new)

      contains

      subroutine advem(advtyp,fld,fldc,u,v,fco,fcn,posdef,
     &                 scal,scali,dt2)
      implicit none
c
      integer, intent(in)    :: advtyp
      real,    intent(in)    :: posdef,dt2
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: fld
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: fldc,u,v,fco,fcn,scal,scali
c
c --- wrapper for advection schemes
c
c --- a recent text on advection schemes is:
c --- D.R. Durran (1999): Numerical Methods for wave equations in
c --- geophysical fluid dynamics, Springer.
c
c --- advtyp= 0 for 1st order PCM    (Donor Cell)
c --- advtyp= 1 for 2nd order MPDATA (old to new, as in 2.1.03)
c --- advtyp= 2 for 2nd order FCT    (Leapfrog time step)
c --- advtyp= 4 for 4th order FCT    (Leapfrog time step)
c
c --- time steps are "old", "center" and "new".
c
c --- fld    - scalar field, at old time step on input but new on output
c --- fldc   - scalar field, at center time step
c --- u,v    - mass fluxes satisfying continuity equation (old to new)
c --- fco    - thickness of the layer at old time step
c --- fcn    - thickness of the layer at new time step
c --- posdef - offset for MPDATA to make the field positive
c --- scal   - spatial increments (squared)
c --- scali  - inverse of scal
c --- dt2    - temporal increment (from old to new, i.e. two time steps)
c
c  on return, fld's valid halo will be 0 wide.
c
      real    offset
      real*8  sumold,sumnew,sumcor
      integer i,j
c
#if defined(RELO)
      if     (.not.allocated(fmx)) then
        allocate(
     &           fmx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           fmn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           flx(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           fly(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         fldlo(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         fmxlo(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         fmnlo(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           fax(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           fay(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            rp(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &            rm(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        flxdiv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           tx1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           ty1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         fldao(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         fldan(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 16*(idm+2*nbdy)*(jdm+2*nbdy) )
                 fmx = r_init
                 fmn = r_init
                 flx = r_init
                 fly = r_init
               fldlo = r_init
               fmxlo = r_init
               fmnlo = r_init
                 fax = r_init
                 fay = r_init
                  rp = r_init
                  rm = r_init
              flxdiv = r_init
                 tx1 = r_init
                 ty1 = r_init
               fldao = r_init
               fldan = r_init
      endif !.not.allocated
#endif
c
      if     (advtyp.eq.0) then
        call advem_pcm(   fld,     u,v,fco,fcn,       scal,scali,dt2)
      elseif (advtyp.eq.1) then
        call advem_mpdata(fld,     u,v,fco,fcn,posdef,scal,scali,dt2)
      elseif (advtyp.eq.2) then
        call advem_fct2(  fld,fldc,u,v,fco,fcn,       scal,scali,dt2)
      elseif (advtyp.eq.4) then
        call advem_fct4(  fld,fldc,u,v,fco,fcn,       scal,scali,dt2)
      else
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i4 /)')
     &      'error: advem called with advtyp =',advtyp
        endif
        call xcstop('advem')
               stop 'advem'
      endif
c
      if     (lconserve) then !usually .false.
c
c ---   explicit conservation of tracer (should not be needed).
c
        call xcsum(sumold, fldao,ipa)
        call xcsum(sumnew, fldan,ipa)
c
        if     (sumnew.ne.0.0) then
          offset = (sumold-sumnew)/sumnew
        else
          offset = 0.0
        endif
c
!$OMP   PARALLEL DO PRIVATE(j,i,offset)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1,jj
          do i=1,ii
            if (SEA_P) then
              fld(i,j)=fld(i,j)*(1.0+offset)
c
cdiag         fldan(i,j) = fld(i,j)*fcn(i,j)*scal(i,j)
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
        if     (lpipe .and. lpipe_advem) then
c ---     compare two model runs.
          call pipe_compare_sym1(fld,    ip,'ad:oset:fld ')
        endif
c
cdiag   call xcsum(sumcor, fldan,ipa)
cdiag   if     (mnproc.eq.1) then
cdiag     write(lp,'(a,1p4e16.8)')
cdiag&      'advem: ',sumold,sumnew,sumcor,offset
cdiag   endif
      endif !lconserve
      return
      end subroutine advem

      subroutine advem_mpdata(fld,u,v,fco,fcn,posdef,scal,scali,dt2)
      implicit none
c
      real,    intent(in)    :: posdef,dt2
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: fld
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: u,v,scal,scali,fco,fcn
c
c LeapFrog 2nd order MPDATA.
c combined monotone scheme, for details see section 3.3 (eqs. 34 to 37)
c in smolarkiewicz and clark, 1986, j.comput.phys.,67,no 2, p. 396-438
c and smolarkiewicz and grabowski, 1989, j.comput.phys. and recently
c P.K. Smolarkiewicz and L.J. Margolin (1998): MPDATA: A finite
c difference solver for geophysical flows, J.Comput.Phys. 140 459-480.
c
c time steps are "old", "center" and "new".
c
c  fld    - scalar field, must be >0, old input but new output
c  u,v    - mass fluxes satisfying continuity equation (old to new)
c  fco    - thickness of the layer at old time step
c  fcn    - thickness of the layer at new time step
c  posdef - offset to make the field positive
c  scal   - spatial increments (squared)
c  scali  - inverse of scal
c  dt2    - temporal increment (from old to new)
c
c  on return, fld's valid halo will be 0 wide.
c
      real,    parameter :: onemu=9806.e-12 !very small layer thickness
c
      real    fcn2,fco2,flxdn,flxdp,flydn,flydp,q
      integer i,j,l,ia,ib,ja,jb,mbdy_a,margin
c
      integer       itest,jtest,ittest,jttest
      common/testpt/itest,jtest,ittest,jttest
      save  /testpt/
c
      mbdy_a = mbdy_advtyp(1)  ! = 5
c
c --- compute low-order and part of antidiffusive fluxes
c
c --- rhs: u, v, fld+
c --- lhs: flx, fly, fmx, fmn
c
      margin = mbdy_a - 1
c
!$OMP PARALLEL DO PRIVATE(j,i,q,jb,ja,ib,ia)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            tx1(i,j)=.5*abs(u(i,j))*(fld(i,j)-fld(i-1,j))
            if (u(i,j).ge.0.0) then
              q=fld(i-1,j)
            else
              q=fld(i  ,j)
            endif
            flx(i,j)=u(i,j)*(q+posdef)
          endif !iu
          if (SEA_V) then
            ty1(i,j)=.5*abs(v(i,j))*(fld(i,j)-fld(i,j-1))
            if (v(i,j).ge.0.0) then
              q=fld(i,j-1)
            else
              q=fld(i,j  )
            endif
            fly(i,j)=v(i,j)*(q+posdef)
          endif !iv
          if (SEA_P) then
            ia=ipim1(i,j) !i-1 if sea and i if land
            ib=ipip1(i,j) !i+1 if sea and i if land
            ja=ipjm1(i,j) !j-1 if sea and j if land
            jb=ipjp1(i,j) !j+1 if sea and j if land
            fmx(i,j)=max(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))+posdef
            fmn(i,j)=min(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))+posdef
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(tx1, iu,'ad:11:tx1   ',
     &                         ty1, iv,'ad:11:ty1   ')
        call pipe_compare_sym2(flx, iu,'ad:11:flx   ',
     &                         fly, iv,'ad:11:fly   ')
        call pipe_compare_sym1(fmx, ip,'ad:11:fmx   ')
        call pipe_compare_sym1(fmn, ip,'ad:11:fmn   ')
      endif
c
c --- rhs: u, v, fld+
c --- lhs: flx, fly, fmx, fmn
c
      margin = mbdy_a - 1
c
      do j=1-margin,jj+margin
        do l=1,isp(j) !ok
          if     (ifp(j,l).ge. 1-margin) then
            flx(ifp(j,l)  ,j)=0.0
          endif
          if     (ilp(j,l).lt.ii+margin) then
            flx(ilp(j,l)+1,j)=0.0
          endif
        enddo !l
      enddo !j
c
      do i=1-margin,ii+margin
        do l=1,jsp(i) !ok
          if     (jfp(i,l).ge. 1-margin) then
            fly(i,jfp(i,l)  )=0.0
          endif
          if     (jlp(i,l).lt.jj+margin) then
            fly(i,jlp(i,l)+1)=0.0
          endif
        enddo !l
      enddo !i
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:22:flx   ',
     &                         fly, iv,'ad:33:fly   ')
      endif
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag   i=itest
cdiag   j=jtest
cdiag   write (lp,'(a,2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.             1pe9.2,0pf9.3/1pe39.2/0pf39.3)')
cdiag.    'advem (1)',i+i0,j+j0,
cdiag.    fld(i-1,j),u(i,j),fld(i,j-1),v(i,j),
cdiag.    fld(i,j),v(i,j+1),fld(i,j+1),u(i+1,j),fld(i+1,j)
cdiag endif
c
c --- rhs: flx+, fly+, fco, fmn, fmx, fcn
c --- lhs: fld
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
!...........lo order Donor Cell step
            flxdiv(i,j)=((flx(i+1,j)-flx(i,j))+
     &                   (fly(i,j+1)-fly(i,j)) )*dt2*scali(i,j)
            q=(fld(i,j)+posdef)*(fco(i,j)+onemu)-flxdiv(i,j)
            !max,min should only be active for very thin layers
            fldlo(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                                     q/(fcn(i,j)+onemu) ))
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(fldlo,  ip,'ad:610:fldlo')
        call pipe_compare_sym1(flxdiv, ip,'ad:610:flxdv')
      endif
c
c --- finish computation of antidiffusive fluxes
c
c --- rhs: tx1, u, ty1, v, flxdiv+, fco+, fcn+
c --- lhs: flx, fly
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,fcn2,fco2)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        fco2=0.0
        fcn2=0.0
        do i=1-margin,ii+margin
          if (SEA_U) then
            fco2=fco(i,j)+fco(i-1,j)  ! inforce order on flx calc
            fcn2=fcn(i,j)+fcn(i-1,j)  ! inforce order on flx calc
            flx(i,j)=tx1(i,j)-u(i,j)*(flxdiv(i,j)+flxdiv(i-1,j))
     &         /((fco2+fcn2)+onemu)
          endif !iu
          if (SEA_V) then
            fco2=fco(i,j)+fco(i,j-1)  ! inforce order on fly calc
            fcn2=fcn(i,j)+fcn(i,j-1)  ! inforce order on fly calc
            fly(i,j)=ty1(i,j)-v(i,j)*(flxdiv(i,j)+flxdiv(i,j-1))
     &         /((fco2+fcn2)+onemu)
          endif !iv
        enddo !i
        if (fco2*fcn2.eq.1.e30) flx(1-nbdy,j)=0.0  ! prevent removal of fc*2
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx,  iu,'ad: 8:flx   ',
     &                         fly,  iv,'ad: 8:fly   ')
      endif
c
c --- limit antidiffusive fluxes
c --- rp and rm used to be called flp and fln
c
c --- rhs: fmx, fmn, fldlo, fcn, flx+, fly+
c --- lhs: rp, rm
c
      margin = mbdy_a - 3
c
!$OMP PARALLEL DO PRIVATE(j,i,flxdn,flxdp,flydn,flydp)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            flxdp=min(0.0,flx(i+1,j))-max(0.0,flx(i,j))
            flxdn=max(0.0,flx(i+1,j))-min(0.0,flx(i,j))
            flydp=min(0.0,fly(i,j+1))-max(0.0,fly(i,j))
            flydn=max(0.0,fly(i,j+1))-min(0.0,fly(i,j))
            rp(i,j)=(fmx(i,j)-fldlo(i,j))*(fcn(i,j)*scal(i,j))/
     &       ((onemu-(flxdp+flydp))*dt2)
            rm(i,j)=(fldlo(i,j)-fmn(i,j))*(fcn(i,j)*scal(i,j))/
     &       ((onemu+(flxdn+flydn))*dt2)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(rp, ip,'ad:16:flp   ')
        call pipe_compare_sym1(rm, ip,'ad:16:fln   ')
      endif
c
c --- rhs: flx, fly, rp+, rm+
c --- lhs: flx, fly
c
      margin = mbdy_a - 4
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            flx(i,j)=max(0.0,flx(i,j))*min(1.0,rp(i,j),rm(i-1,j))
     &              +min(0.0,flx(i,j))*min(1.0,rp(i-1,j),rm(i,j))
          endif !iu
          if (SEA_V) then
            fly(i,j)=max(0.0,fly(i,j))*min(1.0,rp(i,j),rm(i,j-1))
     &              +min(0.0,fly(i,j))*min(1.0,rp(i,j-1),rm(i,j))
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:18:flx   ',
     &                         fly, iv,'ad:18:fly   ')
      endif
c
cdiag i=itest
cdiag j=jtest
cdiag write (lp,'(''advem (2)'',2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.1pe9.2,0pf9.3/1pe39.2/0pf39.3)') i,j,fldlo(i-1,j),u(i,j),fldlo(i,j-1),
cdiag.v(i,j),fldlo(i,j),v(i,j+1),fldlo(i,j+1),u(i+1,j),fldlo(i+1,j)
c
c --- rhs: flx+, fly+, fldlo, fcn, fmx
c --- lhs: flxdiv, fld
c
      margin = mbdy_a - 5
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            fldao(i,j) = fld(i,j)*fco(i,j)*scal(i,j)
c
!...........apply antidiffusive flux correction
            flxdiv(i,j)=((flx(i+1,j)-flx(i,j))+
     &                   (fly(i,j+1)-fly(i,j)) )*dt2*scali(i,j)
            !max,min should only be active for very thin layers
            fld(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                    fldlo(i,j)-flxdiv(i,j)/(fcn(i,j)+onemu) ))
            fld(i,j)=fld(i,j)-posdef
c
            fldan(i,j) = fld(i,j)*fcn(i,j)*scal(i,j)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(flxdiv, ip,'ad:620:flxdv')
        call pipe_compare_sym1(fld,    ip,'ad:1620:fld ')
      endif
      return
      end subroutine advem_mpdata

      subroutine advem_pcm(fld,u,v,fco,fcn,scal,scali,dt2)
      implicit none
c
      real,    intent(in)    :: dt2
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: fld
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: u,v,scal,scali,fco,fcn
c
c Piecewise Constant Method (Donor Cell, Upwind)
c Over two time steps (may require half the normal time step for stability).
c
c time steps are "old", "center" and "new".
c
c  fld   - scalar field, need not be >0, old input but new output
c  u,v   - mass fluxes satisfying continuity equation (old to new)
c  fco   - thickness of the layer at old    time step
c  fcn   - thickness of the layer at new    time step
c  scal  - spatial increments (squared)
c  scali - inverse of scal
c  dt2   - temporal increment (from old to new)
c
c  on return, fld's valid halo will be 0 wide.
c
      real,    parameter :: onemu=9806.e-12 !very small layer thickness
      real,    parameter :: onecm=98.06     !one cm in pressure units
c
      real    q
      integer i,j,l,ia,ib,ja,jb,mbdy_a,margin
c
      integer       itest,jtest,ittest,jttest
      common/testpt/itest,jtest,ittest,jttest
      save  /testpt/
c
c --- rhs: u, v, fld+
c --- lhs: flx, fly
c
      mbdy_a = mbdy_advtyp(0)  ! = 2
c
      margin = mbdy_a - 1
c
!$OMP PARALLEL DO PRIVATE(j,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            if (u(i,j).ge.0.0) then
              q=fld(i-1,j)
            else
              q=fld(i  ,j)
            endif
            flx(i,j)=u(i,j)*q
          endif !iu
          if (SEA_V) then
            if (v(i,j).ge.0.0) then
              q=fld(i,j-1)
            else
              q=fld(i,j  )
            endif
            fly(i,j)=v(i,j)*q
          endif !iv
          if (SEA_P) then
            ia=ipim1(i,j) !i-1 if sea and i if land
            ib=ipip1(i,j) !i+1 if sea and i if land
            ja=ipjm1(i,j) !j-1 if sea and j if land
            jb=ipjp1(i,j) !j+1 if sea and j if land
            fmx(i,j)=max(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))
            fmn(i,j)=min(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:11:flx   ',
     &                         fly, iv,'ad:11:fly   ')
      endif
c
      do j=1-margin,jj+margin
        do l=1,isp(j) !ok
          if     (ifp(j,l).ge. 1-margin) then
            flx(ifp(j,l)  ,j)=0.0
          endif
          if     (ilp(j,l).lt.ii+margin) then
            flx(ilp(j,l)+1,j)=0.0
          endif
        enddo !l
      enddo !j
c
      do i=1-margin,ii+margin
        do l=1,jsp(i) !ok
          if     (jfp(i,l).ge. 1-margin) then
            fly(i,jfp(i,l)  )=0.0
          endif
          if     (jlp(i,l).lt.jj+margin) then
            fly(i,jlp(i,l)+1)=0.0
          endif
        enddo !l
      enddo !i
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:22:flx   ',
     &                         fly, iv,'ad:33:fly   ')
      endif
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag   i=itest
cdiag   j=jtest
cdiag   write (lp,'(a,2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.             1pe9.2,0pf9.3/1pe39.2/0pf39.3)')
cdiag.    'advem (1)',i+i0,j+j0,
cdiag.    fld(i-1,j),u(i,j),fld(i,j-1),v(i,j),
cdiag.    fld(i,j),v(i,j+1),fld(i,j+1),u(i+1,j),fld(i+1,j)
cdiag endif
c
c --- rhs: flx+, fly+, fld, fco, fcn
c --- lhs: flxdiv, fld
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            fldao(i,j) = fld(i,j)*fco(i,j)*scal(i,j)
c
            flxdiv(i,j)=((flx(i+1,j)-flx(i,j))+
     &                   (fly(i,j+1)-fly(i,j)) )*dt2*scali(i,j)
            q=fld(i,j)*(fco(i,j)+onemu)-flxdiv(i,j)
            !max,min should only be active for very thin layers
            fld(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                                   q/(fcn(i,j)+onemu) ))
c
            fldan(i,j) = fld( i,j)*fcn(i,j)*scal(i,j)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(fld,    ip,'ad:610:fld  ')
        call pipe_compare_sym1(flxdiv, ip,'ad:610:flxdv')
      endif
c
cdiag i=itest
cdiag j=jtest
cdiag write (lp,'(''advem (2)'',2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.1pe9.2,0pf9.3/1pe39.2/0pf39.3)') i,j,fld(i-1,j),u(i,j),fld(i,j-1),
cdiag.v(i,j),fld(i,j),v(i,j+1),fld(i,j+1),u(i+1,j),fld(i+1,j)
      return
      end subroutine advem_pcm

      subroutine advem_fct2(fld,fldc,u,v,fco,fcn,scal,scali,dt2)
      implicit none
c
      real,    intent(in)    :: dt2
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: fld
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: fldc,u,v,scal,scali,fco,fcn
c
c Leapfrog 2nd order FCT
c S.T. Zalesak (1979): Fully multidimensional flux-corrected
c transport algorithms for fluids, J.Comput.Phys. 31 335-362.
c
c time steps are "old", "center" and "new".
c
c  fld   - scalar field, need not be >0, old input but new output
c  fldc  - scalar field at center time step
c  u,v   - mass fluxes satisfying continuity equation (old to new)
c  fco   - thickness of the layer at old    time step
c  fcn   - thickness of the layer at new    time step
c  scal  - spatial increments (squared)
c  scali - inverse of scal
c  dt2   - temporal increment (from old to new)
c
c  on return, fld's valid halo will be 0 wide.
c
      real,    parameter :: onemu=9806.e-12 !very small layer thickness
      real,    parameter :: epsil=1.e-20
c
      real    flxdn,flxdp,flydn,flydp,q
      integer i,j,l,ia,ib,ja,jb,mbdy_a,margin
c
      integer       itest,jtest,ittest,jttest
      common/testpt/itest,jtest,ittest,jttest
      save  /testpt/
c
      real :: fhx, fhy, fqmax, fqmin, famax, famin
      real :: qdt2, qp, qm, fact

      mbdy_a = mbdy_advtyp(2)  ! = 5
c
c --- compute low-order and part of antidiffusive fluxes
c
c --- rhs: u, v, fld+
c --- lhs: flx, fly, fmx, fmn
c
      margin = mbdy_a - 1
c
!$OMP PARALLEL DO PRIVATE(j,i,q,jb,ja,ib,ia)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            if (u(i,j).ge.0.0) then
              q=fld(i-1,j)
            else
              q=fld(i  ,j)
            endif
            flx(i,j)=u(i,j)*q
          endif !iu
          if (SEA_V) then
            if (v(i,j).ge.0.0) then
              q=fld(i,j-1)
            else
              q=fld(i,j  )
            endif
            fly(i,j)=v(i,j)*q
          endif !iv
          if (SEA_P) then
            ia=ipim1(i,j) !i-1 if sea and i if land
            ib=ipip1(i,j) !i+1 if sea and i if land
            ja=ipjm1(i,j) !j-1 if sea and j if land
            jb=ipjp1(i,j) !j+1 if sea and j if land
            fmx(i,j)=max(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))
            fmn(i,j)=min(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
!       call pipe_compare_sym2(tx1, iu,'ad:11:tx1   ',
!    &                         ty1, iv,'ad:11:ty1   ')
!       call pipe_compare_sym2(flx, iu,'ad:11:flx   ',
!    &                         fly, iv,'ad:11:fly   ')
      endif
c
      do j=1-margin,jj+margin
        do l=1,isp(j) !ok
          if     (ifp(j,l).ge. 1-margin) then
            flx(ifp(j,l)  ,j)=0.0
          endif
          if     (ilp(j,l).lt.ii+margin) then
            flx(ilp(j,l)+1,j)=0.0
          endif
        enddo !l
      enddo !j
c
      do i=1-margin,ii+margin
        do l=1,jsp(i) !ok
          if     (jfp(i,l).ge. 1-margin) then
            fly(i,jfp(i,l)  )=0.0
          endif
          if     (jlp(i,l).lt.jj+margin) then
            fly(i,jlp(i,l)+1)=0.0
          endif
        enddo !l
      enddo !i
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:22:flx   ',
     &                         fly, iv,'ad:33:fly   ')
      endif
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag   i=itest
cdiag   j=jtest
cdiag   write (lp,'(a,2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.             1pe9.2,0pf9.3/1pe39.2/0pf39.3)')
cdiag.    'advem (1)',i+i0,j+j0,
cdiag.    fld(i-1,j),u(i,j),fld(i,j-1),v(i,j),
cdiag.    fld(i,j),v(i,j+1),fld(i,j+1),u(i+1,j),fld(i+1,j)
cdiag endif
c
c --- rhs: flx+, fly+, fld, fmx, fmn, fco, fcn
c --- lhs: fldlo, fmxlo, fmnlo, flxdiv
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
!...........lo order Donor Cell step
            flxdiv(i,j)=((flx(i+1,j)-flx(i,j))+
     &                   (fly(i,j+1)-fly(i,j)) )*dt2*scali(i,j)
            q=fld(i,j)*(fco(i,j)+onemu)-flxdiv(i,j)
            !max,min should only be active for very thin layers
            fldlo(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                                     q/(fcn(i,j)+onemu) ))
            fmxlo(i,j) = max(fld(i,j),fldc(i,j),fldlo(i,j))
            fmnlo(i,j) = min(fld(i,j),fldc(i,j),fldlo(i,j))
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(fldlo,  ip,'ad:610:fldlo')
        call pipe_compare_sym1(flxdiv, ip,'ad:610:flxdv')
      endif
c
!.....Leapfrog step using high order scheme
c
c --- rhs: u, v, fld+, flx, fly
c --- lhs: fax, fay
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,q,jb,ja,ib,ia,fhx,fhy)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            fhx=u(i,j)*0.5*(fldc(i,j)+fldc(i-1,j))  ! 2nd order in space
            fax(i,j)= fhx-flx(i,j)                  ! anti-diffusion x-flux
          endif !iu
          if (SEA_V) then
            fhy=v(i,j)*0.5*(fldc(i,j)+fldc(i,j-1))  ! 2nd order in space
            fay(i,j)= fhy-fly(i,j)                  ! anti-diffusion y-flux
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO

      do j=1-margin,jj+margin
        do l=1,isp(j) !ok
          if     (ifp(j,l).ge. 1-margin) then
            fax(ifp(j,l)  ,j)=0.0
          endif
          if     (ilp(j,l).lt.ii+margin) then
            fax(ilp(j,l)+1,j)=0.0
          endif
        enddo !l
      enddo !j
c
      do i=1-margin,ii+margin
        do l=1,jsp(i) !ok
          if     (jfp(i,l).ge. 1-margin) then
            fay(i,jfp(i,l)  )=0.0
          endif
          if     (jlp(i,l).lt.jj+margin) then
            fay(i,jlp(i,l)+1)=0.0
          endif
        enddo !l
      enddo !i
!========================================================
c
c --- finish computation of antidiffusive fluxes
c
c --- rhs: fmnlo+,fmxlo+,fax+,fay+,fldlo,fcn,scal
c --- lhs: rp, rm
c
      margin = mbdy_a - 3
c
      qdt2 = 1.0/dt2
!$OMP PARALLEL DO PRIVATE(j,i,ia,ib,ja,jb,
!$OMP&                    fqmax,fqmin,famax,famin,qp,qm)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            ia=ipim1(i,j) !i-1 if sea and i if land
            ib=ipip1(i,j) !i+1 if sea and i if land
            ja=ipjm1(i,j) !j-1 if sea and j if land
            jb=ipjp1(i,j) !j+1 if sea and j if land
            fqmax = max(fmxlo(i,j),fmxlo(ia,j),fmxlo(ib,j),
     &                             fmxlo(i,ja),fmxlo(i,jb))
            fqmin = min(fmnlo(i,j),fmnlo(ia,j),fmnlo(ib,j),
     &                             fmnlo(i,ja),fmnlo(i,jb))
            famax = max(0.0,fax(i, j) ) - min(0.0,fax(ib,j) ) +
     &              max(0.0,fay(i, j) ) - min(0.0,fay(i, jb))
            famin = max(0.0,fax(ib,j) ) - min(0.0,fax(i, j) ) +
     &              max(0.0,fay(i, jb)) - min(0.0,fay(i, j) )
            if (famax > 0.0) then
              qp = (fqmax-fldlo(i,j)) *fcn(i,j)*scal(i,j)*qdt2
              rp(i,j) = min(1.0, qp/famax)
            else
              rp(i,j) = 0.0
            endif
            if (famin > 0.0) then
              qm = (fldlo(i,j)-fqmin) *fcn(i,j)*scal(i,j)*qdt2
              rm(i,j) = min(1.0, qm/famin)
            else
              rm(i,j) = 0.0
            endif
            fmx(i,j) = fqmax  !less restrictive maximum
            fmn(i,j) = fqmin  !less restrictive minimum
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- rhs: rp+, rm+
c --- lhs: fax, fay
c
      margin = mbdy_a - 4
c
!$OMP PARALLEL DO PRIVATE(j,i,fact)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            if (fax(i,j) < 0.0) then
              fact = min(rp(i-1,j),rm(i,j))
            else
              fact = min(rp(i,j),rm(i-1,j))
            endif
            fax(i,j) = fact*fax(i,j)
          endif !iu
          if (SEA_V) then
            if (fay(i,j) < 0.0) then
              fact = min(rp(i,j-1),rm(i,j))
            else
              fact = min(rp(i,j),rm(i,j-1))
            endif
            fay(i,j) = fact*fay(i,j)
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:18:flx   ',
     &                         fly, iv,'ad:18:fly   ')
      endif
c
cdiag i=itest
cdiag j=jtest
cdiag write (lp,'(''advem (2)'',2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.1pe9.2,0pf9.3/1pe39.2/0pf39.3)') i,j,fldlo(i-1,j),u(i,j),fldlo(i,j-1),
cdiag.v(i,j),fldlo(i,j),v(i,j+1),fldlo(i,j+1),u(i+1,j),fldlo(i+1,j)
c
c --- rhs: fax+, fay+, fld, fcn, fmx, fmn
c --- lhs: fld
c
      margin = mbdy_a - 5
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            fldao(i,j) = fld(i,j)*fco(i,j)*scal(i,j)
c
!...........apply antidiffusive flux correction
            flxdiv(i,j)=((fax(i+1,j)-fax(i,j))+
     &                   (fay(i,j+1)-fay(i,j)) )*dt2*scali(i,j)
            !max,min should only be active for very thin layers
            fld(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                    fldlo(i,j)-flxdiv(i,j)/(fcn(i,j)+onemu) ))
c
            fldan(i,j) = fld(i,j)*fcn(i,j)*scal(i,j)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(flxdiv, ip,'ad:620:flxdv')
        call pipe_compare_sym1(fld,    ip,'ad:1620:fld ')
      endif
      return
      end subroutine advem_fct2

      subroutine advem_fct4(fld,fldc,u,v,fco,fcn,scal,scali,dt2)
      implicit none
c
      real,    intent(in)    :: dt2
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(inout) :: fld
      real, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &         intent(in)    :: fldc,u,v,scal,scali,fco,fcn
c
c Leapfrog 4th order FCT
c S.T. Zalesak (1979): Fully multidimensional flux-corrected
c transport algorithms for fluids, J.Comput.Phys. 31 335-362.
c
c time steps are "old", "center" and "new".
c
c  fld   - scalar field, need not be >0, old input but new output
c  fldc  - scalar field at center time step
c  u,v   - mass fluxes satisfying continuity equation (old to new)
c  fco   - thickness of the layer at old    time step
c  fcn   - thickness of the layer at new    time step
c  scal  - spatial increments (squared)
c  scali - inverse of scal
c  dt2   - temporal increment (from old to new)
c
c  on return, fld's valid halo will be 0 wide.
c
      real,    parameter :: onemu=9806.e-12 !very small layer thickness
      real,    parameter :: epsil=1.e-20
c
      real,    parameter :: ft14= 7.0/12.0,  !4th centered inner coeff
     &                      ft24=-1.0/12.0   !4th centered outer coeff
c
      real    flxdn,flxdp,flydn,flydp,q
      integer i,j,l,ia,ib,ja,jb,mbdy_a,margin
c
      integer       itest,jtest,ittest,jttest
      common/testpt/itest,jtest,ittest,jttest
      save  /testpt/
c
      real :: fhx, fhy, fqmax, fqmin, famax, famin
      real :: qdt2, qp, qm, fact

      mbdy_a = mbdy_advtyp(4)  ! = 5
c
c --- compute low-order and part of antidiffusive fluxes
c
c --- rhs: u, v, fld+
c --- lhs: flx, fly, fmx, fmn
c
      margin = mbdy_a - 1
c
!$OMP PARALLEL DO PRIVATE(j,i,q,fhx,fhy,jb,ja,ib,ia)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            if (u(i,j).ge.0.0) then
              q=fld(i-1,j)
            else
              q=fld(i  ,j)
            endif
            flx(i,j)=u(i,j)*q
          endif !iu
          if (SEA_V) then
            if (v(i,j).ge.0.0) then
              q=fld(i,j-1)
            else
              q=fld(i,j  )
            endif
            fly(i,j)=v(i,j)*q
          endif !iv
          if (SEA_P) then
            ia=ipim1(i,j) !i-1 if sea and i if land
            ib=ipip1(i,j) !i+1 if sea and i if land
            ja=ipjm1(i,j) !j-1 if sea and j if land
            jb=ipjp1(i,j) !j+1 if sea and j if land
            fmx(i,j)=max(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))
            fmn(i,j)=min(fld(i,j),fld(ia,j),fld(ib,j),
     &                            fld(i,ja),fld(i,jb))
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
!       call pipe_compare_sym2(tx1, iu,'ad:11:tx1   ',
!    &                         ty1, iv,'ad:11:ty1   ')
!       call pipe_compare_sym2(flx, iu,'ad:11:flx   ',
!    &                         fly, iv,'ad:11:fly   ')
      endif
c
      do j=1-margin,jj+margin
        do l=1,isp(j) !ok
          if     (ifp(j,l).ge. 1-margin) then
            flx(ifp(j,l)  ,j)=0.0
          endif
          if     (ilp(j,l).lt.ii+margin) then
            flx(ilp(j,l)+1,j)=0.0
          endif
        enddo !l
      enddo !j
c
      do i=1-margin,ii+margin
        do l=1,jsp(i) !ok
          if     (jfp(i,l).ge. 1-margin) then
            fly(i,jfp(i,l)  )=0.0
          endif
          if     (jlp(i,l).lt.jj+margin) then
            fly(i,jlp(i,l)+1)=0.0
          endif
        enddo !l
      enddo !i
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:22:flx   ',
     &                         fly, iv,'ad:33:fly   ')
      endif
cdiag if     (itest.gt.0 .and. jtest.gt.0) then
cdiag   i=itest
cdiag   j=jtest
cdiag   write (lp,'(a,2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.             1pe9.2,0pf9.3/1pe39.2/0pf39.3)')
cdiag.    'advem (1)',i+i0,j+j0,
cdiag.    fldc(i-1,j),u(i,j),fldc(i,j-1),v(i,j),
cdiag.    fldc(i,j),v(i,j+1),fldc(i,j+1),u(i+1,j),fldc(i+1,j)
cdiag endif
c
c --- rhs: flx+, fly+, fld, fmx, fmn, fco, fcn, flxdiv
c --- lhs: fldlo, fmxlo, fmnlo, flxdiv
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,q)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
!...........lo order Donor Cell step
            flxdiv(i,j)=((flx(i+1,j)-flx(i,j))+
     &                   (fly(i,j+1)-fly(i,j)) )*dt2*scali(i,j)
            q=fld(i,j)*(fco(i,j)+onemu)-flxdiv(i,j)
            !max,min should only be active for very thin layers
            fldlo(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                                     q/(fcn(i,j)+onemu) ))
            fmxlo(i,j) = max(fld(i,j),fldc(i,j),fldlo(i,j))
            fmnlo(i,j) = min(fld(i,j),fldc(i,j),fldlo(i,j))
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(fldlo,  ip,'ad:610:fldlo')
        call pipe_compare_sym1(flxdiv, ip,'ad:610:flxdv')
      endif
c
!.....Leapfrog step using high order scheme
c
c --- rhs: u, v, fldi++, flx, fly
c --- lhs: fax, fay
c
      margin = mbdy_a - 2
c
!$OMP PARALLEL DO PRIVATE(j,i,q,jb,ja,ib,ia,fhx,fhy)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            if     (iu(i-1,j).eq.0 .or. iu(i+1,j).eq.0) then
              ! 2nd order time centered
              fhx=u(i,j)*0.5*(fldc(i,j)+fldc(i-1,j)) 
            else
              ! 4th order time centered
              fhx=u(i,j)*(ft14*(fldc(i,  j)+fldc(i-1,j))+
     &                    ft24*(fldc(i+1,j)+fldc(i-2,j)) )
            endif
            fax(i,j)= fhx-flx(i,j)  ! anti-diffusion x-flux
          endif !iu
          if (SEA_V) then
            if     (iv(i,j-1).eq.0 .or. iv(i,j+1).eq.0) then
              ! 2nd order time centered
              fhy=v(i,j)*0.5*(fldc(i,j)+fldc(i,j-1))
            else
              ! 4th order time centered
              fhy=v(i,j)*(ft14*(fldc(i,j)  +fldc(i,j-1))+
     &                    ft24*(fldc(i,j+1)+fldc(i,j-2)) )
            endif
            fay(i,j)= fhy-fly(i,j)  ! anti-diffusion y-flux
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO

      do j=1-margin,jj+margin
        do l=1,isp(j) !ok
          if     (ifp(j,l).ge. 1-margin) then
            fax(ifp(j,l)  ,j)=0.0
          endif
          if     (ilp(j,l).lt.ii+margin) then
            fax(ilp(j,l)+1,j)=0.0
          endif
        enddo !l
      enddo !j
c
      do i=1-margin,ii+margin
        do l=1,jsp(i) !ok
          if     (jfp(i,l).ge. 1-margin) then
            fay(i,jfp(i,l)  )=0.0
          endif
          if     (jlp(i,l).lt.jj+margin) then
            fay(i,jlp(i,l)+1)=0.0
          endif
        enddo !l
      enddo !i
!========================================================
c
c --- finish computation of antidiffusive fluxes
c
c --- rhs: fmnlo+,fmxlo+,fax+,fay+,fldlo,fcn,scal
c --- lhs: rp, rm
c
      margin = mbdy_a - 3
c
      qdt2 = 1.0/dt2
!$OMP PARALLEL DO PRIVATE(j,i,ia,ib,ja,jb,
!$OMP&                    fqmax,fqmin,famax,famin,qp,qm)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            ia=ipim1(i,j) !i-1 if sea and i if land
            ib=ipip1(i,j) !i+1 if sea and i if land
            ja=ipjm1(i,j) !j-1 if sea and j if land
            jb=ipjp1(i,j) !j+1 if sea and j if land
            fqmax = max(fmxlo(i,j),fmxlo(ia,j),fmxlo(ib,j),
     &                             fmxlo(i,ja),fmxlo(i,jb))
            fqmin = min(fmnlo(i,j),fmnlo(ia,j),fmnlo(ib,j),
     &                             fmnlo(i,ja),fmnlo(i,jb))
            famax = max(0.0,fax(i, j) ) - min(0.0,fax(ib,j) ) +
     &              max(0.0,fay(i, j) ) - min(0.0,fay(i, jb))
            famin = max(0.0,fax(ib,j) ) - min(0.0,fax(i, j) ) +
     &              max(0.0,fay(i, jb)) - min(0.0,fay(i, j) )
            if (famax > 0.0) then
              qp = (fqmax-fldlo(i,j)) *fcn(i,j)*scal(i,j)*qdt2
              rp(i,j) = min(1.0, qp/famax)
            else
              rp(i,j) = 0.0
            endif
            if (famin > 0.0) then
              qm = (fldlo(i,j)-fqmin) *fcn(i,j)*scal(i,j)*qdt2
              rm(i,j) = min(1.0, qm/famin)
            else
              rm(i,j) = 0.0
            endif
            fmx(i,j) = fqmax  !less restrictive maximum
            fmn(i,j) = fqmin  !less restrictive minimum
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
c --- rhs: rp+, rm+
c --- lhs: fax, fay
c
      margin = mbdy_a - 4
c
!$OMP PARALLEL DO PRIVATE(j,i,fact)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_U) then
            if (fax(i,j) < 0.0) then
              fact = min(rp(i-1,j),rm(i,j))
            else
              fact = min(rp(i,j),rm(i-1,j))
            endif
            fax(i,j) = fact*fax(i,j)
          endif !iu
          if (SEA_V) then
            if (fay(i,j) < 0.0) then
              fact = min(rp(i,j-1),rm(i,j))
            else
              fact = min(rp(i,j),rm(i,j-1))
            endif
            fay(i,j) = fact*fay(i,j)
          endif !iv
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym2(flx, iu,'ad:18:flx   ',
     &                         fly, iv,'ad:18:fly   ')
      endif
c
cdiag i=itest
cdiag j=jtest
cdiag write (lp,'(''advem (2)'',2i5,f22.3/1pe39.2/0pf21.3,1pe9.2,0pf9.3,
cdiag.1pe9.2,0pf9.3/1pe39.2/0pf39.3)') i,j,fldlo(i-1,j),u(i,j),fldlo(i,j-1),
cdiag.v(i,j),fldlo(i,j),v(i,j+1),fldlo(i,j+1),u(i+1,j),fldlo(i+1,j)
c
c --- rhs: fax+, fay+, fld, fcn, fmx, fmn
c --- lhs: fld
c
      margin = mbdy_a - 5
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
            fldao(i,j) = fld(i,j)*fco(i,j)*scal(i,j)
c
!...........apply antidiffusive flux correction
            flxdiv(i,j)=((fax(i+1,j)-fax(i,j))+
     &                   (fay(i,j+1)-fay(i,j)) )*dt2*scali(i,j)
            !max,min should only be active for very thin layers
            fld(i,j)=max( fmn(i,j), min( fmx(i,j),
     &                    fldlo(i,j)-flxdiv(i,j)/(fcn(i,j)+onemu) ))
c
            fldan(i,j) = fld( i,j)*fcn(i,j)*scal(i,j)
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
      if     (lpipe .and. lpipe_advem) then
c ---   compare two model runs.
        call pipe_compare_sym1(flxdiv, ip,'ad:620:flxdv')
        call pipe_compare_sym1(fld,    ip,'ad:1620:fld ')
      endif
      return
      end subroutine advem_fct4

      subroutine tsadvc(m,n)
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer m,n
c
c --- -----------------------------------------------------
c --- thermodynamic variable(s): advection and diffusion.
c --- -----------------------------------------------------
c --- on entry:
c --- saln(:,:,:,n) = time step t-1
c --- saln(:,:,:,m) = time step t
c
c ---  dpo(:,:,:,n) = time step t-1
c ---  dpo(:,:,:,m) = time step t
c ---  dp( :,:,:,m) = time step t   with RA time smoothing
c ---  dp( :,:,:,n) = time step t+1
c
c --- on exit:
c --- saln(:,:,:,m) = time step t   with RA time smoothing
c --- saln(:,:,:,n) = time step t+1
c --- -----------------------------------------------------
c
      logical, parameter :: lpipe_tsadvc=.false.  !extra checking (when pipe on)
c
      real,    parameter :: onemu=9806.e-12 !pico-m, very small layer thickness
      real,    parameter :: onezm=9806.e-20 !zepto-m, insignificant thickness
c
#if defined(RELO)
      real, save, allocatable, dimension (:,:) ::
     & sold,told,q2old,q2lold
      real, save, allocatable, dimension (:,:,:) ::
     & trold
#else
      real, save, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     & sold,told,q2old,q2lold
      real, save, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,mxtrcr) ::
     & trold
#endif
c
      logical latemp,lath3d,ldtemp,ldth3d
      integer i,isave,j,jsave,k,ktr,l,mbdy,mdf,margin
      real sminn,smaxx,flxdiv,th3d_t,
     &     factor,q,dpsold,dpsmid,dpsnew,
     &     dpold,dpmid,dpnew,qdpmidn,smin,ufa,ufb,vfa,vfb
      real xmin(kdm),xmax(kdm)
      real sminny(jdm),smaxxy(jdm)
c
      character*12 text,textu,textv
c
c --- for mpdata, select posdef:
c ---   1. as a power of 2
c ---   2. so that the ratio of the standard deviation to the mean of
c ---      each field is approximately the same:
c ---        0 for -saln-,   256 for -temp-, 32 for -th3d-,
c ---        0 for -tracer-,   1 for -q2-
      real       pdzero,pdtemp,pdth3d,pdq2
      parameter (pdzero=0.0, pdtemp=256.0, pdth3d=32.0, pdq2=1.0)
c
      real harmon,a,b
      include 'stmt_fns.h'
c
c --- harmonic mean
      harmon(a,b)=2.*a*b/(a+b)
#if defined(RELO)
c
      if     (.not.allocated(sold)) then
        allocate(
     &           sold(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           told(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
        call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
                 sold = r_init
                 told = r_init
        if (mxlmy) then
          allocate(
     &            q2old(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           q2lold(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
          call mem_stat_add( 2*(idm+2*nbdy)*(jdm+2*nbdy) )
                  q2old = r_init
                 q2lold = r_init
        endif !mxlmy
        if     (ntracr.gt.0) then
          allocate(
     &           trold(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ntracr) )
          call mem_stat_add( (idm+2*nbdy)*(jdm+2*nbdy)*ntracr )
                 trold = r_init
        endif !ntracr
      endif !.not.allocated
#endif
c
      mbdy = mbdy_advtyp(abs(advtyp))  ! 2-8 depending on advection scheme
c
      if     (nbdy.lt.mbdy) then
        if     (mnproc.eq.1) then
          write(lp,'(/ a,i3,a /)')
     &      'error: nbdy (dimensions.h) must be at least',
     &      mbdy,' for the advection scheme indicated by advtyp'
        endif
        call xcstop('tsadvc')
               stop 'tsadvc'
      endif
c
      l = mbdy
c --- dp halo is up to date
      call xctilr(saln(1-nbdy,1-nbdy,1,1),1,2*kk, l,l, halo_ps)
      call xctilr(temp(1-nbdy,1-nbdy,1,1),1,2*kk, l,l, halo_ps)
      call xctilr(th3d(1-nbdy,1-nbdy,1,1),1,2*kk, l,l, halo_ps)
      call xctilr(uflx(1-nbdy,1-nbdy,1  ),1,  kk, l,l, halo_uv)
      call xctilr(vflx(1-nbdy,1-nbdy,1  ),1,  kk, l,l, halo_vv)
      do ktr= 1,ntracr
        call xctilr(tracer(1-nbdy,1-nbdy,1,1,ktr),1,2*kk, l,l, halo_ps)
      enddo !ktr
      if (mxlmy) then
        call xctilr(q2( 1-nbdy,1-nbdy,0,1),1,2*kk+4, l,l, halo_ps)
        call xctilr(q2l(1-nbdy,1-nbdy,0,1),1,2*kk+4, l,l, halo_ps)
      endif
c
      do 81 k=1,kk
c
c --- ---------------------------------------------------
c --- advection of thermodynamic variable(s) (and tracer)
c --- ---------------------------------------------------
c
c --- for isopycnic vertical coordinates:
c ---   advect -th3d- and -saln- in the mixed layer (layer 1),
c ---   advect            -saln- only in all other layers
c --- for hybrid vertical coordinates:
c ---   advect -temp- and -saln- in all layers if advflg==0,
c ---   advect -th3d- and -saln- in all layers if advflg==1
c
      latemp =  k.le.nhybrd .and. advflg.eq.0       ! advect temp
      lath3d = (k.le.nhybrd .and. advflg.eq.1) .or.
     &         (k.eq.1      .and. isopyc     )      ! advect th3d
c
c --- smooth mixed-layer mass fluxes in lateral direction
      if(isopyc .and. k.eq.1) then
c
c ---   rhs: vflx+, uflx+
c ---   lhs: vflux, uflux
c
        margin = mbdy - 1
c
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_V) then
              if (iv(i-1,j).ne.0) then
                vfa=vflx(i-1,j,1)
              else 
                vfa=vflx(i,  j,1)
              endif
              if (iv(i+1,j).ne.0) then
                vfb=vflx(i+1,j,1)
              else
                vfb=vflx(i,  j,1)
              endif
              vflux(i,j)=.5*vflx(i,j,1)+.25*(vfa+vfb)
            endif !iv
            if (SEA_U) then
              if (iu(i,j-1).ne.0) then
                ufa=uflx(i,j-1,1)
              else
                ufa=uflx(i,j,  1)
              endif
              if (iu(i,j+1).ne.0) then
                ufb=uflx(i,j+1,1)
              else
                ufb=uflx(i,j,  1)
              endif
              uflux(i,j)=.5*uflx(i,j,1)+.25*(ufa+ufb)
            endif !iu
          enddo !i
        enddo !j
      endif
c
c ---   rhs: temp, saln, uflux+, vflux+, dp
c ---   lhs: told, sold, util1, util2, util3, temp, th3d
c
c ---   util1 = fco = thickness of the layer at old    time step
c ---   util2 = fcn = thickness of the layer at new    time step
c
        margin = mbdy - 1  ! util[12] at mbdy-2
c
!$OMP PARALLEL DO PRIVATE(j,i,ktr,flxdiv)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1-margin,jj+margin
        do i=1-margin,ii+margin
          if (SEA_P) then
c
c ---       save for time smoothing
            if     (latemp) then
              told(i,j)=temp(i,j,k,n)
            elseif (lath3d) then
              told(i,j)=th3d(i,j,k,n)
            endif
            sold(i,j)=saln(i,j,k,n)
            do ktr= 1,ntracr
              trold(i,j,ktr)=tracer(i,j,k,n,ktr)
            enddo
            if (mxlmy) then
              q2old( i,j)=q2( i,j,k,n)
              q2lold(i,j)=q2l(i,j,k,n)
            endif
c
c --- before calling 'advem', make sure (a) mass fluxes are consistent
c --- with layer thickness change, and (b) all fields are positive-definite
            if(isopyc .and. k.eq.1) then
              flxdiv=((uflux(i+1,j)  -uflux(i,j)  )
     &               +(vflux(i,j+1)  -vflux(i,j)  ))*delt1*scp2i(i,j)
            else
              flxdiv=((uflx( i+1,j,k)-uflx( i,j,k))
     &               +(vflx( i,j+1,k)-vflx( i,j,k)))*delt1*scp2i(i,j)
            endif
            util1(i,j)=max(dp(i,j,k,n)+flxdiv,0.0)  !old
            util2(i,j)=max(dp(i,j,k,n),       0.0)  !new
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO
c
      if     (lpipe .and. lpipe_tsadvc) then
c ---   compare two model runs.
        write(text,'(a10,i2)') '49:sold,k=',k
        call pipe_compare_sym1(sold,ip,text)
        write(text,'(a10,i2)') '49:told,k=',k
        call pipe_compare_sym1(told,ip,text)
        write(text,'(a10,i2)') '49:utl1,k=',k
        call pipe_compare_sym1(util1,ip,text)
        write(text,'(a10,i2)') '49:utl2,k=',k
        call pipe_compare_sym1(util2,ip,text)
        write (textu,'(a9,i3)') 'uflx   k=',k
        write (textv,'(a9,i3)') 'vflx   k=',k
        call pipe_compare_sym2(uflx(1-nbdy,1-nbdy,k),  iu,textu,
     &                         vflx(1-nbdy,1-nbdy,k),  iv,textv)
        write (text,'(a9,i3)') 'temp.n k=',k
        call pipe_compare_sym1(temp(1-nbdy,1-nbdy,k,n),ip,text)
        write (text,'(a9,i3)') 'saln.n k=',k
        call pipe_compare_sym1(saln(1-nbdy,1-nbdy,k,n),ip,text)
        write (text,'(a9,i3)') 'th3d.n k=',k
        call pipe_compare_sym1(th3d(1-nbdy,1-nbdy,k,n),ip,text)
      endif
c
c --- rhs: temp.[mn], th3d.[mn], saln.[mn], uflx, vflx, util[12]
c --- lhs: temp.n, th3d.n, saln.n
c
      if     (latemp) then
        call advem(advtyp,temp( 1-nbdy,1-nbdy,k,n),
     &                    temp( 1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdtemp, scp2,scp2i,delt1)
        call advem(advtyp,saln( 1-nbdy,1-nbdy,k,n),
     &                    saln( 1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdzero, scp2,scp2i,delt1)
      elseif (lath3d .and. hybrid) then
        call advem(advtyp,th3d( 1-nbdy,1-nbdy,k,n),
     &                    th3d( 1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdth3d, scp2,scp2i,delt1)
        call advem(advtyp,saln( 1-nbdy,1-nbdy,k,n),
     &                    saln( 1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdzero, scp2,scp2i,delt1)
      elseif (lath3d .and. isopyc) then  ! MICOM-like upper layer
        call advem(advtyp,th3d( 1-nbdy,1-nbdy,k,n),
     &                    th3d( 1-nbdy,1-nbdy,k,m),
     &                    uflux,
     &                    vflux,
     &                    util1,util2,
     &                    pdth3d, scp2,scp2i,delt1)
        call advem(advtyp,saln( 1-nbdy,1-nbdy,k,n),
     &                    saln( 1-nbdy,1-nbdy,k,m),
     &                    uflux,
     &                    vflux,
     &                    util1,util2,
     &                    pdzero, scp2,scp2i,delt1)
      else   ! exactly isopycnal layer
        call advem(advtyp,saln( 1-nbdy,1-nbdy,k,n),
     &                    saln( 1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdzero, scp2,scp2i,delt1)
      endif
      do ktr= 1,ntracr
        if     (trcflg(ktr).eq.2) then !temperature tracer
          call advem(advtyp,tracer(1-nbdy,1-nbdy,k,n,ktr),
     &                      tracer(1-nbdy,1-nbdy,k,m,ktr),
     &                      uflx(  1-nbdy,1-nbdy,k),
     &                      vflx(  1-nbdy,1-nbdy,k),
     &                      util1,util2,
     &                      pdtemp, scp2,scp2i,delt1)
        else
          call advem(advtyp,tracer(1-nbdy,1-nbdy,k,n,ktr),
     &                      tracer(1-nbdy,1-nbdy,k,m,ktr),
     &                      uflx(  1-nbdy,1-nbdy,k),
     &                      vflx(  1-nbdy,1-nbdy,k),
     &                      util1,util2,
     &                      pdzero, scp2,scp2i,delt1)
        endif !trcflg
      enddo !ktr
      if (mxlmy) then
        call advem(advtyp,q2(   1-nbdy,1-nbdy,k,n),
     &                    q2(   1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdq2,   scp2,scp2i,delt1)
        call advem(advtyp,q2l(  1-nbdy,1-nbdy,k,n),
     &                    q2l(  1-nbdy,1-nbdy,k,m),
     &                    uflx( 1-nbdy,1-nbdy,k),
     &                    vflx( 1-nbdy,1-nbdy,k),
     &                    util1,util2,
     &                    pdq2,   scp2,scp2i,delt1)
      endif
c
      if     (lpipe .and. lpipe_tsadvc) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'temp.n k=',k
        call pipe_compare_sym1(temp(1-nbdy,1-nbdy,k,n),ip,text)
        write (text,'(a9,i3)') 'saln.n k=',k
        call pipe_compare_sym1(saln(1-nbdy,1-nbdy,k,n),ip,text)
        write (text,'(a9,i3)') 'th3d.n k=',k
        call pipe_compare_sym1(th3d(1-nbdy,1-nbdy,k,n),ip,text)
      endif
c
c --- Robert-Asselin time filter of scalar fields
c --- Note that this is smoothing dp * scalar,
c ---  but the filter is not conservative across 3 time levels.
c
c --- rhs: temp.n, th3d.n, saln.n, dpo, dp.m, dp.n, sold, told
c --- lhs: temp.n, th3d.n, dp.m, saln.m, temp.m, th3d.m
c
      margin = 0  !after advem
c
!$OMP PARALLEL DO PRIVATE(j,i,ktr,dpsold,dpsmid,dpsnew,q,    !NOCSD
!$OMP&                    dpold,dpmid,dpnew,qdpmidn,smin)    !NOCSD
!$OMP&         SCHEDULE(STATIC,jblk) !NOCSD
      do j=1-margin,jj+margin
        sminny(j)= 999.  !simplifies OpenMP parallelization
        smaxxy(j)=-999.  !simplifies OpenMP parallelization
!DIR$   PREFERVECTOR
        do i=1-margin,ii+margin
          if (SEA_P) then
            if     (dp(i,j,k,n).gt.onemm) then
              sminny(j)=min(sminny(j),saln(i,j,k,n))
              smaxxy(j)=max(smaxxy(j),saln(i,j,k,n))
            endif
c
            if     (dp( i,j,k,m).gt.onezm) then !effectively non-zero
            dpold   =     dpo(i,j,k,n)
            dpmid   =     dpo(i,j,k,m)
            dpnew   =     dp( i,j,k,n)
            qdpmidn = 1.0/dp( i,j,k,m)
c
            dpsold = dpold*sold(i,j)
            dpsmid = dpmid*saln(i,j,k,m)
            dpsnew = dpnew*saln(i,j,k,n)
            q      = 0.5*ra2fac*(dpsold+dpsnew-2.0*dpsmid)
            saln(i,j,k,m)= (dpsmid + q) * qdpmidn
            if     (latemp) then
              dpsold = dpold*told(i,j)
              dpsmid = dpmid*temp(i,j,k,m)
              dpsnew = dpnew*temp(i,j,k,n)
              q      = 0.5*ra2fac*(dpsold+dpsnew-2.0*dpsmid)
              temp(i,j,k,m) = (dpsmid + q) * qdpmidn
c ---         update dependent thermodynamic variable
              th3d(i,j,k,m) = sig(temp(i,j,k,m),saln(i,j,k,m))-thbase
            elseif (lath3d) then
              dpsold = dpold*told(i,j)
              dpsmid = dpmid*th3d(i,j,k,m)
              dpsnew = dpnew*th3d(i,j,k,n)
              q      = 0.5*ra2fac*(dpsold+dpsnew-2.0*dpsmid)
              th3d(i,j,k,m) = (dpsmid + q) * qdpmidn
c ---         update dependent thermodynamic variable
              temp(i,j,k,m) = tofsig(th3d(i,j,k,m)+thbase,
     &                               saln(i,j,k,m))
            else   ! exactly isopycnal layer
              th3d(i,j,k,m) = theta(i,j,k)
c ---         update dependent thermodynamic variable
              temp(i,j,k,m) = tofsig(th3d(i,j,k,m)+thbase,
     &                               saln(i,j,k,m))
            endif
            do ktr= 1,ntracr
c ---         non-negative version that exactly conserves constant tracers
              smin   = min(    trold(i,j,    ktr),
     &                        tracer(i,j,k,m,ktr),
     &                        tracer(i,j,k,n,ktr) )
              dpsold = dpold*( trold(i,j,    ktr) - smin)  !>=0
              dpsmid = dpmid*(tracer(i,j,k,m,ktr) - smin)  !>=0
              dpsnew = dpnew*(tracer(i,j,k,n,ktr) - smin)  !>=0
              q      = 0.5*ra2fac*(dpsold+dpsnew-2.0*dpsmid)
cdiag           if (i.eq.itest.and.j.eq.jtest.and.ktr.eq.1) then
cdiag               write(lp,'(a,i3,1p3g15.6)')
cdiag.                'RA dpo:',k,dpold*qonem,dpmid*qonem,dpnew*qonem
cdiag               write(lp,'(a,i3,3f15.6)')
cdiag.                'RA tro:',k, trold(i,j,    ktr),
cdiag.                            tracer(i,j,k,m,ktr),
cdiag.                            tracer(i,j,k,n,ktr)
cdiag               write(lp,'(a,i3,1p2g15.6)')
cdiag.                'RA dpm:',k,dpmid*qonem,
cdiag.                            dp(i,j,k,m)*qonem
cdiag               write(lp,'(a,i3,2f15.6)')
cdiag.                'RA trm:',k,tracer(i,j,k,m,ktr),
cdiag.                            smin + (dpsmid + q) * qdpmidn
cdiag               call flush(lp)
cdiag           endif !debug
              tracer(i,j,k,m,ktr) = smin + (dpsmid + q) * qdpmidn
            enddo !ktr
            if (mxlmy) then
              dpsold = dpold*q2old(i,j)
              dpsmid = dpmid*   q2(i,j,k,m)
              dpsnew = dpnew*   q2(i,j,k,n)
              q      = 0.5*ra2fac*(dpsold+dpsnew-2.0*dpsmid)
              q2(i,j,k,m) = (dpsmid + q) * qdpmidn
              dpsold = dpold*q2lold(i,j)
              dpsmid = dpmid*   q2l(i,j,k,m)
              dpsnew = dpnew*   q2l(i,j,k,n)
              q      = 0.5*ra2fac*(dpsold+dpsnew-2.0*dpsmid)
              q2l(i,j,k,m) = (dpsmid + q) * qdpmidn
            endif !mxlmy
            endif !effectively non-zero 
          endif !ip
        enddo !i
      enddo !j
!$OMP END PARALLEL DO !NOCSD
c
      xmin(k) = minval(sminny(1:jj))
      xmax(k) = maxval(smaxxy(1:jj))
c
      if     (lpipe .and. lpipe_tsadvc) then
c ---   compare two model runs.
        write (text,'(a9,i3)') 'temp.m k=',k
        call pipe_compare_sym1(temp(1-nbdy,1-nbdy,k,m),ip,text)
        write (text,'(a9,i3)') 'temp.n k=',k
        call pipe_compare_sym1(temp(1-nbdy,1-nbdy,k,n),ip,text)
        write (text,'(a9,i3)') 'sold   k=',k
        call pipe_compare_sym1(sold,ip,text)
        write (text,'(a9,i3)') 'saln.m k=',k
        call pipe_compare_sym1(saln(1-nbdy,1-nbdy,k,m),ip,text)
        write (text,'(a9,i3)') 'saln.n k=',k
        call pipe_compare_sym1(saln(1-nbdy,1-nbdy,k,n),ip,text)
        write (text,'(a9,i3)') 'told   k=',k
        call pipe_compare_sym1(told,ip,text)
        write (text,'(a9,i3)') 'th3d.m k=',k
        call pipe_compare_sym1(th3d(1-nbdy,1-nbdy,k,m),ip,text)
        write (text,'(a9,i3)') 'th3d.n k=',k
        call pipe_compare_sym1(th3d(1-nbdy,1-nbdy,k,n),ip,text)
      endif
c
cdiag if (itest.gt.0.0and.jtest.gt.0)
cdiag.write (lp,'(i9,2i5,i3,'' th,s,dp after advection  '',2f9.3,f8.2)')
cdiag.nstep,itest,jtest,k,temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag.dp(itest,jtest,k,n)*qonem
c
 81   continue  ! k=1,kk
c
      call pipe_comparall(m,n, 'advem,  step')
c
c --- check for negative scalar fields.
c
      if     (mod(nstep,3).eq.0 .or. diagno) then
        call xcminr(xmin(1:kk))
        call xcmaxr(xmax(1:kk))
c
        do k= 1,kk
          sminn=xmin(k)
          smaxx=xmax(k)
c
          if (sminn.lt.0.0) then
            do j=1,jj
              do i=1,ii
                if (SEA_P) then
                  if (saln(i,j,k,n).eq.sminn) then
                    write (lp,'(i9,a,2i5,i3,a,f10.2)') 
     &                nstep,' i,j,k =',i+i0,j+j0,k,
     &                ' neg. saln after advem call ',
     &                saln(i,j,k,n)
                  endif !sminn
                endif !ip
              enddo !i
            enddo !j
            call xcsync(flush_lp)
          endif
c
          if (diagno) then
            if     (mnproc.eq.1) then
            write (lp,'(i9,i3, a,2f9.3, a,1pe9.2,a)')
     &        nstep,k,
     &        ' min/max of s after advection:',sminn,smaxx,
     &        '   (range:',smaxx-sminn,')'
            call flush(lp)
            endif
          endif
        enddo !k
      endif !every 3 time steps or diagno
c
c --- --------------------------------------
c --- diffusion of thermodynamic variable(s)
c --- --------------------------------------
c
      if     (temdf2.gt.0.0) then
        mdf = 2  !Laplacian
        call xctilr(saln(   1-nbdy,1-nbdy,1,n),1,kk, mdf,mdf, halo_ps)
        call xctilr(temp(   1-nbdy,1-nbdy,1,n),1,kk, mdf,mdf, halo_ps)
        call xctilr(th3d(   1-nbdy,1-nbdy,1,n),1,kk, mdf,mdf, halo_ps)
        if (mxlmy) then
          call xctilr(q2( 1-nbdy,1-nbdy,0,n),1,kk+2, mdf,mdf, halo_ps)
          call xctilr(q2l(1-nbdy,1-nbdy,0,n),1,kk+2, mdf,mdf, halo_ps)
        endif
        do ktr= 1,ntracr
          call xctilr(tracer(1-nbdy,1-nbdy,1,n,ktr),
     &                                         1,kk, mdf,mdf, halo_ps)
        enddo !ktr
c
        do k=1,kk
c
c ---     for isopycnic vertical coordinates:
c ---       diffuse -th3d- and -saln- in the mixed layer (layer 1),
c ---       diffuse            -saln- only in all other layers
c ---     for hybrid vertical coordinates:
c ---       diffuse -saln- in all layers
c ---       diffuse -temp- in all layers if temdfc < 0.0
c ---       diffuse -th3d- in all layers if temdfc < 1.0
c ---       if 0.0 < temdfc < 1.0:
c ---         combine -temp- and -th3d- diffusion in density space
c
          ldtemp =  k.le.nhybrd .and. temdfc.gt.0.0     ! diffus temp
          ldth3d = (k.le.nhybrd .and. temdfc.lt.1.0) .or.
     &             (k.eq.1      .and. isopyc     )      ! diffus th3d
          if     (ldtemp .and. ldth3d) then ! diffus temp and th3d
            call tsdff_2x(th3d(1-nbdy,1-nbdy,k,n),
     &                    temp(1-nbdy,1-nbdy,k,n))
            call tsdff_1x(saln(1-nbdy,1-nbdy,k,n))
          elseif (ldtemp) then ! diffus temp
            call tsdff_2x(temp(1-nbdy,1-nbdy,k,n),
     &                    saln(1-nbdy,1-nbdy,k,n))
          elseif (ldth3d) then ! diffus th3d
            call tsdff_2x(th3d(1-nbdy,1-nbdy,k,n),
     &                    saln(1-nbdy,1-nbdy,k,n))
          else   ! exactly isopycnal layer
            call tsdff_1x(saln(1-nbdy,1-nbdy,k,n))
          endif
          if (mxlmy) then
            call tsdff_2x(q2(  1-nbdy,1-nbdy,k,n),
     &                    q2l( 1-nbdy,1-nbdy,k,n))
          endif !mxlmy
          do ktr= 1,ntracr,2
            if     (ktr+1.le.ntracr) then
              call tsdff_2x(tracer(1-nbdy,1-nbdy,k,n,ktr),
     &                      tracer(1-nbdy,1-nbdy,k,n,ktr+1))
            else
              call tsdff_1x(tracer(1-nbdy,1-nbdy,k,n,ktr))
            endif
          enddo !ktr
        enddo !k
c
c       non-independent thermodynamic variable
c
        margin = 0
!$OMP   PARALLEL DO PRIVATE(j,k,i,ldtemp,ldth3d,th3d_t)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do k=1,kk
            ldtemp =  k.le.nhybrd .and. temdfc.gt.0.0     ! diffus temp
            ldth3d = (k.le.nhybrd .and. temdfc.lt.1.0) .or.
     &               (k.eq.1      .and. isopyc     )      ! diffus th3d
            do i=1-margin,ii+margin
              if (SEA_P) then
                if     (ldtemp .and. ldth3d) then
c ---             combine -temp- and -th3d- diffusion in density space
                  th3d_t       =sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
                  th3d(i,j,k,n)=(1.0-temdfc)*th3d(i,j,k,n) +
     &                               temdfc *th3d_t
                  temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                                 saln(i,j,k,n))
                elseif (ldtemp) then
                  th3d(i,j,k,n)=sig(temp(i,j,k,n),saln(i,j,k,n))-thbase
                elseif (ldth3d) then
                  temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                                 saln(i,j,k,n))
                else   ! exactly isopycnal layer
                  th3d(i,j,k,n)=theta(i,j,k)
                  temp(i,j,k,n)=tofsig(th3d(i,j,k,n)+thbase,
     &                                 saln(i,j,k,n))
                endif !ld....
              endif !ip
            enddo !i
          enddo !k
        enddo !j
!$OMP   END PARALLEL DO
      endif !temdf2.gt.0.0
c
      do k=1,kk
        if     (lpipe .and. lpipe_tsadvc) then
c ---     compare two model runs.
          write (text,'(a9,i3)') 'util1  k=',k
          call pipe_compare_sym1(util1,ip,text)
          write (text,'(a9,i3)') 'util2  k=',k
          call pipe_compare_sym1(util2,ip,text)
          write (text,'(a9,i3)') 'temp.n k=',k
          call pipe_compare_sym1(temp(1-nbdy,1-nbdy,k,n),ip,text)
          write (text,'(a9,i3)') 'saln.n k=',k
          call pipe_compare_sym1(saln(1-nbdy,1-nbdy,k,n),ip,text)
          write (text,'(a9,i3)') 'th3d.n k=',k
          call pipe_compare_sym1(th3d(1-nbdy,1-nbdy,k,n),ip,text)
        endif
c
cdiag   if (itest.gt.0.and.jtest.gt.0) then
cdiag&    write (lp,'(i9,2i5,i3,a,2f9.3,f8.2)')
cdiag&      nstep,itest+i0,jtest+j0,k,
cdiag&      ' t,s,dp after isopyc.mix.',
cdiag&      temp(itest,jtest,k,n),saln(itest,jtest,k,n),
cdiag&      dp(itest,jtest,k,n)*qonem
cdiag&    call flush(lp)
cdiag&  endif
c
      enddo !k

      return
 
      contains
c
      subroutine tsdff_2x(fld1,fld2)
      real fld1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &     fld2(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c --- Laplacian diffusion for two scalar fields
c
        margin = 1
c
!$OMP   PARALLEL DO PRIVATE(j,i,factor)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_U) then
              factor=temdf2*aspux(i,j)*
     &               scuy(i,j)*harmon(max(dp(i-1,j,k,n),onemu)
     &                               ,max(dp(i  ,j,k,n),onemu))
              uflux (i,j)=factor*(fld1(i-1,j)-fld1(i,j))
              uflux2(i,j)=factor*(fld2(i-1,j)-fld2(i,j))
            endif !iu
            if (SEA_V) then
              factor=temdf2*aspvy(i,j)*
     &               scvx(i,j)*harmon(max(dp(i,j-1,k,n),onemu)
     &                               ,max(dp(i,j  ,k,n),onemu))
              vflux (i,j)=factor*(fld1(i,j-1)-fld1(i,j))
              vflux2(i,j)=factor*(fld2(i,j-1)-fld2(i,j))
            endif !iv
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        if     (lpipe .and. lpipe_tsadvc) then
c ---     compare two model runs.
          write (textu,'(a9,i3)') 'uflux  k=',k
          write (textv,'(a9,i3)') 'vflux  k=',k
          call pipe_compare_sym2(uflux, iu,textu,
     &                           vflux, iv,textv)
          write (textu,'(a9,i3)') 'uflux2 k=',k
          write (textv,'(a9,i3)') 'vflux2 k=',k
          call pipe_compare_sym2(uflux2,iu,textu,
     &                           vflux2,iv,textv)
        endif
c
c ---   rhs: dp.n, uflux+, vflux+, uflux2+, vflux2+
c ---   lhs: saln.n, temp.n, th3d.n
c
        margin = 0
c
!$OMP   PARALLEL DO PRIVATE(j,i,factor)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_P) then
              factor=-delt1/(scp2(i,j)*max(dp(i,j,k,n),onemu))
              util1(i,j)=((uflux (i+1,j)-uflux (i,j))
     &                   +(vflux (i,j+1)-vflux (i,j)))*factor
              fld1(i,j)=fld1(i,j)+util1(i,j)
              util2(i,j)=((uflux2(i+1,j)-uflux2(i,j))
     &                   +(vflux2(i,j+1)-vflux2(i,j)))*factor
              fld2(i,j)=fld2(i,j)+util2(i,j)
c
cdiag         if (i.eq.itest.and.j.eq.jtest) then
cdiag           if (1.le.i .and. i.le.ii .and.
cdiag.              1.le.j .and. j.le.jj      ) then
cdiag.            write (lp,100) nstep,i+i0,j+j0,k,'t,s,dt,ds',
cdiag.            fld1(i,j),fld2(i,j),util1(i,j),util2(i,j)
cdiag.            call flush(lp)
cdiag.          endif
cdiag.        endif
c
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        if     (lpipe .and. lpipe_tsadvc) then
c ---     compare two model runs.
          write (text,'(a9,i3)') 'util1  k=',k
          call pipe_compare_sym1(util1,ip,text)
          write (text,'(a9,i3)') 'util2  k=',k
          call pipe_compare_sym1(util2,ip,text)
          write (text,'(a9,i3)') 'fld1.n k=',k
          call pipe_compare_sym1(fld1(1-nbdy,1-nbdy),ip,text)
          write (text,'(a9,i3)') 'fld2.n k=',k
          call pipe_compare_sym1(fld2(1-nbdy,1-nbdy),ip,text)
        endif
c
      return
      end subroutine tsdff_2x
c
      subroutine tsdff_1x(fld1)
      real fld1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c --- Laplacian  diffusion for a single scalar field
c
        margin = 1
c
!$OMP   PARALLEL DO PRIVATE(j,i,factor)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_U) then
              factor=temdf2*aspux(i,j)*
     &               scuy(i,j)*harmon(max(dp(i-1,j,k,n),onemu)
     &                               ,max(dp(i  ,j,k,n),onemu))
              uflux (i,j)=factor*(fld1(i-1,j)-fld1(i,j))
            endif !iu
            if (SEA_V) then
              factor=temdf2*aspvy(i,j)*
     &               scvx(i,j)*harmon(max(dp(i,j-1,k,n),onemu)
     &                               ,max(dp(i,j  ,k,n),onemu))
              vflux (i,j)=factor*(fld1(i,j-1)-fld1(i,j))
            endif !iv
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        if     (lpipe .and. lpipe_tsadvc) then
c ---     compare two model runs.
          write (textu,'(a9,i3)') 'uflux  k=',k
          write (textv,'(a9,i3)') 'vflux  k=',k
          call pipe_compare_sym2(uflux, iu,textu,
     &                           vflux, iv,textv)
        endif
c
c ---   rhs: dp.n, uflux+, vflux+, uflux2+, vflux2+
c ---   lhs: saln.n, temp.n, th3d.n
c
        margin = 0
c
!$OMP   PARALLEL DO PRIVATE(j,i,factor)
!$OMP&           SCHEDULE(STATIC,jblk)
        do j=1-margin,jj+margin
          do i=1-margin,ii+margin
            if (SEA_P) then
              factor=-delt1/(scp2(i,j)*max(dp(i,j,k,n),onemu))
              util1(i,j)=((uflux (i+1,j)-uflux (i,j))
     &                   +(vflux (i,j+1)-vflux (i,j)))*factor
              fld1(i,j)=fld1(i,j)+util1(i,j)
c
cdiag         if (i.eq.itest.and.j.eq.jtest) then
cdiag           if (1.le.i .and. i.le.ii .and.
cdiag.              1.le.j .and. j.le.jj      ) then
cdiag.            write (lp,100) nstep,i+i0,j+j0,k,'t,s,dt,ds',
cdiag.            fld1(i,j),0.0,util1(i,j),0.0
cdiag.            call flush(lp)
cdiag.          endif
cdiag.        endif
c
            endif !ip
          enddo !i
        enddo !j
!$OMP   END PARALLEL DO
c
        if     (lpipe .and. lpipe_tsadvc) then
c ---     compare two model runs.
          write (text,'(a9,i3)') 'util1  k=',k
          call pipe_compare_sym1(util1,ip,text)
          write (text,'(a9,i3)') 'fld1.n k=',k
          call pipe_compare_sym1(fld1(1-nbdy,1-nbdy),ip,text)
        endif
c
      return
      end subroutine tsdff_1x

c-----end contains

      end subroutine tsadvc
c
      end module mod_tsadvc
c
c  Revision history:
c
c> June 1995 - eliminated setting of salinity in massless layers (loop 46)
c>             (this is now done in mxlayr.f)
c> Aug. 1995 - omitted t/s/dp time smoothin, case of abrupt mxlayr.thk.change
c> Sep. 1995 - increased temdf2 if mixed layer occupies >90% of column
c> Mar. 2000 - removed 'cushn' and added logic to assure global conservation
c> Apr. 2000 - conversion to SI units
c> Apr. 2000 - changed i/j loop nesting to j/i
c> Aug. 2000 - temp advection and diffusion only for hybrid vertical coordinate
c> Nov. 2000 - nhybrd T&S advection layers, kdm-nhybrd S advection layers
c> Nov. 2000 - T&S or th&S advection/diffusion based on advflg
c> Feb. 2001 - placed advem in a module
c> May  2002 - diffusion coefficent based on max(sc?x,sc?y)
c> Aug. 2003 - separate PCM and MPDATA versions (advtyp)
c> Aug. 2003 - added FCT2 and UTOPIA advection options.
c> Nov. 2003 - per layer diffusion routine for 1 or 2 scalar fields
c> Feb. 2008 - fixed famin,famax bugs in FCT2/4
c> Jun. 2008 - fixed q2,q2l halo update bug
c> Aug  2011 - reworked Robert-Asselin filter, RA of dp now in cnuity
c> Aug  2012 - RA filter now exactly conserves constant tracers
c> Sep  2012 - RA filter not applied if thickness < onezm
c> Apr. 2014 - replace ip with ipa for mass sums
c> May  2014 - use land/sea masks (e.g. ip) to skip land
c> May  2014 - use ipim1,ipip1,ipjm1,ipjp1 as sea-only neighbors
