#if ! defined(RELO)
      subroutine latbdf(n,lll)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_tides      ! HYCOM tides
      implicit none
c     
      integer n,lll
c     
c     --- apply lateral boundary conditions to   barotropic  flow field
c     
c     --- port flow version:
c     --- NOT similar to the standard 'Browning and Kreiss' MICOM/HYCOM open
c     --- boundary condition. This version uses algorithms based on a 
c     --- 1 invariant Flather boundary condition (setting the gradient
c     --- of the incoming characteristic to zero).
c
c     --- The tangential velocity is not constrained.
c
c     --- see also: latbdp
c     
c     --- the code is as similar as possible to that for the standard case.
c     --- so for example, 'speed' is in fact 1/SQRT(gH) which represents
c     --- c1/g in the notation of (Bleck and Sun, Open boundary conditions
c     --- for MICOM).  The 1/g allows for the use of pressure fields.
c
c     --- Note that East, West, North and South refers to the grid 
c     --- (i.e i,j points) and NOT geographic East, West, North and South
c     
c     --- the first call is made during initialization.
c     
c     --- Iris Lohmann, Carlos Lozano, NCEP, April 2006
c     
      logical, parameter :: ldebug_latbdf=.false.
c
      integer, parameter :: mports=1  !maximum number of ports
c
      integer, parameter :: nchar =120
c     
      logical    lfatal,lfatalp
      integer    i,j,isec,ifrst,ilast,l
      real       aline(nchar),          
     &    dline(itdm+jtdm),xline(itdm+jtdm),
     &    pline(itdm+jtdm),uline(itdm+jtdm)

      real        sum,svspin,fatal
      character*3 char3
c 
      integer nports    
      integer lnport(mports),kdport(mports)
      integer jfport(mports),jlport(mports),
     &        ifport(mports),ilport(mports)
      real    svpnow(mports),svport(mports)

      save lnport
      save svpnow,svport
      save nports,kdport,ifport,ilport,jfport,jlport
c     
      real uportw(jtdm,mports),speedw(jtdm,mports),rspedw(jtdm,mports),
     &     uporte(jtdm,mports),speede(jtdm,mports),rspede(jtdm,mports),
     &     vportn(itdm,mports),speedn(itdm,mports),rspedn(itdm,mports),
     &     vports(itdm,mports),speeds(itdm,mports),rspeds(itdm,mports)
      save uportw,speedw,rspedw,uporte,speede,rspede,
     &     vportn,speedn,rspedn,vports,speeds,rspeds

c     tides stuff

      integer npts_p,kdpt_p(mports),
     &               ifpt_p(mports),ilpt_p(mports),
     &               jfpt_p(mports),jlpt_p(mports),lnpt_p(mports)
      integer npts_v,kdpt_v(mports),
     &               ifpt_v(mports),ilpt_v(mports),
     &               jfpt_v(mports),jlpt_v(mports),lnpt_v(mports)
      integer npts_u,kdpt_u(mports),
     &               ifpt_u(mports),ilpt_u(mports),
     &               jfpt_u(mports),jlpt_u(mports),lnpt_u(mports)

c     the max of itdm and jtdm is ok for any port
c     number of tidal consituents (ncon) from mod_tides
      real z1r_p(mports,max(jtdm,itdm),ncon)
      real z1i_p(mports,max(jtdm,itdm),ncon)
      real z1r_u(mports,max(jtdm,itdm),ncon)
      real z1i_u(mports,max(jtdm,itdm),ncon)
      real z1r_v(mports,max(jtdm,itdm),ncon)
      real z1i_v(mports,max(jtdm,itdm),ncon)
      real tmp1, tmp2

      real tmpr(max(itdm,jtdm),ncon), tmpi(max(itdm,jtdm),ncon)

      real  upred(max(itdm,jtdm)),zpred(max(itdm,jtdm))
      real  udpred(max(itdm,jtdm))
      real  vpred(max(itdm,jtdm))
      real  ulow(max(itdm,jtdm),mports),plow(max(itdm,jtdm),mports)
      real  uu(max(itdm,jtdm)),vv(max(itdm,jtdm))
      integer bnd_init(mports)

      real*8 d_time
      real*8 timermp,frmp
      logical astroflag
      integer jn,in,ic

      save z1r_p, z1i_p
      save z1r_u, z1i_u
      save bnd_init
      save ulow,plow
c     
      character*13 fmt
      save         fmt
      data         fmt / '(i4,1x,120i1)' /
c
c     USER-INPUT: optimization coefficients for the 1 inv algorithm.
      real w_1,w_1c
      save w_1 
      data w_1 / 0.1 / 
c     
      integer lcount
      save    lcount
      data    lcount / 0 /

c     
c     Comment out the next line to run without 
c     boundary tides
      d_time = time_8 + lll*dlt/86400.d0

c     add 15384 for obtaining mjd
      lcount = lcount + 1
c
c     set 1-invariant coefficient
      w_1c=1.0-w_1
c
c     
c     --- the first call just initializes data structures.
c     
      if     (lcount.eq.1) then
c     
        open(unit=uoff+99,file=trim(flnminp)//'ports.input')
c     
c     ---   'nports' = number of boundary port sections.
        call blkini(nports,'nports')
        if     (mnproc.eq.1) then
          write(lp,*)
        endif
        if     (nports.lt.0 .or. nports.gt.mports) then
          if     (mnproc.eq.1) then
            write(lp,*) 
            write(lp,*) 'error in latbdf - illegal nports value'
            if     (nports.gt.mports) then
              write(lp,*) 'increase parameter mports to',nports
            endif
            write(lp,*) 
            call flush(lp)
          endif
          call xcstop('(latbdp)')
          stop '(latbdp)'
        endif
c     
c     ---   read in the ports one at a time
c     
        do l= 1,nports
c     
c     ---     port location is w.r.t. u (EW) or v (NS) grid
c     ---     and identifies the sea at the port
c     ---     the minimum index is 0
c     
c     ---     'kdport' = port orientation (1=N, 2=S, 3=E, 4=W, 5=S(Fundy))
c     ---     'ifport' = first i-index
c     ---     'ilport' = last  i-index (=ifport for N or S orientation)
c     ---     'jfport' = first j-index
c     ---     'jlport' = last  j-index (=jfport for E or W orientation)
c     ---     'svpnow' = existing port transport in Sv (+ve towards E or S)
c     ---     'svport' = target   port transport in Sv (+ve towards E or S)
c     ---     'lnport' = port length (calculated, not input)
          call blkini(kdport(l),'kdport')
          call blkini(ifport(l),'ifport')
          call blkini(ilport(l),'ilport')
          call blkini(jfport(l),'jfport')
          call blkini(jlport(l),'jlport')
          call blkinr(svpnow(l),'svpnow','(a6," =",f10.4," Sv")')
          call blkinr(svport(l),'svport','(a6," =",f10.4," Sv")')
          if     (mnproc.eq.1) then
            write(lp,*)
          endif
c     
          lnport(l) = ilport(l)-ifport(l)+jlport(l)-jfport(l)+1
c     
c     ---     sanity check.
c     
          if     (kdport(l).eq.3.or.kdport(l).eq.4) then
            if     (ifport(l).ne.ilport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'error in latbdp - port direction',
     &               ' and orientation are not consistent'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif
          else
            if     (jfport(l).ne.jlport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'error in latbdp - port direction',
     &               ' and orientation are not consistent'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif
          endif
          if     (ifport(l).gt.ilport(l) .or.
     &         jfport(l).gt.jlport(l)     ) then
            if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdp - port',
     &             ' location is not consistent'
              write(lp,*) 
              call flush(lp)
            endif
            call xcstop('(latbdp)')
            stop '(latbdp)'
          endif
        enddo
c     
        close(unit=uoff+99)

c **********************************************

c------ read low frequency input-file

c       initialize the low-frequency u and eta to zero
          
        do i = 1,max(itdm,jtdm)
          do j = 1,mports
            ulow(i,j) =0.0
            plow(i,j) =0.0
          enddo
        enddo

        open(unit=uoff+99,file=trim(flnminp)//'lowfreq.input')

c       the file is read in order west, east, south, north

        do l= 1,nports

          if     (kdport(l).eq.4) then     
c         western port                     
            do j= jfport(l),jlport(l)     
              read(uoff+99,*) ulow(j,l),plow(j,l)
            enddo
          elseif (kdport(l).eq.3) then    
c         eastern port
            do j= jfport(l),jlport(l)
              read(uoff+99,*) ulow(j,l),plow(j,l)
            enddo
          elseif (kdport(l).eq.2) then
c         southern port  
            do i= ifport(l),ilport(l)
              read(uoff+99,*) ulow(i,l),plow(i,l)
            enddo   
          elseif (kdport(l).eq.1) then
c         northern port  
            do i= ifport(l),ilport(l)
              read(uoff+99,*) ulow(i,l),plow(i,l)
            enddo    
          endif  !kdport

        enddo !l=1,nports

        close(uoff+99)

c************************************************
c ****** READ TIDAL CONSTITUENTS ****************

        if (tidflg.ge.1) then


c     initialize the tidal constituents to zero
          
          do i = 1,mports
            do j = 1,max(itdm,jtdm)
              do ic  = 1,ncon
                z1r_p(i,j,ic)  = 0.
                z1i_p(i,j,ic)  = 0.
                z1r_u(i,j,ic)  = 0.
                z1i_u(i,j,ic)  = 0.
                z1r_v(i,j,ic)  = 0.
                z1i_v(i,j,ic)  = 0.
              enddo
            enddo
          enddo


          do j = 1,max(itdm,jtdm)
            uu(j) = 0.
            vv(j) = 0.
            upred(j) = 0.
            vpred(j) = 0.
            zpred(j) = 0.
          enddo

c  --------------------------------------------------------

c     Now the P points

          open(unit=uoff+99,file=trim(flnminp)//'tidalports_p.input')

c     ---   'nports' = number of boundary port sections.
          call blkini(npts_p,'npts_p')
          if     (mnproc.eq.1) then
            write(lp,*)
          endif 

          if     (npts_p.ne.nports) then
            if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error number of ports needs to be same'
              write(lp,*) 
              call flush(lp)
            endif
            call xcstop('(latbdp)')
            stop '(latbdp)'
          endif

   
          
          do l= 1,nports
c     
c     ---     port location is w.r.t. u (EW) or v (NS) grid
c     ---     and identifies the sea at the port
c     ---     the minimum index is 0
c     
c     ---     'kdport' = port orientation (1=N, 2=S, 3=E, 4=W, 5=Fundy(S))
c     ---     'ifport' = first i-index
c     ---     'ilport' = last  i-index (=ifport for N or S orientation)
c     ---     'jfport' = first j-index
c     ---     'jlport' = last  j-index (=jfport for E or W orientation)
c     ---     'lnport' = port length (calculated, not input)


            call blkini(kdpt_p(l),'kdpt_p')
            
            if     (kdpt_p(l).ne.kdport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the kdport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif
            

            call blkini(ifpt_p(l),'ifpt_p')

            if     (ifpt_p(l).ne.ifport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the ifport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif


            call blkini(ilpt_p(l),'ilpt_p')
            if     (ilpt_p(l).ne.ilport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the ilport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif


            call blkini(jfpt_p(l),'jfpt_p')

            if     (jfpt_p(l).ne.jfport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the jfport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif



            call blkini(jlpt_p(l),'jlpt_p')
            
            if     (jlpt_p(l).ne.jlport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the jlport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif
           
            if     (kdport(l).eq.4) then
c     
c     western port
c                 
              do j= jfport(l),jlport(l)        
                do ic = 1, ncon
                  read(uoff+99,*)  tmp1, tmp2
                  z1r_p(l,j,ic) = tmp1
                  z1i_p(l,j,ic) = tmp2
                enddo 
              enddo             
c     
            elseif (kdport(l).eq.3) then
c     
c     eastern port
              
              do j= jfport(l),jlport(l)
                do ic = 1, ncon
                  read(uoff+99,*)  tmp1, tmp2
                  z1r_p(l,j,ic) = tmp1
                  z1i_p(l,j,ic) = tmp2
                enddo 
              enddo
c     
            elseif (kdport(l).eq.1) then
c     
c     northern port
c                  
              do i= ifport(l),ilport(l)
                do ic = 1, ncon
                  read(uoff+99,*)  tmp1, tmp2
                  z1r_p(l,i,ic) = tmp1
                  z1i_p(l,i,ic) = tmp2
                enddo 
              enddo 
c     
            elseif (kdport(l).eq.2) then
c     
c     southern port
c     
              do i= ifport(l),ilport(l)
                do ic = 1, ncon
                   read(uoff+99,*)  tmp1, tmp2
                  z1r_p(l,i,ic) = tmp1
                  z1i_p(l,i,ic) = tmp2
                enddo 
              enddo
              
            endif !kdport=

c     Close the l = 1, nports loop
          enddo !nports

          close(uoff+99)

c -------------------------------------------------------------          
c     Now the normal-velocity points

          
          open(unit=uoff+99,file=trim(flnminp)//'tidalports_v.input')

c     ---   'nports' = number of boundary port sections.
          call blkini(npts_u,'npts_u')
          if     (mnproc.eq.1) then
            write(lp,*)
          endif

          if     (npts_u.ne.nports) then
            if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error number of ports needs to be same'
              write(lp,*) 
              call flush(lp)
            endif
            call xcstop('(latbdp)')
            stop '(latbdp)'
          endif          
          
          do l= 1,nports
c     
c     ---     port location is w.r.t. u (EW) or v (NS) grid
c     ---     and identifies the sea at the port
c     ---     the minimum index is 0
c     
c     ---     'kdport' = port orientation (1=N, 2=S, 3=E, 4=W, 5=Fundy(S))
c     ---     'ifport' = first i-index
c     ---     'ilport' = last  i-index (=ifport for N or S orientation)
c     ---     'jfport' = first j-index
c     ---     'jlport' = last  j-index (=jfport for E or W orientation)
c     ---     'svpnow' = existing port transport in Sv (+ve towards E or S)
c     ---     'svport' = target   port transport in Sv (+ve towards E or S)
c     ---     'lnport' = port length (calculated, not input)



            call blkini(kdpt_u(l),'kdpt_u')
            
            if     (kdpt_u(l).ne.kdport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the kdport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif
            

            call blkini(ifpt_u(l),'ifpt_u')

            if     (ifpt_u(l).ne.ifport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the ifport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif


            call blkini(ilpt_u(l),'ilpt_u')
            if     (ilpt_u(l).ne.ilport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the ilport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif


            call blkini(jfpt_u(l),'jfpt_u')

            if     (jfpt_u(l).ne.jfport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the jfport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif


            call blkini(jlpt_u(l),'jlpt_u')
            
            if     (jlpt_u(l).ne.jlport(l)) then
              if     (mnproc.eq.1) then
                write(lp,*) 
                write(lp,*) 'Mismatch of the jlport'
                write(lp,*) 
                call flush(lp)
              endif
              call xcstop('(latbdp)')
              stop '(latbdp)'
            endif
            

            if     (kdport(l).eq.4) then
c     
c     western port
c     
              
              do j= jfport(l),jlport(l)  
                do ic = 1, ncon
                  read(uoff+99,*) tmp1, tmp2
                  z1r_u(l,j,ic) = tmp1
                  z1i_u(l,j,ic) = tmp2 
                enddo 
              enddo                   
c     
            elseif (kdport(l).eq.3) then
c     
c     eastern port
              
              do j= jfport(l),jlport(l)
                do ic = 1, ncon
                  read(uoff+99,*) tmp1, tmp2
                  z1r_u(l,j,ic) = tmp1
                  z1i_u(l,j,ic) = tmp2 
                enddo 
              enddo
c     
            elseif (kdport(l).eq.1) then
c     
c     northern port
c     
              do i= ifport(l),ilport(l)
                do ic = 1, ncon
                  read(uoff+99,*) tmp1, tmp2
                  z1r_u(l,i,ic) = tmp1
                  z1i_u(l,i,ic) = tmp2 
                enddo 
              enddo
c     
            elseif (kdport(l).eq.2) then
c     
c     southern port
c                  
              do i= ifport(l),ilport(l)
                do ic = 1, ncon
                  read(uoff+99,*) tmp1, tmp2
                  z1r_u(l,i,ic) = tmp1
                  z1i_u(l,i,ic) = tmp2 
                enddo 
              enddo

            endif !kdport=

c         Close the l = 1, nports loop
          enddo !nports

          close(uoff+99)
          
c*****************END OF READING THE TIDAL CONSTITUENTS
        endif !tidflg.ge.1

c     
c ---   check ports against masks,
c ---   mark the port locations on masks and print them out.
c     
        lfatal = .false.
        do l= 1,nports
          lfatalp = .false.
c     
          if     (kdport(l).eq.4) then
c     
c         western port
c     
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                 j.le.j0 .or. j.gt.j0+jj     ) then
                cycle         ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9 !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0+1,j-j0).ne.1 .or.
     &               iu(i-i0+2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7 !indicate an error
              endif
            enddo
c     
          elseif (kdport(l).eq.3) then
c     
c         eastern port
c     
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                 j.le.j0 .or. j.gt.j0+jj     ) then
                cycle         ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9 !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0-1,j-j0).ne.1 .or.
     &               iu(i-i0-2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7 !indicate an error
              endif
            enddo
c     
          elseif (kdport(l).eq.1) then
c     
c         northern port
c     
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.3 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                 j.le.j0 .or. j.gt.j0+jj     ) then
                cycle         ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9 !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0-1).ne.1 .or.
     &               iv(i-i0,j-j0-2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7 !indicate an error
              endif
            enddo
c     
          elseif (kdport(l).eq.2.or.kdport(l).eq.5) then
c     
c         southern port
c     
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm   .or.
     &                j.lt.1 .or. j.gt.jtdm-2     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                 j.le.j0 .or. j.gt.j0+jj     ) then
                cycle         ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9 !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0+1).ne.1 .or.
     &               iv(i-i0,j-j0+2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7 !indicate an error
              endif
            enddo
c     
          endif !kdport=
c     
          if     (lfatalp) then
            write(lp,*) 
            write(lp,*) 'error in latbdp - port ',l,' mislocated',
     &             '  (mnproc = ',mnproc,')'
            write(lp,*) 
            call flush(lp)
          endif
          lfatal = lfatal .or. lfatalp

        enddo !nports
c     
c       local lfatal to global lfatal
c     
        if     (lfatal) then
          fatal = 1.0
        else
          fatal = 0.0
        endif
        call xcmaxr(fatal)
        lfatal = fatal.gt.0.5
c     
c ---   write out  -iu-  and -iv- arrays, if they are not too big
c ---   data are written in strips nchar points wide
        if     (lfatal .or. max(itdm,jtdm).le.2*nchar) then
          util1(1:ii,1:jj) = iu(1:ii,1:jj) ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
              write (lp,'(a,i5,a,i5)')
     &               'iu array, cols',ifrst+1,' --',ilast
            endif
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
                write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif
            enddo
          enddo
          if     (mnproc.eq.1) then
            write (lp,*)
          endif
          call xcsync(flush_lp)
c     
          util1(1:ii,1:jj) = iv(1:ii,1:jj) ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
              write (lp,'(a,i5,a,i5)')
     &               'iv array, cols',ifrst+1,' --',ilast
            endif
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
                write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif
            enddo
          enddo
          if     (mnproc.eq.1) then
            write (lp,*)
          endif
          call xcsync(flush_lp)
        endif                 ! small region
c     
        if     (lfatal) then
          write(lp,*) 
          write(lp,*) 'error in latbdp - bad port(s)'
          write(lp,*) 
          call flush(lp)
          call xchalt('(latbdp)')
          stop '(latbdp)'
        endif
c     
c ---   restore iu and iv, and zero iuopn and ivopn.
c     
!     $OMP PARALLEL DO PRIVATE(j,i)
!     $OMP&         SCHEDULE(STATIC,jblk)
        do j= 1,jj
          do i= 1,ii
            iu(i,j) = max( iu(i,j), 0 )
            iv(i,j) = max( iv(i,j), 0 )
          enddo
        enddo
!     $OMP PARALLEL DO PRIVATE(j,i)
!     $OMP&         SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            iuopn(i,j) = 0
            ivopn(i,j) = 0
          enddo
        enddo

c     
c  ---  initialize the ports
c     
        do l= 1,nports
          if     (kdport(l).eq.4) then
c     
c         western port
c     
            sum = 0.0
            i = ifport(l)
            j = jfport(l) 
            call xclget(dline(j),lnport(l), depths,i,j,0,1, 0)
            call xclget(xline(j),lnport(l), scuy,  i,  j,0,1, 0)
            do j= jfport(l),jlport(l)
              sum = sum + dline(j)*xline(j)
            enddo
            sum = 1.e6/sum
            do j= jfport(l),jlport(l)
              uportw(j,l) = sum
              speedw(j,l) = sqrt(1.0*thref/(onem*dline(j)))
              rspedw(j,l) = 1.0/speedw(j,l)
              if     (i.ge.i0+ 1-nbdy .and.
     &               i.le.i0+ii+nbdy .and.
     &               j.ge.j0+ 1-nbdy .and.
     &               j.le.j0+jj+nbdy      ) then
                iuopn(i-i0,j-j0) = 1
              endif
            enddo
c     
          elseif (kdport(l).eq.3) then
c     
c         eastern port
c     
            sum = 0.0
            i = ifport(l)-1
            j = jfport(l)
            call xclget(dline(j),lnport(l), depths,i,  j,0,1, 0)
            call xclget(xline(j),lnport(l), scuy,  i+1,j,0,1, 0)
            do j= jfport(l),jlport(l)
              sum = sum + dline(j)*xline(j)
            enddo
            sum = 1.e6/sum
            do j= jfport(l),jlport(l)
              uporte(j,l) = sum
              speede(j,l) = sqrt(1.0*thref/(onem*dline(j)))
              rspede(j,l) = 1.0/speede(j,l)
              if     (i+1.ge.i0+ 1-nbdy .and.
     &               i+1.le.i0+ii+nbdy .and.
     &               j  .ge.j0+ 1-nbdy .and.
     &               j  .le.j0+jj+nbdy      ) then
                iuopn(i-i0+1,j-j0) = 1
              endif
            enddo
c     
          elseif (kdport(l).eq.1) then
c     
c         northern port
c     
            sum = 0.0
            j = jfport(l)-1
            i = ifport(l)
            call xclget(dline(i),lnport(l), depths,i,j,  1,0, 0)
            call xclget(xline(i),lnport(l), scuy,  i,j+1,1,0, 0)
            do i= ifport(l),ilport(l)
              sum = sum + dline(i)*xline(i)
            enddo
            sum = 1.e6/sum
            do i= ifport(l),ilport(l)
              vportn(i,l) = sum
              speedn(i,l) = sqrt(1.0*thref/(onem*dline(i)))
              rspedn(i,l) = 1.0/speedn(i,l)
              if     (i  .ge.i0+ 1-nbdy .and.
     &               i  .le.i0+ii+nbdy .and.
     &               j+1.ge.j0+ 1-nbdy .and.
     &               j+1.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0+1) = 1
              endif
            enddo
c     
          elseif (kdport(l).eq.2.or.kdport(l).eq.5) then
c     
c         southern port
c     
            sum = 0.0
            j = jfport(l)
            i = ifport(l)
            call xclget(dline(i),lnport(l), depths,i,j,1,0, 0)
            call xclget(xline(i),lnport(l), scuy,  i,j,  1,0, 0)
            do i= ifport(l),ilport(l)
              sum = sum + dline(i)*xline(i)
            enddo
            sum = 1.e6/sum
            do i= ifport(l),ilport(l)
              vports(i,l) = sum
              speeds(i,l) = sqrt(1.0*thref/(onem*dline(i)))
              rspeds(i,l) = 1.0/speeds(i,l)
              if     (i.ge.i0+ 1-nbdy .and.
     &               i.le.i0+ii+nbdy .and.
     &               j.ge.j0+ 1-nbdy .and.
     &               j.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0) = 1
              endif
            enddo
c     
          endif
c     
        enddo !nports

        if     (mnproc.eq.1) then
          write(lp,*) 
          call flush(lp)
        endif
c     
c       end of initialization
c     
        call xcsync(flush_lp)
        return
      endif !lcount=1
c     
c --- 'wellposed' treatment of pressure and normal velocity fields
c --- not in fact wellposed with this exterior data
c
c --- set ramping factor 
c     when ramp_time=0:no ramping of tide (=full tide)
c     when ramp_time>0: ramping of tide, if...  
c     ...ramp_orig<=timermp<=(ramp_orig+ramp_time)
      if     (ramp_time.gt.0.0) then
        timermp=d_time 
        if     (timermp.ge.ramp_orig) then
          timermp=(timermp-ramp_orig)/ramp_time
c         frmp=(1-exp(-10*timermp))    
          frmp=(1-exp(-5*timermp))
        else      
          frmp=0.0
        endif   
      else   
        frmp=1.0  
      endif   

c     
      do l=1,nports

        if     (kdport(l).eq.4) then
c     
c       western port
c     
          i = ifport(l)
          j = jfport(l)
          call xclget(dline(j),  lnport(l),
     &           depthu,                 i,j,0,1, 0)            
          call xclget(pline(j),  lnport(l),
     &           pbavg(1-nbdy,1-nbdy,n), i,  j,0,1, 0)             
          call xclget(uline(j),lnport(l),
     &           ubavg(1-nbdy,1-nbdy,n), i+1,j,0,1, 0)

            
          if (tidflg.ge.1) then  !tide

            do jn= jfport(l),jlport(l)
              do ic = 1, ncon
                tmpr(jn,ic) = z1r_u(l,jn,ic)
                tmpi(jn,ic) = z1i_u(l,jn,ic)
              enddo
            enddo
                            
            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,uu,j,max(itdm,jtdm),lnport(l)) !normal 
            
c           Note!! uu and vv from tpx are transports
            do jn= jfport(l),jlport(l)
               upred(jn)=uu(jn)*frmp*onem/dline(jn)
            enddo
              
            do jn= jfport(l),jlport(l)
              do ic = 1, ncon
                tmpr(jn,ic) = z1r_p(l,jn,ic)
                tmpi(jn,ic) = z1i_p(l,jn,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,zpred,j,max(itdm,jtdm),lnport(l)) 
                                     
            do j= jfport(l),jlport(l)
              zpred(j)=zpred(j)*onem*frmp
            enddo


            do j= jfport(l),jlport(l) 
c         ----set both u and eta at boundary; 1 invariant weighted:
              uline(j)=ulow(j,l)+upred(j)
     &            +w_1*speedw(j,l)*((plow(j,l)+zpred(j))-pline(j))
              pline(j)=w_1c*(plow(j,l)+zpred(j))+w_1*pline(j)

            enddo
      

          else  !no bnd-tide
 

            do j= jfport(l),jlport(l)
c       ----  set both u and eta; 1 invariant weighted:
              uline(j)=ulow(j,l)+
     &                     w_1*speedw(j,l)*(plow(j,l)-pline(j))
              pline(j)=w_1c*plow(j,l)+w_1*pline(j)


            enddo 

          endif   !tide/no tide            
            
          j = jfport(l)                        
          call xclput(pline(j),  lnport(l),
     &           pbavg(1-nbdy,1-nbdy,n), i,  j,0,1)
          call xclput(uline(j),lnport(l),
     &           ubavg(1-nbdy,1-nbdy,n), i,  j,0,1) 
c     

        elseif (kdport(l).eq.3) then

c         eastern port
c     
          i = ifport(l)-1
          j = jfport(l)
          call xclget(dline(j),  lnport(l),
     &           depthu,                 i+1,j,0,1, 0)
          call xclget(pline(j),  lnport(l), 
     &            pbavg(1-nbdy,1-nbdy,n), i,  j,0,1, 0)
          call xclget(uline(j),lnport(l),
     &           ubavg(1-nbdy,1-nbdy,n), i,  j,0,1, 0)


          if (tidflg.ge.1) then !tide

            do jn= jfport(l),jlport(l)
              do ic = 1, ncon
                tmpr(jn,ic) = z1r_u(l,jn,ic)
                tmpi(jn,ic) = z1i_u(l,jn,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,uu,j,max(itdm,jtdm),lnport(l)) !normal 

c           Note!! uu and vv from tpx are transports
            do jn= jfport(l),jlport(l)
              upred(jn)=uu(jn)*frmp*onem/dline(jn)
            enddo

            do jn= jfport(l),jlport(l)
              do ic = 1, ncon
                tmpr(jn,ic) = z1r_p(l,jn,ic)
                tmpi(jn,ic) = z1i_p(l,jn,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,zpred,j,max(itdm,jtdm),lnport(l)) 
            

            do j= jfport(l),jlport(l)               
              zpred(j) = zpred(j)*onem*frmp
            enddo

            do j= jfport(l),jlport(l)
c         ----set u and eta on boundary; 1 invariant weighted:
              uline(j)=ulow(j,l)+upred(j)
     &             +w_1*speede(j,l)*(pline(j)-(plow(j,l)+zpred(j)))
              pline(j)=w_1c*(plow(j,l)+zpred(j))+w_1*pline(j)
            enddo


          else !no bnd-tide


            do j= jfport(l),jlport(l)
c         ----set u and eta on boundary; 1 invariant weighted:
              uline(j)=ulow(j,l)
     &             -w_1*speede(j,l)*(plow(j,l)-pline(j))
              pline(j)=w_1c*plow(j,l)+w_1*pline(j)
            enddo

          endif   !tide/no tide         
                        
          j = jfport(l)
          call xclput(pline(j),  lnport(l), 
     &           pbavg(1-nbdy,1-nbdy,n), i,  j,0,1)
          call xclput(uline(j),lnport(l),
     &           ubavg(1-nbdy,1-nbdy,n), i+1,j,0,1)


        elseif (kdport(l).eq.1) then
c     
c         northern port
c     
          j = jfport(l)-1
          i = ifport(l)
          call xclget(dline(i),  lnport(l),
     &           depthv,                 i,j+1,1,0, 0)
          call xclget(pline(i),  lnport(l), 
     &           pbavg(1-nbdy,1-nbdy,n), i,j,  1,0, 0)
          call xclget(uline(i),lnport(l),
     &           vbavg(1-nbdy,1-nbdy,n), i,j,  1,0, 0)


          if (tidflg.ge.1) then !tide

            do in= ifport(l),ilport(l)
              do ic = 1, ncon
                tmpr(in,ic) = z1r_u(l,in,ic)
                tmpi(in,ic) = z1i_u(l,in,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &           astroflag,uu,i,max(itdm,jtdm),lnport(l)) !normal 

c           Note!! uu and vv from tpx are transports
            do in= ifport(l),ilport(l)
              vpred(in)=uu(in)*frmp*onem/dline(in)
            enddo

            do in= ifport(l),ilport(l)
              do ic = 1, ncon
                tmpr(in,ic) = z1r_p(l,in,ic)
                tmpi(in,ic) = z1i_p(l,in,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,zpred,i,max(itdm,jtdm),lnport(l)) 
           
            do i= ifport(l),ilport(l)               
              zpred(i) =zpred(i)*onem*frmp
            enddo

            do i= ifport(l),ilport(l)   
c         ----set u and eta at boundary; 1 invariant weighted:
              uline(i)=ulow(i,l)+vpred(i)
     &              +w_1*speedn(i,l)*(pline(i)-(plow(i,l)+zpred(i)))
              pline(i)=w_1c*(plow(i,l)+zpred(i))+w_1*pline(i)
            enddo

          else !no bnd-tide

            do i= ifport(l),ilport(l)
c         ----set u and eta at boundary; 1 invariant weighted:
              uline(i)=ulow(i,l)-
     &              w_1*speedn(i,l)*(plow(i,l)-pline(i))
              pline(i)=w_1c*plow(i,l)+w_1*pline(i)
            enddo
          
          endif  !tide/no-tide

          i = ifport(l)
          call xclput(pline(i),  lnport(l),
     &           pbavg(1-nbdy,1-nbdy,n), i,j,  1,0)
          call xclput(uline(i),lnport(l),
     &           vbavg(1-nbdy,1-nbdy,n), i,j+1,1,0)
c     

        elseif (kdport(l).eq.2.or.kdport(l).eq.5) then
c     
c         southern port
c     
          j = jfport(l)
          i = ifport(l)
          call xclget(dline(i),  lnport(l),
     &           depthv,                 i,j,  1,0, 0)
          call xclget(pline(i),  lnport(l),
     &           pbavg(1-nbdy,1-nbdy,n), i,j,  1,0, 0)
          call xclget(uline(i),lnport(l),
     &           vbavg(1-nbdy,1-nbdy,n), i,j+1,1,0, 0)


          if (tidflg.ge.1) then  !tide

            do in= ifport(l),ilport(l)
              do ic = 1, ncon
                tmpr(in,ic) = z1r_u(l,in,ic)
                tmpi(in,ic) = z1i_u(l,in,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,uu,i,max(itdm,jtdm),lnport(l)) !normal 

c           Note!! uu and vv from tpx are transports
            do in= ifport(l),ilport(l)
              vpred(in)=uu(in)*frmp*onem/dline(in)
            enddo

            do in= ifport(l),ilport(l)
              do ic = 1, ncon
                tmpr(in,ic) = z1r_p(l,in,ic)
                tmpi(in,ic) = z1i_p(l,in,ic)
              enddo
            enddo

            call tides_driver(tmpr,tmpi,d_time,
     &             astroflag,zpred,i,max(itdm,jtdm),lnport(l)) 

            do i= ifport(l),ilport(l)
              zpred(i) =  zpred(i)*onem*frmp 
            enddo

            do i= ifport(l),ilport(l)
c         ----set u and eta at boundary; 1 invariant weighted:
              uline(i)=ulow(i,l)+vpred(i)
     &              +w_1*speeds(i,l)*(pline(i)-(plow(i,l)+zpred(i)))
              pline(i)=w_1c*(plow(i,l)+zpred(i))+w_1*pline(i)
            enddo

          else !no bnd-tide

            do i= ifport(l),ilport(l)
c         ----set u and eta at boundary; 1 invariant weighted:
              uline(i)=ulow(i,l)+
     &              w_1*speeds(i,l)*(plow(i,l)-pline(i))
              pline(i)=w_1c*plow(i,l)+w_1*pline(i)
            enddo

          endif  !tide/no-tide

          i = ifport(l)
          call xclput(pline(i),  lnport(l),
     &           pbavg(1-nbdy,1-nbdy,n), i,j,  1,0) 
          call xclput(uline(i),lnport(l),
     &           vbavg(1-nbdy,1-nbdy,n), i,j,  1,0)
c    
        endif !kdport

      enddo !nports
c     
      return
      end subroutine latbdf

      subroutine latbdp(n)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      implicit none
c
      integer n
c
c --- apply lateral boundary conditions to   barotropic  flow field
c
c --- port flow version:
c --- similar to the standard 'Browning and Kreiss' MICOM/HYCOM open
c --- boundary condition, except that the exterior normal velocity
c --- is constant in time and exterior pressure = interior pressure.
c --- tangential velocity is not constrained.
c
c --- see also: latbdp
c
c --- the code is as similar as possible to that for the standard case.
c --- so for example, 'speed' is in fact 1/SQRT(gH) which represents
c --- c1/g in the notation of (Bleck and Sun, Open boundary conditions
c --- for MICOM).  The 1/g allows for the use of pressure fields.
c     
c --- Note that East, West, North and South refers to the grid 
c --- (i.e i,j points) and NOT geographic East, West, North and South
c
c --- the first call is made during initialization.
c
c --- Alan J. Wallcraft,  NRL,  November 1999.
c
      logical, parameter :: ldebug_latbdp=.false.
c
      integer, parameter :: mports=1  !maximum number of ports
c
      integer, parameter :: nchar=120
c
      logical     lfatal,lfatalp
      integer     i,j,isec,ifrst,ilast,l
      real        aline(nchar),
     &            dline(itdm+jtdm),xline(itdm+jtdm),
     &            pline(itdm+jtdm),uline(itdm+jtdm,2)
      real        crs,fin,sum,svspin,uvscl,uvscl2,fatal
      real*8      tstep
      character*3 char3
c
      integer nports,kdport(mports),
     &               ifport(mports),ilport(mports),
     &               jfport(mports),jlport(mports),lnport(mports)
      real    pefold,svpnow(mports),svport(mports)
      real*8  refold
      save    nports,kdport,ifport,ilport,jfport,jlport,lnport
      save    pefold,svpnow,svport,refold
c
      real    uportw(jtdm),speedw(jtdm),rspedw(jtdm),
     &        uporte(jtdm),speede(jtdm),rspede(jtdm),
     &        vportn(itdm),speedn(itdm),rspedn(itdm),
     &        vports(itdm),speeds(itdm),rspeds(itdm)
      save    uportw,speedw,rspedw,uporte,speede,rspede,
     &        vportn,speedn,rspedn,vports,speeds,rspeds
c
      character*13 fmt
      save         fmt
      data         fmt / '(i4,1x,120i1)' /
c
      integer lcount
      save    lcount
      data    lcount / 0 /
c
      lcount = lcount + 1
c
c --- the first call just initializes data structures.
c
      if     (lcount.eq.1) then
c
        open(unit=uoff+99,file=trim(flnminp)//'ports.input')
c
c ---   'nports' = number of boundary port sections.
        call blkini(nports,'nports')
        if     (mnproc.eq.1) then
        write(lp,*)
        endif
        if     (nports.lt.0 .or. nports.gt.mports) then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbdp - illegal nports value'
          if     (nports.gt.mports) then
            write(lp,*) 'increase parameter mports to',nports
          endif
          write(lp,*) 
          call flush(lp)
          endif
          call xcstop('(latbdp)')
                 stop '(latbdp)'
        endif
c
c ---   'pefold' = port transport e-folding time in days
        call blkinr(pefold,'pefold','(a6," =",f10.4," days")')
        if     (mnproc.eq.1) then
        write(lp,*)
        endif
c
c ---   switch units from days to baroclinic time steps
c ---   shift lcount to prevent underflow (lcount*refold.ge.0.001)
c
        tstep  = pefold*(86400.d0/batrop)
        refold = 1.d0/tstep
        lcount = lcount + int(tstep)/1000
c
c ---   read in the ports one at a time
c
        do l= 1,nports
c
c ---     port location is w.r.t. u (EW) or v (NS) grid
c ---     and identifies the sea at the port
c ---     the minimum index is 0
c
c ---     'kdport' = port orientation (1=N, 2=S, 3=E, 4=W)
c ---     'ifport' = first i-index
c ---     'ilport' = last  i-index (=ifport for N or S orientation)
c ---     'jfport' = first j-index
c ---     'jlport' = last  j-index (=jfport for E or W orientation)
c ---     'svpnow' = existing port transport in Sv (+ve towards E or S)
c ---     'svport' = target   port transport in Sv (+ve towards E or S)
c ---     'lnport' = port length (calculated, not input)
          call blkini(kdport(l),'kdport')
          call blkini(ifport(l),'ifport')
          call blkini(ilport(l),'ilport')
          call blkini(jfport(l),'jfport')
          call blkini(jlport(l),'jlport')
          call blkinr(svpnow(l),'svpnow','(a6," =",f10.4," Sv")')
          call blkinr(svport(l),'svport','(a6," =",f10.4," Sv")')
          if     (mnproc.eq.1) then
          write(lp,*)
          endif
c
          lnport(l) = ilport(l)-ifport(l)+jlport(l)-jfport(l)+1
c
c ---     sanity check.
c
          if     (kdport(l).gt.2) then
            if     (ifport(l).ne.ilport(l)) then
              if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdp - port direction',
     &                     ' and orientation are not consistent'
              write(lp,*) 
              call flush(lp)
              endif
              call xcstop('(latbdp)')
                     stop '(latbdp)'
            endif
          else
            if     (jfport(l).ne.jlport(l)) then
              if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdp - port direction',
     &                     ' and orientation are not consistent'
              write(lp,*) 
              call flush(lp)
              endif
              call xcstop('(latbdp)')
                     stop '(latbdp)'
            endif
          endif
          if     (ifport(l).gt.ilport(l) .or.
     &            jfport(l).gt.jlport(l)     ) then
            if     (mnproc.eq.1) then
            write(lp,*) 
            write(lp,*) 'error in latbdp - port',
     &                   ' location is not consistent'
            write(lp,*) 
            call flush(lp)
            endif
            call xcstop('(latbdp)')
                   stop '(latbdp)'
          endif
        enddo
c
        close(unit=uoff+99)
c
c ---   check ports against masks,
c ---   mark the port locations on masks and print them out.
c
        lfatal = .false.
        do l= 1,nports
          lfatalp = .false.
c
          if     (kdport(l).eq.4) then
c
c           western port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9  !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0+1,j-j0).ne.1 .or.
     &                iu(i-i0+2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9  !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0-1,j-j0).ne.1 .or.
     &                iu(i-i0-2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.3 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9  !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0-1).ne.1 .or.
     &                iv(i-i0,j-j0-2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm   .or.
     &                j.lt.1 .or. j.gt.jtdm-2     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9  !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0+1).ne.1 .or.
     &                iv(i-i0,j-j0+2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          endif
c
          if     (lfatalp) then
            write(lp,*) 
            write(lp,*) 'error in latbdp - port ',l,' mislocated',
     &                  '  (mnproc = ',mnproc,')'
            write(lp,*) 
            call flush(lp)
          endif
          lfatal = lfatal .or. lfatalp
        enddo
c
c       local lfatal to global lfatal
c
        if     (lfatal) then
          fatal = 1.0
        else
          fatal = 0.0
        endif
        call xcmaxr(fatal)
        lfatal = fatal.gt.0.5
c
c ---   write out  -iu-  and -iv- arrays, if they are not too big
c ---   data are written in strips nchar points wide
        if     (lfatal .or. max(itdm,jtdm).le.2*nchar) then
          util1(1:ii,1:jj) = iu(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'iu array, cols',ifrst+1,' --',ilast
            endif
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
c
          util1(1:ii,1:jj) = iv(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'iv array, cols',ifrst+1,' --',ilast
            endif
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
        endif  ! small region
c
        if     (lfatal) then
          write(lp,*) 
          write(lp,*) 'error in latbdp - bad port(s)'
          write(lp,*) 
          call flush(lp)
          call xchalt('(latbdp)')
                 stop '(latbdp)'
        endif
c
c ---   restore iu and iv, and zero iuopn and ivopn.
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1,jj
          do i= 1,ii
            iu(i,j) = max( iu(i,j), 0 )
            iv(i,j) = max( iv(i,j), 0 )
          enddo
        enddo
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            iuopn(i,j) = 0
            ivopn(i,j) = 0
          enddo
        enddo
c
c ---   initialize the ports
c
        do l= 1,nports
          if     (kdport(l).eq.4) then
c
c           western port
c
            sum = 0.0
            i = ifport(l)
            j = jfport(l)
            call xclget(dline(j),lnport(l), depths,i+1,j,0,1, 0)
            call xclget(xline(j),lnport(l), scuy,  i,  j,0,1, 0)
            do j= jfport(l),jlport(l)
              sum = sum + dline(j)*xline(j)
            enddo
            sum = 1.e6/sum
            do j= jfport(l),jlport(l)
              uportw(j) = sum
              speedw(j) = sqrt(thref/(onem*dline(j)))
              rspedw(j) = 1.0/speedw(j)
              if     (mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1p2e13.5)') 
     &          'w port: ',l,i,j,uportw(j),speedw(j)
              endif
c
              if     (i.ge.i0+ 1-nbdy .and.
     &                i.le.i0+ii+nbdy .and.
     &                j.ge.j0+ 1-nbdy .and.
     &                j.le.j0+jj+nbdy      ) then
                iuopn(i-i0,j-j0) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            sum = 0.0
            i = ifport(l)-1
            j = jfport(l)
            call xclget(dline(j),lnport(l), depths,i,  j,0,1, 0)
            call xclget(xline(j),lnport(l), scuy,  i+1,j,0,1, 0)
            do j= jfport(l),jlport(l)
              sum = sum + dline(j)*xline(j)
            enddo
            sum = 1.e6/sum
            do j= jfport(l),jlport(l)
              uporte(j) = sum
              speede(j) = sqrt(thref/(onem*dline(j)))
              rspede(j) = 1.0/speede(j)
              if     (mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1p2e13.5)') 
     &          'e port: ',l,i,j,uporte(j),speede(j)
              endif
c
              if     (i+1.ge.i0+ 1-nbdy .and.
     &                i+1.le.i0+ii+nbdy .and.
     &                j  .ge.j0+ 1-nbdy .and.
     &                j  .le.j0+jj+nbdy      ) then
                iuopn(i-i0+1,j-j0) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            sum = 0.0
            j = jfport(l)-1
            i = ifport(l)
            call xclget(dline(i),lnport(l), depths,i,j,  1,0, 0)
            call xclget(xline(i),lnport(l), scuy,  i,j+1,1,0, 0)
            do i= ifport(l),ilport(l)
              sum = sum + dline(i)*xline(i)
            enddo
            sum = 1.e6/sum
            do i= ifport(l),ilport(l)
              vportn(i) = sum
              speedn(i) = sqrt(thref/(onem*dline(i)))
              rspedn(i) = 1.0/speedn(i)
              if     (mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1p2e13.5)') 
     &          'n port: ',l,i,j,vportn(i),speedn(i)
              endif
c
              if     (i  .ge.i0+ 1-nbdy .and.
     &                i  .le.i0+ii+nbdy .and.
     &                j+1.ge.j0+ 1-nbdy .and.
     &                j+1.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0+1) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            sum = 0.0
            j = jfport(l)
            i = ifport(l)
            call xclget(dline(i),lnport(l), depths,i,j+1,1,0, 0)
            call xclget(xline(i),lnport(l), scuy,  i,j,  1,0, 0)
            do i= ifport(l),ilport(l)
              sum = sum + dline(i)*xline(i)
            enddo
            sum = 1.e6/sum
            do i= ifport(l),ilport(l)
              vports(i) = sum
              speeds(i) = sqrt(thref/(onem*dline(i)))
              rspeds(i) = 1.0/speeds(i)
              if     (mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1p2e13.5)') 
     &          's port: ',l,i,j,vports(i),speeds(i)
              endif
c
              if     (i.ge.i0+ 1-nbdy .and.
     &                i.le.i0+ii+nbdy .and.
     &                j.ge.j0+ 1-nbdy .and.
     &                j.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0) = 1
              endif
            enddo
c
          endif
c
          if     (mnproc.eq.1) then
          write(lp,*) 'port, now/target velocity = ',
     &                l,svpnow(l)*sum,svport(l)*sum
          call flush(lp)
          endif
        enddo
        if     (mnproc.eq.1) then
        write(lp,*) 
        call flush(lp)
        endif
c
c       end of initialization
c
        call xcsync(flush_lp)
        return
      endif
c
c --- 'wellposed' treatment of pressure and normal velocity fields
c --- not in fact wellposed with this exterior data
c
      tstep  = lcount
      svspin = exp( -tstep*refold )
      do l= 1,nports
        uvscl = svport(l) + svspin*(svpnow(l)-svport(l))
c
        if     (kdport(l).eq.4) then
c
c         western port
c
          i = ifport(l)
          j = jfport(l)
          call xclget(dline(j),  lnport(l),
     &                depthu,                 i+1,j,0,1, 0)
          call xclget(xline(j),  lnport(l),
     &                scuy,                   i,  j,0,1, 0)
          call xclget(pline(j),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,  j,0,1, 0)
          call xclget(uline(j,1),lnport(l),
     &                ubavg(1-nbdy,1-nbdy,n), i+1,j,0,1, 0)
          call xclget(uline(j,2),lnport(l),
     &                ubavg(1-nbdy,1-nbdy,n), i+2,j,0,1, 0)
          sum = 0.0
          do j= jfport(l),jlport(l)
            crs=uvscl*uportw(j)+speedw(j)*pline(j)
            fin=1.5*uline(j,1)-.5*uline(j,2)-speedw(j)*pline(j)
            sum=sum+((crs+fin)-uline(j,1))*dline(j)*xline(j)
          enddo
          uvscl2 = uvscl + (uvscl - sum/(onem*1.e6))
          sum = 0.0
          do j= jfport(l),jlport(l)
            crs=uvscl2*uportw(j)+speedw(j)*pline(j)
            fin=1.5*uline(j,1)-.5*uline(j,2)-speedw(j)*pline(j)
            pline(j)  =.5*(crs-fin)*rspedw(j)
            uline(j,1)=(crs+fin)-uline(j,1)
            sum=sum+uline(j,1)*dline(j)*xline(j)
          enddo
          j = jfport(l)
          call xclput(pline(j),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,  j,0,1)
          call xclput(uline(j,1),lnport(l),
     &                ubavg(1-nbdy,1-nbdy,n), i,  j,0,1)
c
              if     (ldebug_latbdp .and. mnproc.eq.1) then
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(pb - ',
     &                                      l,lnport(l),i,  j,0,1
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(ub - ',
     &                                      l,lnport(l),i,  j,0,1
                call flush(lp)
              endif
c
        elseif (kdport(l).eq.3) then
c
c         eastern port
c
          i = ifport(l)-1
          j = jfport(l)
          call xclget(dline(j),  lnport(l),
     &                depthu,                 i+1,j,0,1, 0)
          call xclget(xline(j),  lnport(l),
     &                scuy,                   i+1,j,0,1, 0)
          call xclget(pline(j),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,  j,0,1, 0)
          call xclget(uline(j,1),lnport(l),
     &                ubavg(1-nbdy,1-nbdy,n), i,  j,0,1, 0)
          call xclget(uline(j,2),lnport(l),
     &                ubavg(1-nbdy,1-nbdy,n), i-1,j,0,1, 0)
          sum = 0.0
          do j= jfport(l),jlport(l)
            crs=uvscl*uporte(j)-speede(j)*pline(j)
            fin=1.5*uline(j,1)-.5*uline(j,2)+speede(j)*pline(j)
            sum=sum+((crs+fin)-uline(j,1))*dline(j)*xline(j)
          enddo
          uvscl2 = uvscl + (uvscl - sum/(onem*1.e6))
          sum = 0.0
          do j= jfport(l),jlport(l)
            crs=uvscl2*uporte(j)-speede(j)*pline(j)
            fin=1.5*uline(j,1)-.5*uline(j,2)+speede(j)*pline(j)
            pline(j)  =.5*(fin-crs)*rspede(j)
            uline(j,1)=(fin+crs)-uline(j,1)
            sum=sum+uline(j,1)*dline(j)*xline(j)
*             if     (mnproc.eq.1) then
*             write(lp,'(a,i2,2i5,1p2e13.5)') 
*    &          'e port: ',l,i,j,pline(j),uline(j,1)
*             endif
          enddo
          j = jfport(l)
          call xclput(pline(j),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,  j,0,1)
          call xclput(uline(j,1),lnport(l),
     &                ubavg(1-nbdy,1-nbdy,n), i+1,j,0,1)
c
              if     (ldebug_latbdp .and. mnproc.eq.1) then
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(pb - ',
     &                                      l,lnport(l),i,  j,0,1
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(ub - ',
     &                                      l,lnport(l),i+1,j,0,1
                call flush(lp)
              endif
c
        elseif (kdport(l).eq.1) then
c
c         northern port
c
          j = jfport(l)-1
          i = ifport(l)
          call xclget(dline(i),  lnport(l),
     &                depthv,                 i,j+1,1,0, 0)
          call xclget(xline(i),  lnport(l),
     &                scux,                   i,j+1,1,0, 0)
          call xclget(pline(i),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,j,  1,0, 0)
          call xclget(uline(i,1),lnport(l),
     &                vbavg(1-nbdy,1-nbdy,n), i,j,  1,0, 0)
          call xclget(uline(i,2),lnport(l),
     &                vbavg(1-nbdy,1-nbdy,n), i,j-1,1,0, 0)
          sum = 0.0
          do i= ifport(l),ilport(l)
            crs=uvscl*vportn(i)-speedn(i)*pline(i)
            fin=1.5*uline(i,1)-.5*uline(i,2)+speedn(i)*pline(i)
            sum=sum+((fin+crs)-uline(i,1))*dline(i)*xline(i)
          enddo
          uvscl2 = uvscl + (uvscl - sum/(onem*1.e6))
          sum = 0.0
          do i= ifport(l),ilport(l)
            crs=uvscl2*vportn(i)-speedn(i)*pline(i)
            fin=1.5*uline(i,1)-.5*uline(i,2)+speedn(i)*pline(i)
            pline(i)  =.5*(fin-crs)*rspedn(i)
            uline(i,1)=(fin+crs)-uline(i,1)
            sum=sum+uline(i,1)*dline(i)*xline(i)
          enddo
          i = ifport(l)
          call xclput(pline(i),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,j,  1,0)
          call xclput(uline(i,1),lnport(l),
     &                vbavg(1-nbdy,1-nbdy,n), i,j+1,1,0)
c
              if     (ldebug_latbdp .and. mnproc.eq.1) then
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(pb - ',
     &                                      l,lnport(l),i,j,  1,0
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(vb - ',
     &                                      l,lnport(l),i,j+1,1,0
                call flush(lp)
              endif
c
        elseif (kdport(l).eq.2) then
c
c         southern port
c
          j = jfport(l)
          i = ifport(l)
          call xclget(dline(i),  lnport(l),
     &                depthv,                 i,j,  1,0, 0)
          call xclget(xline(i),  lnport(l),
     &                scux,                   i,j,  1,0, 0)
          call xclget(pline(i),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,j,  1,0, 0)
          call xclget(uline(i,1),lnport(l),
     &                vbavg(1-nbdy,1-nbdy,n), i,j+1,1,0, 0)
          call xclget(uline(i,2),lnport(l),
     &                vbavg(1-nbdy,1-nbdy,n), i,j+2,1,0, 0)
          sum = 0.0
          do i= ifport(l),ilport(l)
            crs=uvscl*vports(i)+speeds(i)*pline(i)
            fin=1.5*uline(i,1)-.5*uline(i,2)-speeds(i)*pline(i)
            sum=sum+((crs+fin)-uline(i,1))*dline(i)*xline(i)
          enddo
          uvscl2 = uvscl + (uvscl - sum/(onem*1.e6))
          sum = 0.0
          do i= ifport(l),ilport(l)
            crs=uvscl2*vports(i)+speeds(i)*pline(i)
            fin=1.5*uline(i,1)-.5*uline(i,2)-speeds(i)*pline(i)
            pline(i)  =.5*(crs-fin)*rspeds(i)
            uline(i,1)=(crs+fin)-uline(i,1)
            sum=sum+uline(i,1)*dline(i)*xline(i)
          enddo
          i = ifport(l)
          call xclput(pline(i),  lnport(l),
     &                pbavg(1-nbdy,1-nbdy,n), i,j,  1,0)
          call xclput(uline(i,1),lnport(l),
     &                vbavg(1-nbdy,1-nbdy,n), i,j,  1,0)
c
              if     (ldebug_latbdp .and. mnproc.eq.1) then
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(pb - ',
     &                                      l,lnport(l),i,j,  1,0
                write(lp,'(a,i2,3i5,2i2)') 'l,xclput(vb - ',
     &                                      l,lnport(l),i,j,  1,0
                call flush(lp)
              endif
c
        endif
c
*       if     (mod(lcount,512).eq.0) then
*         if     (mnproc.eq.1) then
*         write(lp,*) 'latbdp - l,sv,sum = ',l,uvscl,sum/(onem*1.e6)
*         call flush(lp)
*         endif
*       endif
      enddo
c
      return
      end subroutine latbdp
#endif /* ! RELO */
      subroutine latbdt(n,lll)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_tides      ! HYCOM Tidal Data
      implicit none
c
      integer n,lll
c
c --- apply lateral boundary conditions to   barotropic  flow field
c
c --- nested sub-region version:
c --- Uses the 'Browning and Kreiss' MICOM open boundary condition.
c
c --- Tidal z,u & v components added to height and velocity fields
c --- at open boundary points when tidflg = 1 or 3. 
c
c --- Note that 'speed' is in fact qonem*SQRT(g/H).
c --- The qonem allows for the use of pressure fields.
c     
c --- Note that East, West, North and South refers to the grid 
c --- (i.e i,j points) and NOT geographic East, West, North and South
c
c --- the first call is made during initialization.
c
c --- Alan J. Wallcraft,  NRL,  July, 2001.
c --- Dan Moore,          QNA,  August 2011 (tidal components).
c
      logical, parameter :: ldebug_latbdt=.false.  !usually .false.
c
      integer, parameter :: mnp=1       !mnflg for xciegt and xciput
                                        !0 == all tasks; 1 == task 1;
      integer, parameter :: nchar=120
c
      logical     lfatal,lfatalp
      integer     i,j,k,isec,ifrst,ilast,l,np
      real        aline(nchar)
      real        crs,fin,fatal
      character*3 char3
c
      integer, save :: 
     &      nportpts,
     &        nports
      integer, save, allocatable ::
     &      nxport(:),
     &      kdport(:),
     &      ifport(:),ilport(:),
     &      jfport(:),jlport(:),lnport(:)
c
      integer       jline_start,jline,jtide_start,jtide,tidcon1
c
      integer, save, allocatable  :: iaub(:),iavb(:),jaub(:),javb(:),
     &                               iaui(:),iavi(:),jaui(:),javi(:),
     &                               iau2(:),iav2(:),jau2(:),jav2(:),
     &                               iapi(:),iapr(:),japi(:),japr(:)
      real,    save, allocatable  :: pspeed(:),rspeed(:),
     &                               pline(:),uline(:),vline(:),
     &                                        ulin2(:),vlin2(:),
     &                               plnst(:),ulnst(:),vlnst(:)
c
      real,    save, allocatable  :: z_R(:,:,:),z_I(:,:,:),z_A(:)
      real,    save, allocatable  :: port_tide(:,:)
      real*8,  save               :: d_time
c
      character*13 fmt
      save         fmt
      data         fmt / '(i4,1x,120i1)' /
c
      integer lcount
      save    lcount
      data    lcount / 0 /
c
      lcount = lcount + 1
c
c --- immediate return if no ports
c
      if     (lcount.ne.1 .and. nports.eq.0) then
        return
      endif
c
c --- the first call just initializes data structures.
c
      if     (lcount.eq.1) then
c
        open(unit=uoff+99,file=trim(flnminp)//'ports.input')
c
c ---   'nports' = number of boundary port sections.
        call blkini(nports,'nports')
        if     (mnproc.eq.1) then
        write(lp,*)
        endif
        if     (nports.lt.0) then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbdt - illegal nports value'
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbdt)')
                 stop '(latbdt)'
        endif
c
        if     (nports.eq.0) then  !no ports
          close(unit=uoff+99)
          return
        endif
c
        allocate(
     &      nxport(nports),
     &      kdport(nports),
     &      ifport(nports),
     &      ilport(nports),
     &      jfport(nports),
     &      jlport(nports),
     &      lnport(nports) )
c
c ---   read in the ports one at a time
c
        nportpts = 0
        do l= 1,nports
c
c ---     port location is w.r.t. u (EW) or v (NS) grid
c ---     and identifies the sea at the port
c ---     the minimum index is 0
c
c ---     'kdport' = port orientation (1=N, 2=S, 3=E, 4=W)
c ---     'ifport' = first i-index
c ---     'ilport' = last  i-index (=ifport for N or S orientation)
c ---     'jfport' = first j-index
c ---     'jlport' = last  j-index (=jfport for E or W orientation)
c ---     'lnport' = port length (calculated, not input)
          call blkini(kdport(l),'kdport')
          call blkini(ifport(l),'ifport')
          call blkini(ilport(l),'ilport')
          call blkini(jfport(l),'jfport')
          call blkini(jlport(l),'jlport')
          if     (mnproc.eq.1) then
          write(lp,*)
          endif
c
          nxport(l) = nportpts+1
          lnport(l) = ilport(l)-ifport(l)+
     &                jlport(l)-jfport(l)+1
          nportpts  = nportpts+lnport(l)
c
c ---     sanity check.
c
          if     (kdport(l).gt.2) then
            if     (ifport(l).ne.ilport(l)) then
              if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdt - port direction',
     &                     ' and orientation are not consistent'
              write(lp,*) 
              call flush(lp)
              endif !1st tile
              call xcstop('(latbdt)')
                     stop '(latbdt)'
            endif
          else
            if     (jfport(l).ne.jlport(l)) then
              if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdt - port direction',
     &                     ' and orientation are not consistent'
              write(lp,*) 
              call flush(lp)
              endif !1st tile
              call xcstop('(latbdt)')
                     stop '(latbdt)'
            endif
          endif
          if     (ifport(l).gt.ilport(l) .or.
     &            jfport(l).gt.jlport(l)     ) then
            if     (mnproc.eq.1) then
            write(lp,*) 
            write(lp,*) 'error in latbdt - port',
     &                   ' location is not consistent'
            write(lp,*) 
            call flush(lp)
            endif !1st tile
            call xcstop('(latbdt)')
                   stop '(latbdt)'
          endif
        enddo
c
        close(unit=uoff+99)
c
c ---   check ports against masks,
c ---   mark the port locations on masks and print them out.
c
        lfatal = .false.
        do l= 1,nports
          lfatalp = .false.
c
          if     (kdport(l).eq.4) then
c
c           western port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9  !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0+1,j-j0).ne.1 .or.
     &                iu(i-i0+2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9  !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0-1,j-j0).ne.1 .or.
     &                iu(i-i0-2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.3 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9  !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0-1).ne.1 .or.
     &                iv(i-i0,j-j0-2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm   .or.
     &                j.lt.1 .or. j.gt.jtdm-2     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9  !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0+1).ne.1 .or.
     &                iv(i-i0,j-j0+2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          endif
c
          if     (lfatalp) then
            write(lp,*) 
            write(lp,*) 'error in latbdt - port ',l,' mislocated',
     &                  '  (mnproc = ',mnproc,')'
            write(lp,*) 
            call flush(lp)
          endif
          lfatal = lfatal .or. lfatalp
        enddo  !l=1,nports
c
c       local lfatal to global lfatal
c
        if     (lfatal) then
          fatal = 1.0
        else
          fatal = 0.0
        endif
        call xcmaxr(fatal)
        lfatal = fatal.gt.0.5
c
c ---   write out  -iu-  and -iv- arrays, if they are not too big
c ---   data are written in strips nchar points wide
c
        if     (lfatal .or. max(itdm,jtdm).le.2*nchar) then
          util1(1:ii,1:jj) = iu(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'iu array, cols',ifrst+1,' --',ilast
            endif !1st tile
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif !1st tile
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
c
          util1(1:ii,1:jj) = iv(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'iv array, cols',ifrst+1,' --',ilast
            endif !1st tile
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif !1st tile
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
        endif  ! small region
c
        if     (lfatal) then
          write(lp,*) 
          write(lp,*) 'error in latbdt - bad port(s)'
          write(lp,*) 
          call flush(lp)
          call xchalt('(latbdt)')
                 stop '(latbdt)'
        endif
c
c ---   restore iu and iv, and zero iuopn and ivopn.
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1,jj
          do i= 1,ii
            iu(i,j) = max( iu(i,j), 0 )
            iv(i,j) = max( iv(i,j), 0 )
          enddo
        enddo
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            iuopn(i,j) = 0
            ivopn(i,j) = 0
          enddo
        enddo
c
c ---   define the nested boundary input mask.
c
        do j= 1,jj
          do i= 1,ii
            maskbc(i,j) = 0
          enddo
        enddo
c
        do l= 1,nports
          if     (kdport(l).eq.4) then
c
c           western port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            i = ifport(l)-1
            do j= jfport(l),jlport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            j = jfport(l)-1
            do i= ifport(l),ilport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          endif
        enddo  !l=1,nports
c
        if     (ldebug_latbdt) then
          util1(1:ii,1:jj) = maskbc(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'bc array, cols',ifrst+1,' --',ilast
            endif !1st tile
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif !1st tile
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
        endif !ldebug_latbdt
c
c ---   initialize the ports
c
        allocate(iaub(nportpts),jaub(nportpts),
     &           iavb(nportpts),javb(nportpts),
     &           iaui(nportpts),jaui(nportpts),
     &           iavi(nportpts),javi(nportpts),
     &           iau2(nportpts),jau2(nportpts),
     &           iav2(nportpts),jav2(nportpts),
     &           iapi(nportpts),japi(nportpts), 
     &           iapr(nportpts),japr(nportpts) )
c
        do l= 1,nports
          if     (kdport(l).eq.4) then  !western port
            np  = nxport(l)-jfport(l)
            i   = ifport(l)
            do j= jfport(l),jlport(l)
              iapi(np+j) = i
              japi(np+j) = j
c ---         u is normal to the port
              iaub(np+j) = i
              jaub(np+j) = j
              iaui(np+j) = i+1
              jaui(np+j) = j
              iau2(np+j) = i+2
              jau2(np+j) = j
c ---         v is tangential to the port
              iavb(np+j) = i  !j=jfport not active
              javb(np+j) = j  !j=jfport not active
              iavi(np+j) = i
              javi(np+j) = j
              iav2(np+j) = i  !not active
              jav2(np+j) = j  !not active
                if     (ldebug_latbdt .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja w:',np+j,
     &              iaub(np+j),jaub(np+j),iaui(np+j),jaui(np+j)
                endif !ldebug_latbdt
            enddo !j
          elseif (kdport(l).eq.3) then  !eastern port
            np  = nxport(l)-jfport(l)
            i   = ifport(l)-1
            do j= jfport(l),jlport(l)
              iapi(np+j) = i
              japi(np+j) = j
c ---         u is normal to the port
              iaub(np+j) = i+1
              jaub(np+j) = j
              iaui(np+j) = i
              jaui(np+j) = j
              iau2(np+j) = i-1
              jau2(np+j) = j
c ---         v is tangential to the port
              iavb(np+j) = i  !j=jfport not active
              javb(np+j) = j  !j=jfport not active
              iavi(np+j) = i
              javi(np+j) = j
              iav2(np+j) = i  !not active
              jav2(np+j) = j  !not active
                if     (ldebug_latbdt .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja e:',np+j,
     &              iaub(np+j),jaub(np+j),iaui(np+j),jaui(np+j)
                endif !ldebug_latbdt
            enddo !j
          elseif (kdport(l).eq.1) then  !northern port
            np  = nxport(l)-ifport(l)
            j   = jfport(l)-1
            do i= ifport(l),ilport(l)
              iapi(np+i) = i
              japi(np+i) = j
c ---         v is normal to the port
              iavb(np+i) = i
              javb(np+i) = j+1
              iavi(np+i) = i
              javi(np+i) = j
              iav2(np+i) = i
              jav2(np+i) = j-1
c ---         u is tangential to the port
              iaub(np+i) = i  !i=ifport not active
              jaub(np+i) = j  !i=ifport not active
              iaui(np+i) = i
              jaui(np+i) = j
              iau2(np+i) = i  !not active
              jau2(np+i) = j  !not active
                if     (ldebug_latbdt .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja n:',np+i,
     &              iavb(np+i),javb(np+i),iavi(np+i),javi(np+i)
                endif !ldebug_latbdt
            enddo !i
          elseif (kdport(l).eq.2) then  !southern port
            np  = nxport(l)-ifport(l)
            j   = jfport(l)
            do i= ifport(l),ilport(l)
              iapi(np+i) = i
              japi(np+i) = j
c ---         v is normal to the port
              iavb(np+i) = i
              javb(np+i) = j
              iavi(np+i) = i
              javi(np+i) = j+1
              iav2(np+i) = i
              jav2(np+i) = j+2
c ---         u is tangential to the port
              iaub(np+i) = i  !i=ifport not active
              jaub(np+i) = j  !i=ifport not active
              iaui(np+i) = i
              jaui(np+i) = j
              iau2(np+i) = i  !not active
              jau2(np+i) = j  !not active
                if     (ldebug_latbdt .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja n:',np+i,
     &              iavb(np+i),javb(np+i),iavi(np+i),javi(np+i)
                endif !ldebug_latbdt
            enddo !i
          endif !kdport
        enddo  !l=1,nports
c
        do i= 1,nportpts
          iapr(i) = iapi(nportpts+1-i)
          japr(i) = japi(nportpts+1-i)
        enddo
c
        allocate(pspeed(nportpts),
     &           rspeed(nportpts),
     &            pline(nportpts),
     &            uline(nportpts),
     &            ulin2(nportpts),
     &            vline(nportpts),
     &            vlin2(nportpts),
     &            plnst(nportpts),
     &            ulnst(nportpts),
     &            vlnst(nportpts) )
c
        call xciget(pline,nportpts,depths,iapi,japi,0)
c
        do l= 1,nports
          if     (kdport(l).eq.4) then
c
c           western port
c
            np =  nxport(l)-jfport(l)
            i  =  ifport(l)
            do j= jfport(l),jlport(l)
              pspeed(np+j) = qonem*sqrt(g/pline(np+j))
              rspeed(np+j) = 1.0/pspeed(np+j)
              if     (ldebug_latbdt .and. mnproc.eq.1) then
              write(lp,'(a,i2,4i5,1pe13.5)') 
     &          'w port: ',l,i,j,iapi(np+j),japi(np+j),pspeed(np+j)
              endif !ldebug_latbdt
c
              if     (i.ge.i0+ 1-nbdy .and.
     &                i.le.i0+ii+nbdy .and.
     &                j.ge.j0+ 1-nbdy .and.
     &                j.le.j0+jj+nbdy      ) then
                iuopn(i-i0,j-j0) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            np =  nxport(l)-jfport(l)
            i  =  ifport(l)-1
            do j= jfport(l),jlport(l)
              pspeed(np+j) = qonem*sqrt(g/pline(np+j))
              rspeed(np+j) = 1.0/pspeed(np+j)
              if     (ldebug_latbdt .and. mnproc.eq.1) then
              write(lp,'(a,i2,4i5,1pe13.5)') 
     &          'e port: ',l,i,j,iapi(np+j),japi(np+j),pspeed(np+j)
              endif !ldebug_latbdt
c
              if     (i+1.ge.i0+ 1-nbdy .and.
     &                i+1.le.i0+ii+nbdy .and.
     &                j  .ge.j0+ 1-nbdy .and.
     &                j  .le.j0+jj+nbdy      ) then
                iuopn(i-i0+1,j-j0) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            np =  nxport(l)-ifport(l)
            j  =  jfport(l)-1
            do i= ifport(l),ilport(l)
              pspeed(np+i) = qonem*sqrt(g/pline(np+i))
              rspeed(np+i) = 1.0/pspeed(np+i)
              if     (ldebug_latbdt .and. mnproc.eq.1) then
              write(lp,'(a,i2,4i5,1pe13.5)') 
     &          'n port: ',l,i,j,iapi(np+i),japi(np+i),pspeed(np+i)
              endif !ldebug_latbdt
c
              if     (i  .ge.i0+ 1-nbdy .and.
     &                i  .le.i0+ii+nbdy .and.
     &                j+1.ge.j0+ 1-nbdy .and.
     &                j+1.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0+1) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            np =  nxport(l)-ifport(l)
            j  =  jfport(l)
            do i= ifport(l),ilport(l)
              pspeed(np+i) = qonem*sqrt(g/pline(np+i))
              rspeed(np+i) = 1.0/pspeed(np+i)
              if     (ldebug_latbdt .and. mnproc.eq.1) then
              write(lp,'(a,i2,4i5,1pe13.5)') 
     &          's port: ',l,i,j,iapi(np+i),japi(np+i),pspeed(np+i)
              endif !ldebug_latbdt
c
              if     (i.ge.i0+ 1-nbdy .and.
     &                i.le.i0+ii+nbdy .and.
     &                j.ge.j0+ 1-nbdy .and.
     &                j.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0) = 1
              endif
            enddo
c
          endif !kdport
        enddo  !l=1,nports
        if     (ldebug_latbdt .and. mnproc.eq.1) then
        write(lp,*) 
        call flush(lp)
        endif !ldebug_latbdt

        if     (tidflg.eq.1 .or. tidflg.eq.3) then
c ---     last index is 1:3 for z,u,v
          allocate(port_tide(nportpts,3))
          allocate( z_A(     nportpts  ))
          allocate( z_R(ncon,nportpts,3),z_I(ncon,nportpts,3))
c
c ---     read in Z, u & v tidal component at open boundary ports
c
          call latbd_tide(z_A,z_R,z_I,nportpts)
        endif !tidflg
c
c       end of initialization
c
        call xcsync(flush_lp)
        return
      endif
c
c --- nested input only required on first barotropic time step.
c
      if     (lll.eq.1) then
        do j= 1,jj
          do i= 1,ii
            if     (maskbc(i,j).eq.1) then
              util3(i,j) = pbnest(i,j,lb0)*wb0+pbnest(i,j,lb1)*wb1
              util4(i,j) = ubpnst(i,j,lb0)*wb0+ubpnst(i,j,lb1)*wb1
              util5(i,j) = vbpnst(i,j,lb0)*wb0+vbpnst(i,j,lb1)*wb1
            endif
          enddo
        enddo
c
        call xciget(plnst,nportpts,util3,iapi,japi,mnp)
        call xciget(ulnst,nportpts,util4,iapi,japi,mnp)
        call xciget(vlnst,nportpts,util5,iapi,japi,mnp)
c
        if     (tidflg.eq.1 .or. tidflg.eq.3) then
          d_time = time_8 + lll*dlt/86400.d0
          call tides_ports(d_time,nportpts,z_R,z_I,z_A, port_tide)
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do i= 1,nportpts
              plnst(i)=plnst(i)+  onem*port_tide(i,1)
              ulnst(i)=ulnst(i)+0.01d0*port_tide(i,2)
              vlnst(i)=vlnst(i)+0.01d0*port_tide(i,3)
            enddo !i
          endif !mnp
            if     (ldebug_latbdt .and. mnproc.eq.max(mnp,1)) then
              do l= 1,nports
                i=nxport(l)-1 + lnport(l)/2
                if     (kdport(l).eq.4) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'W z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                elseif (kdport(l).eq.3) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'E z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                elseif (kdport(l).eq.1) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'N z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                elseif (kdport(l).eq.2) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'S z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                endif !kdport
              enddo  !l=1,nports
            endif !ldebug_latbdt
        endif !tidflg
      endif !lll.eq.1
c
c return if initialization, continue if integrating
c
      if     (n.eq.0) then
        return
      endif
c
      call xciget(uline,nportpts,ubavg(1-nbdy,1-nbdy,n),iaui,jaui,mnp)
      call xciget(ulin2,nportpts,ubavg(1-nbdy,1-nbdy,n),iau2,jau2,mnp)
      call xciget(vline,nportpts,vbavg(1-nbdy,1-nbdy,n),iavi,javi,mnp)
      call xciget(vlin2,nportpts,vbavg(1-nbdy,1-nbdy,n),iav2,jav2,mnp)
      call xciget(pline,nportpts,pbavg(1-nbdy,1-nbdy,n),iapi,japi,mnp)
c
c --- 'wellposed' treatment of pressure and velocity fields.
c
      do l= 1,nports
c
        if     (kdport(l).eq.4) then
c
c         western port
c
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do j= nxport(l),nxport(l)-1+lnport(l)
              crs=                 ulnst(j)+pspeed(j)*plnst(j)
              fin=1.5*uline(j)-0.5*ulin2(j)-pspeed(j)*pline(j)
              pline(j)=0.5*(crs-fin)*rspeed(j)
              uline(j)=    (crs+fin) -uline(j)
            enddo
            do j= nxport(l)+1,nxport(l)-1+lnport(l)
              vline(j)=0.5*(vlnst(j-1)+vlnst(j))
            enddo
          endif !mnp
c
        elseif (kdport(l).eq.3) then
c
c         eastern port
c
            if     (ldebug_latbdt .and. mnproc.eq.max(mnp,1)) then
              i=nxport(l)-1 + lnport(l)
              write(lp,'(a,2i5,1pe13.5)') 'e port, uline:',
     &                             iaui(i),jaui(i),uline(i)
              write(lp,'(a,2i5,1pe13.5)') 'e port, ulin2:',
     &                             iau2(i),jau2(i),ulin2(i)
              write(lp,'(a,2i5,1pe13.5)') 'e port, pline:',
     &                             iapi(i),japi(i),pline(i)
              write(lp,'(a,2i5,1pe13.5)') 'e port, plnst:',
     &                             iapi(i),japi(i),plnst(i)
              write(lp,'(a,2i5,1pe13.5)') 'e port, ulnst:',
     &                             iapi(i),japi(i),ulnst(i)
            endif !1st tile
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do j= nxport(l),nxport(l)-1+lnport(l)
              crs=                 ulnst(j)-pspeed(j)*plnst(j)
              fin=1.5*uline(j)-0.5*ulin2(j)+pspeed(j)*pline(j)
              pline(j)=0.5*(fin-crs)*rspeed(j)
              uline(j)=    (fin+crs) -uline(j)
            enddo
            do j= nxport(l)+1,nxport(l)-1+lnport(l)
              vline(j)=0.5*(vlnst(j-1)+vlnst(j))
            enddo
          endif !mnp
            if     (ldebug_latbdt .and. mnproc.eq.max(mnp,1)) then
              j=jlport(l)
              i=nxport(l)-1 + lnport(l)
              write(lp,'(a,2i5,1p2e13.5)') 'e port,   crs:',
     &                                            0,j,crs,fin
              write(lp,'(a,2i5,1p1e13.5)') 'e port, pbavg:',
     &                             iapi(i),japi(i),pline(i)
              write(lp,'(a,2i5,1p1e13.5)') 'e port, ubavg:',
     &                             iaub(i),jaub(i),uline(i)
              write(lp,'(a,2i5,1p1e13.5)') 'e port, vbavg:',
     &                             iavb(i),javb(i),vline(i)
              write(lp,*)
              call flush(lp)
            endif !1st tile
c
        elseif (kdport(l).eq.1) then
c
c         northern port
c
            if     (ldebug_latbdt .and. mnproc.eq.max(mnp,1)) then
              j=nxport(l)-1 + lnport(l)
              write(lp,'(a,2i5,1pe13.5)') 'n port, vline:',
     &                             iavi(j),javi(j),vline(j)
              write(lp,'(a,2i5,1pe13.5)') 'n port, vlin2:',
     &                             iav2(j),jav2(j),vlin2(j)
              write(lp,'(a,2i5,1p1e13.5)') 'n port, pline:',
     &                             iapi(j),japi(j),pline(j)
              write(lp,'(a,2i5,1p1e13.5)') 'n port, plnst:',
     &                             iapi(j),japi(j),vlnst(j)
              write(lp,'(a,2i5,1p1e13.5)') 'n port, plnst:',
     &                             iapi(j),japi(j),vlnst(j)
            endif !ldebug_latbdt
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do i= nxport(l),nxport(l)-1+lnport(l)
              crs=                 vlnst(i)-pspeed(i)*plnst(i)
              fin=1.5*vline(i)-0.5*vlin2(i)+pspeed(i)*pline(i)
              pline(i)=0.5*(fin-crs)*rspeed(i)
              vline(i)=    (fin+crs) -vline(i)
            enddo
            do i= nxport(l)+1,nxport(l)-1+lnport(l)
              uline(i)=0.5*(ulnst(i-1)+ulnst(i))
            enddo
          endif !mnp
            if     (ldebug_latbdt .and. mnproc.eq.max(mnp,1)) then
              i=ilport(l)
              j=nxport(l)-1 + lnport(l)
              write(lp,'(a,2i5,1p2e13.5)') 'n port,   crs:',
     &                                            0,i,crs,fin
              write(lp,'(a,2i5,1p1e13.5)') 'n port, pbavg:',
     &                             iapi(j),japi(j),pline(j)
              write(lp,'(a,2i5,1p1e13.5)') 'n port, vbavg:',
     &                             iavb(j),javb(j),vline(j)
              write(lp,'(a,2i5,1p1e13.5)') 'n port, ubavg:',
     &                             iaub(j),jaub(j),uline(j)
              write(lp,*)
              call flush(lp)
            endif !ldebug_latbdt
c
        elseif (kdport(l).eq.2) then
c
c         southern port
c
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do i= nxport(l),nxport(l)-1+lnport(l)
              crs=                 vlnst(i)+pspeed(i)*plnst(i)
              fin=1.5*vline(i)-0.5*vlin2(i)-pspeed(i)*pline(i)
              pline(i)=0.5*(crs-fin)*rspeed(i)
              vline(i)=    (crs+fin) -vline(i)
            enddo
            do i= nxport(l)+1,nxport(l)-1+lnport(l)
              uline(i)=0.5*(ulnst(i-1)+ulnst(i))
            enddo
          endif !mnp
c
        endif !kdport
c
      enddo  !l=1,nports
c     
c --- update the boundary points.
c --- the same p-point can appear twice at a boundary corner.  only
c --- the last appearence counts, so alternate the list direction.
c
      call xciput(uline,nportpts,ubavg(1-nbdy,1-nbdy,n),iaub,jaub,mnp)
      call xciput(vline,nportpts,vbavg(1-nbdy,1-nbdy,n),iavb,javb,mnp)
      if     (mod(lll,2).eq.1) then
        call xciput(pline,nportpts,pbavg(1-nbdy,1-nbdy,n),iapi,japi,mnp)
      else
        do i= 1,nportpts
          ulin2(i) = pline(nportpts+1-i)  !ulin2 is now pline reversed
        enddo
        call xciput(ulin2,nportpts,pbavg(1-nbdy,1-nbdy,n),iapr,japr,mnp)
      endif
c
      return
      end subroutine latbdt

      subroutine latbdtf(n,lll)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_tides      ! HYCOM Tidal Data
      implicit none
c
      integer n,lll
c
c --- apply lateral boundary conditions to   barotropic  flow field
c
c --- nested sub-region version:
c --- Uses a combination of Flather and clamped open boundary condition.
c
c --- The tangential velocity is not constrained.
c
c --- Tidal z,u & v components added to height and velocity fields
c --- at open boundary points when tidflg = 1 or 3. 
c
c --- Note that 'speed' is in fact qonem*SQRT(g/H).
c --- The qonem allows for the use of pressure fields.
c     
c --- Note that East, West, North and South refers to the grid 
c --- (i.e i,j points) and NOT geographic East, West, North and South
c
c --- the first call is made during initialization.
c
c --- Based on latbdt, with Flather from latbdf.
c --- Alan J. Wallcraft,  NRL,  March 2012.
c
      logical, parameter :: ldebug_latbdtf=.false.  !usually .false.
c
      integer, parameter :: mnp=1         !mnflg for xciget and xciput
                                          !0 == all tasks; 1 == task 1;
      integer, parameter :: nchar=120
c
      real,    parameter :: w_f=1.0      !fraction Flather
      real,    parameter :: w_c=1.0-w_f  !fraction clamped SSH and velocity
c
      logical     lfatal,lfatalp
      integer     i,j,k,isec,ifrst,ilast,l,np
      real        aline(nchar)
      real        fatal
      character*3 char3
c
      integer, save :: 
     &      nportpts,
     &        nports
      integer, save, allocatable ::
     &      nxport(:),
     &      kdport(:),
     &      ifport(:),ilport(:),
     &      jfport(:),jlport(:),lnport(:)
c
      integer       jline_start,jline,jtide_start,jtide,tidcon1
c
      integer, save, allocatable  :: iaub(:),iavb(:),jaub(:),javb(:),
     &                               iaui(:),iavi(:),jaui(:),javi(:),
     &                               iapi(:),        japi(:)
      real,    save, allocatable  :: pspeed(:),
     &                               pline(:),uline(:),vline(:),
     &                               plnst(:),ulnst(:),vlnst(:)
      real,    save, allocatable  :: z_R(:,:,:),z_I(:,:,:),z_A(:)
      real,    save, allocatable  :: port_tide(:,:)
      real*8,  save               :: d_time
c
      character*13 fmt
      save         fmt
      data         fmt / '(i4,1x,120i1)' /
c
      integer lcount
      save    lcount
      data    lcount / 0 /
c
      lcount = lcount + 1
c
c --- immediate return if no ports
c
      if     (lcount.ne.1 .and. nports.eq.0) then
        return
      endif
c
c --- the first call just initializes data structures.
c
      if     (lcount.eq.1) then
c
        open(unit=uoff+99,file=trim(flnminp)//'ports.input')
c
c ---   'nports' = number of boundary port sections.
        call blkini(nports,'nports')
        if     (mnproc.eq.1) then
        write(lp,*)
        endif
        if     (nports.lt.0) then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbdtf - illegal nports value'
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbdtf)')
                 stop '(latbdtf)'
        endif
c
        if     (nports.eq.0) then  !no ports
          close(unit=uoff+99)
          return
        endif
c
        allocate(
     &      nxport(nports),
     &      kdport(nports),
     &      ifport(nports),
     &      ilport(nports),
     &      jfport(nports),
     &      jlport(nports),
     &      lnport(nports) )
c
c ---   read in the ports one at a time
c
        nportpts = 0
        do l= 1,nports
c
c ---     port location is w.r.t. u (EW) or v (NS) grid
c ---     and identifies the sea at the port
c ---     the minimum index is 0
c
c ---     'kdport' = port orientation (1=N, 2=S, 3=E, 4=W)
c ---     'ifport' = first i-index
c ---     'ilport' = last  i-index (=ifport for N or S orientation)
c ---     'jfport' = first j-index
c ---     'jlport' = last  j-index (=jfport for E or W orientation)
c ---     'lnport' = port length (calculated, not input)
          call blkini(kdport(l),'kdport')
          call blkini(ifport(l),'ifport')
          call blkini(ilport(l),'ilport')
          call blkini(jfport(l),'jfport')
          call blkini(jlport(l),'jlport')
          if     (mnproc.eq.1) then
          write(lp,*)
          endif
c
          nxport(l) = nportpts+1            !start  of this port
          lnport(l) = ilport(l)-ifport(l)+
     &                jlport(l)-jfport(l)+1 !length of this port
          nportpts  = nportpts+lnport(l)    !length of all ports
c
c ---     sanity check.
c
          if     (kdport(l).gt.2) then
            if     (ifport(l).ne.ilport(l)) then
              if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdtf - port direction',
     &                     ' and orientation are not consistent'
              write(lp,*) 
              call flush(lp)
              endif !1st tile
              call xcstop('(latbdtf)')
                     stop '(latbdtf)'
            endif
          else
            if     (jfport(l).ne.jlport(l)) then
              if     (mnproc.eq.1) then
              write(lp,*) 
              write(lp,*) 'error in latbdtf - port direction',
     &                     ' and orientation are not consistent'
              write(lp,*) 
              call flush(lp)
              endif !1st tile
              call xcstop('(latbdtf)')
                     stop '(latbdtf)'
            endif
          endif
          if     (ifport(l).gt.ilport(l) .or.
     &            jfport(l).gt.jlport(l)     ) then
            if     (mnproc.eq.1) then
            write(lp,*) 
            write(lp,*) 'error in latbdtf - port',
     &                   ' location is not consistent'
            write(lp,*) 
            call flush(lp)
            endif !1st tile
            call xcstop('(latbdtf)')
                   stop '(latbdtf)'
          endif
        enddo !l
c
        close(unit=uoff+99)
c
c ---   check ports against masks,
c ---   mark the port locations on masks and print them out.
c
        lfatal = .false.
        do l= 1,nports
          lfatalp = .false.
c
          if     (kdport(l).eq.4) then
c
c           western port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .false.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9  !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0+1,j-j0).ne.1 .or.
     &                iu(i-i0+2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.1 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iu(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  9  !indicate an error
              else
                iu(i-i0,j-j0) = -1
              endif
              if     (iu(i-i0-1,j-j0).ne.1 .or.
     &                iu(i-i0-2,j-j0).ne.1     ) then
                lfatalp = .true.
                iu(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm .or.
     &                j.lt.3 .or. j.gt.jtdm     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9  !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0-1).ne.1 .or.
     &                iv(i-i0,j-j0-2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.lt.1 .or. i.gt.itdm   .or.
     &                j.lt.1 .or. j.gt.jtdm-2     ) then
                lfatalp = .true.
              elseif (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              elseif (iv(i-i0,j-j0).ne.0) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  9  !indicate an error
              else
                iv(i-i0,j-j0) = -1
              endif
              if     (iv(i-i0,j-j0+1).ne.1 .or.
     &                iv(i-i0,j-j0+2).ne.1     ) then
                lfatalp = .true.
                iv(i-i0,j-j0) =  7  !indicate an error
              endif
            enddo
c
          endif
c
          if     (lfatalp) then
            write(lp,*) 
            write(lp,*) 'error in latbdtf - port ',l,' mislocated',
     &                  '  (mnproc = ',mnproc,')'
            write(lp,*) 
            call flush(lp)
          endif
          lfatal = lfatal .or. lfatalp
        enddo  !l=1,nports
c
c       local lfatal to global lfatal
c
        if     (lfatal) then
          fatal = 1.0
        else
          fatal = 0.0
        endif
        call xcmaxr(fatal)
        lfatal = fatal.gt.0.5
c
c ---   write out  -iu-  and -iv- arrays, if they are not too big
c ---   data are written in strips nchar points wide
c
        if     (lfatal .or. max(itdm,jtdm).le.2*nchar) then
          util1(1:ii,1:jj) = iu(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'iu array, cols',ifrst+1,' --',ilast
            endif !1st tile
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif !1st tile
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
c
          util1(1:ii,1:jj) = iv(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'iv array, cols',ifrst+1,' --',ilast
            endif !1st tile
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif !1st tile
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
        endif  ! small region
c
        if     (lfatal) then
          write(lp,*) 
          write(lp,*) 'error in latbdtf - bad port(s)'
          write(lp,*) 
          call flush(lp)
          call xchalt('(latbdtf)')
                 stop '(latbdtf)'
        endif
c
c ---   restore iu and iv, and zero iuopn and ivopn.
c
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1,jj
          do i= 1,ii
            iu(i,j) = max( iu(i,j), 0 )
            iv(i,j) = max( iv(i,j), 0 )
          enddo
        enddo
!$OMP PARALLEL DO PRIVATE(j,i)
!$OMP&         SCHEDULE(STATIC,jblk)
        do j= 1-nbdy,jj+nbdy
          do i= 1-nbdy,ii+nbdy
            iuopn(i,j) = 0
            ivopn(i,j) = 0
          enddo
        enddo
c
c ---   define the nested boundary input mask.
c
        do j= 1,jj
          do i= 1,ii
            maskbc(i,j) = 0
          enddo
        enddo
c
        do l= 1,nports
          if     (kdport(l).eq.4) then
c
c           western port
c
            i = ifport(l)
            do j= jfport(l),jlport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            i = ifport(l)-1
            do j= jfport(l),jlport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            j = jfport(l)-1
            do i= ifport(l),ilport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            j = jfport(l)
            do i= ifport(l),ilport(l)
              if     (i.le.i0 .or. i.gt.i0+ii .or.
     &                j.le.j0 .or. j.gt.j0+jj     ) then
                cycle  ! not on this tile.
              else
                maskbc(i-i0,j-j0) = 1
              endif
            enddo
          endif
        enddo  !l=1,nports
c
        if     (ldebug_latbdtf) then
          util1(1:ii,1:jj) = maskbc(1:ii,1:jj)  ! xclget is for real arrays
          isec=(itdm-1)/nchar
          do ifrst=0,nchar*isec,nchar
            ilast=min(itdm,ifrst+nchar)
            write (char3,'(i3)') ilast-ifrst
            fmt(8:10)=char3
            if     (mnproc.eq.1) then
            write (lp,'(a,i5,a,i5)')
     &        'bc array, cols',ifrst+1,' --',ilast
            endif !1st tile
            do j= jtdm,1,-1
              call xclget(aline,ilast-ifrst, util1,ifrst+1,j,1,0, 1)
              if     (mnproc.eq.1) then
              write (lp,fmt) j,(nint(aline(i)),i=1,ilast-ifrst)
              endif !1st tile
            enddo
          enddo
          if     (mnproc.eq.1) then
          write (lp,*)
          endif
          call xcsync(flush_lp)
        endif !ldebug_latbdtf
c
c ---   initialize the ports
c
        allocate(iaub(nportpts),jaub(nportpts),
     &           iavb(nportpts),javb(nportpts),
     &           iaui(nportpts),jaui(nportpts),
     &           iavi(nportpts),javi(nportpts),
     &           iapi(nportpts),japi(nportpts) )
c
        do l= 1,nports
          if     (kdport(l).eq.4) then  !western port
            np  = nxport(l)-jfport(l)
            i   = ifport(l)
            do j= jfport(l),jlport(l)
              iaub(np+j) = i
              jaub(np+j) = j
              iaui(np+j) = i+1
              jaui(np+j) = j
              iapi(np+j) = i
              japi(np+j) = j
c ---         v not active
              iavb(np+j) = i
              javb(np+j) = j
              iavi(np+j) = i
              javi(np+j) = j
                if     (ldebug_latbdtf .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja w:',np+j,
     &              iaub(np+j),jaub(np+j),iaui(np+j),jaui(np+j)
                endif !ldebug_latbdtf
            enddo !j
          elseif (kdport(l).eq.3) then  !eastern port
            np  = nxport(l)-jfport(l)
            i   = ifport(l)-1
            do j= jfport(l),jlport(l)
              iaub(np+j) = i+1
              jaub(np+j) = j
              iaui(np+j) = i
              jaui(np+j) = j
              iapi(np+j) = i
              japi(np+j) = j
c ---         v not active
              iavb(np+j) = i
              javb(np+j) = j
              iavi(np+j) = i
              javi(np+j) = j
                if     (ldebug_latbdtf .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja e:',np+j,
     &              iaub(np+j),jaub(np+j),iaui(np+j),jaui(np+j)
                endif !ldebug_latbdtf
            enddo !j
          elseif (kdport(l).eq.1) then  !northern port
            np  = nxport(l)-ifport(l)
            j   = jfport(l)-1
            do i= ifport(l),ilport(l)
              iavb(np+i) = i
              javb(np+i) = j+1
              iavi(np+i) = i
              javi(np+i) = j
              iapi(np+i) = i
              japi(np+i) = j
c ---         u not active
              iaub(np+i) = i
              jaub(np+i) = j
              iaui(np+i) = i
              jaui(np+i) = j
                if     (ldebug_latbdtf .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja n:',np+i,
     &              iavb(np+i),javb(np+i),iavi(np+i),javi(np+i)
                endif !ldebug_latbdtf
            enddo !i
          elseif (kdport(l).eq.2) then  !southern port
            np  = nxport(l)-ifport(l)
            j   = jfport(l)
            do i= ifport(l),ilport(l)
              iavb(np+i) = i
              javb(np+i) = j
              iavi(np+i) = i
              javi(np+i) = j+1
              iapi(np+i) = i
              japi(np+i) = j
c ---         u not active
              iaub(np+i) = i
              jaub(np+i) = j
              iaui(np+i) = i
              jaui(np+i) = j
                if     (ldebug_latbdtf .and. mnproc.eq.1) then
                  write(lp,'(a,5i5)') 'n,ia,ja n:',np+i,
     &              iavb(np+i),javb(np+i),iavi(np+i),javi(np+i)
                endif !ldebug_latbdtf
            enddo !i
          endif !kdport
        enddo  !l=1,nports
c
        allocate(pspeed(nportpts),
     &            pline(nportpts),
     &            uline(nportpts),
     &            vline(nportpts),
     &            plnst(nportpts),
     &            ulnst(nportpts),
     &            vlnst(nportpts) )
c
        call xciget(pline,nportpts,depths,iapi,japi,0)
c
        do l= 1,nports
          if     (kdport(l).eq.4) then
c
c           western port
c
            np  = nxport(l)-jfport(l)
            i   = ifport(l)
            do j= jfport(l),jlport(l)
              pspeed(np+j) = qonem*sqrt(g/pline(np+j))
              if     (ldebug_latbdtf .and. mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1pe13.5)') 
     &          'w port: ',l,i,j,pspeed(np+j)
              endif !ldebug_latbdtf
c
              if     (i.ge.i0+ 1-nbdy .and.
     &                i.le.i0+ii+nbdy .and.
     &                j.ge.j0+ 1-nbdy .and.
     &                j.le.j0+jj+nbdy      ) then
                iuopn(i-i0,j-j0) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.3) then
c
c           eastern port
c
            np  = nxport(l)-jfport(l)
            i   = ifport(l)-1
            do j= jfport(l),jlport(l)
              pspeed(np+j) = qonem*sqrt(g/pline(np+j))
              if     (ldebug_latbdtf .and. mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1pe13.5)') 
     &          'e port: ',l,i,j,pspeed(np+j)
              endif !ldebug_latbdtf
c
              if     (i+1.ge.i0+ 1-nbdy .and.
     &                i+1.le.i0+ii+nbdy .and.
     &                j  .ge.j0+ 1-nbdy .and.
     &                j  .le.j0+jj+nbdy      ) then
                iuopn(i-i0+1,j-j0) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.1) then
c
c           northern port
c
            np  = nxport(l)-ifport(l)
            j   = jfport(l)-1
            do i= ifport(l),ilport(l)
              pspeed(np+i) = qonem*sqrt(g/pline(np+i))
              if     (ldebug_latbdtf .and. mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1pe13.5)') 
     &          'n port: ',l,i,j,pspeed(np+i)
              endif !ldebug_latbdtf
c
              if     (i  .ge.i0+ 1-nbdy .and.
     &                i  .le.i0+ii+nbdy .and.
     &                j+1.ge.j0+ 1-nbdy .and.
     &                j+1.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0+1) = 1
              endif
            enddo
c
          elseif (kdport(l).eq.2) then
c
c           southern port
c
            np  = nxport(l)-ifport(l)
            j   = jfport(l)
            do i= ifport(l),ilport(l)
              pspeed(np+i) = qonem*sqrt(g/pline(np+i))
              if     (ldebug_latbdtf .and. mnproc.eq.1) then
              write(lp,'(a,i2,2i5,1pe13.5)') 
     &          's port: ',l,i,j,pspeed(np+i)
              endif !ldebug_latbdtf
c
              if     (i.ge.i0+ 1-nbdy .and.
     &                i.le.i0+ii+nbdy .and.
     &                j.ge.j0+ 1-nbdy .and.
     &                j.le.j0+jj+nbdy      ) then
                ivopn(i-i0,j-j0) = 1
              endif
            enddo
c
          endif !kdport
        enddo  !l=1,nports
        if     (ldebug_latbdtf .and. mnproc.eq.1) then
        write(lp,*) 
        call flush(lp)
        endif !ldebug_latbdtf

        if     (tidflg.eq.1 .or. tidflg.eq.3) then
c ---     last index is 1:3 for z,u,v
          allocate(port_tide(nportpts,3))
          allocate( z_A(     nportpts  ))
          allocate( z_R(ncon,nportpts,3),z_I(ncon,nportpts,3))
c
c ---     read in Z, u & v tidal component at open boundary ports
c
          call latbd_tide(z_A,z_R,z_I,nportpts)
        endif !tidflg
c
c       end of initialization
c
        call xcsync(flush_lp)
        return
      endif
c
c --- nested input only required on first barotropic time step.
c
      if     (lll.eq.1) then
        do j= 1,jj
          do i= 1,ii
            if     (maskbc(i,j).eq.1) then
              util1(i,j) = ubnest(i,j,lb0)*wb0+ubnest(i,j,lb1)*wb1
              util2(i,j) = vbnest(i,j,lb0)*wb0+vbnest(i,j,lb1)*wb1
              util3(i,j) = pbnest(i,j,lb0)*wb0+pbnest(i,j,lb1)*wb1
            endif
          enddo
        enddo
c
        call xciget(ulnst,nportpts,util1,iaub,jaub,mnp)
        call xciget(vlnst,nportpts,util2,iavb,javb,mnp)
        call xciget(plnst,nportpts,util3,iapi,japi,mnp)
c
        if     (tidflg.eq.1 .or. tidflg.eq.3) then
          d_time = time_8 + lll*dlt/86400.d0
          call tides_ports(d_time,nportpts,z_R,z_I,z_A, port_tide)
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do i= 1,nportpts
              plnst(i)=plnst(i)+  onem*port_tide(i,1)
              ulnst(i)=ulnst(i)+0.01d0*port_tide(i,2)
              vlnst(i)=vlnst(i)+0.01d0*port_tide(i,3)
            enddo !i
          endif !mnp
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              do l= 1,nports
                i=nxport(l)-1 + lnport(l)/2
                if     (kdport(l).eq.4) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'W z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                elseif (kdport(l).eq.3) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'E z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                elseif (kdport(l).eq.1) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'N z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                elseif (kdport(l).eq.2) then
                  write(lp,'(a,i4,a,i4,a,f12.4,3f14.5)')
     &               'S z,u,v at (',iapi(i),',',japi(i),')=',
     &               d_time,(port_tide(i,k),k=1,3)
                endif !kdport
              enddo  !l=1,nports
            endif !ldebug_latbdtf
        endif !tidflg
      endif !lll.eq.1
c
      call xciget(uline,nportpts,ubavg(1-nbdy,1-nbdy,n),iaui,jaui,mnp)
      call xciget(vline,nportpts,vbavg(1-nbdy,1-nbdy,n),iavi,javi,mnp)
      call xciget(pline,nportpts,pbavg(1-nbdy,1-nbdy,n),iapi,japi,mnp)
c
c --- 'wellposed' treatment of pressure and velocity fields.
c
      do l= 1,nports
c
        if     (kdport(l).eq.4) then
c
c         western port
c
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              j=jfport(l)-1 + lnport(l)/2
              i=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 'w port, uline:',j,uline(i)
              write(lp,'(a,i4,e14.5)') 'w port, pline:',j,pline(i)*qonem
              write(lp,'(a,i4,e14.5)') 'w port, plnst:',j,plnst(i)*qonem
              write(lp,'(a,i4,e14.5)') 'w port, ulnst:',j,ulnst(i)
            endif !1st tile
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do j= nxport(l),nxport(l)-1+lnport(l)
              pline(j)=w_f*pline(j)+w_c*plnst(j)
              uline(j)=ulnst(j)+pspeed(j)*(plnst(j)-pline(j))  !new pline
            enddo !j
          endif !mnp
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              j=jfport(l)-1 + lnport(l)/2
              i=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 'e port, pbavg:',j,pline(i)*qonem
              write(lp,'(a,i4,e14.5)') 'e port, ubavg:',j,uline(i)
              write(lp,*)
              call flush(lp)
            endif !1st tile
c
        elseif (kdport(l).eq.3) then
c
c         eastern port
c
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              j=jfport(l)-1 + lnport(l)/2
              i=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 'e port, uline:',j,uline(i)
              write(lp,'(a,i4,e14.5)') 'e port, pline:',j,pline(i)*qonem
              write(lp,'(a,i4,e14.5)') 'e port, plnst:',j,plnst(i)*qonem
              write(lp,'(a,i4,e14.5)') 'e port, ulnst:',j,ulnst(i)
            endif !1st tile
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do j= nxport(l),nxport(l)-1+lnport(l)
              pline(j)=w_f*pline(j)+w_c*plnst(j)
              uline(j)=ulnst(j)+pspeed(j)*(pline(j)-plnst(j))  !new pline
            enddo
          endif !mnp
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              j=jfport(l)-1 + lnport(l)/2
              i=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 'e port, pbavg:',j,pline(i)*qonem
              write(lp,'(a,i4,e14.5)') 'e port, ubavg:',j,uline(i)
              write(lp,*)
              call flush(lp)
            endif !1st tile
c
        elseif (kdport(l).eq.1) then
c
c         northern port
c
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              i=ifport(l)-1 + lnport(l)/2
              j=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 'n port, vline:',i,vline(j)
              write(lp,'(a,i4,e14.5)') 'n port, pline:',i,pline(j)*qonem
              write(lp,'(a,i4,e14.5)') 'n port, plnst:',i,plnst(j)*qonem
              write(lp,'(a,i4,e14.5)') 'n port, vlnst:',i,vlnst(j)
            endif !ldebug_latbdtf
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do i= nxport(l),nxport(l)-1+lnport(l)
              pline(i)=w_f*pline(i)+w_c*plnst(i)
              vline(i)=vlnst(i)+pspeed(i)*(pline(i)-plnst(i))  !new pline
            enddo
          endif !mnp
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              i=ifport(l)-1 + lnport(l)/2
              j=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 'n port, pbavg:',i,pline(j)*qonem
              write(lp,'(a,i4,e14.5)') 'n port, vbavg:',i,vline(j)
              write(lp,*)
              call flush(lp)
            endif !ldebug_latbdtf
c
        elseif (kdport(l).eq.2) then
c
c         southern port
c
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              i=ifport(l)-1 + lnport(l)/2
              j=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 's port, vline:',i,vline(j)
              write(lp,'(a,i4,e14.5)') 's port, pline:',i,pline(j)*qonem
              write(lp,'(a,i4,e14.5)') 's port, plnst:',i,plnst(j)*qonem
              write(lp,'(a,i4,e14.5)') 's port, vlnst:',i,vlnst(j)
            endif !ldebug_latbdtf
          if     (mnp.eq.0 .or. mnp.eq.mnproc) then
            do i= nxport(l),nxport(l)-1+lnport(l)
              pline(i)=w_f*pline(i)+w_c*plnst(i)
              vline(i)=vlnst(i)+pspeed(i)*(plnst(i)-pline(i))  !new pline
            enddo
          endif !mnp
            if     (ldebug_latbdtf .and. mnproc.eq.max(mnp,1)) then
              i=ifport(l)-1 + lnport(l)/2
              j=nxport(l)-1 + lnport(l)/2
              write(lp,'(a,i4,e14.5)') 's port, pbavg:',i,pline(j)*qonem
              write(lp,'(a,i4,e14.5)') 's port, vbavg:',i,vline(j)
              write(lp,*)
              call flush(lp)
            endif !ldebug_latbdtf
c
        endif !kdport
c
      enddo  !l=1,nports
c
c --- update the boundary points.
c --- the same point can appear multiple times and only the last
c --- appearence has effect.  this is ok because they are all the same.
c --- for each point, one of u and v will be unchanged (i.e. updated 
c --- with its existing value).
c
      if     (w_f.ne.1.0) then
        call xciput(pline,nportpts,pbavg(1-nbdy,1-nbdy,n),iapi,japi,mnp)
      endif !clamped
      call xciput(uline,nportpts,ubavg(1-nbdy,1-nbdy,n),iaub,jaub,mnp)
      call xciput(vline,nportpts,vbavg(1-nbdy,1-nbdy,n),iavb,javb,mnp)
c
      return
      end subroutine latbdtf

      subroutine latbd_tide(z_A,z_R,z_I,nportpts)
      use mod_xc         ! HYCOM communication interface
      use mod_cb_arrays  ! HYCOM saved arrays
      use mod_tides      ! HYCOM Tidal Data
      implicit none
c
      integer nportpts
      real    z_A(nportpts),z_R(ncon,nportpts,3),z_I(ncon,nportpts,3)
c
c     read in Z, u & v tidal component at open boundary ports
c
c     These are generated using OTPSnc (or OTPS2) extract_HC
c       http://volkov.oce.orst.edu/tides/otps.html
c       http://volkov.oce.orst.edu/tides/tpxo8_atlas.html
c
      logical, parameter :: ldebug_latbd_t=.false.  !usually .false.
c
      integer       i,j
      real          dum1,dum2
      logical       Tide_Modes_Correct
      character*156 Tide_Line
c
      Character*2  Tide_Names(8)
      save         Tide_Names
      data         Tide_Names/'m2','s2','k1','o1','n2','p1','k2','q1'/
c
c       Default is zero
c
        z_R(:,:,:) = 0.0
        z_I(:,:,:) = 0.0
c
c       Read Pang (angle of xward wrt eward) for each port point
c
        open(unit=uoff+99,file=trim(flnminp)//'ports_a.input')
        read(uoff+99,'(a)')Tide_Line
        if     (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !ldebug_latbd_t
        if     (Tide_Line(1:25).ne.
     &    '    Lat     Lon  |   Pang') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_a.input'
          write(lp,*) 'Initial Line not correct!'
          write(lp,*) 'Expecting:'//
     & '    Lat     Lon  |  Pang'
          write(lp,*) '      Got:'//Tide_Line(1:25)
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !1st line
        do i= 1, nportpts
          read(uoff+99,'(2F9.3,F11.6)')dum1,dum2,z_A(i)
          if     (ldebug_latbd_t .and. mnproc.eq.1) then
            write(6,'(a,i4,a,F11.6)')
     &                         'Port Point ',i,'  pang= ',z_A(i)
          endif !ldebug_latbd_t
        enddo !i
        close(unit=uoff+99)
        if     (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,*)'Pang read for',nportpts,' open boundary points'
        endif !ldebug_latbd_t
c
c       Read z tidal components (real, imag) for each port point
c
        open(unit=uoff+99,file=trim(flnminp)//'ports_z.input')
        read(uoff+99,'(a)')Tide_Line
        if     (mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !1st tile
        read(uoff+99,'(a)')Tide_Line    
        if     (mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !1st tile
        if     (Tide_Line(1:15).ne.' Elevations (m)') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_z.input'
          write(lp,*) 'Second Line not correct!'
          write(lp,*) 'Expecting: Elevations (m)'
          write(lp,*) '      Got:'//Tide_Line(1:15)
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !2nd line
        read(uoff+99,'(a)')Tide_Line
        if     (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !ldebug_latbd_t
c ---   always have an OTPSnc header, data can be from OTPS2
        if     (Tide_Line(1:50).ne.
     &    '    Lat     Lon  |   m2_Re   m2_Im   s2_Re   s2_Im') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_z.input'
          write(lp,*) 'Third Line not correct!'
          write(lp,*) 'Expecting:'//
     & '    Lat     Lon  |   m2_Re   m2_Im   s2_Re   s2_Im'
          write(lp,*) '      Got:'//Tide_Line(1:50)
          if     (Tide_Line(22:27).eq.'m2_amp') then
           write(lp,*)'ports_z.input file is in Amplitude, Phase Mode!' 
          endif
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !3rd line
        Tide_Modes_Correct=.true.
        do i=1,8
          Tide_Modes_Correct=Tide_Modes_Correct .and.
     &      Tide_Line( 6+16*i:10+16*i).eq.Tide_Names(i)//'_Re' .and.
     &      Tide_Line(14+16*i:18+16*i).eq.Tide_Names(i)//'_Im'     
        enddo
        if     (.not.Tide_Modes_Correct) then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_z.input'
          write(lp,*) 'Tidal Modes may be in wrong order!'
          write(lp,*) 'Expecting: m2,s2,k1,o1,n2,p1,k2,q1'
          write(lp,*) '      Got: ',(Tide_Line(6+16*i:8+16*i),i=1,8)
          if     (Tide_Line(22:27).eq.'m2_amp') then
            write(lp,*)
     &        'ports_z.input file is in Amplitude, Phase Mode!' 
          endif
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !.not.Tide_Modes_Correct
        do i= 1, nportpts
          read(uoff+99,'(a)')Tide_Line
          if     (index(Tide_Line,'Site').eq.0) then
            if     (len_trim(Tide_Line).eq.147) then
c ---         from OTPS2
              read(Tide_Line,'(F9.4,F10.4,16F8.3)')dum1,dum2,
     &                   (z_R(j,i,1),z_I(j,i,1),j=1,ncon) 
            else
c ---         from OTPSnc
              read(Tide_Line,'(2F9.3,16F8.3)')dum1,dum2,
     &                   (z_R(j,i,1),z_I(j,i,1),j=1,ncon) 
            endif
          elseif (mnproc.eq.1) then
            write(lp,'(a,i5,a / a)')
     &        'WARNING: port location',i,' treated as all zeros',
     &        trim(Tide_Line)
          endif
          if     (ldebug_latbd_t .and. mnproc.eq.1) then
            write(6,'(a,i4,a,8F8.3/23x,8F8.3)')
     &                         'Port Point ',i,'  z(j)= ',
     &                          (z_R(j,i,1),j=1,ncon),
     &                          (z_I(j,i,1),j=1,ncon)
          endif !ldebug_latbd_t
        enddo !i
        close(unit=uoff+99)
        if     (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,'(18a)')'z tidal Modes:',
     &       (' '//Tide_Line(14+8*i:17+8*i),i=1,16)
         write(lp,*)'read for',nportpts,' open boundary points'
        endif !ldebug_latbd_t
c
c       Read u tidal components (real, imag) for each port point

        open(unit=uoff+99,file=trim(flnminp)//'ports_u.input')
        read(uoff+99,'(a)')Tide_Line
        if     (mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !1st tile
        read(uoff+99,'(a)')Tide_Line    
        if     (mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !1st tile
        if   (Tide_Line(1:20).ne.' WE velocity  (cm/s)') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_u.input'
          write(lp,*) 'Second Line not correct!'
          write(lp,*) 'Expecting: WE velocity  (cm/s)'
          write(lp,*) '      Got:'//Tide_Line(1:20)
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !2nd line
        read(uoff+99,'(a)')Tide_Line
        if     (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !ldebug_latbd_t
c ---   always have an OTPSnc header, data can be from OTPS2
        if     (Tide_Line(1:50).ne.
     &    '    Lat     Lon  |   m2_Re   m2_Im   s2_Re   s2_Im') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_u.input'
          write(lp,*) 'Third Line not correct!'
          write(lp,*) 'Expecting:'//
     &      '    Lat     Lon  |   m2_Re   m2_Im   s2_Re   s2_Im'
          write(lp,*) '      Got:'//Tide_Line(1:50)
          if     (Tide_Line(22:27).eq.'m2_amp') then
            write(lp,*)'ports_u.input file is in Amplitude, Phase Mode?'
            write(lp,*)'Expecting mode_Re  mode_Im  data' 
          endif
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !3rd line
        Tide_Modes_Correct=.true.
        do i=1,8
          Tide_Modes_Correct=Tide_Modes_Correct .and.
     &      Tide_Line( 6+16*i:10+16*i).eq.Tide_Names(i)//'_Re' .and.
     &      Tide_Line(14+16*i:18+16*i).eq.Tide_Names(i)//'_Im'     
        enddo !i
        if     (.not.Tide_Modes_Correct) then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_u.input'
          write(lp,*) 'Tidal Modes may be in wrong order!'
          write(lp,*) 'Expecting: m2,s2,k1,o1,n2,p1,k2,q1'
          write(lp,*) '      Got: ',(Tide_Line(6+16*i:8+16*i),i=1,8)
          if     (Tide_Line(22:27).eq.'m2_amp') then
            write(lp,*)
     &        'ports_u.input file is in Amplitude, Phase Mode!' 
            write(lp,*)
     &        'Expecting mode_Re  mode_Im  data' 
          endif
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !.not.Tide_Modes_Correct
        do i= 1,nportpts
          read(uoff+99,'(a)')Tide_Line
          if     (index(Tide_Line,'Site').eq.0) then
            if     (len_trim(Tide_Line).eq.147) then
c ---         from OTPS2
              read(Tide_Line,'(F9.4,F10.4,16F8.3)')dum1,dum2,
     &                   (z_R(j,i,2),z_I(j,i,2),j=1,ncon) 
            else
c ---         from OTPSnc
              read(Tide_Line,'(2F9.3,16F8.3)')dum1,dum2,
     &                   (z_R(j,i,2),z_I(j,i,2),j=1,ncon) 
            endif
          elseif (mnproc.eq.1) then
            write(lp,'(a,i5,a / a)')
     &        'WARNING: port location',i,' treated as all zeros',
     &        trim(Tide_Line)
          endif
          if     (ldebug_latbd_t .and. mnproc.eq.1) then
            write(6,'(a,i4,a,8F8.3/23x,8F8.3)')
     &                         'Port Point ',i,'  z(j)= ',
     &                          (z_R(j,i,2),j=1,ncon),
     &                          (z_I(j,i,2),j=1,ncon)
          endif !ldebug_latbd_t
        enddo !i
        close(unit=uoff+99)
        if (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,'(18a)')'u tidal Modes:',
     &          (' '//Tide_Line(14+8*i:17+8*i),i=1,16)
          write(lp,*)'read for',nportpts,' open boundary points'
        endif !ldebug_latbd_t
c
c       Read v tidal components (real, imag) for each port point
c
        open(unit=uoff+99,file=trim(flnminp)//'ports_v.input')
        read(uoff+99,'(a)')Tide_Line
        if     (mnproc.eq.1) then
          write(lp,'(a /)') trim(Tide_Line)
        endif !1st tile
        read(uoff+99,'(a)')Tide_Line    
        if     (mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !1st tile
        if     (Tide_Line(1:20).ne.' SN velocity  (cm/s)') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_v.input'
          write(lp,*) 'Second Line not correct!'
          write(lp,*) 'Expecting: SN velocity  (cm/s)'
          write(lp,*) '      Got:'//Tide_Line(1:20)
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !2nd line
        read(uoff+99,'(a)')Tide_Line
        if     (ldebug_latbd_t .and. mnproc.eq.1) then
          write(lp,'(a)') trim(Tide_Line)
        endif !ldebug_latbd_t
c ---   always have an OTPSnc header, data can be from OTPS2
        if     (Tide_Line(1:50).ne.
     &    '    Lat     Lon  |   m2_Re   m2_Im   s2_Re   s2_Im') then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_v.input'
          write(lp,*) 'Third Line not correct!'
          write(lp,*) 'Expecting:'//
     &      '    Lat     Lon  |   m2_Re   m2_Im   s2_Re   s2_Im'
          write(lp,*) '      Got:'//Tide_Line(1:50)
          if     (Tide_Line(22:27).eq.'m2_amp') then
            write(lp,*)'ports_v.input file is in Amplitude, Phase Mode?'
            write(lp,*)'Expecting mode_Re  mode_Im  data' 
          endif
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !3rd line
c       write(6,*)'Modes = ',('/'//Tide_Line(14+8*i:18+8*i),i=1,16)
        Tide_Modes_Correct=.true.
        do i= 1,8
          Tide_Modes_Correct=Tide_Modes_Correct .and.
     &      Tide_Line( 6+16*i:10+16*i).eq.Tide_Names(i)//'_Re' .and.
     &      Tide_Line(14+16*i:18+16*i).eq.Tide_Names(i)//'_Im'     
        enddo !i
        if     (.not.Tide_Modes_Correct) then
          if     (mnproc.eq.1) then
          write(lp,*) 
          write(lp,*) 'error in latbd_tide - ports_v.input'
          write(lp,*) 'Tidal Modes may be in wrong order!'
          write(lp,*) 'Expecting: m2,s2,k1,o1,n2,p1,k2,q1'
          write(lp,*) '      Got: ',(Tide_Line(6+16*i:8+16*i),i=1,8)
          if     (Tide_Line(22:27).eq.'m2_amp') then
            write(lp,*)
     &        'ports_v.input file is in Amplitude, Phase Mode!' 
            write(lp,*)
     &        'Expecting mode_Re  mode_Im  data' 
          endif
          write(lp,*) 
          call flush(lp)
          endif !1st tile
          call xcstop('(latbd_tide)')
                 stop '(latbd_tide)'
        endif !.not.Tide_Modes_Correct
        do i= 1,nportpts
          read(uoff+99,'(a)')Tide_Line
          if     (index(Tide_Line,'Site').eq.0) then
            if     (len_trim(Tide_Line).eq.147) then
c ---         from OTPS2
              read(Tide_Line,'(F9.4,F10.4,16F8.3)')dum1,dum2,
     &                   (z_R(j,i,3),z_I(j,i,3),j=1,ncon) 
            else
c ---         from OTPSnc
              read(Tide_Line,'(2F9.3,16F8.3)')dum1,dum2,
     &                   (z_R(j,i,3),z_I(j,i,3),j=1,ncon) 
            endif
          elseif (mnproc.eq.1) then
            write(lp,'(a,i5,a / a)')
     &        'WARNING: port location',i,' treated as all zeros',
     &        trim(Tide_Line)
          endif
          if     (ldebug_latbd_t .and. mnproc.eq.1) then
            write(6,'(a,i4,a,8F8.3/23x,8F8.3)')
     &                         'Port Point ',i,'  z(j)= ',
     &                          (z_R(j,i,3),j=1,ncon),
     &                          (z_I(j,i,3),j=1,ncon)
          endif !ldebug_latbd_t
        enddo !i
        close(unit=uoff+99)
        if     (mnproc.eq.1 .and. ldebug_latbd_t) then
          write(lp,'(18a)')'v tidal Modes:',
     &          (' '//Tide_Line(14+8*i:17+8*i),i=1,16)
          write(lp,*)'read for',nportpts,' open boundary points'
        endif !ldebug_latbd_t
c
      return
      end subroutine latbd_tide
c
c
c> Revision history:
c>
c> Mar. 2004 -- fixed bug in latbdp's speed calculation 
c> Nov. 2006 -- added latbdf
c> Aug. 2011 -- tidal response at ports added
c> Mar. 2012 -- added latbdtf
c> Mar. 2012 -- replaced speed[nsew] with pspeed, so that ports can "overlap"
c  Mar. 2012 -- added mnp to select mnflg for xclegt and xclput
c> Apr. 2012 -- added latbd_tide
c> Apr. 2012 -- replaced xclget and xclput with xciget and xciput in latbdtf
c> June 2012 -- replaced xclget and xclput with xciget and xciput in latbdt
c> June 2012 -- removed parameter mports, arrays are allocated at run time
c> June 2012 -- fixed bnest interpolation bug, use lb[01] not ln[01]
c> Mar. 2013 -- latbd_tide: added support for OTPS2 data with OTPSnc headers
c> June 2013 -- added   latbdtc for clammped velocity obc.
c> May  2014 -- removed latbdtc
