
c/* BARRIER       set a barrier; for SPMD versions      */
#if defined(MPI)
# define BARRIER call mpi_barrier(mpi_comm_hycom,mpierr)
#if defined(USE_SCOUPLER)
# define BARRIER call mpi_barrier(mpi_comm_ocean,mpierr)
#endif
#elif defined(SHMEM)
# define BARRIER call shmem_barrier_all()
#endif

c/* BARRIER_MP    halo synchronization; SHMEM only      */
c/* BARRIER_NP    halo synchronization; SHMEM only      */
#if defined(RINGB)
#define BARRIER_MP call xctbar(idproc(mproc-1,nproc),idproc(mproc+1,nproc))
#define BARRIER_NP call xctbar(idproc(mproc,nproc-1),idproc(mproc,nproc+1))
#else
#define BARRIER_MP BARRIER
#define BARRIER_NP BARRIER
#endif

#if defined(MPI)
c/* #define MPISR */
c/* MTYPE4        mpi type for real*4                   */
c/* MTYPER        mpi type for real                     */
c/* MTYPED        mpi type for real*8                   */
c/* MTYPEI        mpi type for integer                  */
c/* MPI_SEND      either mpi_send  or mpi_ssend         */
c/* MPI_ISEND     either mpi_isend or mpi_issend        */
#if defined(NOMPIR8) /* LAM does not support mpi_real[48] */
#if defined(REAL4)
# define MTYPE4 mpi_real
# define MTYPER mpi_real
# define MTYPED mpi_double_precision
# define MTYPEI mpi_integer
#else /* REAL8 */
# define MTYPE4 mpi_real
# define MTYPER mpi_double_precision
# define MTYPED mpi_double_precision
# define MTYPEI mpi_integer
#endif
#else /* most MPI's allow mpi_real[48] */
#if defined(REAL4)
# define MTYPE4 mpi_real4
# define MTYPER mpi_real4
# define MTYPED mpi_real8
# define MTYPEI mpi_integer
#else /* REAL8 */
# define MTYPE4 mpi_real4
# define MTYPER mpi_real8
# define MTYPED mpi_real8
# define MTYPEI mpi_integer
#endif
#endif
#if defined(SSEND)
# define MPI_SEND mpi_ssend
# define MPI_ISEND mpi_issend
#else
# define MPI_SEND mpi_send
# define MPI_ISEND mpi_isend
#endif /* SSEND:else */
#endif /* MPI */

#if defined(SHMEM)
c/* SHMEM_GET4    get real*4  variables                 */
c/* SHMEM_GETR    get real    variables                 */
c/* SHMEM_GETD    get real*8  variables                 */
c/* SHMEM_GETI    get integer variables                 */
c/* SHMEM_MYPE    return number of this PE (0...npes-1) */
c/* SHMEM_NPES    return number of PEs                  */
#if defined(REAL4)
# define SHMEM_GET4 shmem_get32
# define SHMEM_GETR shmem_get32
# define SHMEM_GETD shmem_get64
# define SHMEM_GETI shmem_integer_get
#else /* REAL8 */
# define SHMEM_GET4 shmem_get32
# define SHMEM_GETR shmem_get64
# define SHMEM_GETD shmem_get64
# define SHMEM_GETI shmem_integer_get
#endif
# define SHMEM_MYPE shmem_my_pe
# define SHMEM_NPES shmem_n_pes
#endif /* SHMEM */

c
c-----------------------------------------------------------------------
c
c     auxillary routines that involve off-processor communication.
c     message passing version, contained in module mod_xc.
c
c     author:  Alan J. Wallcraft,  NRL.
c
c-----------------------------------------------------------------------
c
      subroutine xcaget(aa, a, mnflg)
      implicit none
c
      real,    intent(out)   :: aa(itdm,jtdm)
      real,    intent(in)    :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) convert an entire 2-D array from tiled to non-tiled layout.
c
c  3) mnflg selects which nodes must return the array
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa              real           output    non-tiled target array
c    a               real           input     tiled source array
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer i,j,mp,np,mnp
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
#endif
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 1)
        nxc = 1
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(al)) then
        allocate(       al(itdm,jdm) )
        call mem_stat_add( itdm*jdm )
        allocate(       at(idm*jdm) )
        call mem_stat_add( idm*jdm )
      endif
#endif
c
c     gather each row of tiles onto the first tile in the row.
c
#if defined(MPI)
      if     (mproc.eq.mpe_1(nproc)) then
        do j= 1,jj
          do i= 1,i0
            al(i,j) = vland
          enddo
          do i= 1,ii
            al(i0+i,j) = a(i,j)
          enddo
          do i= i0+ii+1,itdm
            al(i,j) = vland
          enddo
        enddo
        do mp= mpe_1(nproc)+1,mpe_e(nproc)
          call MPI_RECV(at,ii_pe(mp,nproc)*jj,MTYPER,
     &                  idproc(mp,nproc), 9941,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          do j= 1,jj
            do i= 1,ii_pe(mp,nproc)
              al(i0_pe(mp,nproc)+i,j) = at(i+(j-1)*ii_pe(mp,nproc))
            enddo
          enddo
        enddo
      else  !mproc>1
        do j= 1,jj
          do i= 1,ii
            at(i+(j-1)*ii) = a(i,j)
          enddo
        enddo
        call MPI_SEND(at,ii*jj,MTYPER,
     &                idproc(mpe_1(nproc),nproc), 9941,
     &                mpi_comm_hycom_, mpierr)
      endif
#elif defined(SHMEM)
      do j= 1,jj
        do i= 1,ii
          at(i+(j-1)*ii) = a(i,j)
        enddo
      enddo
      BARRIER
      if     (mproc.eq.mpe_1(nproc)) then
        do j= 1,jj
          do i= 1,i0
            al(i,j) = vland
          enddo
          do i= 1,ii
            al(i0+i,j) = a(i,j)
          enddo
          do i= i0+ii+1,itdm
            al(i,j) = vland
          enddo
        enddo
        do mp= mpe_1(nproc)+1,mpe_e(nproc)
          call SHMEM_GETR(at,
     &                    at,ii_pe(mp,nproc)*jj, idproc(mp,nproc))
          do j= 1,jj
            do i= 1,ii_pe(mp,nproc)
              al(i0_pe(mp,nproc)+i,j) = at(i+(j-1)*ii_pe(mp,nproc))
            enddo
          enddo
        enddo
      endif
      BARRIER
#endif /* MPI:SHMEM */
c
c     gather each row of tiles onto the output array.
c
#if defined(MPI)
      mnp = max(mnflg,1)
c
      if     (mnproc.eq.mnp) then
        if (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,itdm
              aa(i,j+j0) = al(i,j)
            enddo
          enddo
        endif
        do np= 1,jpr
          mp = mpe_1(np)
          if     (idproc(mp,np).ne.idproc(mproc,nproc)) then
            call MPI_RECV(al,itdm*jj_pe(mp,np),MTYPER,
     &                    idproc(mp,np), 9942,
     &                    mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
            do j= 1,jj_pe(mp,np)
              do i= 1,itdm
                aa(i,j+j0_pe(mp,np)) = al(i,j)
              enddo
            enddo
          endif
        enddo
      elseif (mproc.eq.mpe_1(nproc)) then
        call MPI_SEND(al,itdm*jj,MTYPER,
     &                idproc1(mnp), 9942,
     &                mpi_comm_hycom_, mpierr)
      endif
c
      if     (mnflg.eq.0) then
        call mpi_bcast(aa,itdm*jtdm,MTYPER,
     &                 idproc1(1),mpi_comm_hycom_,mpierr)
      endif
#elif defined(SHMEM)
      if     (mnflg.eq.0 .or. mnproc.eq.mnflg) then
        if (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,itdm
              aa(i,j+j0) = al(i,j)
            enddo
          enddo
        endif
        do np= 1,jpr
          mp = mpe_1(np)
          if     (idproc(mp,np).ne.idproc(mproc,nproc)) then
            call SHMEM_GETR(al,
     &                      al,itdm*jj_pe(mp,np), idproc(mp,np))
            do j= 1,jj_pe(mp,np)
              do i= 1,itdm
                aa(i,j+j0_pe(mp,np)) = al(i,j)
              enddo
            enddo
          endif
        enddo
      endif
      ! no barrier needed here because of double buffering (at and then al)
#endif /* MPI:SHMEM */
#if defined(TIMER)
c
      if     (nxc.eq. 1) then
        call xctmr1( 1)
        nxc = 0
      endif
#endif
      return
      end subroutine xcaget

      subroutine xcaget4(aa, a, mnflg)
      implicit none
c
      real*4,  intent(out)   :: aa(itdm,*)
      real*4,  intent(in)    :: a(ii,jj)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) convert an entire 2-D array from tiled to non-tiled layout.
c
c  2) Special version for zaiord and zaiowr only.
c     arrays are real*4 and tiled array has no halo.
c
c  3) mnflg selects which nodes must return the array
c        =-1; first node in each row returns that row
c        = n; node number n (mnproc=n)
c
c  4) The size of aa depends on mnflg
c      =-1; aa needed on 1st node in each row, dimension (itdm,jdm)
c      = n; aa needed on nth node,             dimension (itdm,jtdm)
c
c  5) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa              real           output    non-tiled target array
c    a               real           input     tiled source array
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer i,j,mp,np,mnp
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 1)
        nxc = 1
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(at4)) then
        if     (mproc.eq.mpe_1(nproc)) then
          allocate(       al4(itdm,jdm) )
          allocate(      alt4(idm*jdm,ipr) )
          call mem_stat_add( (itdm*jdm)/2     ) !real*4, so /2
          call mem_stat_add( (idm *jdm*jpr)/2 ) !real*4, so /2
        endif !first tile in the row
        allocate(       at4(idm*jdm) )
        call mem_stat_add( (idm*jdm) /2 ) !real*4, so /2
      endif
#endif
c
c     gather each row of tiles onto the first tile in the row.
c
#if defined(MPI)
      if     (mproc.eq.mpe_1(nproc)) then
        do j= 1,jj
          do i= 1,i0
            al4(i,j) = vland4
          enddo
          do i= 1,ii
            al4(i0+i,j) = a(i,j)
          enddo
          do i= i0+ii+1,itdm
            al4(i,j) = vland4
          enddo
        enddo
        do mp= mpe_1(nproc)+1,mpe_e(nproc)
          call MPI_RECV(at4,ii_pe(mp,nproc)*jj,MTYPE4,
     &                  idproc(mp,nproc), 9941,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          do j= 1,jj
            do i= 1,ii_pe(mp,nproc)
              al4(i0_pe(mp,nproc)+i,j) = at4(i+(j-1)*ii_pe(mp,nproc))
            enddo
          enddo
        enddo
      else  !mproc>1
        call MPI_SEND(a,ii*jj,MTYPE4,
     &                idproc(mpe_1(nproc),nproc), 9941,
     &                mpi_comm_hycom_, mpierr)
      endif
#elif defined(SHMEM)
      do j= 1,jj
        do i= 1,ii
          at4(i+(j-1)*ii) = a(i,j)
        enddo
      enddo
      BARRIER
      if     (mproc.eq.mpe_1(nproc)) then
        do j= 1,jj
          do i= 1,i0
            al4(i,j) = vland4
          enddo
          do i= 1,ii
            al4(i0+i,j) = a(i,j)
          enddo
          do i= i0+ii+1,itdm
            al4(i,j) = vland4
          enddo
        enddo
        do mp= mpe_1(nproc)+1,mpe_e(nproc)
          call SHMEM_GET4(at4,
     &                    at4,ii_pe(mp,nproc)*jj, idproc(mp,nproc))
          do j= 1,jj
            do i= 1,ii_pe(mp,nproc)
              al4(i0_pe(mp,nproc)+i,j) = at4(i+(j-1)*ii_pe(mp,nproc))
            enddo
          enddo
        enddo
      endif
      BARRIER
#endif /* MPI:SHMEM */
      if     (mnflg.eq.-1) then
c
c       we are essentially done.
c
        if (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,itdm
              aa(i,j) = al4(i,j)
            enddo
          enddo  
        endif  
      else !mnflg.ne.-1
c
c     gather each row of tiles onto the output array.
c
#if defined(MPI)
      mnp = max(mnflg,1)
c
      if     (mnproc.eq.mnp) then
        if (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,itdm
              aa(i,j+j0) = al4(i,j)
            enddo
          enddo
        endif
        do np= 1,jpr
          mp = mpe_1(np)
          if     (idproc(mp,np).ne.idproc(mproc,nproc)) then
            call MPI_RECV(al4,itdm*jj_pe(mp,np),MTYPE4,
     &                    idproc(mp,np), 9942,
     &                    mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
            do j= 1,jj_pe(mp,np)
              do i= 1,itdm
                aa(i,j+j0_pe(mp,np)) = al4(i,j)
              enddo
            enddo
          endif
        enddo
      elseif (mproc.eq.mpe_1(nproc)) then
        call MPI_SEND(al4,itdm*jj,MTYPE4,
     &                idproc1(mnp), 9942,
     &                mpi_comm_hycom_, mpierr)
      endif
#elif defined(SHMEM)
      if     (mnproc.eq.mnflg) then
        if (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,itdm
              aa(i,j+j0) = al4(i,j)
            enddo
          enddo
        endif
        do np= 1,jpr
          mp = mpe_1(np)
          if     (idproc(mp,np).ne.idproc(mproc,nproc)) then
            call SHMEM_GET4(al4,
     &                      al4,itdm*jj_pe(mp,np), idproc(mp,np))
            do j= 1,jj_pe(mp,np)
              do i= 1,itdm
                aa(i,j+j0_pe(mp,np)) = al4(i,j)
              enddo
            enddo
          endif
        enddo
      endif
      ! no barrier needed here because of double buffering (at and then al)
#endif /* MPI:SHMEM */
      endif !mnflg.eq.-1:else                                              
#if defined(TIMER)
c
      if     (nxc.eq. 1) then
        call xctmr1( 1)
        nxc = 0
      endif
#endif
      return
      end subroutine xcaget4

      subroutine xcaput(aa, a, mnflg)
      implicit none
c
      real,    intent(inout) :: aa(itdm,jtdm)
      real,    intent(out)   :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) convert an entire 2-D array from non-tiled to tiled layout.
c
c  3) mnflg selects which nodes must contain the non-tiled array
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c     if mnflg.ne.0 the array aa may be broadcast to all nodes,
c      so aa must exist and be overwritable on all nodes.
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa              real           input     non-tiled source array
c    a               real           output    tiled target array
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer i,j,mp,np,mnp
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
      integer mpireqa(jpr),mpireqb(ipr)
c
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        BARRIER
        call xctmr0( 4)
        nxc = 4
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(al)) then
        allocate(       al(itdm,jdm) )
        call mem_stat_add( itdm*jdm )
        allocate(       at(idm*jdm) )
        call mem_stat_add( idm*jdm )
      endif
#endif
*
*     if     (mnproc.eq.1) then
*       write(lp,'(a,g20.10)') 'xcaput - vland =',vland
*     endif
c
c     use xclput for now,
c     this is slow for mnflg.ne.0, but easy to implement.
c
      if     (mnflg.ne.0) then
c       "broadcast" row sections of aa to all processors in the row.
        if     (mnproc.ne.mnflg) then
          aa(:,:) = vland
        endif
#if defined(MPI)
        if     (mnproc.eq.mnflg) then
          j = 0
          do np= 1,jpr
            mp = mpe_1(np)
            if     (np.eq.nproc .and. mp.eq.mproc) then
              al(:,1:jj) = aa(:,j0+1:j0+jj)
            else
              j = j + 1
              call MPI_ISEND(aa(1,j0_pe(mp,np)+1),
     &                      itdm*jj_pe(mp,np),MTYPER,
     &                      idproc(mp,np), 9951,
     &                      mpi_comm_hycom_, mpireqa(j), mpierr)
            endif
          enddo
          call mpi_waitall( j, mpireqa, MPI_STATUSES_IGNORE, mpierr)
        elseif (mproc.eq.mpe_1(nproc)) then
          call MPI_RECV(al,itdm*jj,MTYPER,
     &                  idproc1(mnflg), 9951,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
        endif
c
        if     (mproc.eq.mpe_1(nproc)) then
          i = 0
          do mp= mpe_1(nproc)+1,mpe_e(nproc)
            i = i + 1
            call MPI_ISEND(al,itdm*jj,MTYPER,
     &                    idproc(mp,nproc), 9952,
     &                    mpi_comm_hycom_, mpireqb(i), mpierr)
          enddo
          call mpi_waitall( i, mpireqb, MPI_STATUSES_IGNORE, mpierr)
        else
          call MPI_RECV(al,itdm*jj,MTYPER,
     &                  idproc(mpe_1(nproc),nproc), 9952,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
        endif
c
        aa(:,j0+1:j0+jj) = al(:,1:jj)
#elif defined(SHMEM)
c       assume aa is in common.
        BARRIER
        if     (mnproc.ne.mnflg) then
          do j= 1,jj
            call SHMEM_GETR(aa(1,j0+j),
     &                      aa(1,j0+j),itdm,idproc1(mnflg))
          enddo
        endif
        BARRIER
#endif
      endif
      do j= 1,jtdm
        call xclput(aa(1,j),itdm, a, 1,j,1,0)
      enddo
#if defined(TIMER)
c
      if     (nxc.eq. 4) then
        call xctmr1( 4)
        nxc = 0
      endif
#endif
      return
      end subroutine xcaput

      subroutine xcaput4(aa, a, mnflg)
      implicit none
c
      real*4,  intent(in)    :: aa(itdm,*)
      real*4,  intent(out)   :: a(ii,jj)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) convert an entire 2-D array from non-tiled to tiled layout.
c
c  2) Special version for zaiord and zaiowr only.
c     arrays are real*4 and tiled array has no halo.
c
c  3) mnflg selects which nodes must contain the non-tiled array
c        =-1; first node in each row contains that row
c        = n; node number n (mnproc=n)
c     the array aa is unchanged on exit, and need not exist
c      on the nodes that do not reference it.
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa              real           input     non-tiled source array
c    a               real           output    tiled target array
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer i,msg,j,mp,np,mnp
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
      integer mpireqa(jpr),mpireqb(ipr)
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        BARRIER
        call xctmr0( 4)
        nxc = 4
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(at4)) then
        if     (mproc.eq.mpe_1(nproc)) then
          allocate(       al4(itdm,jdm) )
          allocate(      alt4(idm *jdm,ipr) )
          call mem_stat_add( (itdm*jdm)/2     ) !real*4, so /2
          call mem_stat_add( (idm *jdm*jpr)/2 ) !real*4, so /2
        endif !first tile in the row
        allocate(       at4(idm*jdm) )
        call mem_stat_add( (idm*jdm) /2 ) !real*4, so /2
      endif
#endif
*
*     if     (mnproc.eq.1) then
*       write(lp,'(a,g20.10)') 'xcaput - vland =',vland
*     endif
c
      if     (mnflg.gt.0) then
#if defined(MPI)
c       "broadcast" row sections of aa to first processor in each row.
        if     (mnproc.eq.mnflg) then
          j = 0
          do np= 1,jpr
            mp = mpe_1(np)
            if     (np.eq.nproc .and. mp.eq.mproc) then
              al4(:,1:jj) = aa(:,j0+1:j0+jj)
            else
              j = j + 1
              call MPI_ISEND(aa(1,j0_pe(mp,np)+1),
     &                      itdm*jj_pe(mp,np),MTYPE4,
     &                      idproc(mp,np), 9951,
     &                      mpi_comm_hycom_, mpireqa(j), mpierr)
            endif
          enddo
          call mpi_waitall( j, mpireqa, MPI_STATUSES_IGNORE, mpierr)
        elseif (mproc.eq.mpe_1(nproc)) then
          call MPI_RECV(al4,itdm*jj,MTYPE4,
     &                  idproc1(mnflg), 9951,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
        endif
c
c       "broadcast" partial row sections to the rest of the row's processors
        if     (mproc.eq.mpe_1(nproc)) then
          msg = 0
          do mp= mpe_1(nproc)+1,mpe_e(nproc)
            do j= 1,jj
              do i= 1,ii_pe(mp,nproc)
                alt4(i+(j-1)*ii_pe(mp,nproc),mp) = 
     &            al4(i0_pe(mp,nproc)+i,j)
              enddo
            enddo
            msg = msg + 1
            call MPI_ISEND(alt4(1,mp),ii_pe(mp,nproc)*jj,MTYPE4,
     &                    idproc(mp,nproc), 9952,
     &                    mpi_comm_hycom_, mpireqb(msg), mpierr)
          enddo !mp
c
          do j= 1,jj
            do i= 1,ii
              a(i,j) = al4(i0+i,j)
            enddo
          enddo
c
          call mpi_waitall( msg, mpireqb, MPI_STATUSES_IGNORE, mpierr)
        else
          call MPI_RECV(at4,ii*jj,MTYPE4,
     &                  idproc(mpe_1(nproc),nproc), 9952,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          do j= 1,jj
            do i= 1,ii
              a(i,j) = at4(i+(j-1)*ii)
            enddo
          enddo
        endif
#elif defined(SHMEM)
c       assume aa is in common.
        BARRIER
c       "broadcast" row sections of aa to first processor in each row.
        if     (mproc.eq.mpe_1(nproc)) then
          call SHMEM_GET4(al4(1,   1),
     &                     aa(1,j0+1),itdm*jj,idproc1(mnflg))
          do mp= mpe_1(nproc)+1,mpe_e(nproc)
            do j= 1,jj
              do i= 1,ii
                alt4(i+(j-1)*ii,mp) = al4(i0+i,j)
              enddo
            enddo
          enddo !mp
        endif
        BARRIER
c       "broadcast" partial row sections to the rest of the row's processors
        if     (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,ii
              a(i,j) = al4(i0+i,j)
            enddo
          enddo
        else
          call SHMEM_GET4( at4(1,    1),
     &                    alt4(1,mproc),ii*jj,idproc1(mpe_1(nproc)))
          do j= 1,jj
            do i= 1,ii
              a(i,j) = at4(i+(j-1)*ii)
            enddo
          enddo
        endif
        BARRIER
#endif
      elseif (mnflg.eq.-1) then
#if defined(MPI)
c       "broadcast" partial row sections of aa to all processors in the row.
        if     (mproc.eq.mpe_1(nproc)) then
          msg = 0
          do mp= mpe_1(nproc)+1,mpe_e(nproc)
            do j= 1,jj
              do i= 1,ii_pe(mp,nproc)
                alt4(i+(j-1)*ii_pe(mp,nproc),mp) = 
     &            aa(i0_pe(mp,nproc)+i,j)
              enddo
            enddo
            msg = msg + 1
            call MPI_ISEND(alt4(1,mp),ii_pe(mp,nproc)*jj,MTYPE4,
     &                    idproc(mp,nproc), 9953,
     &                    mpi_comm_hycom_, mpireqb(msg), mpierr)
          enddo !mp
c
          do j= 1,jj
            do i= 1,ii
              a(i,j) = aa(i0+i,j)
            enddo
          enddo
c
          call mpi_waitall( msg, mpireqb, MPI_STATUSES_IGNORE, mpierr)
        else
          call MPI_RECV(at4,ii*jj,MTYPE4,
     &                  idproc(mpe_1(nproc),nproc), 9953,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          do j= 1,jj
            do i= 1,ii
              a(i,j) = at4(i+(j-1)*ii)
            enddo
          enddo
        endif
#elif defined(SHMEM)
c       "broadcast" partial row sections to the rest of the row's processors
        if     (mproc.eq.mpe_1(nproc)) then
          do mp= mpe_1(nproc)+1,mpe_e(nproc)
            do j= 1,jj
              do i= 1,ii_pe(mp,nproc)
                alt4(i+(j-1)*ii_pe(mp,nproc),mp) = 
     &            aa(i0_pe(mp,nproc)+i,j)
              enddo
            enddo
          enddo !mp
        endif
        BARRIER
        if     (mproc.eq.mpe_1(nproc)) then
          do j= 1,jj
            do i= 1,ii
              a(i,j) = aa(i0+i,j)
            enddo
          enddo
        else
          call SHMEM_GET4( at4(1,    1),
     &                    alt4(1,mproc),ii*jj,idproc1(mpe_1(nproc)))
          do j= 1,jj
            do i= 1,ii
              a(i,j) = at4(i+(j-1)*ii)
            enddo
          enddo
        endif
        BARRIER
#endif
      endif !mnflg.gt.0:mnflg.eq.-1
#if defined(TIMER)
c
      if     (nxc.eq. 4) then
        call xctmr1( 4)
        nxc = 0
      endif
#endif
      return
      end subroutine xcaput4

      subroutine xcastr(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a(:)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) broadcast array a to all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    mnflg           integer        input     node originator flag
c*
c**********
c
      integer i,is0,isl,mn,n,nn
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 9)
        nxc =  9
      endif
#endif
c
c
c     stripmine a.
c
      n = size(a)
c
      do is0= 0,n-1,nmax
        isl = min(is0+nmax,n)
        nn = isl - is0
        if     (mnproc.eq.mnflg) then
          do i= 1,nn
            b(i) = a(is0+i)
          enddo
        endif
#if defined(MPI)
        call mpi_bcast(b,nn,MTYPER,
     &                 idproc1(mnflg),
     &                 mpi_comm_hycom_,mpierr)
#elif defined(SHMEM)
        BARRIER
        if     (mnproc.ne.mnflg) then
c         get from source processor
          call SHMEM_GETR(b,b,nn, idproc1(mnflg))
        endif
        BARRIER
#endif
        if     (mnproc.ne.mnflg) then
          do i= 1,nn
            a(is0+i) = b(i)
          enddo
        endif
      enddo  ! stripmine loop
#if defined(TIMER)
c
      if     (nxc.eq. 9) then
        call xctmr1( 9)
        nxc = 0
      endif
#endif
      return
      end subroutine xcastr

      subroutine xceget(aelem, a, ia,ja, mnflg)
      implicit none
c
      real,    intent(out)   ::         aelem
      real,    intent(in)    :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    ::         ia,ja
      integer, intent(in), optional ::  mnflg
c
c**********
c*
c  1) find the value of a(ia,ja) on the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes must return the line
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     note that aelem might, or might not, be returned on other nodes
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aelem           real           output    required element
c    a               real           input     tiled source array
c    ia              integer        input     1st non-tiled index into a
c    ja              integer        input     2nd non-tiled index into a
c    mnflg           integer        input     node return flag
c
c  4) the global variable vland is returned when outside active tiles.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
c     double buffer to reduce the number of required barriers.
c
      real,    save, dimension(0:1) :: elem
      integer, save :: kdb = 0
c
      integer i,j,lmnflg,mp,np
#if defined(TIMER)
c
*     if     (nxc.eq.0) then
*       call xctmr0( 2)
*       nxc = 2
*     endif
#endif
c     
      if     (present(mnflg)) then
        lmnflg = mnflg
      else
        lmnflg = 0  ! default
      endif
c
      kdb = mod(kdb+1,2)  ! the least recently used of the two buffers
c
c     find the host tile.
c
      np  = npe_j(max(0,min(jtdm+1,ja))   )
      mp  = mpe_i(max(0,min(itdm+1,ia)),np)
c
      if      (mp.le.0) then
c
c       no tile.
c
        elem(kdb) = vland
      elseif  (mp.eq.mproc .and. np.eq.nproc) then
c
c       this tile.
c
        i = ia - i0
        j = ja - j0
c
        elem(kdb) = a(i,j)
#if defined(MPI)
        if     (lmnflg.eq.0) then
          call mpi_bcast(elem(kdb),1,MTYPER,
     &                   idproc(mp,np),mpi_comm_hycom_,mpierr)
        elseif (lmnflg.ne.mnproc) then
          call MPI_SEND(elem(kdb),1,MTYPER,
     &                  idproc1(lmnflg), 9932,
     &                  mpi_comm_hycom_, mpierr)
        endif !lmnflg
#elif defined(SHMEM)
        BARRIER
#endif
      else
c
c       another tile.
c
#if defined(MPI)
        if     (lmnflg.eq.0) then
          call mpi_bcast(elem(kdb),1,MTYPER,
     &                   idproc(mp,np),mpi_comm_hycom_,mpierr)
        elseif (lmnflg.eq.mnproc) then
          call MPI_RECV( elem(kdb),1,MTYPER,
     &                   idproc(mp,np), 9932,
     &                   mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
        endif !lmnflg
#elif defined(SHMEM)
        BARRIER
        call SHMEM_GETR(elem(kdb),
     &                  elem(kdb),1,idproc(mp,np))
        ! no barrier needed here because of double buffering
#endif
      endif
      aelem = elem(kdb)
#if defined(TIMER)
c
*     if     (nxc.eq. 2) then
*       call xctmr1( 2)
*       nxc = 0
*     endif
#endif
      return
      end subroutine xceget

      subroutine xceput(aelem, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in), optional ::  mnflg
      integer, intent(in)    ::         ia,ja
      real,    intent(inout) ::         aelem
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) fill a single element in the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes hold the element on entry
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     all nodes hold the element on exit
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aelem           real           in/out    element value
c    a               real           in/out    tiled target array
c    ia              integer        input     1st non-tiled index into a
c    ja              integer        input     2nd non-tiled index into a
c    mnflg           integer        input     node source flag
c*
c**********
c
      real, save :: a1(1)
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
#if defined(TIMER)
c
*     if     (nxc.eq.0) then
*       call xctmr0( 5)
*       nxc = 5
*     endif
#endif
c     
      if     (present(mnflg)) then
        if     (mnflg.ne.0) then
          if     (mnproc.eq.mnflg) then
            a1(1) = aelem
            call xcastr(a1, mnflg)  !broadcast the input element
          else
            call xcastr(a1, mnflg)  !broadcast the input element
            aelem = a1(1)
          endif !mnproc
        endif !mnflg>0
      endif !present
c
      if     (i0.lt.ia .and. ia.le.i0+ii .and.
     &        j0.lt.ja .and. ja.le.j0+jj      ) then
c
c       this tile.
c
        a(ia-i0,ja-j0) = aelem
      endif
#if defined(TIMER)
c
*     if     (nxc.eq. 5) then
*       call xctmr1( 5)
*       nxc = 0
*     endif
#endif
      return
      end subroutine xceput

      subroutine xcgetc(iline)
      implicit none
c
      integer, intent(inout) :: iline(81)
c
c**********
c*
c  1) machine specific routine for broadcasting iline.
c
c  2) only use in zagetc (hence the name).
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
c     broadcast to all processors
c
#if defined(MPI)
      call mpi_bcast(iline,81,MTYPEI,
     &               idproc1(1),mpi_comm_hycom_,mpierr)
#elif defined(SHMEM)
      BARRIER
      if     (mnproc.ne.1) then
        call SHMEM_GETI(iline,
     &                  iline,81, idproc1(1))
      endif
      ! no barrier needed here because zagetc is using two buffers
#endif
      return
      end subroutine xcgetc

      subroutine xchalt(cerror)
      implicit none
c
      character*(*), intent(in) :: cerror
c
c**********
c*
c  1) stop all processes.
c
c  2) only one processes need call this routine, i.e. it is for
c      emergency stops.  use 'xcstop' for ordinary stops called
c      by all processes.
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    cerror          char*(*)       input     error message
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
c     message passing version.
c
      if     (cerror.ne.' ') then
        write(lp,*) '**************************************************'
        write(lp,*) cerror
      endif
      write(lp,*) '**************************************************'
      write(lp,*) 'XCHALT CALLED ON PROC = ',mnproc,mproc,nproc
      write(lp,*) '**************************************************'
      call flush(lp)
c
#if defined(MPI)
      call mpi_abort(mpi_comm_hycom_,9)
#else
      call abort()
#endif
      stop '(xchalt)'
      end subroutine xchalt

      subroutine xciget(alist,nl, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in)  ::          nl
      real,    intent(out) ::          alist(nl)
      real,    intent(in)  :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)  ::          ia(nl),ja(nl)
      integer, intent(in), optional :: mnflg
c
c**********
c*
c  1) find the value of a(ia(:),ja(:)) on the non-tiled 2-D grid.
c
c  2) mnflg selects which nodes must return the line
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     note that alist might, or might not, be returned on other nodes
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    alist           real           output    required elements
c    nl              integer        input     dimension of alist
c    a               real           input     tiled source array
c    ia              integer        input     1st non-tiled indexes into a
c    ja              integer        input     2nd non-tiled indexes into a
c    mnflg           integer        input     node return flag
c
c  4) the global variable vland is returned when outside active tiles.
c*
c**********
c
      integer i,lmnflg,nl_sm
c     
      if     (present(mnflg)) then
        lmnflg = mnflg
      else
        lmnflg = 0  ! default
      endif
c
c     stripmine alist in itdm+jtdm chunks
c
      do i= 1,nl,itdm+jtdm
        nl_sm = min( itdm+jtdm, nl+1-i )
        call xciget_sm(alist(i),nl_sm, a, ia(i),ja(i), lmnflg)
      enddo !i
      return
      end subroutine xciget

      subroutine xciget_sm(alist,nl, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in)  :: nl
      real,    intent(out) :: alist(nl)
      real,    intent(in)  :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)  :: ia(nl),ja(nl)
      integer, intent(in)  :: mnflg
c
c**********
c*
c  1) find the value of a(ia(:),ja(:)) on the non-tiled 2-D grid.
c     version for nl<=itdm+jtdm, always invoke via xciget
c
c  2) mnflg selects which nodes must return the line
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     note that alist might, or might not, be returned on other nodes
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    alist           real           output    required elements
c    nl              integer        input     dimension of alist
c    a               real           input     tiled source array
c    ia              integer        input     1st non-tiled indexes into a
c    ja              integer        input     2nd non-tiled indexes into a
c    mnflg           integer        input     node return flag
c
c  4) the global variable vland is returned when outside active tiles.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c     
c     double buffer to remove the need for barriers.
c     pad alX to guard against false sharing.
c
#if defined(RELO)
      real,    save, allocatable, dimension(:,:) ::
     &  als,alr
#else
      real,    save, dimension(-47:itdm+jtdm+48,0:1) ::
     &  als,alr
#endif
      integer, save, dimension(0:ijqr-1,0:1) :: nlp,nxp
c     
      integer, save :: kdb = 0
      integer       :: i,np,mp,mnp,nxpa
c
#if defined(RELO)
c
      if     (.not.allocated(als)) then
        allocate(     als(-47:itdm+jtdm+48,0:1),
     &                alr(-47:itdm+jtdm+48,0:1) )
        call mem_stat_add( 2*(itdm+jtdm+96)*2 )
      endif
#endif
c     
      kdb = mod(kdb+1,2)  ! the least recently used of the two buffers
c
c --- calculate the gatherv sizes and offsets on all tiles
c
      nlp(0:ijqr-1,kdb) = 0
      do i= 1,nl
        np  = npe_j(max(0,min(jtdm+1,ja(i)))   )
        mp  = mpe_i(max(0,min(itdm+1,ia(i))),np)
        if     (mp.gt.0) then
          mnp = idproc(mp,np)
          nlp(mnp,kdb) = nlp(mnp,kdb) + 1
          if     (mp.eq.mproc .and. np.eq.nproc) then
c ---       update the local send buffer
            als(nlp(mnp,kdb),kdb) = a(ia(i)-i0,ja(i)-j0)
          endif
        endif !mp
      enddo !i
c
      nxp(0,kdb) = 0
      do i= 1,ijqr-1
        nxp(i,kdb) = nxp(i-1,kdb) + nlp(i-1,kdb)
      enddo
      nxpa = nxp(ijqr-1,kdb) + nlp(ijqr-1,kdb)
c
#if defined(MPI)
      if     (mnflg.eq.0) then
c ---   gatherv onto the 1st mpi task, then broadcast the result
        mnp = idproc(mproc,nproc)
        call mpi_gatherv(als(1,kdb),nlp(mnp,kdb),MTYPER,
     &                   alr(1,kdb),nlp(0,kdb),nxp(0,kdb),MTYPER,
     &                   idproc1(1),
     &                   mpi_comm_hycom_, mpierr)
        call mpi_bcast(  alr(1,kdb),nxpa,MTYPER,
     &                   idproc1(1),
     &                   mpi_comm_hycom_, mpierr)
      else
        mnp = idproc(mproc,nproc)
        call mpi_gatherv(als(1,kdb),nlp(mnp,kdb),MTYPER,
     &                   alr(1,kdb),nlp(0,kdb),nxp(0,kdb),MTYPER,
     &                   idproc1(mnflg),
     &                   mpi_comm_hycom_, mpierr)
      endif !mnflg
#elif defined(SHMEM)
      call xcstop('xciget: shmem not implemented')
      stop '(xciget)'
#endif
c
      if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
c ---   repeat the gatherv size logic to unwind the recv buffer
        nlp(0:ijqr-1,kdb) = 0
        do i= 1,nl
          np  = npe_j(max(0,min(jtdm+1,ja(i)))   )
          mp  = mpe_i(max(0,min(itdm+1,ia(i))),np)
          if     (mp.le.0) then
            alist(i) = vland
          else
            mnp = idproc(mp,np)
            nlp(mnp,kdb) = nlp(mnp,kdb) + 1
            alist(i) = alr(nlp(mnp,kdb)+nxp(mnp,kdb),kdb)
          endif !mp
        enddo !i
      endif !mnflg
      return
      end subroutine xciget_sm

      subroutine xciput(alist,nl, a, ia,ja, mnflg)
      implicit none
c
      integer, intent(in)    ::        nl
      integer, intent(in), optional :: mnflg
      integer, intent(in)    ::        ia(nl),ja(nl)
      real,    intent(inout) ::        alist(nl)
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) fill a list of elements in the non-tiled 2-D grid.
c     also updates the halo.
c
c  2) mnflg selects which nodes hold the elements on entry
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     all nodes hold the elements on exit
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    alist           real           in/out    element values
c    nl              integer        input     dimension of alist
c    a               real           in/out    tiled target array
c    ia              integer        input     1st non-tiled indexes into a
c    ja              integer        input     2nd non-tiled indexes into a
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer i,iai,jai
#if defined(TIMER)
c
*     if     (nxc.eq.0) then
*       call xctmr0( 5)
*       nxc = 5
*     endif
#endif
c     
      if     (present(mnflg)) then
        if     (mnflg.ne.0) then
          call xcastr(alist, mnflg)  !broadcast the input list
        endif
      endif
c
      do i= 1,nl
        iai = ia(i)-i0
        jai = ja(i)-j0
        if     (iai.ge.1-nbdy .and. iai.le.ii+nbdy .and.
     &          jai.ge.1-nbdy .and. jai.le.jj+nbdy      ) then
c
c         on this tile with halo.
c
          a(iai,jai) = alist(i)
        endif !on tile
c
        if     (nreg.ne.0 .and. ia(i).ge.itdm-nbdy+1) then  ! periodic
          iai = ia(i)-itdm-i0
          if     (iai.ge.1-nbdy .and. iai.le.ii+nbdy .and.
     &            jai.ge.1-nbdy .and. jai.le.jj+nbdy      ) then
            a(iai,jai) = alist(i)
          endif !on tile
        endif !periodic, large ia(i)
        if     (nreg.ne.0 .and. ia(i).le.nbdy) then  ! periodic
          iai = ia(i)+itdm-i0
          if     (iai.ge.1-nbdy .and. iai.le.ii+nbdy .and.
     &            jai.ge.1-nbdy .and. jai.le.jj+nbdy      ) then
            a(iai,jai) = alist(i)
          endif !on tile
        endif !periodic, small ia(i)
      enddo !i
#if defined(TIMER)
c
*     if     (nxc.eq. 5) then
*       call xctmr1( 5)
*       nxc = 0
*     endif
#endif
      return
      end subroutine xciput

      subroutine xclget(aline,nl, a, i1,j1,iinc,jinc, mnflg)
      implicit none
c
      integer, intent(in)    ::  nl,i1,j1,iinc,jinc,mnflg
      real,    intent(out)   ::  aline(nl)
      real,    intent(in)    ::  a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) extract a line of elements from the non-tiled 2-D grid.
c
c  2) aline(i) == aa(i1+iinc*(i-1),j1+jinc*(i-1)), for i=1...nl.
c     where aa is the non-tiled representation of a, and
c     iinc and jinc can each be -1, 0, or +1.
c
c  3) mnflg selects which nodes must return the line
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aline           real           output    required line of elements
c    nl              integer        input     dimension of aline
c    a               real           input     tiled source array
c    i1              integer        input     1st non-tiled index into a
c    j1              integer        input     2nd non-tiled index into a
c    iinc            integer        input     1st index increment
c    jinc            integer        input     2nd index increment
c    mnflg           integer        input     node return flag
c
c  5) the global variable vland is returned when outside active tiles.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
c     pad al to guard against false sharing.
c     double buffer to reduce the number of required barriers.
c
#if defined(RELO)
      real, save, allocatable, dimension (:,:) :: al
#else
      real, save, dimension(-47:itdm+jtdm+48,0:1) :: al
#endif
c
      integer, save        :: kdb = 0
      integer, allocatable :: ia(:),ja(:)
c
      integer i1n,iif,iil,j1n,jjf,jjl,l,lx0,nxl,mp,np
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 3)
        nxc = 3
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(al)) then
        allocate(    al(-47:itdm+jtdm+48,0:1) )
        call mem_stat_add( (itdm+jtdm+96)*2 )
      endif
#endif
c
      kdb = mod(kdb+1,2)  ! the least recently used of the two buffers
c
      if     ((jinc.eq.0 .and. iinc.eq.1) .or. nl.eq.1) then
c
c ---   horizontal forward line.
c
        al(1:nl,kdb) = vland
c
        np  = npe_j(j1)
        do mp= mpe_1(np),mpe_e(np)
          iif = max(i1,     i0_pe(mp,np)+1)
          iil = min(i1+nl-1,i0_pe(mp,np)+ii_pe(mp,np))
          lx0 = iif - i1
          nxl = iil - iif + 1
          if     (nxl.le.0) then
            cycle  ! no elements from tile (mp,np)
          endif
c
          if      (mp.eq.mproc .and. np.eq.nproc) then
c
c           this tile.
c
            do l= lx0+1,lx0+nxl
              al(l,kdb) = a(i1+l-1-i0,j1-j0)
            enddo
#if defined(MPI)
            if     (mnflg.eq.0) then
              call mpi_bcast(al(lx0+1,kdb),nxl,MTYPER,
     &                       idproc(mp,np),mpi_comm_hycom_,mpierr)
            elseif (mnflg.ne.mnproc) then
              call MPI_SEND(al(lx0+1,kdb),nxl,MTYPER,
     &                      idproc1(mnflg), 9931,
     &                      mpi_comm_hycom_, mpierr)
            endif
          else
c
c           another tile.
c
            if     (mnflg.eq.0) then
              call mpi_bcast(al(lx0+1,kdb),nxl,MTYPER,
     &                       idproc(mp,np),mpi_comm_hycom_,mpierr)
            elseif (mnflg.eq.mnproc) then
              call MPI_RECV(al(lx0+1,kdb),nxl,MTYPER,
     &                      idproc(mp,np), 9931,
     &                      mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
            endif
#endif /* MPI */
          endif
c
          if     (lx0+nxl.eq.nl) then
            exit
          endif
        enddo  ! np=1,jpr
#if defined(SHMEM)
c
c       spliting process into two phases saves on barriers.
c
        BARRIER
        do mp= mpe_1(np),mpe_e(np)
          iif = max(i1,     i0_pe(mp,np)+1)
          iil = min(i1+nl-1,i0_pe(mp,np)+ii_pe(mp,np))
          lx0 = iif - i1
          nxl = iil - iif + 1
          if     (nxl.le.0) then
            cycle  ! no elements from tile (mp,np)
          endif
c
          if      (mp.eq.mproc .and. np.eq.nproc) then
c
c           nothing to do here (see 1st phase, above).
c
          else
c
c           another tile.
c
            if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
              call SHMEM_GETR(al(lx0+1,kdb),
     &                        al(lx0+1,kdb),nxl,idproc(mp,np))
            endif
          endif
c
          if     (lx0+nxl.eq.nl) then
            exit
          endif
        enddo  ! np=1,jpr
        ! no barrier needed here because of double buffering
#endif /* SHMEM */
c
        if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
          aline(1:nl) = al(1:nl,kdb)
        endif
      elseif (iinc.eq.0 .and. jinc.eq.1) then
c
c ---   vertical forward line.
c
        al(1:nl,kdb) = vland
c
        do np= 1,jpr
          mp = mpe_i(i1,np)
          if     (mp.le.0) then
            cycle  ! an all-land column
          endif
          jjf = max(j1,     j0_pe(mp,np)+1)
          jjl = min(j1+nl-1,j0_pe(mp,np)+jj_pe(mp,np))
          lx0 = jjf - j1
          nxl = jjl - jjf + 1
          if     (nxl.le.0) then
            cycle  ! no elements from tile (mp,np)
          endif
c
          if      (mp.eq.mproc .and. np.eq.nproc) then
c
c           this tile.
c
            do l= lx0+1,lx0+nxl
              al(l,kdb) = a(i1-i0,j1+l-1-j0)
            enddo
#if defined(MPI)
            if     (mnflg.eq.0) then
              call mpi_bcast(al(lx0+1,kdb),nxl,MTYPER,
     &                       idproc(mp,np),mpi_comm_hycom_,mpierr)
            elseif (mnflg.ne.mnproc) then
              call MPI_SEND(al(lx0+1,kdb),nxl,MTYPER,
     &                      idproc1(mnflg), 9931,
     &                      mpi_comm_hycom_, mpierr)
            endif
          else
c
c           another tile.
c
            if     (mnflg.eq.0) then
              call mpi_bcast(al(lx0+1,kdb),nxl,MTYPER,
     &                       idproc(mp,np),mpi_comm_hycom_,mpierr)
            elseif (mnflg.eq.mnproc) then
              call MPI_RECV(al(lx0+1,kdb),nxl,MTYPER,
     &                      idproc(mp,np), 9931,
     &                      mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
            endif
#endif /* MPI */
          endif
c
          if     (lx0+nxl.eq.nl) then
            exit
          endif
        enddo  ! np=1,jpr
#if defined(SHMEM)
c
c       spliting process into two phases saves on barriers.
c
        BARRIER
        do np= 1,jpr
          mp = mpe_i(i1,np)
          if     (mp.le.0) then
            cycle  ! an all-land column
          endif
          jjf = max(j1,     j0_pe(mp,np)+1)
          jjl = min(j1+nl-1,j0_pe(mp,np)+jj_pe(mp,np))
          lx0 = jjf - j1
          nxl = jjl - jjf + 1
          if     (nxl.le.0) then
            cycle  ! no elements from tile (mp,np)
          endif
c
          if      (mp.eq.mproc .and. np.eq.nproc) then
c
c           nothing to do here (see 1st phase, above).
c
          else
c
c           another tile.
c
            if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
              call SHMEM_GETR(al(lx0+1,kdb),
     &                        al(lx0+1,kdb),nxl,idproc(mp,np))
            endif
          endif
c
          if     (lx0+nxl.eq.nl) then
            exit
          endif
        enddo  ! np=1,jpr
        ! no barrier needed here because of double buffering
#endif /* SHMEM */
c
        if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
          aline(1:nl) = al(1:nl,kdb)
        endif
      elseif (nl.eq.1) then
c
c       diagonal and reversing lines, single element
c
        call xceget(aline(1),a,i1,j1, mnflg)
      else
c
c       diagonal and reversing lines - call xciget.
c
        allocate(ia(nl),ja(nl))
        do l= 1,nl
          ia(l) = i1+iinc*(l-1)
          ja(l) = j1+jinc*(l-1)
        enddo !l
        call xciget(aline,nl, a, ia,ja, mnflg)
        deallocate(ia,ja)
      endif
#if defined(TIMER)
c
      if     (nxc.eq. 3) then
        call xctmr1( 3)
        nxc = 0
      endif
#endif
      return
      end subroutine xclget

      subroutine xclput(aline,nl, a, i1,j1,iinc,jinc, mnflg)
      implicit none
c
      integer, intent(in), optional ::  mnflg
      integer, intent(in)    ::         nl,i1,j1,iinc,jinc
      real,    intent(inout) ::         aline(nl)
      real,    intent(inout) ::  a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) fill a line of elements in the non-tiled 2-D grid.
c
c  2) aline(i) == aa(i1+i1*(i-1),j1+j1*(i-1)), for i=1...nl.
c     where aa is the non-tiled representation of a, and
c     one of iinc and jinc must be 0, and the other must be 1.
c     also updates the halo.
c
c  3) mnflg selects which nodes hold the line on entry
c        = 0; all nodes (default)
c        = n; node number n (mnproc=n)
c     all nodes hold the line on exit
c
c  4) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aline           real           in/out    line of element values
c    nl              integer        input     dimension of aline
c    a               real           in/out    tiled target array
c    i1              integer        input     1st non-tiled index into a
c    j1              integer        input     2nd non-tiled index into a
c    iinc            integer        input     1st index increment
c    jinc            integer        input     2nd index increment
c    mnflg           integer        input     node source flag
c*
c**********
c
      integer i,j
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 5)
        nxc = 5
      endif
#endif
c
      if     (present(mnflg)) then
        if     (mnflg.ne.0) then
          call xcastr(aline, mnflg)  !broadcast the input line
        endif
      endif
c
      if     (jinc.eq.0) then
        j = j1-j0
        if     (j.ge.1-nbdy .and. j.le.jj+nbdy) then
          do i= max(1-nbdy,i1-i0),min(i1+nl-1-i0,ii+nbdy)
            a(i,j) = aline(i+i0-i1+1)
          enddo
          if     (nreg.ne.0 .and. i1+nl-1.ge.itdm-nbdy+1) then  ! periodic
            do i= max(1-nbdy,i1-itdm-i0),min(i1+nl-1-itdm-i0,0)
              a(i,j) = aline(i+itdm+i0-i1+1)
            enddo
          endif
          if     (nreg.ne.0 .and. i1.le.nbdy) then  ! periodic
            do i= max(ii+1,i1+itdm-i0),min(i1+nl-1+itdm-i0,ii+nbdy)
              a(i,j) = aline(i-itdm+i0-i1+1)
            enddo
          endif
        endif !j
      elseif (iinc.eq.0) then
        i = i1-i0
        if     (i.ge.1-nbdy .and. i.le.ii+nbdy) then
          do j= max(1-nbdy,j1-j0),min(j1-j0+nl-1,jj+nbdy)
            a(i,j) = aline(j+j0-j1+1)
          enddo
        endif !on tile
        if     (nreg.ne.0 .and. i1.ge.itdm-nbdy+1) then  ! periodic
          i = i1-itdm-i0 !e.g. i1==itdm becomes 0-i0
          if     (i.ge.1-nbdy .and. i.le.0) then
            do j= max(1-nbdy,j1-j0),min(j1-j0+nl-1,jj+nbdy)
              a(i,j) = aline(j+j0-j1+1)
            enddo
          endif !in halo
        endif !near itdm
        if     (nreg.ne.0 .and. i1.le.nbdy) then  ! periodic
          i = i1+itdm-i0 !e.g. i1==1 becomes itdm+1-i0
          if     (i.ge.ii+1 .and. i.le.ii+nbdy) then
            do j= max(1-nbdy,j1-j0),min(j1-j0+nl-1,jj+nbdy)
              a(i,j) = aline(j+j0-j1+1)
            enddo
          endif !in halo
        endif !near 1
      endif
#if defined(TIMER)
c
      if     (nxc.eq. 5) then
        call xctmr1( 5)
        nxc = 0
      endif
#endif
      return
      end subroutine xclput

      subroutine xcmaxr_0(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace scalar a with its element-wise maximum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c    mnflg           integer        input     node return flag
c*
c**********
c
      real a1(1)
c
      a1(1) = a
      call xcmaxr_1(a1, mnflg)
      a = a1(1)
      return
      end subroutine xcmaxr_0

      subroutine xcmaxr_1(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a(:)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace array a with its element-wise maximum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer i,is0,isl,mn,mnp,n,nn
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0(10)
        nxc = 10
      endif
#endif
c
c     stripmine a.
c
      n = size(a)
c
      do is0= 0,n-1,nmax
        isl = min(is0+nmax,n)
        nn = isl - is0
        do i= 1,nn
          b(i) = a(is0+i)
        enddo
c
#if defined(MPI)
        if     (mnflg.eq.0) then
          call mpi_allreduce(b,c,nn,MTYPER,mpi_max,
     &                       mpi_comm_hycom_,mpierr)
        else
          call mpi_reduce(   b,c,nn,MTYPER,mpi_max,
     &                       idproc1(mnflg),
     &                       mpi_comm_hycom_,mpierr)
        endif
#elif defined(SHMEM)
        BARRIER
        mnp = max(1,mnflg)
        if     (mnproc.eq.mnp) then
c         form global maximum on one processor
          do i= 1,nn
            c(i) = b(i)
          enddo
          do mn= 1,ijpr
            if     (mn.ne.mnp) then
              call SHMEM_GETR(b,b,nn, idproc1(mn))
              do i= 1,nn
                c(i) = max(c(i),b(i))
              enddo
            endif !.ne.mnp
          enddo
          BARRIER
        elseif (mnflg.eq.0) then
c         get global maximum from 1st processor
          BARRIER
          call SHMEM_GETR(c,c,nn, idproc1(mnp))
        endif
        ! no barrier needed here because using two buffers (b and c)
#endif
        if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
          do i= 1,nn
            a(is0+i) = c(i)
          enddo
        endif
      enddo  ! stripmine loop
#if defined(TIMER)
c
      if     (nxc.eq.10) then
        call xctmr1(10)
        nxc = 0
      endif
#endif
      return
      end subroutine xcmaxr_1

      subroutine xcmaxr_0o(a)
      implicit none
c
      real, intent(inout) :: a
c
c**********
c*
c  1) replace scalar a with its element-wise maximum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c*
c**********
c
      integer mnflg
      real    a1(1)
c
      mnflg = 0  !all nodes
      a1(1) = a
      call xcmaxr_1(a1, mnflg)
      a = a1(1)
      return
      end subroutine xcmaxr_0o

      subroutine xcmaxr_1o(a)
      implicit none
c
      real, intent(inout) :: a(:)
c
c**********
c*
c  1) replace array a with its element-wise maximum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c*
c**********
c
      integer mnflg
c
      mnflg = 0  !all nodes
      call xcmaxr_1(a, mnflg)
      return
      end subroutine xcmaxr_1o

      subroutine xcminr_0(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace scalar a with its element-wise minimum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c    mnflg           integer        input     node return flag
c*
c**********
c
      real a1(1)
c
      a1(1) = a
      call xcminr_1(a1, mnflg)
      a = a1(1)
      return
      end subroutine xcminr_0

      subroutine xcminr_1(a, mnflg)
      implicit none
c
      real,    intent(inout) :: a(:)
      integer, intent(in)    :: mnflg
c
c**********
c*
c  1) replace array a with its element-wise minimum over all tiles.
c
c  2) mnflg selects which nodes must return the minimum
c        = 0; all nodes
c        = n; node number n (mnproc=n)
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    mnflg           integer        input     node return flag
c*
c**********
c
      integer i,is0,isl,mn,mnp,n,nn
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0(10)
        nxc = 10
      endif
#endif
c
c     stripmine a.
c
      n = size(a)
c
      do is0= 0,n-1,nmax
        isl = min(is0+nmax,n)
        nn = isl - is0
        do i= 1,nn
          b(i) = a(is0+i)
        enddo
c
#if defined(MPI)
        if     (mnflg.eq.0) then
          call mpi_allreduce(b,c,nn,MTYPER,mpi_min,
     &                       mpi_comm_hycom_,mpierr)
        else
          call mpi_reduce(   b,c,nn,MTYPER,mpi_min,
     &                       idproc1(mnflg),
     &                       mpi_comm_hycom_,mpierr)
        endif
#elif defined(SHMEM)
        BARRIER
        mnp = max(1,mnflg)
        if     (mnproc.eq.mnp) then
c         form global minimum on one processor
          do i= 1,nn
            c(i) = b(i)
          enddo
          do mn= 1,ijpr
            if     (mn.ne.mnp) then
              call SHMEM_GETR(b,b,nn, idproc1(mn))
              do i= 1,nn
                c(i) = min(c(i),b(i))
              enddo
            endif !.ne.mnp
          enddo
          BARRIER
        elseif (mnflg.eq.0) then
c         get global minimum from 1st processor
          BARRIER
          call SHMEM_GETR(c,c,nn, idproc1(mnp))
        endif
        ! no barrier needed here because using two buffers (b and c)
#endif
        if     (mnflg.eq.0 .or. mnflg.eq.mnproc) then
          do i= 1,nn
            a(is0+i) = c(i)
          enddo
        endif
      enddo
#if defined(TIMER)
c
      if     (nxc.eq.10) then
        call xctmr1(10)
        nxc = 0
      endif
#endif
      return
      end subroutine xcminr_1

      subroutine xcminr_0o(a)
      implicit none
c
      real, intent(inout) :: a
c
c**********
c*
c  1) replace scalar a with its element-wise minimum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target variable
c*
c**********
c
      integer mnflg
      real    a1(1)
c
      mnflg = 0  !all nodes
      a1(1) = a
      call xcminr_1(a1, mnflg)
      a = a1(1)
      return
      end subroutine xcminr_0o

      subroutine xcminr_1o(a)
      implicit none
c
      real, intent(inout) :: a(:)
c
c**********
c*
c  1) replace array a with its element-wise minimum over all tiles.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c*
c**********
c
      integer mnflg
c
      mnflg = 0  !all nodes
      call xcminr_1(a, mnflg)
      return
      end subroutine xcminr_1o

#if defined(OCEANS2)
      subroutine xcpipe(aa_master,what_master, aa_slave,what_slave)
      implicit none
c
      real,         intent(out) :: aa_master(itdm,jtdm)
      real,         intent(in)  ::  aa_slave(itdm,jtdm)
      character*12, intent(out) :: what_master
      character*12, intent(in)  :: what_slave
c
c**********
c*
c  1) copy aa_slave on task idp1_2 to aa_master on task idp1_1
c     only called from pipe_compare
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    aa_master       real           output    non-tiled target array
c    aa_slave        real            input    non-tiled source array
c    what_master     character*12   output    non-tiled target array name
c    what_slave      character*12    input    non-tiled source array name
c*
c**********
c
      include 'mpif.h'
      integer mpierr
c
      integer iwhat(12)
      save    iwhat
c
      integer i
c
c     only the 1st task on master and slave are involved in the copy.
c     note the use of mpi_comm_world
c
      if     (mnproc.eq.1) then
        if     (nocean.eq.1) then
c ---     master
          call MPI_RECV(iwhat,12,MTYPEI,
     &                  idp1_2, 9943,
     &                  mpi_comm_world, MPI_STATUS_IGNORE, mpierr)
          do i= 1,12
            what_master(i:i) = char(iwhat(i))
          enddo !i
          call MPI_RECV(aa_master,itdm*jtdm,MTYPER,
     &                  idp1_2, 9944,
     &                  mpi_comm_world, MPI_STATUS_IGNORE, mpierr)
        else  !nocean==2
c ---     slave
          do i= 1,12
            iwhat(i) = ichar(what_slave(i:i))
          enddo !i
          call MPI_SEND(iwhat,12,MTYPEI,
     &                idp1_1, 9943,
     &                mpi_comm_world, mpierr)
          call MPI_SEND(aa_slave,itdm*jtdm,MTYPER,
     &                idp1_1, 9944,
     &                mpi_comm_world, mpierr)
        endif !master:slave
      endif !mnproc==1
      return
      end subroutine xcpipe
#endif /* OCEANS2 */

#if defined(USE_ESMF)
      subroutine xcspmd(mpi_comm_vm)
      implicit none
c
      integer mpi_comm_vm
#else
      subroutine xcspmd
#if defined(USE_CCSM3)
      use ccsm3
      use ccsm3_io
      endif
#endif
      implicit none
#endif
c
c**********
c*
c  1) initialize data structures that identify the tiles.
c
c  2) data structures (public):
c      ipr     - 1st 2-D node dimension (<=iqr)
c      jpr     - 2nd 2-D node dimension (<=jqr)
c      ijpr    -     1-D node dimension (ipr*jpr)
c      mproc   - 1st 2-D node index
c      nproc   - 2nd 2-D node index
c      mnproc  -     1-D node index
c      mp_1st  - 1st node in this row of 2-D nodes,        mpe_1(nproc)
c      i0      - 1st dimension offset for this tile, i0_pe(mproc,nproc)
c      ii      - 1st dimension extent for this tile, ii_pe(mproc,nproc)
c      j0      - 2nd dimension offset for this tile, j0_pe(mproc,nproc)
c      jj      - 2nd dimension extent for this tile, jj_pe(mproc,nproc)
c      nreg    -     region type
c      vland   -     fill value for land (standard value 0.0)
c
c  3) data structures (private):
c      idproc  -     2-D node addresses, with periodic wrap
c      idproc1 -     1-D node addresses, with periodic wrap
c      idhalo  -     left and right halo target nodes
c      i0_pe   - 1st dimension tile offsets
c      ii_pe   - 1st dimension tile extents (<=idm)
c      j0_pe   - 2nd dimension tile offsets
c      jj_pe   - 2nd dimension tile extents (<=jdm)
c      mpe_1   - 1st node in each row of 2-D nodes
c      mpe_e   - end node in each row of 2-D nodes
c      mpe_i   - mapping from 1st global dimension to 2-D nodes
c      npe_j   - mapping from 2nd global dimension to 2-D nodes
c      i1sum   - local index of 1st partial sum on each tile
c      iisum   - number of partial sums on each tile
c      m0_top  - tile offset:       top neighbors (0:jpr-1)
c      mm_top  - tile extent:       top neighbors (<=jpr)
c      i0_st   - halo offsets: send top neighbors
c      ii_st   - halo lengths: send top neighbors
c      i0_gt   - halo offsets:  get top neighbors
c      ii_gt   - halo lengths:  get top neighbors
c      m0_bot  - tile offset:       bot neighbors (0:jpr-1)
c      mm_bot  - tile extent:       bot neighbors (<=jpr)
c      i0_sb   - halo offsets: send bot neighbors
c      ii_sb   - halo lengths: send bot neighbors
c      i0_gb   - halo offsets:  get bot neighbors
c      ii_gb   - halo lengths:  get bot neighbors
c
c  4) all data structures are based on the processor number and
c     the patch distribution file, 'patch.input'.
c*
c**********
c
      integer i,idm_in,itdm_in,ios,ixsum,
     &        j,jdm_in,jtdm_in,l,m,mm,mn,mypei,n,npesi
c
      character*80 cfile
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
c     standard mpi (message passing) version.
c
      call mpi_init(mpierr)
#if defined(USE_ESMF)
      call mpi_comm_dup(mpi_comm_vm,    mpi_comm_hycom, mpierr)
      lp = 6
#elif defined(USE_CCSM3)
      call mpi_comm_dup(mpi_comm_ocn,   mpi_comm_hycom, mpierr)
      lp = 6
c<-hsk
#elif defined(USE_SCOUPLER)
      call OC_CMP_START
      mpi_comm_hycom_=mpi_comm_ocean
      lp = 6
c-> hsk
#elif defined(OCEANS2)
c --- two copies of HYCOM on distinct MPI tasks, master/slave via mod_pipe.
      call mpi_init(mpierr)
      call mpi_comm_rank(mpi_comm_world, mypei, mpierr)
      open(unit=uoff+99,file='./patch.input', iostat=ios)
      if     (ios.ne.0) then
        if     (mypei.eq.0) then
          write(6,'(a)') "xcspmd: can't open ./patch.input"
          call flush(6)
        endif !1st task
        call mpi_finalize(mpierr)
        stop '(xcspmd)'
      endif !ios!=0
      read(uoff+99,'(/i6/)',iostat=ios) ijpr
      if     (ios.ne.0) then
        if     (mypei.eq.0) then
          write(6,'(a)') "xcspmd: can't read ./patch.input"
          call flush(6)
        endif !1st task
        call mpi_finalize(mpierr)
        stop '(xcspmd)'
      endif !ios!=0
      close(uoff+99)
      idp1_1 = 0     !1st mpi task of 1st ocean
      idp1_2 = ijpr  !1st mpi task of 2nd ocean
      if     (mypei.lt.idp1_2) then
        nocean = 1  !master ocean
        call mpi_comm_split(mpi_comm_world, nocean,mypei,
     &                      mpi_comm_hycom, mpierr)
        lp = 6
      else
        nocean = 2  !slave ocean
        call mpi_comm_split(mpi_comm_world, nocean,mypei,
     &                      mpi_comm_hycom, mpierr)
        lp = uoff+6
        write(cfile,"(a,i6.6,a)") "./OCEAN2/stdout",mypei+1,".log"
        open(unit=lp,file=trim(cfile),status='new')
      endif
#else
      mpi_comm_hycom_=mpi_comm_hycom
      call mpi_init(mpierr)
      call mpi_comm_dup(mpi_comm_world, mpi_comm_hycom_, mpierr)
      lp = 6
#endif
c
      call mpi_comm_rank(mpi_comm_hycom_, mypei, mpierr)
      call mpi_comm_size(mpi_comm_hycom_, npesi, mpierr)
c
      mnproc = mypei + 1  ! mnproc counts from 1
#if defined(DEBUG_ALL)
      write(lp,'(a,i5)') 'mnproc =',mnproc
      call xcsync(flush_lp)
#endif
#elif defined(SHMEM)
      integer SHMEM_MYPE,SHMEM_NPES
c
c     shmem version.
c
      call start_pes(0)
c
      lp = 6
c
      mypei = SHMEM_MYPE()
      npesi = SHMEM_NPES()
c
      mnproc = mypei + 1  ! mnproc counts from 1
#else
      lp = 6
c
      write(lp,*)
      write(lp,*) '***** ERROR - UNDEFINED SPMD MACHINE TYPE *****'
      write(lp,*)
      call flush(lp)
      stop '(xcspmd)'
#endif
c
      vland  = 0.0
      vland4 = 0.0
c
#if defined(T3E)
      open(unit=lp,delim='none')  ! forces open on stdout
#endif
c
c     initialize timers early to allow xcstop to work.
c
      call xctmri
c
c     read in the tile locations and sizes.
c     patch distibution file on unit 21 (fort.21).
c
c     here is an example of a patch file, with a leading "!" added:
c
!  npes   npe   mpe   idm   jdm  ibig  jbig  nreg
!    16     4     4    57    52    19    15     0
!
!ispt(  1) =     22    35    42    49
!iipe(  1) =     13     7     7     7
!ispt(  2) =      1    15    25    34
!iipe(  2) =     14    10     9     9
!ispt(  3) =      2    21    29    38
!iipe(  3) =     19     8     9    10
!ispt(  4) =     18    28    35    42
!iipe(  4) =     10     7     7     9
! 
!jspt(  1) =      1    15    26    38
!jjpe(  1) =     14    11    12    15
c
c     ispt(j) is the 1st i-point on each tile in the j-th row
c     iipe(j) is the i-extent    of each tile in the j-th row
c     jspt(1) is the 1st j-point on each tile in all columns
c     jjpe(1) is the j-extent    of each tile in all columns
c
c     note that each tile row can have a different tile layout, 
c     but each tile column is identical.  So a tile can have at
c     most one nieghbor in the E and W directions, but several
c     nieghbors in the N and S directions.
c
c     iipe can be zero, indicating an empty tile (not included in the
c     active tile count, npes) and therefore at least a halo widths
c     land separation between active tiles to its east and west.  In
c     periodic cases, the last non-empty tile can periodic wrap to the
c     first tile in the row (i.e. trailing "empty" tiles can be null
c     tiles, rather than all-land tiles).
c
#if defined(USE_CCSM3)
      open(unit=uoff+99,file=trim(flnmptchd)//'patch.input',
     &     iostat=ios)
#elif defined(OCEANS2)
      if     (nocean.eq.2) then
        open(unit=uoff+99,file="./OCEAN2/patch.input",
     &       iostat=ios)
      else
        open(unit=uoff+99,file='./patch.input',
     &       iostat=ios)
      endif
#else
      open(unit=uoff+99,file='./patch.input',
     &     iostat=ios)
#endif
      if     (ios.ne.0) then
        call xcstop('xcspmd: error opening patch.input')
        stop '(xcspmd)'
      endif
      read(uoff+99,'(/8i6/)',iostat=ios) ijpr,ipr,jpr,
     &                  itdm_in,jtdm_in,idm_in,jdm_in,nreg
      if     (ios.ne.0) then
        call xcstop('xcspmd: error reading patch.input')
        stop '(xcspmd)'
      elseif (ijpr.gt.ijqr .or. ipr.gt.iqr .or. jpr.gt.jqr) then
        if     (mnproc.eq.1) then
          write(lp,'(a,3i5)') 'input: ijpr,ipr,jpr =',ijpr,ipr,jpr
          write(lp,'(a,3i5)') 'param: ijqr,iqr,jqr =',ijqr,iqr,jqr
          call flush(lp)
        endif
        call xcstop('xcspmd: patch.input for wrong ipr,jpr,ijpr')
        stop '(xcspmd)'
#if defined(ARCTIC)
      elseif (nreg.ne.3) then  ! not arctic
        if     (mnproc.eq.1) then
          write(lp,'(a,i5)') 'input: nreg =',nreg
          call flush(lp)
        endif
        call xcstop('xcspmd: patch.input must be for arctic')
        stop '(xcspmd)'
#else
      elseif (nreg.lt.0 .or. nreg.gt.2) then  ! not closed or periodic
                                              ! use TYPE=one/omp for f-plane 
        if     (mnproc.eq.1) then
          write(lp,'(a,i5)') 'input: nreg =',nreg
          call flush(lp)
        endif
        call xcstop('xcspmd: patch.input for wrong nreg')
        stop '(xcspmd)'
#endif /* ARCTIC:else */
      endif
c
#if defined(RELO)
c --- region's horizontal dimensions are from patch.input:
c
      itdm = itdm_in
      jtdm = jtdm_in
      idm  = idm_in
      jdm  = jdm_in
#else
      if     (itdm_in.ne.itdm .or. jtdm_in.ne.jtdm) then
        if     (mnproc.eq.1) then
          write(lp,'(a,2i5)') 'input: itdm,jtdm =',itdm_in,jtdm_in
          write(lp,'(a,2i5)') 'param: itdm,jtdm =',itdm,   jtdm
          call flush(lp)
        endif
        call xcstop('xcspmd: patch.input for wrong itdm,jtdm')
        stop '(xcspmd)'
      elseif (idm_in.gt.idm .or. jdm_in.gt.jdm) then
        if     (mnproc.eq.1) then
          write(lp,'(a,2i5)') 'input: idm,jdm =',idm_in,jdm_in
          write(lp,'(a,2i5)') 'param: idm,jdm =',idm,   jdm
          call flush(lp)
        endif
        call xcstop('xcspmd: patch.input for wrong idm,jdm')
        stop '(xcspmd)'
      endif
#endif
c
c     individual tile rows.
c
      do n= 1,jpr
        read( uoff+99,'(12x,8i6)') (i0_pe(m,n),m=1,ipr)  ! ispt=i0+1
        read( uoff+99,'(12x,8i6)') (ii_pe(m,n),m=1,ipr)  ! iipe
        if (maxval(ii_pe(1:ipr,n)).le.0) then
          call xcstop('xcspmd: patch.input has an empty row')
          stop '(xcspmd)'
        endif
        do m= 1,ipr
          i0_pe(m,n) = i0_pe(m,n) - 1
        enddo
      enddo
#if defined(ARCTIC)
c
c --- all arctic patch tiles must be the same size or empty,
c --- and empty tiles must be "twinned" across the top boundary.
c
      if     (ipr.gt.1) then
        do m= 1,ipr
          if     (ii_pe(m,jpr).eq.0) then
            if     (ii_pe(ipr+1-m,jpr).ne.0) then
              if     (mnproc.eq.1) then
                write(lp,'(a,i3,a,i3,a)') 
     &           'error - tile',m,',jpr is empty but tile',
     &                    ipr+1-m,',jpr is not'
                call flush(lp)
              endif
              call xcstop('xcspmd: arctic empty tiles are not twins')
              stop '(xcspmd)'
            endif
          elseif (ii_pe(m,jpr).ne.itdm/ipr) then
            if     (mnproc.eq.1) then
              write(lp,'(a,i5)') 
     &         'error - arctic patch tiles should have ii =',itdm/ipr
              call flush(lp)
            endif
            call xcstop('xcspmd: arctic tiles are not the right size')
            stop '(xcspmd)'
          endif
        enddo !m
      endif
#endif /* ARCTIC */
c
c     the generic tile column (must cover entire column).
c
      read( uoff+99,*)
      read( uoff+99,'(12x,8i6)') (j0_pe(1,n),n=1,jpr)  ! jspt = io+1
      read( uoff+99,'(12x,8i6)') (jj_pe(1,n),n=1,jpr)  ! jjpe
      if     (j0_pe(1,1).ne.1) then
        call xcstop('xcspmd: patch.input for wrong jspt')
        stop '(xcspmd)'
      endif 
      j0_pe(1,1) = 0
      do n= 2,jpr
        j0_pe(1,n) = j0_pe(1,n) - 1
        if (j0_pe(1,n).ne.j0_pe(1,n-1)+jj_pe(1,n-1)) then
          call xcstop('xcspmd: patch.input non-contiguous')
          stop '(xcspmd)'
        endif
      enddo
      if     (j0_pe(1,jpr)+jj_pe(1,jpr).ne.jtdm) then
        call xcstop('xcspmd: patch.input for wrong jjpe')
        stop '(xcspmd)'
      endif 
      do m= 2,ipr
        j0_pe(m,:) = j0_pe(1,:)
        jj_pe(m,:) = jj_pe(1,:)
      enddo
      close(uoff+99)
c
#if defined(RELO)
      allocate( mpe_i(0:itdm+1,0:jqr), npe_j(0:jtdm+1) )
      call mem_stat_add( (itdm+2)*(jqr+1)+jtdm+2 )
#endif
c
#if defined(MPI)
c
c     do we have the right number of pes?
c
      if     (npesi.ne.ijpr) then
        if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) '***** ERROR - WRONG MPI SIZE *****'
          write(lp,*)
          write(lp,*) 'NPES    = ',npesi
          write(lp,*) '   IJPR = ',    ijpr
          write(lp,*) 'IPR,JPR = ',ipr,jpr
          write(lp,*)
          call flush(lp)
        endif
        call xcstop('Error in xcspmd')
        stop
      endif
c
c     mpi messages are sent and received by pe number (0:ijpr-1).
c
      null_tile = mpi_proc_null
c
      mn = 0
      do n= 1,jpr
        mpe_1(n) = 0
        do m= 1,ipr
          if     (ii_pe(m,n).eq.0) then
            idproc(m,n)   = null_tile
          else
            idproc1(mn+1) = mn
            idproc(m,n)   = mn
            mn = mn + 1
            if     (mnproc.eq.mn) then
              mproc = m
              nproc = n
            endif
            mpe_e(n) = m
            if     (mpe_1(n).eq.0) then
              mpe_1(n) = m
            endif
          endif
        enddo
      enddo
c
      mp_1st = mpe_1(nproc)  !1st node in this row (public)
c
c     mpi-2 i/o group (public), see mod_za.
c
      if     (mproc.eq.mp_1st) then
        i = 1
        call mpi_comm_split(mpi_comm_hycom_, i,0,
     &                      group_1st_in_row, mpierr)
      else
        i = 0
        call mpi_comm_split(mpi_comm_hycom_, i,0,
     &                      group_1st_in_row, mpierr)
        call mpi_comm_free( group_1st_in_row, mpierr)
      endif
#elif defined(SHMEM)
c
c     do we have the right number of pes?
c
      if     (npesi.ne.ijpr) then
        if     (mnproc.eq.1) then
          write(lp,*)
          write(lp,*) '***** ERROR - WRONG SHMEM SIZE *****'
          write(lp,*)
          write(lp,*) 'NPES    = ',npesi
          write(lp,*) '   IJPR = ',    ijpr
          write(lp,*) 'IPR,JPR = ',ipr,jpr
          write(lp,*)
          call flush(lp)
        endif
        call xcstop('Error in xcspmd')
        stop
      endif
c
c     shmem messages are sent and received by pe number (0:ijpr-1).
c
      null_tile = -1
c
      mn = 0
      do n= 1,jpr
        mpe_1(n) = 0
        do m= 1,ipr
          if     (ii_pe(m,n).eq.0) then
            idproc(m,n)   = null_tile
          else
            idproc1(mn+1) = mn
            idproc(m,n)   = mn
            mn = mn + 1
            if     (mnproc.eq.mn) then
              mproc = m
              nproc = n
            endif
            mpe_e(n) = m
            if     (mpe_1(n).eq.0) then
              mpe_1(n) = m
            endif
          endif
        enddo
      enddo
c
      mp_1st = mpe_1(nproc)  !1st node in this row (public)
#endif
c
      if     (mn.ne.ijpr) then
        if     (mnproc.eq.1) then
          write(lp,'(a,i5)') 'input: ijpr =',ijpr
          write(lp,'(a,i5)') 'calc:  ijpr =',mn
          call flush(lp)
        endif
        call xcstop('xcspmd: wrong number of sea tiles')
        stop '(xcspmd)'
      endif
c
      if     (nreg.eq.0) then
c
c       longitudinal tile dimension is closed (not periodic)
c
        do n= 1,jpr
          idproc(    0,n) = null_tile
          idproc(ipr+1,n) = null_tile
        enddo
      else
c
c       longitudinal tile dimension is potentially periodic.
c
        do n= 1,jpr
          idproc(    0,n) = null_tile
          idproc(ipr+1,n) = null_tile
c
          i = maxval((i0_pe(1:ipr,n)+ii_pe(1:ipr,n)))
          if     (i0_pe(1,n).eq.0 .and. i.eq.itdm) then
            idproc(         0,n) = idproc(mpe_e(n),n)
            idproc(mpe_e(n)+1,n) = idproc(       1,n)
          endif
        enddo
      endif
#if defined(ARCTIC)
c
c     must have ipr even or 1 for arctic boundary case.
c
      if     (ipr.gt.1 .and. mod(ipr,2).ne.0) then
        call xcstop('Error in xcspmd (arctic) - ipr must be even')
        stop '(xcspmd)'
      endif
c
c     latitudinal tile dimension is closed/arctic.
c
      do m= 1,ipr
        idproc(m,    0) = null_tile
        idproc(m,jpr+1) = idproc(ipr+1-m,jpr)  !arctic tile mapping
      enddo
      idproc(    0,    0) = null_tile
      idproc(ipr+1,    0) = null_tile
      idproc(    0,jpr+1) = idproc(ipr,jpr+1)
      idproc(ipr+1,jpr+1) = idproc(1,  jpr+1)
#else
c
c     latitudinal tile dimension is closed
c
      do m= 0,ipr+1
        idproc(m,    0) = null_tile
        idproc(m,jpr+1) = null_tile
      enddo
#endif /* ARCTIC:else */
c
c     1-d tiling logic is easier if assumed periodic.
c
      idproc1(     0) = idproc1(ijpr)
      idproc1(ijpr+1) = idproc1(   1)
c
c     mapping from global i,j to mp,np.
c     ia,ja is on tile mpe_i(ia,npe_j(ja)),npe_j(ja), 
c     or on no tile if mpe_i(ia,npe_j(ja)) is 0 or -1.
c
      mpe_i(     :,0) = -1  !for npe_j out of range
      mpe_i(     0,:) = -1  !for i out of range
      mpe_i(itdm+1,:) = -1  !for i out of range
      do n= 1,jpr
        mpe_i(1:itdm,n) = 0  ! default is an empty tile
        do m= 1,ipr  ! i-map potentially varies with n
          if     (ii_pe(m,n).gt.0) then
            do i= i0_pe(m,n)+1,i0_pe(m,n)+ii_pe(m,n)
              mpe_i(i,n) = m
            enddo
            if     (m.ne.ipr) then
              if     (ii_pe(m+1,n).gt.0) then
                do i= i0_pe(m,n)+ii_pe(m,n)+1,i0_pe(m+1,n)
                  mpe_i(i,n) = -1  ! gap between tiles
                enddo
              endif
            endif
          endif
        enddo
        m = 1  ! only one j-map
          do j= j0_pe(m,n)+1,j0_pe(m,n)+jj_pe(m,n)
            npe_j(j)   = n
          enddo
          npe_j(     0) = 0  !for j out of range
          npe_j(jtdm+1) = 0  !for j out of range
      enddo
c
c     do each partial sum on the tile that owns its center point.
c       i1sum - local index of 1st partial sum on each tile
c       iisum - number of partial sums on each tile
c     see xcsum for how i1sum and iisum are used.
c
      ixsum = 0
      do n= 1,jpr
        do m= 1,ipr
          if     (ii_pe(m,n).le.0) then
            i1sum(m,n) =  0
            iisum(m,n) =  0
          else
c ---       partial sum specific definition of idhalo
            idhalo(1) = idproc(m-1,n)
            idhalo(2) = idproc(m+1,n)
            if     (idhalo(1).ne.null_tile .and. m.ne.1) then
              if (i0_pe(m,n).ne.i0_pe(m-1,n)+ii_pe(m-1,n)) then
                idhalo(1) = null_tile
              endif
            endif
            if     (m.eq.mpe_e(n)) then
              idhalo(2) = null_tile
            elseif (idhalo(2).ne.null_tile) then
              if     (i0_pe(m,n)+ii_pe(m,n).ne.i0_pe(m+1,n)) then
                idhalo(2) = null_tile
              endif
            endif
            i1sum(m,n) = -99
            iisum(m,n) =  0
            do i= 1+nbdy,itdm+nbdy,2*nbdy+1
              if     (i0_pe(m,n).lt.i .and.
     &                              i.le.i0_pe(m,n)+ii_pe(m,n)) then
                iisum(m,n) = iisum(m,n) + 1
                if     (iisum(m,n).eq.1) then
                  i1sum(m,n) = i - nbdy - i0_pe(m,n)
                endif
              elseif (idhalo(1).eq.null_tile .and.
     &                i.gt.i0_pe(m,n)-nbdy   .and.
     &                i.le.i0_pe(m,n)             ) then
                iisum(m,n) = iisum(m,n) + 1
                if     (iisum(m,n).eq.1) then
                  i1sum(m,n) = i - nbdy - i0_pe(m,n)
                endif
              elseif (idhalo(2).eq.null_tile          .and.
     &                i.gt.i0_pe(m,n)+ii_pe(m,n)      .and.
     &                i.le.i0_pe(m,n)+ii_pe(m,n)+nbdy      ) then
                iisum(m,n) = iisum(m,n) + 1
                if     (iisum(m,n).eq.1) then
                  i1sum(m,n) = i - nbdy - i0_pe(m,n)
                endif
              endif
            enddo
          endif
          ixsum = max( ixsum, iisum(m,n) )
        enddo !m
      enddo !n
c
c     local tile extents.
c
      i0 = i0_pe(mproc,nproc)
      ii = ii_pe(mproc,nproc)
      j0 = j0_pe(mproc,nproc)
      jj = jj_pe(mproc,nproc)
c
c     left and right halo targets
c
      idhalo(1) = idproc(mproc-1,nproc)
      idhalo(2) = idproc(mproc+1,nproc)
c
      if     (idhalo(1).ne.null_tile .and. mproc.ne.1) then
c
c       is the left tile touching this one?
c
        if (i0.ne.i0_pe(mproc-1,nproc)+ii_pe(mproc-1,nproc)) then
          idhalo(1) = null_tile
        endif
      endif
c
      if     (idhalo(2).ne.null_tile .and. mproc.ne.mpe_e(nproc)) then
c
c       is the right tile touching this one?
c
        if     (i0+ii.ne.i0_pe(mproc+1,nproc)) then
          idhalo(2) = null_tile
        endif
      endif
c
c     local halo exchange data structures
c
c     m0_top - tile offset:       top neighbors
c     mm_top - tile extent:       top neighbors (<=jpr)
c     i0_st  - halo offsets: send top neighbors
c     ii_st  - halo lengths: send top neighbors
c     i0_gt  - halo offsets:  get top neighbors
c     ii_gt  - halo lengths:  get top neighbors
c     m0_bot - tile offset:       bot neighbors
c     mm_bot - tile extent:       bot neighbors (<=jpr)
c     i0_sb  - halo offsets: send bot neighbors
c     ii_sb  - halo lengths: send bot neighbors
c     i0_gb  - halo offsets:  get bot neighbors
c     ii_gb  - halo lengths:  get bot neighbors
c
c     note that send is also receive, and is w.r.t. the local  tile.
c     similarly get  is also put,     and is w.r.t. the remote tile.
c
      if     (nproc.eq.jpr) then
#if defined(ARCTIC)
c       single, same size, top arctic nieghbor
        m0_top   = mproc - 1
        mm_top   =  1
        i0_st(1) =  0
        i0_gt(1) =  0
        ii_st(1) = ii
        ii_gt(1) = ii
#else
c       no top nieghbor (closed boundary)
        m0_top = 0
        mm_top = 0
#endif /* ARCTIC:else */
      else
        n = nproc + 1
        m0_top = 0
        mm_top = 0
        m      = 0
        do i= 1,ii
          if     (mpe_i(i0+i,n).ne.m) then
            if     (mm_top.eq.0) then
              m0_top = mpe_i(i0+i,n) - 1
            elseif (m.ne.-1) then
              ii_st(mm_top) = i-1 - i0_st(mm_top) 
              ii_gt(mm_top) = ii_st(mm_top)
            endif
            m = mpe_i(i0+i,n)
            if     (m.gt.0) then
              mm_top        = mm_top + 1
              i0_st(mm_top) = i-1
              i0_gt(mm_top) = i-1 + i0-i0_pe(m,n)
            elseif (m.eq.0) then
              mm_top        = mm_top + 1
              i0_st(mm_top) = i-1
              i0_gt(mm_top) = i0_gt(mm_top-1) + ii_gt(mm_top-1)
*           elseif (m.eq.-1) then  !do nothing
            endif
          endif
        enddo
        if     (mm_top.gt.0) then
          if     (m.gt.0) then
            ii_st(mm_top) = ii - i0_st(mm_top) 
            ii_gt(mm_top) = ii_st(mm_top)
          elseif (m.eq.0) then
            mm_top = mm_top-1
*         elseif (m.eq.-1) then  !do nothing
          endif
        endif
      endif  !nproc.eq.1:else
c
      if     (nproc.eq.1) then
c       no bottom nieghbor (closed boundary)
        m0_bot = 0
        mm_bot = 0
      else
        n = nproc - 1
        m0_bot = 0
        mm_bot = 0
        m      = 0
        do i= 1,ii
          if     (mpe_i(i0+i,n).ne.m) then
            if     (mm_bot.eq.0) then
              m0_bot = mpe_i(i0+i,n) - 1
            elseif (m.ne.-1) then
              ii_sb(mm_bot) = i-1 - i0_sb(mm_bot) 
              ii_gb(mm_bot) = ii_sb(mm_bot)
            endif
            m = mpe_i(i0+i,n)
            if     (m.gt.0) then
              mm_bot        = mm_bot + 1
              i0_sb(mm_bot) = i-1
              i0_gb(mm_bot) = i-1 + i0-i0_pe(m,n)
            elseif (m.eq.0) then
              mm_bot        = mm_bot + 1
              i0_sb(mm_bot) = i-1
              i0_gb(mm_bot) = i0_gb(mm_bot-1) + ii_gb(mm_bot-1)
*           elseif (m.eq.-1) then  !do nothing
            endif
          endif
        enddo
        if     (mm_bot.gt.0) then
          if     (m.gt.0) then
            ii_sb(mm_bot) = ii - i0_sb(mm_bot) 
            ii_gb(mm_bot) = ii_sb(mm_bot)
          elseif (m.eq.0) then
            mm_bot = mm_bot-1
*         elseif (m.eq.-1) then  !do nothing
          endif
        endif
      endif  !nproc.eq.1:else
c
c     printout the tile data structures.
c
      if     (mnproc.eq.1) then
        write(lp,'(/a)')
     &   'mnproc mproc nproc     i0   ii     j0   jj  i1sum iisum'
        mn = 0
        do n= 1,jpr
          do m= 1,ipr
            if     (ii_pe(m,n).ne.0) then
              mn= mn + 1
              write(lp,'(i6,2i6,i7,i5,i7,i5,i7,i6)')
     &           mn,m,n,
     &           i0_pe(m,n),ii_pe(m,n),
     &           j0_pe(m,n),jj_pe(m,n),
     &           i1sum(m,n),iisum(m,n)
            endif
          enddo !m
        enddo !n
        write(lp,*)
#if defined(ARCTIC)
        write(lp,'(a)')
     &   'mnproc mproc nproc mnarct'
        mn = 0
        do n= 1,jpr
          do m= 1,ipr
            if     (ii_pe(m,n).ne.0) then
              mn= mn + 1
              if     (n.eq.jpr) then
                write(lp,'(i6,2i6,i7)')
     &           mn,m,n,idproc(m,n+1)
              endif
            endif
          enddo !m
        enddo !n
        write(lp,*)
#endif /* ARCTIC */
#if defined(DEBUG_ALL)
        do n= 1,jpr
          write(lp,*) 'mpe_1,mpe_e = ',mpe_1(n),mpe_e(n)
        enddo
        do n= 1,jpr
          write(lp,*) 'mpe_i = ',mpe_i(:,n)
        enddo
        write(lp,*)
        write(lp,*) 'npe_j = ',npe_j(:)
        write(lp,*)
#endif
      endif
      call xcsync(flush_lp)
c
#if defined(DEBUG_ALL)
      do n= 1,jpr
        do m= 1,ipr
          if     (mproc.eq.m .and. nproc.eq.n) then
            write(lp,'(a,2i3,i4,i3,16(i5,i4))')
     &         'm,n,_top,_st = ',
     &          m,n,m0_top,mm_top,(i0_st(l),ii_st(l), l= 1,mm_top)
#if defined(SHMEM)
            write(lp,'(a,2i3,i4,i3,16(i5,i4))')
     &         'm,n,_top,_gt = ',
     &          m,n,m0_top,mm_top,(i0_gt(l),ii_gt(l), l= 1,mm_top)
#endif
            if     (m.eq.ipr) then
              write(lp,*) !blank line
            endif
          endif
          call xcsync(flush_lp)
        enddo !m
        do m= 1,ipr
          if     (mproc.eq.m .and. nproc.eq.n) then
            write(lp,'(a,2i3,i4,i3,16(i5,i4))')
     &         'm,n,_bot,_sb = ',
     &          m,n,m0_bot,mm_bot,(i0_sb(l),ii_sb(l), l= 1,mm_bot)
#if defined(SHMEM)
            write(lp,'(a,2i3,i4,i3,16(i5,i4))')
     &         'm,n,_bot,_gb = ',
     &          m,n,m0_bot,mm_bot,(i0_gb(l),ii_gb(l), l= 1,mm_bot)
#endif
            if     (m.eq.ipr) then
              write(lp,*) !blank line
            endif
          endif
          call xcsync(flush_lp)
        enddo !m
        do m= 1,ipr
          if     (mproc.eq.m .and. nproc.eq.n) then
            write(lp,'(a,2i3,3i5)')
     &         'm,n,id,idhalo   = ',
     &          m,n,idproc(m,n),idhalo
            if     (m.eq.ipr) then
              write(lp,*) !blank line
            endif
          endif
          call xcsync(flush_lp)
        enddo !m
      enddo !n
#endif /* DEBUG_ALL */
#if defined(USE_ESMF)
c
c --- ESMF tiling fills the global array (1:itdm,1:jtdm)
c
      mn=0
      do n=1,jpr
        do m=1,ipr
          if(ii_pe(m,n).ne.0)then
            mn= mn + 1
#if defined(ARCTIC)
c ---       don't include the last row (it is a copy of the previous one)
            deBlockList(2,1,mn) =      j0_pe(m,n)+1
            deBlockList(2,2,mn) = min( j0_pe(m,n)+jj_pe(m,n), jtdm-1 )
#else
            deBlockList(2,1,mn) = j0_pe(m,n)+1
            deBlockList(2,2,mn) = j0_pe(m,n)+jj_pe(m,n)
#endif /* ARCTIC:else */
            if     (m.eq.mpe_1(n)) then
              if     (m.eq.mpe_e(n)) then
c ---           only tile in row
                deBlockList(1,1,mn) = 1
                deBlockList(1,2,mn) = itdm
              else
c ---           1st tile in row, right edge may be extended later
                deBlockList(1,1,mn) = 1
                deBlockList(1,2,mn) = i0_pe(m,n)+ii_pe(m,n)
              endif
            elseif (m.eq.mpe_e(n)) then
c ---         last tile in row
              deBlockList(1,1,mn) = i0_pe(m,n)+1
              deBlockList(1,2,mn) = itdm
              if     (deBlockList(1,1,mn)    .ne.
     &                deBlockList(1,2,mn-1)+1    ) then
c ---           a hole in the HYCOM tiling, over land, is filled here
                deBlockList(1,2,mn-1) = (deBlockList(1,2,mn-1)+
     &                                   deBlockList(1,1,mn)   )/2
                deBlockList(1,1,mn)   = deBlockList(1,2,mn-1)+1
              endif
            else
c ---         middle tile in row, right edge may be extended later
              deBlockList(1,1,mn) = i0_pe(m,n)+1
              deBlockList(1,2,mn) = i0_pe(m,n)+ii_pe(m,n)
              if     (deBlockList(1,1,mn)    .ne.
     &                deBlockList(1,2,mn-1)+1    ) then
c ---           a hole in the HYCOM tiling, over land, is filled here
                deBlockList(1,2,mn-1) = (deBlockList(1,2,mn-1)+
     &                                   deBlockList(1,1,mn)   )/2
                deBlockList(1,1,mn)   = deBlockList(1,2,mn-1)+1
              endif
            endif
          endif
        enddo !m
      enddo !n
c
      if     (mnproc.eq.1) then
        write(lp,'(/a)')
     &   'mnproc mproc nproc   BL11   BL21   BL12   BL22'
        mn = 0
        do n= 1,jpr
          do m= 1,ipr
            if     (ii_pe(m,n).ne.0) then
               mn=mn+1
               write(lp,'(i6,2i6,4i7)')
     &           mn,m,n,
     &           deBlockList(1,1,mn),
     &           deBlockList(2,1,mn),
     &           deBlockList(1,2,mn),
     &           deBlockList(2,2,mn)
            endif
          enddo !m
        enddo !n
        write(lp,*)
      endif !1st tile
      call xcsync(flush_lp)
#endif
c
c     mxsum large enough?
c
#if defined(RELO)
      mxsum = ((idm+4*nbdy)/(2*nbdy+1))*jdm
c
#endif
      if     (ixsum.gt.mxsum) then
        if     (mnproc.eq.1) then
        write(lp,'(a,2i5)') 'mxsum,ixsum =',mxsum,ixsum
        call flush(lp)
        endif
        call xcstop('Error in xcspmd - mxsum too small')
        stop '(xcspmd)'
      endif
c
#if defined(TIMER)
c     initialize timer names.
c
      call xctmrn( 1,'xcaget')
      call xctmrn( 2,'xceget')
      call xctmrn( 3,'xclget')
      call xctmrn( 4,'xcaput')
      call xctmrn( 5,'xcXput')
      call xctmrn( 6,'xcsum ')
      call xctmrn( 9,'xcastr')
      call xctmrn(10,'xcmaxr')
      call xctmrn(12,'xctilr')
#if defined(ARCTIC)
      call xctmrn(13,'xctila')
#endif
#endif
      return
      end subroutine xcspmd

      subroutine xcstop(cerror)
      implicit none
c
      character*(*), intent(in) :: cerror
c
c**********
c*
c  1) stop all processes.
c
c  2) all processes must call this routine.
c     use 'xchalt' for emergency stops.
c
c  3) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    cerror          char*(*)       input     error message
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
c     print active timers.
c
      call xctmrp
c
c     message passing version, set barrier and then stop everything.
c     use xcsync as the barrier so that stdout is flushed.
c     note that the system will hang unless all processes call xcstop.
c
      call xcsync(flush_lp)
      if     (mnproc.eq.1 .and. cerror.ne.' ') then
        write(lp,*) '**************************************************'
        write(lp,*) cerror
        write(lp,*) '**************************************************'
        write(lp,*)
      endif
      call xcsync(flush_lp)
c
#if defined(USE_ESMF) || defined(USE_CCSM3) || defined(OCEANS2) 
c
c --- we may not be running on all processes, so call mpi_abort
c
      if     (mnproc.eq.1) then
        call mpi_abort(mpi_comm_hycom_,9)
      endif
      call xcsync(flush_lp)
#elif defined(MPI)
      write(lp,*) 'mpi_finalize called on processor ',mnproc
      call xcsync(flush_lp)
      call mpi_finalize(mpierr)
c
#endif
#if defined(STOP2003)
c --- Fortran 2003 STOP outputs many lines, replace with exit
      call exit(0)
#else
      stop '(xcstop)'
#endif
      end subroutine xcstop

      subroutine xcsum(sum, a,mask)
      implicit none
c
      real*8,  intent(out)   :: sum
      real,    intent(inout) :: a(   1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mask(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) sum a 2-d array, where mask==1
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    sum             real*8         output    sum of a
c    a               real           input     source array
c    mask            integer        input     mask array
c
c  3) sum is bit for bit reproducable for the same halo size, nbdy.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
      real*8     zero8
      parameter (zero8=0.0)
c
      real*8  sum8
      real    vsave
      integer i,i1,j,l,mp,np
      integer  mpi_comm_hycom_
#if defined(USE_SCOUPLER) 
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 6)
        nxc = 6
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(sum8t)) then
        allocate(    sum8t(mxsum), sum8j(jdm) )
        call mem_stat_add( mxsum +       jdm )
      endif
#endif
c
c     halo update so that 2*nbdy+1 wide strips are on chip.
c
      vsave = vland
      vland = 0.0
      call xctilr(a,1,1, nbdy,0, halo_ps)
      vland = vsave
c
c     row sums in 2*nbdy+1 wide strips.
c
!$OMP PARALLEL DO PRIVATE(j,i1,i,l,sum8)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do l= 1,iisum(mproc,nproc)
          i1   = i1sum(mproc,nproc) + (l-1)*(2*nbdy+1)
          sum8 = zero8
          do i= max(i1,1-nbdy),min(i1+2*nbdy,ii+nbdy,itdm-i0)
            if     (mask(i,j).eq.1) then
              sum8 = sum8 + a(i,j)
            endif
          enddo
          sum8t(l + (j-1)*iisum(mproc,nproc)) = sum8
        enddo
      enddo
!$OMP END PARALLEL DO
c
c     complete row sums on first processor in each row.
c
#if defined(SHMEM)
      BARRIER
#endif
      if     (mproc.eq.mpe_1(nproc)) then
        do j=1,jj
          sum8j(j) = zero8
          do l= 1,iisum(mproc,nproc)
            sum8j(j) = sum8j(j) + sum8t(l + (j-1)*iisum(mproc,nproc))
          enddo
*         write(lp,'(a,i3,i5,f12.2)') 'xcsum: np,j,sum = ',
*    &                                1,j,sum8j(j)
        enddo
c
c       remote sums.
c
        do mp= mpe_1(nproc)+1,mpe_e(nproc)
          l = iisum(mp,nproc)*jj
          if     (l.gt.0) then
#if defined(MPI)
            call MPI_RECV(sum8t,l,MTYPED,
     &                    idproc(mp,nproc), 9900,
     &                    mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(SHMEM)
            call SHMEM_GETD(sum8t,
     &                      sum8t,l,idproc(mp,nproc))
#endif
            do j=1,jj
              do l= 1,iisum(mp,nproc)
                sum8j(j) = sum8j(j) + sum8t(l + (j-1)*iisum(mp,nproc))
              enddo
*             write(lp,'(a,i3,i5,f12.2)') 'xcsum: np,j,sum = ',
*    &                                    mp,j,sum8j(j)
            enddo
          endif
        enddo
#if defined(MPI)
      else
        l = iisum(mproc,nproc)*jj
        if     (l.gt.0) then
          call MPI_SEND(sum8t,l,MTYPED,
     &                  idproc(mpe_1(nproc),nproc), 9900,
     &                  mpi_comm_hycom_, mpierr)
        endif
#endif
      endif
c
c     sum of row sums, on first processor.
c
#if defined(SHMEM)
      BARRIER
#endif
      if     (mnproc.eq.1) then
        sum8 = zero8
        do j= 1,jj
          sum8 = sum8 + sum8j(j)
        enddo
*       write(lp,'(a,i5,f12.2)') 'xcsum: jj,sum = ',jj,sum8
c
        do np= 2,jpr
          mp = mpe_1(np)
#if defined(MPI)
          call MPI_RECV(sum8j,jj_pe(mp,np),MTYPED,
     &                  idproc(mp,np), 9901,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(SHMEM)
          call SHMEM_GETD(sum8j,
     &                    sum8j,jj_pe(mp,np),idproc(mp,np))
#endif
          do j= 1,jj_pe(mp,np)
            sum8 = sum8 + sum8j(j)
          enddo
*         write(lp,'(a,i5,f12.2)') 'xcsum: jj,sum = ',
*    &                             jj_pe(mp,np),sum8
        enddo
        sum8s = sum8
#if defined(MPI)
      elseif (mproc.eq.mpe_1(nproc)) then
        call MPI_SEND(sum8j,jj,MTYPED,
     &                idproc1(1), 9901,
     &                mpi_comm_hycom_, mpierr)
#endif
      endif
c
c     broadcast result to all processors.
c
#if defined(MPI)
      call mpi_bcast(sum8s,1,MTYPED,
     &               idproc1(1),mpi_comm_hycom_,mpierr)
#elif defined(SHMEM)
      BARRIER
      if     (mnproc.ne.1) then
        call SHMEM_GETD(sum8s,
     &                  sum8s,1,idproc1(1))
      endif
#endif
c
      sum = sum8s
#if defined(TIMER)
c
      if     (nxc.eq. 6) then
        call xctmr1( 6)
        nxc = 0
      endif
#endif
      return
      end subroutine xcsum

      subroutine xcsumj(sumj, a,mask)
      implicit none
c
      real*8,  intent(out)   :: sumj(jtdm)
      real,    intent(inout) :: a(   1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
      integer, intent(in)    :: mask(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
c
c**********
c*
c  1) rwo-sum of a 2-d array, where mask==1, on first processor only.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    sumj            real*8         output    row-sum of a
c    a               real           input     source array
c    mask            integer        input     mask array
c
c  3) sum is bit for bit reproducable for the same halo size, nbdy.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
      real*8     zero8
      parameter (zero8=0.0)
c
      real*8  sum8
      real    vsave
      integer i,i1,j,l,mp,np
      integer  mpi_comm_hycom_
#if defined(USE_SCOUPLER) 
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0( 6)
        nxc = 6
      endif
#endif
#if defined(RELO)
c
      if     (.not.allocated(sum8t)) then
        allocate(    sum8t(mxsum), sum8j(jdm) )
        call mem_stat_add( mxsum +       jdm )
      endif
#endif
c
c     halo update so that 2*nbdy+1 wide strips are on chip.
c
      vsave = vland
      vland = 0.0
      call xctilr(a,1,1, nbdy,0, halo_ps)
      vland = vsave
c
c     row sums in 2*nbdy+1 wide strips.
c
!$OMP PARALLEL DO PRIVATE(j,i1,i,l,sum8)
!$OMP&         SCHEDULE(STATIC,jblk)
      do j=1,jj
        do l= 1,iisum(mproc,nproc)
          i1   = i1sum(mproc,nproc) + (l-1)*(2*nbdy+1)
          sum8 = zero8
          do i= max(i1,1-nbdy),min(i1+2*nbdy,ii+nbdy,itdm-i0)
            if     (mask(i,j).eq.1) then
              sum8 = sum8 + a(i,j)
            endif
          enddo
          sum8t(l + (j-1)*iisum(mproc,nproc)) = sum8
        enddo
      enddo
!$OMP END PARALLEL DO
c
c     complete row sums on first processor in each row.
c
#if defined(SHMEM)
      BARRIER
#endif
      if     (mproc.eq.mpe_1(nproc)) then
        do j=1,jj
          sum8j(j) = zero8
          do l= 1,iisum(mproc,nproc)
            sum8j(j) = sum8j(j) + sum8t(l + (j-1)*iisum(mproc,nproc))
          enddo
c          write(lp,'(a,i3,i5,f12.2)') 'xcsum: np,j,sum = ',
c     &                                1,j,sum8j(j)
        enddo
c
c       remote sums.
c
        do mp= mpe_1(nproc)+1,mpe_e(nproc)
          l = iisum(mp,nproc)*jj
          if     (l.gt.0) then
#if defined(MPI)
            call MPI_RECV(sum8t,l,MTYPED,
     &                    idproc(mp,nproc), 9900,
     &                    mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(SHMEM)
            call SHMEM_GETD(sum8t,
     &                      sum8t,l,idproc(mp,nproc))
#endif
            do j=1,jj
              do l= 1,iisum(mp,nproc)
                sum8j(j) = sum8j(j) + sum8t(l + (j-1)*iisum(mp,nproc))
              enddo
c              write(lp,'(a,i3,i5,f12.2)') 'xcsum: np,j,sum = ',
c     &                                    mp,j,sum8j(j)
            enddo
          endif
        enddo
#if defined(MPI)
      else
        l = iisum(mproc,nproc)*jj
        if     (l.gt.0) then
          call MPI_SEND(sum8t,l,MTYPED,
     &                  idproc(mpe_1(nproc),nproc), 9900,
     &                  mpi_comm_hycom_, mpierr)
        endif
#endif
      endif
c
c     send row sums to first processor.
c
#if defined(SHMEM)
      BARRIER
#endif
      if     (mnproc.eq.1) then
        do j= 1,jj
          sumj(j) = sum8j(j)
        enddo
c
        do np= 2,jpr
          mp = mpe_1(np)
#if defined(MPI)
          call MPI_RECV(sum8j,jj_pe(mp,np),MTYPED,
     &                  idproc(mp,np), 9901,
     &                  mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(SHMEM)
          call SHMEM_GETD(sum8j,
     &                    sum8j,jj_pe(mp,np),idproc(mp,np))
#endif
          do j= 1,jj_pe(1,np)
            sumj(j+j0_pe(1,np)) = sum8j(j)
          enddo
        enddo
#if defined(MPI)
      elseif (mproc.eq.mpe_1(nproc)) then
        call MPI_SEND(sum8j,jj,MTYPED,
     &                idproc1(1), 9901,
     &                mpi_comm_hycom_, mpierr)
#endif
      endif
#if defined(TIMER)
c
      if     (nxc.eq. 6) then
        call xctmr1( 6)
        nxc = 0
      endif
#endif
      return
      end subroutine xcsumj

      subroutine xcsync(lflush)
      implicit none
c
      logical, intent(in) :: lflush
c
c**********
c*
c  1) barrier, no processor exits until all arrive (and flush stdout).
c
c  2) some MPI implementations only flush stdout as a collective
c     operation, and hence the lflush=.true. option to flush stdout.
c
c  3) typically this is just a wrapper to the "BARRIER" macro.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
      if     (lflush) then
#if defined(MPI) && defined(AIX) && ! (defined(AIX_NOFL) || defined(USE_ESMF) || defined(USE_CCSM3) || defined(OCEANS2) )
c        call mp_flush(1)  ! flushes stdout, and implies a barrier
! Coupling insertion:->
        call CMP_FLUSH(lp)
!<-Coupling insertion
#else
        call flush(lp)
        BARRIER
#endif
      else
        BARRIER
      endif
      return
      end subroutine xcsync

#if defined(SHMEM) && defined(RINGB)
      subroutine xctbar(ipe1,ipe2)
      implicit none
c
      integer, intent(in) :: ipe1,ipe2
c
c**********
c*
c  1) sync with processors ipe1 and ipe2.
c
c  2) this is a global collective operation, and the calls on ipe1
c     and ipe2 must list this processor as one of the two targets.
c
c  3) this is used in place of a global barrier in halo operations,
c     but it only provides syncronization of one or two processors 
c     with the local processor.
c
c  4) ipe1 and/or ipe2 can be null_tile, to indicate no processor.
c*
c**********
c
      integer    cache_line,ilarge
      parameter (cache_line=32, ilarge=2**30)
c
      integer, save, dimension(cache_line,-1:ijqr-1) :: ibp
c
      integer i
c
      integer icount
      save    icount
      data    icount / -1 /
c
      icount = mod(icount+1,ilarge)
      if     (icount.eq.0) then
        call shmem_barrier_all()
        do i= -1,ijpr-1
          ibp(1,i) = -1
        enddo
        call shmem_barrier_all()
      endif
c
      ibp(1,-1) = icount
      if     (ipe1.ne.null_tile) then
        call shmem_integer_put(ibp(1,mnproc-1),icount,1,ipe1)
      endif
      if     (ipe2.ne.null_tile) then
        call shmem_integer_put(ibp(1,mnproc-1),icount,1,ipe2)
      endif
      call shmem_fence
c
      i = -1
      do while (i.lt.icount)
c       this assignment statement must not be optimized away.
cdir$   suppress
        i = min(ibp(1,ipe1),ibp(1,ipe2))
      enddo
      return
      end subroutine xctbar
#endif /* SHMEM && RINGB */

#if defined(ARCTIC)
      recursive subroutine xctila(a,l1,ld,itype)
      implicit none
c
      integer, intent(in)    :: l1,ld,itype
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ld)
c
c**********
c*
c  1) update the top row of a real array.
c     only needed when importing a tripole grid array.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    l1              integer        input     3rd dim. start index
c    ld              integer        input     3rd dimension of a
c    mh              integer        input     1st (EW) update halo size
c    nh              integer        input     2nd (NS) update halo size
c    itype           integer        input     grid and field type
c
c  3) itype selects both the grid and field type
c        itype= 1; p-grid, scalar field
c        itype=11; p-grid, vector field
c
c  4) this version for a global tripole (arctic bipolar patch) grid
c*
c**********
c
c     halo buffer (in common for enhanced MPI safety).
c
#if defined(RELO)
      real,    save, allocatable, dimension(:,:) :: ai
#else
      integer, parameter :: ilen=idm*kdm
      real,    save, dimension(ilen,2) :: ai
#endif
      integer i,io,k,l,lg0,ls0,lm,m
      real    sarc
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
c
c     persistent communication handles.
c
      integer mpireqa(4*iqr),nreqa,
     &        klnold
      save    mpireqa,       nreqa,
     &        klnold
c
      data nreqa,klnold / 0,0 /
c
      integer mpi_comm_hycom_
#if defined(USE_SCOUPLER)
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
#endif /* MPI */
#if defined(RELO)
      if     (.not.allocated(ai)) then
        allocate(       ai(idm*kdm,2) )
        call mem_stat_add( idm*kdm*2 )
      endif
#endif
c
c --- split large requests into smaller pieces
c
      if     (ld-l1+1.gt.kdm) then
        do k= l1,ld,kdm
          l = min(k+kdm-1,ld)
          call xctila(a,k,l,itype)
        enddo
        return
      endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0(13)
        nxc = 13
      endif
#endif
c
#if defined(SHMEM)
c --- not needed if ipr.eq.1, but simpler to include it
      BARRIER
#endif
      if     (nproc.eq.jpr) then  !tiles involved in arctic boundary
        if     (itype.lt.10) then
          sarc =  1.0
        else
          sarc = -1.0
        endif
c
        if     (ipr.eq.1) then
          do k= l1,ld
            do i= 1,ii
              io = ii-mod(i-1,ii)
              if     (a(io,jj-1,k).ne.vland) then
                a(i,jj,k) = sarc*a(io,jj-1,k)
              else
                a(i,jj,k) = vland
              endif
            enddo !i
          enddo !k
        else
          l = 0
          do i= 1,ii  ! outer loop to simplify multiple neighbor case
            io = ii+1-i !ii:1:-1
            do k= l1,ld
              l = l + 1
              if     (a(io,jj-1,k).ne.vland) then
                ai(l,1) = sarc*a(io,jj-1,k)
              else
                ai(l,1) = vland
              endif
            enddo !k
          enddo !i
*         write(lp,'(a,5i6)') 'xctila - l1,ld,ii,l,mnproc = ',
*    &                                  l1,ld,ii,l,mnproc
c
#if defined(MPI)
          if     (klnold.ne.(ld-l1+1)) then  !new mpi init needed
#if defined(DEBUG_ALL)
*           if     (mproc.eq.mp_1st) then
*             write(lp,'(a,3i6)') 'xctila - l1,ld,itype = ',
*    &                                      l1,ld,itype
*           endif
*           call xcsync(flush_lp)
#endif
            do i= 1,nreqa
              call mpi_request_free(mpireqa(i), mpierr)
            enddo
            klnold = ld-l1+1
c
c           loop through all neigboring tiles.
c
            l = 0
            do m= 1,mm_top
              l   = l + 1
              ls0 = i0_st(m)*(ld-l1+1)
              lm  = ii_st(m)*(ld-l1+1)
              call mpi_send_init(
     &          ai(ls0+1,1),lm,MTYPER,
     &          idproc(m0_top+m,nproc+1), 99053,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            do m= 1,mm_top
              l   = l + 1
              ls0 = i0_st(m)*(ld-l1+1)
              lm  = ii_st(m)*(ld-l1+1)
              call mpi_recv_init(
     &          ai(ls0+1,2),lm,MTYPER,
     &          idproc(m0_top+m,nproc+1), 99053,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            nreqa = l
          endif  !new mpi_init
          if     (nreqa.gt.0) then
            call mpi_startall(nreqa, mpireqa, mpierr)
            call mpi_waitall( nreqa, mpireqa, MPI_STATUSES_IGNORE,
     &                                        mpierr)
          endif
#elif defined(SHMEM)
c
c         loop through all neigboring tiles, barrier issued above.
c
          do m= 1,mm_top
            if     (idproc(m0_top+m,nproc+1).ne.null_tile) then
              lg0 = i0_gt(m)*(ld-l1+1)
              ls0 = i0_st(m)*(ld-l1+1)
              lm  = ii_st(m)*(ld-l1+1)
              if     (nproc.eq.jpr) then
                call SHMEM_GETR(ai(ls0+1,2),
     &                          ai(lg0+1,1),lm,  !buffer 1
     &                          idproc(m0_top+m,nproc+1))
              endif
            endif
          enddo
#endif    /* MPI:SHMEM */
c
          l = 0
          do i= 1,ii  ! outer loop to simplify multiple neighbor case
            do k= l1,ld
              l = l + 1
              a(i,jj,k) = ai(l,2)
            enddo
          enddo
        endif  !ipr.eq.1:else
      endif !nproc.eq.jpr
c
#if defined(TIMER)
      if     (nxc.eq.13) then
        call xctmr1(13)
        nxc = 0
      endif
#endif
      return
      end subroutine xctila
      recursive subroutine xctilr(a,l1,ld,mh,nh,itype)
      implicit none
c
      integer, intent(in)    :: l1,ld,mh,nh,itype
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ld)
c
c**********
c*
c  1) update the tile overlap halo of a real array.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    l1              integer        input     3rd dim. start index
c    ld              integer        input     3rd dimension of a
c    mh              integer        input     1st (EW) update halo size
c    nh              integer        input     2nd (NS) update halo size
c    itype           integer        input     grid and field type
c
c  3) itype selects both the grid and field type
c        itype= 1; p-grid, scalar field
c        itype= 2; q-grid, scalar field
c        itype= 3; u-grid, scalar field
c        itype= 4; v-grid, scalar field
c        itype=11; p-grid, vector field
c        itype=12; q-grid, vector field
c        itype=13; u-grid, vector field
c        itype=14; v-grid, vector field
c
c  4) the global variable vland is returned by halos over land.
c
c  5) this version for a global tripole (arctic bipolar patch) grid
c*
c**********
c
c     halo buffer
c
#if defined(RELO)
      real, save, allocatable, dimension(:,:) :: ai,aj,aia
#else
      integer, parameter :: ilen= idm        *kdm*nbdy+64,
     &                      jlen=(jdm+2*nbdy)*kdm*nbdy+64
      real, save, dimension (ilen,4) :: ai
      real, save, dimension (jlen,4) :: aj
      real, save, dimension (kdm*nbdy+64,2) :: aia
#endif
c
      integer i,io,itynew,j,k,l,lg0,ls0,lm,m,mhl,nhl
      real    sarc
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
c     persistent communication handles.
c
      integer mpireqa(4*iqr),mpireqb(4),nreqa,
     &        klmold,klnold,mhlold,nhlold,ityold
      save    mpireqa,mpireqb,nreqa,
     &        klmold,klnold,mhlold,nhlold,ityold
c
      data nreqa,klmold,klnold,mhlold,nhlold,ityold / 0,0,0,0,0,0 /
c
      integer  mpi_comm_hycom_
#if defined(USE_SCOUPLER) 
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
#endif /* MPI */
#if defined(RELO)
c
      if     (.not.allocated(ai)) then
        allocate(        ai( idm        *kdm*nbdy+64 ,4) )
        call mem_stat_add( ( idm        *kdm*nbdy+64)*4 )
        allocate(        aj((jdm+2*nbdy)*kdm*nbdy+64 ,4) )
        call mem_stat_add( ((jdm+2*nbdy)*kdm*nbdy+64)*4 )
        allocate(      aia( kdm*nbdy+64, 2) )
        call mem_stat_add( (kdm*nbdy+64)*2 )
      endif
#endif
c
c --- split large requests into smaller pieces
c
      if     (ld-l1+1.gt.kdm) then
        do k= l1,ld,kdm
          l = min(k+kdm-1,ld)
          call xctilr(a,k,l,mh,nh,itype)
        enddo
        return
      endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0(12)
        nxc = 12
      endif
#endif
c
      mhl = max(0,min(mh,nbdy))
      nhl = max(0,min(nh,nbdy))
c
      if     (itype.lt.10) then
        sarc =  1.0
      else
        sarc = -1.0
      endif
c
      if     (nhl.gt.0) then
        if     (ipr.eq.1 .and. jpr.eq.1) then
          do k= l1,ld
            do j= 1,nhl
              do i= 1,ii
                a(i, 1-j,k) = vland
              enddo
              if     (itype.eq.1 .or. itype.eq.11) then
                do i= 1,ii
                  io = ii-mod(i-1,ii)
                  if     (a(io,jj-1-j,k).ne.vland) then
                    a(i,jj+j,k) = sarc*a(io,jj-1-j,k)
                  else
                    a(i,jj+j,k) = vland
                  endif
                enddo !i
              elseif (itype.eq.2 .or. itype.eq.12) then
                do i= 1,ii
                  io = mod(ii-(i-1),ii)+1
                  if     (a(io,jj-j,k).ne.vland) then
                    a(i,jj+j,k) = sarc*a(io,jj-j,k)
                  else
                    a(i,jj+j,k) = vland
                  endif
                enddo !i
              elseif (itype.eq.3 .or. itype.eq.13) then
                do i= 1,ii
                  io = mod(ii-(i-1),ii)+1
                  if     (a(io,jj-1-j,k).ne.vland) then
                    a(i,jj+j,k) = sarc*a(io,jj-1-j,k)
                  else
                    a(i,jj+j,k) = vland
                  endif
                enddo !i
              elseif (itype.eq.4 .or. itype.eq.14) then
                do i= 1,ii
                  io = ii-mod(i-1,ii)
                  if     (a(io,jj-j,k).ne.vland) then
                    a(i,jj+j,k) = sarc*a(io,jj-j,k)
                  else
                    a(i,jj+j,k) = vland
                  endif
                enddo !i
              endif !itype
            enddo !j
          enddo !k
        else
          if     (nproc.ne.jpr) then
            l = 0
            do i= 1,ii  ! outer loop to simplify multiple neighbor case
              do k= l1,ld
                do j= 1,nhl
                  l = l + 1
                  ai(l,1) = a(i,jj+1-j,k)
                  ai(l,2) = a(i,     j,k)
                  ai(l,3) = vland
                  ai(l,4) = vland
                enddo !j
              enddo !k
            enddo !i
          elseif (itype.eq.1 .or. itype.eq.11) then !p-grid
            l = 0
            do i= 1,ii  ! outer loop to simplify multiple neighbor case
              io = ii+1-i !ii:1:-1
              do k= l1,ld
                do j= 1,nhl
                  l = l + 1
                  if     (a(io,jj-1-j,k).ne.vland) then
                    ai(l,1) = sarc*a(io,jj-1-j,k)
                  else
                    ai(l,1) = vland
                  endif
                  ai(l,2) = a(i,j,k)
                  ai(l,3) = vland
                  ai(l,4) = vland
                enddo !j
              enddo !k
            enddo !i
          elseif (itype.eq.3 .or. itype.eq.13) then  !u-grid
            l = 0
            do i= 1,ii  ! outer loop to simplify multiple neighbor case
              io = ii+2-i !ii+1:2:-1
              do k= l1,ld
                do j= 1,nhl
                  l = l + 1
                  if     (a(io,jj-1-j,k).ne.vland) then
                    ai(l,1) = sarc*a(io,jj-1-j,k)
                  else
                    ai(l,1) = vland
                  endif
                  ai(l,2) = a(i,j,k)
                  ai(l,3) = vland
                  ai(l,4) = vland
                enddo !j
              enddo !k
            enddo !i
            l = 0
            do k= l1,ld
              do j= 1,nhl
                l = l + 1
                if     (a(1,jj-1-j,k).ne.vland) then
                  aia(l,1) = sarc*a(1,jj-1-j,k)
                else
                  aia(l,1) = vland
                endif
                aia(l,2) = vland
              enddo !j
            enddo !k
          elseif (itype.eq.2 .or. itype.eq.12) then  !q-grid
            l = 0
            do i= 1,ii  ! outer loop to simplify multiple neighbor case
              io = ii+2-i !ii+1:2:-1
              do k= l1,ld
                do j= 1,nhl
                  l = l + 1
                  if     (a(io,jj-j,k).ne.vland) then
                    ai(l,1) = sarc*a(io,jj-j,k)
                  else
                    ai(l,1) = vland
                  endif
                  ai(l,2) = a(i,j,k)
                  ai(l,3) = vland
                  ai(l,4) = vland
                enddo !j
              enddo !k
            enddo !i
            l = 0
            do k= l1,ld
              do j= 1,nhl
                l = l + 1
                if     (a(1,jj-j,k).ne.vland) then
                  aia(l,1) = sarc*a(1,jj-j,k)
                else
                  aia(l,1) = vland
                endif
                aia(l,2) = vland
              enddo !j
            enddo !k
          else  !v-grid
            l = 0
            do i= 1,ii  ! outer loop to simplify multiple neighbor case
              io = ii+1-i  !ii:1:-1
              do k= l1,ld
                do j= 1,nhl
                  l = l + 1
                  if     (a(io,jj-j,k).ne.vland) then
                    ai(l,1) = sarc*a(io,jj-j,k)
                  else
                    ai(l,1) = vland
                  endif
                  ai(l,2) = a(i,j,k)
                  ai(l,3) = vland
                  ai(l,4) = vland
                enddo !j
              enddo !k
            enddo !i
          endif  !itype
*         write(lp,'(a,6i6)') 'xctilr - nhl,l1,ld,ii,l,mnproc = ',
*    &                                  nhl,l1,ld,ii,l,mnproc
*         call xcsync(flush_lp)
c
#if defined(MPI)
          if     (itype.eq.2 .or. itype.eq.12 .or.        !q-grid
     &            itype.eq.3 .or. itype.eq.13     ) then  !u-grid
            itynew = 2
          else
            itynew = 1
          endif
          if     (klnold.ne.(ld-l1+1) .or.
     &            nhlold.ne.nhl       .or.
     &            ityold.ne.itynew        ) then  !new mpi init needed
#if defined(DEBUG_ALL)
*           if     (mnproc.eq.1) then
*             write(lp,'(a,4i6)') 'xctilr - nhl,l1,ld,itype = ',
*    &                                      nhl,l1,ld,itype
*           endif
*           call xcsync(flush_lp)
#endif
            do i= 1,nreqa
              call mpi_request_free(mpireqa(i), mpierr)
            enddo
c
c           loop through all neigboring tiles.
c
            l = 0
            do m= 1,mm_top
              l   = l + 1
              ls0 = i0_st(m)*nhl*(ld-l1+1)
              lm  = ii_st(m)*nhl*(ld-l1+1)
              if     (nproc.ne.jpr) then
                call mpi_send_init(
     &            ai(ls0+1,1),lm,MTYPER,
     &            idproc(m0_top+m,nproc+1), 9905,
     &            mpi_comm_hycom_, mpireqa(l), mpierr)
              else !arctic
                call mpi_send_init(
     &            ai(ls0+1,1),lm,MTYPER,
     &            idproc(m0_top+m,nproc+1), 99051,
     &            mpi_comm_hycom_, mpireqa(l), mpierr)
              endif
            enddo
            if     (nproc.eq.jpr) then !arctic
              if     (itype.eq.2 .or. itype.eq.12 .or.        !q-grid
     &                itype.eq.3 .or. itype.eq.13     ) then  !u-grid
                l   = l + 1
                lm  = nhl*(ld-l1+1)
                call mpi_send_init(
     &            aia(1,1),lm,MTYPER,
     &            idproc(mod(ipr+1-mproc,ipr)+1,nproc), 99052,
     &            mpi_comm_hycom_, mpireqa(l), mpierr)
              endif !q-grid,u-grid
            endif
            do m= 1,mm_bot
              l   = l + 1
              ls0 = i0_sb(m)*nhl*(ld-l1+1)
              lm  = ii_sb(m)*nhl*(ld-l1+1)
              call mpi_send_init(
     &          ai(ls0+1,2),lm,MTYPER,idproc(m0_bot+m,nproc-1), 9906,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            do m= 1,mm_top
              l   = l + 1
              ls0 = i0_st(m)*nhl*(ld-l1+1)
              lm  = ii_st(m)*nhl*(ld-l1+1)
              if     (nproc.ne.jpr) then
                call mpi_recv_init(
     &            ai(ls0+1,4),lm,MTYPER,
     &            idproc(m0_top+m,nproc+1), 9906,
     &            mpi_comm_hycom_, mpireqa(l), mpierr)
              else !arctic
                call mpi_recv_init(
     &            ai(ls0+1,4),lm,MTYPER,
     &            idproc(m0_top+m,nproc+1), 99051,
     &            mpi_comm_hycom_, mpireqa(l), mpierr)
              endif
            enddo
            if     (nproc.eq.jpr) then !arctic
              if     (itype.eq.2 .or. itype.eq.12 .or.        !q-grid
     &                itype.eq.3 .or. itype.eq.13     ) then  !u-grid
                l   = l + 1
                lm  = nhl*(ld-l1+1)
                call mpi_recv_init(
     &            aia(1,2),lm,MTYPER,
     &            idproc(mod(ipr+1-mproc,ipr)+1,nproc), 99052,
     &            mpi_comm_hycom_, mpireqa(l), mpierr)
              endif !q-grid,u-grid
            endif
            do m= 1,mm_bot
              l   = l + 1
              ls0 = i0_sb(m)*nhl*(ld-l1+1)
              lm  = ii_sb(m)*nhl*(ld-l1+1)
              call mpi_recv_init(
     &          ai(ls0+1,3),lm,MTYPER,idproc(m0_bot+m,nproc-1), 9905,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            nreqa = l
          endif
          if     (nreqa.gt.0) then
            call mpi_startall(nreqa, mpireqa, mpierr)
            call mpi_waitall( nreqa, mpireqa, MPI_STATUSES_IGNORE,
     &                                        mpierr)
          endif
#elif defined(SHMEM)
          BARRIER
c
c         loop through all neigboring tiles.
c
          do m= 1,mm_top
            if     (idproc(m0_top+m,nproc+1).ne.null_tile) then
              lg0 = i0_gt(m)*nhl*(ld-l1+1)
              ls0 = i0_st(m)*nhl*(ld-l1+1)
              lm  = ii_st(m)*nhl*(ld-l1+1)
              if     (nproc.ne.jpr) then
                call SHMEM_GETR(ai(ls0+1,4),
     &                          ai(lg0+1,2),lm,
     &                          idproc(m0_top+m,nproc+1))
              else !arctic
                call SHMEM_GETR(ai(ls0+1,4),
     &                          ai(lg0+1,1),lm,  !buffer 1
     &                          idproc(m0_top+m,nproc+1))
              endif
            endif
          enddo
          if     (nproc.eq.jpr) then !arctic
            if     (itype.eq.2 .or. itype.eq.12 .or.        !q-grid
     &              itype.eq.3 .or. itype.eq.13     ) then  !u-grid
              lm  = nhl*(ld-l1+1)
              call SHMEM_GETR(aia(1,2),
     &                        aia(1,1),lm,
     &                        idproc(mod(ipr+1-mproc)+1,nproc))
            endif !q-grid,u-grid
          endif
c
          do m= 1,mm_bot
            if     (idproc(m0_bot+m,nproc-1).ne.null_tile) then
              lg0 = i0_gb(m)*nhl*(ld-l1+1)
              ls0 = i0_sb(m)*nhl*(ld-l1+1)
              lm  = ii_sb(m)*nhl*(ld-l1+1)
              call SHMEM_GETR(ai(ls0+1,3),
     &                        ai(lg0+1,1),lm, idproc(m0_bot+m,nproc-1))
            endif
          enddo
#endif  /* MPI:SHMEM */
c
          if     (nproc.eq.jpr) then  !arctic
            if     (itype.eq.2 .or. itype.eq.12 .or.        !q-grid
     &              itype.eq.3 .or. itype.eq.13     ) then  !u-grid
              l  = 0
              do k= l1,ld
                do j= 1,nhl
                  l  = l  + 1
                  ai(l,4) = aia(l,2)
                enddo !j
              enddo !k
            endif !q-grid,u-grid
          endif !arctic
          l = 0
          do i= 1,ii  ! outer loop to simplify multiple neighbor case
            do k= l1,ld
              do j= 1,nhl
                l = l + 1
                a(i, 1-j,k) = ai(l,3)
                a(i,jj+j,k) = ai(l,4)
              enddo
            enddo
          enddo
        endif  ! jpr.eq.1:else
      endif  ! nhl.gt.0
c
      if     (mhl.gt.0) then
        if     (ipr.eq.1) then
          if     (nreg.eq.0) then
            do k= l1,ld
              do j= 1-nhl,jj+nhl
                do i= 1,mhl
                  a( 1-i,j,k) = vland
                  a(ii+i,j,k) = vland
                enddo
              enddo
            enddo
          else
            do k= l1,ld
              do j= 1-nhl,jj+nhl
                do i= 1,mhl
                  a( 1-i,j,k) = a(ii+1-i,j,k)
                  a(ii+i,j,k) = a(     i,j,k)
                enddo
              enddo
            enddo
          endif
        else
          l = 0
          do k= l1,ld
            do j= 1-nhl,jj+nhl
              do i= 1,mhl
                l = l + 1
                aj(l,1) = a(ii+1-i,j,k)
                aj(l,2) = a(     i,j,k)
                aj(l,3) = vland
                aj(l,4) = vland
              enddo
            enddo
          enddo
*         write(lp,'(a,6i6)') 'xctilr - mhl,l1,ld,jj,l,mnproc = ',
*    &                                  mhl,l1,ld,jj,l,mnproc
*         call xcsync(flush_lp)
#if defined(MPISR)
          call mpi_sendrecv(
     &          aj(1,1),l,MTYPER,idhalo(2), 9907,
     &          aj(1,4),l,MTYPER,idhalo(2), 9908,
     &          mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          call mpi_sendrecv(
     &          aj(1,2),l,MTYPER,idhalo(1), 9908,
     &          aj(1,3),l,MTYPER,idhalo(1), 9907,
     &          mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(MPI)
          if     (klmold.ne.(ld-l1+1) .or.
     &            mhlold.ne.mhl       .or.
     &            nhlold.ne.nhl           ) then  !new mpi init
#if defined(DEBUG_ALL)
*           if     (mnproc.eq.1) then
*             write(lp,'(a,3i6)') 'xctilr - mhl,l1,ld       = ',
*    &                                      mhl,l1,ld
*           endif
*           call xcsync(flush_lp)
#endif
            if     (klmold.ne.0) then
              call mpi_request_free(mpireqb(1), mpierr)
              call mpi_request_free(mpireqb(2), mpierr)
              call mpi_request_free(mpireqb(3), mpierr)
              call mpi_request_free(mpireqb(4), mpierr)
            endif
            call mpi_send_init(
     &            aj(1,1),l,MTYPER,idhalo(2), 9907,
     &            mpi_comm_hycom_, mpireqb(1), mpierr)
            call mpi_send_init(
     &            aj(1,2),l,MTYPER,idhalo(1), 9908,
     &            mpi_comm_hycom_, mpireqb(2), mpierr)
            call mpi_recv_init(
     &            aj(1,3),l,MTYPER,idhalo(1), 9907,
     &            mpi_comm_hycom_, mpireqb(3), mpierr)
            call mpi_recv_init(
     &            aj(1,4),l,MTYPER,idhalo(2), 9908,
     &            mpi_comm_hycom_, mpireqb(4), mpierr)
          endif
          call mpi_startall(4, mpireqb, mpierr)
          call mpi_waitall( 4, mpireqb, MPI_STATUSES_IGNORE, mpierr)
#elif defined(SHMEM)
          BARRIER_MP
          if     (idhalo(1).ne.null_tile) then
            call SHMEM_GETR(aj(1,3),
     &                      aj(1,1),l,idhalo(1))
          endif
          if     (idhalo(2).ne.null_tile) then
            call SHMEM_GETR(aj(1,4),
     &                      aj(1,2),l,idhalo(2))
          endif
          BARRIER_MP
#endif
          l = 0
          do k= l1,ld
            do j= 1-nhl,jj+nhl
              do i= 1,mhl
                l = l + 1
                a( 1-i,j,k) = aj(l,3)
                a(ii+i,j,k) = aj(l,4)
              enddo
            enddo
          enddo
        endif  ! ipr.eq.1:else
      endif  ! mhl.gt.0
c
      klnold = ld-l1+1
      klmold = ld-l1+1
      nhlold = nhl
      mhlold = mhl
      ityold = itynew
#if defined(TIMER)
c
      if     (nxc.eq.12) then
        call xctmr1(12)
        nxc = 0
      endif
#endif
      return
      end subroutine xctilr
#else /* !ARCTIC */
      recursive subroutine xctilr(a,l1,ld,mh,nh,itype)
      implicit none
c
      integer, intent(in)    :: l1,ld,mh,nh,itype
      real,    intent(inout) :: a(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy,ld)
c
c**********
c*
c  1) update the tile overlap halo of a real array.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    a               real           in/out    target array
c    l1              integer        input     3rd dim. start index
c    ld              integer        input     3rd dimension of a
c    mh              integer        input     1st (EW) update halo size
c    nh              integer        input     2nd (NS) update halo size
c    itype           integer        input     grid and field type
c
c  3) itype selects both the grid and field type
c        itype= 1; p-grid, scalar field
c        itype= 2; q-grid, scalar field
c        itype= 3; u-grid, scalar field
c        itype= 4; v-grid, scalar field
c        itype=11; p-grid, vector field
c        itype=12; q-grid, vector field
c        itype=13; u-grid, vector field
c        itype=14; v-grid, vector field
c     it is ignored here because all types are the same unless
c      the grid includes the arctic ocean
c
c  4) the global variable vland is returned by halos over land.
c*
c**********
c
c     halo buffer.
c
#if defined(RELO)
      real, save, allocatable, dimension (:,:) :: ai,aj
#else
      integer, parameter :: ilen= idm        *kdm*nbdy+64,
     &                      jlen=(jdm+2*nbdy)*kdm*nbdy+64
      real, save, dimension(ilen,4) :: ai
      real, save, dimension(jlen,4) :: aj
#endif
c
      integer i,j,k,l,lg0,ls0,lm,m,mhl,nhl
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
c
c     persistent communication handles.
c
      integer mpireqa(4*iqr),mpireqb(4),
     &        ilold,jlold,nreqa
      save    mpireqa,mpireqb,
     &        ilold,jlold,nreqa
c
      data ilold,jlold / 0,0 /
c
      integer  mpi_comm_hycom_
#if defined(USE_SCOUPLER) 
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
#endif /* MPI */
#if defined(RELO)
c
      if     (.not.allocated(ai)) then
        allocate(        ai( idm        *kdm*nbdy+64 ,4) )
        call mem_stat_add( ( idm        *kdm*nbdy+64)*4 )
        allocate(        aj((jdm+2*nbdy)*kdm*nbdy+64 ,4) )
        call mem_stat_add( ((jdm+2*nbdy)*kdm*nbdy+64)*4 )
      endif
#endif
c
c --- split large requests into smaller pieces
c
      if     (ld-l1+1.gt.kdm) then
        do k= l1,ld,kdm
          l = min(k+kdm-1,ld)
          call xctilr(a,k,l,mh,nh,itype)
        enddo
        return
      endif
c
#if defined(TIMER)
c
      if     (nxc.eq.0) then
        call xctmr0(12)
        nxc = 12
      endif
#endif
c
      mhl = max(0,min(mh,nbdy))
      nhl = max(0,min(nh,nbdy))
c
      if     (nhl.gt.0) then
        if     (jpr.eq.1) then
          do k= l1,ld
            do j= 1,nhl
              do i= 1,ii
                a(i, 1-j,k) = vland
                a(i,jj+j,k) = vland
              enddo
            enddo
          enddo
        else
          l = 0
          do i= 1,ii  ! outer loop to simplify multiple neighbor case
            do k= l1,ld
              do j= 1,nhl
                l = l + 1
                ai(l,1) = a(i,jj+1-j,k)
                ai(l,2) = a(i,     j,k)
                ai(l,3) = vland
                ai(l,4) = vland
              enddo
            enddo
          enddo
*         write(lp,'(a,6i6)') 'xctilr - nhl,l1,ld,ii,l,mnproc = ',
*    &                                  nhl,l1,ld,ii,l,mnproc
*         call xcsync(flush_lp)
c
#if defined(MPI)
          if     (jlold.ne.l) then
            if     (jlold.ne.0) then
              do i= 1,nreqa
                call mpi_request_free(mpireqa(i), mpierr)
              enddo
            endif
            jlold = l
c
c           loop through all neigboring tiles.
c
            l = 0
            do m= 1,mm_top
              l   = l + 1
              ls0 = i0_st(m)*nhl*(ld-l1+1)
              lm  = ii_st(m)*nhl*(ld-l1+1)
              call mpi_send_init(
     &          ai(ls0+1,1),lm,MTYPER,idproc(m0_top+m,nproc+1), 9905,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            do m= 1,mm_bot
              l   = l + 1
              ls0 = i0_sb(m)*nhl*(ld-l1+1)
              lm  = ii_sb(m)*nhl*(ld-l1+1)
              call mpi_send_init(
     &          ai(ls0+1,2),lm,MTYPER,idproc(m0_bot+m,nproc-1), 9906,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            do m= 1,mm_top
              l   = l + 1
              ls0 = i0_st(m)*nhl*(ld-l1+1)
              lm  = ii_st(m)*nhl*(ld-l1+1)
              call mpi_recv_init(
     &          ai(ls0+1,4),lm,MTYPER,idproc(m0_top+m,nproc+1), 9906,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            do m= 1,mm_bot
              l   = l + 1
              ls0 = i0_sb(m)*nhl*(ld-l1+1)
              lm  = ii_sb(m)*nhl*(ld-l1+1)
              call mpi_recv_init(
     &          ai(ls0+1,3),lm,MTYPER,idproc(m0_bot+m,nproc-1), 9905,
     &          mpi_comm_hycom_, mpireqa(l), mpierr)
            enddo
            nreqa = l
          endif
          if     (nreqa.gt.0) then
            call mpi_startall(nreqa, mpireqa, mpierr)
            call mpi_waitall( nreqa, mpireqa, MPI_STATUSES_IGNORE,
     &                                        mpierr)
          endif
#elif defined(SHMEM)
          BARRIER
c
c         loop through all neigboring tiles.
c
          do m= 1,mm_top
            if     (idproc(m0_top+m,nproc+1).ne.null_tile) then
              lg0 = i0_gt(m)*nhl*(ld-l1+1)
              ls0 = i0_st(m)*nhl*(ld-l1+1)
              lm  = ii_st(m)*nhl*(ld-l1+1)
              call SHMEM_GETR(ai(ls0+1,4),
     &                        ai(lg0+1,2),lm, idproc(m0_top+m,nproc+1))
            endif
          enddo
c
          do m= 1,mm_bot
            if     (idproc(m0_bot+m,nproc-1).ne.null_tile) then
              lg0 = i0_gb(m)*nhl*(ld-l1+1)
              ls0 = i0_sb(m)*nhl*(ld-l1+1)
              lm  = ii_sb(m)*nhl*(ld-l1+1)
              call SHMEM_GETR(ai(ls0+1,3),
     &                        ai(lg0+1,1),lm, idproc(m0_bot+m,nproc-1))
            endif
          enddo
#endif  /* MPI:SHMEM */
c
          l = 0
          do i= 1,ii  ! outer loop to simplify multiple neighbor case
            do k= l1,ld
              do j= 1,nhl
                l = l + 1
                a(i, 1-j,k) = ai(l,3)
                a(i,jj+j,k) = ai(l,4)
              enddo
            enddo
          enddo
        endif  ! jpr.eq.1:else
      endif  ! nhl.gt.0
c
      if     (mhl.gt.0) then
        if     (ipr.eq.1) then
          if     (nreg.eq.0) then
            do k= l1,ld
              do j= 1-nhl,jj+nhl
                do i= 1,mhl
                  a( 1-i,j,k) = vland
                  a(ii+i,j,k) = vland
                enddo
              enddo
            enddo
          else
            do k= l1,ld
              do j= 1-nhl,jj+nhl
                do i= 1,mhl
                  a( 1-i,j,k) = a(ii+1-i,j,k)
                  a(ii+i,j,k) = a(     i,j,k)
                enddo
              enddo
            enddo
          endif
        else
          l = 0
          do k= l1,ld
            do j= 1-nhl,jj+nhl
              do i= 1,mhl
                l = l + 1
                aj(l,1) = a(ii+1-i,j,k)
                aj(l,2) = a(     i,j,k)
                aj(l,3) = vland
                aj(l,4) = vland
              enddo
            enddo
          enddo
*         write(lp,'(a,6i6)') 'xctilr - mhl,l1,ld,jj,l,mnproc = ',
*    &                                  mhl,l1,ld,jj,l,mnproc
*         call xcsync(flush_lp)
#if defined(MPISR)
          call mpi_sendrecv(
     &          aj(1,1),l,MTYPER,idhalo(2), 9907,
     &          aj(1,4),l,MTYPER,idhalo(2), 9908,
     &          mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          call mpi_sendrecv(
     &          aj(1,2),l,MTYPER,idhalo(1), 9908,
     &          aj(1,3),l,MTYPER,idhalo(1), 9907,
     &          mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(MPI)
          if     (ilold.ne.l) then
            if     (ilold.ne.0) then
              call mpi_request_free(mpireqb(1), mpierr)
              call mpi_request_free(mpireqb(2), mpierr)
              call mpi_request_free(mpireqb(3), mpierr)
              call mpi_request_free(mpireqb(4), mpierr)
            endif
            ilold = l
            call mpi_send_init(
     &            aj(1,1),l,MTYPER,idhalo(2), 9907,
     &            mpi_comm_hycom_, mpireqb(1), mpierr)
            call mpi_send_init(
     &            aj(1,2),l,MTYPER,idhalo(1), 9908,
     &            mpi_comm_hycom_, mpireqb(2), mpierr)
            call mpi_recv_init(
     &            aj(1,3),l,MTYPER,idhalo(1), 9907,
     &            mpi_comm_hycom_, mpireqb(3), mpierr)
            call mpi_recv_init(
     &            aj(1,4),l,MTYPER,idhalo(2), 9908,
     &            mpi_comm_hycom_, mpireqb(4), mpierr)
          endif
          call mpi_startall(4, mpireqb, mpierr)
          call mpi_waitall( 4, mpireqb, MPI_STATUSES_IGNORE, mpierr)
#elif defined(SHMEM)
          BARRIER_MP
          if     (idhalo(1).ne.null_tile) then
            call SHMEM_GETR(aj(1,3),
     &                      aj(1,1),l,idhalo(1))
          endif
          if     (idhalo(2).ne.null_tile) then
            call SHMEM_GETR(aj(1,4),
     &                      aj(1,2),l,idhalo(2))
          endif
          BARRIER_MP
#endif
          l = 0
          do k= l1,ld
            do j= 1-nhl,jj+nhl
              do i= 1,mhl
                l = l + 1
                a( 1-i,j,k) = aj(l,3)
                a(ii+i,j,k) = aj(l,4)
              enddo
            enddo
          enddo
        endif  ! ipr.eq.1:else
      endif  ! mhl.gt.0
#if defined(TIMER)
c
      if     (nxc.eq.12) then
        call xctmr1(12)
        nxc = 0
      endif
#endif
      return
      end subroutine xctilr
#endif /* ARCTIC:else */

      subroutine xctmri
      implicit none
c
c**********
c*
c  1) initialize timers.
c
c  2) timers  1:32 are for message passing routines,
c     timers 33:80 are for general hycom routines,
c     timers 81:96 are for user selected routines.
c     timer     97 is the total time.
c
c  3) call xctmri    to initialize timers (called in xcspmd),
c     call xctmr0(n) to start timer n,
c     call xctmr1(n) to stop  timer n and add event to timer sum,
c     call xctnrn(n,cname) to register a name for timer n,
c     call xctmrp to printout timer statistics (called by xcstop).
c
c  4) time every 50-th event above 5,000.
c*
c**********
c
      integer i
c
      real*8     zero8
      parameter (zero8=0.0)
c
      nxc = 0
      do i= 1,97
        cc(i) = '      '
        nc(i) = 0
        tc(i) = zero8
      enddo
c
      call xctmrn(97,'total ')
      call xctmr0(97)
      return
      end subroutine xctmri

      subroutine xctmr0(n)
      implicit none
c
      integer, intent(in) :: n
c
c**********
c*
c  1) start timer n.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    n               integer        input     timer number
c
c  3) time every 50-th event above 5,000.
c*
c**********
c
      real*8 wtime
c
#if defined(DEBUG_TIMER_ALL)
      if     (              cc(n).ne.'      ') then
        write(lp,'(i5,2x,a,a)') mnproc,'call ',cc(n)
        call flush(lp)
      endif
#endif
#if defined(DEBUG_TIMER)
      if     (n.gt.32 .and. cc(n).ne.'      ') then
        if     (mnproc.eq.1) then
        write(lp,*) 'call ',cc(n)
        call flush(lp)
        endif
      endif
#endif
      if     (timer_on) then
        if     (mod(nc(n),50).eq.0 .or. nc(n).le.5000) then
          t0(n) = wtime()
        endif
      endif !timer_on
      return
      end subroutine xctmr0

      subroutine xctmr1(n)
      implicit none
c
      integer, intent(in) :: n
c
c**********
c*
c  1) add time since call to xctim0 to timer n.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    n               integer        input     timer number
c
c  3) time every 50-th event above 5,000.
c*
c**********
c
      real*8  wtime
c
      if     (timer_on) then
        if     (nc(n).gt.5000) then
          if     (mod(nc(n),50).eq.0) then
            tc(n) = tc(n) + 50.0*(wtime() - t0(n))
          endif
        else
          tc(n) = tc(n) + (wtime() - t0(n))
        endif
        nc(n) = nc(n) + 1
      endif !timer_on
#if defined(DEBUG_TIMER_ALL)
      if     (              cc(n).ne.'      ') then
        write(lp,'(i5,2x,a,a)') mnproc,'exit ',cc(n)
        call flush(lp)
      endif
#endif
#if defined(DEBUG_TIMER)
      if     (n.gt.32 .and. cc(n).ne.'      ') then
        if     (mnproc.eq.1) then
        write(lp,*) 'exit ',cc(n)
        call flush(lp)
        endif
      endif
#endif
      return
      end subroutine xctmr1

      subroutine xctmrn(n,cname)
      implicit none
c
      character*6, intent(in) :: cname
      integer,     intent(in) :: n
c
c**********
c*
c  1) register name of timer n.
c
c  2) parameters:
c       name            type         usage            description
c    ----------      ----------     -------  ----------------------------
c    n               integer        input     timer number
c    cname           char*(6)       input     timer name
c*
c**********
c
      cc(n) = cname
      return
      end subroutine xctmrn

#if defined(TIMER_ALLOUT)
      subroutine xctmrp
      implicit none
c
c**********
c*
c  1) print all active timers, on all processors.
c
c  2) on exit all timers are reset to zero.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
      real*8       tci
      integer      i,mn
      character*16 cmnproc
c
      real*8     zero8
      parameter (zero8=0.0)
c
      integer  mpi_comm_hycom_
#if defined(USE_SCOUPLER) 
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
c
c     get total time.
c
      call xctmr1(97)
c
      call xcsync(flush_lp)  !includes a barrier for shmem
      if     (mnproc.ne.1) then
#if defined(MPI)
        call MPI_SEND(tc,97,MTYPED,
     &                idproc1(1), 9949,
     &                mpi_comm_hycom_, mpierr)
#endif
      else   !mnproc.eq.1
        do mn= 1,ijpr
          if     (mn.ne.1) then
#if defined(MPI)
            call MPI_RECV(tc,97,MTYPED,
     &                    idproc1(mn), 9949,
     &                    mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
#elif defined(SHMEM)
            call SHMEM_GETD(tc,tc,97,idproc1(mn))
#endif
          endif
          write(lp,6000) mn,ijpr
          tcxc(1) = zero8
          do i= 1,32
            if     (nc(i).ne.0) then
              if     (nc(i).le.5000) then
                tci = tc(i)
              else !correct for timing every 50th event
                tci = (tc(i)*nc(i))/(nc(i)-mod(nc(i),50))
              endif
              if     (cc(i).ne.'      ') then
                write(lp,6100) cc(i),nc(i),tci,tci/nc(i)
              else
                write(lp,6150)    i, nc(i),tci,tci/nc(i)
              endif
              if     (cc(i)(1:2).eq.'xc') then
                tcxc(1) = tcxc(1) + tci  !communication overhead
              endif
            endif !nc(i)>0
          enddo !i
          write(lp,6100) 'xc****',1,tcxc(1),tcxc(1)
          do i= 33,97
            if     (nc(i).ne.0) then
              if     (nc(i).le.5000) then
                tci = tc(i)
              else !correct for timing every 50th event
                tci = (tc(i)*nc(i))/(nc(i)-mod(nc(i),50))
              endif
              if     (cc(i).ne.'      ') then
                write(lp,6100) cc(i),nc(i),tci,tci/nc(i)
              else
                write(lp,6150)    i, nc(i),tci,tci/nc(i)
              endif
            endif
          enddo !i
        enddo !mn
        call mem_stat_print('processor     1:')
      endif !mnproc.ne.1:else
      call xcsync(flush_lp)  !includes a barrier for shmem
      if     (nproc.eq.2 .and. mproc.eq.mpe_1(nproc)) then
        write(cmnproc,'(a,i6,a)') 'processor',mnproc,':'
        call mem_stat_print(cmnproc)
      endif
      call xcsync(flush_lp)
      if     (mnproc.eq.ijpr) then
        write(cmnproc,'(a,i6,a)') 'processor',mnproc,':'
        call mem_stat_print(cmnproc)
        write(lp,*)
      endif
      call xcsync(flush_lp)
c
c     reset timers to zero.
c
      do i= 1,97
        nc(i) = 0
        tc(i) = zero8
      enddo
      tcxc(1) = zero8
c
c     start a new total time measurement.
c
      call xctmr0(97)
      return
c
 6000 format(/ /
     &    3x,' timer statistics, processor',i5,' out of',i5 /
     &    3x,'-----------------------------------------------' /)
 6100 format(3x,a6,
     &   '   calls =',i9,
     &   '   time =',f11.5,
     &   '   time/call =',f14.8)
 6150 format(3x,'   #',i2,
     &   '   calls =',i9,
     &   '   time =',f11.5,
     &   '   time/call =',f14.8)
      end subroutine xctmrp
#else
      subroutine xctmrp
      implicit none
c
c**********
c*
c  1) print all active timers.
c
c  2) on exit all timers are reset to zero.
c*
c**********
c
#if defined(MPI)
      include 'mpif.h'
      integer mpierr
#endif
c
      real*8       tci
      integer      i,mn,mnloc
      character*16 cmnproc
c
      real*8     zero8
      parameter (zero8=0.0)
      integer  mpi_comm_hycom_
#if defined(USE_SCOUPLER) 
      mpi_comm_hycom_=mpi_comm_ocean
#else
      mpi_comm_hycom_=mpi_comm_hycom
#endif
c
c     get total time.
c
      call xctmr1(97)
c
c     report time on the processor with the least communication overhead
c
      tcxc(2) = mnproc
      tcxc(1) = zero8
      do i= 1,32
        if     (nc(i).ne.0 .and. cc(i)(1:2).eq.'xc') then
          tcxc(1) = tcxc(1) + tc(i)  !communication overhead
        endif
      enddo !i
c
      if     (ijpr.ne.1) then
#if defined(MPI)
        tcxl(1) = tcxc(1)
        tcxl(2) = tcxc(2)
        call mpi_allreduce(tcxl,tcxc,1,
     &                     mpi_2double_precision,mpi_minloc,
     &                     mpi_comm_hycom_,mpierr)
        mnloc = tcxc(2)  !processor with the least comm. overhead
        if     (mnproc.eq.1) then
          if     (mnloc.ne.1) then
            call MPI_RECV(tc,97,MTYPED,
     &                    idproc1(mnloc), 9949,
     &                    mpi_comm_hycom_, MPI_STATUS_IGNORE, mpierr)
          endif
        elseif (mnproc.eq.mnloc) then
          call MPI_SEND(tc,97,MTYPED,
     &                  idproc1(1), 9949,
     &                  mpi_comm_hycom_, mpierr)
        endif
#elif defined(SHMEM)
        BARRIER
        if     (mnproc.eq.1) then
          mnloc = 1
          do mn= 2,ijpr
            call SHMEM_GETD(tcxc(2),tcxc(1),1,idproc1(mn))
            if     (tcxc(2).gt.tcxc(1)) then
              tcxc(1) = tcxc(2)
              mnloc   = mn
            endif
          enddo
          tcxc(2) = mnloc  !processor with the least comm. overhead
        endif
        if     (mnloc.ne.1) then
          call SHMEM_GETD(tc,tc,97,idproc1(mnloc))
        endif
        BARRIER
#endif
      endif
c
      call xcsync(flush_lp)
      if     (mnproc.eq.1) then
        write(lp,6000) mnloc,ijpr
        do i= 1,32
          if     (nc(i).ne.0) then
            if     (nc(i).le.5000) then
              tci = tc(i)
            else !correct for timing every 50th event
              tci = (tc(i)*nc(i))/(nc(i)-mod(nc(i),50))
            endif
            if     (cc(i).ne.'      ') then
              write(lp,6100) cc(i),nc(i),tci,tci/nc(i)
            else
              write(lp,6150)    i, nc(i),tci,tci/nc(i)
            endif
            if     (cc(i)(1:2).eq.'xc') then
              tcxc(1) = tcxc(1) + tci  !communication overhead
            endif
          endif !nc(i)>0
        enddo !i
        write(lp,6100) 'xc****',1,tcxc(1),tcxc(1)
        do i= 33,97
          if     (nc(i).ne.0) then
            if     (nc(i).le.5000) then
              tci = tc(i)
            else !correct for timing every 50th event
              tci = (tc(i)*nc(i))/(nc(i)-mod(nc(i),50))
            endif
            if     (cc(i).ne.'      ') then
              write(lp,6100) cc(i),nc(i),tci,tci/nc(i)
            else
              write(lp,6150)    i, nc(i),tci,tci/nc(i)
            endif
          endif
        enddo !i
        call mem_stat_print('processor     1:')
      endif !mnproc.eq.1
      call xcsync(flush_lp)
      if     (nproc.eq.2 .and. mproc.eq.mpe_1(nproc)) then
        write(cmnproc,'(a,i6,a)') 'processor',mnproc,':'
        call mem_stat_print(cmnproc)
      endif
      call xcsync(flush_lp)
      if     (mnproc.eq.ijpr) then
        write(cmnproc,'(a,i6,a)') 'processor',mnproc,':'
        call mem_stat_print(cmnproc)
        write(lp,*)
      endif
      call xcsync(flush_lp)
c
c     reset timers to zero.
c
      do i= 1,97
        nc(i) = 0
        tc(i) = zero8
      enddo
      tcxc(1) = zero8
c
c     start a new total time measurement.
c
      call xctmr0(97)
      return
c
 6000 format(/ /
     &    3x,' timer statistics, processor',i5,' out of',i5 /
     &    3x,'-----------------------------------------------' /)
 6100 format(3x,a6,
     &   '   calls =',i9,
     &   '   time =',f11.5,
     &   '   time/call =',f14.8)
 6150 format(3x,'   #',i2,
     &   '   calls =',i9,
     &   '   time =',f11.5,
     &   '   time/call =',f14.8)
      end subroutine xctmrp
#endif /* TIMER_ALLOUT:else */
c
c  Revision history:
c
c> Nov. 2009 - iisum bugfix for periodic domains
c> Nov. 2011 - time every 50-th event above 5,000 (was 1,000).
c> Mar. 2012 - added optional mnflg to xclput
c> Mar. 2012 - bugfix to periodic case in xclput
c> Apr. 2012 - added optional mnflg to xceget and xceput
c> Apr. 2012 - added xciget and xciput
c> Nov. 2012 - added the OCEANS2 macro and xcpipe
c> Dec. 2012 - iisum bugfix for xcsumj
c> Jan. 2014 - replaced mpistat with MPI_STATUS[ES]_IGNORE
c> Jan. 2014 - ARCTIC xctilr bugfix for when called after xcsum
c> Feb. 2015 - reduced buffering memory requirements
