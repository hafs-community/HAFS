      module mod_dimensions
      implicit none
      public ! everything is public
#if defined(RELO)
      include 'dimensions_relo.h'
c
c --- halo size
      integer, parameter :: nbdy=6
c
c --- ms-1  = max. number of interruptions of any tile row or column by land
      integer, parameter :: ms=99  ! should be enough for any region
c
c --- ijqr  = maximum total number of active tiles (= ipr*jpr)
      integer, parameter :: ijqr=iqr*jqr
c
c --- itdm  = total grid dimension in i direction
c --- jtdm  = total grid dimension in j direction
c --- kdm   =       grid dimension in k direction
c --- kk    =       grid dimension in k direction
      integer, save :: itdm,jtdm,kdm,kk
c
c --- idm   = maximum single tile grid dimension in i direction
c --- jdm   = maximum single tile grid dimension in j direction
      integer, save :: idm,jdm
c
c --- kkwall= grid dimension in k direction for wall relax arrays
c --- kknest= grid dimension in k direction for nest relax arrays
      integer, save :: kkwall,kknest
c
c --- kkmy25= grid dimension in k direction for M-Y 2.5 arrays
      integer, save :: kkmy25
c
c --- natm  = number of saved atmospheric fields
      integer, save :: natm
c
c --- OpenMP will allocate jblk rows to each thread in turn
      integer, save :: jblk
c
c --- for CCSM array dimensions
      integer, save :: imt1,imt2,jmt1,jmt2
c
c --- actual extent of this tile is (i0+1:i0+ii,j0+1:j0+jj,1:kk)
      integer, save :: i0,j0,ii,jj
c
c --- information (gindex) that keeps do loops from running into land
      integer, save, allocatable, dimension (:,:) ::
     &         ip,iu,iv,iq, iuopn,ivopn, ipa, ishlf,
     &         ipim1, ipip1, ipjm1, ipjp1,
     &         ipim1x,ipip1x,ipjm1x,ipjp1x
      integer, save, allocatable, dimension (:,:) ::
     &         ifp,ilp,ifq,ilq,ifu,ilu,ifv,ilv
      integer, save, allocatable, dimension (:,:) ::
     &         jfp,jlp,jfq,jlq,jfu,jlu,jfv,jlv
      integer, save, allocatable, dimension (:) ::
     &         isp,isq,isu,isv
      integer, save, allocatable, dimension (:) ::
     &         jsp,jsq,jsu,jsv
      logical, save, allocatable, dimension (:) ::
     &         allip,alliq,alliu,alliv
#else
      include 'dimensions.h'
c
c --- halo size
      integer, parameter :: nbdy=6
c
c --- OpenMP will allocate jblk rows to each thread in turn
      integer, parameter :: jblk=(jdm+2*nbdy+mxthrd-1)/mxthrd
c
c --- ms-1  = max. number of interruptions of any tile row or column by land
      integer, parameter :: ms=99  ! should be enough for any region
c
c --- ijqr  = maximum total number of active tiles (= ipr*jpr)
      integer, parameter :: ijqr=iqr*jqr
c
c --- for CCSM array dimensions
      integer, parameter :: imt1=1-nbdy,imt2=idm+nbdy,
     &                      jmt1=1-nbdy,jmt2=jdm+nbdy
c
c --- actual extent of this tile is (i0+1:i0+ii,j0+1:j0+jj,1:kk)
      integer, parameter :: kk=kdm
      integer, save      :: i0,j0,ii,jj
c
c --- information to keep do loops from running into land
      integer, save, dimension (1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) ::
     &         ip,iu,iv,iq, iuopn,ivopn, ipa, ishlf,
     &         ipim1, ipip1, ipjm1, ipjp1,
     &         ipim1x,ipip1x,ipjm1x,ipjp1x
      integer, save, dimension (1-nbdy:jdm+nbdy,ms) :: 
     &         ifp,ilp,ifq,ilq,ifu,ilu,ifv,ilv
      integer, save, dimension (1-nbdy:idm+nbdy,ms) :: 
     &         jfp,jlp,jfq,jlq,jfu,jlu,jfv,jlv
      integer, save, dimension (1-nbdy:jdm+nbdy) :: 
     &         isp,isq,isu,isv
      integer, save, dimension (1-nbdy:idm+nbdy) :: 
     &         jsp,jsq,jsu,jsv
      logical, save, dimension (1-nbdy:jdm+nbdy) ::
     &         allip,alliq,alliu,alliv
#endif
c
c --- line printer unit (stdout)
      integer, save :: lp
c
      real, save :: r_init  !value that allocated arrays should be set to
                            !initialize with set_r_init (to NaN or huge)
c
      integer*8, save, private ::
     &  mem_alloc_now  = 0   !memory in words explicitly allocated by HYCOM
     &, mem_alloc_high = 0   !high water mark of memory  allocated by HYCOM

      contains

#if defined(RELO)
      subroutine gindex_allocate
      implicit none
c
c --- allocate information (gindex) that keeps do loops from running into land
c
      jblk = (jdm+2*nbdy+mxthrd-1)/mxthrd
c
      imt1 =   1-nbdy
      imt2 = idm+nbdy
      jmt1 =   1-nbdy
      jmt2 = jdm+nbdy
c
      allocate(
     &           ip(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           iu(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           iv(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &           iq(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        iuopn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ivopn(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ishlf(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &          ipa(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       ipim1x(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       ipip1x(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       ipjm1x(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &       ipjp1x(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ipim1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ipip1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ipjm1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy),
     &        ipjp1(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy) )
      call mem_stat_add( 8*(idm+2*nbdy)*(jdm+2*nbdy) )  !real=2*int
c
      allocate(
     &              ifp(1-nbdy:jdm+nbdy,ms),
     &              ilp(1-nbdy:jdm+nbdy,ms),
     &              ifq(1-nbdy:jdm+nbdy,ms),
     &              ilq(1-nbdy:jdm+nbdy,ms),
     &              ifu(1-nbdy:jdm+nbdy,ms),
     &              ilu(1-nbdy:jdm+nbdy,ms),
     &              ifv(1-nbdy:jdm+nbdy,ms),
     &              ilv(1-nbdy:jdm+nbdy,ms) )
      call mem_stat_add( 4*(jdm+2*nbdy)*ms )  !real=2*int
c
      allocate(
     &              jfp(1-nbdy:idm+nbdy,ms),
     &              jlp(1-nbdy:idm+nbdy,ms),
     &              jfq(1-nbdy:idm+nbdy,ms),
     &              jlq(1-nbdy:idm+nbdy,ms),
     &              jfu(1-nbdy:idm+nbdy,ms),
     &              jlu(1-nbdy:idm+nbdy,ms),
     &              jfv(1-nbdy:idm+nbdy,ms),
     &              jlv(1-nbdy:idm+nbdy,ms) )
      call mem_stat_add( 4*(idm+2*nbdy)*ms )  !real=2*int
c
      allocate(
     &             isp(1-nbdy:jdm+nbdy),
     &             isq(1-nbdy:jdm+nbdy),
     &             isu(1-nbdy:jdm+nbdy),
     &             isv(1-nbdy:jdm+nbdy) )
      call mem_stat_add( 2*(jdm+2*nbdy) )  !real=2*int
c
      allocate(
     &             jsp(1-nbdy:idm+nbdy),
     &             jsq(1-nbdy:idm+nbdy),
     &             jsu(1-nbdy:idm+nbdy),
     &             jsv(1-nbdy:idm+nbdy) )
      call mem_stat_add( 2*(idm+2*nbdy) )  !real=2*int
c
      allocate(
     &           allip(1-nbdy:jdm+nbdy),
     &           alliq(1-nbdy:jdm+nbdy),
     &           alliu(1-nbdy:jdm+nbdy),
     &           alliv(1-nbdy:jdm+nbdy) )
      call mem_stat_add( 2*(jdm+2*nbdy) )  !real=2*logical
c
      end subroutine gindex_allocate
#endif /* RELO */

      subroutine mem_stat_add(mem_words)
      implicit none
c
      integer mem_words
c
c --- add mem_words to mem_alloc_ statistics
c ---   note that mem_words can be negative
c --- call after every allocate and deallocate statement
c
      mem_alloc_now  = mem_alloc_now + mem_words
      mem_alloc_high = max( mem_alloc_high, mem_alloc_now )
c
      end subroutine mem_stat_add

      subroutine mem_stat_print(ctitle)
      implicit none
c
      character*(*) ctitle
c
c --- print memory statistics
c --- only call mem_stat_print on the processors that you want to print
c
      real*8    a3_1,a3_now,a3_high,gb_now,gb_high
      integer*8 mem_3d,mem_3df
c
      gb_now  = mem_alloc_now  / (1024.d0**3/8.d0)
      gb_high = mem_alloc_high / (1024.d0**3/8.d0)
c
      a3_1    = (idm+2*nbdy)*(jdm+2*nbdy)*kdm
c
      mem_3d  = mem_alloc_now /((idm+2*nbdy)*(jdm+2*nbdy)*kdm)
      mem_3df = mem_alloc_now  -
     &                  mem_3d*((idm+2*nbdy)*(jdm+2*nbdy)*kdm)
      a3_now  = mem_3d + mem_3df/a3_1
c
      mem_3d  = mem_alloc_high/((idm+2*nbdy)*(jdm+2*nbdy)*kdm)
      mem_3df = mem_alloc_high -
     &                  mem_3d*((idm+2*nbdy)*(jdm+2*nbdy)*kdm)
      a3_high = mem_3d + mem_3df/a3_1
c
      write(lp,'(/a,a,2i16/a,a,4x,2f16.3/a,a,4x,2f16.3/)')
     &  trim(ctitle),' memory (words) now,high =',
     &  mem_alloc_now,mem_alloc_high,
     &  trim(ctitle),' memory (GB)    now,high =',
     &  gb_now,gb_high,
     &  trim(ctitle),' eq. 3-D arrays now,high =',
     &  a3_now,a3_high
c
      end subroutine mem_stat_print

#if defined(NAN2003)
      subroutine set_r_init
      use ieee_arithmetic, only : ieee_value,
     &                            ieee_quiet_nan
      implicit none
c
c --- inititialize r_init
c --- the value that allocated arrays should be set to
c --- version with ieee_arithmetic intrinsic module
c
      r_init = ieee_value(r_init, ieee_quiet_nan)
c
      end subroutine set_r_init
#else
      subroutine set_r_init
      implicit none
c
c --- inititialize r_init
c --- the value that allocated arrays should be set to
c --- version without ieee_arithmetic intrinsic module
c
      r_init = huge(r_init)
c
      end subroutine set_r_init
#endif

      end module mod_dimensions
c
c> Revision history:
c>
c> Jan. 2014 - inline some of dimensions.h
c> Jan. 2014 - add r_init and set_r_init; r_init is NaN via /* NAN2003 */ macro
c> Jan. 2014 - dimensions_relo.h for relocatable (region independent) case
c> Feb. 2014 - removed margin, no longer a global variable
c> Apr. 2014 - added ishlf
c> May  2014 - added ipim1,ipip1,ipjm1,ipjp1,ipim1x,ipip1x,ipjm1x,ipjp1x
c> May  2014 - added allip,alliq,alliu,alliv
c> Feb. 2015 - added gb_now,gb_high
