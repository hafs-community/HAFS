!------------------------------------------------------------
! This file contains the routines needed to perform linear
! interpolation on sphere.
!
!
! Ning Wang, Jan 2007, init version
! This file contains the routines needed to perform linear
! interpolation on sphere.
!
!
! Ning Wang, Jan 2007, initial version
! Ning Wang, Jan 2011, Added some subroutines and comments.
!
! General purpose subroutines:
!  (1) slint_init(grid1, n1, grid2, n2)
!      grid1, grid2: array of lat-lons that specifies source
!                    and target grid;
!       n1, n2, gripoint numners of source and target grids.
!  (2) slint_init_fn(grid_file1, n1, grid_file2, n2, nn)
!    grid_file1: file name for the source grid specification;
!    grid_file2: file name for the target grid specification;
!    n1, n2: grid point numbers of source and target grids; 
!  slint_init (...) initialize the associated data structures
!  and computes the interpolation coefficients. 
!        
!  (3) bilinear_interp (src_data, tgt_data)
!    src_data: an array of n1 elements that contains the data
!              at source grid points.
!    tgt_data: an array of n2 elements that contains the data
!              at target grid points.
!  bilinear_interp (...)  interpolates the src_data bilinearly 
!  to tgt_dat. 
!
!  (4) nn_interp (src_data, tgt_data)
!  Same as bilinear_interp, except it assigns the nearest 
!  neighbor's value in the src_data to the tgt_data.  
!
! Special and legacy subroutines:
!    Following the similar naming convention as those used 
!    in general subroutines. Details see in-line comments.    
!
! Ning Wang, July 2011, important revision to the package.
!
!-------------------------------------------------------------
MODULE slint
    TYPE GRID
      INTEGER :: type
      INTEGER :: ngp, mx, my
      REAL, ALLOCATABLE :: latlon(:,:)
      REAL, ALLOCATABLE :: coeffs(:,:)
      REAL, ALLOCATABLE :: data(:)
      INTEGER, ALLOCATABLE :: nn(:,:)
      INTEGER, ALLOCATABLE :: nn_ic(:,:)
      INTEGER, ALLOCATABLE :: num_nn_ic(:)
      INTEGER, ALLOCATABLE :: nnic_sel(:,:)
      INTEGER, ALLOCATABLE :: num_nnic_sel(:)
       
    END TYPE GRID

    TYPE(GRID) src_grid, tgt_grid

    REAL :: avg_nd

CONTAINS

! General purpose subroutines, 
SUBROUTINE slint_init(grid1, n1, grid2, n2)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n1, n2
    REAL,INTENT(IN) :: grid1(n1, 2), grid2(n2, 2)

    CALL init_intern_array(grid1, n1, grid2, n2, 0)

END SUBROUTINE slint_init

SUBROUTINE slint_init_f2c(grid1, n1, grid2, n2)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n1, n2
    REAL,INTENT(IN) :: grid1(n1, 2), grid2(n2, 2)
 
    REAL :: pi
    pi = acos(-1.0) 
    avg_nd = sqrt(4.0*pi)/sqrt(REAL(n2))

    CALL init_intern_array(grid1, n1, grid2, n2, 1)

END SUBROUTINE slint_init_f2c

SUBROUTINE slint_init_save(fn, grid1, n1, grid2, n2)
    IMPLICIT NONE

    CHARACTER *(*), INTENT(IN) :: fn
    INTEGER :: n1, n2
    REAL,INTENT(IN) :: grid1(n1, 2), grid2(n2, 2)

    CALL init_intern_array(grid1, n1, grid2, n2, 0)
    
    OPEN (10,file=fn,form='unformatted')
    WRITE(10) n1, n2 
    WRITE(10) tgt_grid%nn, tgt_grid%coeffs
    WRITE(10) src_grid%latlon, tgt_grid%latlon
   
    CLOSE(10)

END SUBROUTINE slint_init_save

SUBROUTINE slint_init_compute(grid1,n1,grid2,n2,regrid_map)
  IMPLICIT NONE

  TYPE(grid) :: regrid_map
  INTEGER :: n1, n2
  REAL,INTENT(IN) :: grid1(n1,2), grid2(n2,2)

  call init_intern_array(grid1,n1,grid2,n2,0)
  regrid_map = tgt_grid

END SUBROUTINE slint_init_compute

SUBROUTINE slint_init_read(fn)

    IMPLICIT NONE
    CHARACTER(len=256) fn
    CHARACTER*80 :: header(10)

    INTEGER :: n1, n2

    
    OPEN (10,file=fn,form='unformatted')
    READ(10) n1, n2

    src_grid%type = 1
    src_grid%ngp = n1
    tgt_grid%type = 1
    tgt_grid%ngp = n2
   print *,'... callling slint_init_read ...'
   print *,' src_grid%ngp =',n1
   print *,' tgt_grid%ngp =',n2
    

    IF (ALLOCATED(tgt_grid%nn)) THEN
      DEALLOCATE(tgt_grid%nn)
    ENDIF
    IF (ALLOCATED(tgt_grid%coeffs)) THEN
      DEALLOCATE(tgt_grid%coeffs)
    ENDIF
    IF (ALLOCATED(src_grid%latlon)) THEN
      DEALLOCATE(src_grid%latlon)
    ENDIF
    IF (ALLOCATED(tgt_grid%latlon)) THEN
      DEALLOCATE(tgt_grid%latlon)
    ENDIF
    ALLOCATE(tgt_grid%nn(3, n2))
    ALLOCATE(tgt_grid%coeffs(3, n2))
    ALLOCATE(src_grid%latlon(2, n1))
    ALLOCATE(tgt_grid%latlon(2, n2))

    READ(10) tgt_grid%nn, tgt_grid%coeffs
    READ(10) src_grid%latlon, tgt_grid%latlon
    CLOSE(10)

!zhang
   OPEN(30,file='tgt_grid.dat',form='unformatted')
   !WRITE(30)header
   WRITE(30)tgt_grid%ngp
   WRITE(30)tgt_grid%latlon(1,:)
   WRITE(30)tgt_grid%latlon(2,:)
!   print *, 'MM_tgt_grid%nn     =',minval(tgt_grid%nn),maxval(tgt_grid%nn)
!   print *, 'MM_tgt_grid%coeffs =',minval(tgt_grid%coeffs),maxval(tgt_grid%coeffs)
!   print *, 'MM_src_grid%latlon =',minval(src_grid%latlon),maxval(src_grid%latlon)
!   print *, 'MM_tgt_grid%latlon(1,:) =',minval(tgt_grid%latlon(1,:)),maxval(tgt_grid%latlon(1,:))
!   print *, 'MM_tgt_grid%latlon(2,:) =',minval(tgt_grid%latlon(2,:)),maxval(tgt_grid%latlon(2,:))
!   print *, 'tgt_grid%ngp            =', tgt_grid%ngp
!   stop '... writing tgt_grid file and stop...'
!zhang
print *, 'size of tgt_grid%latlon(1,:)',size(tgt_grid%latlon(1,:))
print *, 'size of tgt_grid%latlon(2,:)',size(tgt_grid%latlon(2,:))

END SUBROUTINE slint_init_read

SUBROUTINE bilinear_interp (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL v(3)
    REAL*8 c(3)
    INTEGER i

!print *,'DEBUG: begin bilinear_interp()'
    DO i = 1, tgt_grid%ngp
      c = tgt_grid%coeffs(1:3,i)
      v(1) = src_data(tgt_grid%nn(1, i))
      v(2) = src_data(tgt_grid%nn(2, i))
      v(3) = src_data(tgt_grid%nn(3, i))
      tgt_data(i) = c(1) * v(1) + c(2) * v(2) + c(3) * v(3)
    END DO
!print *,'DEBUG: end bilinear_interp()'

END SUBROUTINE bilinear_interp

SUBROUTINE bilinear_interp_f2c (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL v(3), tgt_v1, tgt_v2, wgt
    REAL*8 c(3)
    INTEGER i, j

!print *,'DEBUG: begin bilinear_interp_f2c()'
    DO i = 1, tgt_grid%ngp
      c = tgt_grid%coeffs(1:3,i)
      v(1) = src_data(tgt_grid%nn(1, i))
      v(2) = src_data(tgt_grid%nn(2, i))
      v(3) = src_data(tgt_grid%nn(3, i))
      tgt_v1 = c(1) * v(1) + c(2) * v(2) + c(3) * v(3)
      tgt_v2 = 0.0
      DO j = 1, tgt_grid%num_nn_ic(i)
        tgt_v2 = tgt_v2 + tgt_grid%nn_ic(j, i)
      ENDDO
      tgt_v2 = tgt_v2 / tgt_grid%num_nn_ic(i)
      wgt = 3.0 / max(3.0, REAL(tgt_grid%num_nn_ic(i))) 
      tgt_data(i) = wgt * tgt_v1 + (1.0 - wgt) * tgt_v2   
    END DO
!print *,'DEBUG: end bilinear_interp_f2c()'

END SUBROUTINE bilinear_interp_f2c

SUBROUTINE nn_interp (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL v(3)
    REAL*8 c(3)
    INTEGER i

    DO i = 1, tgt_grid%ngp
      v(1) = src_data(tgt_grid%nn(1, i))
      tgt_data(i) = v(1)
    END DO

END SUBROUTINE nn_interp

SUBROUTINE nn_interp_f2c (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL :: v(3)
    REAL*8 :: c(3)
    REAL :: tgt_v1, tgt_v2, wgt
    INTEGER :: i, j, k, histo, maxhisto, maxhisto_idx

    maxhisto_idx = 1
    DO i = 1, tgt_grid%ngp
      maxhisto = 0
      DO j = 1, tgt_grid%num_nn_ic(i) 
        histo = 1
        DO k = j+1, tgt_grid%num_nn_ic(i)
          IF (src_data(tgt_grid%nn_ic(k, i)) ==  &
              src_data(tgt_grid%nn_ic(j, i))) THEN
           histo = histo + 1  
          ENDIF
        ENDDO
        IF (histo > maxhisto) THEN
          maxhisto = histo
          maxhisto_idx = j
        ENDIF
      ENDDO
      IF (maxhisto == 0) THEN
        tgt_data(i) = src_data(tgt_grid%nn(1, i))
      ELSE
        tgt_data(i) = src_data(tgt_grid%nn_ic(maxhisto_idx, i))  
      ENDIF
    END DO

END SUBROUTINE nn_interp_f2c

SUBROUTINE nn_interp_f2c_save (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL :: v(3)
    REAL*8 :: c(3)
    INTEGER :: i, j, k, histo, maxhisto, maxhisto_idx
    INTEGER :: nn_ic_sel(16), nn_ic_max(16)

    maxhisto_idx = 1
    DO i = 1, tgt_grid%ngp
      maxhisto = 0
      DO j = 1, tgt_grid%num_nn_ic(i) 
        histo = 1
        nn_ic_sel(histo) = tgt_grid%nn_ic(j, i)
        DO k = j+1, tgt_grid%num_nn_ic(i)
          IF (src_data(tgt_grid%nn_ic(k, i)) ==  &
              src_data(tgt_grid%nn_ic(j, i))) THEN
           histo = histo + 1  
           nn_ic_sel(histo) = tgt_grid%nn_ic(k, i)
          ENDIF
        ENDDO
        IF (histo > maxhisto) THEN
          maxhisto = histo
          maxhisto_idx = j
          DO k = 1, maxhisto
           nn_ic_max(k) = nn_ic_sel(k)
          ENDDO 
        ENDIF
      ENDDO
      IF (maxhisto == 0) THEN
        tgt_data(i) = src_data(tgt_grid%nn(1, i))
        tgt_grid%num_nnic_sel(i) = 0
      ELSE
        tgt_data(i) = src_data(tgt_grid%nn_ic(maxhisto_idx, i))  
        DO k = 1, maxhisto
          tgt_grid%nnic_sel(k, i) = nn_ic_max(k)
        ENDDO
        tgt_grid%num_nnic_sel(i) = maxhisto
      ENDIF
    ENDDO

END SUBROUTINE nn_interp_f2c_save

SUBROUTINE nn_interp_f2c_use_saved(src_data, tgt_data) ! Plurality 'interpolation'
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL ::  tgt_v
    INTEGER :: i, j, k, histo, maxhisto, maxhisto_idx
    INTEGER :: nn_ic_sel(16)

    DO i = 1, tgt_grid%ngp
      IF (tgt_grid%num_nnic_sel(i) == 0) THEN
        tgt_data(i) = src_data(tgt_grid%nn(1, i))
      ELSE
        maxhisto = 0
        DO j = 1, tgt_grid%num_nnic_sel(i) 
          histo = 1
          nn_ic_sel(histo) = tgt_grid%nnic_sel(j, i)
          DO k = j+1, tgt_grid%num_nnic_sel(i)
            IF (src_data(tgt_grid%nnic_sel(k, i)) ==  &
                src_data(tgt_grid%nnic_sel(j, i))) THEN
              histo = histo + 1
              nn_ic_sel(histo) = tgt_grid%nnic_sel(k, i)
            ENDIF
          ENDDO
          IF (histo > maxhisto) THEN
            maxhisto = histo
            maxhisto_idx = j
          ENDIF
        ENDDO
        tgt_data(i) = src_data(tgt_grid%nnic_sel(maxhisto_idx, i)) 
      ENDIF
    END DO

END SUBROUTINE nn_interp_f2c_use_saved

SUBROUTINE bilinear_interp_f2c_use_saved(src_data, tgt_data) ! Average 'interpolation'
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    REAL*8 :: c(3)
    REAL :: v(3)
    REAL ::  tgt_v1, tgt_v2, wgt
    INTEGER :: i, j

    DO i = 1, tgt_grid%ngp
      c = tgt_grid%coeffs(1:3,i)
      v(1) = src_data(tgt_grid%nn(1, i))
      v(2) = src_data(tgt_grid%nn(2, i))
      v(3) = src_data(tgt_grid%nn(3, i))
      tgt_v1 = c(1) * v(1) + c(2) * v(2) + c(3) * v(3)

      tgt_v2 = 0.0
      DO j = 1, tgt_grid%num_nnic_sel(i) 
        tgt_v2 = tgt_v2 + src_data(tgt_grid%nnic_sel(j, i)) 
      ENDDO
      tgt_v2 = tgt_v2 / tgt_grid%num_nnic_sel(i)

      wgt = 3.0 / max(3.0, REAL(tgt_grid%num_nnic_sel(i))) 
      tgt_data(i) = wgt * tgt_v1 + (1.0 - wgt) * tgt_v2   
    END DO

END SUBROUTINE bilinear_interp_f2c_use_saved

SUBROUTINE bilinear_interp_uv(src_u, tgt_u, src_v, tgt_v)
    IMPLICIT NONE
! Arguments    
    REAL, intent(in) :: src_u(*), src_v(*)
    REAL, intent(out) :: tgt_u(*), tgt_v(*)

! Local workspace
    REAL u, v, c(3)
    REAL lat, lon, uxyz_src(3, 3), uxyz_tgt(3) 
    REAL mag_src(3), mag_tgt, mag_tmp
    INTEGER i, ipn, nip
    REAL, PARAMETER :: eps = 0.00001

    nip = tgt_grid%ngp

    DO ipn = 1, nip
      DO i = 1, 3
        lat = src_grid%latlon(1, tgt_grid%nn(i, ipn))
        lon = src_grid%latlon(2, tgt_grid%nn(i, ipn))
        u = src_u(tgt_grid%nn(i, ipn))
        v = src_v(tgt_grid%nn(i, ipn))
        CALL uv2xyz(u, v, lat, lon, uxyz_src(1, i), uxyz_src(2, i), uxyz_src(3, i))
        mag_src(i) = sqrt(u*u+v*v)
      ENDDO
      c = tgt_grid%coeffs(1:3,ipn)
      uxyz_tgt(1:3) = c(1) * uxyz_src(1:3, 1) + c(2) * uxyz_src(1:3, 2) &
                    + c(3) * uxyz_src(1:3, 3)
      mag_tgt = c(1) * mag_src(1) + c(2) * mag_src(2) + c(3) * mag_src(3)
      lat = tgt_grid%latlon(1, ipn)
      lon = tgt_grid%latlon(2, ipn)
      CALL xyz2uv(u, v, lat, lon, uxyz_tgt(1), uxyz_tgt(2), uxyz_tgt(3))
      mag_tmp = sqrt(u*u+v*v)
      IF (mag_tmp < eps) THEN
        mag_tgt = 1.0
      ELSE
        mag_tgt = mag_tgt / mag_tmp 
      ENDIF
      tgt_u(ipn) = u * mag_tgt
      tgt_v(ipn) = v * mag_tgt
    END DO
END SUBROUTINE bilinear_interp_uv

! Internal subroutines called by those within the module
SUBROUTINE init_intern_array(grid1, n1, grid2, n2, f2c)
    USE kd, ONLY: init_kd_tree, close_kd_tree
    IMPLICIT NONE
   
    INTEGER, INTENT(IN) :: n1, n2, f2c
    REAL grid1(n1, 2), grid2(n2, 2)

    INTEGER i, j, g_idx, seq

    CALL init_kd_tree(grid1, n1, 1)
    src_grid%type = 1
    src_grid%ngp = n1
    IF (ALLOCATED(src_grid%latlon)) THEN
      DEALLOCATE(src_grid%latlon)
    ENDIF
    IF (ALLOCATED(src_grid%data)) THEN
      DEALLOCATE(src_grid%data)
    ENDIF
    ALLOCATE(src_grid%latlon(2, n1))
    ALLOCATE(src_grid%data(n1))

    DO i = 1, n1
      src_grid%latlon(1,i) = grid1(i, 1) 
      src_grid%latlon(2,i) = grid1(i, 2) 
    END DO

    tgt_grid%type = 1
    tgt_grid%ngp = n2

    IF (ALLOCATED(tgt_grid%latlon)) THEN
      DEALLOCATE(tgt_grid%latlon)
    ENDIF
    IF (ALLOCATED(tgt_grid%data)) THEN
      DEALLOCATE(tgt_grid%data)
    ENDIF
    IF (ALLOCATED(tgt_grid%coeffs)) THEN
      DEALLOCATE(tgt_grid%coeffs)
    ENDIF
    IF (ALLOCATED(tgt_grid%nn)) THEN
      DEALLOCATE(tgt_grid%nn)
    ENDIF
    IF (ALLOCATED(tgt_grid%nn_ic)) THEN
      DEALLOCATE(tgt_grid%nn_ic)
    ENDIF
    IF (ALLOCATED(tgt_grid%num_nn_ic)) THEN
      DEALLOCATE(tgt_grid%num_nn_ic)
    ENDIF
    IF (ALLOCATED(tgt_grid%nnic_sel)) THEN
      DEALLOCATE(tgt_grid%nnic_sel)
    ENDIF
    IF (ALLOCATED(tgt_grid%num_nnic_sel)) THEN
      DEALLOCATE(tgt_grid%num_nnic_sel)
    ENDIF
    ALLOCATE(tgt_grid%latlon(2, n2))
    ALLOCATE(tgt_grid%data(n2))
    ALLOCATE(tgt_grid%coeffs(3, n2))
    ALLOCATE(tgt_grid%nn(3, n2))
    IF (f2c == 1) THEN
      ALLOCATE(tgt_grid%nn_ic(16, n2))
      ALLOCATE(tgt_grid%num_nn_ic(n2))
      ALLOCATE(tgt_grid%nnic_sel(16, n2))
      ALLOCATE(tgt_grid%num_nnic_sel(n2))
    ENDIF
      
    DO i = 1, n2
      tgt_grid%latlon(1,i) = grid2(i, 1) 
      tgt_grid%latlon(2,i) = grid2(i, 2) 
    END DO

    CALL coeff_comp(f2c)
    CALL close_kd_tree()

END SUBROUTINE init_intern_array

! Subroutine to compute interpolation coefficients
SUBROUTINE coeff_comp(f2c)
    USE kd, ONLY:set_k
    IMPLICIT NONE

    INTEGER f2c

    REAL latlon(2, 3), intsec(2), gcd1, gcd2
    REAL hp1(3), hp2(3), min_dist
    INTEGER i, j,mx, my, g_idx, nn(3), num
    REAL epsilon, r2d , t1, t2

    epsilon = 0.00000000001
    r2d = 180.0/(ATAN(1.0) * 4.0)

    IF (src_grid%type == 1 .AND. tgt_grid%type == 0) THEN
      mx = tgt_grid%mx
      my = tgt_grid%my
      DO i = 1, mx
        DO j = 1, my
          g_idx = (i + (j - 1) * mx)
          CALL nsn(tgt_grid%latlon(1,g_idx), nn, num, min_dist)
          tgt_grid%nn(1:3, g_idx) = nn(1:3)
          IF (min_dist < epsilon) THEN
            tgt_grid%coeffs(1, g_idx) = 1.0  
            tgt_grid%coeffs(2, g_idx) = 0.0  
            tgt_grid%coeffs(3, g_idx) = 0.0  
          ELSE IF (num == 2) THEN
            latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
            latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
            CALL gcd_ratio(latlon(1,1), latlon(1,2), tgt_grid%latlon(1,g_idx), gcd1, t1)
            tgt_grid%coeffs(1, g_idx) = (1.0 - t1)   
            tgt_grid%coeffs(2, g_idx) = t1 
            tgt_grid%coeffs(3, g_idx) = 0.0  
          ELSE
            latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
            latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
            latlon(1:2,3) = src_grid%latlon(1:2,nn(3))
            CALL intersection (latlon(1,1), tgt_grid%latlon(1,g_idx), latlon(1,2), latlon(1,3), intsec) 
            CALL gcd_ratio(latlon(1,2), latlon(1,3), intsec, gcd1, t1)
            CALL gcd_ratio(latlon(1,1), intsec, tgt_grid%latlon(1,g_idx), gcd2, t2)

            IF (t1 /= t1 .OR. t2 /= t2) THEN
              PRINT*, 't1 or t2 NaN:', t1, t2
            ENDIF

            tgt_grid%coeffs(1, g_idx) = (1.0 - t2)   
            tgt_grid%coeffs(2, g_idx) = t2 * (1.0 - t1)    
            tgt_grid%coeffs(3, g_idx) = t2 * t1 
          END IF
        END DO
      END DO
    ELSE IF (src_grid%type == 1 .AND. tgt_grid%type == 1) THEN
      DO i = 1, tgt_grid%ngp
        IF (f2c == 1) CALL set_k(1)
        CALL nsn(tgt_grid%latlon(1,i), nn, num, min_dist)
        tgt_grid%nn(1:3, i) = nn(1:3)
        IF (min_dist < epsilon) THEN 
          tgt_grid%coeffs(1, i) = 1.0  
          tgt_grid%coeffs(2, i) = 0.0  
          tgt_grid%coeffs(3, i) = 0.0  
        ELSE IF (num == 2) THEN
          latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
          latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
          CALL gcd_ratio(latlon(1,1), latlon(1,2), tgt_grid%latlon(1,i), gcd1, t1)
          tgt_grid%coeffs(1, i) = (1.0 - t1)   
          tgt_grid%coeffs(2, i) = t1 
          tgt_grid%coeffs(3, i) = 0.0  
        ELSE
          latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
          latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
          latlon(1:2,3) = src_grid%latlon(1:2,nn(3))
          CALL intersection (latlon(1,1), tgt_grid%latlon(1,i), latlon(1,2), latlon(1,3), intsec) 
          CALL gcd_ratio(latlon(1,2), latlon(1,3), intsec, gcd1, t1)
          CALL gcd_ratio(latlon(1,1), intsec, tgt_grid%latlon(1,i), gcd2, t2)

          IF (t1 /= t1 .OR. t2 /= t2) THEN
            PRINT*, 't1 or t2 NaN:', t1, t2
          ENDIF

          tgt_grid%coeffs(1, i) = (1.0 - t2)   
          tgt_grid%coeffs(2, i) = t2 * (1.0 - t1)    
          tgt_grid%coeffs(3, i) = t2 * t1 
        END IF
        IF (f2c == 1) THEN
          CALL set_k(400)
          CALL nsn2(tgt_grid%latlon(1,i),tgt_grid%nn_ic(1:16, i),tgt_grid%num_nn_ic(i))
          print*, 'tgt_grid%num_nn_ic(i):',tgt_grid%num_nn_ic(i)
          print*, tgt_grid%nn_ic(1:16, i)
        ENDIF
      END DO
    END IF

END SUBROUTINE coeff_comp

! Subroutine to compute interpolation coefficients, distance weight
SUBROUTINE coeff_comp1(nn_w)
    USE kd, ONLY:knn_search
    IMPLICIT NONE

    INTEGER nn_w

    REAL latlon(2, 3), intsec(2), gcd1, gcd2, part_gcd1, part_gcd2
    REAL hp1(3), hp2(3), min_dist
    INTEGER i, j, mx, my, g_idx, nn(3), num
    REAL epsilon, r2d , d1, d2, d3, rd1, rd2, rd3, srds 

    epsilon = 0.00000000001
    r2d = 180.0/(ATAN(1.0) * 4.0)

    IF (src_grid%type == 1 .AND. tgt_grid%type == 0) THEN
      mx = tgt_grid%mx
      my = tgt_grid%my
      DO i = 1, mx
        DO j = 1, my
          g_idx = (i + (j - 1) * mx)
          CALL nsn(tgt_grid%latlon(1,g_idx), nn, num, min_dist)
          tgt_grid%nn(1:3, g_idx) = nn(1:3)
          IF (min_dist < epsilon .OR. nn_w == 1) THEN 
            tgt_grid%coeffs(1, g_idx) = 1.0  
            tgt_grid%coeffs(2, g_idx) = 0.0  
            tgt_grid%coeffs(3, g_idx) = 0.0  
          ELSE 
            latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
            latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
            latlon(1:2,3) = src_grid%latlon(1:2,nn(3))
            d1 =  gc_dist(latlon(1,1), tgt_grid%latlon(1,g_idx))
            d2 =  gc_dist(latlon(1,2), tgt_grid%latlon(1,g_idx))
            d3 =  gc_dist(latlon(1,3), tgt_grid%latlon(1,g_idx))

            rd1 = 1.0/d1
            rd2 = 1.0/d2
            rd3 = 1.0/d3
            srds = rd1 + rd2 + rd3

            tgt_grid%coeffs(1, g_idx) = rd1 / srds    
            tgt_grid%coeffs(2, g_idx) = rd2 / srds    
            tgt_grid%coeffs(3, g_idx) = rd3 / srds
          END IF
        END DO
      END DO
    ELSE IF (src_grid%type == 1 .AND. tgt_grid%type == 1) THEN
      DO i = 1, tgt_grid%ngp
        CALL nsn(tgt_grid%latlon(1,i), nn, num, min_dist)
        tgt_grid%nn(1:3, i) = nn(1:3)
        IF (min_dist < epsilon .OR. nn_w == 1) THEN 
          tgt_grid%coeffs(1, i) = 1.0  
          tgt_grid%coeffs(2, i) = 0.0  
          tgt_grid%coeffs(3, i) = 0.0  
        ELSE
          latlon(1:2,1) = src_grid%latlon(1:2,nn(1))
          latlon(1:2,2) = src_grid%latlon(1:2,nn(2))
          latlon(1:2,3) = src_grid%latlon(1:2,nn(3))
          d1 =  gc_dist(latlon(1,1), tgt_grid%latlon(1,i))
          d2 =  gc_dist(latlon(1,2), tgt_grid%latlon(1,i))
          d3 =  gc_dist(latlon(1,3), tgt_grid%latlon(1,i))

          rd1 = 1.0/d1
          rd2 = 1.0/d2
          rd3 = 1.0/d3
          srds = rd1 + rd2 + rd3

          tgt_grid%coeffs(1, i) = rd1 / srds    
          tgt_grid%coeffs(2, i) = rd2 / srds    
          tgt_grid%coeffs(3, i) = rd3 / srds
        END IF
      END DO
    END IF

END SUBROUTINE coeff_comp1 

! Some legacy subroutines, keep them in the package temporarily for backward compatiablity.

SUBROUTINE nn_int (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    CALL nn_interp(src_data, tgt_data)

END SUBROUTINE nn_int

SUBROUTINE bl_int (src_data, tgt_data)
    IMPLICIT NONE

    REAL, INTENT(IN) :: src_data(*)
    REAL, INTENT(OUT) :: tgt_data(*)

    CALL bilinear_interp(src_data, tgt_data)

END SUBROUTINE bl_int

SUBROUTINE bilinear_init_i2r(mx, my, llpoints, nip)
    USE kd, ONLY:init_kd_tree, close_kd_tree
    IMPLICIT NONE
   
    INTEGER, intent(in) :: mx, my, nip
    REAL   , intent(in) :: llpoints(nip,2)

    INTEGER i, j, g_idx, seq
    REAL pi

    CALL init_kd_tree(llpoints, nip, 1)

    src_grid%type = 1
    src_grid%ngp = nip
    IF (ALLOCATED(src_grid%latlon)) THEN
      DEALLOCATE(src_grid%latlon)
    END IF
    IF (ALLOCATED(src_grid%data)) THEN
      DEALLOCATE(src_grid%data)
    END IF
    ALLOCATE(src_grid%latlon(2, nip))
    ALLOCATE(src_grid%data(nip))

    DO i = 1, nip
      src_grid%latlon(1,i) = llpoints(i, 1) 
      src_grid%latlon(2,i) = llpoints(i, 2) 
    END DO

    tgt_grid%type = 0
    tgt_grid%mx = mx
    tgt_grid%my = my

    IF (ALLOCATED(tgt_grid%latlon)) THEN
      DEALLOCATE(tgt_grid%latlon)
    ENDIF
    IF (ALLOCATED(tgt_grid%data)) THEN
      DEALLOCATE(tgt_grid%data)
    ENDIF
    IF (ALLOCATED(tgt_grid%coeffs)) THEN 
      DEALLOCATE(tgt_grid%coeffs)
    ENDIF
    IF (ALLOCATED(tgt_grid%nn)) THEN
      DEALLOCATE(tgt_grid%nn)
    ENDIF

    ALLOCATE(tgt_grid%latlon(2, mx * my))
    ALLOCATE(tgt_grid%nn(3, mx * my))
    ALLOCATE(tgt_grid%coeffs(3, mx * my))
    ALLOCATE(tgt_grid%data(mx * my))
    pi = 4.0*ATAN(1.0)
    DO i = 1, mx 
      DO j = 1, my
        g_idx = (i + (j - 1) * mx)
        tgt_grid%latlon(1, g_idx) = (REAL(j - 1) - REAL(my - 1) * 0.5)  * pi / REAL(my - 1) 
        tgt_grid%latlon(1, g_idx) = -tgt_grid%latlon(1, g_idx) 
        tgt_grid%latlon(2, g_idx) = REAL(i - 1) * 2.0 * pi / REAL(mx) 
      END DO
    END DO
      
    CALL coeff_comp(0)
    CALL close_kd_tree()

END SUBROUTINE bilinear_init_i2r

SUBROUTINE bilinear_interp_i2r(k, nlevels, vardata, data_xyz) 
    IMPLICIT NONE

    INTEGER k, nlevels
    REAL vardata(*)
    REAL data_xyz(*)
  
    INTEGER i, j, n, mx, my

    n = src_grid%ngp
    DO i = 1, n
      src_grid%data(i) = vardata(k + (i - 1) * nlevels) 
    END DO

    CALL bilinear_interp(src_grid%data, tgt_grid%data)

    mx = tgt_grid%mx
    my = tgt_grid%my
    DO i = 1, mx
      DO j = 1, my
        data_xyz((k - 1) * mx * my + (j - 1) * mx + i) =  &
        tgt_grid%data((j - 1) * mx + i) 
      END DO
    END DO

END SUBROUTINE bilinear_interp_i2r

! subrountines for spherical curve interpolation 
SUBROUTINE gcd_ratio (p1, p2, p, gcd, p_gcd)
    IMPLICIT NONE

    REAL p1(2), p2(2), p(2), gcd, p_gcd 
    REAL gcdp1p2, gcdp1p, gcdp2p
    REAL r2d, eps

    r2d = 180.0 / (atan(1.0)*4.0)
    eps = 1.0E-6

    gcdp1p2 = gc_dist(p1, p2)
    gcdp1p = gc_dist(p1, p) 
    gcdp2p = gc_dist(p2, p) 

    IF (gcdp1p2 <= eps) THEN
      !PRINT*, 'nearest neighbor almost overlap!!'
      !PRINT*, p1*r2d, p2*r2d, p*r2d
      p_gcd = 0.0
    ELSE IF (gcdp1p <= gcdp1p2 .AND. gcdp2p <= gcdp1p2) THEN ! p inside the p1p2 segment
      p_gcd = gcdp1p / gcdp1p2
    ELSE IF (gcdp1p > gcdp1p2) THEN ! outside of end point p2
      p_gcd = 1.0 ! don't allow ! extrapolation
!      IF (gcdp2p > eps) THEN 
!        PRINT*, 'extrapolation! outside of p2' 
!        PRINT*, p1*r2d, p2*r2d, p*r2d
!        PRINT*, gcdp1p, gcdp1p2, gc_dist(p,p2)
!      ENDIF
    ELSE IF (gcdp2p > gcdp1p2) THEN ! outside of end point p1
      p_gcd = 0.0 ! don't allow ! extrapolation
!      IF (gcdp1p > eps) THEN
!        PRINT*, 'extrapolation! outside of p1'
!        PRINT*, p1*r2d, p2*r2d, p*r2d
!        PRINT*, gcdp2p, gcdp1p2,  gc_dist(p,p1)
!      ENDIF
    ENDIF 
      gcd = gcdp1p2 

END SUBROUTINE gcd_ratio

! Great circle distance calculation, law of cosine formula 
!FUNCTION gc_dist(p1, p2)
!    IMPLICIT NONE
!
!    REAL gc_dist
!    REAL, INTENT(IN) :: p1(2), p2(2)
!
!    gc_dist = ACOS(COS(p1(1)) * COS(p2(1)) * COS(p1(2) - p2(2)) + SIN(p1(1)) * SIN(p2(1))) 
!
!END FUNCTION gc_dist


! Great circle distance calculation, using Haversine formula. 
! It is more accurate to compute small angular distances.
FUNCTION gc_dist(p1, p2)

    IMPLICIT NONE

    REAL gc_dist
    REAL, INTENT(IN) :: p1(2), p2(2)

    REAL dlatov2, dlonov2, a 

    dlatov2 = (p2(1)-p1(1))/2.0
    dlonov2 = (p2(2)-p1(2))/2.0
    a = sin(dlatov2) * sin(dlatov2) + cos(p1(1))*cos(p2(1))*sin(dlonov2)*sin(dlonov2)
    gc_dist = 2.0 * atan2(sqrt(a), sqrt(1.0-a))

END FUNCTION gc_dist

LOGICAL FUNCTION enclosure(p1, p2, p3, p, co_gc)
    IMPLICIT NONE
    REAL, INTENT(IN) :: p1(2), p2(2), p3(2), p(2)
    INTEGER, INTENT(OUT) :: co_gc

    REAL*8 p1_xy(2), p2_xy(2), p3_xy(2), p_xy(2)
    REAL*8 cp1_z, cp2_z, cp3_z, cos_d2c, eps, eps2

    eps  = 0.00000001
    eps2 = 0.00000000001
    eps2 = 0.0000001
    co_gc = 0
    cos_d2c = sin(p(1))*sin(p1(1)) + cos(p(1))*cos(p1(1))*cos(p1(2)-p(2)) 
    p1_xy(1) = (cos(p1(1))*sin(p1(2) - p(2))) / cos_d2c
    p1_xy(2) = (cos(p(1))*sin(p1(1)) - sin(p(1))*cos(p1(1))*cos(p1(2) - p(2))) / cos_d2c

    cos_d2c = sin(p(1))*sin(p2(1)) + cos(p(1))*cos(p2(1))*cos(p2(2)-p(2)) 
    p2_xy(1) = (cos(p2(1))*sin(p2(2) - p(2))) / cos_d2c
    p2_xy(2) = (cos(p(1))*sin(p2(1)) - sin(p(1))*cos(p2(1))*cos(p2(2) - p(2))) / cos_d2c

    cos_d2c = sin(p(1))*sin(p3(1)) + cos(p(1))*cos(p3(1))*cos(p3(2)-p(2)) 
    p3_xy(1) = (cos(p3(1))*sin(p3(2) - p(2))) / cos_d2c
    p3_xy(2) = (cos(p(1))*sin(p3(1)) - sin(p(1))*cos(p3(1))*cos(p3(2) - p(2))) / cos_d2c

    cp1_z = p1_xy(1)*p2_xy(2) - p1_xy(2)*p2_xy(1)
    cp2_z = p2_xy(1)*p3_xy(2) - p2_xy(2)*p3_xy(1)
    cp3_z = p3_xy(1)*p1_xy(2) - p3_xy(2)*p1_xy(1)
    
    IF (abs(cp1_z) < eps2) co_gc = 1
    IF (abs(cp2_z) < eps2) co_gc = 2
    IF (abs(cp3_z) < eps2) co_gc = 3

    IF (cp1_z*cp2_z .LT. -eps2) THEN
      enclosure = .false.
      RETURN
    ENDIF

    IF (cp1_z*cp3_z .LT. -eps2) THEN
      enclosure = .false.
      RETURN
    ENDIF
   
    enclosure = .true.
    RETURN

END FUNCTION enclosure

SUBROUTINE nsn(q_ll, nn, num, min_dist)
    USE kd, ONLY: num_k
    USE kd, ONLY: num_k, knn_search_ts
    IMPLICIT NONE 
    REAL, INTENT(IN) :: q_ll(2) 
    INTEGER, INTENT(out) :: nn(3), num
    REAL,INTENT(OUT) :: min_dist
    
    REAL nn_ll(2, 3), hp1(3), hp2(3)
    REAL qxyz(3), nnxyz(3), min_d(num_k)
    REAL eps

    INTEGER nni(3), co_gc, nn_swp, num_nn

    eps = 0.00000000001

    hp1 = 0.0
    hp2 = 0.0
!    CALL knn_search(q_ll, nni, min_d, hp1, hp2, 1.0, num_nn)
    CALL knn_search_ts(q_ll, nni, min_d, hp1, hp2, 1.0, 1, num_nn)
    nn(1) = nni(1)
    nn_ll(1:2, 1) = src_grid%latlon(1:2,nn(1))   ! the first vertex  
    min_dist = min_d(1)

    IF (min_dist < eps) THEN   ! if the nearest neighbor is too close
      nn(2) = nn(1)
      nn(3) = nn(1)
      num  = 1
      RETURN
    ENDIF
    
    CALL ll2xyz(q_ll, qxyz)
    CALL ll2xyz(nn_ll(1:2,1), nnxyz)
    hp1 = qxyz - nnxyz / inner_product(qxyz, nnxyz) 
    CALL cross_product2(qxyz, nnxyz, hp2)
    hp1 = hp1 / sqrt(inner_product(hp1, hp1))
    hp2 = hp2 / sqrt(inner_product(hp2, hp2))

!    CALL knn_search(q_ll, nni, min_d, hp1, hp2, 1.0, num_nn)
    CALL knn_search_ts(q_ll, nni, min_d, hp1, hp2, 1.0, 1, num_nn)
    nn(2) = nni(1)
    nn_ll(1:2, 2) = src_grid%latlon(1:2,nn(2))   ! the second vertex  

    hp2 = -hp2
    CALL ll2xyz(nn_ll(1:2,2), nnxyz)
    CALL cross_product2(qxyz, nnxyz, hp1)
!    CALL knn_search(q_ll, nni, min_d, hp1, hp2, 1.0, num_nn)
    CALL knn_search_ts(q_ll, nni, min_d, hp1, hp2, 1.0, 1, num_nn)
    nn(3) = nni(1)
    nn_ll(1:2, 3) = src_grid%latlon(1:2,nn(3))   ! the third vertex  

    IF (enclosure(nn_ll(1:2, 1), nn_ll(1:2, 2), nn_ll(1:2, 3), q_ll, co_gc)) THEN
      num = 3
    ELSE 
!      PRINT*, 'inside test fails'
    END IF

    IF (co_gc /= 0) THEN
      num = 2
      IF (nn(1) == nn(3)) THEN
        RETURN
      ELSE IF (co_gc == 1) THEN
        RETURN
      ELSE IF (co_gc == 2) THEN
        nn_swp = nn(1)
        nn(1) = nn(2)
        nn(2) = nn(3)
        nn(3) = nn_swp
        RETURN
      ELSE IF (co_gc == 3) THEN
        nn_swp = nn(2)
        nn(2) = nn(3)
        nn(3) = nn_swp
      RETURN
      ENDIF
    ENDIF
END SUBROUTINE nsn

SUBROUTINE nsn2(q_ll, nn, num)
    USE kd, ONLY: num_k, knn_search
    IMPLICIT NONE 
    REAL, INTENT(IN) :: q_ll(2) 
    INTEGER, INTENT(OUT) :: nn(:), num
    
    REAL :: hp1(3), hp2(3), cv(3,2)
    REAL :: min_d(num_k), nn_d(16)
    INTEGER :: nni(num_k)
    INTEGER :: i, j, quad, num_nn, stride
    REAL :: eps, sr

    eps = 0.00000000001

    num = 0
    nn_d = 1.0
    sr = 0.5 * avg_nd
!    sr = 0.5 * 0.032
    
    DO quad =  1, 4
      CALL coor_vec(q_ll,quad,cv) 
      hp1 = cv(:,1)
      hp2 = cv(:,2)
      CALL knn_search(q_ll, nni, min_d, hp1, hp2, sr, num_nn)
print*, 'num_nn', num_nn
      IF (num_nn <= 4) THEN
        stride = 1
      ELSE
        stride = num_nn / 3
      ENDIF
      IF (num_nn >= 2) THEN
! first point of the quadrant
        CALL sort_insert(nn, nn_d, nni(1), min_d(1), num)
!        num = num + 1
!        nn(num) = nni(1)
! middle points 
        j = 0
        DO i = 1+stride, num_nn-1, stride
          j = j + 1
          IF (j > 2) EXIT
          CALL sort_insert(nn, nn_d, nni(i), min_d(i), num)
!          num = num + 1
!          nn(num) = nni(i)
        ENDDO
! last point of the quadrant
        CALL sort_insert(nn, nn_d, nni(num_nn), min_d(num_nn), num)
!        num = num + 1
!        nn(num) = nni(num_nn)
      ELSE IF (num_nn == 1) THEN
        CALL sort_insert(nn, nn_d, nni(1), min_d(1), num)
!        num = num + 1
!        nn(num) = nni(1)
      ENDIF  
    ENDDO
    
END SUBROUTINE nsn2

SUBROUTINE sort_insert(nnic, nnic_d, nn_idx, nn_d, num_nnic)
    IMPLICIT NONE
    INTEGER :: nnic(:), nn_idx, num_nnic
    REAL :: nnic_d(:), nn_d

    INTEGER :: i, j

    DO i = 1, num_nnic + 1 
      IF (nn_d < nnic_d(i)) THEN
        DO j =  num_nnic, i, -1 
          nnic(j+1) = nnic(j)
          nnic_d(j+1) = nnic_d(j) 
        ENDDO
        nnic(i) = nn_idx
        nnic_d(i) = nn_d
        num_nnic = num_nnic + 1
        EXIT
      ENDIF
    ENDDO

END SUBROUTINE sort_insert 


SUBROUTINE coor_vec(qll,quad,cv)
    USE kd, ONLY: ll2xyz
    IMPLICIT NONE

    REAL, INTENT(IN) :: qll(2) 
    INTEGER, INTENT(IN) :: quad
    REAL, INTENT(OUT) :: cv(3,2)

    REAL u, v

    REAL qxyz(3), qxyzuv(3), nv(3)

    IF (quad == 1) THEN
      u = 1.0
      v = 0.0 
    ELSE IF (quad == 2) THEN
      u = 0.0
      v = 1.0 
    ELSE IF (quad == 3) THEN
      u = -1.0
      v = 0.0 
    ELSE IF (quad == 4) THEN
      u = 0.0
      v = -1.0 
    ENDIF

    CALL uv2xyz(u, v, qll(1),qll(2), qxyzuv(1), qxyzuv(2), qxyzuv(3))
    CALL normalize(qxyzuv)
    cv(:,2) = qxyzuv

    CALL ll2xyz(qll, qxyz)
    CALL cross_product2(qxyz,qxyzuv,nv)
    CALL normalize(nv)
    cv(:,1) = nv

END SUBROUTINE coor_vec

SUBROUTINE intersection (p1, p2, p3, p4, p)
    IMPLICIT NONE
    REAL p1(2), p2(2), p3(2), p4(2), p(2)
    REAL gc1(3), gc2(3), e(3)
    REAL pi

    pi = ATAN(1.0) * 4.0

    CALL cross_product1(p1, p2, gc1)
    CALL cross_product1(p4, p3, gc2)
    CALL cross_product2(gc1, gc2, e)

    CALL xyz2ll(e, p)

    IF (gc_dist(p2, p) > pi / 4.0) THEN
      p(2) = p(2) + pi
      p(1) = -p(1)
    END IF
 
    IF (p(2) < 0.0) THEN
      p(2) = p(2) + 2.0 * pi
    END IF

END SUBROUTINE intersection

FUNCTION inner_product(x1, x2)
    IMPLICIT NONE

    REAL x1(3), x2(3), inner_product

    inner_product = x1(1)*x2(1) + x1(2)*x2(2) + x1(3)*x2(3)

END FUNCTION inner_product

SUBROUTINE cross_product1(p1, p2, gc)
    IMPLICIT NONE

    REAL p1(2), p2(2), gc(3)
    REAL a, b, c, d, e, f, g

    a = SIN(p1(1) + p2(1))
    b = SIN(p1(1) - p2(1))
    c = SIN((p1(2) + p2(2))/ 2.0)
    d = SIN((p1(2) - p2(2))/ 2.0)
    e = COS((p1(2) + p2(2))/ 2.0)
    f = COS((p1(2) - p2(2))/ 2.0)
    g = COS(p1(1)) * COS(p2(1)) 

    gc(1) = b * c * f  - a * e * d
    gc(2) = b * e * f  + a * c * d
    gc(3) = 2.0 * g * d * f

END SUBROUTINE cross_product1

SUBROUTINE cross_product2(e1, e2, e)
    IMPLICIT NONE
    REAL e1(3), e2(3), e(3)
 
    e(1) = e1(2) * e2(3) - e2(2) * e1(3)
    e(2) = e1(3) * e2(1) - e2(3) * e1(1)
    e(3) = e1(1) * e2(2) - e2(1) * e1(2)

END SUBROUTINE cross_product2

SUBROUTINE xyz2ll(e, p)
    IMPLICIT NONE
    REAL e(3), p(2)

    p(1) = atan2(e(3), SQRT(e(1) * e(1) + e(2) * e(2)))
    p(2) = atan2(-e(2), e(1))

END SUBROUTINE xyz2ll

SUBROUTINE ll2xyz(p, e)
    IMPLICIT NONE
    REAL p(2)
    REAL e(3)

    e(1) = cos(p(1)) * cos(p(2))
    e(2) = cos(p(1)) * sin(p(2))
    e(3) = sin(p(1))

END SUBROUTINE ll2xyz

SUBROUTINE normalize(x)
    IMPLICIT NONE
    REAL x(3)

    x = x / sqrt(inner_product(x, x))

END SUBROUTINE normalize

END MODULE slint
