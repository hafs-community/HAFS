!==========================================================================
! Module for the k-d tree
!
!  The algorithm is based on Bentley's optimal k-d tree, plus 
!  some modifications of mine. The priority queue was omitted
!  since the number of nearest neighbors to be searched is 
!  always relatively small (3-11). 
!
!  Ning Wang, Nov. 2006:  Initial implementation.
!
!  Ning Wang, Jul. 2011:  Created a new version of the k-d tree, which
!    includes following modifications and enhancements.
!    a). wrapped kd_datastru.F90 and kd.F90 into one module;
!    b). simplified interface to the recursive subroutine SearchRec;
!    c). added a new capability to the k-d tree, such that the search
!        space can be bounded with a pair of hyper-planes for each
!        query (search).
!  Ning Wang, May 2015 : Implemented a thread-safe search algorithm. 
!==========================================================================
MODULE kd
     INTEGER, PARAMETER :: max_d = 3
     INTEGER, PARAMETER :: max_k = 3 

     TYPE Node
       LOGICAL :: bucket   ! true if it is a bucket node
       INTEGER :: discrim  ! discriminant dimension  
       REAL :: cutval      ! the cut value for the space
       INTEGER :: lopt     ! lower point  
       INTEGER :: hipt     ! high point  
       TYPE(Node), POINTER :: loson  
       TYPE(Node), POINTER :: hison 
     END TYPE Node

     TYPE QueryWkSpace
       REAL :: partsum            ! partial sum of distance square to subspace 
       REAL :: q2cut(max_d)   ! query to cut plane distance
       INTEGER :: num_nn          ! number of nearest neighbors found
       REAL :: nnd(max_k)       ! distance square to the nearest neighbors 
       INTEGER :: nni(max_k)    ! nearest neighbor index 
       REAL :: curDistSq          ! current nearest neighbor distance square 
       REAL :: tc1(max_d)       ! vectors point to the testing corner1 (vertex) 
       REAL :: tc2(max_d)       ! vectors point to the testing corner2 (vertex) 
     END TYPE QueryWkSpace

     TYPE QueryInfo
       REAL :: q_xyz(max_d)     ! query point location vector 
       REAL :: hp1(max_d)       ! normal vector for hyperplane1
       REAL :: hp2(max_d)       ! normal vector for hyperplane2
       INTEGER :: k                  ! number of niearest neighbors searching for
     END TYPE QueryInfo 
   
     INTEGER n, d, gc, bottom_level, dir, num_k, nnn
     REAL, ALLOCATABLE :: hp1(:), hp2(:), tc1(:), tc2(:), qry(:)

     REAL dnnd

     TYPE(Node), TARGET, ALLOCATABLE :: nodes(:)
     REAL, ALLOCATABLE :: points(:,:)
     INTEGER, ALLOCATABLE :: perm(:)
     INTEGER, TARGET, ALLOCATABLE :: nni(:)
     REAL, ALLOCATABLE :: nnd(:)
     REAL, ALLOCATABLE :: q2cut(:)
     TYPE(Node), POINTER :: root

     REAL :: curDistSq, partsum
CONTAINS

SUBROUTINE BuildTree(num, dim, points)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: num, dim
    REAL, INTENT(IN) :: points(dim, num)

    INTEGER :: i
        
    IF (ALLOCATED(nodes)) THEN
      DEALLOCATE(nodes)
    END IF
    IF (ALLOCATED(perm)) THEN
      DEALLOCATE(perm)
    END IF
    IF (ALLOCATED(q2cut).AND.ALLOCATED(hp1).AND.ALLOCATED(hp2)) THEN
      DEALLOCATE(q2cut, hp1, hp2)
    END IF
    IF (ALLOCATED(qry).AND.ALLOCATED(tc1).AND.ALLOCATED(tc2)) THEN
      DEALLOCATE(qry, tc1, tc2)
    END IF

    ALLOCATE(nodes(2 * num))
    ALLOCATE(perm(num))
    ALLOCATE(q2cut(dim), hp1(dim), hp2(dim))
    ALLOCATE(qry(dim), tc1(dim), tc2(dim))

    DO i = 1, num
      perm(i) = i
    END DO

    n = num
    d = dim
    dir = 0
    gc = 1

    bottom_level = log(REAL(n)) / log(2.0) + 1
    root => BuildTreeRec(points, 1, n, 1)

END SUBROUTINE BuildTree

SUBROUTINE DeleteTree()

    DEALLOCATE(nodes)
    DEALLOCATE(perm)
    DEALLOCATE(q2cut)
    DEALLOCATE(hp1, hp2, tc1, tc2, qry)
    DEALLOCATE(points)

END SUBROUTINE DeleteTree

RECURSIVE FUNCTION BuildTreeRec(points, l, u, level) RESULT(theNode)

    IMPLICIT NONE

    REAL, INTENT(IN) :: points(d, n)
    INTEGER, INTENT(IN) :: l, u, level
    TYPE(Node), POINTER :: theNode

    INTEGER :: m

    theNode => NewNode() ! get a new node

    IF (level >= bottom_level) THEN
      theNode%bucket = .true.
      theNode%lopt = l
      theNode%hipt = u
    ELSE 
      theNode%bucket = .false.
      m = (l + u ) / 2 
      theNode%discrim = dir2cut(l, u, points)
      CALL partition(points,l, u, m, theNode%discrim) 
      theNode%cutval = points(theNode%discrim, perm(m))
      theNode%loson => BuildTreeRec(points, l, m, level + 1)
      theNode%hison => BuildTreeRec(points, m+1, u, level + 1) 
    END IF 

END FUNCTION BuildTreeRec

SUBROUTINE Set_k(k)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: k
    LOGICAL :: alloc_array = .false.

    IF (k > num_k) THEN
      alloc_array = .true.
    END IF

    num_k = k

    IF (alloc_array) THEN
      IF (ALLOCATED(nni)) THEN
        DEALLOCATE(nni)
      END IF
      IF (ALLOCATED(nnd)) THEN
        DEALLOCATE(nnd)
      END IF

      ALLOCATE(nni(k))
      ALLOCATE(nnd(k))
    ENDIF

END SUBROUTINE Set_k

SUBROUTINE Set_dnnd(d_nnd)
    IMPLICIT NONE
    REAL, INTENT(IN) :: d_nnd

    dnnd = d_nnd

END SUBROUTINE Set_dnnd

SUBROUTINE Set_qry(q)
    IMPLICIT NONE

    REAL, INTENT(IN) :: q(d)

    qry = q

END SUBROUTINE Set_qry

SUBROUTINE Set_hps(hp_1, hp_2)
    IMPLICIT NONE

    REAL::hp_1(d), hp_2(d)

    hp1 = hp_1
    hp2 = hp_2

END SUBROUTINE Set_hps

SUBROUTINE Set_tcs(hp_1, hp_2)
    IMPLICIT NONE

    REAL::hp_1(d), hp_2(d)
    INTEGER i

    DO i = 1, d
      tc1(i) =  REAL(sgn(hp_1(i)))
      tc2(i) =  REAL(sgn(hp_2(i)))
    END DO

END SUBROUTINE Set_tcs

FUNCTION sgn(x)
    IMPLICIT NONE
    REAL x
    INTEGER sgn
    IF (x .GE. 0.0) THEN
        sgn = 1
    ELSE
        sgn = -1
    END IF
END FUNCTION sgn

SUBROUTINE Search(query) 
  
    IMPLICIT NONE

    REAL :: query(d)

    INTEGER :: i

    partsum = 0
    DO i = 1, d
      q2cut(i) = 0.0
    END DO
    
    DO i = 1, num_k
      nnd(i) = dnnd * dnnd
      nni(i) = 0
    END DO
    curDistSq = dnnd * dnnd 
    CALL SearchRec(query, root)

END SUBROUTINE Search
 
RECURSIVE SUBROUTINE SearchRec(query, tnode) 

    IMPLICIT NONE

    REAL, INTENT(IN) :: query(d)
    TYPE(Node), INTENT(IN) :: tnode

    REAL :: cur_q2c, cur_ps, cur_tc1, cur_tc2, distSq, d2o
    INTEGER :: i

    IF (tnode%bucket) THEN
      DO i = tnode%lopt, tnode%hipt
        IF (in_ss(points(1:3, perm(i)), points(1:3, perm(i)))) THEN 
        distSq = inp(query, points(1:3, perm(i))) 
        IF (distSq < curDistSq) THEN
          curDistSq = distSq
          CALL insert(perm(i))
        END IF
        END IF
      END DO   
    ELSE 
      d2o = query(tnode%discrim) - tnode%cutval
      IF (d2o < 0.0) THEN
        cur_ps = partsum
        cur_q2c = q2cut(tnode%discrim)
        cur_tc1 = tc1(tnode%discrim)
        cur_tc2 = tc2(tnode%discrim)
        CALL SearchRec(query, tnode%loson)
        partsum = cur_ps + d2o * d2o - q2cut(tnode%discrim) 
        q2cut(tnode%discrim) = d2o * d2o 
        tc1(tnode%discrim) = max(tnode%cutval, tc1(tnode%discrim))
        tc2(tnode%discrim) = max(tnode%cutval, tc2(tnode%discrim))
        IF (partsum < curDistSq .AND. in_ss(tc1, tc2) ) THEN
          CALL SearchRec(query, tnode%hison) 
        END IF
        q2cut(tnode%discrim) = cur_q2c
        tc1(tnode%discrim) = cur_tc1
        tc2(tnode%discrim) = cur_tc2
      ELSE
        cur_ps = partsum
        cur_q2c = q2cut(tnode%discrim)
        cur_tc1 = tc1(tnode%discrim)
        cur_tc2 = tc2(tnode%discrim)
        CALL SearchRec(query, tnode%hison)
        partsum = cur_ps + d2o * d2o - q2cut(tnode%discrim) 
        q2cut(tnode%discrim) = d2o * d2o 
        tc1(tnode%discrim) = min(tnode%cutval, tc1(tnode%discrim))
        tc2(tnode%discrim) = min(tnode%cutval, tc2(tnode%discrim))
        IF (partsum < curDistSq .AND. in_ss(tc1, tc2)) THEN
          CALL SearchRec(query, tnode%loson) 
        END IF
        q2cut(tnode%discrim) = cur_q2c
        tc1(tnode%discrim) = cur_tc1
        tc2(tnode%discrim) = cur_tc2
      END IF
    END IF

 END SUBROUTINE SearchRec

 FUNCTION in_ss(tc1, tc2)
     IMPLICIT NONE
     REAL, INTENT(IN) :: tc1(d), tc2(d)
     LOGICAL in_ss

     REAL ip, tc(d)
     INTEGER i

     tc = tc1 - qry
     ip = 0.0
     DO i = 1, d
       ip = ip + tc(i) * hp1(i)
     END DO
     IF (ip < 0) THEN
       in_ss = .false.
       RETURN
     ENDIF

     tc = tc2 - qry
     ip = 0.0
     DO i = 1, d
       ip = ip + tc(i) * hp2(i)
     END DO
     IF (ip < 0) THEN
       in_ss = .false.
       RETURN
     ENDIF

     in_ss = .true.
 END FUNCTION in_ss

! Subroutines and functions that are helps creatation and searching k-d tree.
! get a new node
FUNCTION NewNode() 
    IMPLICIT NONE

    TYPE(Node), POINTER :: NewNode

    NewNode => nodes(gc)
    gc = gc + 1
   
END FUNCTION NewNode

! partition the points along dir 'discrim' into lower and upper parts 
SUBROUTINE partition(points, l, u, m, discrim)
    IMPLICIT NONE

    REAL, INTENT(IN) :: points(d, n)
    INTEGER, INTENT(IN) :: l, u, m, discrim

    REAL :: v
    INTEGER :: i, j, t, r, lo

    r = u
    lo = l

    DO WHILE ( r > lo)
      v = points(discrim, perm(r))
      i = lo
      j = r - 1
      DO WHILE (.true.)
        DO WHILE (points(discrim,perm(i)) < v) 
          i = i + 1
        END DO
        DO WHILE (points(discrim, perm(j)) >= v .AND. j > lo)
          j = j - 1
        END DO
        IF (i >= j) EXIT
        t = perm(i)
        perm(i) = perm(j)
        perm(j) = t
      END DO
      t = perm(i)
      perm(i) = perm(r)
      perm(r) = t
      IF (i >= m) r = i - 1;
      IF (i <= m) lo = i + 1
    END DO
      
END SUBROUTINE partition

! function returns the direction to divide
FUNCTION dir2cut(l, u, points)
    IMPLICIT NONE

    REAL :: points(d, n)
    INTEGER :: l, u

    INTEGER :: dir2cut

    dir = dir + 1
    IF (dir > d) THEN
      dir = 1
    END IF
    dir2cut = dir
   
END FUNCTION dir2cut

! function to compute the inner product of p1-p2
FUNCTION inp(p1, p2) 
    IMPLICIT NONE
        
    REAL, INTENT(IN) :: p1(d), p2(d)
    REAL :: inp

    REAL sum, dif
    INTEGER i

    sum = 0
    DO i = 1, d
      dif = p1(i) - p2(i)
      sum = sum + dif * dif
    END DO 

    inp = sum

END FUNCTION inp

! subroutine to insert the current nn  
SUBROUTINE insert(pt_idx)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: pt_idx
 
    INTEGER :: i, j

    DO i = 1, num_k	
      IF (curDistSq < nnd(i)) THEN
        DO j = num_k, i + 1, -1
          nni(j) = nni(j - 1)
          nnd(j) = nnd(j - 1)
        END DO
        nni(i) = pt_idx
        nnd(i) = curDistSq
        nnn = min(nnn+1,num_k)
        EXIT            
      END IF
    END DO
      
    curDistSq = nnd(num_k)
          
END SUBROUTINE insert
          
SUBROUTINE insert_ts(pt_idx, qws, query)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: pt_idx
    TYPE(QueryWkSpace) :: qws
    TYPE(QueryInfo) :: query
 
    INTEGER :: i, j

    DO i = 1, query%k
      IF (qws%curDistSq < qws%nnd(i)) THEN
        DO j = query%k, i + 1, -1
          qws%nni(j) = qws%nni(j - 1)
          qws%nnd(j) = qws%nnd(j - 1)
        END DO
        qws%nni(i) = pt_idx
        qws%nnd(i) = qws%curDistSq
        qws%num_nn = min(qws%num_nn+1,query%k)
        EXIT            
      END IF
    END DO
      
    qws%curDistSq = qws%nnd(query%k)
          
END SUBROUTINE insert_ts
          

! function to return the index array
SUBROUTINE result()
    IMPLICIT NONE
    INTEGER :: i
      
    DO i = 1, num_k
      PRINT*, nni(i)
      PRINT*, points(:, nni(i)), nnd(i)
    END DO 

END SUBROUTINE result

SUBROUTINE init_kd_tree(llpoints, n, k)
    IMPLICIT NONE
    REAL, INTENT(IN) :: llpoints(n,2) 
    INTEGER, INTENT(IN) :: n,k

    INTEGER :: i, seq, dim

    dim = 3
    num_k = 0
    IF (ALLOCATED(points)) THEN
      DEALLOCATE(points)
    ENDIF
    ALLOCATE(points(dim,n))
    CALL lls2xyzs(llpoints, points, n)
    CALL BuildTree(n, dim, points)
    CALL Set_k(k)

END SUBROUTINE init_kd_tree

SUBROUTINE close_kd_tree()

    CALL DeleteTree()

END SUBROUTINE close_kd_tree
         
SUBROUTINE knn_search(q_ll, nn, min_dists, hp1, hp2, d_nnd, num_nn)
    IMPLICIT NONE
    
    REAL, INTENT(IN) :: q_ll(2) 
    REAL, INTENT(IN) :: hp1(3), hp2(3), d_nnd
    REAL, INTENT(OUT) :: min_dists(num_k)
    INTEGER, INTENT(OUT) :: nn(num_k), num_nn

    REAL :: q(3)
    INTEGER i
    
    nnn = 0
    CALL ll2xyz(q_ll, q)
    CALL Set_qry(q)
    CALL Set_hps(hp1, hp2)
    CALL Set_tcs(hp1, hp2)
    CALL Set_dnnd(d_nnd)
    CALL Search(q) 
    
    DO i = 1, num_k
      nn(i) = nni(i)
      min_dists(i) = nnd(i)
    END DO 
    num_nn = nnn

END SUBROUTINE knn_search

SUBROUTINE knn_search_ts(q_ll, nn, min_dists, hp1, hp2, nn_sr, k, num_nn)
    IMPLICIT NONE
    
    REAL, INTENT(IN) :: q_ll(2) 
    REAL, INTENT(IN) :: hp1(3), hp2(3), nn_sr
    INTEGER, INTENT(IN) :: k
    REAL, INTENT(OUT) :: min_dists(k)
    INTEGER, INTENT(OUT) :: nn(k), num_nn

    REAL :: q_xyz(3)
    INTEGER i
    TYPE(QueryWkSpace) qws 
    TYPE(QueryInfo) query

! set up work space for the current thread
    qws%partsum = 0.0
    qws%q2cut(1:d) = 0.0
    qws%nnd(1:k) = nn_sr * nn_sr
    qws%nni(1:k) = 0
    qws%curDistSq = nn_sr * nn_sr 
    DO i = 1, d
      qws%tc1(i) =  REAL(sgn(hp1(i)))
      qws%tc2(i) =  REAL(sgn(hp2(i)))
    ENDDO
    qws%num_nn = 0

! set up query info for the current thread
    CALL ll2xyz(q_ll, q_xyz)
    query%q_xyz = q_xyz
    query%hp1 = hp1
    query%hp2 = hp2
    query%k = k
   
    CALL Searchrec_ts(query, root, qws) 

    num_nn = qws%num_nn 
    nn(1:num_nn) = qws%nni(1:num_nn)
    min_dists(1:num_nn) = qws%nnd(1:num_nn)
    
END SUBROUTINE knn_search_ts

RECURSIVE SUBROUTINE SearchRec_ts(query, tnode, qws) 

    IMPLICIT NONE

    TYPE(QueryInfo), INTENT(IN) :: query
    TYPE(Node), INTENT(IN) :: tnode
    TYPE(QueryWkSpace), INTENT(INOUT) :: qws 

    REAL :: cur_q2c, cur_ps, cur_tc1, cur_tc2, distSq, d2o
    INTEGER :: i

    IF (tnode%bucket) THEN
      DO i = tnode%lopt, tnode%hipt
        IF (pt_in_ss(points(1:3, perm(i)), query)) THEN 
        distSq = inp(query%q_xyz, points(1:3, perm(i))) 
        IF (distSq < qws%curDistSq) THEN
          qws%curDistSq = distSq
          CALL insert_ts(perm(i), qws, query)
        END IF
        END IF
      END DO   
    ELSE 
      d2o = query%q_xyz(tnode%discrim) - tnode%cutval
      IF (d2o < 0.0) THEN
        cur_ps = qws%partsum
        cur_q2c = qws%q2cut(tnode%discrim)
        cur_tc1 = qws%tc1(tnode%discrim)
        cur_tc2 = qws%tc2(tnode%discrim)
        CALL SearchRec_ts(query, tnode%loson,qws)
        qws%partsum = cur_ps + d2o * d2o - qws%q2cut(tnode%discrim) 
        qws%q2cut(tnode%discrim) = d2o * d2o 
        qws%tc1(tnode%discrim) = max(tnode%cutval, qws%tc1(tnode%discrim))
        qws%tc2(tnode%discrim) = max(tnode%cutval, qws%tc2(tnode%discrim))
        IF (qws%partsum < qws%curDistSq .AND. in_ss_ts(query, qws) ) THEN
          CALL SearchRec_ts(query, tnode%hison, qws) 
        END IF
        qws%q2cut(tnode%discrim) = cur_q2c
        qws%tc1(tnode%discrim) = cur_tc1
        qws%tc2(tnode%discrim) = cur_tc2
      ELSE
        cur_ps = qws%partsum
        cur_q2c = qws%q2cut(tnode%discrim)
        cur_tc1 = qws%tc1(tnode%discrim)
        cur_tc2 = qws%tc2(tnode%discrim)
        CALL SearchRec_ts(query, tnode%hison, qws)
        qws%partsum = cur_ps + d2o * d2o - qws%q2cut(tnode%discrim) 
        qws%q2cut(tnode%discrim) = d2o * d2o 
        qws%tc1(tnode%discrim) = min(tnode%cutval, qws%tc1(tnode%discrim))
        qws%tc2(tnode%discrim) = min(tnode%cutval, qws%tc2(tnode%discrim))
        IF (qws%partsum < qws%curDistSq .AND. in_ss_ts(query, qws)) THEN
          CALL SearchRec_ts(query, tnode%loson, qws) 
        END IF
        qws%q2cut(tnode%discrim) = cur_q2c
        qws%tc1(tnode%discrim) = cur_tc1
        qws%tc2(tnode%discrim) = cur_tc2
      END IF
    END IF

END SUBROUTINE SearchRec_ts

FUNCTION in_ss_ts(qry, qws) ! in search subspace test
    IMPLICIT NONE
    TYPE(QueryInfo), INTENT(IN) :: qry
    TYPE(QueryWkSpace), INTENT(IN) :: qws
    LOGICAL in_ss_ts

    REAL ip, tc(d)
    INTEGER i

    tc = qws%tc1 - qry%q_xyz
    ip = 0.0
    DO i = 1, d
      ip = ip + tc(i) * qry%hp1(i)
    END DO
    IF (ip < 0) THEN
      in_ss_ts = .false.
      RETURN
    ENDIF

    tc = qws%tc2 - qry%q_xyz
    ip = 0.0
    DO i = 1, d
      ip = ip + tc(i) * qry%hp2(i)
    END DO
    IF (ip < 0) THEN
      in_ss_ts = .false.
      RETURN
    ENDIF

    in_ss_ts = .true.
END FUNCTION in_ss_ts

FUNCTION pt_in_ss(pt, qry) ! in search subspace test
    IMPLICIT NONE
    REAL, INTENT(IN) :: pt(d)
    TYPE(QueryInfo), INTENT(IN) :: qry
    LOGICAL pt_in_ss

    REAL ip, pt_t(d)
    INTEGER i

    pt_t = pt - qry%q_xyz
 
    ip = 0.0
    DO i = 1, d
      ip = ip + pt_t(i) * qry%hp1(i)
    END DO
    IF (ip < 0) THEN
      pt_in_ss = .false.
      RETURN
    ENDIF

    ip = 0.0
    DO i = 1, d
      ip = ip + pt_t(i) * qry%hp2(i)
    END DO
    IF (ip < 0) THEN
      pt_in_ss = .false.
      RETURN
    ENDIF

    pt_in_ss = .true.
END FUNCTION pt_in_ss

! Subroutines and functions that are helps creatation and searching k-d tree.
SUBROUTINE ll2xyz(p, e)
    IMPLICIT NONE
    REAL p(2)
    REAL e(3)

    e(1) = cos(p(1)) * cos(p(2))
    e(2) = cos(p(1)) * sin(p(2))
    e(3) = sin(p(1))

END SUBROUTINE ll2xyz

SUBROUTINE lls2xyzs(llpts, xyzpts, n)
    IMPLICIT NONE

    INTEGER :: n
    REAL :: llpts(n, 2), xyzpts(3, n)  
   
    INTEGER :: i

    DO i = 1, n
      xyzpts(1,i) = cos(llpts(i,1)) * cos(llpts(i,2))
      xyzpts(2,i) = cos(llpts(i,1)) * sin(llpts(i,2))
      xyzpts(3,i) = sin(llpts(i,1))
    END DO 

END SUBROUTINE lls2xyzs

END MODULE kd
