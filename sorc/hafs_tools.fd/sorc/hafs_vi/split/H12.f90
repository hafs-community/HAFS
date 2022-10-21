    subroutine h12(mode,lpivot,l1,m,u,iue,up,c,ice,icv,ncv)
    !From http://jacobwilliams.github.io/slsqp/proc/h12.html
    implicit none

    integer,intent(in):: mode
    !! `1` or `2` --selects algorithm ***h1*** to construct and apply a
    !! householder transformation, or algorithm ***h2*** to apply a
    !! previously constructed transformation.
    integer,intent(in):: lpivot !! the index of the pivot element
    integer,intent(in):: l1
    !! if `l1 <= m` the transformation will be constructed to
    !! zero elements indexed from `l1` through `m`.
    !! if `l1 > m` the subroutine does an identity transformation.
    integer,intent(in):: m      !! see `li`.
    integer,intent(in):: iue    !! see `u`.
    integer,parameter:: wp=8
    real(wp),dimension(iue,*),intent(inout) :: u
    !! on entry with `mode = 1`, `u` contains the pivot
    !! vector.  `iue` is the storage increment between elements. on exit when `mode = 1`, `u` and `up` contain quantities
    !! defining the vector `u` of the householder transformation. on entry with `mode = 2`, `u` and `up` should contain quantities
    !! previously computed with `mode = 1`. these will not be modified during the entry with `mode = 2`. `dimension[u(iue,m)]`
    real(wp),intent(inout)                  :: up     !! see `u`.
    real(wp),dimension(*),intent(inout)     :: c
    !! on entry with `mode = 1 or 2`, `c` contains a matrix which
    !! will be regarded as a set of vectors to which the householder transformation is
    !! to be applied. on exit `c` contains the set of transformed vectors.
    integer,intent(in):: ice    !! storage increment between elements of vectors in `c`.
    integer,intent(in):: icv    !! storage increment between vectors in `c`.
    integer,intent(in):: ncv
    !! number of vectors in `c` to be transformed. if `ncv <= 0`
    !! no operations will be done on `c`.

    integer  :: i, i2, i3, i4, incr, j
    real(wp) :: b, cl, clinv, sm
    real :: zero, one

    zero=0.
    one=1.
    if ( 0>=lpivot .or. lpivot>=l1 .or. l1>m ) return
    cl = abs(u(1,lpivot))
    if ( mode/=2 ) then
        ! construct the transformation.
        do j = l1 , m
            cl = max(abs(u(1,j)),cl)
        end do
        if ( cl<=zero ) return
        clinv = one/cl
        sm = (u(1,lpivot)*clinv)**2
        do j = l1 , m
            sm = sm + (u(1,j)*clinv)**2
        end do
        cl = cl*sqrt(sm)
        if ( u(1,lpivot)>zero ) cl = -cl
        up = u(1,lpivot) - cl
        u(1,lpivot) = cl
    else if ( cl<=zero ) then
        return
    end if

    if ( ncv>0 ) then
        ! apply the transformation i+u*(u**t)/b to c.
        b = up*u(1,lpivot)
        ! b must be nonpositive here.
        if ( b<zero ) then
            b = one/b
            i2 = 1 - icv + ice*(lpivot-1)
            incr = ice*(l1-lpivot)
            do j = 1 , ncv
                i2 = i2 + icv
                i3 = i2 + incr
                i4 = i3
                sm = c(i2)*up
                do i = l1 , m
                    sm = sm + c(i3)*u(1,i)
                    i3 = i3 + ice
                end do
                if ( abs(sm)>zero ) then
                    sm = sm*b
                    c(i2) = c(i2) + sm*up
                    do i = l1 , m
                        c(i4) = c(i4) + sm*u(1,i)
                        i4 = i4 + ice
                    end do
                end if
            end do
        end if
    end if

    end subroutine h12
