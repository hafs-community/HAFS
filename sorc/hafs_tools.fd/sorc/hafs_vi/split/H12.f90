    subroutine h12(mode,lpivot,l1,m,u,iue,up,c,ice,icv,ncv)
    !Adopted from http://jacobwilliams.github.io/slsqp/proc/h12.html
    !History Jacob Williams, refactored into modern Fortran, Jan. 2016.
    !Modern Fortran Edition of the SLSQP Optimizer
    !https://github.com/jacobwilliams/slsqp
    !
    !Copyright (c) 2016-2024, Jacob Williams
    !All rights reserved.
    !
    !Redistribution and use in source and binary forms, with or without modification,
    !are permitted provided that the following conditions are met:
    !
    !* Redistributions of source code must retain the above copyright notice, this
    !  list of conditions and the following disclaimer.
    !
    !* Redistributions in binary form must reproduce the above copyright notice, this
    !  list of conditions and the following disclaimer in the documentation and/or
    !  other materials provided with the distribution.
    !
    !* The names of its contributors may not be used to endorse or promote products
    !  derived from this software without specific prior written permission.
    !
    !THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
    !ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    !WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    !DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
    !ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
    !(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    !LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    !ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    !(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    !SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    !
    !--------------------------------------------------------------------------------
    !Original License:
    !
    !SLSQP - Sequential Least Squares Programming
    !Copyright (c) 1988, Dieter Kraft (kraft@hm.edu)
    !Copyright (c) 1994, Association for Computing Machinery
    !
    !Permission is hereby granted, free of charge, to any person obtaining a
    !copy of this software and associated documentation files (the
    !"Software"), to deal in the Software without restriction, including
    !without limitation the rights to use, copy, modify, merge, publish,
    !distribute, sublicense, and/or sell copies of the Software, and to
    !permit persons to whom the Software is furnished to do so, subject to
    !the following conditions:
    !
    !The above copyright notice and this permission notice shall be included
    !in all copies or substantial portions of the Software.
    !
    !THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    !OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    !MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    !IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    !CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    !TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    !SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
