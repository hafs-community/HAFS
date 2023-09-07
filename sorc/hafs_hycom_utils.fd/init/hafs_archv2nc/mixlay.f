      subroutine mixlay(mld,r,p,flag,ii,jj,kk)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),r(ii,jj,kk),p(ii,jj,kk+1),flag
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value.  can also be used for a temperature
c     mixed layer by providing -temp in r.
c
c  2) input arguments:
c       mld   - density (or -temperature) at the mld (> r(:,:,1))
c       r     - density (or -temperature) in layer space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       flag  - data void (land) marker
c       ii    - 1st dimension of r,p
c       jj    - 2nd dimension of r,p
c       kk    - 3rd dimension of r  (number of layers)
c
c  3) output arguments:
c       mld   - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c           r(:,:,   1) <= mld(:,:)
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, July 2003.
c*
c**********
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    q,z,zc,zm
c
      itest = ii/2
      jtest = jj/2
c
      do j= 1,jj
        do i= 1,ii
          if     (r(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            z = p(i,j,kk+1)
            do k= 2,kk
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f8.3,f9.2)')
     &            i,j,k,
     &            '   r,rmld,zc =',r(i,j,k),mld(i,j),
     &                             0.5*(p(i,j,k)+p(i,j,k+1))
              endif !debug
*               
              if     (r(i,j,k).ge.mld(i,j)) then
c
c               MLD is between the centers of layers k-1 and k
c
                zm = 0.5*(p(i,j,k)+p(i,j,k-1))
                zc = 0.5*(p(i,j,k)+p(i,j,k+1))
                q  = (r(i,j,k)-mld(i,j))/(r(i,j,k)-r(i,j,k-1))
                z  = q*zm + (1.0-q)*zc
                exit
              endif
            enddo !k
            mld(i,j) = z
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_ild(mld,t,p,flag,ii,jj,kk, tjump)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),t(ii,jj,kk),p(ii,jj,kk+1),flag,tjump
c
c**********
c*
c  1) calculate the mixed layer depth based on the temperature difference
c     w.r.t. the surface value.
c
c  2) input arguments:
c       t     - temperature in layer space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       flag  - data void (land) marker
c       tjump - temperature jump at the mld w.r.t. the surface (>0.0)
c       ii    - 1st dimension of t,p
c       jj    - 2nd dimension of t,p
c       kk    - 3rd dimension of t  (number of layers)
c
c  3) output arguments:
c       mld   - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, Feb. 2010.
c*
c**********
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    q,tmld,tneg,z,zc,zm
c
      itest = ii/2
      jtest = jj/2
c
      do j= 1,jj
        do i= 1,ii
          if     (t(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            tmld = t(i,j,1) + tjump
            tneg = t(i,j,1) - tjump
            z    = p(i,j,kk+1)
            do k= 2,kk
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,3f8.3,f9.2)')
     &            i,j,k,
     &            '   t,tmld,tneg,zc =',t(i,j,k),tmld,tneg,
     &                             0.5*(p(i,j,k)+p(i,j,k+1))
              endif !debug
*               
              if     (t(i,j,k).ge.tmld) then
c
c               MLD is between the centers of layers k-1 and k
c
                zm = 0.5*(p(i,j,k)+p(i,j,k-1))
                zc = 0.5*(p(i,j,k)+p(i,j,k+1))
                q  = (t(i,j,k)-tmld)/(t(i,j,k)-t(i,j,k-1))
                z  = q*zm + (1.0-q)*zc
                exit
              elseif (t(i,j,k).le.tneg) then
c
c               MLD is between the centers of layers k-1 and k
c
                zm = 0.5*(p(i,j,k)+p(i,j,k-1))
                zc = 0.5*(p(i,j,k)+p(i,j,k+1))
                q  = (t(i,j,k)-tneg)/(t(i,j,k)-t(i,j,k-1))
                z  = q*zm + (1.0-q)*zc
                exit
              endif
            enddo !k
            mld(i,j) = z
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_loc(mld,temp,saln,p,flag,ii,jj,kk, tmljmp)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),
     &        temp(ii,jj,kk),saln(ii,jj,kk),p(ii,jj,kk+1),flag,tmljmp
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value equivalent to a temperature difference
c     of tmljmp.  uses locally referenced potential density.
c
c  2) input arguments:
c       temp   - temperature in layer space
c       saln   - salinity    in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry
c       flag   - data void (land) marker
c       ii     - 1st dimension of temp,saln,p
c       jj     - 2nd dimension of temp,saln,p
c       kk     - 3rd dimension of temp,saln  (number of layers)
c       tmljmp - data void (land) marker
c
c  3) output arguments:
c       mld    - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, December 2006.
c*
c**********
c
      real       epsil
      parameter (epsil=1.0e-11)
c
c-----------------------------------------------------------------------------
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      real    sigloc,dsiglocdt,dsiglocds
      real    s,t,prs
      real    c1p,c2p,c3p,c4p,c5p,c6p,c7p
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1p(prs)=alphap(1)+1.e-5*prs*(betap(1)+1.e-5*prs*gammap(1))
      c2p(prs)=alphap(2)+1.e-5*prs*(betap(2)+1.e-5*prs*gammap(2))
      c3p(prs)=alphap(3)+1.e-5*prs*(betap(3)+1.e-5*prs*gammap(3))
      c4p(prs)=alphap(4)+1.e-5*prs*(betap(4)+1.e-5*prs*gammap(4))
      c5p(prs)=alphap(5)+1.e-5*prs*(betap(5)+1.e-5*prs*gammap(5))
      c6p(prs)=alphap(6)+1.e-5*prs*(betap(6)+1.e-5*prs*gammap(6))
      c7p(prs)=alphap(7)+1.e-5*prs*(betap(7)+1.e-5*prs*gammap(7))
      sigloc(t,s,prs)=c1p(prs)+c3p(prs)*s+
     &       t*(c2p(prs)+c5p(prs)*s+t*(c4p(prs)+c7p(prs)*s+c6p(prs)*t))
      dsiglocdt(t,s,prs)=(c2p(prs)+c5p(prs)*s+
     &       2.*t*(c4p(prs)+c7p(prs)*s+1.5*c6p(prs)*t))
      dsiglocds(t,s,prs)=(c3p(prs)+t*(c5p(prs)+t*c7p(prs)))
c-----------------------------------------------------------------------------
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    onem
      real    zgrid(kk+1),thloc(kk),dp(kk),prsk,sigmlj
      REAL    thsur,thtop,thbot,thjmp(kk)
      REAL    alfadt,betads
c
      itest = ii/2
      jtest = jj/2
c
      onem = 9806.0
c
      do j= 1,jj
        do i= 1,ii
          if     (temp(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            sigmlj = -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0)
            sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,i3,a,2f8.4)')
     &          i,j,k,
     &          '   sigmlj =',
     &          -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0),
     &          sigmlj
            endif      !debug
*
            thloc(1) = sigloc(temp(i,j,1),saln(i,j,1),0.0)
            zgrid(1) = -0.5*p(i,j,2)
               dp(1) =      p(i,j,2)
            do k= 2,kk
              prsk  = onem*p(i,j,k)
              alfadt=0.5*(dsiglocdt(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocdt(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (temp(i,j,k-1)-temp(i,j,k))
              betads=0.5*(dsiglocds(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocds(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (saln(i,j,k-1)-saln(i,j,k))
              thloc(k) = thloc(k-1)-alfadt-betads
              zgrid(k) = -0.5*(p(i,j,k+1) + p(i,j,k))
                 dp(k) =       p(i,j,k+1) - p(i,j,k)
            enddo !k
            zgrid(kk+1) = -p(i,j,kk+1)
c
            mld(i,j) = -zgrid(kk+1)  !bottom
            thjmp(1) = 0.0
            thsur = thloc(1)
            do k=2,kk
              thsur    = min(thloc(k),thsur)  !ignore surface inversion
              thjmp(k) = max(thloc(k)-thsur,
     &                       thjmp(k-1)) !stable profile simplifies the code
*               
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f8.3,f8.4,f9.2)')
     &            i,j,k,
     &            '   th,thsur,jmp,zc =',
     &            thloc(k),thsur,thjmp(k),-zgrid(k)
              endif !debug
c               
              if (thjmp(k).ge.sigmlj) then
c               
c ---           find the two densities on the interface between layers
c ---           k-1 and k, using PLM assuming layers k-2 and k+1 are PCM.
c
                if     (k.eq.2) then
                  thtop = thjmp(1)
                else
                  thtop = thjmp(k-1) +
     &                      min(thjmp(k-1)-thjmp(k-2),
     &                          thjmp(k)  -thjmp(k-1) )
                endif !k.eq.2:else
                if     (k.eq.kk) then
                  thbot = thjmp(k)
                else
                  thsur      = min(thloc(k+1),thsur)
                  thjmp(k+1) = max(thloc(k+1)-thsur,
     &                             thjmp(k))
                  thbot = thjmp(k) -
     &                      min(thjmp(k+1)-thjmp(k),
     &                          thjmp(k)  -thjmp(k-1) )
                endif !k.eq.kk:else
                if     (thtop.gt.thbot) then
                  thtop = 0.5*(thtop+thbot)  !make stable at interface
                  thbot = thtop
                endif
c                   
                if      (thtop.gt.sigmlj) then
c                 
c ---             in bottom half of layer k-1
c
                  mld(i,j) =
     &              -zgrid(k-1) +
     &                       0.5*dp(k-1)*
     &                       (sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &              i,j,k,
     &              '   bot half: z,dp,q =',
     &               -zgrid(k-1),
     &                dp(k-1),
     &                   0.5*(sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
                endif !debug
*                 
                elseif (thbot.ge.sigmlj) then
c               
c ---             at layer interface
c
                  mld(i,j) =
     &              -zgrid(k-1) + 0.5*dp(k-1)
*                 
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &                i,j,k,
     &                '  interface: z,dp,q =',
     &                 -zgrid(k-1),
     &                  dp(k-1),
     &                  -0.5
                  endif !debug
*                 
                else
c                 
c ---             in top half of layer k
c
                  mld(i,j) =
     &              -zgrid(k) -
     &                       0.5*dp(k)*
     &                       (1.0-(sigmlj  +epsil-thbot)/
     &                            (thjmp(k)+epsil-thbot) )
*                 
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &                i,j,k,
     &                '   top half: z,dp,q =',
     &                 -zgrid(k),
     &                  dp(k),
     &                 -0.5*(1.0-(sigmlj  +epsil-thbot)/
     &                           (thjmp(k)+epsil-thbot) )
                  endif !debug
*                 
                endif !part of layer
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,2f8.3,f8.4,f9.2)')
     &              i,j,k,
     &              '   thsur,top,bot,dpmixl =',
     &              thsur,thtop,thbot,mld(i,j)
                endif !debug
*                 
                exit  !calculated dpmixl
              endif  !found dpmixl layer
            enddo !k
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_locppm(mld,temp,saln,p,flag,ii,jj,kk, tmljmp)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),
     &        temp(ii,jj,kk),saln(ii,jj,kk),p(ii,jj,kk+1),flag,tmljmp
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value equivalent to a temperature difference
c     of tmljmp.  uses locally referenced potential density.
c
c  2) input arguments:
c       temp   - temperature in layer space
c       saln   - salinity    in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry
c       flag   - data void (land) marker
c       ii     - 1st dimension of temp,saln,p
c       jj     - 2nd dimension of temp,saln,p
c       kk     - 3rd dimension of temp,saln  (number of layers)
c       tmljmp - data void (land) marker
c
c  3) output arguments:
c       mld    - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, December 2006.
c*
c**********
c
      real       epsil
      parameter (epsil=1.0e-11)
c
c-----------------------------------------------------------------------------
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      real    sigloc,dsiglocdt,dsiglocds
      real    s,t,prs
      real    c1p,c2p,c3p,c4p,c5p,c6p,c7p
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1p(prs)=alphap(1)+1.e-5*prs*(betap(1)+1.e-5*prs*gammap(1))
      c2p(prs)=alphap(2)+1.e-5*prs*(betap(2)+1.e-5*prs*gammap(2))
      c3p(prs)=alphap(3)+1.e-5*prs*(betap(3)+1.e-5*prs*gammap(3))
      c4p(prs)=alphap(4)+1.e-5*prs*(betap(4)+1.e-5*prs*gammap(4))
      c5p(prs)=alphap(5)+1.e-5*prs*(betap(5)+1.e-5*prs*gammap(5))
      c6p(prs)=alphap(6)+1.e-5*prs*(betap(6)+1.e-5*prs*gammap(6))
      c7p(prs)=alphap(7)+1.e-5*prs*(betap(7)+1.e-5*prs*gammap(7))
      sigloc(t,s,prs)=c1p(prs)+c3p(prs)*s+
     &       t*(c2p(prs)+c5p(prs)*s+t*(c4p(prs)+c7p(prs)*s+c6p(prs)*t))
      dsiglocdt(t,s,prs)=(c2p(prs)+c5p(prs)*s+
     &       2.*t*(c4p(prs)+c7p(prs)*s+1.5*c6p(prs)*t))
      dsiglocds(t,s,prs)=(c3p(prs)+t*(c5p(prs)+t*c7p(prs)))
c-----------------------------------------------------------------------------
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    onem
      real    zgrid(kk+1),thloc(kk),dp(kk),prsk,sigmlj,z
      REAL    thsur,thtop,thjmp(kk)
      REAL    alfadt,betads
c
      itest = ii/2
      jtest = jj/2
c
      onem  = 9806.0  !pressure units
c
      do j= 1,jj
        do i= 1,ii
          if     (temp(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            sigmlj = -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0)
            sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,i3,a,2f8.4)')
     &          i,j,k,
     &          '   sigmlj =',
     &          -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0),
     &          sigmlj
            endif      !debug
*
            thloc(1) = sigloc(temp(i,j,1),saln(i,j,1),0.0)
            zgrid(1) = -0.5*p(i,j,2)
               dp(1) =      p(i,j,2)
            do k= 2,kk
              prsk  = onem*p(i,j,k)
              alfadt=0.5*(dsiglocdt(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocdt(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (temp(i,j,k-1)-temp(i,j,k))
              betads=0.5*(dsiglocds(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocds(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (saln(i,j,k-1)-saln(i,j,k))
              thloc(k) = thloc(k-1)-alfadt-betads
              zgrid(k) = -0.5*(p(i,j,k+1) + p(i,j,k))
                 dp(k) =       p(i,j,k+1) - p(i,j,k)
              zgrid(k) = min( zgrid(k), zgrid(k-1) - 0.001 ) !negative
                 dp(k) = max(    dp(k), 0.001 )
            enddo !k
            zgrid(kk+1) = -p(i,j,kk+1)
c
            mld(i,j) = -zgrid(kk+1)  !bottom
            thjmp(1) = 0.0
            thsur = thloc(1)
            do k=2,kk
              thsur    = min(thloc(k),thsur)  !ignore surface inversion
              thjmp(k) = max(thloc(k)-thsur,
     &                       thjmp(k-1)) !stable profile simplifies the code
*               
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f8.3,f8.4,f9.2)')
     &            i,j,k,
     &            '   th,thsur,jmp,zc =',
     &            thloc(k),thsur,thjmp(k),-zgrid(k)
              endif !debug
c               
              if (thjmp(k).ge.sigmlj) then
c
c ---           find the density on the interface between layers
c ---           k-1 and k, using the same cubic polynominal as PQM
c
                if     (k.eq.2) then
c ---             linear between cell centers
                  thtop = thjmp(1) + (thjmp(2)-thjmp(1))*
     &                               dp(1)/(dp(1)+dp(2))
                elseif (k.eq.kk) then
c ---             linear between cell centers
                  thtop = thjmp(kk) + (thjmp(kk-1)-thjmp(kk))*
     &                                 dp(kk)/(dp(kk)+dp(kk-1))
                else
                  thsur      = min(thloc(k+1),thsur)
                  thjmp(k+1) = max(thloc(k+1)-thsur,
     &                             thjmp(k))
                  z     = zgrid(k-1) - 0.5*dp(k-1)
                  thtop = thjmp(k-2)*((z        -zgrid(k-1))*
     &                                (z        -zgrid(k  ))*
     &                                (z        -zgrid(k+1)) )/
     &                               ((zgrid(k-2)-zgrid(k-1))*
     &                                (zgrid(k-2)-zgrid(k  ))*
     &                                (zgrid(k-2)-zgrid(k+1)) ) +
     &                    thjmp(k-1)*((z        -zgrid(k-2))*
     &                                (z        -zgrid(k  ))*
     &                                (z        -zgrid(k+1)) )/
     &                               ((zgrid(k-1)-zgrid(k-2))*
     &                                (zgrid(k-1)-zgrid(k  ))*
     &                                (zgrid(k-1)-zgrid(k+1)) ) +
     &                    thjmp(k  )*((z        -zgrid(k-2))*
     &                                (z        -zgrid(k-1))*
     &                                (z        -zgrid(k+1)) )/
     &                               ((zgrid(k  )-zgrid(k-2))*
     &                                (zgrid(k  )-zgrid(k-1))*
     &                                (zgrid(k  )-zgrid(k+1)) ) +
     &                    thjmp(k+1)*((z        -zgrid(k-2))*
     &                                (z        -zgrid(k-1))*
     &                                (z        -zgrid(k  )) )/
     &                               ((zgrid(k+1)-zgrid(k-2))*
     &                                (zgrid(k+1)-zgrid(k-1))*
     &                                (zgrid(k+1)-zgrid(k  )) )
                  thtop = max( thjmp(k-1), min( thjmp(k), thtop ) )
                endif !k.eq.2:k.eq.kk:else
c                   
                if      (thtop.ge.sigmlj) then
c                 
c ---             in bottom half of layer k-1, use linear interpolation
c
                  mld(i,j) =
     &              -zgrid(k-1) +
     &                       0.5*dp(k-1)*
     &                       (sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &              i,j,k,
     &              '   bot half: z,dp,q =',
     &               -zgrid(k-1),
     &                dp(k-1),
     &                   0.5*(sigmlj+epsil-thjmp(k-1))/
     &                       (thtop +epsil-thjmp(k-1))
                endif !debug
*                 
                else
c                 
c ---             in top half of layer k,  use linear interpolation
c
                  mld(i,j) =
     &              -zgrid(k) -
     &                       0.5*dp(k)*
     &                       (1.0-(sigmlj  +epsil-thtop)/
     &                            (thjmp(k)+epsil-thtop) )
*                 
                  if (ldebug_dpmixl .and.
     &                i.eq.itest.and.j.eq.jtest) then
                    write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &                i,j,k,
     &                '   top half: z,dp,q =',
     &                 -zgrid(k),
     &                  dp(k),
     &                 -0.5*(1.0-(sigmlj  +epsil-thtop)/
     &                           (thjmp(k)+epsil-thtop) )
                  endif !debug
*                 
                endif !part of layer
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f8.3,f8.4,f9.2)')
     &              i,j,k,
     &              '   thsur,top,dpmixl =',
     &              thsur,thtop,mld(i,j)
                endif !debug
*                 
                exit  !calculated dpmixl
              endif  !found dpmixl layer
            enddo !k
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_loclcc(mld,temp,saln,p,flag,ii,jj,kk, tmljmp)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),
     &        temp(ii,jj,kk),saln(ii,jj,kk),p(ii,jj,kk+1),flag,tmljmp
c
c**********
c*
c  1) calculate the mixed layer depth based on the density difference
c     w.r.t. the surface value equivalent to a temperature difference
c     of tmljmp.  uses locally referenced potential density.
c     LCC (linear between cell centers) version.
c
c  2) input arguments:
c       temp   - temperature in layer space
c       saln   - salinity    in layer space
c       p      - layer interface depths (non-negative m)
c                  p(:,:,   1) is the surface
c                  p(:,:,kk+1) is the bathymetry
c       flag   - data void (land) marker
c       ii     - 1st dimension of temp,saln,p
c       jj     - 2nd dimension of temp,saln,p
c       kk     - 3rd dimension of temp,saln  (number of layers)
c       tmljmp - data void (land) marker
c
c  3) output arguments:
c       mld    - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, December 2006.
c*
c**********
c
      real       epsil
      parameter (epsil=1.0e-11)
c
c-----------------------------------------------------------------------------
c
c --- sub-coefficients for locally referenced sigma
c --- a fit towards Jackett & McDougall (1995)
      real, parameter, dimension(7) ::
     &  alphap = (/ -0.1364705627213484   , 0.04681812123458564,
     &               0.80700383913187     ,-0.007453530323180844,
     &              -0.002944183249153631 , 0.00003435702568990446,
     &               0.0000348657661057688 /)
     & ,betap  = (/  0.05064226654169138  ,-0.0003571087848996894,
     &              -0.0000876148051892879, 5.252431910751829e-6,
     &               1.579762259448864e-6 ,-3.466867400295792e-8,
     &              -1.687643078774232e-8 /)
     & ,gammap = (/ -5.526396144304812e-6 , 4.885838128243163e-8,
     &               9.96026931578033e-9  ,-7.251389796582352e-10,
     &              -3.987360250058777e-11, 4.006307891935698e-12,
     &               8.26367520608008e-13 /)
c
      real    sigloc,dsiglocdt,dsiglocds
      real    s,t,prs
      real    c1p,c2p,c3p,c4p,c5p,c6p,c7p
c --- locally referenced sigma, a fit towards Jackett & McDougall (1995)
c --- t: potential temperature; s: psu; prs: pressure
      c1p(prs)=alphap(1)+1.e-5*prs*(betap(1)+1.e-5*prs*gammap(1))
      c2p(prs)=alphap(2)+1.e-5*prs*(betap(2)+1.e-5*prs*gammap(2))
      c3p(prs)=alphap(3)+1.e-5*prs*(betap(3)+1.e-5*prs*gammap(3))
      c4p(prs)=alphap(4)+1.e-5*prs*(betap(4)+1.e-5*prs*gammap(4))
      c5p(prs)=alphap(5)+1.e-5*prs*(betap(5)+1.e-5*prs*gammap(5))
      c6p(prs)=alphap(6)+1.e-5*prs*(betap(6)+1.e-5*prs*gammap(6))
      c7p(prs)=alphap(7)+1.e-5*prs*(betap(7)+1.e-5*prs*gammap(7))
      sigloc(t,s,prs)=c1p(prs)+c3p(prs)*s+
     &       t*(c2p(prs)+c5p(prs)*s+t*(c4p(prs)+c7p(prs)*s+c6p(prs)*t))
      dsiglocdt(t,s,prs)=(c2p(prs)+c5p(prs)*s+
     &       2.*t*(c4p(prs)+c7p(prs)*s+1.5*c6p(prs)*t))
      dsiglocds(t,s,prs)=(c3p(prs)+t*(c5p(prs)+t*c7p(prs)))
c-----------------------------------------------------------------------------
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer i,j,k
      integer itest,jtest
      real    onem
      real    zgrid(kk+1),thloc(kk),dp(kk),prsk,sigmlj
      REAL    thsur,thtop,thbot,thjmp(kk)
      REAL    alfadt,betads
c
      itest = ii/2
      jtest = jj/2
c
      onem = 9806.0
c
      do j= 1,jj
        do i= 1,ii
          if     (temp(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            sigmlj = -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0)
            sigmlj = max(sigmlj,tmljmp*0.03)  !cold-water fix
*
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,i3,a,2f8.4)')
     &          i,j,k,
     &          '   sigmlj =',
     &          -tmljmp*dsiglocdt(temp(i,j,1),saln(i,j,1),0.0),
     &          sigmlj
            endif      !debug
*
            thloc(1) = sigloc(temp(i,j,1),saln(i,j,1),0.0)
            zgrid(1) = -0.5*p(i,j,2)
               dp(1) =      p(i,j,2)
            do k= 2,kk
              prsk  = onem*p(i,j,k)
              alfadt=0.5*(dsiglocdt(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocdt(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (temp(i,j,k-1)-temp(i,j,k))
              betads=0.5*(dsiglocds(temp(i,j,k-1),saln(i,j,k-1),prsk)+
     &                    dsiglocds(temp(i,j,k),  saln(i,j,k),  prsk) )*
     &                   (saln(i,j,k-1)-saln(i,j,k))
              thloc(k) = thloc(k-1)-alfadt-betads
              zgrid(k) = -0.5*(p(i,j,k+1) + p(i,j,k))
                 dp(k) =       p(i,j,k+1) - p(i,j,k)
            enddo !k
            zgrid(kk+1) = -p(i,j,kk+1)
c
            mld(i,j) = -zgrid(kk+1)  !bottom
            thjmp(1) = 0.0
            thsur = thloc(1)
            do k=2,kk
              thsur    = min(thloc(k),thsur)  !ignore surface inversion
              thjmp(k) = max(thloc(k)-thsur,
     &                       thjmp(k-1)) !stable profile simplifies the code
*               
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f8.3,f8.4,f9.2)')
     &            i,j,k,
     &            '   th,thsur,jmp,zc =',
     &            thloc(k),thsur,thjmp(k),-zgrid(k)
              endif !debug
c               
              if (thjmp(k).ge.sigmlj) then
c                 
c ---           Between the centers of layers k-1 and k
c
                mld(i,j) =
     &              -zgrid(k-1) +
     &                       0.5*(dp(k-1)+dp(k))*
     &                       (sigmlj        -thjmp(k-1))/
     &                       (thjmp(k)+epsil-thjmp(k-1))
*                 
                if (ldebug_dpmixl .and.
     &              i.eq.itest.and.j.eq.jtest) then
                  write (6,'(2i5,i3,a,f9.2,f9.3,f9.4)')
     &              i,j,k,
     &              '   cell cen: z,dp,q =',
     &               -zgrid(k-1),
     &                0.5*(dp(k-1)+dp(k)),
     &                (sigmlj        -thjmp(k-1))/
     &                (thjmp(k)+epsil-thjmp(k-1))
                  write (6,'(2i5,i3,a,f8.3,f9.2)')
     &              i,j,k,
     &              '       thsur,dpmixl =',
     &              thsur,mld(i,j)
                endif !debug
*                 
                exit  !calculated dpmixl
              endif  !found dpmixl layer
            enddo !k
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_lorb(mld,t,p,flag,ii,jj,kk)
      implicit none
c
      integer ii,jj,kk
      real    mld(ii,jj),t(ii,jj,kk),p(ii,jj,kk+1),flag
c
c**********
c*
c  1) calculate the mixed layer depth based on Lorbacher et.al.
c     can also be used for a potential density mixed layer by
c     providing -4.0*th3d in t.
c
c     note: input is potential temperature but it is used here as 
c           temperature.
c
c  2) input arguments:
c       t     - temperature (or -4.0*density) in layer space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       flag  - data void (land) marker
c       ii    - 1st dimension of t,p
c       jj    - 2nd dimension of t,p
c       kk    - 3rd dimension of t  (number of layers)
c
c  3) output arguments:
c       mld   - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, July 2007.
c*
c**********
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer mldfound
      integer i,j,k,kz
      integer itest,jtest
      real    tz(kk),zm(kk),qe,zmld
c
      itest = ii/2
      jtest = jj/2
c
      do j= 1,jj
        do i= 1,ii
          if     (t(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            kz = kk
            qe = 0.0
            do k= 1,kk
              qe    = max( 0.5*(p(i,j,k)+p(i,j,k+1)), qe+0.01 )
              zm(k) = -qe  !zm(k) < zm(k-1) < 0
              tz(k) = t(i,j,k)
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,f8.3,f9.2)')
     &            i,j,k,
     &            '   tz,zm =',tz(k),zm(k)
              endif !debug
              if     (p(i,j,k+1).gt.p(i,j,kk+1)-0.01) then
                kz = k  !found 1st layer within 1 cm of bottom
                exit
              endif
            enddo !k
            if     (kz.le.2 .or. zm(1)-zm(kz).lt.6.0) then
              mld(i,j) = p(i,j,kk+1)
            else
              call mld_lorb(zm,tz,kz, zmld, qe,mldfound)
              if     (mldfound.eq.1) then
                if     (zmld.eq.zm(1)) then
                  mld(i,j) = -zm(2)  !must be deeper than the "surface"
                else
                  mld(i,j) = -zmld
                endif
              else
                mld(i,j) =  p(i,j,kk+1)
              endif
            endif
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end
      subroutine mixlay_rp33(mld,t,s,p,flag,ii,jj,kk,dxmin,mode)
      implicit none
c
      integer ii,jj,kk,mode
      real    mld(ii,jj),t(ii,jj,kk),s(ii,jj,kk),p(ii,jj,kk+1),
     &        flag,dxmin
c
c**********
c*
c  1) calculate the mixed layer depth based on NAVO's rp33 routine.
c
c     note: input is potential temperature but it is used here as 
c           temperature.
c
c  2) input arguments:
c       t     - temperature in layer space 
c       s     - salinity    in layer space
c       p     - layer interface depths (non-negative m)
c                 p(:,:,   1) is the surface
c                 p(:,:,kk+1) is the bathymetry
c       flag  - data void (land) marker
c       ii    - 1st dimension of t,p
c       jj    - 2nd dimension of t,p
c       kk    - 3rd dimension of t  (number of layers)
c       mode  - =1:mld from temperature; =2:mld from density
c       dxmin - surface to MLD difference (dtmin or dsgmin)
c
c  3) output arguments:
c       mld   - mixed layer depth
c
c  4) except at data voids, on input must have:
c           p(:,:,   1) == zero (surface)
c           p(:,:, l+1) >= p(:,:,l)
c           p(:,:,kk+1) == bathymetry
c
c  5) Alan J. Wallcraft, Naval Research Laboratory, July 2007.
c*
c**********
c
      logical    ldebug_dpmixl
      parameter (ldebug_dpmixl=.true. )
c
      integer mldfound
      integer i,j,k,kz
      integer itest,jtest
      real    tz(kk),sz(kk),rz(kk),zz(kk),zmld
c
      itest = ii/2
      jtest = jj/2
c
      do j= 1,jj
        do i= 1,ii
          if     (t(i,j,1).eq.flag) then
            mld(i,j) = flag  ! land
          else
            kz = kk
            do k= 1,kk
              zz(k) = 0.5*(p(i,j,k)+p(i,j,k+1))
              tz(k) = t(i,j,k)
              sz(k) = s(i,j,k)
              if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
                write (6,'(2i5,i3,a,2f8.3,f9.2)')
     &            i,j,k,
     &            '   tz,sz,zz =',tz(k),sz(k),zz(k)
              endif !debug
              if     (p(i,j,k+1).gt.p(i,j,kk+1)-0.01) then
                kz = k  !found 1st layer within 1 cm of bottom
                exit
              endif
            enddo !k
            if     (kz.le.2) then
              mld(i,j) = p(i,j,kk+1)
            else
              call mld_rp33(tz,sz,rz,kz, zz,zmld,
     &                      dxmin,mode,.true.) !hokeymld=.true.
              if     (zmld.gt.0.0) then
                mld(i,j) = zmld
              else
                mld(i,j) = p(i,j,kk+1)
              endif
            endif
            if (ldebug_dpmixl .and. i.eq.itest.and.j.eq.jtest) then
              write (6,'(2i5,a,f9.2)')
     &            i,j,'            mld =',mld(i,j)
            endif !debug
          endif
        enddo !i
      enddo !j
      return
      end

C*****************************************
       SUBROUTINE MLD_LORB(Z,T,LDIM,MLD,qe,mldfound)
C*****************************************
C   input:
C         z= depth in meter is negative and decreasing with depth.
C            z(1) = level closest to the surface
C         t= temperature in Celsius or Kelvin
C
C         To apply the routine to salinity or potential density profiles
C         we recommend to give the input in 10PSU and 4kg/m^3, respectively,
C         since the ratio of the standard deviation of salinity/potential density
C         to the one of temperature tend to correspond to 0.1/0.25 (in the upper 500m).
C   output:
C          mld = mixed layer depth (negative) (our $h_{mix/c}$)
C           qe = quality index QI_mix; a measure of the stratification
C          imf = logical; imf=1: mld found within the profile
C                         imf=0: mld not found in the profile
C
C    Note!: Some limitations of this routine:
C           1.) z must be decreasing monotonically. No two values of the same depth must
C               exist.
C           2.) If the mld is in the last 30m of the profile, this routine may not work well,
C               since the boundary condition sig30 is not defined for the last 30meter, to
C               avoid false detection of mld, when the true mld is below the end of the
C               profile. If you kown that the mld is in the profile, you my want to
C               delete/alter the following lines in the definition of sig30 in the function
C               'gradients':
C               if (z(i)-z(ldim)<30.0)
C                  sig30(i)=0.0;
C               end
C           3.) The method works with 5meter smoothed gradients, which can lead to shallow
C               baised mld estimates, in some profiles. These profiles are typically very well
C               mixed layers followed by very sharp gradients. In such profiles our estimate
C               may end up a few meters (<5m) above a the 'real' mld. The error is mostly
C               smaller than that of a delta T=0.2K estimate, but it is into the opposite
C               direction (too shallow). For such profiles it is better to not use 5 meter
C               smoothed gradients (as defined in the function 'gradients'). But in general the
C               5meter smoothed gradients will lead to the best estimates, since the boundary
C               conditions are based on them. So do not alter this value unless you know what
C               you do.
C
C   Reference: Lorbacher, K., D. Dommenget and P.P. Niiler (2006): Ocean mixed layer depth:
C              A subsurface proxy for ocean-atmosphere variability.  accepted JGR-Ocean.
C
C   Authors: Katja Lorbacher & Dietmar Dommenget (ddommenget@ifm-geomar.de)


      REAL mld, mldpo
      REAL Z(LDIM),T(LDIM),gt(LDIM),res(ldim),sig30(ldim)

C  gradients; resolution; 30m variance
      call gradients(z,t,gt,res,sig30,ldim)

C  hit bottom if no significant variability is in the profile
      if (maxval(sig30)<.02) then
	 mldfound=0
         mld=minval(z)
	 return
      end if

C  check the range in which the mld can be found
      call first_guess(z,t,abs(gt),sig30,imld0,ldim)

C  find level closest to mld
      call mld_0level(z,t,imld0,gt,res,sig30,imld,ipo,ldim)

C  interpolation between levels
      mld=mldpo(z,t,gt,imld,ipo,ldim)
      mldfound=1

C Q-skill estimate
      qe=qskill(z,t,mld,ldim)

      END SUBROUTINE

C ################   subroutine  gradients   #######################
      subroutine gradients(z,t,gt,res,sig30,ldim)

      REAL Z(LDIM),T(LDIM),gt(LDIM),res(ldim),sig30(ldim)

C    gradients are calculated over at least smoo meter for a smoother estimate
      smoo=5.

      gt(ldim)=0.0
      sig30(ldim)=0.0
      do i=1,ldim-1
         i2=min(ldim,i+1)
	 do while(i2 < ldim .and. z(i)-z(i2) <= smoo)
	    i2=i2+1
	 end do
C	gradients
	 gt(i)=max(-.6,min(.6,(t(i)-t(i2))/(z(i)-z(i2))))
C	resolution
         res(i)=max(0.1,z(i)-z(min(ldim,i+1)))
C	find level 30m below current level for sig30
	 if (z(i)-z(ldim)> 30.0) then
	    i2=i+1
	    do while(i2 < ldim .and. z(i)-z(i2) < 30.)
	       i2=i2+1
	    end do
         end if
         if (z(i)-z(ldim)<=30.0) i2=ldim
C	standard deviation over 30m below current level, sig30
         sig30(i)=std(t(i:i2),i2-i+1)
	 if(z(i)-z(ldim)<30.0) sig30(i)=0.0
      end do
      res(ldim)=res(ldim-1)
      end

C ################    subroutine  first_guess   #######################
      subroutine  first_guess(z,t,gt,sig30,imld,ldim)
C     find first level that excedes a significant gradient & sig30
      REAL z(LDIM),t(LDIM),gt(LDIM),sig30(ldim)

      imld=1
      xmgt=0.25*maxval(gt)
      do while( imld < ldim .and.(gt(imld)<xmgt .or. sig30(imld)<.02 ))
         imld=imld+1
      end do
      imld=max(imld,3)

      end

C ################     subroutine  mld_0level   #######################
      subroutine  mld_0level(z,t,imld0,gt,res,sig30,imld,ipo,ldim)
C   find local extreme in curvature with some  boundary conditions
C   and decide how to interpolate

      REAL z(ldim),t(ldim),gt(ldim),res(ldim),sig30(ldim),ct(ldim)

C  gradient threshold
      xx=std(gt(1:imld0),imld0)
      sgtl=min(0.005,xx)
      sgtl=max(0.002,sgtl)
      sgth=min(0.01,xx)
      sgth=max(0.004,sgth)

C  define curvature
      ct(1)=0.
      do i=2,ldim
         ct(i)=(gt(i)-gt(i-1))/(z(i-1)-z(i))
      end do

C  find local extreme in ct
      i=1
      imld=0
      do while( imld==0 .and. i<imld0-1)
         i=i+1
	 xmct=max(ct(max(1,i-1)),ct(i+1))
	 x2ct=max(-ct(max(1,i-1)),-ct(i+1))
	 if( ( (gt(i)>=0 .and. ct(i)>0. .and. ct(i)>xmct)     ! local maxima
     &     .or.(-gt(i)>0 .and.-ct(i)>0. .and.-ct(i)>x2ct))    ! local maxima
     &   .and. sig30(i)>0.02                                         ! signifcant t-cline
     &   .and.( (res(i)<6.  .and. abs(gt(i))>sgth) .or.              ! signifcant gradient
     &          (res(i)>=6. .and. abs(gt(i))>sgtl))        ) then    ! signifcant gradient
	     imld=i                                                  ! significant local maxima
         end if
      end do

C  no local maxima in gradient change => take first level > 0.7 maxgt
      if (imld==0) then
          imld=1
	  xmax=0.7*maxval(abs(gt(1:imld0)))
          do while( imld < imld0 .and. abs(gt(imld))<xmax)
               imld=imld+1
          end do
      end if

C  check for interpolation
      ipo=0
      i1=max(1,imld-1)
      i2=min(ldim,imld+2)
      resi=maxval(res(i1:i2))
C linear interpolation possible in the interval [z(imld-1) z(imld)]
      if (imld .ne. 1 .and. imld<ldim) then
          ipo=2
	  if ( resi .lt. 6 ) then
C            high resolution always linear interpo
             ipo=2
	  else if ( imld+3 .le. ldim ) then
C            low resolution check if  exp. interpo. possible
	     ipo=1
             if (gt(imld+1)/gt(imld)>=1.0) ipo=2                ! NO not convex
	     if (ipo==2 .and. gt(imld).ne.0  ) then
	        if (abs(gt(imld+2)/gt(imld+1)-0.5)<0.5
     &          .and. gt(imld-1)/gt(imld)<0.1 ) then
		   imld=imld+1                                  ! but convex for imld+1
                   ipo=1
                end if
             end if
	     if (gt(imld).ne.0) then
	        if (abs(gt(imld+1)/gt(imld)-0.875)<0.125
     &             .and. gt(imld-1)/gt(imld)>0.1) ipo=2         ! doubtful gradients
             end if
	     if (gt(imld).ne.0) then
                if (gt(imld+1)/gt(imld)<=0.) ipo=2               ! change of sign
             end if
	  end if
      end if
      if (resi .lt. 6) sgt=sgth
      if (resi .ge. 6) sgt=sgtl
      if ( ipo .eq. 2 .and. gt(imld).ne.0
     &   .and.( abs(gt(imld-1)/gt(imld)-.05) .lt. .05
     &          .or. abs(gt(imld-1)) .lt. sgt )
     &   ) then
        imld=imld+1
	 end if

      end

C ################     FUNCTION  interpol   #######################
      function mldpo(z,t,gt,imld,ipo,ldim)
C interpolation in the interval [z(imld-1) z(imld)]
      REAL mldpo,mld,mgt,mean,sgt
      REAL z(ldim),t(ldim),gt(ldim)
      REAL x1(3),x2(3),x(2),dy(2),xdt(imld-1),tdiff(imld-1)
      mld=z(imld)
      if (ipo==1) then
c        interpolation by exponential fit f(z) = C+A*exp(B*z)
         do i=1,2
            x(i)=(abs(z(imld+i-1))+abs(z(imld+i)))/2.
*****       dy(i)=log(sign(1.,t(imld)-t(imld+1))*gt(imld+i-1))  *****
            sgt=      sign(1.,t(imld)-t(imld+1))*gt(imld+i-1)
            if     (sgt.gt.0.0) then
               dy(i)=log(sgt)  !log(x) must have x>0
            else
               dy(i)=-100.0
            endif
	 end do
	 sum1=sum(x(1:2)*dy(1:2))
	 sum2=sum(x(1:2))*sum(dy(1:2))/2.
	 sum3=sum(x(1:2)**2)
	 sum4=sum(x(1:2))*sum(x(1:2))/2.
	 if (sum1-sum2.ne.0. .and. sum3-sum4.ne.0.)
     &       bx=(sum1-sum2)/(sum3-sum4)
         ax=sum(dy(1:2))/2-bx*sum(x(1:2))/2.
	 b=-bx
	 if (0.<ax .and. ax<50. .and. b .ne. 0.) then
            a=exp(ax)/b
            c=1./(2+1)*(sum(t(imld:imld+2))
     &                -sum(a*exp(-abs(z(imld:imld+2))*b)) )
            x1(1:3)=t(imld:imld+2)
	    x2(1:3)=c+a*exp(-b*abs(z(imld:imld+2)))
            r=corrcoef(x1,x2,3)
            tref=mean(t(1:max(1,imld-1)),max(1,imld-1))
            tx=mean(x2,3)+std(x2,3)/std(x1,3)*r*(tref-mean(x1,3))
            if((tx-c)/a>0. .and. b .ne. 0.) mld=-abs(log((tx-c)/a)/(-b))

C     empirical correction based on observational errors
C     it shifts the mld further up from the level-imld
C     it reflects the fact that the exponential profile below the mld does
C     break down near the mld, it continues with a smaller gradient
C     (a smooth transition zone)
            dlx=z(imld-1)-z(imld)
	    dx=mld-z(imld)
            if (dx<=0.4*dlx) mld=z(imld)+2.5*dx-0.1*dlx

C     interpo overshoot -> empirical correct. based on obs. err.
C     (better than setting mld onto a level)
	    if (mld>=z(imld-1)) then
	       if(gt(imld-1)<0.01*(z(imld-1)-z(imld))) then
                  dd=0.5*(1-min(1.,gt(imld+2)/gt(imld)))-0.25
               else
	          dd=10*gt(imld-1)
	       end if
	       if(gt(imld+2)/gt(imld)>1.0) then
	          mld=z(imld-1)-0.25*(z(imld-1)-z(imld))
	       else
	          mld=z(imld-1)+dd*(z(imld-1)-z(imld))
	       end if
            end if
	 end if
      end if

      if (ipo==2) then
C     linear  interpolation
         if ( gt(imld)/gt(imld-1) >  1.0 ) then
            dx=(t(imld-1)-t(imld))/((gt(imld)+gt(imld-1))/2.)
         else
C        linear interpo would overshoot: find a dT to go down from imld-1
            call diff(tdiff,t(1:imld-1),imld-1)

C	    print*,t(1:imld-1),abs(tdiff(1:imld-1)),imld-1

	    dt=maxval( abs(tdiff) )
            dt=max(0.01,dt)
            dx=sign(1.,t(imld)-t(imld-1))
     &	        *dt/gt(imld-1)-(z(imld)-z(imld-1))
C	 print*,'hallo 2',dx,sign(1.,t(imld)-t(imld-1))
c     &	        ,dt,gt(imld-1),(z(imld)-z(imld-1))
        end if
C       empirical correction based on obs. err.
         if (      0.25*(z(imld-1)-z(imld))<=dx
     &       .and. dx<=0.75*(z(imld-1)-z(imld))) then
            dx=dx+(z(imld-1)-z(imld))/5.
         end if
	 mld=z(imld)+dx;
      end if

C     interpolation must be within [z(imld-1), z(imld+1)]
      if (mld > z(max(1,imld-1))) mld=z(max(1,imld-1))
      if (mld < z(min(ldim,imld+1))) mld=z(min(ldim,imld+1))
      mldpo=mld
      end

C ################     FUNCTION  standard deviation   #######################
      function std(x,ldim)
      real x(ldim)
      xmean=sum(x)/ldim
      std=sqrt(sum((x-xmean)*(x-xmean))/(ldim-1))
      end
C ################     FUNCTION mean   #######################
      function mean(x,ldim)
      real x(ldim),mean
      mean=sum(x)/ldim
      end
C ################     subroutine  diff   #######################
      subroutine diff(dx,x,ldim)
      real x(ldim),dx(ldim)
      dx=0.
      do i=1,ldim-1
         dx(i)=x(i+1)-x(i)
      end do
      if (ldim > 1) dx(ldim)=x(ldim)-x(ldim-1)
      end
C ################     function CORRCOEF   #######################
      function corrcoef(X,Y,IDIM)
      DIMENSION X(IDIM),Y(IDIM)
      XMN=SUM(X(1:IDIM))/IDIM
      YMN=SUM(X(1:IDIM))/IDIM
      XY=SUM((X(1:IDIM)-XMN)*(Y(1:IDIM)-YMN))
      X2=SUM((X(1:IDIM)-XMN)**2)
      Y2=SUM((Y(1:IDIM)-YMN)**2)
      IF(X2*Y2.GT.0.0) COR=XY/SQRT(X2*Y2)
      IF(X2*Y2.LE.0.0) COR=1.0
      corrcoef=cor
      end
C ################     subroutine CALC_SST   #######################
      SUBROUTINE CALC_SST(LEVELS,TEMP,LDIM,XMLD,SST)
      REAL LEVELS(LDIM),TEMP(LDIM)
      SST=TEMP(1)
      I=2
      DO WHILE( LEVELS(I).GE.-10 .AND. XMLD.LT.LEVELS(I)
     &     .AND. I.LE.LDIM )
         SST=SST+TEMP(I)
         I=I+1
      ENDDO
      SST=SST/(I-1)
      END
C ################     function  qskill  #######################
      FUNCTION qskill(z,t,mld,ldim)

      REAL qskill,mld
      REAL z(ldim),t(ldim)

      RANGE=1.5
C FIND RANGE FOR QE
      I=1
      DO WHILE ( I.LE.ldim .AND. -mld .GE. -z(I) )
         IMLD=I
         I=I+1
      ENDDO
      IMLD=MAX(2,IMLD)
      IF( IMLD.GE.LDIM ) THEN
C         PRINT*,'ERROR HIT BOTTOM => ERR=1.0'
         qskill=1.0
         RETURN
      ENDIF
      IF (-RANGE*MLD .GT. -z(LDIM) ) THEN
         IEND=LDIM
      ELSE
         DO WHILE ( -RANGE*MLD .GT. -z(I) )
            IEND=I
            I=I+1
         ENDDO
      ENDIF
      IEND=MAX(IEND,IMLD+1)
      IEND=MIN(IEND,LDIM)

      v1=std(t(1:imld),imld)
      v2=std(t(1:iend),iend)
      IF(v2.GT.0.0) qskill=1.-v1/v2
      IF(qskill.LT.0.) qskill=0.

      END


      subroutine mld_rp33 (temp, salt, sigmat, nz, zstd, pld,
     1                     dxmin,mldmode,hokeymld)
c------------------------------------------------------------------------------
c  Name:  mixlyr (mld_rp33)
c  Purpose:
c     this routine finds the mixed layer depth in the input
c     temperature and salinity profiles using either:
c        (1) positive density difference of dsgmin kg/m**3 between the
c            surface and depth if temperature and salinity profiles
c            are available
c        (2) negative temperature difference of dtmin degC between the
c            surface and depth if only a temperature profile is
c            available
c     note that the mixed layer is interpolated linearly between
c     depths where the above differences were found.
c  Inputs:  salt, temp, zstd
c  Outputs: pld
c  Calling routines:  SYNSTD/dosynth
c  Routines called:   svan
c  Glossary:
c  input parameters:
c     salt* salinity profile at standard depths
c     temp* temperature profile at standard depths
c     zstd* standard depth array
c       mldmode = 1  compute mixed-layer depth from temperature
c               = 2  compute mixed-layer depth from density if salinity
c                    is available.
c  output parameters:
c     pld mixed layer depth (m)
c  parameter:
c     nz  number of standard depths array dimension
c     zdum  missing value indicator
c  Method:
c
c------------------------------------------------------------------------------
c
      real       salt(*), sigmat(nz), temp(*), zstd(*)
c
      logical finished, hokeymld
c
c     set mixed layer definitions (temperature and density)
c
******data dtmin /0.25/, dsgmin /0.125/  !now from dxmin
      data spval,spchk /-1.e34,-1.e33/
c
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c  check for valid salinity profile
c
      mlmode=mldmode
      if(salt(1).lt.spchk) mlmode=1
      pld = spval
      if (mlmode.eq.2) then
        dsgmin = dxmin
c
c  calculate density profile relative to
c  the surface
c
         do L = 1, nz
           sigmat(L) = spval
           if(salt(L).gt.0. .and. temp(L).gt.-2.) then
             anom =  svan (salt(L), temp(L), zstd(1), sigmat(L))
           else
             sigmat(L)=-1.e34
           endif
         enddo
c
c  find MLD from density profile
c
         pld=-1.e34
         do L = 2, nz
           if(sigmat(1).gt.-1.e33.and.sigmat(L).gt.-1.e33) then
c
c  check magnitude of change
c  calculate mixed layer depth
c
             if (sigmat(L) - sigmat(1).gt.dsgmin) then
               w=abs(((sigmat(1) + dsgmin) - sigmat(L)) /
     1           (sigmat(L) - sigmat(L - 1)))
               pld = zstd(L - 1)*w + zstd(L)*(1. - w)
               go to 30
             endif
           endif
         enddo
c
c  mixed layer missing
c
 30          continue
c
      else
        if (temp(1).gt.spchk) then
          dtmin = dxmin
c
c  find MLD from temperature profile
c
          pld = -1.e34
          tmin=temp(1)
          kmin=1
          tmax=temp(1)
          kmax=1
          finished=.false.
          do L = 2, nz
            if (temp(1).gt.-2. .and. temp(L).gt.-2.) then
c
c optional and somewhat hokey:
c  if the temperature is increasing (more than 0.15 Deg C greater than
c  the minumum temperature above it), then set mixed layer depth
c  to the depth where the minumum temperature occured above this point.
c  However, if the minimum temperature occured at the surface, then
c  set the mld to the depth where temperature is 0.15 degrees higher
c  than at the surface.
c
               if(hokeymld) then
                 if(tmin-temp(L).le.-0.15) then
                   if(temp(L).lt.temp(L-1)) then
                     pld=zstd(L-1)
                     goto 50
                   endif
                 endif
               endif
c
c  check magnitude of change
c  calculate mixed layer depth
c
               if (temp(1) - temp(L).gt.dtmin) then
                  w = abs(((temp(1) - dtmin) - temp(L)) /
     1                (temp(L - 1) - temp(L)))
                  pld = zstd(L - 1)*w + zstd(L)*(1. - w)
                  go to 50
               endif
            endif
            if(temp(L).le.tmin) then
              kmin=L
              tmin=temp(L)
            endif
            if(temp(L).ge.tmax) then
              kmax=L
              tmax=temp(L)
            endif
            if(L.eq.nz .and. pld.lt.0) then
c    half channel condition
               pld=zstd(L)
            endif
          enddo
c
c  mixed layer missing
c  missing mixed layer depth value is set to -1
c  cmrc20061002
        else
          pld=-1.0
        endif
 50        continue
c
      endif
      return
      end

c------------------------------------------------------------------------------
      real function svan (s, t, p0, sigma)
c------------------------------------------------------------------------------
c  Name:	svan
c  Purpose:
c     computes specific volume anomaly (steric anomaly) based on 1980
c     equation of state for seawater and the 1978 practical salinity
c     scale.
c  Inputs:	p0, s, t
c  Outputs:	sigma, svan
c  Calling routines:  DYNHT/htrel, DYNHT/htrelbot
c  Routines called:   sqrt
c
c  Glossary:	
c  input parameters:
c      p0 - pressure in decibars
c       t - temperature (celsius)
c       s - salinity (pss-78)
c  output parameters:
c   sigma - density anomaly (kg/m**3)
c    svan - specific volume anomaly (1.0e-8 m**3/kg)
c
c  Method:	
c     references:
c        Millero et al. (1980), Deep Sea Res., 27a, 255-264
c        Millero and Poisson 1981, Deep-Sea Res., 28a, 625-629.
c
c     units:
c        pressure         p0        decibars
c        temperature      t         dec celsius (ipts-68)
c        salinity         s         (pss-78)
c        spec. vol. ano.  svan      1.0e-8 m**3/kg
c        density ano.     sigma     kg/m**3
c
c     check values: svan=981.30210e-8 m**3/kg for
c                   s=40 (pss-78), t=40 deg c, p0=10000 decibars
c                   sigma= 59.82037 kg/m**3 for
c                   s=40 (pss-78), t=40 deg c, p0=10000 decibars
c
c
c------------------------------------------------------------------------------
c
      real p, t, s, sig, sr, r1, r2, r3, r4
      real a, b, c, d, e, a1, b1, aw, bw, k0, kw, k35
c
      equivalence (e,d,b1), (bw,b,r3), (c,a1,r2)
      equivalence (aw,a,r1), (kw,k0)
c
      data r3500, r4 /1028.1063, 4.8314e-4/
      data dr350 /28.106331/
c
c     r4 is referred to as c in Millero and Poisson 1981
c     convert pressure to bars and take square root of salinity
c
      p = p0 / 10.
      sr = sqrt (abs (s))
c
c     pure water density at atmospheric pressure
c     Bigg P.H., (1967) Br. J. Applied Physics, 8, 521-537
c
      r1 = ((((6.536332e-9 * t - 1.120083e-6) * t + 1.001685e-4) * t
     *     -9.095290e-3) * t + 6.793952e-2) * t - 28.263737
c
c     sea water density at atm. pressure
c     coefficient involving density
c     r2 = 'a' in notation of Millero and Poisson 1981
c
      r2 = (((5.3875e-9 * t-8.2467e-7) * t+7.6438e-5) * t-4.0899e-3) * t
     *     + 8.24493e-1
c
c     r3 = 'b' in notation of Millero and Poisson 1981
c
      r3 = (-1.6546e-6 * t + 1.0227e-4) * t - 5.72466e-3
c
c     international one-atmosphere equation of state of seawater
c
      sig = (r4 * s + r3 * sr + r2) * s + r1
c
c     specific volume at atmospheric pressure
c
      v350p = 1. / r3500
      sva = -sig * v350p / (r3500 + sig)
      sigma = sig + dr350
c
c     scale specific vol. anomaly to normally reported units
c
      svan = sva * 1.0e+8
      if (p .ne. 0.) then
c
c     new high pressure equation of state for seawater
c     Millero et al, 1980, DSR 27a, 255-264.
c     constant notation follows article
c
c     compute compression terms
c
        e = (9.1697e-10 * t + 2.0816e-8) * t - 9.9348e-7
        bw = (5.2787e-8 * t - 6.12293e-6) * t + 3.47718e-5
        b = bw + e * s
c
        d = 1.91075e-4
        c = (-1.6078e-6 * t - 1.0981e-5) * t + 2.2838e-3
        aw = ((-5.77905e-7 * t + 1.16092e-4) * t + 1.43713e-3) * t
     1       - 0.1194975
        a = (d * sr + c) * s + aw
c
        b1 = (-5.3009e-4 * t + 1.6483e-2) * t + 7.944e-2
        a1 = ((-6.1670e-5 * t + 1.09987e-2) * t-0.603459) * t + 54.6746
        kw = (((-5.155288e-5 * t + 1.360477e-2) * t-2.327105) * t
     1       + 148.4206) * t - 1930.06
        k0 = (b1 * sr + a1) * s + kw
c
c     evaluate pressure polynomial
c
c     k equals the secant bulk modulus of seawater
c     dk = k(s,t,p) - k(35,0,p)
c     k35 = k(35,0,p)
c
        dk = (b * p + a) * p + k0
        k35 = (5.03217e-5 * p + 3.359406) * p + 21582.27
        gam = p / k35
        pk = 1.0 - gam
        sva = sva * pk + (v350p + sva) * p * dk / (k35 * (k35 + dk))
c
c     scale specific volume anomaly to normally reported units
c
        svan = sva * 1.0e+8
        v350p = v350p * pk
c
c     compute density anomaly with respect to 1000.0 kg/m**3
c     1) dr350. density anomaly at 35 (pss-78), 0 deg. c and 0 decibars
c     2) dr35p: dentity anomaly 35 (pss-78), 0 deg. c, pres. variation
c     3) dvan : density anomaly variations involving specific vol. anom
c
c     check value: sigma = 59.82037 kg/m**3 for s = 40 (pss-78),
c     t = 40 deg c, p0 = 10000 decibars.
c
        dr35p = gam / v350p
        dvan = sva / (v350p * (v350p + sva))
        sigma = dr350 + dr35p - dvan
      endif
c
      return
      end
