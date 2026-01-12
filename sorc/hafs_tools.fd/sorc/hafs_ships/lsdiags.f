      subroutine lsdiags(mx,my,mp,nx,ny,np,inest,u,v,t,z,rh,
     +                   us,vs,ts,ps,rhs,sst,ohc,tpw,
     +                   rlon,rlat,plev,slon,slat,irepos,rmiss,
     +                   usnd,vsnd,tsnd,zsnd,rhsnd,
     +                   usnds,vsnds,tsnds,psnds,rhsnds,
     +                   ms,ns,diagvar,diaglab,ierr)
c
c     This set of subroutines is for calculating large scale and vortex scale
c     diagnostics for tropical cyclone analysis. This code was adapted from the 
c     SHIPS model diagnostic code, and is designed for diagnostics at a single
c     time period. Model input is assumed to be on constant pressure levels with
c     on an evenly spaced lat/lon grid. Missing values are assumed to be .le. rmiss. 
c
c     Last Modified: 02/11/2011, version 1.1
c 
c     Input: 
c            mx,my,mp:   Maximum lon,lat,P array dimensions
c            nx,ny,np:   Usable lat,lon,P array dimensions
c            inest:      Flag for smaller or nested grids, uses smaller areas for averaging
c            u,v,t,z,rh: 3-D (lon,lat,P) arrays of wind components (m/s), 
c                        temperature (deg C), geopotential height (m) and 
c                        relative humidity (%)
c            us,vs:      2-D (lon,lat) surface or lowest model level horizontal winds (m/s)
c            ts:         2-D surface or lowest model level T (deg C)
c            ps:         2-D Sea-level pressure (hPa)     
c            rhs:        2-D surface or lowest model level relative humidity (%)
c            sst:        2-D sea surface temperature (deg C)
c            ohc:        2-D oceanic heat content (kJ/cm2)
c            tpw:        2-D total precipitable water (mm)
c            rlon:       1-D array of model grid longitudes (deg E, 0 to 360 convention)
c            rlat:       1-D array of model grid latitudes  (deg N, -90 to 90 convention)
c            plev:       1-D Pressure levels of model grid (hPa)
c            slat:       Storm latitude  (deg N, -90 to 90 convention)
c            slon:       Storm longitude (deg E, 0 to 360 convention)
c            irepos:     Flag for repositioning storm based on local center finding routine
c                        =1 to reposition, =0 for no repositioning
c            rmiss:      Value for missing values in model field arrays 
c                        (if field value .le. rmiss, it is considerd missing, 
c                        -999.9 is a good choice)
c            ms:         Max dimension of diagvar and diaglab arrays
c
c     Output: 
c            usnd,vnsd:       Sounding of area averaged u,v
c            tsnd,zsnd,rhsnd: Sounding of area average t,z,rh (note: area for TD variables can
c                                                                   be different than wind)
c            usnds,vnsds:        Area averaged surface u,v
c            tsnds,psnds,rhsnds: Area averaged surface t,p,rh (note: area for TD variables can
c                                                                   be different than wind)
c            tpwa:       Area averaged tpw
c            ns:         Number of variables in diagvar array
c            diagvar:    1-D array containing diagnostic variables
c            diaglab:    Labels (a50) for diagnostic variables
c            ierr:       Error flag (=0 for normal return)
c 
      use diag_util
c     **Passed variables
c
c       ++ Basic model fields and coordinates
      dimension u(mx,my,mp),v(mx,my,mp),z(mx,my,mp),
     +          t(mx,my,mp),rh(mx,my,mp)
      dimension us(mx,my),vs(mx,my),ts(mx,my),ps(mx,my),rhs(mx,my)
      dimension sst(mx,my),ohc(mx,my),tpw(mx,my)
      dimension rlon(mx),rlat(my),plev(mp)
c
c      ++ Sounding variables
      dimension usnd(mp),vsnd(mp),tsnd(mp),zsnd(mp),rhsnd(mp)
c
c     ++ Diagnostic variables and labels
      dimension diagvar(ms)
      character *50 diaglab(ms)
c
c     **Local variables
c
c       ++Cylindrical grid variables (Note: mpl must be .ge. mp)
      parameter (mr=150,mt=16,mpl=100)
      dimension r(0:mr),theta(0:mt)
      dimension xrt(0:mr,0:mt),yrt(0:mr,0:mt)
      dimension sinth(0:mt),costh(0:mt)
c
c       ++Arrays for interpolating fields to the cylindrical grid
      dimension uc(0:mr,0:mt,mpl),vc(0:mr,0:mt,mpl),tc(0:mr,0:mt,mpl)
      dimension zc(0:mr,0:mt,mpl),rhc(0:mr,0:mt,mpl)
      dimension urc(0:mr,0:mt,mpl),vtc(0:mr,0:mt,mpl)
      dimension urca(0:mr,mpl),vtca(0:mr,mpl)
c
      dimension usc(0:mr,0:mt),vsc(0:mr,0:mt),tsc(0:mr,0:mt)
      dimension psc(0:mr,0:mt),rhsc(0:mr,0:mt)
      dimension sstc(0:mr,0:mt),ohcc(0:mr,0:mt),tpwc(0:mr,0:mt)
c
      dimension vmax(0:mt),rmw(0:mt)
c
c     Set defaults
      ierr = 0
c
c     ** Begin specification of diagnostic calculation parameters
c 
      if (inest .eq. 0) then
c     Parent grid or global grid specified, use default radii
c
c        ++ Inner and outer radii (km) for area average of model sounding (thermo variables)
         r1st = 200.0
         r2st = 800.0
c
c        ++ Inner and outer radii (km) for area average of model sounding (wind variables)
         r1sw =   0.0
         r2sw = 500.0
c
c        ++ Maximum search radius (km) for vortex parameters,
c          min sfc pressure, max winds and radius of maximum wind.
c          (suggest 500 km for global models, 200 km for regional models)
         srmax = 500.0
c
c        ++ Maximum radius for SST (km) and OHC (km) area averages
         ssmax = 50.0
         somax = 50.0
c
c        ++ Pressure levels for vertical shear calculation
         psbot = 850.0
         pstop = 200.0
c
c        ++ Pressure and radius (km) for area averaged vorticity
         pva = 850.0
         rva = 1000.0
c
c        ++ Pressure and radius (km) for area averaged divergence
         pda = 200.0
         rda = 1000.0
c
c        ++ Pressure and radius (km) for averaging tangential wind
         pta = 850.0
         rta = 600.0
c
c        ++ Cylindrical grid parameters (r0, dr in km, dtheta (dt) in deg, +x axis is east)
c           nr,nt = number of radial and azimuthal grid intervals. 
c           Note: dr should be .le. model grid spacing, radial domain should extend to at
c                 least 1000 km. 
         nr=150
         nt=8
         r0=0.0
         dr=10.0
         dt=360.0/float(nt)
c 
      else
c     Use smaller radii for nested grids, represent storm instead of large-scale environment
c
c        ++ Inner and outer radii (km) for area average of model sounding (thermo variables)
         r1st = 0.0
         r2st = 200.0
c
c        ++ Inner and outer radii (km) for area average of model sounding (wind variables)
         r1sw =   0.0
         r2sw = 200.0
c
c        ++ Maximum search radius (km) for vortex parameters,
c           min sfc pressure, max winds and radius of maximum wind.
c           (suggest 500 km for global models, 200 km for regional models)
         srmax = 200.0
c
c        ++ Maximum radius for SST (km) and OHC (km) area averages
         ssmax = 50.0
         somax = 50.0
c
c        ++ Pressure levels for vertical shear calculation
         psbot = 850.0
         pstop = 200.0
c
c        ++ Pressure and radius (km) for area averaged vorticity
         pva = 850.0
         rva = 200.0
c
c        ++ Pressure and radius (km) for area averaged divergence
         pda = 200.0
         rda = 200.0
c
c        ++ Pressure and radius (km) for averaging tangential wind
         pta = 850.0
         rta = 200.0
c
c        ++ Cylindrical grid parameters (r0, dr in km, dtheta (dt) in deg, +x axis is east)
c           nr,nt = number of radial and azimuthal grid intervals. 
c           Note: dr should be .le. model grid spacing, radial domain should extend to at
c                 least 1000 km. 
         nr=150
         nt=8
         r0=0.0
         dr=2.0
         dt=360.0/float(nt)
c 
      endif
c 
c     ** End specification of diagnostic calculation parameters
c
c     Specify physical and numerical constants
      pi = 3.14159265
      dtr = pi/180.0
      cctk = 273.15
      cmtk = 1.944
c
c     Check local array dimensions
      if (mp .gt. mpl) then
         ierr=1
         return
      endif
c
c     Calculate cylindrical grid info (r in km, theta in deg, +x axis is east)
      call cgcal(mr,mt,nr,nt,dr,dt,r0,dtr,r,theta,xrt,yrt,sinth,costh)
c
c     Calculate parameters to convert between lat/lon and storm-centered
c     cylindrical grid. These must be recalculated if storm center is moved.
      call gridcon(mx,my,nx,ny,rlon,rlat,slon,slat,dtr,
     +             dlon,dlat,dx,dy,glon1,glat1,xg1,yg1)
c
c     Interpolate 3D model fields to cylindrical grid
      call lltocg3(u,uc,mx,my,mp,mpl,nx,ny,np,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg3(v,vc,mx,my,mp,mpl,nx,ny,np,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg3(t,tc,mx,my,mp,mpl,nx,ny,np,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg3(z,zc,mx,my,mp,mpl,nx,ny,np,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg3(rh,rhc,mx,my,mp,mpl,nx,ny,np,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
c
c     Interpolate 2D model fields to cylindrical grid
      call lltocg2(us,usc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(vs,vsc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(ts,tsc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(ps,psc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(rhs,rhsc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(sst,sstc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(ohc,ohcc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
      call lltocg2(tpw,tpwc,mx,my,nx,ny,mr,mt,nr,nt,
     +             dx,dy,xg1,yg1,xrt,yrt,rmiss)
c
c     Calculate radial and tangential winds
      call uvtort(uc,vc,sinth,costh,mr,mt,mpl,nr,nt,np,urc,vtc,rmiss)
c
c     Azimuthally average radial and tangential winds
      call azavg3(urc,urca,mr,mt,mpl,nr,nt,np,rmiss)
      call azavg3(vtc,vtca,mr,mt,mpl,nr,nt,np,rmiss)
c
c     ** Begin vortex scale and ocean diagnostic calculations
c       ++ Find max wind, min sea level pressure, RMW
      call vorparm(usc,vsc,psc,r,srmax,mr,mt,nr,nt,rmiss,
     +             vmax,rmw,vmaxa,rmwa,pmin)
c
c       ++ Initialize SST, OHC, TPW to missing
      ssta = rmiss
      ohca = rmiss
      tpwa = rmiss
c 
c     ** End vortex scale and ocean diagnostic calculations
c
c     ** Begin large scale diagnostic calculations
c
c       ++ Area average sounding
      do k=1,np
         call caavg(uc(1,1,k),r1sw,r2sw,mr,mt,nr,nt,
     +              rmiss,r,theta,usnd(k))
         call caavg(vc(1,1,k),r1sw,r2sw,mr,mt,nr,nt,
     +              rmiss,r,theta,vsnd(k))
         call caavg(tc(1,1,k),r1st,r2st,mr,mt,nr,nt,
     +              rmiss,r,theta,tsnd(k))
         call caavg(zc(1,1,k),r1st,r2st,mr,mt,nr,nt,
     +              rmiss,r,theta,zsnd(k))
         call caavg(rhc(1,1,k),r1st,r2st,mr,mt,nr,nt,
     +              rmiss,r,theta,rhsnd(k))
      enddo
c 
c       ++ Surface area averages
      call caavg(usc,r1sw,r2sw,mr,mt,nr,nt,rmiss,r,theta,usnds)
      call caavg(vsc,r1sw,r2sw,mr,mt,nr,nt,rmiss,r,theta,vsnds)
      call caavg(tsc,r1st,r2st,mr,mt,nr,nt,rmiss,r,theta,tsnds)
      call caavg(psc,r1st,r2st,mr,mt,nr,nt,rmiss,r,theta,psnds)
      call caavg(rhsc,r1st,r2st,mr,mt,nr,nt,rmiss,r,theta,rhsnds)
c 
c      ++ TPW, SST and OHC area averages
      call caavg(tpwc,0.0,srmax,mr,mt,nr,nt,rmiss,r,theta,tpwa)
      call caavg(sstc,0.0,ssmax,mr,mt,nr,nt,rmiss,r,theta,ssta)
      call caavg(ohcc,0.0,somax,mr,mt,nr,nt,rmiss,r,theta,ohca)
c
c       ++ Vertical shear and direction (heading)
      call shrcal(mp,np,plev,psbot,pstop,usnd,vsnd,rmiss,
     +                               shrmag,shrhead,ierr)
      if (ierr .ne. 0) return
c
c       ++ Area average vorticity and divergence
      call aavd(urca,vtca,pva,rva,rmiss,r,plev,vavgpv,davgpv,
     +                               mr,mp,mpl,nr,np,ierr)
      if (ierr .ne. 0) return
      call aavd(urca,vtca,pda,rda,rmiss,r,plev,vavgpd,davgpd,
     +                               mr,mp,mpl,nr,np,ierr)
      if (ierr .ne. 0) return
c 
c       ++ Radially averaged tangential wind
      call ravt(urca,vtca,pta,rta,r,plev,vtbar,urbar,
     +          mr,mp,mpl,nr,np,ierr,rmiss)
c
c       ++ Distance to nearest major landmass
      call gbland(slon,slat,dtl)
c
c     ** End large-scale diagnostic calculations
c
c     ** Begin unit conversions
c
c       ++Convert max sfc wind from m/s to kt
      if (vmaxa .gt. rmiss) then
         vmaxa = cmtk*vmaxa
      endif
c       
c       ++ Scale SST by 10, convert K to C
      if (ssta .gt. rmiss) then
         ssta = (ssta-273.15)*10.0
      endif
c
c       ++Convert u,v soundings and surface from m/s to kt
      do k=1,np
         if (usnd(k) .gt. rmiss) then
            usnd(k) = cmtk*usnd(k)
         endif
         if (vsnd(k) .gt. rmiss) then
            vsnd(k) = cmtk*vsnd(k)
         endif
      enddo
      if (usnds .gt. rmiss) then
         usnds = cmtk*usnds
      endif
      if (vsnds .gt. rmiss) then
         vsnds = cmtk*vsnds
      endif
c
c       ++Convert shr magnitude from m/s to kt
      if (shrmag .gt. rmiss) then
        shrmag = cmtk*shrmag
      endif
c
c       ++Scale average vorticity and divergence by 10**7
      if (vavgpv .gt. rmiss) then
         vavgpv = vavgpv*(10.0**7)
      endif
      if (davgpv .gt. rmiss) then
         davgpv = davgpv*(10.0**7)
      endif
      if (vavgpd .gt. rmiss) then
         vavgpd = vavgpd*(10.0**7)
      endif
      if (davgpd .gt. rmiss) then
         davgpd = davgpd*(10.0**7)
      endif
c
c       ++Scale radially averaged tangential wind by 10
      if (vtbar .gt. rmiss) then
         vtbar = 10.0*vtbar
      endif
      if (urbar .gt. rmiss) then
         urbar = 10.0*urbar
      endif
c
c     ** End unit conversions
c
      write(6,600) slon,slat,dtl
  600 format(/,' slon,slat,dtl:  ',f7.2,1x,f7.2,1x,f8.1)
      write(6,601) vmaxa,rmwa,pmin
  601 format(  ' vmax,rmw,pmin:  ',f7.2,1x,f7.2,1x,f8.1)
      write(6,602) ssta,ohca
  602 format(  ' sst,ohc:        ',f7.2,1x,f7.2)
      write(6,605) shrmag,shrhead
  605 format(  ' shrmag,shrhead: ',f7.2,1x,f7.2)
      write(6,606) vavgpv,davgpd
  606 format(  ' v850,d200:      ',f7.2,1x,f7.2)
      write(6,607) vtbar,urbar
  607 format(  ' vtbar,urbar:    ',f7.2,1x,f7.2)
c
c     Choose diagnostic variables to send back to calling routine
c***  editted here to include 16 instead of 14 (str speed, heading)
      ns = 16
c
      diagvar( 1) = slat
      diagvar( 2) = slon
      diagvar( 3) = vmaxa
      diagvar( 4) = rmwa
      diagvar( 5) = pmin
      diagvar( 6) = shrmag
      diagvar( 7) = shrhead
      diagvar( 8) = rmiss
      diagvar( 9) = rmiss
      diagvar(10) = ssta
      diagvar(11) = ohca
      diagvar(12) = tpwa
      diagvar(13) = dtl
      diagvar(14) = vtbar
      diagvar(15) = vavgpv
      diagvar(16) = davgpd
c
      diaglab( 1) = 'LAT (Deg N)'
      diaglab( 2) = 'LON (Deg E, 0 to 360)'
      diaglab( 3) = 'MAXWIND (kt)'
      diaglab( 4) = 'RMW (km)'
      diaglab( 5) = 'MIN_SLP (hPa)'
      diaglab( 6) = 'SHR_MAG (kt)'
      diaglab( 7) = 'SHR_HDG (Heading in deg)'
      diaglab( 8) = 'STM_SPD (kt)'
      diaglab( 9) = 'STM_DIR (Heading in deg)'
      diaglab(10) = 'SST (Deg C *10'
      diaglab(11) = 'OHC (kJ/cm**2)'
      diaglab(12) = 'TPW (mm)'
      diaglab(13) = 'LAND (km)'
      diaglab(14) = '850TANG (m/s *10)'
      diaglab(15) = '850VORT (/s *10**7)'
      diaglab(16) = '200DVRG (/s *10**7)'
c 
      return
c
      end
      subroutine lintcf(fxy,x1,dx,y1,dy,mx,my,nx,ny,xi,yi,rmiss,fxyii)
c     This routine bi-linearly interpolates fxy to the point             
c     (xi,yi) to give fxyii. Points with missing values (fxy .le. rmiss) 
c     are excluded from the interpolation. If all points are missing, 
c     fxyii is set to rmiss.                                            
c                                                                        
c     Input:   fxy(mx,my) - array to be interpolated                     
c              dx,dy      - spacing of evenly spaced x,y points          
c              x1,y1      - x,y coorindates of lower-left point          
c              mx,my      - max dimensions of fxy                        
c              nx,ny      - working dimensions of fxy                    
c              xi,yi      - coordinates of point to be interpolated      
c                                                                        
c     Output:  fxyii      - value of interpolated function at (xi,yi)    
c                                                                        
      dimension fxy(mx,my)                                              
c                                                                       
      data itemp /0/
c
c     Find the indices of the lower-left point of the                
c     grid box containing the point (xi,yi)                              
      i0 = 1 + int( (xi-x1)/dx )                                       
      j0 = 1 + int( (yi-y1)/dy )                                       
c                                                                       
      fxyii = rmiss
c 
c     Check index bounds                                                 
c      if (i0 .lt.    1) i0=   1                                          
c      if (i0 .gt. nx-1) i0=nx-1                                          
c      if (j0 .lt.    1) j0=   1                                          
c      if (j0 .gt. ny-1) j0=ny-1                                          
      if (i0 .lt.    1) return                                          
      if (i0 .gt. nx-1) return                                          
      if (j0 .lt.    1) return                                          
      if (j0 .gt. ny-1) return  
c                                                                        
      i1 = i0+1                                                         
      j1 = j0+1                                                         
c                                                                        
c     Calculate normalized x,y distances                                 
      xn = ( (xi-x1) - dx*float(i0-1) )/dx                             
      yn = ( (yi-y1) - dy*float(j0-1) )/dy                              
c                                                                       
      if (xn .lt. 0.0) xn = 0.0                                         
      if (xn .gt. 1.0) xn = 1.0                                         
      if (yn .lt. 0.0) yn = 0.0                                         
      if (yn .gt. 1.0) yn = 1.0                                         
c                                                                        
c     Calculate coefficients for interpolation function                  
      f00 = fxy(i0,j0)                                                  
      f10 = fxy(i1,j0)                                                 
      f01 = fxy(i0,j1)                                                  
      f11 = fxy(i1,j1)                                                  
c                                                                       
      w00 = 1.0 + xn*yn - xn - yn
      w10 = xn*(1.0-yn)
      w01 = yn*(1.0-xn)
      w11 = xn*yn
c
      if (f00 .le. rmiss) w00 = 0.0
      if (f01 .le. rmiss) w01 = 0.0
      if (f10 .le. rmiss) w10 = 0.0
      if (f11 .le. rmiss) w11 = 0.0
c
      wtsum = w00 + w01 + w10 + w11
      if (wtsum .le. 0.0) then
         fxyii = rmiss
      else
         fxyii = (w00*f00 + w10*f10 + w01*f01 + w11*f11)/wtsum
      endif
c
      return                                                           
      end   
      subroutine caavg(frt,r1,r2,mr,mt,nr,nt,rmiss,r,theta,fbar)
c     This routine calculates the area average of f(r,theta)
c     from r=r1 to r=r2. 
c
      dimension frt(0:mr,0:mt) 
      dimension r(0:mr),theta(0:mt)
c
c     Find indices corresponding to r1 and r2
      ir1 = 0
      ir2 = nt
c
      do i=nr,0,-1
         if (r(i) .le. r1) then
            ir1 = i
            go to 1000
         endif
      enddo
 1000 continue
c
      do i=1,nr
         if (r(i) .ge. r2) then
            ir2 = i
            go to 1100
         endif
      enddo
 1100 continue
c
      if (ir1 .ge. ir2) then
         fbar = rmiss
         return
      endif
c
      fbar = 0.0
      abar = 0.0
      do i=ir1,ir2
      do j=0,nt-1
         if (frt(i,j) .gt. rmiss) then
            abar = abar + r(i)
            fbar = fbar + r(i)*frt(i,j)
         endif
      enddo
      enddo
c           
      if (abar .gt. 0.0) then
         fbar = fbar/abar
      else
         fbar = rmiss
      endif
c
      return
      end
      subroutine cgcal(mr,mt,nr,nt,dr,dt,r0,dtr,r,theta,xrt,yrt,
     +                                              sinth,costh)
c     This routine calculate the radial and azimuthal
c     points of the cylindrical grid. The x,y coordinates of the cg
c     and the sines and cosines of the azimuthals are also calculated. 
c
      dimension r(0:mr),theta(0:mt)
      dimension xrt(0:mr,0:mt),yrt(0:mr,0:mt)
      dimension sinth(0:mt),costh(0:mt)
c
      do ii=0,nr
         r(ii) = r0 + dr*float(ii)
      enddo
c
      do jj=0,nt
         theta(jj) = dt*float(jj)
      enddo
c
      do jj=0,nt
         sinth(jj) = sin(dtr*theta(jj))
         costh(jj) = cos(dtr*theta(jj))
      enddo
c
      do ii=0,nr
      do jj=0,nt
         xrt(ii,jj) = r(ii)*costh(jj)
         yrt(ii,jj) = r(ii)*sinth(jj)
      enddo
      enddo
c
      return
      end
      subroutine gridcon(mx,my,nx,ny,rlon,rlat,slon,slat,dtr,
     +                   dlon,dlat,dx,dy,glon1,glat1,xg1,yg1)
c     This routine calculates variables needed to convert between
c     the lat/lon and cylindrical grids
c
      dimension rlon(mx),rlat(my)
c
c     Calcualte dx,dy of lat/lon grid at storm center location
      dlon = rlon(2)-rlon(1)
      dlat = rlat(2)-rlat(1)
c     
      dy = 111.1*dlat
      dx = 111.1*dlon*cos(dtr*slat)
c
c     Assuming storm center is origin, calculate x,y of lower left
c     model grid point
      glat1 = rlat(1)
      glon1 = rlon(1)
c     if (glon1 .lt. 0.0) glon1 = glon1 + 360.0
c
      xg1 = dx*(glon1-slon)/dlon
      yg1 = dy*(glat1-slat)/dlat
c
      return
      end
      subroutine lltocg3(f3,fc,mx,my,mp,mpl,nx,ny,np,mr,mt,nr,nt,
     +                   dx,dy,xg1,yg1,xrt,yrt,rmiss)
c     This routine interpolotes the function f3 from an evenly spaced grid to a
c     cylindrical grid. This version is where f3 is a 3D function (lon,lat,P). All
c     pressure levels are interpolated.
c
      dimension f3(mx,my,mp)
      dimension fc(0:mr,0:mt,mpl)
      dimension xrt(0:mr,0:mt),yrt(0:mr,0:mt)
c
      do k=1,np
         do ii=0,nr
         do jj=0,nt
            call lintcf(f3(1,1,k),xg1,dx,yg1,dy,mx,my,nx,ny,
     +                  xrt(ii,jj),yrt(ii,jj),rmiss,fxyii)
            fc(ii,jj,k) = fxyii
         enddo
         enddo
      enddo
c
      return
      end
      subroutine lltocg2(f2,fc,mx,my,nx,ny,mr,mt,nr,nt,
     +                   dx,dy,xg1,yg1,xrt,yrt,rmiss)
c     This routine interpolotes the function f2 from an evenly spaced grid to a
c     cylindrical grid. This version is where f2 is a 2D function (lon,lat).
c
      dimension f2(mx,my)
      dimension fc(0:mr,0:mt)
      dimension xrt(0:mr,0:mt),yrt(0:mr,0:mt)
c
      do ii=0,nr
      do jj=0,nt
         call lintcf(f2,xg1,dx,yg1,dy,mx,my,nx,ny,
     +               xrt(ii,jj),yrt(ii,jj),rmiss,fxyii)
         fc(ii,jj) = fxyii
      enddo
      enddo
c
      return
      end
      subroutine shrcal(mp,np,plev,pbot,ptop,usnd,vsnd,rmiss,
     +                                      shrmag,shrhead,ierr)
c     This routine calculates the vertical shear and direction
c 
      use diag_util
c 
      dimension usnd(mp),vsnd(mp),plev(mp)
c
c     Find vertical indices and weights closest to requested 
c     pressure levels.
      call pfind(pbot,plev,mp,np,kb1,kb2,wb1,wb2,ierrb)
      call pfind(ptop,plev,mp,np,kt1,kt2,wt1,wt2,ierrt)
c
      if (ierrb .ne. 0 .or. ierrt .ne. 0) then
         ierr = 2
         return
      endif
c
      ubot = wb1*usnd(kb1) + wb2*usnd(kb2)
      vbot = wb1*vsnd(kb1) + wb2*vsnd(kb2)
      utop = wt1*usnd(kt1) + wt2*usnd(kt2)
      vtop = wt1*vsnd(kt1) + wt2*vsnd(kt2)
c
      ushr = utop-ubot
      vshr = vtop-vbot
c
      call ctorh(ushr,vshr,shrmag,shrhead)
c 
      if ((usnd(kb1) .le. rmiss) .or. (usnd(kb2) .le. rmiss) .or. 
     +    (usnd(kt1) .le. rmiss) .or. (usnd(kt2) .le. rmiss) .or.
     +    (vsnd(kb1) .le. rmiss) .or. (vsnd(kb2) .le. rmiss) .or. 
     +    (vsnd(kt1) .le. rmiss) .or. (vsnd(kt2) .le. rmiss)) then
         shrmag = rmiss
         shrhead = rmiss
      endif
c
      return
      end
      subroutine pfind(p,plev,mp,np,k1,k2,w1,w2,ierrp)
c     This routine searches for the two pressure levels in the vertical
c     pressure domain closest to p and calculates the weights for each. 
c     If the requested level is not in the domain, ierrp is set to 1. 
c
      dimension plev(mp)
c
      ierrp = 0
c
c     Make sure the requested p is in the domain
      if (plev(np) .gt. plev(1)) then
         pmax = plev(np)
         pmin = plev( 1)
      else
         pmax = plev( 1)
         pmin = plev(np) 
      endif
c
      if (p .gt. pmax .or. p .lt. pmin) then
         ierrp = 1
         k1 = -999
         k2 = -999
         w1 = -999.9
         w2 = -999.9
         return
      endif
c
      dpmin = 10000.0
      k1 = -999
      k2 = -999
c
      do k=1,np
         dp = abs(plev(k) - p)
         if (dp .lt. dpmin) then
            dpmin = dp
            k1  = k 
         endif
      enddo
c
      if (k1 .eq. 1) then
         k2 = 2
      elseif (k1 .eq. np) then
         k2 = np-1
      else
         dpp = abs(plev(k1+1)-p)
         dpm = abs(plev(k1-1)-p)
         if (dpp .lt. dpm) then
            k2 = k1 + 1
         else
            k2 = k1-1
         endif
      endif
c
      w1 = (p-plev(k2))/(plev(k1)-plev(k2))
      w2 = (plev(k1)-p)/(plev(k1)-plev(k2))
c
      return
      end
      subroutine uvtort(uc,vc,sinth,costh,mr,mt,mp,nr,nt,np,urc,vtc,
     +                  rmiss)
c     This routine converts cartesian velocity components uc,vc on a cylindrical
c     grid to radial and tangential wind components on the same grid. 
c
      dimension uc(0:mr,0:mt,mp),vc(0:mr,0:mt,mp)
      dimension urc(0:mr,0:mt,mp),vtc(0:mr,0:mt,mp)
      dimension sinth(0:mt),costh(0:mt)
c
      do k=1,np
      do jj=0,nt
      do ii=0,nr
         urc(ii,jj,k) =  uc(ii,jj,k)*costh(jj) + vc(ii,jj,k)*sinth(jj)
         vtc(ii,jj,k) = -uc(ii,jj,k)*sinth(jj) + vc(ii,jj,k)*costh(jj)
         if ((uc(ii,jj,k) .le. rmiss) .or. 
     +       (vc(ii,jj,k) .le. rmiss)) then
            urc(ii,jj,k) = rmiss
            vtc(ii,jj,k) = rmiss
         endif
      enddo
      enddo
      enddo
c
      return
      end
      subroutine azavg3(frtp,frp,mr,mt,mp,nr,nt,np,rmiss)
c     The routine performs an azimuthal average of frtp to give frp.
c
      dimension frtp(0:mr,0:mt,mp),frp(0:mr,mp)
c
c      cf = 1.0/float(nt)
c
      do k=1,np
      do ii=0,nr
         icf=0
         frp(ii,k) = 0.0
         do jj = 0,nt-1
            if (frtp(ii,jj,k) .gt. rmiss) then
               icf=icf+1
               frp(ii,k) = frp(ii,k) + frtp(ii,jj,k)
            endif
         enddo
         if (icf .eq. nt) then
            cf = 1.0/float(icf)
            frp(ii,k) = cf*frp(ii,k)
         else
            frp(ii,k) = rmiss
         endif
      enddo
      enddo
c 
      return
      end
      subroutine aavd(urca,vtca,pvda,rvda,rmiss,r,plev,vavg,davg,
     +                mr,mp,mpl,nr,np,ierr)
c     This routine calculates the area averaged vorticity and divergence. The average
c     is from r=0 to r=rvda, so only the azmithally averaged ur and vt at r=rvda are needed.
c
      dimension urca(0:mr,mpl),vtca(0:mr,mpl)
      dimension r(0:mr),plev(mp)
c 
c     Find requested pressure level
      call pfind(pvda,plev,mp,np,k1,k2,w1,w2,ierrt)
      if (ierrt .ne. 0) then
         ierr = 3
         return
      endif
c
c     Make sure radial grid is large enough for requested radius
      if (rvda .gt. r(nr)) then
         ierr = 4
         return
      endif
c
c     Find index of nearest radial grid point
      drmin = 9999.9
      iim = -9
      rrm = -9999.9
      do ii=1,nr
            dr = abs(r(ii)-rvda)
            if (dr .lt. drmin) then
               drmin = dr
               iim = ii
            endif
      enddo
c 
      vavg = rmiss
      davg = rmiss
      rmm  = 1000.0*r(iim)
      if ((vtca(iim,k1) .gt. rmiss) .and. 
     +    (vtca(iim,k2) .gt. rmiss)) then
         vavg = 2.0*(w1*vtca(iim,k1) + w2*vtca(iim,k2))/rmm
      endif
      if ((urca(iim,k1) .gt. rmiss) .and. 
     +    (urca(iim,k2) .gt. rmiss)) then
         davg = 2.0*(w1*urca(iim,k1) + w2*urca(iim,k2))/rmm
      endif
c 
      return
      end
      subroutine ravt(urca,vtca,pta,rta,r,plev,vtbar,urbar,
     +                mr,mp,mpl,nr,np,ierr,rmiss)
c     This routine radially averages the tangential and radial wind.
c     The average is not radially weighted. 
c
      dimension urca(0:mr,mpl),vtca(0:mr,mpl)
      dimension r(0:mr),plev(mp)
c 
c     Find requested pressure level
      call pfind(pta,plev,mp,np,k1,k2,w1,w2,ierrt)
      if (ierrt .ne. 0) then
         ierr = 5
         return
      endif
c
c     Make sure radial grid is large enough for requested radius
      if (rta .gt. r(nr)) then
         ierr = 6
         return
      endif
c
c     Find index of nearest radial grid point
      drmin = 9999.9
      iim = -9
      do ii=1,nr
         dr = abs(r(ii)-rta)
         if (dr .lt. drmin) then
            drmin = dr
            iim = ii
         endif
      enddo
c
      vtbar = 0.0
      urbar = 0.0
      ifv = 0
      ifu = 0
      do ii=0,iim
         if ((vtca(ii,k1) .gt. rmiss) .and. 
     +       (vtca(ii,k2) .gt. rmiss)) then
            vtbar = vtbar + (w1*vtca(ii,k1) + w2*vtca(ii,k2))
            ifv = ifv + 1
         endif
         if ((urca(ii,k1) .gt. rmiss) .and. 
     +       (urca(ii,k2) .gt. rmiss)) then
            urbar = urbar + (w1*urca(ii,k1) + w2*urca(ii,k2))
            ifu = ifu + 1
         endif
      enddo
      if (ifv .ne. 0) then
         cfv = 1.0/float(ifv)
         vtbar = cfv*vtbar
      else
         vtbar = rmiss
      endif
      if (ifu .ne. 0) then
         cfu = 1.0/float(ifu)
         urbar = cfu*urbar
      else
         urbar = rmiss
      endif
c
      return
      end
      subroutine vorparm(usc,vsc,psc,r,srmax,mr,mt,nr,nt,rmiss,
     +                   vmax,rmw,vmaxa,rmwa,pmin)
c     This routine finds the max wind, radius of max wind and 
c     minimum sea level pressure between r=0 and r=srmax.
c
      dimension usc(0:mr,0:mt),vsc(0:mr,0:mt),psc(0:mr,0:mt)
      dimension vmax(0:mt),rmw(0:mt)
      dimension r(0:mr)
c
      vmaxa = rmiss
      rmwa  = rmiss
      pmin = rmiss
c
c     Find vmax and rmw at each azimuth
      do jj=0,nt
         vmax(jj) = rmiss
         rmw(jj)  = rmiss
         uvmax    = -1.0
         rmax     = -1.0
         do ii=0,nr
            if (r(ii)      .gt. srmax) go to 1000
            if (usc(ii,jj) .le. rmiss) go to 1000
            if (vsc(ii,jj) .le. rmiss) go to 1000
c
            uv = sqrt(usc(ii,jj)**2 + vsc(ii,jj)**2)
            if (uv .gt. uvmax) then
               uvmax = uv
               rmax  = r(ii)
            endif
 1000       continue
         enddo
c
         vmax(jj) = uvmax
         rmw(jj)  = rmax
      enddo
c
c     Azimuthally average rmw and find max value of vmax
      vmaxa = rmiss
      rmwa  = 0.0
      count = 0.0
      do jj=0,nt-1
         if (vmax(jj) .gt. vmaxa) vmaxa = vmax(jj)
c
         if (rmw(jj) .gt. rmiss) then
            count = count + 1.0
            rmwa = rmwa + rmw(jj)
         endif
      enddo
c
      if (count .gt. 0.0) then
         rmwa = rmwa/count
      else
         rmwa = rmiss
      endif
c
c     Find minimum sea-level pressure
      pmin = 10000.0*100.0
      do jj=0,nt-1
      do ii=0,nr
         if (r(ii)      .gt. srmax) go to 2000
         if (psc(ii,jj) .le. rmiss) go to 2000
c
         if (psc(ii,jj) .lt. pmin) pmin = psc(ii,jj)
c
 2000    continue
      enddo
      enddo
c 
c     Convert pmin from Pa to hPa (mb)
      pmin = pmin/100.0
c 
      if (pmin .gt. 9999.0) pmin=rmiss
      if (vmaxa .le. 0.0) then
         vmaxa=rmiss
         rmwa=rmiss
      endif
c
c     do i=0,nr
c        write(6,888) r(i),(psc(i,j),j=0,nt,2)
c 888    format(f7.1,1x,16(f7.1,1x))
c     enddo
c
      return
      end
