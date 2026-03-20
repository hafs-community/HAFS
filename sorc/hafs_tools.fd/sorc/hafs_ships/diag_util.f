      MODULE diag_util
c-----7---------------------------------------------------------------72
c     This module contains various subroutines used in the production
c      of diagnostic files from model fields.
c
c     Needs: dataformats.inc
c            dataio.f
c        
c     Current subroutines:
c        subroutine readADeck()
c        subroutine readgrid()     -modified in version 2.0
c        subroutine writeparams()
c        subroutine ctorh()
c        subroutine rhtoc()
c        subroutine tspdcal()
c
c     Last Modified: 03/02/2012, version 2.0
c-----7---------------------------------------------------------------72
c 
      include 'dataformats.inc'
      !also uses subroutines from dataio.f
c      IMPLICIT NONE
c 
      CONTAINS
c 
c-----7---------------------------------------------------------------72
c     subroutine readADeck
c-----7---------------------------------------------------------------72
      SUBROUTINE readADeck(luad,cdtg,cmodel,iftime,imiss,
     +                     ierr,ilat,ilon,ivmax,ipmin)
c-----7---------------------------------------------------------------72
c     Given the dtg, model, and forecast time; return the latitude,
c      longitude, vmax and pmin
c
c     ierr returns 1 if file not read in correctly
c
c-----7---------------------------------------------------------------72
c 
      IMPLICIT NONE
c 
      integer, intent(in) :: luad
      character(*), intent(in) :: cdtg
      character(*), intent(in) :: cmodel
      integer, intent(in) :: iftime
      integer, intent(in) :: imiss
      integer, intent(out) :: ierr
      integer, intent(out) :: ilat,ilon,ivmax,ipmin
c 
c      real, parameter :: rmiss = 9999.
c      integer, parameter :: mpt = 0
      integer :: result
      real :: templl
      type (BIG_AID_DATA) :: tempDTG
      type (AID_DATA) :: tempmodel
      type (A_RECORD) :: tempRecord
c 
c     initialize error flags
      ierr = 0
      result = 1
c 
c     initialize lat, lon, vmax, pmin
      ilat = imiss
      ilon = imiss
      ivmax = imiss
      ipmin = imiss
c 
      !finds all Aid for the given DTG
      call getBigAidDTG(luad,cdtg,tempDTG,result)
      if (result .eq. 0) go to 1002
c 
      !narrows down to the model at the given DTG
      call getTech(tempDTG,cmodel,tempmodel,result)
      if (result .eq. 0) go to 1002
c 
      !pulls the model forecast for the specified forecast time
      call getAidTAU(tempmodel,iftime,tempRecord,result)
      if (result .eq. 0) go to 1003
c 
      templl=tempRecord%lat
      if (tempRecord%NS .eq. 'S') templl = templl*(-1.0)
      ilat = nint(templl*10.0)
      templl=tempRecord%lon
      if (tempRecord%EW .eq. 'W') templl = 360 - templl
      ilon = nint(templl*10.0)
      ivmax = tempRecord%vmax
      ipmin = tempRecord%mslp
c 
      !write out ADeck info for debugging
c      write(*,*) 'ilat, ilon = ', ilat, ilon
c      write(*,*) 'ivmax, ipmin = ', ivmax, ipmin
c 
      return
c 
 1002 continue
      ierr = 1
      write(*,*) 'ADeck read failure'
      return
c 
 1003 continue
c      write(*,*) 'ADeck entry not found for specified forecast time'
      return
c 
      END SUBROUTINE readADeck
c 
c-----7---------------------------------------------------------------72
c     subroutine readgrid
c-----7---------------------------------------------------------------72
      SUBROUTINE readgrid(luin,nx,ny,nlevs,iplevs,rmiss,ierrc,
     +                    u,v,t,rh,z,us,vs,ts,rhs,ps,
     +                    sst,ohc,tpw)
c-----7---------------------------------------------------------------72
c     Fills in the model fields from the grid.
c
c     ierrc returns 1 if file not read in correctly
c
c-----7---------------------------------------------------------------72
c 
      IMPLICIT NONE
c 
      integer, intent(in) :: luin
      integer, intent(in) :: nx, ny, nlevs
      integer, dimension(nlevs), intent(in) :: iplevs
      real, intent(in) :: rmiss
      integer, intent(out) :: ierrc
      real, dimension(nx,ny,nlevs), intent(inout) :: u,v,t,rh,z
      real, dimension(nx,ny), intent(inout) :: us,vs,ts,rhs,ps
      real, dimension(nx,ny), intent(inout) :: sst,ohc,tpw
c 
c     local variables
      integer :: k, n, m, nxtemp, nytemp, ierr, numfields, iplevcurr
      character(len=80) :: ctemp, cfield
c
c     additional temporary local variables for binary read
      integer :: lubf
      character(len=80) :: fnbf
c 
c     initialize error flags
      ierrc = 0
      ierr=0
      numfields=0
c
c     set up binary file unit number
      lubf=42
c 
      !read in fields one at a time, starting with 2-line header
  100 continue
      read(luin,'(a80)',iostat=ierr) cfield
      if (ierr .ne. 0) go to 120
c      write(6,*) "cfield=", cfield
c      if (cfield(1:6) .ne. 'FIELD:') go to 1004
c  110 continue
c      read(luin,'(a80)',iostat=ierr) ctemp
c      if (ierr .ne. 0) go to 120
c      if (ctemp(1:6) .eq. 'FIELD:') then
c         cfield = ctemp
c         go to 110
c      endif
      open(unit=lubf,file=cfield,form='unformatted',
     +     status='old',err=1004)
c
c     Case statement determines which fields go into which variables
      select case (cfield(1:2))
      case ('SS')   !SST
         read(lubf) sst
c         write(6, *) 'lubf=', lubf
C         write(6, *) 'sst=', sst(400,500)
         do m=1,ny
            do n=1,nx
c               read(luin,*) sst(n,m)
               if ((sst(n,m) .le. rmiss) .or. 
     +            (sst(n,m) .ge. (-rmiss))) sst(n,m) = rmiss
               !check for land mask -- SST set to 0K (<1K should work)
               if (sst(n,m) .lt. 273.15) sst(n,m) = rmiss
c               if ((sst(n,m) .lt. 250.0) .and.
c     +            (sst(n,m) .gt. rmiss)) write (*,*) sst(n,m)
            enddo
         enddo
c         write(6,*) 'sst=', sst(400,500)
         close(lubf)
      case ('TP')   !TPW
         read(lubf) tpw
         do m=1,ny
            do n=1,nx
c               read(luin,*) tpw(n,m)
               if ((tpw(n,m) .le. rmiss) .or. 
     +            (tpw(n,m) .ge. (-rmiss))) tpw(n,m) = rmiss
            enddo
         enddo
         close(lubf)
      case ('OH')   !OHC
         !OHC not currently available, this is placeholder
         !will need to double-check accuracy if added in
         read(lubf) ohc
         do m=1,ny
            do n=1,nx
c               read(luin,*) ohc(n,m)
               if ((ohc(n,m) .le. rmiss) .or. 
     +            (ohc(n,m) .ge. (-rmiss))) ohc(n,m) = rmiss
            enddo
         enddo
         close(lubf)
      case ('U_')   !U: surface, iplevs
         if (cfield(3:6) .eq. 'SURF') then
            read(lubf) us
            do m=1,ny
               do n=1,nx
c                  read(luin,*) us(n,m)
                  if ((us(n,m) .le. rmiss) .or. 
     +               (us(n,m) .ge. (-rmiss))) us(n,m) = rmiss
               enddo
            enddo
            close(lubf)
c     Begin testing for binary file
c           open binary file
c            fnbf='U_SURF_p.bin'
c            lubf=42
c            open(unit=lubf,file=fnbf,form='unformatted',
c     +           status='old',err=1004)
c           read binary file
c            us=rmiss
c            read(lubf) us
c            do m=1,ny
c               do n=1,nx
c                  if ((us(n,m) .le. rmiss) .or. 
c     +               (us(n,m) .ge. (-rmiss))) us(n,m) = rmiss
c               enddo
c            enddo
c           close binary file
c            close(lubf)
c     End binary testing
         else
            read(cfield(3:6), '(i4.4)') iplevcurr
            do k=1,nlevs
               if (iplevs(k) .eq. iplevcurr) then
                  read(lubf) u(:,:,k)
                  do m=1,ny
                     do n=1,nx
c                        read(luin,*) u(n,m,k)
                        if ((u(n,m,k) .le. rmiss) .or. 
     +                  (u(n,m,k) .ge. (-rmiss))) u(n,m,k) = rmiss
                     enddo
                  enddo
               endif
            enddo
            close(lubf)
         endif
      case ('V_')   !V: surface, iplevs
         if (cfield(3:6) .eq. 'SURF') then
            read(lubf) vs
            do m=1,ny
               do n=1,nx
c                  read(luin,*) vs(n,m)
                  if ((vs(n,m) .le. rmiss) .or. 
     +               (vs(n,m) .ge. (-rmiss))) vs(n,m) = rmiss
               enddo
            enddo
            close(lubf)
         else
            read(cfield(3:6), '(i4.4)') iplevcurr
            do k=1,nlevs
               if (iplevs(k) .eq. iplevcurr) then
                  read(lubf) v(:,:,k)
                  do m=1,ny
                     do n=1,nx
c                        read(luin,*) v(n,m,k)
                        if ((v(n,m,k) .le. rmiss) .or. 
     +                  (v(n,m,k) .ge. (-rmiss))) v(n,m,k) = rmiss
                     enddo
                  enddo
               endif
            enddo
            close(lubf)
         endif
      case ('T_')   !T: surface, iplevs
         if (cfield(3:6) .eq. 'SURF') then
            read(lubf) ts
            do m=1,ny
               do n=1,nx
c                  read(luin,*) ts(n,m)
                  if ((ts(n,m) .le. rmiss) .or. 
     +               (ts(n,m) .ge. (-rmiss))) ts(n,m) = rmiss
               enddo
            enddo
            close(lubf)
         else
            read(cfield(3:6), '(i4.4)') iplevcurr
            do k=1,nlevs
               if (iplevs(k) .eq. iplevcurr) then
                  read(lubf) t(:,:,k)
                  do m=1,ny
                     do n=1,nx
c                        read(luin,*) t(n,m,k)
                        if ((t(n,m,k) .le. rmiss) .or. 
     +                  (t(n,m,k) .ge. (-rmiss))) t(n,m,k) = rmiss
                     enddo
                  enddo
               endif
            enddo
            close(lubf)
         endif
      case ('R_')   !RH: surface, iplevs
         if (cfield(3:6) .eq. 'SURF') then
            read(lubf) rhs
            do m=1,ny
               do n=1,nx
c                  read(luin,*) rhs(n,m)
                  if ((rhs(n,m) .le. rmiss) .or. 
     +               (rhs(n,m) .ge. (-rmiss))) rhs(n,m) = rmiss
               enddo
            enddo
            close(lubf)
         else
            read(cfield(3:6), '(i4.4)') iplevcurr
            do k=1,nlevs
               if (iplevs(k) .eq. iplevcurr) then
                  read(lubf) rh(:,:,k)
                  do m=1,ny
                     do n=1,nx
c                        read(luin,*) rh(n,m,k)
                        if ((rh(n,m,k) .le. rmiss) .or. 
     +                  (rh(n,m,k) .ge. (-rmiss))) rh(n,m,k) = rmiss
                     enddo
                  enddo
               endif
            enddo
            close(lubf)
         endif
      case ('Z_')
         read(cfield(3:6), '(i4.4)') iplevcurr
         do k=1,nlevs
            if (iplevs(k) .eq. iplevcurr) then
               read(lubf) z(:,:,k)
               do m=1,ny
                  do n=1,nx
c                     read(luin,*) z(n,m,k)
                     if ((z(n,m,k) .le. rmiss) .or. 
     +               (z(n,m,k) .ge. (-rmiss*1000.0))) z(n,m,k) = rmiss
                  enddo
               enddo
            endif
         enddo
         close(lubf)
      case ('P_')
         if (cfield(3:6) .eq. 'SURF') then
            read(lubf) ps
            do m=1,ny
               do n=1,nx
c                  read(luin,*) ps(n,m)
                  if ((ps(n,m) .le. rmiss) .or. 
     +            (ps(n,m) .ge. (-rmiss*1000.0))) ps(n,m) = rmiss
               enddo
            enddo
         endif
         close(lubf)
      case default
         write(*,*) 'Warning: field not used in diagnostic, skipping ',
     +              cfield
         close(lubf)
      end select
      numfields=numfields+1
      go to 100
  120 continue
c      debugging option, see number of fields read in
c       note: will include fields that had no match in the select case
c             but will not include empty fields
c      write(*,*) 'number of fields: ', numfields
c 
      return
c 
 1004 continue
      ierrc = 1
      write(*,*) 'Grid field read failure'
      return
c 
      END SUBROUTINE readgrid
c 
c-----7---------------------------------------------------------------72
c     subroutine writeparams
c-----7---------------------------------------------------------------72
      SUBROUTINE writeparams(luou,ierr,rmiss,imiss,imissd,
     +                 nvar,diagvar,idiagvar,nsnd,
     +                 usnd,vsnd,tsnd,rsnd,zsnd,
     +                 usfc,vsfc,tsfc,rsfc,psfc)
c-----7---------------------------------------------------------------72
c     Given the dtg, model, and forecast time; return the latitude,
c      longitude, vmax and pmin
c
c     ierr returns 1 if file not read in correctly
c
c-----7---------------------------------------------------------------72
c 
      IMPLICIT NONE
c 
      integer, intent(in) :: luou
      integer, intent(in) :: imiss, imissd
      real, intent(in) :: rmiss
      integer, intent(in) :: nvar
      real, dimension(nvar), intent(in) :: diagvar
      integer, dimension(nvar), intent(in) :: idiagvar
      integer, intent(in) :: nsnd
      real, dimension(nsnd), intent(in) :: usnd, vsnd, tsnd, rsnd, zsnd
      real, intent(in) :: usfc, vsfc, tsfc, rsfc, psfc
      integer, intent(out) :: ierr
c 
c     local variables
      integer :: n
c 
c     initialize error flag
      ierr = 0
c     initialize write out format
  110    format(i6)
c 
      do n=1,nvar
         write(luou,110) idiagvar(n)
      enddo
c 
      !write out surface variables
      if (tsfc .le. rmiss) then
         write(luou,110) imissd
      else
         write(luou,110) nint((tsfc-273.15)*10.0)
      endif
      if (rsfc .le. rmiss) then
         write(luou,110) imissd
      else
         write(luou,110) nint(rsfc)
      endif
      if (psfc .le. rmiss) then
         write(luou,110) imissd
      else
         write(luou,110) nint(psfc/100.0)
      endif
      if (usfc .le. rmiss) then
         write(luou,110) imissd
      else
         write(luou,110) nint(usfc*10.0)
      endif
      if (vsfc .le. rmiss) then
         write(luou,110) imissd
      else
         write(luou,110) nint(vsfc*10.0)
      endif
c 
      !write out sounding variables
      do n=1,nsnd
         if (tsnd(n) .le. rmiss) then
            write(luou,110) imissd
         else
            write(luou,110) nint((tsnd(n)-273.15)*10.0)
         endif
         if (rsnd(n) .le. rmiss) then
            write(luou,110) imissd
         else
            write(luou,110) nint(rsnd(n))
         endif
         if (zsnd(n) .le. rmiss) then
            write(luou,110) imissd
         else
            write(luou,110) nint(zsnd(n)/10.0)
         endif
         if (usnd(n) .le. rmiss) then
            write(luou,110) imissd
         else
            write(luou,110) nint(usnd(n)*10.0)
         endif
         if (vsnd(n) .le. rmiss) then
            write(luou,110) imissd
         else
            write(luou,110) nint(vsnd(n)*10.0)
         endif
      enddo
c 
      return
c 
 1009 continue
      ierr = 1
      write(*,*) 'Parameter write failure'
      return
c 
      END SUBROUTINE writeparams
c 
c-----7---------------------------------------------------------------72
c     subroutine ctorh
c-----7---------------------------------------------------------------72
      SUBROUTINE ctorh(x,y,r,theta)
c     This routine converts from Cartesion coordinates
c     to radial coordinates, where theta is in
c     degrees measured clockwise from
c     the +y-axis (standard meteorological heading).
c 
      IMPLICIT NONE
c 
      !list calling variables
      real, intent(in) :: x, y         !cartesian coordinates
      real, intent(out) ::  r, theta   !radial coordinates
c
      !list local variables
      real :: rtd   !radians to degrees conversion factor
c 
      r = sqrt(x*x + y*y)
c 
      if (r .le. 0.0) then
         theta = 0.0
         return
      endif
c 
      rtd = 57.296
      theta = rtd*acos(x/r)
      if (y .lt. 0.0) theta = 360.0 - theta
c
c     Convert theta to heading
      theta = 90.0 - theta
      if (theta .lt. 0.0) theta = theta + 360.0
c 
      return
      END SUBROUTINE ctorh
c 
c-----7---------------------------------------------------------------72
c     subroutine rhtoc
c-----7---------------------------------------------------------------72
      SUBROUTINE rhtoc(r,thetah,x,y)
c     This routine converts from radial coordinates
c     to Cartesian coordinates, where theta is in
c     degrees measured clockwise from
c     the +y-axis (standard meteorological heading).
c 
      IMPLICIT NONE
c 
      !list calling variables
      real, intent(in) ::  r, thetah   !radial coordinates
      real, intent(out) :: x, y        !cartesian coordinates
c
      !list local variables
      real :: theta   !converted theta
      real :: rtd     !radians to degrees conversion factor
c
c     Convert theta from heading to standard definition
      theta = 90.0 - thetah
      if (theta .lt. 0.0) theta = theta + 360.0
c 
      rtd = 57.296
      x = r*cos(theta/rtd)
      y = r*sin(theta/rtd)
c 
      return
      END SUBROUTINE rhtoc
c 
c-----7---------------------------------------------------------------72
c     subroutine tspdcal
c-----7---------------------------------------------------------------72
      SUBROUTINE tspdcal(tlat,tlon,ttime,mft,rmissval,cx,cy,cmag)
c     This routine calculates the components of the storm
c     motion (cx,cy) in knots, given the lat/lon as a function
c     of time. Centered or one-sided differences are used as a appropriate.
c     Missing lat/lon points (equal to rmissval) are accounted for. 
c
c     This routine assumes lat is in range: -90..90 degrees
c                          lon is in range: 0..360 degrees
c                          rmissval is large negative number
c     
      IMPLICIT NONE
c 
      !list calling variables
      integer, intent(in) :: mft        !maximum index value
      real, intent(in) ::    rmissval   !real missing value default
      real, dimension(mft), intent(in) ::  tlat, tlon, ttime
                                        !lat, lon, time series
      real, dimension(mft), intent(out) :: cx, cy, cmag
                                        !storm motion components, total
c
      !list local variables
      real ::    dtr, dtnmi          !conversion factors
      integer :: k                   !loop counter
      integer :: im, i0, ip, icode   !missing value flags
      real ::    dlat, dlon, dt      !change in lat, lon, time
      real ::    alat, cfac          !intermediate calculations
c
c     Initialize variables to missing
      do k=1,mft
         cx(k) =   rmissval
         cy(k) =   rmissval
         cmag(k) = rmissval
      enddo
c 
      dtnmi = 60.0
      dtr   = 3.14159/180.0
c 
      do k=1,mft
         im=0
         i0=0
         ip=0
         if ((k .gt. 1) .and. (tlat(k-1) .gt. rmissval)) im=1
         if (tlat(k  ) .gt. rmissval) i0=1
         if ((k .lt. mft) .and. (tlat(k+1) .gt. rmissval)) ip=1
c 
         icode = ip + 10*i0 + 100*im
c 
         if (icode .eq. 101 .or. icode .eq. 111) then
c           Use centered differences
            dlat =     (tlat(k+1)-tlat(k-1))
            dlon =     (tlon(k+1)-tlon(k-1))
            dt   =     (ttime(k+1)-ttime(k-1))
            alat = 0.5*(tlat(k+1)+tlat(k-1))
            cfac = cos(dtr*alat)
c 
            cy(k) =      dtnmi*dlat/dt
            cx(k) = cfac*dtnmi*dlon/dt
            cmag(k) = sqrt(cx(k)**2 + cy(k)**2)
         elseif (icode .eq. 011) then
c           Use forward difference
            dlat =     (tlat(k+1)-tlat(k))
            dlon =     (tlon(k+1)-tlon(k))
            dt   =     (ttime(k+1)-ttime(k))
            alat = 0.5*(tlat(k+1)+tlat(k))
            cfac = cos(dtr*alat)
c 
            cy(k) =      dtnmi*dlat/dt
            cx(k) = cfac*dtnmi*dlon/dt
            cmag(k) = sqrt(cx(k)**2 + cy(k)**2)
         elseif (icode .eq. 110) then
c           Use backward difference
            dlat =     (tlat(k)-tlat(k-1))
            dlon =     (tlon(k)-tlon(k-1))
            dt   =     (ttime(k)-ttime(k-1))
            alat = 0.5*(tlat(k)+tlat(k-1))
            cfac = cos(dtr*alat)
c 
            cy(k) =      dtnmi*dlat/dt
            cx(k) = cfac*dtnmi*dlon/dt
            cmag(k) = sqrt(cx(k)**2 + cy(k)**2)
         else
c           Two of three points are missing. Can not calculate speed components
            cx(k) = rmissval
            cy(k) = rmissval
            cmag(k) = rmissval
         endif
c        write(6,888) k*6,tlat(k),tlon(k),cx(k),cy(k)
c 888    format('t,lat,lon,cx,cy: ',i4,1x,4(f8.1,1x))
      enddo
c 
      return
      END SUBROUTINE tspdcal
c 
      END MODULE diag_util
