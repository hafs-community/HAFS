program statsin_domain
!------------------------------------------------------
! This program reads the 'statsin' program to get the 
! center lon/lat at each time level. The max and min
! of the longitude and the max and min of the latitude
! are computed for the duration of the simulation.
! alon    = lon read in
! alat    = lat read in
! emaxlat = max latitude
! eminlat = min latitude
! emaxlon = max longitude
! eminlon = min longitude  
! The extent of the domain plotted is then determined by 
! slat    = eminlat - 5.0
! nlat    = emaxlat + 5.0    
! wlon    = eminlon - 5.0 
! elon    = emaxlon + 5.0
! August 2007 William O'Connor 
!------------------------------------------------------
implicit none
real :: emaxlat,eminlat,emaxlon,eminlon,slat,nlat,wlon,elon,alon,alat 
!------------------------                    
! initialize max and min values to excessive values
emaxlat = -100.
eminlat = +100.
emaxlon = -500.
eminlon = +500.
!------------------------                    
! open the 'statsin' ascii data file
open(unit=10,status='old',form='formatted',file='statsin')
! read and process to end of file with this DO FOREVER loop  
do
read(10,'(19x,f7.2,6x,f7.2)',end=99) alon,alat
write(6,'(a15,f7.2,a15,f7.2)') 'alon = ',alon,' alat = ',alat 
  if(alon.gt.emaxlon) emaxlon=alon 
  if(alon.lt.eminlon) eminlon=alon
  if(alat.gt.emaxlat) emaxlat=alat
  if(alat.lt.eminlat) eminlat=alat 
enddo 
99 continue
close(10) 
!------------------------                    

write(6,'(a20,f7.2)') 'emaxlon = ',emaxlon
write(6,'(a20,f7.2)') 'eminlon = ',eminlon
write(6,'(a20,f7.2)') 'emaxlat = ',emaxlat  
write(6,'(a20,f7.2)') 'eminlat = ',eminlat

! Set the boundaries of the domain to extend 6 deg beyon the 
! max min values and add 360 deg in longitude  

slat = eminlat - 6.0
nlat = emaxlat + 6.0
wlon = eminlon - 6.0 + 360.
elon = emaxlon + 6.0 + 360. 

write(6,'(a20,f7.2)') 'slat = ',slat 
write(6,'(a20,f7.2)') 'nlat = ',nlat  
write(6,'(a20,f7.2)') 'wlon = ',wlon  
write(6,'(a20,f7.2)') 'elon = ',elon 
!------------------------                    
! write the boundary values to a file to be read with awk
open(unit=90,form='formatted',status='new',file='bndry_pts.txt') 
write(90,'(f7.2,5x,f7.2,5x,f7.2,5x,f7.2)') slat,nlat,wlon,elon
close(90) 
!------------------------                    
stop 
end program statsin_domain
