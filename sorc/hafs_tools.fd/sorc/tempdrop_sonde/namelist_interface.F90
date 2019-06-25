module namelist_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! tempdrop-sonde :: namelist_interface
  ! Copyright (C) 2017 Henry R. Winterbottom

  ! Email: henry.winterbottom@noaa.gov

  ! This program is free software: you can redistribute it and/or
  ! modify it under the terms of the GNU General Public License as
  ! published by the Free Software Foundation, either version 3 of the
  ! License, or (at your option) any later version.

  ! This program is distributed in the hope that it will be useful,
  ! but WITHOUT ANY WARRANTY; without even the implied warranty of
  ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  ! General Public License for more details.

  ! You should have received a copy of the GNU General Public License
  ! along with this program.  If not, see
  ! <http://www.gnu.org/licenses/>.

  !=======================================================================

  ! Define associated modules and subroutines

  use kinds_interface

  ! Define interfaces and attributes for module routines
  
  implicit none

  !-----------------------------------------------------------------------

  ! DESCRIPTION (alphabetized):

  ! * datapath; a FORTRAN character string specifying the full-path to
  !   the directory to contain output files written by the respective
  !   routines.

  ! * debug; a FORTRAN logical value specifying whether to include
  !   debug information during execution.

  ! * if_normalize; a FORTRAN logical value specifying whether to 
  !   normalize the geographical coordinate values computed for the 
  !   advection trajectory of the sondes.

  ! * sonde_filelist; a FORTRAN character string specifying the
  !   full-path to the external file containing a list of TEMPDROP
  !   formatted sondes to be decoded.

  ! * write_hsa; a FORTRAN logical value specifying whether to write
  !   National Oceanic and Atmospheric Administration (NOAA) Atlantic
  !   Oceanographic and Meteorological Laboratory (AOML) Hurricane
  !   Research Division (HRD) spline analysis (HSA) formatted files.

  ! * write_hsa_drift; a FORTRAN logical value specifying whether to
  !   write National Oceanic and Atmospheric Administration (NOAA)
  !   Atlantic Oceanographic and Meteorological Laboratory (AOML)
  !   Hurricane Research Division (HRD) spline analysis (HSA)
  !   formatted files which include the geographical positions
  !   estimated as a function of sonde drift.

  ! * write_nc_skewt; a FORTRAN logical value specifying whether to
  !   write a network common data format (netcdf) file containing
  !   interpolated National Oceanic and Atmospheric Administration
  !   (NOAA) Atlantic Oceanographic and Meteorological Laboratory
  !   (AOML) Hurricane Research Division (HRD) spline analysis (HSA)
  !   values; write_hsa_drift must be true.

  !-----------------------------------------------------------------------

  ! Define local variables

  character(len=500)                                                    :: &
       & datapath = './'
  character(len=500)                                                    :: &
       & sonde_filelist = 'NOT USED'
  logical                                                               :: &
       & debug = .false.
  logical                                                               :: &
       & if_normalize = .false.
  logical                                                               :: &
       & write_hsa = .false.
  logical                                                               :: &
       & write_hsa_drift = .false.
  logical                                                               :: &
       & write_nc_skewt = .false.    
  namelist /share/ datapath, debug, if_normalize, sonde_filelist,          &
       & write_hsa, write_hsa_drift, write_nc_skewt
 
  !-----------------------------------------------------------------------

contains
  
  !=======================================================================

  ! SUBROUTINE: 

  ! namelist.f90

  ! DESCRIPTION: 

  ! This subroutine acts as the interface to the namelist file,
  ! provided as 'tempdrop-sonde.input' by the user.

  !-----------------------------------------------------------------------

  subroutine namelist()

    ! Define variables computed within routine

    logical                                                             :: is_it_there
    integer                                                             :: unit_nml

    !=====================================================================
    
    ! Define local variables

    unit_nml     = 9
    is_it_there  = .false.
    inquire(file = 'tempdrop-sonde.input',exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = 'tempdrop-sonde.input',                             &
            unit   = unit_nml        ,                                   &
            status = 'old'         ,                                     &
            form   = 'formatted'     ,                                   &
            action = 'read')
       read(unit_nml,NML = share)
       close(unit_nml)

    else  ! if(is_it_there)

       ! Define local variables

       write(6,500)
       stop
       
    end if ! if(is_it_there)
       
    ! Define local variables
    
    write(6,*) '&SHARE'             
    write(6,*) 'DATAPATH                      = ',                       &
         & trim(adjustl(datapath))
    write(6,*) 'DEBUG                         = ', debug
    write(6,*) 'IF_NORMALIZE                  = ', if_normalize
    write(6,*) 'SONDE_FILELIST                = ',                       &
         & trim(adjustl(sonde_filelist))
    write(6,*) 'WRITE_HSA                     = ', write_hsa
    write(6,*) 'WRITE_HSA_DRIFT               = ', write_hsa_drift
    write(6,*) 'WRITE_NC_SKEWT                = ', write_nc_skewt
    write(6,*) '/'
500 format('NAMELISTPARAMS: tempdrop-sonde.input not found in the ',     &
         & 'current working directory. ABORTING!!!!')

    !===================================================================

  end subroutine namelist
  
  !=======================================================================

end module namelist_interface
