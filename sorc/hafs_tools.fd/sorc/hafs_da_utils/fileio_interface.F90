module fileio_interface

  !=======================================================================

  !$$$ MODULE DOCUMENTATION BLOCK
  
  ! da-utils :: fileio_interface
  ! Copyright (C) 2019 Henry R. Winterbottom

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

  use bufrio_interface
  use json_interface
  use kinds_interface
  use namelist_interface
  use nemsio_interface
  use nemsio_module
  use netcdf
  use netcdf_interface
  use variable_interface

  ! Define interfaces and attributes for module routines
  
  implicit none
  private
  public :: fileio_interface_read
  public :: fileio_interface_write
  interface fileio_interface_read
     module procedure read_conv_bufr
     module procedure read_json_nems
     module procedure read_list
     module procedure read_nems_init
     module procedure read_nems_var
     module procedure read_sat_bufr
  end interface fileio_interface_read
  interface fileio_interface_varinfo
     module procedure varinfo_convbufr
     module procedure varinfo_gsidiagisovar
     module procedure varinfo_gsidiagprfvar
     module procedure varinfo_nems2ncdf
     module procedure varinfo_satbufr
  end interface fileio_interface_varinfo
  interface fileio_interface_write
     module procedure write_netcdf_convbufr
     module procedure write_netcdf_gsidiagisovar
     module procedure write_netcdf_gsidiagprfvar
     module procedure write_netcdf_nems2ncdf
     module procedure write_netcdf_satbufr
  end interface fileio_interface_write

  ! Define local variables

  logical                                                               :: is_init_nems2ncdf = .true.

  !-----------------------------------------------------------------------

contains

  !=======================================================================

  ! SUBROUTINE:

  ! ncep_said_table.f90

  ! DESCRIPTION:

  ! This subroutine returns the satellite mission name (sname)
  ! corresponding to the user-specified satellite identification value
  ! (e.g., said).

  ! REFERENCES:

  ! https://www.nco.ncep.noaa.gov/sib/jeff/CodeFlag_0_STDv30_LOC7.html#001007

  ! NOTE:

  ! FORTRAN indexing for table values implemented within this
  ! subroutine.

  ! INPUT VARIABLES:

  ! * said; a FORTRAN integer value specifying the satellite
  !   identification value.

  ! * sname; a FORTRAN character string.

  ! OUTPUT VARIABLES:

  ! * sname; a FORTRAN character string containing the satellite
  !   mission name corresponding to the user-specified satellite
  !   identification value.

  !-----------------------------------------------------------------------

  subroutine ncep_said_table(said,sname)

    ! Define variables passed to routine

    character(len=100)                                                  :: sname
    integer                                                             :: said

    ! Define variables computed within routine

    character(len=100),         dimension(:),               allocatable :: snames
    integer,                    dimension(:),               allocatable :: saids

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Allocate memory for local variables

    if(.not. allocated(snames)) allocate(snames(1024))
    if(.not. allocated(saids))  allocate(saids(1024))

    ! Loop through local variable

    do i = 1, size(snames)

       ! Define local variables

       snames(i) = 'RESERVED'
       saids(i)  = i - 1

    end do ! do i = 1, size(snames)

    ! Define local variables

    snames(2)    = 'ERS 1'
    snames(3)    = 'ERS 2'
    snames(4)    = 'METOP-1 (METOP-B)'
    snames(5)    = 'METOP-2 (METOP-A)'
    snames(6)    = 'METOP-3 (METOP-C)'
    snames(21)   = 'SPOT1'
    snames(22)   = 'SPOT2'
    snames(23)   = 'SPOT3'
    snames(24)   = 'SPOT4'
    snames(41)   = 'OERSTED'
    snames(42)   = 'CHAMP'
    snames(43)   = 'TerraSAR-X'
    snames(44)   = 'TanDEM-X'
    snames(45)   = 'PAZ'
    snames(47)   = 'SMOS'
    snames(48)   = 'CryoSat-2'
    snames(49)   = 'AEOLUS'
    snames(51)   = 'METEOSAT 3'
    snames(52)   = 'METEOSAT 4'
    snames(53)   = 'METEOSAT 5'
    snames(54)   = 'METEOSAT 6'
    snames(55)   = 'METEOSAT 7'
    snames(56)   = 'METEOSAT 8'
    snames(57)   = 'METEOSAT 9'
    snames(58)   = 'METEOSAT 10'
    snames(59)   = 'METEOSAT 1'
    snames(60)   = 'METEOSAT 2'
    snames(61)   = 'ENVISAT'
    snames(62)   = 'Sentinel 3A'
    snames(63)   = 'Sentinel 1A'
    snames(64)   = 'Sentinel 1B'
    snames(65)   = 'Sentinel 5P'
    snames(66)   = 'Sentinel 3B'
    snames(71)   = 'METEOSAT 11'
    snames(121)  = 'ADEOS'
    snames(122)  = 'ADEOS II'
    snames(123)  = 'GCOM-W1'
    snames(141)  = 'GOSAT'
    snames(151)  = 'GMS 3'
    snames(152)  = 'GMS 4'
    snames(153)  = 'GMS 5'
    snames(154)  = 'GMS'
    snames(155)  = 'GMS-2'
    snames(172)  = 'MTSAT-1R'
    snames(173)  = 'MTSAT-2'
    snames(174)  = 'Himawari-8'
    snames(175)  = 'Himawari-9'
    snames(201)  = 'NOAA 8'
    snames(202)  = 'NOAA 9'
    snames(203)  = 'NOAA 10'
    snames(204)  = 'NOAA 11'
    snames(205)  = 'NOAA 12'
    snames(206)  = 'NOAA 14'
    snames(207)  = 'NOAA 15'
    snames(208)  = 'NOAA 16'
    snames(209)  = 'NOAA 17'
    snames(210)  = 'NOAA 18'
    snames(221)  = 'LANDSAT 5'
    snames(222)  = 'LANDSAT 4'
    snames(223)  = 'LANDSAT 7'
    snames(224)  = 'NOAA 19'
    snames(225)  = 'NPP'
    snames(226)  = 'NOAA 20'
    snames(227)  = 'NOAA 21'
    snames(241)  = 'DMSP 7'
    snames(242)  = 'DMSP 8'
    snames(243)  = 'DMSP 9'
    snames(244)  = 'DMSP 10'
    snames(245)  = 'DMSP 11'
    snames(246)  = 'DMSP 12'
    snames(247)  = 'DMSP 13'
    snames(248)  = 'DMSP 14'
    snames(249)  = 'DMSP 15'
    snames(250)  = 'DMSP 16'
    snames(251)  = 'GOES 6'
    snames(252)  = 'GOES 7'
    snames(253)  = 'GOES 8'
    snames(254)  = 'GOES 9'
    snames(255)  = 'GOES 10'
    snames(256)  = 'GOES 11'
    snames(257)  = 'GOES 12'
    snames(258)  = 'GOES 13'
    snames(259)  = 'GOES 14'
    snames(260)  = 'GOES 15'
    snames(261)  = 'JASON 1'
    snames(262)  = 'JASON 2'
    snames(270)  = 'Spire Lemur 3U Cubesat'
    snames(271)  = 'GOES 16'
    snames(272)  = 'GOES 17'
    snames(273)  = 'GOES 18'
    snames(274)  = 'GOES 19'
    snames(282)  = 'QUIKSCAT'
    snames(283)  = 'TRMM'
    snames(284)  = 'CORIOLIS'
    snames(286)  = 'DMSP 17'
    snames(287)  = 'DMSP 18'
    snames(288)  = 'DMSP 19'
    snames(289)  = 'GPM-core'
    snames(290)  = 'Orbiting Carbon Observatory - 2 (OCO-2, NASA)'
    snames(311)  = 'GOMS 1'
    snames(312)  = 'GOMS 2'
    snames(321)  = 'METEOR 2-21'
    snames(322)  = 'METEOR 3-5'
    snames(323)  = 'METEOR 3M-1'
    snames(324)  = 'METEOR 3M-2'
    snames(342)  = 'RESURS 01-4'
    snames(411)  = 'KALPANA-1'
    snames(422)  = 'Oceansat-2'
    snames(423)  = 'ScatSat-1'
    snames(431)  = 'INSAT 1B'
    snames(432)  = 'INSAT 1C'
    snames(433)  = 'INSAT 1D'
    snames(441)  = 'Megha-Tropiques'
    snames(442)  = 'SARAL'
    snames(451)  = 'INSAT 2A'
    snames(452)  = 'INSAT 2B'
    snames(453)  = 'INSAT 2E'
    snames(471)  = 'INSAT 3A'
    snames(472)  = 'INSAT 3D'
    snames(473)  = 'INSAT 3E'
    snames(474)  = 'INSAT 3DR'
    snames(475)  = 'INSAT 3DS'
    snames(501)  = 'FY-1C'
    snames(502)  = 'FY-1D'
    snames(503)  = 'Hai Yang 2A (HY-2A, SOA/NSOAS China)'
    snames(511)  = 'FY-2'
    snames(513)  = 'FY-2B'
    snames(514)  = 'FY-2C'
    snames(515)  = 'FY-2D'
    snames(516)  = 'FY-2E'
    snames(517)  = 'FY-2F'
    snames(518)  = 'FY-2G'
    snames(521)  = 'FY-3A'
    snames(522)  = 'FY-3B'
    snames(523)  = 'FY-3C'
    snames(531)  = 'FY-4A'
    snames(701)  = 'TIROS M (ITOS 1)'
    snames(702)  = 'NOAA 1'
    snames(703)  = 'NOAA 2'
    snames(704)  = 'NOAA 3'
    snames(705)  = 'NOAA 4'
    snames(706)  = 'NOAA 5'
    snames(707)  = 'NOAA 6'
    snames(708)  = 'NOAA 7'
    snames(709)  = 'TIROS-N'
    snames(711)  = 'GOES (SMS 1)'
    snames(712)  = 'GOES (SMS 2)'
    snames(721)  = 'TOPEX'
    snames(722)  = 'GFO'
    snames(723)  = 'GRACE A'
    snames(724)  = 'GRACE B'
    snames(725)  = 'COSMIC-2 P1'
    snames(726)  = 'COSMIC-2 P2'
    snames(727)  = 'COSMIC-2 P3'
    snames(728)  = 'COSMIC-2 P4'
    snames(729)  = 'COSMIC-2 P5'
    snames(730)  = 'COSMIC-2 P6'
    snames(732)  = 'GOES 1'
    snames(733)  = 'GOES 2'
    snames(734)  = 'GOES 3'
    snames(735)  = 'GOES 4'
    snames(736)  = 'GOES 5'
    snames(741)  = 'COSMIC-1'
    snames(742)  = 'COSMIC-2'
    snames(743)  = 'COSMIC-3'
    snames(744)  = 'COSMIC-4'
    snames(745)  = 'COSMIC-5'
    snames(746)  = 'COSMIC-6'
    snames(751)  = 'COSMIC-2 E1'
    snames(752)  = 'COSMIC-2 E2'
    snames(753)  = 'COSMIC-2 E3'
    snames(754)  = 'COSMIC-2 E4'
    snames(755)  = 'COSMIC-2 E5'
    snames(756)  = 'COSMIC-2 E6'
    snames(764)  = 'NIMBUS 3'
    snames(765)  = 'NIMBUS 4'
    snames(766)  = 'NIMBUS 5'
    snames(767)  = 'NIMBUS 6'
    snames(768)  = 'NIMBUS 7'
    snames(781)  = 'ERBS'
    snames(782)  = 'UARS'
    snames(783)  = 'EARTH PROBE'
    snames(784)  = 'TERRA'
    snames(785)  = 'AQUA'
    snames(786)  = 'AURA'
    snames(787)  = 'C/NOFS'
    snames(788)  = 'CALIPSO'
    snames(789)  = 'CloudSat'
    snames(801)  = 'SUNSAT'
    snames(802)  = 'International Space Station (ISS)'
    snames(811)  = 'COMS-1'
    snames(812)  = 'COMS-2'
    snames(813)  = 'SCISAT-1'
    snames(814)  = 'ODIN'
    snames(821)  = 'SAC-C'
    snames(822)  = 'SAC-D'
    snames(826)  = 'KOMPSAT-5'
    snames(851)  = 'Combination of TERRA and AQUA'
    snames(852)  = 'Combination of NOAA 16 to NOAA 19'
    snames(853)  = 'Combination of METOP-1 to METOP-3'
    snames(854)  = 'Combination of METEOSAT and DMSP'
    snames(855)  = 'Non-specific mixture of geostationary and low earth orbiting satellites'
    snames(856)  = 'Combination of INSAT 3D and INSAT 3DR'
    snames(1024) = 'Missing value'

    ! Loop through local variable

    do i = 1, size(snames)

       ! Check local variable and proceed accordingly

       if(said .eq. saids(i)) then
       
          ! Define local variables

          sname = snames(i)
          goto 1000

       end if ! if(said .eq. saids(i))

    end do ! do i = 1, size(snames)

    ! Define local variables

1000 continue

    ! Deallocate memory for local variables

    if(allocated(snames)) deallocate(snames)
    if(allocated(saids))  deallocate(saids)

    !=====================================================================

  end subroutine ncep_said_table

  !=======================================================================

  ! SUBROUTINE:

  ! read_conv_bufr.f90

  ! DESCRIPTION:

  ! This subroutine parses an external BUFR-formatted file and returns
  ! the observation attributes corresponding to the user-specified
  ! observation type.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * obtsype; a FORTRAN integer specifying the observation type.

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable containing all
  !   attributes (i.e., geographical position, elevation, time, and
  !   subtype if applicable) for the user specified observation type.

  !-----------------------------------------------------------------------

  subroutine read_conv_bufr(filename,obstype,bufr)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr
    character(len=500)                                                  :: filename
    integer                                                             :: obstype

    !=====================================================================

    ! Define local variables

    call conv_bufrio_interface_read(filename,obstype,bufr)

    !=====================================================================
    
  end subroutine read_conv_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! read_json_nems.f90

  ! DESCRIPTION:

  ! This subroutine parses a JSON formatted file and returns a FORTRAN
  ! json_nems_struct variable (json) containing all records within the
  ! user specified file.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   JSON formatted file.

  ! * json; a FORTRAN json_nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * json; a FORTRAN json_nems_struct variable containing the
  !   contents of the user specified JSON formatted file.

  !-----------------------------------------------------------------------

  subroutine read_json_nems(filename,json)

    ! Define variables passed to routine

    type(json_nems_struct),     dimension(:),               allocatable :: json
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call json_interface_read(filename,json)

    !=====================================================================

  end subroutine read_json_nems

  !=======================================================================

  ! SUBROUTINE:

  ! read_list.f90

  ! DESCRIPTION:

  ! This subroutine parses an external file containing a list of files
  ! and returns an array of the file names contained within.

  ! NOTE: The filenames array is allocated by this routine; it is
  ! assumed that this array is deallocated at a higher-level in the
  ! coding structure.

  ! INPUT VARIABLES:

  ! * list_filename; a FORTRAN character string specifying the
  !   full-path to the external file containing the list of file
  !   names.

  ! * filenames; a FORTRAN unallocated array of character strings.

  ! OUTPUT VARIABLES:

  ! * filenames; a FORTRAN array of dimension nfiles containing the
  !   file names within the external file containing the respective
  !   list.

  !-----------------------------------------------------------------------

  subroutine read_list(list_filename,filenames)

    ! Define variables passed to routine

    character(len=500),         dimension(:),               allocatable :: filenames 
    character(len=500)                                                  :: list_filename

    ! Define variables computed within routine

    character(len=500)                                                  :: dummy
    integer                                                             :: nfiles

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    nfiles = 0
    open(99,file=trim(adjustl(list_filename)),form='formatted')
1000 read(99,'(a)',end=1001) dummy
    nfiles = nfiles + 1
    goto 1000
1001 continue
    close(99)
    open(99,file=trim(adjustl(list_filename)),form='formatted')

    ! Allocate memory for local variables

    if(.not. allocated(filenames)) allocate(filenames(nfiles))

    ! Loop through local variable

    do i = 1, nfiles

       ! Define local variables

       read(99,*) filenames(i)

    end do ! do i = 1, nfiles

    ! Define local variables

    close(99)

    !=====================================================================

  end subroutine read_list

  !=======================================================================

  ! SUBROUTINE:

  ! read_nems_init.f90

  ! DESCRIPTION:

  ! This subroutine parses a NCEP NEMS formatted file and returns a
  ! FORTRAN structure (nems) containing the gridded variables.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   NCEP NEMS formatted file.

  ! * nems; a FORTRAN nems_struct variable.

  ! OUTPUT VARIABLES:

  ! * nems; a FORTRAN nems_struct variable containing the gridded NEMS
  !   variables.

  !-----------------------------------------------------------------------

  subroutine read_nems_init(filename,nems)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nems
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables
    
    call nemsio_interface_init(filename,nems)
    call variable_interface_setup_struct(nems)

    !=====================================================================

  end subroutine read_nems_init

  !=======================================================================

  ! SUBROUTINE:

  ! read_nems_var.f90

  ! DESCRIPTION:

  ! This subroutine parses a NCEP NEMS formatted file and returns a
  ! FORTRAN structure (nems) containing the gridded variable.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the path to the
  !   NCEP NEMS formatted file.

  ! * nems; a FORTRAN nems_struct variable.

  ! * varname; a FORTRAN nemsio_charkind variable defining the NEMS
  !   variable name; if the respective variable does not exist within
  !   the NEMS file, the respective FORTRAN nems_struct prfvar
  !   attribute will be assigned a missing value.

  ! OUTPUT VARIABLES:

  ! * nems; a FORTRAN nems_struct variable containing the gridded NEMS
  !   variable.

  !-----------------------------------------------------------------------

  subroutine read_nems_var(filename,nems,varname,is_sfcvar,is_prfvar)

    ! Define variables passed to routine

    type(nems_struct)                                                   :: nems
    character(len=500)                                                  :: filename
    character(nemsio_charkind)                                          :: varname
    logical                                                             :: is_prfvar
    logical                                                             :: is_sfcvar
    
    !=====================================================================

    ! Define local variables

    call nemsio_interface_read(filename,nems,varname,is_sfcvar,is_prfvar)

    !=====================================================================

  end subroutine read_nems_var

  !=======================================================================

  ! SUBROUTINE:

  ! read_sat_bufr.f90

  ! DESCRIPTION:

  ! This subroutine parses an external BUFR-formatted file and returns
  ! the observation attributes for the respective satellite platform.

  ! INPUT VARIABLES:

  ! * filename; a FORTRAN character string specifying the full-path to
  !   the BUFR-formatted file to be parsed.

  ! * bufr; a FORTRAN satbufr_struct variable.

  ! OUTPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable containing all
  !   attributes for the respective satellite platform.

  !-----------------------------------------------------------------------

  subroutine read_sat_bufr(filename,bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    character(len=500)                                                  :: filename

    !=====================================================================

    ! Define local variables

    call sat_bufrio_interface_read(filename,bufr)

    !=====================================================================
    
  end subroutine read_sat_bufr

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_convbufr.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_convbufr(bufr,varinfo)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 1
    varinfo%nvars  = 5
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nrecs'
    dimval(1)  = bufr%nrecs

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'Observation latitude locations.'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'Degrees.'
    varinfo%varname(1)      = 'lats'
    varinfo%varndims(1)     = 1
    varinfo%varnattrs(1)    = 2
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'Observation longitude locations.'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'Degrees.'
    varinfo%varname(2)      = 'lons'
    varinfo%varndims(2)     = 1
    varinfo%varnattrs(2)    = 2
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'Offset-seconds relative to analysis time.'
    varinfo%varattrs(3,2,1) = 'units'
    varinfo%varattrs(3,2,2) = 'Seconds.'
    varinfo%varname(3)      = 'dseconds'
    varinfo%varndims(3)     = 1
    varinfo%varnattrs(3)    = 2
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(1)
    varinfo%varattrs(4,1,1) = 'title'
    varinfo%varattrs(4,1,2) = 'Elevation relative to surface.'
    varinfo%varattrs(4,2,1) = 'units'
    varinfo%varattrs(4,2,2) = 'Meters.'
    varinfo%varname(4)      = 'elev'
    varinfo%varndims(4)     = 1
    varinfo%varnattrs(4)    = 2
    varinfo%vartype(4)      = 'float'
    varinfo%vardimid(4,1)   = dimid(1)
    varinfo%varattrs(5,1,1) = 'title'
    varinfo%varattrs(5,1,2) = 'Observation sub-type.'
    varinfo%varname(5)      = 'subtype'
    varinfo%varndims(5)     = 1
    varinfo%varnattrs(5)    = 1
    varinfo%vartype(5)      = 'float'
    varinfo%vardimid(5,1)   = dimid(1)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_convbufr

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_gsidiagisovar.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_gsidiagisovar(gsidiagisovar,varinfo)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: gsidiagisovar
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 1
    varinfo%nvars  = 8
    varinfo%nattrs = 1
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nobs'
    dimval(1)  = gsidiagisovar%nobs

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'Observation latitude locations.'
    varinfo%varname(1)      = 'lats'
    varinfo%varndims(1)     = 1
    varinfo%varnattrs(1)    = 1
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'Observation longitude locations.'
    varinfo%varname(2)      = 'lons'
    varinfo%varndims(2)     = 1
    varinfo%varnattrs(2)    = 1
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'Observation values.'
    varinfo%varname(3)      = 'obs'
    varinfo%varndims(3)     = 1
    varinfo%varnattrs(3)    = 1
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(1)
    varinfo%varattrs(4,1,1) = 'title'
    varinfo%varattrs(4,1,2) = 'Observation minus background value.'
    varinfo%varname(4)      = 'omf'
    varinfo%varndims(4)     = 1
    varinfo%varnattrs(4)    = 1
    varinfo%vartype(4)      = 'float'
    varinfo%vardimid(4,1)   = dimid(1)
    varinfo%varattrs(5,1,1) = 'title'
    varinfo%varattrs(5,1,2) = 'Observation pressure.'
    varinfo%varname(5)      = 'pres'
    varinfo%varndims(5)     = 1
    varinfo%varnattrs(5)    = 1
    varinfo%vartype(5)      = 'float'
    varinfo%vardimid(5,1)   = dimid(1)
    varinfo%varattrs(6,1,1) = 'title'
    varinfo%varattrs(6,1,2) = 'Observation time.'
    varinfo%varname(6)      = 'time'
    varinfo%varndims(6)     = 1
    varinfo%varnattrs(6)    = 1
    varinfo%vartype(6)      = 'float'
    varinfo%vardimid(6,1)   = dimid(1)
    varinfo%varattrs(7,1,1) = 'title'
    varinfo%varattrs(7,1,2) = 'GSI observation code.'
    varinfo%varname(7)      = 'code'
    varinfo%varndims(7)     = 1
    varinfo%varnattrs(7)    = 1
    varinfo%vartype(7)      = 'float'
    varinfo%vardimid(7,1)   = dimid(1)
    varinfo%varattrs(8,1,1) = 'title'
    varinfo%varattrs(8,1,2) = 'Observation height.'
    varinfo%varname(8)      = 'hgt'
    varinfo%varndims(8)     = 1
    varinfo%varnattrs(8)    = 1
    varinfo%vartype(8)      = 'float'
    varinfo%vardimid(8,1)   = dimid(1)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_gsidiagisovar

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_gsidiagprfvar.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_gsidiagprfvar(gsidiagprfvar,varinfo)

    ! Define variables passed to routine

    type(gsidiagprfvar_struct)                                          :: gsidiagprfvar
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 2
    varinfo%nvars  = 14
    varinfo%nattrs = 1
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nobs'
    dimname(2) = 'nlevs'
    dimval(1)  = gsidiagprfvar%nobs
    dimval(2)  = gsidiagprfvar%nlevs

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1)  = 'title'
    varinfo%varattrs(1,1,2)  = 'Observation latitude locations.'
    varinfo%varname(1)       = 'lats'
    varinfo%varndims(1)      = 2
    varinfo%varnattrs(1)     = 1
    varinfo%vartype(1)       = 'float'
    varinfo%vardimid(1,1)    = dimid(1)
    varinfo%vardimid(1,2)    = dimid(2)
    varinfo%varattrs(2,1,1)  = 'title'
    varinfo%varattrs(2,1,2)  = 'Observation longitude locations.'
    varinfo%varname(2)       = 'lons'
    varinfo%varndims(2)      = 2
    varinfo%varnattrs(2)     = 1
    varinfo%vartype(2)       = 'float'
    varinfo%vardimid(2,1)    = dimid(1)
    varinfo%vardimid(2,2)    = dimid(2)
    varinfo%varattrs(3,1,1)  = 'title'
    varinfo%varattrs(3,1,2)  = 'Observation heights.'
    varinfo%varname(3)       = 'hgt'
    varinfo%varndims(3)      = 2
    varinfo%varnattrs(3)     = 1
    varinfo%vartype(3)       = 'float'
    varinfo%vardimid(3,1)    = dimid(1)
    varinfo%vardimid(3,2)    = dimid(2)

    
    varinfo%varattrs(4,1,1)  = 'title'
    varinfo%varattrs(4,1,2)  = 'Observation values.'
    varinfo%varname(4)       = 'obs'
    varinfo%varndims(4)      = 2
    varinfo%varnattrs(4)     = 1
    varinfo%vartype(4)       = 'float'
    varinfo%vardimid(4,1)    = dimid(1)
    varinfo%vardimid(4,2)    = dimid(2)
    varinfo%varattrs(5,1,1)  = 'title'
    varinfo%varattrs(5,1,2)  = 'Observation minus background value.'
    varinfo%varname(5)       = 'omf'
    varinfo%varndims(5)      = 2
    varinfo%varnattrs(5)     = 1
    varinfo%vartype(5)       = 'float'
    varinfo%vardimid(5,1)    = dimid(1)
    varinfo%vardimid(5,2)    = dimid(2)
    varinfo%varattrs(6,1,1)  = 'title'
    varinfo%varattrs(6,1,2)  = 'Observation pressure.'
    varinfo%varname(6)       = 'pres'
    varinfo%varndims(6)      = 2
    varinfo%varnattrs(6)     = 1
    varinfo%vartype(6)       = 'float'
    varinfo%vardimid(6,1)    = dimid(1)
    varinfo%vardimid(6,2)    = dimid(2)
    varinfo%varattrs(7,1,1)  = 'title'
    varinfo%varattrs(7,1,2)  = 'Observation time.'
    varinfo%varname(7)       = 'time'
    varinfo%varndims(7)      = 2
    varinfo%varnattrs(7)     = 1
    varinfo%vartype(7)       = 'float'
    varinfo%vardimid(7,1)    = dimid(1)
    varinfo%vardimid(7,2)    = dimid(2)
    varinfo%varattrs(8,1,1)  = 'title'
    varinfo%varattrs(8,1,2)  = 'GSI observation code.'
    varinfo%varname(8)       = 'code'
    varinfo%varndims(8)      = 2
    varinfo%varnattrs(8)     = 1
    varinfo%vartype(8)       = 'float'
    varinfo%vardimid(8,1)    = dimid(1)
    varinfo%vardimid(8,2)    = dimid(2)
    varinfo%varattrs(9,1,1)  = 'title'
    varinfo%varattrs(9,1,2)  = 'Observation bias within pressure interval.'
    varinfo%varname(9)       = 'bias'
    varinfo%varndims(9)      = 1
    varinfo%varnattrs(9)     = 1
    varinfo%vartype(9)       = 'float'
    varinfo%vardimid(9,1)    = dimid(2)
    varinfo%varattrs(10,1,1) = 'title'
    varinfo%varattrs(10,1,2) = 'Observation mean within pressure interval.'
    varinfo%varname(10)      = 'mean'
    varinfo%varndims(10)     = 1
    varinfo%varnattrs(10)    = 1
    varinfo%vartype(10)      = 'float'
    varinfo%vardimid(10,1)   = dimid(2)
    varinfo%varattrs(11,1,1) = 'title'
    varinfo%varattrs(11,1,2) = 'Pressure level.'
    varinfo%varname(11)      = 'plevs'
    varinfo%varndims(11)     = 1
    varinfo%varnattrs(11)    = 1
    varinfo%vartype(11)      = 'float'
    varinfo%vardimid(11,1)   = dimid(2)
    varinfo%varattrs(12,1,1) = 'title'
    varinfo%varattrs(12,1,2) = 'Observation root mean-squared error within pressure interval.'
    varinfo%varname(12)      = 'rmse'
    varinfo%varndims(12)     = 1
    varinfo%varnattrs(12)    = 1
    varinfo%vartype(12)      = 'float'
    varinfo%vardimid(12,1)   = dimid(2)
    varinfo%varattrs(13,1,1) = 'title'
    varinfo%varattrs(13,1,2) = 'Observation variance within pressure interval.'
    varinfo%varname(13)      = 'vari'
    varinfo%varndims(13)     = 1
    varinfo%varnattrs(13)    = 1
    varinfo%vartype(13)      = 'float'
    varinfo%vardimid(13,1)   = dimid(2)
    varinfo%varattrs(14,1,1) = 'title'
    varinfo%varattrs(14,1,2) = 'Observation count within pressure interval.'
    varinfo%varname(14)      = 'count'
    varinfo%varndims(14)     = 1
    varinfo%varnattrs(14)    = 1
    varinfo%vartype(14)      = 'float'
    varinfo%vardimid(14,1)   = dimid(2)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_gsidiagprfvar

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_nems2ncdf.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * nemsio; a FORTRAN nems_struct variable.

  ! * json; an array of FORTRAN json_nems_struct variables.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_nems2ncdf(nemsio,json,varinfo)

    ! Define variables passed to routine

    type(json_nems_struct)                                              :: json(:)
    type(nems_struct)                                                   :: nemsio
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid
    integer                                                             :: idx
    integer                                                             :: nsvars

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = 3
    nsvars         = 4 
    varinfo%nvars  = nsvars + size(json)
    varinfo%nattrs = 2
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Define local variables

    dimname(1) = 'nx'
    dimname(2) = 'ny'
    dimname(3) = 'nz'
    dimval(1)  = nemsio%nx
    dimval(2)  = nemsio%ny
    dimval(3)  = nemsio%nz

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    varinfo%varattrs(1,1,1) = 'title'
    varinfo%varattrs(1,1,2) = 'latitude'
    varinfo%varattrs(1,2,1) = 'units'
    varinfo%varattrs(1,2,2) = 'degrees'
    varinfo%varname(1)      = 'lat'
    varinfo%varndims(1)     = 2
    varinfo%varnattrs(1)    = 2
    varinfo%vartype(1)      = 'float'
    varinfo%vardimid(1,1)   = dimid(1)
    varinfo%vardimid(1,2)   = dimid(2)
    varinfo%varattrs(2,1,1) = 'title'
    varinfo%varattrs(2,1,2) = 'longitude'
    varinfo%varattrs(2,2,1) = 'units'
    varinfo%varattrs(2,2,2) = 'degrees'
    varinfo%varname(2)      = 'lon'
    varinfo%varndims(2)     = 2
    varinfo%varnattrs(2)    = 2
    varinfo%vartype(2)      = 'float'
    varinfo%vardimid(2,1)   = dimid(1)
    varinfo%vardimid(2,2)   = dimid(2)
    varinfo%varattrs(3,1,1) = 'title'
    varinfo%varattrs(3,1,2) = 'orography'
    varinfo%varattrs(3,2,1) = 'units'
    varinfo%varattrs(3,2,2) = 'meters'
    varinfo%varname(3)      = 'orog'
    varinfo%varndims(3)     = 2
    varinfo%varnattrs(3)    = 2
    varinfo%vartype(3)      = 'float'
    varinfo%vardimid(3,1)   = dimid(1)
    varinfo%vardimid(3,2)   = dimid(2)
    varinfo%varattrs(4,1,1) = 'title'
    varinfo%varattrs(4,1,2) = 'pressure'
    varinfo%varattrs(4,2,1) = 'units'
    varinfo%varattrs(4,2,2) = 'Pascals'
    varinfo%varname(4)      = 'pres'
    varinfo%varndims(4)     = 3
    varinfo%varnattrs(4)    = 2
    varinfo%vartype(4)      = 'float'
    varinfo%vardimid(4,1)   = dimid(1)
    varinfo%vardimid(4,2)   = dimid(2)
    varinfo%vardimid(4,3)   = dimid(3)
    idx                     = 4
    
    ! Loop through local variable
    
    do i = 1, size(json)

       ! Define local variables
       
       idx                       = idx + 1
       varinfo%varattrs(idx,1,1) = 'title'
       varinfo%varattrs(idx,1,2) = trim(adjustl(json(i)%title))
       varinfo%varattrs(idx,2,1) = 'units'
       varinfo%varattrs(idx,2,2) = trim(adjustl(json(i)%units))
       varinfo%varname(idx)      = trim(adjustl(json(i)%ncdf_name))
       varinfo%varnattrs(idx)    = 2
       varinfo%vartype(idx)      = 'float'

       ! Check local variable and proceed accordingly

       if(json(i)%sfcvar) then

          ! Define local variables

          varinfo%varndims(idx)   = 2
          varinfo%vardimid(idx,1) = dimid(1)
          varinfo%vardimid(idx,2) = dimid(2)

       end if ! if(json(i)%sfcvar)

       ! Check local variable and proceed accordingly

       if(json(i)%prfvar) then

          ! Define local variables

          varinfo%varndims(idx)   = 3
          varinfo%vardimid(idx,1) = dimid(1)
          varinfo%vardimid(idx,2) = dimid(2)
          varinfo%vardimid(idx,3) = dimid(3)

       end if ! if(json(i)%prfvar)

    end do ! do i = 1, size(json)

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_nems2ncdf

  !=======================================================================

  ! SUBROUTINE:

  ! varinfo_satbufr.f90

  ! DESCRIPTION:

  ! This subroutine writes the netcdf directives (e.g., varinfo) for
  ! the user specified netcdf formatted file to be written.

  ! INPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable.

  ! * varinfo; a FORTRAN varinfo_struct variable.

  ! OUTPUT VARIABLES:

  ! * varinfo; a FORTRAN varinfo_struct variable specifying the
  !   appropriate fields for external file formats.

  !-----------------------------------------------------------------------

  subroutine varinfo_satbufr(bufr,varinfo)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr
    type(varinfo_struct)                                                :: varinfo

    ! Define variables computed within routine

    character(len=10),          dimension(:),               allocatable :: dimname
    character(len=100)                                                  :: sname
    character(len=15)                                                   :: varname
    character(len=4)                                                    :: said
    integer,                    dimension(:),               allocatable :: dimval
    integer,                    dimension(:),               allocatable :: dimid
    integer                                                             :: nrecs
    integer                                                             :: isaid

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    varinfo%ndims  = bufr%nsats
    varinfo%nvars  = bufr%nsats*3
    varinfo%nattrs = 3
    if(is_ncep_bufr) varinfo%nattrs = varinfo%nattrs + 1
    call variable_interface_setup_struct(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(dimname)) allocate(dimname(varinfo%ndims))
    if(.not. allocated(dimval))  allocate(dimval(varinfo%ndims))
    if(.not. allocated(dimid))   allocate(dimid(varinfo%ndims))

    ! Loop through local variable

    do i = 1, bufr%nsats

       ! Define local variables

       isaid     = int(bufr%usaids(i))
       write(dimname(i),'(a6,i4.4)') 'nrecs_', isaid 
       dimval(i) = 0

       ! Loop through local variable

       do j = 1, bufr%nrecs

          ! Check local variable and proceed accordingly

          if(bufr%said(j) .eq. bufr%usaids(i)) then

             ! Define local variables

             dimval(i) = dimval(i) + 1

          end if ! if(bufr%said(j) .eq. bufr%usaids(i))

       end do ! do j = 1, bufr%nrecs

    end do ! do i = 1, bufr%nsats

    ! Loop through local variable

    do i = 1, varinfo%ndims

       ! Define local variables

       varinfo%dimval(i)  = dimval(i)
       varinfo%dimname(i) = dimname(i)
       dimid(i)           = i

    end do ! do i = 1, varinfo%ndims

    ! Define local variables

    nrecs = 0

    ! Loop through local variable

    do i = 1, bufr%nsats

       ! Define local variables

       isaid                       = int(bufr%usaids(i))
       if(is_ncep_bufr) call ncep_said_table(isaid,sname)
       write(said,'(i4.4)') isaid
       nrecs                       = nrecs + 1
       write(varname,'(a5,i4.4)') 'lats_', isaid 
       varinfo%varattrs(nrecs,1,1) = 'title'
       varinfo%varattrs(nrecs,1,2) = 'Observation latitude locations.'
       varinfo%varattrs(nrecs,2,1) = 'units'
       varinfo%varattrs(nrecs,2,2) = 'Degrees.'
       varinfo%varattrs(nrecs,3,1) = 'SAID'
       varinfo%varattrs(nrecs,3,2) = said
       if(is_ncep_bufr) then
          varinfo%varattrs(nrecs,4,1) = 'SATELLITE'
          varinfo%varattrs(nrecs,4,2) = trim(adjustl(sname))
       end if ! if(is_ncep_bufr)
       varinfo%varname(nrecs)      = trim(adjustl(varname))
       varinfo%varndims(nrecs)     = 1
       varinfo%varnattrs(nrecs)    = varinfo%nattrs
       varinfo%vartype(nrecs)      = 'float'
       varinfo%vardimid(nrecs,1)   = dimid(i)
       nrecs                       = nrecs + 1
       write(varname,'(a5,i4.4)') 'lons_', isaid 
       varinfo%varattrs(nrecs,1,1) = 'title'
       varinfo%varattrs(nrecs,1,2) = 'Observation longitude locations.'
       varinfo%varattrs(nrecs,2,1) = 'units'
       varinfo%varattrs(nrecs,2,2) = 'Degrees.'
       varinfo%varattrs(nrecs,3,1) = 'SAID'
       varinfo%varattrs(nrecs,3,2) = said
       if(is_ncep_bufr) then
          varinfo%varattrs(nrecs,4,1) = 'SATELLITE'
          varinfo%varattrs(nrecs,4,2) = trim(adjustl(sname))
       end if ! if(is_ncep_bufr)
       varinfo%varname(nrecs)      = trim(adjustl(varname))
       varinfo%varndims(nrecs)     = 1
       varinfo%varnattrs(nrecs)    = varinfo%nattrs
       varinfo%vartype(nrecs)      = 'float'
       varinfo%vardimid(nrecs,1)   = dimid(i)
       nrecs                       = nrecs + 1
       write(varname,'(a9,i4.4)') 'dseconds_', isaid 
       varinfo%varattrs(nrecs,1,1) = 'title'
       varinfo%varattrs(nrecs,1,2) = 'Offset-seconds relative to analysis time.'
       varinfo%varattrs(nrecs,2,1) = 'units'
       varinfo%varattrs(nrecs,2,2) = 'Seconds'
       varinfo%varattrs(nrecs,3,1) = 'SAID'
       varinfo%varattrs(nrecs,3,2) = said
       if(is_ncep_bufr) then
          varinfo%varattrs(nrecs,4,1) = 'SATELLITE'
          varinfo%varattrs(nrecs,4,2) = trim(adjustl(sname))
       end if ! if(is_ncep_bufr)
       varinfo%varname(nrecs)      = trim(adjustl(varname))
       varinfo%varndims(nrecs)     = 1
       varinfo%varnattrs(nrecs)    = varinfo%nattrs
       varinfo%vartype(nrecs)      = 'float'
       varinfo%vardimid(nrecs,1)   = dimid(i)

    end do ! do i = 1, bufr%nsats

    ! Deallocate memory for local variables

    if(allocated(dimname)) deallocate(dimname)
    if(allocated(dimval))  deallocate(dimval)
    if(allocated(dimid))   deallocate(dimid)

    !=====================================================================

  end subroutine varinfo_satbufr

  !=======================================================================

  ! SUBROUTINE:

  ! write_netcdf_convbufr.f90

  ! DESCRIPTION:

  ! This subroutine write the relevant FORTRAN convbufr_struct
  ! variables to Network-Common Data Format (netcdf) file.

  ! INPUT VARIABLES:

  ! * bufr; a FORTRAN convbufr_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_netcdf_convbufr(bufr)

    ! Define variables passed to routine

    type(convbufr_struct)                                               :: bufr

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Define local variables

    write(filename,500) bufr%analdate(1:4), bufr%analdate(6:7),            &
         & bufr%analdate(9:10), bufr%analdate(12:13), int(bufr%obstype)
    call fileio_interface_varinfo(bufr,varinfo)
    call netcdf_interface_open(filename,.false.,.false.,.true.)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'bufr_filename',          &
         & trim(adjustl(bufr%bufr_filename)))
    ncstatus = nf90_put_att(ncfileid,nf90_global,'analysis_time',          &
         & bufr%analdate)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'obstype',                &
         & int(bufr%obstype))
    call netcdf_interface_writedef(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(5,1,varinfo%dimval(1),1))

    ! Define local variables

    work(1,1,:,1) = bufr%lat
    work(2,1,:,1) = bufr%lon
    work(3,1,:,1) = bufr%offset_seconds
    work(4,1,:,1) = bufr%elev
    work(5,1,:,1) = real(bufr%subtype)

    ! Loop through local variable

    do i = 1, size(work(:,1,1,1))

       ! Define local variables

       ncstatus = nf90_put_var(ncfileid,varinfo%varid(i),work(i,1,:,1))

    end do ! do i = 1, size(work(:,1,1,1))

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)

    ! Define local variables
    
    call netcdf_interface_close()

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

500 format('BUFR.',a4,a2,a2,a2,'.obstype_',i3.3,'.nc')

    !=====================================================================

  end subroutine write_netcdf_convbufr

  !=======================================================================

  ! SUBROUTINE:

  ! write_netcdf_gsidiagisovar.f90

  ! DESCRIPTION:

  ! This subroutine write the relevant FORTRAN gsidiagisovar_struct
  ! variables to Network-Common Data Format (netcdf) file.

  ! NOTE: If a previous occurance of the constructed filepath exists,
  ! the construct filepath is appended with a 4-digit integer value;
  ! up to 1000 filename attempts are allowed.

  ! INPUT VARIABLES:

  ! * gsidiagisovar; a FORTRAN gsidiagisovar_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_netcdf_gsidiagisovar(gsidiagisovar)

    ! Define variables passed to routine

    type(gsidiagisovar_struct)                                          :: gsidiagisovar

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    logical                                                             :: exist
    logical                                                             :: exist_chk
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================
    
    ! Check local variable and proceed accordingly

    if(gsidiagisovar%obscode .eq. obscode_spval) then
    
       ! Define local variables

       write(filename,500) trim(adjustl(gsidiagisovar%obstype)),           &
            & gsidiagisovar%analdate

    end if ! if(gsidiagisovar%obscode .eq. obscode_spval)

    ! Check local variable and proceed accordingly

    if(gsidiagisovar%obscode .ne. obscode_spval) then

       ! Define local variables
       
       write(filename,501) trim(adjustl(gsidiagisovar%obstype)),           &
            & gsidiagisovar%obscode, gsidiagisovar%analdate

    end if ! if(gsidiagisovar%obscode .ne. obscode_spval)

    ! Define local variables   
       
    filename = trim(adjustl(datapath))//trim(adjustl(filename))
    inquire(file=trim(adjustl(filename)),exist=exist)

    ! Check local variable and proceed accordingly

    if(exist) then

       ! Loop through local variable

       do i = 1, 1000

          ! Check local variable and proceed accordingly

          if(gsidiagisovar%obscode .eq. obscode_spval) then
             
             ! Define local variables

             write(filename,502) trim(adjustl(gsidiagisovar%obstype)),     &
                  & gsidiagisovar%analdate, i

          end if ! if(gsidiagisovar%obscode .eq. obscode_spval)

          ! Check local variable and proceed accordingly

          if(gsidiagisovar%obscode .ne. obscode_spval) then

             ! Define local variables
          
             write(filename,503) trim(adjustl(gsidiagisovar%obstype)),     &
                  & gsidiagisovar%obscode, gsidiagisovar%analdate, i

          end if ! if(gsidiagisovar%obscode .ne. obscode_spval)
             
          ! Define local variables
             
          filename = trim(adjustl(datapath))//trim(adjustl(filename))
          inquire(file=trim(adjustl(filename)),exist=exist_chk)
          if(.not. exist_chk) goto 1000

       end do ! do i = 1, 1000

    end if ! if(exist)

    ! Define local variables

1000 continue
    call fileio_interface_varinfo(gsidiagisovar,varinfo)
    call netcdf_interface_open(filename,.false.,.false.,.true.)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'gsi_diag_file',          &
         & trim(adjustl(gsidiagisovar%gsidiag_filename)))
    ncstatus = nf90_put_att(ncfileid,nf90_global,'analdate',               &
         & gsidiagisovar%analdate)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'bias',                   &
         & gsidiagisovar%bias)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'mean',                   &
         & gsidiagisovar%mean)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'rmse',                   &
         & gsidiagisovar%rmse)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'vari',                   &
         & gsidiagisovar%vari)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'ncount',                 &
         & real(gsidiagisovar%ncount))
    ncstatus = nf90_put_att(ncfileid,nf90_global,'obstype',                &
         & gsidiagisovar%obstype)

    ! Check local variable and proceed accordingly

    if(is_gsidiags_regional) then

       ! Define local variables

       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_latmax',     &
            & gsidiags_regional_latmax)
       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_latmin',     &
            & gsidiags_regional_latmin)
       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_lonmax',     &
            & gsidiags_regional_lonmax)
       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_lonmin',     &
            & gsidiags_regional_lonmin)

    end if ! if(is_gsidiags_regional)

    ! Define local variables
    
    call netcdf_interface_writedef(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(8,1,varinfo%dimval(1),1))
       
    ! Define local variables
    
    work(1,1,:,1) = gsidiagisovar%lat
    work(2,1,:,1) = gsidiagisovar%lon
    work(3,1,:,1) = gsidiagisovar%obs   
    work(4,1,:,1) = gsidiagisovar%omf
    work(5,1,:,1) = gsidiagisovar%pres
    work(6,1,:,1) = gsidiagisovar%time
    work(7,1,:,1) = real(gsidiagisovar%code)
    work(8,1,:,1) = gsidiagisovar%hgt

    ! Loop through local variable

    do i = 1, size(work(:,1,1,1))

       ! Define local variables

       ncstatus = nf90_put_var(ncfileid,varinfo%varid(i),work(i,1,:,1))

    end do ! do i = 1, size(work(:,1,1,1))

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)

    ! Define local variables
    
    call netcdf_interface_close()

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

500 format('GSIDIAGISOVAR.',a,'.',a10,'.nc')
501 format('GSIDIAGISOVAR.',a,i4.4,'.',a10,'.nc')
502 format('GSIDIAGISOVAR.',a,'.',a10,'.',i4.4,'.nc')
503 format('GSIDIAGISOVAR.',a,i4.4'.',a10,'.',i4.4,'.nc')

    !=====================================================================

  end subroutine write_netcdf_gsidiagisovar

  !=======================================================================

  ! SUBROUTINE:

  ! write_netcdf_gsidiagprfvar.f90

  ! DESCRIPTION:

  ! This subroutine write the relevant FORTRAN gsidiaprfvar_struct
  ! variables to Network-Common Data Format (netcdf) file.

  ! NOTE: If a previous occurance of the constructed filepath exists,
  ! the construct filepath is appended with a 4-digit integer value;
  ! up to 1000 filename attempts are allowed.

  ! INPUT VARIABLES:

  ! * gsidiagprfvar; a FORTRAN gsidiagprfvar_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_netcdf_gsidiagprfvar(gsidiagprfvar)

    ! Define variables passed to routine

    type(gsidiagprfvar_struct)                                          :: gsidiagprfvar

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    logical                                                             :: exist
    logical                                                             :: exist_chk
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: n2dvar

    ! Define counting variables

    integer                                                             :: i

    !=====================================================================

    ! Check local variable and proceed accordingly

    if(gsidiagprfvar%obscode .eq. obscode_spval) then
    
       ! Define local variables

       write(filename,500) trim(adjustl(gsidiagprfvar%obstype)),           &
            & gsidiagprfvar%analdate

    else   ! if(gsidiagprfvar%obscode .eq. obscode_spval)

       write(filename,501) trim(adjustl(gsidiagprfvar%obstype)),           &
            & gsidiagprfvar%obscode, gsidiagprfvar%analdate

    end if ! if(gsidiagprfvar%obscode .eq. obscode_spval)

    ! Define local variables
    
    filename = trim(adjustl(datapath))//trim(adjustl(filename))
    inquire(file=trim(adjustl(filename)),exist=exist)

    ! Check local variable and proceed accordingly

    if(exist) then

       ! Loop through local variable

       do i = 1, 1000

          ! Check local variable and proceed accordingly

          if(gsidiagprfvar%obscode .eq. obscode_spval) then
          
             ! Define local variables

             write(filename,502) trim(adjustl(gsidiagprfvar%obstype)),     &
                  & gsidiagprfvar%analdate, i

          else   ! if(gsidiagprfvar%obscode .eq. obscode_spval)

             ! Define local variables

             write(filename,503) trim(adjustl(gsidiagprfvar%obstype)),     &
                  & gsidiagprfvar%obscode,gsidiagprfvar%analdate, i

          end if ! if(gsidiagprfvar%obscode .eq. obscode_spval)

          ! Define local variables

          filename = trim(adjustl(datapath))//trim(adjustl(filename))
          inquire(file=trim(adjustl(filename)),exist=exist_chk)
          if(.not. exist_chk) goto 1000

       end do ! do i = 1, 1000

    end if ! if(exist)

    ! Define local variables

1000 continue
    call fileio_interface_varinfo(gsidiagprfvar,varinfo)
    call netcdf_interface_open(filename,.false.,.false.,.true.)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'gsi_diag_file',          &
         & trim(adjustl(gsidiagprfvar%gsidiag_filename)))
    ncstatus = nf90_put_att(ncfileid,nf90_global,'analdate',               &
         & gsidiagprfvar%analdate)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'obstype',                &
         & gsidiagprfvar%obstype)

    ! Check local variable and proceed accordingly

    if(is_gsidiags_regional) then

       ! Define local variables

       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_latmax',     &
            & gsidiags_regional_latmax)
       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_latmin',     &
            & gsidiags_regional_latmin)
       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_lonmax',     &
            & gsidiags_regional_lonmax)
       ncstatus = nf90_put_att(ncfileid,nf90_global,'regional_lonmin',     &
            & gsidiags_regional_lonmin)

    end if ! if(is_gsidiags_regional)

    ! Define local variables
    
    call netcdf_interface_writedef(varinfo)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(8,1,varinfo%dimval(1),         &
         & varinfo%dimval(2)))
       
    ! Define local variables
    
    work(1,1,:,:) = gsidiagprfvar%lat
    work(2,1,:,:) = gsidiagprfvar%lon
    work(3,1,:,:) = gsidiagprfvar%hgt
    work(4,1,:,:) = gsidiagprfvar%obs   
    work(5,1,:,:) = gsidiagprfvar%omf
    work(6,1,:,:) = gsidiagprfvar%pres
    work(7,1,:,:) = gsidiagprfvar%time
    work(8,1,:,:) = real(gsidiagprfvar%code)
    n2dvar        = size(work(:,1,1,1))

    ! Loop through local variable

    do i = 1, size(work(:,1,1,1))

       ! Define local variables

       ncstatus = nf90_put_var(ncfileid,varinfo%varid(i),work(i,1,:,:))

    end do ! do i = 1, size(work(:,1,1,1))

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(6,1,1,varinfo%dimval(2)))

    ! Define local variables

    work(1,1,1,:) = gsidiagprfvar%bias
    work(2,1,1,:) = gsidiagprfvar%mean
    work(3,1,1,:) = gsidiagprfvar%plevs
    work(4,1,1,:) = gsidiagprfvar%rmse
    work(5,1,1,:) = gsidiagprfvar%vari
    work(6,1,1,:) = real(gsidiagprfvar%ncount)

    ! Loop through local variable

    do i = 1, size(work(:,1,1,1))

       ! Define local variables

       ncstatus = nf90_put_var(ncfileid,varinfo%varid(n2dvar+i),           &
            & work(i,1,1,:))

    end do ! do i = 1, size(work(:,1,1,1))

    ! Define local variables
    
    call netcdf_interface_close()

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

500 format('GSIDIAGPRFVAR.',a,'.',a10,'.nc')
501 format('GSIDIAGPRFVAR.',a,i4.4,'.',a10,'.nc')
502 format('GSIDIAGPRFVAR.',a,'.',a10,'.',i4.4,'.nc')
503 format('GSIDIAGPRFVAR.',a,i4.4,'.',a10,'.',i4.4,'.nc')

    !=====================================================================

  end subroutine write_netcdf_gsidiagprfvar

  !=======================================================================

  ! SUBROUTINE:

  ! write_netcdf_nems2ncdf.f90

  ! DESCRIPTION:

  ! This subroutine writes the relevant FORTRAN json_nems_struct
  ! variable to a Network-Common Data Format (netcdf) file.

  ! INPUT VARIABLES:

  ! * nemsio; a FORTRAN nemsio_struct variable; it is assumed that all
  !   variable arrays within have been initialized prior to calling
  !   this routine.

  ! * json; a FORTRAN json_nems_struct variable containing all
  !   NEMS-formatted variables to be written to the netcdf file.

  !-----------------------------------------------------------------------

  subroutine write_netcdf_nems2ncdf(nemsio,json)

    ! Define variables passed to routine

    type(json_nems_struct)                                              :: json(:)
    type(nems_struct)                                                   :: nemsio

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    character(len=100)                                                  :: varname
    character(len=19)                                                   :: analdate
    logical                                                             :: exist
    logical                                                             :: exist_chk
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: idx
    integer                                                             :: nsvars

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    write(analdate,500) nemsio%idate(1), nemsio%idate(2), nemsio%idate(3), &
         & nemsio%idate(4), nemsio%idate(5), nemsio%idate(6)
    write(filename,501) analdate
    filename = trim(adjustl(datapath))//trim(adjustl(filename))
    inquire(file=trim(adjustl(filename)),exist=exist)

    ! Check local variable and proceed accordingly

    if(exist) then

       ! Loop through local variable

       do i = 1, 1000

          ! Define local variables

          write(filename,502) analdate, i
          filename = trim(adjustl(datapath))//trim(adjustl(filename))
          inquire(file=trim(adjustl(filename)),exist=exist_chk)
          if(.not. exist_chk) goto 1000

       end do ! do i = 1, 1000

    end if ! if(exist)

    ! Define local variables

1000 continue
    call netcdf_interface_open(filename,.false.,.false.,.true.)
    call fileio_interface_varinfo(nemsio,json,varinfo)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'analdate',               &
         & analdate)
    call netcdf_interface_writedef(varinfo)
    nsvars = 4

    ! Allocate memory for local variables

    if(.not. allocated(work)) allocate(work(1,nemsio%nx,nemsio%ny,1))

    ! Define local variables

    work(1,:,:,1) = reshape(nemsio%lat,shape(work(1,:,:,1)))
    varname       = varinfo%varname(1)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,1))
    work(1,:,:,1) = reshape(nemsio%lon,shape(work(1,:,:,1)))
    varname       = varinfo%varname(2)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,1))
    work(1,:,:,1) = reshape(nemsio%orog,shape(work(1,:,:,1)))
    varname       = varinfo%varname(3)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,1))

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)

    ! Allocate memory for local variables

    if(.not. allocated(work))                                              &
         & allocate(work(1,nemsio%nx,nemsio%ny,nemsio%nz))

    ! Loop through local variable

    do i = 1, nemsio%nz

       ! Define local variables

       work(1,:,:,i) = reshape(nemsio%mdlyprs(:,i),shape(work(1,:,:,i)))

    end do ! do i = 1, nemsio%nz

    ! Define local variables

    varname = varinfo%varname(4)
    call netcdf_interface_putvar(filename,varname,work(1,:,:,:))

    ! Deallocate memory for local variables

    if(allocated(work)) deallocate(work)

    ! Loop through local variable

    do j = 1, size(json)

       ! Define local variables

       idx = nsvars + j

       ! Check local variable and proceed accordingly

       if(json(j)%sfcvar) then

          ! Define local variables

          call nemsio_interface_read(nems_filename,nemsio,                  &
               & json(j)%nems_name,.true.,.false.)

          ! Allocate memory for local variables
          
          if(.not. allocated(work)) allocate(work(1,nemsio%nx,nemsio%ny,1))

          ! Define local variables

          work(1,:,:,1) = reshape(nemsio%sfcvar,shape(work(1,:,:,1)))
          
       end if ! if(json(i)%sfcvar)

       ! Check local variable and proceed accordingly

       if(json(j)%prfvar) then

          ! Define local variables

          call nemsio_interface_read(nems_filename,nemsio,                  &
               & json(j)%nems_name,.false.,.true.)

          ! Allocate memory for local variables
          
          if(.not. allocated(work))                                         &
               & allocate(work(1,nemsio%nx,nemsio%ny,nemsio%nz))

          ! Loop through local variable

          do i = 1, nemsio%nz

             ! Define local variables

             work(1,:,:,i) = reshape(nemsio%prfvar(:,i),                    &
                  & shape(work(1,:,:,i)))

          end do ! do i = 1, nemsio%nz

       end if ! if(json(i)%prfvar)

       ! Define local variables

       varname = varinfo%varname(idx)
       call netcdf_interface_putvar(filename,varname,work(1,:,:,:))

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

    end do ! do j = 1, size(json)

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

    call netcdf_interface_close()
500 format(i4.4,'-',i2.2,'-',i2.2,'_',i2.2,':',i2.2,':',i2.2)
501 format('NEMS2NCDF.',a19,'.nc')
502 format('NEMS2NCDF.',a19,'.',i4.4,'.nc')

    !=====================================================================

  end subroutine write_netcdf_nems2ncdf

  !=======================================================================

  ! SUBROUTINE:

  ! write_netcdf_satbufr.f90

  ! DESCRIPTION:

  ! This subroutine write the relevant FORTRAN satbufr_struct
  ! variables to a Network-Common Data Format (netcdf) file.

  ! INPUT VARIABLES:

  ! * bufr; a FORTRAN satbufr_struct variable.

  !-----------------------------------------------------------------------

  subroutine write_netcdf_satbufr(bufr)

    ! Define variables passed to routine

    type(satbufr_struct)                                                :: bufr

    ! Define variables computed within routine

    type(varinfo_struct)                                                :: varinfo
    character(len=500)                                                  :: filename
    logical                                                             :: exist
    logical                                                             :: exist_chk
    real(r_kind),               dimension(:,:,:,:),         allocatable :: work
    integer                                                             :: nrecs
    integer                                                             :: nvar

    ! Define counting variables

    integer                                                             :: i, j

    !=====================================================================

    ! Define local variables

    write(filename,500) bufr%analdate(1:4), bufr%analdate(6:7),            &
         & bufr%analdate(9:10), bufr%analdate(12:13)
    filename = trim(adjustl(datapath))//trim(adjustl(filename))
    inquire(file=trim(adjustl(filename)),exist=exist)

    ! Check local variable and proceed accordingly

    if(exist) then

       ! Loop through local variable

       do i = 1, 1000

          ! Define local variables

          write(filename,501) bufr%analdate(1:4), bufr%analdate(6:7),      &
               & bufr%analdate(9:10), bufr%analdate(12:13), i
          filename = trim(adjustl(datapath))//trim(adjustl(filename))
          inquire(file=trim(adjustl(filename)),exist=exist_chk)
          if(.not. exist_chk) goto 1000

       end do ! do i = 1, 1000

    end if ! if(exist)

    ! Define local variables

1000 continue
    call fileio_interface_varinfo(bufr,varinfo)
    call netcdf_interface_open(filename,.false.,.false.,.true.)
    ncstatus = nf90_put_att(ncfileid,nf90_global,'bufr_filename',          &
         & trim(adjustl(bufr%bufr_filename)))
    ncstatus = nf90_put_att(ncfileid,nf90_global,'analysis_time',          &
         & bufr%analdate)
    call netcdf_interface_writedef(varinfo)
    nvar     = 0
    
    ! Loop through local variable

    do i = 1, bufr%nsats

       ! Define local variables

       nrecs = 0

       ! Allocate memory for local variables

       if(.not. allocated(work)) allocate(work(3,1,varinfo%dimval(i),1))

       ! Loop through local variable

       do j = 1, bufr%nrecs

          ! Check local variable and proceed accordingly

          if(bufr%said(j) .eq. bufr%usaids(i)) then

             ! Define local variables
             
             nrecs             = nrecs + 1
             work(1,1,nrecs,1) = bufr%lat(j)
             work(2,1,nrecs,1) = bufr%lon(j)
             work(3,1,nrecs,1) = bufr%offset_seconds(j)

          end if ! if(bufr%said(j) .eq. bufr%usaids(i))

       end do ! do j = 1, bufr%nrecs

       ! Loop through local variable

       do j = 1, size(work(:,1,1,1))

          ! Define local variables

          nvar     = nvar + 1
          ncstatus = nf90_put_var(ncfileid,varinfo%varid(nvar),           &
               & work(j,1,:,1))

       end do ! do j = 1, size(work(:,1,1,1))

       ! Deallocate memory for local variables

       if(allocated(work)) deallocate(work)

    end do ! do i = 1, bufr%nsats

    ! Define local variables
    
    call netcdf_interface_close()

    ! Deallocate memory for local variables

    call variable_interface_cleanup_struct(varinfo)

    ! Define local variables

500 format('SATBUFR.',a4,a2,a2,a2,'.nc')
501 format('SATBUFR.',a4,a2,a2,a2,'.',i4.4,'.nc')

    !=====================================================================

  end subroutine write_netcdf_satbufr

  !=======================================================================

end module fileio_interface
