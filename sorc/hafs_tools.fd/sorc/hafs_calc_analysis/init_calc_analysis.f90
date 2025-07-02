!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! module init_calc_analysis
!!        contains subroutines for reading namelist
!!        for calc_analysis utility
!! Original: 2019-09-18   martin   - original module
!!           2019-09-25   martin   - update to allow for netCDF I/O
!!           2019-10-24   martin   - update to support nemsio output
!!           2020-01-17   martin   - parallel IO support added
!!           2024-04-04   martin   - aerosol support added
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module init_calc_analysis
  implicit none
contains
  subroutine read_nml
    !! read in namelist parameters from
    !! calc_analysis.nml file in same directory
    !! as executable
    use vars_calc_analysis, only: anal_file, fcst_file, incr_file, aero_file, use_nemsio_anl, do_aero, fhr, mype, npes, jedi
    implicit none
    ! local variables to this subroutine
    character(len=500) :: datapath = './'
    character(len=500) :: analysis_filename = 'atmanl.nc'
    character(len=500) :: firstguess_filename = 'atmges.nc'
    character(len=500) :: increment_filename = 'atminc.nc'
    character(len=500) :: aero_inc_filename = 'aeroinc.nc'
    character(len=2) :: hrstr
    integer, parameter :: lunit = 10
    logical :: lexist = .false.
    namelist /setup/ datapath, analysis_filename, firstguess_filename, increment_filename, aero_inc_filename, fhr, use_nemsio_anl, do_aero, jedi

    fhr = 6 ! default to 6 hour cycle only
    use_nemsio_anl = .false. ! default to using netCDF for background and analysis
    do_aero = .false. ! default to only do atm, not aero also
    jedi = .false. ! default to GSI (not JEDI)

    ! read in the namelist
    inquire(file='calc_analysis.nml', exist=lexist)
    if ( lexist ) then
      open(file='calc_analysis.nml', unit=lunit, status='old', &
           form='formatted', action='read', access='sequential')
      read(lunit,nml=setup)
      close(lunit)
    else
      write(6,*) 'calc_analysis.nml does not exist and should, ABORT!'
      stop 99
    end if

    if (fhr > 0) then
    write(hrstr,'(I0.2)') fhr
    anal_file = trim(adjustl(datapath)) // '/' // trim(adjustl(analysis_filename)) // '.' // hrstr 
    fcst_file = trim(adjustl(datapath)) // '/' // trim(adjustl(firstguess_filename)) // '.' // hrstr 
    incr_file = trim(adjustl(datapath)) // '/' // trim(adjustl(increment_filename)) // '.' // hrstr
    aero_file = trim(adjustl(datapath)) // '/' // trim(adjustl(aero_inc_filename)) // '.' // hrstr
    else
    anal_file = trim(adjustl(datapath)) // '/' // trim(adjustl(analysis_filename)) 
    fcst_file = trim(adjustl(datapath)) // '/' // trim(adjustl(firstguess_filename)) 
    incr_file = trim(adjustl(datapath)) // '/' // trim(adjustl(increment_filename))
    aero_file = trim(adjustl(datapath)) // '/' // trim(adjustl(aero_inc_filename))
    endif

    if (mype == 0) then
      write(6,*) 'Analysis File    = ', trim(anal_file)
      write(6,*) 'First Guess File = ', trim(fcst_file)
      write(6,*) 'Increment File   = ', trim(incr_file) 
      if (do_aero) then
        write(6,*) 'Aerosol Increment File   = ', trim(aero_file)
      end if
      if (fhr > 0) write(6,*) 'Forecast Hour    = ', fhr
      write(6,*) 'Number of PEs    = ', npes
      write(6,*) 'input guess file and increment file should be in netCDF format'
      if (use_nemsio_anl) then
        write(6,*) 'writing analysis in NEMSIO format'
      else
        write(6,*) 'writing analysis in netCDF format'
      end if
      write(6,*) 'Use JEDI format  = ', jedi
    end if
    
  end subroutine read_nml
end module init_calc_analysis
