  program hafs_datool

!=========================================================================
! HAFS DA tool
! Yonghui Weng, 20210201

! command convention
!  hafs_datool.x FUNCTION --in_file=input_files \
!                         --in_grid=input_grids_file \
!                         --in_format=restart \
!                         --out_grid=output_grid_file 
!
! Usage and Examples:
!
! 1) FUNCTIONs:
!    remap: remap one grid file to another grid file
!    vortexreplace: cut an area around vortex from in file to replace the area in the dst file.
!    file_merge: merge nc files
!    domain_merge: merge different domain/coverage/h-resolution files into one specificed grid
!
!
! 2) Arguments
!    --in_dir: input data folder
!    --in_file: inout file[s], if have multi files, use ":" to seperate them
!                     if no dir info, then find the file[s] in in_dir folder;
!    --in_grid: input_grids_file, for FV3, it may be grid_spec.nc
!                     if no this argument, then find grid information from in_file.
! 
! 3) Examples
!    3.1) remap
!       * hafs_datool.x remap --in_file=${in_dir}/20190829.060000.phy_data.nc \
!                             --in_grid=${in_dir}/grid_spec.nc \
!                             --out_grid=${out_dir}/grid_spec.nc 
!           : interpolate 20190829.060000.phy_data.nc to grid_spec.nc grids, if grid_spec.nc 
!             domain is bigger than input, then fill missing values.
!       * mpirun -np 32 hafs_datool.x remap \
!                             --in_file=${in_dir}/20190829.060000.phy_data.nc \
!                             --in_grid=${in_dir}/grid_spec.nc \
!                             --out_grid=${out_dir}/grid_spec.nc \
!                             --out_data=${out_dir}/20190829.060000.phy_data.nc \
!                             --out_file=${out_dir}/20190829.060000.phy_data.nc_merged
!           : merge in_file and out_data to out_grid grids, and output the file as out_file.
!
!    3.2) vortexreplace
!       * hafs_datool.x vortexreplace --in_grid=${in_dir}/grid_spec.nc \
!                             --vortexposition=${user_define_vortex_position_file} \
!                            [--vortexradius=600:1200 ] \
!                            [--tcvital=${tcvital_file} ] \
!                            [--besttrack=${besttrackfile} ] \
!                             --in_file=${in_dir}/20190829.060000.phy_data.nc \
!                             --out_grid=${out_dir}/grid_spec.nc \
!                             --out_file=20190829.060000.phy_data.nc
!
!    3.3) vi_preproc
!       * hafs_datool.x hafsvi_preproc --in_dir=HAFS_restart_folder --infile_date=20200825.180000 \
!                                 [--vortexposition=user_define_vortex_file --tcvital=tcvitalfile \
!                                  --besttrack=bdeckfile ] [--vortexradius=deg ] \
!                                 [--nestdoms=nestdoms ] \ 
!                                 [ --tc_date=tcvital_date] [--res=deg ] \
!                                 [--out_file=output_bin_file]
!
!    3.4) vi_postproc
!       * hafs_datool.x hafsvi_postproc --in_file=[hafs_vi rot-ll bin file] \
!                                       --out_dir=[hafs-restart subfolder]  \
!                                       --infile_date=20200825.180000
!=========================================================================
  use module_mpi
  use var_type
  use netcdf
  
  implicit none

  !----parameter define
  integer              :: i, j, k, n, iind, iargc, rcode, ks, ke, nestdoms
  character (len=2500) :: actions, arg, arg1
  character (len=2500) :: in_dir='w', in_file='w', in_grid='w', &
                          vortex_position_file='w', tcvital_file='w', besttrackfile='w', &
                          out_dir='w', out_grid='w', out_data='w', out_file='w', infile_date='w'
  character (len=50  ) :: vortexradius='w'  ! for vortexreplace, vortexradius=600:900 km 
                                            ! for hafsvi_preproc, vortexradius=30 deg or 45 deg
  character (len=50  ) :: relaxzone=''      ! 
  character (len=50  ) :: tc_date='w'       !
  character (len=50  ) :: res='w'           !
  character (len=50  ) :: debug_levelc=''   !
  character (len=50  ) :: interpolation_pointsc='' !
  character (len=50  ) :: nestdomsc=''      ! number for nest domains, 1-30, 1=nest02.tile2 
                                            ! in vi_preproc, combine all domains and output to one rot-ll grid.

  real, dimension(3)   :: center
!----------------------------------------------------------------
! 0 --- initialization
! Initialize parallel stuff
!  call parallel_start()

!----------------------------------------------------------------
! 1 --- argc and usage
! 1.1 --- get argc
  if (iargc() .lt. 2) then
     write(*,*)' usage: hafs_datool.x function --in_file=inputfile'
     stop
  else
     call getarg(1, actions)

     do i = 2, iargc()
        call getarg(i, arg)
        j=index(trim(arg),'=',.true.)
        n=len_trim(arg)
        select case (arg(1:j-1))
               case ('--in_dir');         in_dir=arg(j+1:n)
               case ('-i', '--in_file');  in_file=arg(j+1:n)
               case ('--in_grid');        in_grid=arg(j+1:n)
               case ('--out_dir');        out_dir=arg(j+1:n)
               case ('--out_grid');       out_grid=arg(j+1:n)
               case ('--out_data');       out_data=arg(j+1:n)
               case ('--out_file');       out_file=arg(j+1:n)
               case ('--vortexposition'); vortex_position_file=arg(j+1:n)
               case ('--tcvital');        tcvital_file=arg(j+1:n)
               case ('--besttrack');      besttrackfile=arg(j+1:n)
               case ('--vortexradius');   vortexradius=arg(j+1:n)
               case ('--infile_date');    infile_date=arg(j+1:n)  !20210312.0930  
               case ('--relaxzone');      relaxzone=arg(j+1:n)
               case ('--tc_date');        tc_date=arg(j+1:n)  !20210312.0930  
               case ('--res');            res=arg(j+1:n)  !0.02
               case ('--debug_level');    debug_levelc=arg(j+1:n)  !
               case ('--interpolation_points'); interpolation_pointsc=arg(j+1:n)  !
               case ('--nestdoms');       nestdomsc=arg(j+1:n)  !
        end select
     enddo
  endif

!----------------------------------------------------------------
! 2 --- process args and initialization
  tc%lat=-9999.0; tc%lon=-9999.0; tc%pmin=-9999.0; tc%vmax=-9999.0; tc%vortexreplace_r(1:2)=-9999.0

! 2.1 --- relaxzone, debug_level, interpolation_points
  gwt%relaxzone=-99; if (len_trim(relaxzone) > 0 ) read(relaxzone,*)gwt%relaxzone

  if (len_trim(debug_levelc) > 1 .and. trim(debug_levelc) .ne. "w") read(debug_levelc,*)debug_level
  if ( debug_level < 0 .or. debug_level > 999999 ) debug_level = 1

  if (len_trim(interpolation_pointsc) > 0 .and. trim(interpolation_pointsc) .ne. "w") read(interpolation_pointsc,*)gwt%max_points
  if ( gwt%max_points > 9999 .or. gwt%max_points < 1 ) gwt%max_points=4

  nestdoms=0; if (len_trim(nestdomsc) > 0 ) read(nestdomsc,*)nestdoms
  if ( nestdoms > 30 .or. nestdoms < 0 ) nestdoms=0

! 2.2 --- tc info requirement
  if ( trim(actions) == "vortexreplace" .or. trim(actions) == "hafsvi_preproc" ) then
     if ( trim(vortex_position_file) == "w" .and. trim(tcvital_file) == "w" .and. trim(besttrackfile) == "w" ) then
        write(*,'(a)')' vortexreplace and hafsvi_preproc functions require at least one vortex information, '
        write(*,'(a)')' please add one of the following arg:'
        write(*,'(a)')'  --vortexposition=user_define_vortex_position_file.txt or '
        write(*,'(a)')'  --tcvital=TCvital_file or '
        write(*,'(a)')'  --besttrack=best-track-file'
        stop
     endif
     if ( len_trim(infile_date) < 2 .and. len_trim(tc_date) < 2 .and. trim(vortex_position_file) == "w" ) then
        write(*,'(a)')' vortexreplace and hafsvi_preproc functions require date information when read tcvital or besttrack , '
        write(*,'(a)')' please add one of the following arg:'
        write(*,'(a)')'  --infile_date=input_file_date or/and'
        write(*,'(a)')'  --tc_date=tc_date'
        stop
     endif
  endif

  if ( trim(actions) == "hafsvi_preproc" ) then
     if ( trim(infile_date) == "w" ) then
        write(*,'(a)')' hafsvi_preproc function requires input file date for filename'
        write(*,'(a)')' please add one of the following arg:'
        write(*,'(a)')'  --infile_date=input_file_date'
        stop
     endif
  endif 
  if ( trim(tc_date) == "w" .and. len_trim(infile_date) > 1 ) then
     tc_date=trim(infile_date)
  endif

! 2.3 --- tc/vortex info
  if ( trim(actions) == "vortexreplace" ) then
     tc%vortexrep=1
  else
     tc%vortexrep=0
  endif
  if ( trim(actions) == "vortexreplace" .or. trim(actions) == "hafsvi_preproc" ) then
     call get_tc_info(trim(vortex_position_file), trim(tcvital_file), trim(besttrackfile), trim(tc_date), &
                   trim(vortexradius))
  endif

!----------------------------------------------------------------
! 3.0 --- remap
  if ( trim(actions) == "remap" .or. trim(actions) == "vortexreplace" ) then
     write(*,'(a)')' --- call hafs_remap for '//trim(in_grid)//' and '//trim(out_grid)
     call hafs_remap(trim(in_dir), trim(in_grid), trim(in_file), trim(out_dir), trim(out_grid), trim(out_data), trim(out_file))
  endif

!----------------------------------------------------------------
! 4.0 --- HAFS VI
  if ( trim(actions) == "hafsvi_preproc" ) then
     write(*,'(a)')' --- call hafsvi_preproc/hafs_datool for '//trim(in_grid)
     call hafsvi_preproc(trim(in_dir), trim(infile_date), nestdoms, trim(vortexradius), trim(res), trim(out_file))
  endif

  if ( trim(actions) == "hafsvi_postproc" ) then
     write(*,'(a)')' --- call hafsvi_postproc/hafs_datool for '//trim(in_file)
     call hafsvi_postproc(trim(in_file), trim(infile_date), trim(out_dir), nestdoms)
  endif

!----------------------------------------------------------------
!  call parallel_finish()

  end program
