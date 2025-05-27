  program hafs_datool

!=========================================================================
! HAFS DA tool
! authors and history:
!      -- 202102, created by Yonghui Weng
!      -- 202112, added HAVSVI pre- and post-processing by Yonghui Weng
!      -- 202206, added MPI and openMP by Yonghui Weng
!      -- 202306, added vi_cloud by JungHoon Shin
!      -- 202404, added ideal vortex by Weiguo Wang
!      -- 202410, added fftw_increment by JungHoon Shin, Xu Lu and Yonghui Weng
!      -- 202411, added u_update_ua and ideal_vortex by Yonghui Weng
!------------------------------------------------------------------------------
!
! command convention
!  hafs_datool.x FUNCTION --in_file=input_files \
!                         --in_grid=input_grids_file \
!                         --in_format=restart \
!                         --out_grid=output_grid_file
!
! Usage and Examples:
!
! 1) FUNCTIONs:
!    remap        : remap one grid file to another grid file
!    vortexreplace: cut an area around vortex from in file to replace the area in the dst file.
!    vi_preproc   : read restart files and generate VI input
!    vi_postproc  : put VI result to restart files
!    fftw_iau     : calculate DA increments, and run FFTW filter
!    u_update_ua  : update ua/va variables with u/v values
!    ua_update_u  : update u/v variables with ua/va values
!    ideal_vortex : update restart files to generate ideal vortex
!
!
! 2) Arguments
!    --in_dir: input data folder
!    --in_file: inout file[s], if have multi files, use ":" to seperate them
!                     if no dir info, then find the file[s] in in_dir folder;
!    --in_grid: input_grids_file, for FV3, it may be grid_spec.nc
!                     if no this argument, then find grid information from in_file.
!
! 3) Usage
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
!                                 [--vi_cloud=vi_cloud ] \
!                                 [--tc_date=tcvital_date] [--res=deg ] \
!                                 [--out_file=output_bin_file or nc_file]
!           : generate VI input file out_file from the restart files in HAFS_restart_folder.
!
!    3.4) vi_postproc
!       * hafs_datool.x hafsvi_postproc --in_file=[hafs_vi rot-ll bin file] \
!                                       --out_dir=[hafs-restart subfolder]  \
!                                       --vi_cloud=vi_cloud  \
!                                       --infile_date=20200825.180000
!           : update restart files in out_dir with VI result.
!
!    3.5) fftw_iau
!       * hafs_datool.x fftw_iau --an_file=analysis_file --bg_file=background_file \
!                                [--infile_date=${ymd}.${hh}0000] \
!                                --in_grid=${in_dir}/grid_spec.nc \
!                                [--vortexposition=user_define_vortex_file --tcvital=tcvitalfile \
!                                  --besttrack=bdeckfile ] \
!                                [--out_file=out_increment_file] \
!                                [--wave_num]
!           : calculate increment ( an_file - bg_file ), and apply FFTW if abs(wave_num)<99, then
!             output increment if out_file, otherwise update an_file
!             so out_file or abs(wave_num)<99 is needed, otherwise do nothing.
!           : examples
!             -- hafs_datool.x fftw_iau --an_file --bg_file --in_grid --out_file=outfile
!                             :just output increments to outfile
!             -- hafs_datool.x fftw_iau --an_file --bg_file --in_grid --tcvital --wave_num=-5
!                             :update an_file with the 5th wave_num increments around tcvital
!             -- hafs_datool.x fftw_iau --an_file --bg_file --in_grid --tcvital --wave_num=5
!                             :update an_file with the cut of sum of the 0-5 wave numbers increments around tcvital
!             -- hafs_datool.x fftw_iau --an_file --bg_file --in_grid --tcvital --wave_num=5 --out_file=outfile
!                             :output the increment to outfile with the cut of sum of the 0-5 wave numbers increments around tcvital
!
!    3.6) u_update_ua and ua_update_u
!       * hafs_datool.x u_update_ua --in_grid=${in_dir}/grid_mspec.nest02_2024_06_30_18.tile2.nc \
!                                   --in_file=${in_dir}/20240630.180000.fv_core.res.nest02.tile2.nc
!       * hafs_datool.x ua_update_u --in_grid=${in_dir}/grid_mspec.nest02_2024_06_30_18.tile2.nc \
!                                   --in_file=${in_dir}/20240630.180000.fv_core.res.nest02.tile2.nc
!
!    3.7) ideal_vortex
!       * hafs_datool.x ideal_vortex --in_dir=HAFS_restart_folder --infile_date=20200825.180000 \
!                                    --besttrack=bdeckfile ] [--vortexradius=deg ] \
!                                   [--nestdoms=nestdoms ]
!         ideal_vortex steps: 1) copy the restart files to a new folder
!                             2) call above command to overwite these files
!       * hafs_datool.x change_sfc_data --in_dir=HAFS_chgres_dir --in_file=sfc_data.tile7.nc
!         change_sfc_data steps: 1) copy in_file to a backup
!                                2) call above command to overwite
!
!=========================================================================
  use module_mpi
  use var_type
  use netcdf

  implicit none

  !----parameter define
  integer              :: i, j, k, n, iind, iargc, rcode, ks, ke, nestdoms, wave_num
  character (len=2500) :: actions, arg, arg1
  character (len=2500) :: in_dir='w', in_file='w', in_grid='w', &
                          vortex_position_file='w', tcvital_file='w', besttrackfile='w', &
                          out_dir='w', out_grid='w', out_data='w', out_file='w', infile_date='w', &
                          in_dir2='w', an_file='w', bg_file='w', vars='all'
  character (len=50  ) :: vortexradius='w'  ! for vortexreplace, vortexradius=600:900 km
                                            ! for hafsvi_preproc, vortexradius=30 deg or 45 deg
  character (len=50  ) :: relaxzone=''      !
  character (len=50  ) :: tc_date='w'       !
  character (len=50  ) :: res='w'           !
  character (len=50  ) :: debug_levelc=''   !
  character (len=50  ) :: interpolation_pointsc='' !
  character (len=50  ) :: nestdomsc=''      ! number for nest domains, 1-30, 1=nest02.tile2
                                            ! in vi_preproc, combine all domains and output to one rot-ll grid.
  character (len=50  ) :: vi_cloud='w'      ! Cloud remapping for VI, 1:GFDL, 2:Thompson & 0: No cloud remapping
  character (len=5   ) :: wave_numc='w'     ! Fourier wave decomposition for DA increment of IAU
                                            ! valid value -99:99, turn off abs(wave_numc)>99
                                            ! positive value means accumulated, i.e.,
                                            ! wave_numc=5: means accumulates 0-5 wave numbers, while
                                            ! wave_numc=-5: only the 5th wave number.

  real, dimension(3)   :: center
!----------------------------------------------------------------
! 0 --- initialization
! Initialize parallel stuff
  call parallel_start()

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
               case ('--in_dir2');        in_dir2=arg(j+1:n)
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
               case ('--debug_level');    debug_levelc=arg(j+1:n)
               case ('--interpolation_points'); interpolation_pointsc=arg(j+1:n)  !
               case ('--nestdoms');       nestdomsc=arg(j+1:n)
               case ('--vi_cloud');       vi_cloud=arg(j+1:n)
               case ('--an_file');        an_file=arg(j+1:n)
               case ('--bg_file');        bg_file=arg(j+1:n)
               case ('--wave_num');       wave_numc=arg(j+1:n)
               case ('--vars');           vars=arg(j+1:n)
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

  wave_num=-999; if (len_trim(wave_numc) > 0 .and. trim(wave_numc) .ne. "w") read(wave_numc,*)wave_num

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
  if ( trim(actions) == "vortexreplace" .or. trim(actions) == "hafsvi_preproc" .or. trim(actions) == "fftw_iau" ) then
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
     if ( index(trim(out_file),'.nc') > 1 ) then
        call hafsvi_preproc_nc(trim(in_dir), trim(infile_date), nestdoms, trim(vortexradius), trim(res), trim(out_file), &
        trim(vi_cloud))
     else
        call hafsvi_preproc(trim(in_dir), trim(infile_date), nestdoms, trim(vortexradius), trim(res), trim(out_file), &
        trim(vi_cloud))
     endif
  endif

  if ( trim(actions) == "hafsvi_postproc" ) then
     write(*,'(a)')' --- call hafsvi_postproc/hafs_datool for '//trim(in_file)
     call hafsvi_postproc(trim(in_file), trim(infile_date), trim(out_dir), nestdoms, trim(vi_cloud))
  endif

!----------------------------------------------------------------
! 5.0 --- fftw_iau
  if ( trim(actions) == "fftw_iau" ) then
     write(*,'(a)')' --- call hafsfftw_iau/hafs_datool for '//trim(an_file)
     call hafsfftw_iau(trim(an_file),trim(in_grid),trim(bg_file),trim(out_file),wave_num,trim(vars))
  endif

!----------------------------------------------------------------
! 6.0 --- u_update_ua & ua_update_u
  if ( trim(actions) == "u_update_ua" .or. trim(actions) == "ua_update_u" ) then
     write(*,'(a)')' --- call u_ua_update//hafs_datool for '//trim(in_file)
     if ( my_proc_id == 0 ) call hafs_u_ua(trim(actions),trim(in_grid),trim(in_file),trim(out_file))
  endif

!----------------------------------------------------------------
! 7.0 --- HAFS ideal_vortex
  if ( trim(actions) == "ideal_vortex" ) then
     write(*,'(a)')' --- call ideal_vortex for '//trim(in_dir)
     if ( my_proc_id == 0 ) call hafs_ideal_vortex(trim(in_dir), trim(infile_date), nestdoms)
  endif
  !  --- HAFS ideal_sfc_data
  if ( trim(actions) == "change_sfc_data" ) then
     write(*,'(a)')' --- call ideal_vortex for '//trim(in_file)
     if ( my_proc_id == 0 ) call hafs_ideal_sfc_data(trim(in_file))
  endif

!----------------------------------------------------------------
  call parallel_finish()

  if ( trim(actions) == "hafsvi_postproc" ) then
     write(*,'(a)')' === finished '//trim(actions)//' '//trim(out_dir)//' for nestdoms '//trim(nestdomsc)//' ==='
  else
     write(*,'(a)')' === finished '//trim(actions)//' '//trim(out_file)//' ==='
  endif

  end program
