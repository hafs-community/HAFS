      subroutine grib_maps(namec,lflag,
     $     table,parms9,parms10,increment,multiplicand,additive,cell_no)

c-test      program see_trim
      implicit none
      character*(*) namec
      integer l,table,parms9,parms10,lflag
      real    increment,multiplicand,additive,cell_no
      character*5 short_name
c-test      namec='boloni'

c-test      do while (namec(1:1).ne.' ')
c-test	read(5,'(a)')namec
      cell_no=0.
      short_name='none'
      l=len_trim(namec)
      if(namec.eq.'bathymetry'               ) then
        short_name='DBSS'
	increment=0.1
        multiplicand=1.0
        additive=0.0
        table=129
        parms9=195
        parms10=9
      elseif(namec.eq.'surface_heat_flux'        ) then
        short_name='SFHFL'
	increment=0.01
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=171
        parms10=1
      elseif(namec.eq.'surface_evaporation_precipitation') then
        short_name='EMNP'
	increment=0.1
        multiplicand=1
        additive=0.0
        table=128
        parms9=188
        parms10=1
      elseif(namec.eq.'evaporative_heat_flux'        ) then
        short_name='LHTFL'
	increment=0.01
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=121
        parms10=1
      elseif(namec.eq.'sensible_heat_flux'  ) then
        short_name='SHTFL'
	increment=0.01
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=122
        parms10=1
      elseif(namec.eq.'assimilation_heat_flux'  ) then
        short_name='ASHFL'
	increment=1.0
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=180
        parms10=1
      elseif(namec.eq.'surface_temperature_trend'  ) then
        short_name='sstt'
	increment=0.0001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=193
        parms10=1
      elseif(namec.eq.'surface_salinity_trend'   ) then
        short_name='ssst'
	increment=0.0001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=194
        parms10=1
      elseif(namec.eq.'ice_coverage'             ) then
        short_name='ICEC'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=91
        parms10=1
      elseif(namec.eq.'ice_thickness'            ) then
        short_name='ICETK'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=92
        parms10=1
      elseif(namec.eq.'ice_temperature'           ) then
        short_name='WTMPC'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=11
        parms10=1
      elseif(namec.eq.'montgomery_potential_surf'       ) then
        short_name='MNTSF'
	increment=0.001  ! changed from 0.1
        multiplicand=1   ! changed from 0.01
        additive=0.0
        table=2
        parms9=37
        parms10=1
      elseif(namec.eq.'sea_surface_height'       ) then
        short_name='SSHG'
	increment=0.001
        multiplicand=0.01
        additive=0.0
        table=129
        parms9=198
        parms10=1
Cdhi
c       table=128
c       parms9=130
c       parms10=102
Cdhi
      elseif(namec.eq.'barotropic_kinetic_energy/mass'  ) then
        short_name='keng'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=190
        parms10=201
      elseif(namec.eq.'barotropic_streamfunction'  ) then
        short_name='STRM'
	increment=0.1
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=35 
        parms10=201
      elseif(namec.eq.'mixed_layer_u_velocity'   ) then
        short_name='UMIX'
	increment=0.001
        multiplicand=0.01 !cm/s to m/s
        additive=0.0
        table=128
        parms9=178
        parms10=240      !
      elseif(namec.eq.'mixed_layer_v_velocity'   ) then
        short_name='VMIX'
	increment=0.001
        multiplicand=0.01
        additive=0.0
        table=128
        parms9=179
        parms10=240
      elseif(namec.eq.'mixed_layer_speed'        ) then
        short_name='SPC'
	increment=0.001
        multiplicand=0.01 ! cm/s to m/s
        additive=0.0
        table=2
        parms9=48
        parms10=240
      elseif(namec.eq.'surface_boundary_layer_thickness'  ) then
        short_name='MIXHT'
	increment=0.1
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=67 
        parms10=240
      elseif(namec.eq.'mixed_layer_thickness'    ) then
        short_name='MIXHT'
	increment=0.1
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=67
        parms10=1
      elseif(namec.eq.'mixed_layer_temperature'  ) then
        short_name='TMIX'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=186
        parms10=240
      elseif(namec.eq.'mixed_layer_salinity'     ) then
        short_name='SMIX'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=187
        parms10=240
      elseif(namec.eq.'mixed_layer_density'      ) then
        short_name='THMIX'
	increment=0.001
        multiplicand=1.0
        additive=1000.
        table=128
        parms9=177
        parms10=240
      elseif(namec.eq.'barotropic_u_velocity '   ) then
        short_name='UBARO'
	increment=0.001
        multiplicand=0.01
        additive=0.0
        table=128
        parms9=183
        parms10=201
      elseif(namec.eq.'barotropic_v_velocity '   ) then
        short_name='VBARO'
	increment=0.001
        multiplicand=0.01
        additive=0.0
        table=128
        parms9=184
        parms10=201
      elseif(namec.eq.'mixed_layer_kinetic_energy/mass'  ) then
        short_name='keng'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=190
        parms10=240
      elseif(namec.eq.  'u_velocity'               ) then
        short_name='UOGRD'
	increment=0.001
        multiplicand=0.01
        additive=0.0
        if (lflag.eq.0) then
           table=2
           parms9=49
           parms10=110
           cell_no=1.
        else
           table=2
           parms9=49
           parms10=160
        endif
      elseif(namec.eq.  'v_velocity'               ) then
        short_name='VOGRD'
	increment=0.001
        multiplicand=0.01
        additive=0.0
        if (lflag.eq.0) then
           table=2
           parms9=50
           parms10=110
           cell_no=2.
        else
           table=2
           parms9=50
           parms10=160
        endif
      elseif(namec.eq.  'speed'                    ) then
        short_name='SPC'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        if (lflag.eq.0) then
           table=2
           parms9=48
           parms10=110
        else
           table=2
           parms9=48
           parms10=160
        endif
      elseif(namec.eq.  'interface_vertical_velocity' ) then
        short_name='DZDT'
	increment=1.e-6
        multiplicand=1.e-6/86400. ! from m/d to m/s
        additive=0.0
        if (lflag.eq.0) then
          table=2
          parms9=40
          parms10=109
        else
          table=2
          parms9=40
          parms10=160
        endiF
      elseif(namec.eq.  'interface_depth'          ) then
c are identical do not use lflag
        short_name='INTFD'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=185
        parms10=109
      elseif(namec.eq.  'layer_thickness'          ) then
        short_name='layth'
	increment=0.01
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=192
        parms10=110   
      elseif(namec.eq.  'layer_temperature'        ) then
        short_name='WTMPC'
	increment=0.001
        multiplicand=1.0
        additive=0.0    
        table=128
        parms9=186
        parms10=110
      elseif(namec.eq.  'layer_salinity'           ) then
        short_name='SALIN'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=187
        parms10=110
      elseif(namec.eq.  'layer_density'            ) then
        short_name='DEN'
	increment=0.001
        multiplicand=1.0
        additive=1000.0
        table=2
        parms9=89
        parms10=110
      elseif(namec.eq.  'layer_kinetic_energy/mass'  ) then
        short_name='keng'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=190
        parms10=110
      elseif(namec.eq.  'layer_streamfunction'     ) then
        short_name='STRM'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=35
        parms10=110
      elseif(namec.eq.  'w_velocity'     ) then
        short_name='DZDT'
	increment=1.e-6
c-- from m/day to m/s
        multiplicand=1.0/86400.0 ! from m/d to m/s
        additive=0.0
        table=2  
        parms9=40
        parms10=160
      elseif(namec.eq.  'temperature'     ) then
        short_name='WTMPC'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=186
        parms10=160
      elseif(namec.eq.  'salinity'     ) then
        short_name='SALIN'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=187
        parms10=160
      elseif(namec.eq.  'density'     ) then
        short_name='DEN'
	increment=0.001
        multiplicand=1.0
        additive=1000.0
        table=2
        parms9=89
        parms10=160
      elseif(namec.eq.  'kinetic_energy/mass'     ) then
        short_name='keng'
	increment=0.001
        multiplicand=1.0
        additive=0.0
        table=128
        parms9=190
        parms10=160
      elseif(namec.eq.  'stress_x '     ) then
        short_name='U FLX'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=124
        parms10=1
      elseif(namec.eq.  'stress_y '     ) then
        short_name='V FLX'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=125
        parms10=1
      elseif(namec.eq.  'precip '     ) then
        short_name='PRATE'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=59
        parms10=1
      elseif(namec.eq.  'rad-flux '     ) then
        short_name='G RAD'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=117
        parms10=1
      elseif(namec.eq.  'swr-flux '     ) then
        short_name='NSWRS'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=111
        parms10=1
      elseif(namec.eq.  'atm-pres '     ) then
        short_name='MSL'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=1013.0
        table=2
        parms9=128
        parms10=1
      elseif(namec.eq.  'sns-flux '     ) then
        short_name='SHTFL'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=122
        parms10=1
      elseif(namec.eq.  'lat-flux '     ) then
        short_name='LHTFL'
	increment=0.01
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=121
        parms10=1
      elseif(namec.eq.  'gfs_strx '     ) then
        short_name='U FLX'
        increment=0.01
        increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=124
        parms10=1
      elseif(namec.eq.  'gfs_stry '     ) then
        short_name='V FLX'
        increment=0.01
        increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=125
        parms10=1
      elseif(namec.eq.  'gfs_prcp '     ) then
        short_name='PRATE'
        increment=0.01
        increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=59
        parms10=1
      elseif(namec.eq.  'gfs_rdfx '     ) then
        short_name='NSWRS'
        increment=0.01
        increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=111
        parms10=1
      elseif(namec.eq.  'gfs_swfx '     ) then
        short_name='G RAD'
        increment=0.01
        increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=117
        parms10=1
      elseif(namec.eq.  'gfs_atps '     ) then
        short_name='MSL'
        increment=0.01
        increment=0.00001
        multiplicand=1.0
        additive=1013.0
        table=2
        parms9=128
        parms10=1
      else    ! no match, set to 0's
        short_name='none'
	increment=0.00001
        multiplicand=1.0
        additive=0.0
        table=2
        parms9=255
        parms10=1
      endif

c-test      write(*,*)namec(1:l),short_name,increment,multiplicand,additive,
c-test     $     table,parms9,parms10
c-test	enddo	

c-test	stop 9
c-test        end	
      return
      end
