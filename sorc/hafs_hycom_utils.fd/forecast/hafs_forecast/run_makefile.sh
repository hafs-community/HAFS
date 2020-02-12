#!/bin/ksh


#for exec in way3 way2 
for exec in way2
do
   make -f makefile.forecast clean
   if [ "$exec" == way3 ]; 
   then
     make -f makefile.forecast CUSE_FLAG="-DUSE_SCOUPLER -DRELO -DSTOKES -DWW3CPL " ../../exec/hwrf_rtofs_reloc_forecast_${exec}
   else 
     make -f makefile.forecast CUSE_FLAG="-DUSE_SCOUPLER -DRELO " ../../exec/hafs_rtofs_reloc_forecast
   fi

   #make -f makefile.forecast ${CPPFLAG} ../../exec/hwrf_rtofs_${exec}_forecast_${exec}
done

