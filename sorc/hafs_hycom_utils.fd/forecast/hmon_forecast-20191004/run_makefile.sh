#!/bin/ksh

exec=reloc
for exec in reloc 
do
   if [ "$exec" == reloc ]; 
   then
     make -f makefile.forecast clean
     make -f makefile.forecast CUSE_FLAG="-DUSE_SCOUPLER -DRELO " ../../exec/hmon_rtofs_${exec}_forecast
   fi
#   if [ "$exec" == cpl3 ];
#   then
#     make -f makefile.forecast clean
#     make -f makefile.forecast CUSE_FLAG="-DUSE_SCOUPLER -DRELO -DSTOKES -DWW3CPL " ../../exec/hmon_rtofs_${exec}_forecast
#   fi
done

