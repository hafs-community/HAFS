#!/bin/bash

python_dir=/work/noaa/marine/yli/soca-shared/soca-diagnostics

mkdir GIF
mkdir images

mv PNG/analysed_sst.*.png images/.
gif_path=analysed_sst.OSTIA_0.05deg.20200825TO30.gif
python3 ${python_dir}/mkmovie.py -o ${gif_path}
mv images/* PNG/.
cp *gif ~/.
mv *.gif GIF/.
exit

mv PNG/*Salt*.png images/.
gif_path=40mem.3dhyb.ana.ctrl.Salt.Layer0.movie.gif
python3 ${python_dir}/mkmovie.py -o ${gif_path}
mv images/* PNG/.
cp *gif ~/.
mv *.gif GIF/.
exit

mv PNG/*Temp*.png images/.
gif_path=40mem.3dhyb.ana.ctrl.Temp.Layer0.movie.gif
python3 ${python_dir}/mkmovie.py -o ${gif_path}
mv images/* PNG/.
cp *gif ~/.
mv *.gif GIF/.
exit

