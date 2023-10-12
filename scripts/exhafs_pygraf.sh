#! /bin/sh

set -xue

HOMEpygraf="$HOMEhafs/sorc/hafs_graphics.fd/pygraf"
COMpygraf="$COMhafs/pygraf"
NOUTHRS=${NOUTHRS:-3}

rm -rf "$DATA"
mkdir "$DATA"
cd "$DATA"

if [ "${SINGLE_THREADED:-YES}" = YES ] ; then
    cd $HOMEpygraf
    set +xue
    source ./pre.sh
    set -xue
    python ./create_graphics.py maps -a 3 -d "$COMhafs" -f "0" "$NHRS" "$NOUTHRS" --file_tmpl "00l.$YMDH.hfsa.storm.atm.f{FCST_TIME:03d}.grb2" --images ./image_lists/arfs.yml hourly --file_type prs -m arfs -n 20  -o "$COMpygraf" -s "$YMDH" -w 10
else
    for FHR in seq 0 "$NHRS" "$NOUTHRS" ; do
        echo "( set -xue ; cd $HOMEpygraf ; source ./pre.sh ; python create_graphics.py maps -a 3 -d '$COMhafs' -f '$FHR' '$FHR' 999 --file_tmpl '00l.$YMDH.hfsa.storm.atm.f{FCST_TIME:03d}.grb2' --images ./image_lists/arfs.yml hourly --file_type prs -m arfs -n 20  -o '$COMpygraf' -s '$YMDH' -w 10 ) 2>&1 | tee out-$FHR.log" >> cmdfile
    done
    $APRUNC "$MPISERIAL" -m cmdfile
fi

