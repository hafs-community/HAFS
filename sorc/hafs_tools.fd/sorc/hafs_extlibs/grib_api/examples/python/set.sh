#!/bin/sh

. ./include.sh

REDIRECT=/dev/null
$PYTHON $examples_src/set.py 2> $REDIRECT > $REDIRECT
rm -f out.set.grib || true
