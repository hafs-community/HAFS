#!/bin/sh

. ./include.sh

REDIRECT=/dev/null
$PYTHON $examples_src/clone.py 2> $REDIRECT > $REDIRECT
rm -f out.clone.grib || true
