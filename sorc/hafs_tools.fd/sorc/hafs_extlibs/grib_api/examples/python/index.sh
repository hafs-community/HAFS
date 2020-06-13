#!/bin/sh

. ./include.sh

REDIRECT=/dev/null
$PYTHON $examples_src/index.py 2> $REDIRECT > $REDIRECT
rm my.idx || true
