#!/bin/sh

. ./include.sh

REDIRECT=/dev/null
$PYTHON $examples_src/nearest.py 2> $REDIRECT > $REDIRECT
