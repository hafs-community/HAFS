#!/bin/bash
(rsync "$@"; if [ $? == 24 ]; then exit 0; else exit $?; fi) 2>&1 \
	    | grep -v 'vanished'
