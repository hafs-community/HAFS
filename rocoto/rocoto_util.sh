#!/bin/sh
# A rocoto util script to check status for all/active/failed tasks, and
# if requested can also rewind the failed tasks.

nopts=$#
if [ $nopts -lt 1 ]; then
  echo Usage:
  echo "  ./rocoto_utils.sh [-a | -f | -r | -s] # Loop over all *.xml files"
  echo "  ./rocoto_utils.sh [-a | -f | -r | -s] hafs # Loop over hafs*.xml files"
  echo "  -a: check all active (SUBMITTING|QUEUED|RUNNING|DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks"
  echo "  -f: check all failed (DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks"
  echo "  -r: check and rewind all failed (DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks"
  echo "  -s: check status for all tasks"
  exit 1
fi

if [ Q$(which rocotostat) = 'Q' ]; then
  echo "Error: Make sure rocotostat and rocotorewind in the path"
  exit 1
fi

cmdopt=${1:--a}

for file in ${2:-''}*.xml
do
  echo ${file}
  fd=$(echo $file | rev | cut -d. -f2- | rev)
  if [ $cmdopt = '-a' ]; then
    # Check all active (SUBMITTING|QUEUED|RUNNING|DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks
    echo $fd active tasks:
    rocotostat -w $file -d ${fd}.db -c all | grep -E 'SUBMITTING|QUEUED|RUNNING|DEAD|UNKNOWN|FAILED|UNAVAILABLE'
  elif [ $cmdopt = '-f' ]; then
    # Checking failed tasks
    echo $fd failed tasks:
    rocotostat -w $file -d ${fd}.db -c all | grep -E 'DEAD|UNKNOWN|FAILED|UNAVAILABLE'
  elif [ $cmdopt = '-r' ]; then
    # Deal with failed tasks
    >temp_deadlist.dat
    rocotostat -w $file -d ${fd}.db -c all | grep -E 'DEAD|UNKNOWN|FAILED|UNAVAILABLE' > temp_deadlist.dat
    ll=$(cat temp_deadlist.dat |wc -l)
    if [ $ll -ne 0 ] ; then
      while read line
      do
        cc=$(echo $line | awk '{print $1}')
        tt=$(echo $line | awk '{print $2}')
        echo "rewinding $file $cc $tt"
        rocotorewind -w $file -d ${fd}.db -c $cc -t $tt
      done < temp_deadlist.dat
    fi
    rm -f temp_deadlist.dat
  elif [ $cmdopt = '-s' ]; then
    # Check status for all tasks
    echo $fd all tasks:
    rocotostat -w $file -d ${fd}.db -c all
  else
    echo "Error: unknown commandline option"
    echo "Usage:"
    echo "  ./rocoto_utils.sh [-a | -f | -r | -s] # Loop over all *.xml files"
    echo "  ./rocoto_utils.sh [-a | -f | -r | -s] hafs # Loop over hafs*.xml files"
    echo "  -a: check all active (SUBMITTING|QUEUED|RUNNING|DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks"
    echo "  -f: check all failed (DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks"
    echo "  -r: check and rewind all failed (DEAD|UNKNOWN|FAILED|UNAVAILABLE) tasks"
    echo "  -s: check status for all tasks"
    exit 1
  fi
done

