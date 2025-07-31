#! /bin/csh
#
# Lew.Gramer@noaa.gov 2025-07-29: script to remove tasks matching a given string
#  from their reservation and run them in the base account.
# USAGE:    $0 [-d <dryrun-0-or-1>] [-r <newres>] [-a <newacct>]
# All args are currently POSITIONAL and optional.

set dryrun=0
if ( "$1" == "-d" ) then
 set dryrun=1
 shift
endif

set newres=""
if ( "$1" == "-r" ) then
 shift
 set newres="$1"
 shift
endif

set newacct="aoml-hafs1"
if ( "$1" == "-a" ) then
 shift
 set newacct="$1"
 shift
endif
 
alias sqg 'squeue -u $USER -o "%.10i %.10P %.60j %.15u %.10T %.10M %.10L %.4D %R %q" | grep "\(USER\|\!*\)"'
alias sqgid 'sqg "\!*" | cut -c 3-11 | grep -v JOBID'
alias supd 'scontrol update jobid="\!:1" \!:2*'

# Do *all* PENDING tasks (may be a no-op for those already outside the reservation)
#foreach id ( `sqgid "$*.*00L.*PEND"` )
# OR... Do *only* tasks PENDING due to "Reservation"
foreach id ( `sqgid "$*.*00L.*PEND.*Reserv"` )
  #echo supd ${id} reservation="" timelimit=3:20:00
  #supd ${id} reservation="" timelimit=3:20:00
  echo supd ${id} reservation=${newres} account=${newacct}
  if ( ! ${dryrun} ) then
    supd ${id} reservation=${newres} account=${newacct}
  endif
end

exit 0

