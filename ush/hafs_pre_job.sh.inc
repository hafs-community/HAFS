################################################################################
# Script Name: hafs_pre_job.sh.inc
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script detects HAFS running platform and prepares module environment.
################################################################################

# Create a test function for sh vs. bash detection.  The name is
# randomly generated to reduce the chances of name collision.
__ms_function_name="setup__test_function__$$"
eval "$__ms_function_name() { /bin/true ; }"

# Determine which shell we are using
__ms_ksh_test=$( eval '__text="text" ; if [[ $__text =~ ^(t).* ]] ; then printf "%s" ${.sh.match[1]} ; fi' 2> /dev/null | cat )
__ms_bash_test=$( eval 'if ( set | grep '$__ms_function_name' | grep -v name > /dev/null 2>&1 ) ; then echo t ; fi ' 2> /dev/null | cat )

if [[ ! -z "$__ms_ksh_test" ]] ; then
  __ms_shell=ksh
elif [[ ! -z "$__ms_bash_test" ]] ; then
  __ms_shell=bash
else
  # Not bash or ksh, so assume sh.
  __ms_shell=sh
fi

target=""
USERNAME=$(echo $LOGNAME | awk '{ print tolower($0)'})

if [[ -d /lfs4 ]] ; then
  # We are on NOAA Jet
  if ( ! eval module help > /dev/null 2>&1 ) ; then
    echo load the module command 1>&2
    source /apps/lmod/lmod/init/$__ms_shell
  fi
  target=jet
  module purge
  module use /apps/modules/modulefiles
  module use /apps/lmod/lmod/modulefiles/Core
elif [[ -d /scratch1/NCEPDEV ]] ; then
  # We are on NOAA Hera
  if ( ! eval module help > /dev/null 2>&1 ) ; then
    echo load the module command 1>&2
    source /apps/lmod/lmod/init/$__ms_shell
  fi
  target=hera
  module purge
  module use /apps/modules/modulefiles
  module use /apps/lmod/lmod/modulefiles/Core
elif [[ -d /work/noaa ]] ; then
  # We are on MSU Orion/Hercules
  if [[ "$(hostname)" =~ "hercules" ]] ; then
    source /apps/other/lmod/lmod/init/$__ms_shell
    target=hercules
    module purge
#    module use /apps/contrib/modulefiles
    ulimit -s 6000000
  else
    if ( ! eval module help > /dev/null 2>&1 ) ; then
      echo load the module command 1>&2
      source /apps/lmod/lmod/init/$__ms_shell
    fi
    target=orion
    module purge
    module use /apps/modulefiles/core
    module use /apps/contrib/modulefiles
    module use /apps/contrib/NCEPLIBS/lib/modulefiles
    module use /apps/contrib/NCEPLIBS/orion/modulefiles
    ulimit -s unlimited
  fi
elif [[ -d /lfs/h1 && -d /lfs/h2 ]] ; then
  # We are on NOAA WCOSS2
  echo load the module command 1>&2
  source /usr/share/lmod/lmod/init/$__ms_shell
# source /usr/share/lmod/lmod/init/sh
  target=wcoss2
# module purge # use module reset instead
  module reset
elif [[ -d /glade ]] ; then
  # We are on NCAR Cheyenne
  if ( ! eval module help > /dev/null 2>&1 ) ; then
    echo load the module command 1>&2
    source /usr/share/Modules/init/$__ms_shell
  fi
  target=cheyenne
  module purge
elif [[ -d /lustre && -d /ncrc ]] ; then
  # We are on GAEA.
  if ( ! eval module help > /dev/null 2>&1 ) ; then
    # We cannot simply load the module command.  The GAEA
    # /etc/profile modifies a number of module-related variables
    # before loading the module command.  Without those variables,
    # the module command fails.  Hence we actually have to source
    # /etc/profile here.
    echo load the module command 1>&2
    source /etc/profile
  fi
  target=gaea
  module purge
elif [[ "$(hostname)" =~ "odin" ]] ; then
  target="odin"
else
  echo WARNING: UNKNOWN PLATFORM 1>&2
fi

unset __ms_shell
unset __ms_ksh_test
unset __ms_bash_test
unset $__ms_function_name
unset __ms_function_name

export WHERE_AM_I=${target}

# The following section is only used for the Rocoto workflow
if [[ Q"${HOMEhafs:-}" != "Q" ]] ; then
  if [[ "${WHERE_AM_I}" = "wcoss2" ]] ; then
    source ${HOMEhafs}/versions/run.ver
    module use /apps/ops/test/nco/modulefiles/core
    module load rocoto/${rocoto_ver:-"1.3.5"}
  fi
  module use ${HOMEhafs}/modulefiles
  module load hafs.${WHERE_AM_I}
fi
