#!/usr/bin/perl
#
# Configuration script for HYCOM code
# 
# Be sure to run as ./configure (to avoid getting a system configure command by mistake)
#

$sw_netcdf_path = "" ;
$sw_hycom_path = "" ;
$sw_core_path = "" ;
$sw_os = "ARCH" ;           # ARCH will match any
$sw_mach = "ARCH" ;         # ARCH will match any
$sw_dmparallel = "" ;
$sw_ompparallel = "" ;
$sw_fc = "\$(SFC)" ;
$sw_cc = "\$(SCC)" ;
$sw_f90 = "\$(SF90)" ;
$sw_usenetcdff = "" ;    # for 3.6.2 and greater, the fortran bindings might be in a separate lib file

# make sure we do not buffer stdout
select((select(STDOUT), $|=1)[0]);

while ( substr( $ARGV[0], 0, 1 ) eq "-" )
 {
  if ( substr( $ARGV[0], 1, 7 ) eq "netcdf=" ) 
  {
    $sw_netcdf_path = substr( $ARGV[0], 8 ) ;
  }
  if ( substr( $ARGV[0], 1, 9 ) eq "corepath=" )
  {
    $sw_core_path = substr( $ARGV[0], 10 ) ;
  }
  if ( substr( $ARGV[0], 1, 8 ) eq "hycompath=" )
  {
    $sw_hycom_path = substr( $ARGV[0], 9 ) ;
  }
  if ( substr( $ARGV[0], 1, 3 ) eq "os=" )
  {
    $sw_os = substr( $ARGV[0], 4 ) ;
  }
  if ( substr( $ARGV[0], 1, 5 ) eq "mach=" )
  {
    $sw_mach = substr( $ARGV[0], 6 ) ;
  }
  if ( substr( $ARGV[0], 1, 11 ) eq "dmparallel=" )
  {
    $sw_dmparallel=substr( $ARGV[0], 12 ) ;
  }
  if ( substr( $ARGV[0], 1, 12 ) eq "ompparallel=" )
  {
    $sw_ompparallel=substr( $ARGV[0], 13 ) ;
  }
  if ( substr( $ARGV[0], 1, 11 ) eq "USENETCDFF=" )
  {
    $sw_usenetcdff = substr( $ARGV[0], 12 ) ;
  }
  shift @ARGV ;
 }


# parse the configure.defaults file

$validresponse = 0 ;
@platforms = qw ( serial dmpar ) ;

# Display the choices to the user and get selection
until ( $validresponse ) {
  printf "------------------------------------------------------------------------\n" ;
  printf "Please select from among the following supported platforms.\n\n" ;

  $opt = 1 ;
  open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
  while ( <CONFIGURE_DEFAULTS> )
  {
    for $paropt ( @platforms ) 
    {
      if ( substr( $_, 0, 5 ) eq "#ARCH" && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 )
	   && ( index($_, $paropt) >= 0 )  )
      {
        $optstr[$opt] = substr($_,6) ;
        $optstr[$opt] =~ s/^[ 	]*// ;
        $optstr[$opt] =~ s/#.*$//g ;
        chomp($optstr[$opt]) ;
        $optstr[$opt] = $optstr[$opt]." (".$paropt.")" ;
        if ( substr( $optstr[$opt], 0,4 ) ne "NULL" )
        {
          printf "  %2d.  %s\n",$opt,$optstr[$opt] ;
          $opt++ ;
        }
      }
    }
  }
  close CONFIGURE_DEFAULTS ;

  $opt -- ;

  printf "\nEnter selection [%d-%d] : ",1,$opt ;
  $response = <STDIN> ;

  if ( $response == -1 ) { exit ; }

  if ( $response >= 1 && $response <= $opt ) 
  { $validresponse = 1 ; }
  else
  { printf("\nInvalid response (%d)\n",$response);}
}
printf "------------------------------------------------------------------------\n" ;

$optchoice = $response ;

if(defined($ENV{NETCDF4_LINK_OPTS})) {
    $netcdf_libs="$ENV{NETCDF4_LINK_OPTS}";
} elsif(defined($ENV{NETCDF_LDFLAGS_F})) {
    $netcdf_libs="$ENV{NETCDF_LDFLAGS_F}";
} else {
    $netcdf_libs="-L$sw_netcdf_path/lib $sw_usenetcdff -lnetcdf";
}

open CONFIGURE_DEFAULTS, "< ./arch/configure.defaults" 
      or die "Cannot open ./arch/configure.defaults for reading" ;
$latchon = 0 ;
while ( <CONFIGURE_DEFAULTS> )
{
  if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 1 )
  {
    $latchon = 0 ;
  }
  if ( $latchon == 1 )
  {
    $_ =~ s/CONFIGURE_NETCDF_PATH/$sw_netcdf_path/g ;
    $_ =~ s/CONFIGURE_NETCDF_LIBS/$netcdf_libs/g ;
    $_ =~ s|CONFIGURE_NETCDF_INCLUDE|-I$sw_netcdf_path/include|g ;
    $_ =~ s/CONFIGURE_HYCOM_PATH/$sw_hycom_path/g ;
    $_ =~ s/CONFIGURE_FC/$sw_fc/g ;
    $_ =~ s/CONFIGURE_F90/$sw_f90/g ;
    $_ =~ s/CONFIGURE_CC/$sw_cc/g ;

    @machopts = ( @machopts, $_ ) ;
  }
  for $paropt ( @platforms )
  {
    if ( substr( $_, 0, 5 ) eq "#ARCH" && $latchon == 0
          && ( index( $_, $sw_os ) >= 0 ) && ( index( $_, $sw_mach ) >= 0 )
          && ( index($_, $paropt) >= 0 ) )
    {

    $x=substr($_,6) ;
    $x=~s/^[     ]*// ;
    $x =~ s/#.*$//g ;
    chomp($x) ;
    $x = $x." (".$paropt.")" ;
    if ( $x eq $optstr[$optchoice] )
    {
      $latchon = 1 ;
      $sw_ompparallel = "" ;
      $sw_dmparallel = "" ;
      $validresponse = 0 ;

      if ( $paropt eq 'dmpar' ) 
      {
        $sw_dmparallel = "RSL_LITE" ;
        $sw_dmparallelflag = "-DDM_PARALLEL" ;
	$sw_fc = "\$(DM_FC)" ;
	$sw_f90 = "\$(DM_F90)" ;
	$sw_cc = "\$(DM_CC)" ;
      }
    }
    }

  }
}
close CONFIGURE_DEFAULTS ;


open CONFIGURE_HYCOM, "> configure.hycom" or die "cannot append configure.hycom" ;
open ARCH_PREAMBLE, "< arch/preamble" or die "cannot open arch/preamble" ;
my @preamble;
# apply substitutions to the preamble...
while ( <ARCH_PREAMBLE> )
  {
  @preamble = ( @preamble, $_ ) ;
  }
close ARCH_PREAMBLE ;
print CONFIGURE_HYCOM @preamble  ;
close ARCH_PREAMBLE ;
printf CONFIGURE_HYCOM "# Settings for %s", $optstr[$optchoice] ;
print CONFIGURE_HYCOM @machopts  ;
open ARCH_POSTAMBLE, "< arch/postamble" or die "cannot open arch/postamble" ;
while ( <ARCH_POSTAMBLE> ) {
  print CONFIGURE_HYCOM;
 }
close ARCH_POSTAMBLE ;
close CONFIGURE_HYCOM ;

printf "Configuration successful. To build the HYCOM, type: compile \n" ;
printf "------------------------------------------------------------------------\n" ;


