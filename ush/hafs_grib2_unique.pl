#!/usr/bin/perl -w
#################################################################################
# Script Name: hafs_grib2_unique.pl
# Authors: NECP/EMC Hurricane Project Team and UFS Hurricane Application Team
# Abstract:
#   This script print only lines where fields 3..N are different.
# History:
#   12/07/2019: Added the script to use grib2 format GFS input files for LBC
#################################################################################
# 
while (<STDIN>) {
  chomp;
  $line = $_;
  $_ =~ s/^[0-9.]*:[0-9]*://;
  if (! defined $inv{$_}) { 
    $inv{$_} = 1;
    print "$line\n";
  }
}
