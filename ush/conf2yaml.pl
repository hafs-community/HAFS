#! /usr/bin/env perl

use strict;
use warnings;

my $prior;

my @lines=<>;

my %allvars;

# First pass: figure out what variables are in each section
my $sec=undef;
for $_ (@lines) {
  if(/^\s*([^=]+)=/) {
    if(defined($sec)) {
      $allvars{$sec}{$1}=1;
    }
  }
  if (/^\[([^]]+)\]/) {
    $sec=$1;
  }
}

# Second pass: convert.
$sec=undef;
for $_ (@lines) {
  chomp;
  if (/^\s*(?:#.*)?$/) {
    print("$_\n");
    next;
  }
  if (/^\[([^]]+)\]/) {
    $prior="$1: # <------- section [$1]\n\n";
    $sec=$1;
    next;
  } elsif (/^\s*([^=]+)=([^{} ;]+)(?:\s*;+(.*))?$/) {
    print($prior) if($prior);
    undef $prior;
    my $opt=$1;
    my $val=$2;
    my $comment=$3;
    print("  $opt: ");
    if ($val =~ /^[0-9]+$/) {   #integer
      print($val);
    } elsif ($val =~ /^(?:[0-9]+|[0-9]+\.[0-9]*(?:[eE][0-9]+)?|\.[0-9]+(?:[eE][0-9]+)?)$/) {
      # float
      print($val)
    } else {
      print("\"$val\"");
    }
    if(defined($comment)) {
      print(" ## $comment\n  # ^--- was $opt=$val\n\n");
    } else {
      print("\n  # ^--- was $opt=$val\n\n");
    }

  } elsif (/^\s*([^=]+)=([^ ;]+)(?:\s*;+(.*))?$/) {
    print($prior) if($prior);
    undef $prior;
    my $opt=$1;
    my $val=$2;
    my $comment=$3;

    print("  $opt: !uexpand \"");
    #print("((($val)))");

    my @splat=split(/([{}])/,$val);
    #print("(|$val => |".(join("|",@splat))."|)");
    my $in=0;
    for my $splat (@splat) {
      #print("((($splat $in)))");
      if ($in) {
        if ($splat eq "}") {
          $in=0;
          #print($splat);
          next;
        }
        if($splat=~m:^ENV\[:) {
          $splat=~s:^ENV\[([^\]|]+)\]:tools.env('$1'):g;
          $splat=~s:^ENV\[([^\]|]+)\|-([^\]]+)\]:tools.env('$1','$2'):g;
        } elsif($splat=~m:[/\[]:) {
          $splat=~s:^(.*/.*)$:doc.$1:g;
          $splat=~s:/:.:g;
          $splat=~s:\[(.*)\]:.$1:g;
        } else {
          if($splat=~/^((?:(?:f|am6|ap6|a)?(YMDHM|YMDH|YMD|YYYY|YY|CC|month|MM|day|DD|cyc|HH|minute|min))|fahr|famin|fahrmin)$/) {
            $splat="cyc.$1";
          } elsif(defined($sec) && !defined($allvars{$sec}{$splat})) {
            $splat="all.$splat";
          }
        }
        print("$splat}");
      } elsif ($splat eq '{') {
        print($splat);
        $in=1;
      } else {
        print($splat);
      }
    }
    if ($comment) {
      print("\" ## $comment\n  # ^--- was $opt=$val\n\n");
    } else {
      print("\"\n  # ^--- was $opt=$val\n\n");
    }
  }
}
