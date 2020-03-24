#!/usr/bin/perl -w
# print only lines where fields 3..N are different
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
