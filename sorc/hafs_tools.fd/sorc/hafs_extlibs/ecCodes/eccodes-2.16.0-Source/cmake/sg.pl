#!/usr/bin/env perl

# (C) Copyright 2011- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

use strict;

#use Data::Dumper;
use File::Basename;

#$Data::Dumper::Indent = 1;
# $ARGV[0] = "test.cc";
# $ARGV[0] = "/usr/include/g++-3/stl_pair.h";
# $ARGV[0] = "/usr/include/g++-3/stl_vector.h";
# $ARGV[0] = "/usr/include/g++-3/stl_list.h";
# $ARGV[0] = "/usr/include/g++-3/stl_map.h";
# $ARGV[0] = "x.cc";
# $ARGV[0] = "/usr/include/g++-3/std/bastring.h";

# script takes 3 parameters:
# (1) file to process
my $file = $ARGV[0];
# (2) [optional] directory to place the generated .b file
my $base = $ARGV[1];
# (3) [optional] c++ namespace
my $namespace = $ARGV[2];

# no argv[1] passed, take basedir from file
if( $base eq "" )
{
	$base = dirname($file);
}

# no argv[1] passed, take basedir from file
if( $namespace eq "" )
{
    $namespace = "eclib"
}

my @c = parser::parse($file);
#print Dumper(\@c);


foreach my $c ( @c )
{
	my $n = $c->name;
	open(STDOUT,">$base/$n.b") || die "$base/$n.b: $!";

	my @init1;
	push @init1, map { "$_(b)" } $c->super;
	push @init1, map { "$_(b(\&$_))" } $c->members;

	my $col1;
	$col1=":\n" if(@init1);
	my $init1 = join(",\n",map {"\t$_"} @init1);

	my @init2;
	push @init2, map { "$_(b(\"$n\"))" } $c->super;
	push @init2, map { "$_(b(\"$n\",\"$_\"))" } $c->members;

	my $col2;
	$col2=":\n" if(@init2);
	my $init2 = join(",\n",map {"\t$_"} @init2);


	my @s = map { "${_}::describe(s,depth+1)"      } $c->super;
	my @m = map { "${namespace}::_describe(s,depth+1,\"$_\",$_)" } $c->members;
	my $d = join(";\n\t","${namespace}::_startClass(s,depth,specName())",@s,@m,"${namespace}::_endClass(s,depth,specName())");

	my @s = map { "${_}::_export(h)"      } $c->super;
	my @m = map { "${namespace}::_export(h,\"$_\",$_)" } $c->members;
	my $D = join(";\n\t","${namespace}::_startClass(h,\"$n\")",@s,@m,"${namespace}::_endClass(h,\"$n\")");

	my $spec = "\"$n\"";
	my @tmpl = $c->template;

	my $spec_type = "const char*";

	if(@tmpl)
	{
		$spec_type = "std::string";
		my $x = join("+ ',' + ",  map { "Traits<$_>::name()"; } @tmpl);
		$spec = <<"EOS";
        std::string("$n<\") + $x + ">"
EOS
		$spec =~ s/\n/ /g;
	}

	my $isa = "${namespace}::Isa::add(t,specName());";
	foreach my $s ( $c->super )
	{
		$isa = "${s}::isa(t);$isa";
	}

	my $schema;
	@s = map { "${_}::schema(s)"      } $c->super;
	@m = map { $a=$_->[0]; $b=$_->[1]; "s.member(\"$a\",member_size($n,$a),member_offset($n,$a),\"$b\")" } $c->members_types;
	$schema = join(";\n\t","s.start(specName(),sizeof($n))",@s,@m,"s.end(specName())");

	print <<"EOF";

${n}(${namespace}::Bless& b)$col1$init1
{
}

${n}(${namespace}::Evolve b)$col2$init2
{
}

static ${spec_type} specName()      { return ${spec}; }
static void isa(TypeInfo* t)  { ${isa} }
static ${namespace}::Isa* isa()             { return ${namespace}::Isa::get(specName());  }

static void schema(${namespace}::Schema& s)
{
	$schema;
}

EOF

if(!$c->has_method("describe"))
{
print <<"EOF";

void describe(std::ostream& s,int depth = 0) const {
	$d;
}


EOF
}

print <<"EOF";

void _export(${namespace}::Exporter& h) const {
	$D;
}


EOF

}
if(0)
{
foreach my $c ( @c )
{
	my $n = $c->name;
	open(OUT,">${n}.b");
	select OUT;
	print "static void schema(${namespace}::Schema& s) {\n";
	foreach my $x ( $c->super )
	{
		print "${x}::schema(s);\n";
		#print "s(\"$x\", 0,sizeof($x));\n";
	}
	foreach my $x ( $c->members )
	{
		print "s(\"${n}::$x\",offsetof($n,$x),sizeof(&(($n*)0)->$x));\n";
	}
	print "}\n";
}
}
package parser;
use Carp;
my @TOKENS;
sub parse {
	my ($file) = @_;
	local $/ = undef;
	open(IN,"<$file") || croak "$file: $!";
	my $x = <IN>;
	close(IN);
	$x =~ s/^#.*$//mg;
	$x =~ s/\/\/.*$//mg;
	@TOKENS =
		grep { length($_);                }
		map  { /\W/ ? split('',$_) :  $_; }
		map  { s/\s//g; $_;               }
		split(/\b/, $x );

	my @c;
	my $x;
	while($x = consume_until("(typedef|template|class|struct)"))
	{
		if($x eq 'typedef')
		{
			consume_until(";");
			next;
		}

		if($x eq 'template')
		{
			push @c, parse_template();
		}
		else
		{
			push @c, parse_class();
		}
	}
	return grep { defined $_; } @c;
}

sub parse_template {
	my @tmp = template_args();
	return parse_class(@tmp) if(next_is("(class|struct)"));
}
sub template_args {
	my @tmp;
	expect_next("<");
	for(;;)
	{
		expect_next("(class|bool|int)");
		push @tmp, next_ident();
		if(next_is("="))
		{
			my $x = consume_until('(,|\>|\<)');
			unshift @TOKENS,$x;
			while($x eq '<')
			{
				consume_block('<','>');
				$x = consume_until('(,|\>|\<)');
				unshift @TOKENS,$x;
			}
		}
		last unless(next_is(","));
	}
	expect_next(">");
	return @tmp;
}
sub parse_class {
	my (@tmp) = @_;
	my $self = {};
	my $name = next_ident();
	$self->{name}     = $name;
	$self->{template} = \@tmp if(@tmp);
	# Foreward declaration
	return if(next_is(";"));
	if(next_is(":"))
	{
		for(;;)
		{
			ignore_while("(public|private|protected|virtual)");
			push @{$self->{super}}, next_ident();
			last unless(next_is(","));
		}
	}
	expect_next('{');
	while(!peek_next('}'))
	{
		# print "... : $TOKENS[0], $TOKENS[1], ... \n";
		if(next_is('\/'))
		{
			if(next_is('\*'))
			{
				while(!next_is('\/'))
				{
					consume_until('\*');
				}
				next;
			}
			else
			{
				unshift @TOKENS, "/";
			}

		}

		if(next_is("(public|private|protected)"))
		{
			expect_next(":");
			next;
		}

		if(next_is("friend"))
		{
			my $x = consume_until("(;|{)");
			if($x eq "{")
			{
				unshift @TOKENS, $x;
				consume_block('{','}');
			}
			next;
		}

		# next_is("explicit");
		if(next_is("(typedef|using|typename|enum)"))
		{
			consume_until(";");
			next;
		}
		if(next_is("(class|struct)"))
		{
			push @{$self->{classes}}, parse_class();
			next;
		}
		my %m;
		while(next_is("template"))
		{
			push @{ $m{template} } , template_args();
		}

		my @x;
#		push @x,"~" while(next_is('\~'));

		$m{explicit} = 1 if(next_is("explicit"));
		$m{static}   = 1 if(next_is("static"));
		$m{virtual}  = 1 if(next_is("virtual"));
		my $x;
		while($x = next_is_ident())
		{
			# print "--- : $x\n";
			push @x, $x;
			push @x,'*' while(next_is('\*'));
			push @x,'&' while(next_is('\&'));
			$m{name} = $x;
			# int a,b,*c; does not work
			my $s;
			if($s = next_is('(,|;|=)'))
			{
				pop @x;
				$m{type} = make_type(@x);
				if(exists $m{static})
				{
					push @{$self->{class_members}}, \%m;
				}
				else
				{
					push @{$self->{members}}, \%m;
				}
				consume_until(";") if($s eq '=');
				last;
			}
			if(peek_next('\('))
			{
				pop @x;
				$m{type} = make_type(@x);
				my @args = consume_block('(',')');
				shift @args;
				pop @args;
				my @a;
				my $n = 0;
				my @z;
				foreach my $a ( @args )
				{
					if($a eq ',' && $n == 0)
					{
						push @a, make_type(@z);
						@z = ();
						next;
					}
					$n++ if($a eq '<');
					$n++ if($a eq '(');
					$n-- if($a eq ')');
					$n-- if($a eq '>');
					push @z,$a;
				}
				push @a, make_type(@z) if(@z);
				$m{const} = 1 if(next_is("const"));
				$m{args}  = \@a;
				if(exists $m{static})
				{
					push @{$self->{class_methods}}, \%m;
				}
				else
				{
					push @{$self->{methods}}, \%m;
				}
				# print "f: $x\n";

				if(next_is(':'))
				{
					# print "{: $TOKENS[0]\n";
					consume_until('\{');
					unshift @TOKENS, '{';
					consume_block('{','}');
					# print "}: $TOKENS[0]\n";
				}
				else
				{
					if(peek_next('\{'))
					{
						consume_block('{','}');
					}
					else
					{
						if(next_is("="))
						{
							expect_next("0");
							$m{abstract} = 1;
						}
						expect_next(";");
					}
				}
				last;
			}
		}

	}
	expect_next("}");
	expect_next(";");
	return bless($self,"class");
}
sub consume_until {
	my ($r) = @_;
	while(@TOKENS)
	{
		my $x = shift @TOKENS;
		return $x if($x =~ /^$r$/);
	}
	return undef;
}
sub consume_block {
	my ($bra,$ket) = @_;
	my $n = 0;
	my @x;
	croak "@TOKENS" unless($bra eq $TOKENS[0]);
	while(@TOKENS)
	{
		my $x = shift @TOKENS;
		$n++ if($x eq $bra);
		$n-- if($x eq $ket);
		push @x,$x;
		return @x if($n == 0);
	}
}
sub ignore_while {
	my ($r) = @_;
	while(@TOKENS)
	{
		return unless($TOKENS[0] =~ /^$r$/);
		shift @TOKENS;
	}
}
sub expect_next {
	my ($r) = @_;
	my $ident = shift @TOKENS;
	croak "$ident is not $r" unless($ident =~ /^$r$/);
	return $ident;
}
sub next_ident {
	my $x = next_is_ident();
	croak "not an ident " unless($x);
	return $x;
}
sub next_is {
	my ($r) = @_;
	if($TOKENS[0] =~ /^$r$/)
	{
		return shift @TOKENS;
	}
	return undef;
}
sub next_is_ident {
	my $op = next_is("operator");
	if($op)
	{
		my $x;
		if($x = next_is("(new|delete)"))
		{
			$op .= " $x";
			if(next_is('\['))
			{
				expect_next('\]');
				$op .= "[]";
			}
			return $op;
		}

		if(next_is('\('))
		{
			expect_next('\)');
			$op .= "()";
		}
		my $z;
		while($z = next_is('[\-+/\*\[\]<>=!]'))
		{
			$op .= $z;
		}
		return $op;
	}
	my $y = next_is('\~');
	my $x = next_is('\w+');
	if($x)
	{
		$x = "$y$x";
		if(peek_next("<"))
		{
			my @x = consume_block("<",">");
			$x .= join("",@x);
		}

		if(next_is(":"))
		{
			if(next_is(":"))
			{
				my $z = next_is_ident();
				return "${x}::${z}";
			}
			else
			{
				unshift @TOKENS, ":";
			}
		}
	}
	return $x;
}
sub peek_next {
	my ($r) = @_;
	if($TOKENS[0] =~ /^$r$/)
	{
		return 1;
	}
	return 0;
}
sub make_type {
	my (@a) = @_;
	my $p;
	my @x;
	foreach my $a ( @a )
	{
		push @x, " " if($p =~ /^(\w+|\&|\*)$/ && $a =~ /^\w+$/);
		push @x, $a;
		$p = $a;
	}
	my $s = join('',@x);
	$s =~ s/>>/> >/g;
	return $s;
}
package class;
sub name {
	my ($self) = @_;
	return $self->{name};
}
sub super {
	my ($self) = @_;
	return $self->{super} ? @{$self->{super}} : ();
}

sub members {
	my ($self) = @_;
	my @x = $self->{members} ? @{$self->{members}} : ();
	return map { $_->{name} } @x;
}

sub methods {
	my ($self) = @_;
	my @x = $self->{methods} ? @{$self->{methods}} : ();
	return map { $_->{name} } @x;
}

sub has_method {
	my ($self,$name) = @_;
	return grep { $_ eq $name } $self->methods;
}

sub members_types {
	my ($self) = @_;
	my @x = $self->{members} ? @{$self->{members}} : ();
	return map { [ $_->{name}, $_->{type} ]  } @x;
}

sub template {
	my ($self) = @_;
	return $self->{template} ? @{$self->{template}} : ();
}
1;
