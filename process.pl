#!/usr/bin/perl -w

# This takes the limited subset of SVG produced by flatten2.pl and 
# Uses Math::Clipper to union/difference all the parts. It will not
# work on general SVG files.

# Usage: process.pl --kerf 130 lifters.csg-3.svg

# Kerf is the radius of the laser beam in micrometres. My laser 
# cutting services specifies 130 for 5/6mm acrylic and 100 for 3mm
# acrylic.


use strict;
use Math::Trig;
use Data::Dumper;
use Math::Clipper ':all';
use Getopt::Long;

my $kerf = 0;
my $result = GetOptions ("kerf=i" => \$kerf);

my $filename = shift;

my $drawOriginal = 0; # Draws the un-offsetted shapes in black

open(INPUTSVG,$filename);
my $outputfilename = $filename;
$outputfilename =~ s/.svg$/-processed.svg/;
open(OUTPUTSVG,">$outputfilename");

my $clipper = Math::Clipper->new;

my $startLine = <INPUTSVG>;
print OUTPUTSVG $startLine;

my @items = ();
while(my $l = <INPUTSVG>)
{
    if($l =~ /^<g>/) {
	@items = ();
    }
    elsif($l =~ /^<\/g>/) {
	# Process all group info	
	my $basePoly = shift @items; # This must not be negative!

	if(!defined($basePoly)) {
	    next;
	}
	if($basePoly->[0] eq 'NEGATIVE') {
	    next;
	}

	print OUTPUTSVG "<g>\n";
	for my $i(@items) {
	    my $nextPoly = $i;
	    if($nextPoly->[0] eq 'NEGATIVE') {	
		next; # Deal with them next
	    }
	    else
	    {
		print "adding base poly: ";
		print Dumper($basePoly);
		if(!is_counter_clockwise($basePoly)) {
		    print "CAUTION: CLOCKWISE wound polygon\n";
		}
		$clipper->clear();
		$clipper->add_subject_polygon($basePoly);
		$clipper->add_clip_polygon($nextPoly);

		$basePoly = $clipper->execute(CT_UNION);
		# Now we have to check whether this is a one-element array
		if(@{$basePoly} > 1) {
		    my $head = shift(@{$basePoly});
		
		    for my $p (@{$basePoly}) {
			dumpPoly($p,1);
		    }
		    $basePoly = $head;
		}
		elsif(@{$basePoly} == 1) {
		    $basePoly = $basePoly->[0];
		}
	    }
	}
	# BUG: If you have two disjoint objects first which are connected by a third,
	# This won't union them all together.
	for my $i(@items) {
	    my $nextPoly = $i;
	    if($nextPoly->[0] eq 'NEGATIVE') {	
		# This just dumps negative objects as new polygons.
		# We should ATTEMPT to difference these and if it makes
		# a single object, just keep that.
		shift @$nextPoly;
		$clipper->clear();
		$clipper->add_subject_polygon($basePoly);
		$clipper->add_clip_polygon($nextPoly);
		my $resultPoly = $clipper->execute(CT_DIFFERENCE);
		if(@{$resultPoly}>1) {
		    # Nope
		    dumpPoly($nextPoly,1);
		}
		else
		{
		    print "Replacing basepoly with result of diff:\n";
		    print Dumper($resultPoly);
		    $basePoly = $resultPoly->[0];
		}
	    }
	}

	print "Results of group: \n";
	dumpPoly($basePoly,0);
	print OUTPUTSVG "</g>\n";
	
    }
    elsif($l =~ /^<circle/) {
	push @items, readCircle($l);
    }
    elsif($l =~ /^<polygon/) {
	push @items, readPoly($l);
    }
    elsif($l =~/<\/svg/) {
	last;
    }
}

print OUTPUTSVG "</svg>\n";

print "End of file.\n";
close (INPUTSVG);
close (OUTPUTSVG);

sub dumpPoly
{
    my ($p,$neg) = @_;
    if(ref($p->[0]->[0]) eq "ARRAY") {
        my $count = 0;
	for my $poly(@$p) {
	    dumpPoly($poly,$neg || $count>1);
            $count++;
	}
    }
    else
    {
        if($drawOriginal) {
            print OUTPUTSVG "<polygon points=\"";
            for my $pp(@$p) {
                my ($x,$y) = ($pp->[0]/1000,$pp->[1]/1000);
                print OUTPUTSVG "$x,$y ";
            }
        }
	print OUTPUTSVG "\" style=\"stroke-width:0.1;stroke:#000000;stroke-opacity:1;fill:none\"/> \n";
        print "About to offset polygon:\n";
        print Dumper($p);
        if($neg) {
            $kerf = -$kerf;
        }
        my $offset_polygons = offset([$p], $kerf, 100, JT_SQUARE);
        print "Results of offsetting:\n";
        print Dumper($offset_polygons);
	print OUTPUTSVG "<polygon points=\"";
	for my $pp(@{$offset_polygons->[0]}) {
	    my ($x,$y) = ($pp->[0]/1000,$pp->[1]/1000);
	    print OUTPUTSVG "$x,$y ";
	}
	print OUTPUTSVG "\" style=\"stroke-width:0.01;stroke:#0000FF;stroke-opacity:1;fill:none\"/> \n";
    }

}

sub readCircle
{
    my $line = shift;
    my ($cx,$cy,$r);
    if($line =~ /\bcx\s*=\s*"(-?\d+\.?\d*)"/ )
    {
	$cx = $1;
    }
    if($line =~ /\bcy\s*=\s*"(-?\d+\.?\d*)"/ )
    {
	$cy = $1;
    }
    if($line =~ /\br\s*=\s*"(-?\d+\.?\d*)"/ )
    {
	$r = $1;
    }
    if(!defined($cx) && !defined($cy) && !defined($r)) {
	print "Circle without cx,cy or r:\n";
	print $line;
	exit(2);
    }
    # Convert this into a polygon...
    my @circlePoly = ();
    for(my $d = 0; $d<360; $d+=9) {
	my @point = ( int(1000*($cx+$r * cos(deg2rad($d)))) , int(1000*($cy+$r * sin(deg2rad($d)))) );
	push @circlePoly, \@point;
    }
    print "Generated circular polygon with radius $r at $cx,$cy\n";
    if(!is_counter_clockwise(\@circlePoly)) {
	@circlePoly = reverse @circlePoly;
    }
    if(!is_counter_clockwise(\@circlePoly)) {
	print Dumper(\@circlePoly);
	printf("WTF1\n");
	exit(3);
    }
    if($line =~ /negative/) {
	unshift @circlePoly, "NEGATIVE";
    }

    return \@circlePoly;
}

sub readPoly
{
    my $line = shift;
    my $points;
    if($line =~ /\bpoints\s*=\s*"([^"]+)"/ )
    {
	$points = $1;
    }
    else
    {
	print "Polygon without points:\n";
	print $line;
	exit(3);
    }
    my @pt = split(/\s+/,$points);
    for(my $i=0; $i<@pt;$i++) {
	print "Processing point $pt[$i]\n";
	my ($x,$y) = split(/,/,$pt[$i]);
	$pt[$i] = [int(1000*$x),int(1000*$y)];
    }
    if(!is_counter_clockwise(\@pt)) {
	@pt = reverse @pt;
    }
    if(!is_counter_clockwise(\@pt)) {
	printf("WTF\n");
	exit(3);
    }
    if($line =~ /negative/) {
	unshift @pt, "NEGATIVE";
    }
    return \@pt;
}

