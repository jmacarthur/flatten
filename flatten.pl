#!/usr/bin/perl -w

use Parse::RecDescent;
use Getopt::Long;
use Data::Dumper;

use strict;

# Usage: flatten2.pl --thickness 3 <filename> 
# Produces a file called test<thickness>.svg

# ------ Command line processing

my $thick = 0;
my $result = GetOptions ("thickness=i" => \$thick);

if($thick == 0) {
    print "Please specify a material thickness with --thickness\n";
    exit(1);
}

my $filename = shift;

# ------ A couple of global variables

my $latchAxes;
my @primitives = ();


# ------ Set up rec descent parser

$::RD_HINT =1;
# Simple parser for OpenSCAD CSG files:
my $parser = Parse::RecDescent->new(q(
    startrule : group
    group: "group" groupargs ( block | terminator ) { { type=>"group", contents=>$item[3] } }
    block: leftbrace statement(s) rbrace  { $item[2] }
    statement: group | cube | difference | union | multmatrix | color | extrude | polygon | cylinder { $item[1] }

    cube: "cube" cubeargs terminator { { type=>"Cube", args=>$item[2] } }

    cylinder: "cylinder" cubeargs terminator { { type=>"cylinder", args=>$item[2] } }
    color: "color" cubeargs block { {type=>"color",args=>$item[2],contents=>$item[3] } } # Ignoring color
    extrude: "linear_extrude" cubeargs block { {type=>"linear_extrude",args=>$item[2],contents=>$item[3] } }
    polygon: "polygon" cubeargs terminator { { type=>"polygon",args=>$item[2] } }
    difference: "difference" groupargs block { { type=>"difference",contents=>$item[3] } }
    union: "union" cubeargs block { {type=>"union",contents=>$item[3] } }
    multmatrix: "multmatrix" cubeargs block { { type=>"Matrix", args=>$item[2], contents=>$item[3] } }
    cubeargs: "(" /[^\)]*/ ")" { $item[2]; }
    terminator: ";" { [{type=>"terminator"}] }
    groupargs: "(" ")"
    leftbrace: "{" { "}" }
    rbrace: "}"
));

sub getCylinderHeight
{
    my $obj = shift;
    my $args = $obj->{args};
    if($args =~ /h\s*=\s*(\d+\.?\d*)/)
    {
	return $1;
    }
    return 0;
}

sub getCubeSizes
{
    my $arg = shift;
    if($arg =~/size\s*=\s*\[(\d+.?\d*),\s*(\d+.?\d*),\s*(\d+.?\d*)\s*\]/)
    {
	return [$1,$2,$3];
    }
    print "Unrecognised cube args: $arg\n";
    exit(6);
}

sub getCylinderRadius
{
    my $arg = shift;
    if($arg =~/r1\s*=\s*(\d+\.?\d*)/)
    {
	return $1;
    }
    print "Unrecognised cylinder args: $arg\n";
    exit(6);
}

sub readMatrix
{
    my $args = shift;
    $args =~ s/[\[\]]//g;
    $args =~ s/\s+//g;
    my @arglist=split(/,/,$args);
    my @matrix = ();
    for(my $x = 0;$x<4;$x++) {
	my @column = ();
	for(my $y = 0;$y<4;$y++) {
	    my $val = shift(@arglist);
	    if (abs($val) < 0.000001) {
		$val = 0;
	    }
	    push @column,$val;
	}
	push @matrix, \@column;
    }
    return @matrix;
}

sub getCubeSize
{
    my $obj = shift;
    my $args = $obj->{args};
    if($args =~ /size\s*=\s*\[(\d+\.?\d*),\s*(\d+\.?\d*),\s*(\d+\.?\d*)\]/)
    {
	my($x,$y,$z) = ($1,$2,$3);
	return($x,$y,$z);
    }
    else
    {
	print "Unrecognised cube arguments: $args\n";
	exit(0);
    }
}

sub checkTranslationMatrix {
    my $node = shift;
    my @matrix = readMatrix($node->{args});
    for(my $x=0;$x<3;$x++) {
	for(my $y=0;$y<3;$y++) {
	    if($matrix[$x][$y] != ($x==$y ? 1 : 0) ) {
		return 0;
	    }
	}
    }

    # Then this is a translation matrix
    $node->{type}="translate";
    $node->{args}=[$matrix[0][3],$matrix[1][3],$matrix[2][3]];
    return 1;
}

sub determinant
{
    my $matrix = shift;
    return $matrix->[0][0]*$matrix->[1][1]*$matrix->[2][2] +
	$matrix->[1][0]*$matrix->[2][1]*$matrix->[0][2] +
	$matrix->[2][0]*$matrix->[0][1]*$matrix->[1][2] -
	$matrix->[2][0]*$matrix->[1][1]*$matrix->[0][2] -
	$matrix->[1][0]*$matrix->[0][1]*$matrix->[2][2] -
	$matrix->[0][0]*$matrix->[2][1]*$matrix->[1][2];
    
}

sub reassignMatrix
{
    my $matrix = shift;
    my @vars = ('x','y','z');
    my $line = "";
    my @counts = (0,0,0);
    for(my $x=0;$x<3;$x++) {
	for(my $y=0;$y<3;$y++) {
	    if($matrix->[$x][$y] == 0) {
		# Do nothing
	    }
	    elsif($matrix->[$x][$y] == 1) {
		$counts[$y] += 1;
	    }
	    elsif($matrix->[$x][$y] == -1) {
		$counts[$y] += 1;
	    }
	    else {
		# Not a reassignment matrix
		return 0;
	    }
	}
    }
    for(my $i=0;$i<0;$i++) {
	if($counts[$i] != 1) {
	    print "not a proper reassignment matrix: axis count: @counts\n";
	    return 0;
	}
    }
    return 1;
}


# Attempts to categorize a transformation matrix as one of several
# base types - translation or reassignment.
# Reassignment is a special case of rotation which only rotates by
# 90 degree steps, 'reassigning' one axis onto another directly.

# Configures the 'type' property of the node passed in when a match
# is found.

sub categorizeMatrix
{
    my $node = shift;
    my @matrix = readMatrix($node->{args});
    
    my $determinant = abs(determinant(\@matrix));
    if($determinant < 0.999999 || $determinant > 1.000001) {
	print "Matrix with non-unit determinant ($determinant)\n";
	exit(1);
    }
    
    if(checkTranslationMatrix($node)) { return 1; }
    my $reassign = reassignMatrix(\@matrix);
    if($reassign) 
    {
	#print "Is reassignment matrix: $reassign\n";
	$node->{type}="Reassign";
	return 1;
    }
    print "Unknown matrix type:\n";
    for(my $x=0;$x<3;$x++) {
	for(my $y=0;$y<3;$y++) {
	    print "$matrix[$x][$y] ";
	}
	print "\n";
    }
}

# Sets up the 'planar' tags for all objects, and populates the 'primitives' 
# array with all primitive objects. It also sets the 'parent' where possible
# so we can move up the tree.

sub annotateTree
{
    my ($tree, $depth, $parent, $negate) = @_;
    if(ref($tree) eq "ARRAY") {
	for my $c (@{$tree}) {
	    annotateTree($c,$depth+1,$parent,$negate);
	}
	return;
    }
    if(ref($tree) ne "HASH") {
	print "Object is not a hash\n";
	return;
    }
    $tree->{parent} = $parent;
    if(!defined($tree->{type})) {
	print "Node has no type\n";
	return;
    }

    $tree->{negate} = $negate;
    if($tree->{type} eq 'Cube') {
	my ($x,$y,$z) = getCubeSize($tree);
	my $planar="";
	if($x==$thick) { $planar.="X"; }
	if($y==$thick) { $planar.="Y"; }
	if($z==$thick) { $planar.="Z"; }
	$tree->{planar} = $planar;
    }
    if($tree->{type} eq 'cylinder') {
	my $h = getCylinderHeight($tree);
	$tree->{planar} = ($h==$thick)?"Z":"";
    }
    if($tree->{type} eq 'linear_extrude') {
	$tree->{planar} = "Z";
    }

    # Process matrices
    if($tree->{type} eq "Matrix") {
	# Attempt to categorise the matrix
	categorizeMatrix($tree);
    }

    if($tree->{type} eq "linear_extrude") {
	push @primitives,$tree;
    }
    elsif(defined($tree->{contents})) {
	if($tree->{type} eq 'difference') {
	    my @contents = @{$tree->{contents}};
	    if(@contents) {
		my $count=0;
		for my $c (@contents) {
		    if($count==0) {
			annotateTree($c,$depth+1,$tree,$negate);
		    }
		    else
		    {
			annotateTree($c,$depth+1,$tree,!$negate);
			if($negate) {
			    print "ERROR: Above is a double-negated object\n";
			    return;
			}
		    }
		    $count++;
		}	   
	    }
	}
	else
	{
	    for my $c (@{$tree->{contents}}) {
		annotateTree($c,$depth+1,$tree,$negate);
	    }
	}
    }
    else # Contents not defined
    {
	if(defined($tree->{args}) && $tree->{type} ne "polygon") {
	    push @primitives,$tree;
	}
    }
}

# Now I want to iterate all the objects in there and check all the non-negated objects are coplanar

sub applyAxesReassign
{
    my ($reassign, $axes) = @_;
    my $output = "";
    for(my $i=0;$i<length($axes);$i++) {
	my $a = substr($axes,$i,1);
	my $neg = ($a eq uc($a));
	my $index = ord(lc($a))-ord('x');
	my $reassigner = $reassign->[$index];
	my $reneg = ($reassigner eq uc($reassigner));
	$output .= ( ($reneg xor $neg) ? uc($reassigner) : lc($reassigner));
    }
    return $output;
}

sub matrixMult
{
    my ($a,$b) = @_;
    my @output = ([0,0,0],[0,0,0],[0,0,0]);
    for(my $c = 0;$c<3;$c++) {
	for(my $r = 0; $r<3;$r++) {
	    my $total = 0;
	    for(my $i=0;$i<3;$i++) {
		$total += $a->[$i][$r]*$b->[$c][$i];
	    }
	    $output[$c][$r] = $total;
	}
    }
    return \@output;
}

sub matrixMultiply1D
{
    my ($a,$b) = @_;
    my @output = ([0,0,0]);
    for(my $r = 0; $r<3;$r++) {
	my $total = 0;
	for(my $c = 0;$c<3;$c++) {
	    $total += $a->[$r][$c]*$b->[$c];
	}
	$output[$r] = $total;
    }
    return \@output;
}


sub matrixToString
{
    my $matrix=shift;
    my $output="";
    for(my $c = 0;$c<3;$c++) {
	$output.="[";
	for(my $r = 0; $r<3;$r++) {
	    $output.=$matrix->[$c][$r]." ";
	}
	$output.="]";
    }
    return $output;
}

sub matrixMultiplyPlanes
{
    my($matrix,$planes) = @_;
    my $x = $matrix->[0][0]*($planes=~/X/i ? 1: 0) + 
	$matrix->[0][1]*($planes=~/Y/i ? 1: 0) +
	$matrix->[0][2]*($planes=~/Z/i ? 1: 0);
    my $y = $matrix->[1][0]*($planes=~/X/i ? 1: 0) + 
	$matrix->[1][1]*($planes=~/Y/i ? 1: 0) +
	$matrix->[1][2]*($planes=~/Z/i ? 1: 0);
    my $z = $matrix->[2][0]*($planes=~/X/i ? 1: 0) + 
	$matrix->[2][1]*($planes=~/Y/i ? 1: 0) +
	$matrix->[2][2]*($planes=~/Z/i ? 1: 0);
    return [$x,$y,$z];
}

sub markLatchAxes
{
    my $axes = shift;
    my $count = 0;
    for(my $i=0;$i<3;$i++)
    {
	if($axes->[$i]==0) {
	    $latchAxes->[$i] = 0;
	}
	if($latchAxes->[$i] != 0) {
	    $count++;
	}
    }
    if($count==0) {
	print "No remaining latched axes. This object cannot be planar.\n";
	return;
    }
    return;
}

sub sqmagnitude
{
    my $vect = shift;
    return $vect->[0]*$vect->[0]+$vect->[1]*$vect->[1]+$vect->[2]*$vect->[2];
}

# Iterates over the object tree and looks for 'planar' tags. Uses these to
# update the global variable latchAxes. Transformation matrixes are applied
# to objects underneath them in the tree.

sub stage2Tree
{
    my ($node, $axes) = @_;
    if(ref($node) eq "ARRAY") {
	for my $c(@$node) {
	    if(!stage2Tree($c,$axes)) {
		return 0;
	    }
	}
	return 1;
    }
    if($node->{type} eq "Cube") {
	if($node->{negate} == 0) {
	    if($node->{planar} eq "") {
		print "Non-planar positive cuboid, cannot continue\n";
		return 0;
	    }	
	    my $finalAxes = matrixMultiplyPlanes($axes,$node->{planar});
	    #print "Planar cube: base planar $node->{planar}, reassign by ".matrixToString($axes)." = ".join(",",@$finalAxes)."\n";
	    markLatchAxes($finalAxes);
	}
	return 1;
    }
    if($node->{type} eq "color") {
	#print "Color applied: ".$node->{args}."\n"
    }
    if($node->{type} eq "cylinder") {
	if($node->{negate} == 0) {
	    if($node->{planar} eq "") {
		print "Non-planar positive cylinder, cannot continue\n";
                print $node->{args}." $node->{planar}\n";
		return 0;
	    }
	    my $finalAxes = matrixMultiplyPlanes($axes,$node->{planar});
	    #print "Planar cylinder: base planar $node->{planar}, reassign by ".matrixToString($axes)." = ".join(",",@$finalAxes)."\n";
	    markLatchAxes($finalAxes);
	}
	return 1; 
    }
    if($node->{type} eq "linear_extrude") {
	if($node->{negate} == 0) {
	    if($node->{planar} eq "") {
		print "Non-planar positive extrusion, cannot continue\n";
		return 0;
	    }
	    my $finalAxes = matrixMultiplyPlanes($axes,$node->{planar});
	    print "Planar extrusion: base planar $node->{planar}, reassign by ".matrixToString($axes)." = ".join(",",@$finalAxes)."\n";
	    markLatchAxes($finalAxes);
	}
	return 1;
    }

    if($node->{type} eq "Reassign") {
	my @m = readMatrix($node->{args});
	#print "Applying reassign...".matrixToString($axes)."x".matrixToString(\@m)."\n";
	$axes = matrixMult($axes, \@m);
	#print "Result:  ".matrixToString($axes)."\n";
    }

    if(defined($node->{contents})) {
	my @contents = @{$node->{contents}};
	for my $c(@contents)
	{
	    if(!stage2Tree($c,$axes)) 
	    {
		return 0;
	    }
	}
    }
    return 1;
}

sub projectTree
{
    my ($node, $axes,$translate) = @_;
    if(ref($node) eq "ARRAY") {
	for my $c(@$node) {
	    projectTree($c,$axes,$translate);
	}
	return 1;
    }
    if($node->{type} eq "Cube") {
	# Parse args, project
	my $cubeSize = getCubeSizes($node->{args});
	my $modCube = matrixMultiply1D($axes,$cubeSize);
	my $corner1 = [0,0,0];
	# Right, our cubeSize array consists of three vectors
	my $v1 = [ $cubeSize->[0], 0, 0 ];
	my $v2 = [ 0,$cubeSize->[1], 0 ];
	my $v3 = [ 0,0,$cubeSize->[2] ];
	# Now, we can project these...
	my $pv1 = matrixMultiply1D($axes,$v1);
	my $pv2 = matrixMultiply1D($axes,$v2);
	my $pv3 = matrixMultiply1D($axes,$v3);
	# One of these should be zero...
	my @vects = ();
	for my $v ($pv1,$pv2,$pv3) {
	    if(sqmagnitude($v) > 0.0001) {
		push @vects,$v;
	    }
	}
	if($#vects!=1) {
	    print "Should have found 2 nonzero vectors, found $#vects\n";
	    return 0;
	}

	print SVG "<polygon points=\"";	
	print SVG "$translate->[0],$translate->[1] ";
	my ($x,$y) = ($translate->[0]+$vects[0]->[0],
		      $translate->[1]+$vects[0]->[1]);
	print SVG "$x,$y ";
        ($x,$y) = ($translate->[0]+$vects[1]->[0]+$vects[0]->[0],
		   $translate->[1]+$vects[1]->[1]+$vects[0]->[1]);
	print SVG "$x,$y ";
        ($x,$y) = ($translate->[0]+$vects[1]->[0],
		   $translate->[1]+$vects[1]->[1]);
	print SVG "$x,$y ";
        print SVG "\" style=\"stroke-width:1;stroke:rgb(0,0,0)\"";
	if($node->{negate}) {
	    print SVG " negative=\"yes\"";
	}
	
	print SVG "/> \n";
	return 1;
    }

    if($node->{type} eq "color") {
        # We don't take any action on color at the moment
    }

    if($node->{type} eq "cylinder") {
	my $finalAxes = matrixMultiplyPlanes($axes,$node->{planar});
	my $radius = getCylinderRadius($node->{args});
	print SVG "<circle cx=\"$translate->[0]\" cy=\"$translate->[1]\" r=\"$radius"."\" style=\"fill:stroke-width:1;stroke:rgb(0,0,0)\"";
	if($node->{negate}) {
	    print SVG " negative=\"yes\"";
	}
	print SVG "/>\n";
	return 1;
    }
    if($node->{type} eq "linear_extrude") {
	if($node->{negate} == 0) {
	    if($node->{planar} eq "") {
		print "Non-planar positive extrusion, cannot continue\n";
		return 0;
	    }
	    my @processedPoints = ();
	    my $finalAxes = matrixMultiplyPlanes($axes,$node->{planar});
	    #print "Planar extrusion: base planar $node->{planar}, reassign by ".matrixToString($axes)." = ".join(",",@$finalAxes)."\n";
	    my $args = $node->{contents}->[0]->{args};

	    if(!defined($args)) {
		$args = $node->{contents}->[0]->{contents}->[0]->{args};
		if(!defined($args))
		{
		    print "Failed to find arguments:\n";
		    #print Dumper($node);
		    exit(100);
		}
	    }
	    print "Need to process polygon args: $args\n";

	    if($args =~ /points\s*=\s*\[\s*(\[.*?\])\s*\],/) {
		my $pointsText = $1;
		print "Polygon points: $pointsText\n";
		my @pointsArray = split(/\]\s*,/,$pointsText);
		for my $pointText(@pointsArray) {
		    if($pointText =~ /\[?\s*(-?\d+\.?\d*e?-?\d*)\s*,\s*(-?\d+\.?\d*e?-?\d*)\s*\]?/)
		    {
			my ($x,$y) = ($1,$2);
			# Now process them...
			print "Process points: $x,$y on axis ".matrixToString($axes)."\n";
			my $pv1 = [$y,$x,0]; # Note: using x,y directly, not projecting - 
			# original below:
			#matrixMultiply1D($axes,[ 0,$x,$y ]); # TODO: why am I putting x,y there?
			print "Resuilts of multiply: $pv1->[0],$pv1->[1],$pv1->[2]\n";
			$pv1->[0] += $translate->[0];
			$pv1->[1] += $translate->[1];
			push @processedPoints, $pv1;
		    }
		    else
		    {
			print "Unrecognisable polygon point text: $pointText\n";
			exit(4);
		    }

		}
	    }
	    else
	    {
		print "Unrecognisable polygon arg text: $args\n";
		exit(5);
	    }
	    
	    print SVG "<polygon points=\"";	
	    for my $p(@processedPoints) {
		print SVG "$p->[0],$p->[1] ";
	    }
	    print SVG "\" style=\"stroke-width:1;stroke:rgb(0,0,0)\"";
	    if($node->{negate}) {
		print SVG " negative=\"yes\"";
	    }
	    
	    print SVG "/> \n";
	    
	}
	return 1;
    }
    if($node->{type} eq "Reassign") {
	my @m = readMatrix($node->{args});
	$axes = matrixMult($axes, \@m);
    }
    if($node->{type} eq "Matrix") {
	my @m = readMatrix($node->{args});
	$axes = matrixMult(\@m,$axes);
    }
    if($node->{type} eq "translate") {
	my $delta = matrixMultiply1D($axes,$node->{args});
	my $dx = $translate->[0]+$delta->[0];
	my $dy = $translate->[1]+$delta->[1];
	$translate = [$dx,$dy,0];
    }
    if(defined($node->{contents})) {
	my @contents = @{$node->{contents}};
	for my $c(@contents)
	{
	    projectTree($c,$axes,$translate);
	}
    }
}

# ------ Read in the file and start the parser

my $testData = "";
open(INPUTCSG, $filename);
while (<INPUTCSG>){
    $testData .= $_;
}

my $tree=$parser->startrule($testData);

# ------ Now we have the CSG in tree form in $tree

annotateTree($tree,0,undef,0);

my @topLevelObjects = ();
my $collectionNo = 0;


# Now, we'll pass through the list of primitives and find the top level ones by
# moving up the tree until there are no parents left. We also set the
# 'collection number' property for each top level object.

my %topLevelObjectsMark = (); # Just used to remember if we've seen this object before.
for my $p(@primitives) {
    my $topLevel = $p;
    my $trace = $p;
    while(defined($trace->{parent})) {
	if($trace->{type} eq 'union' || $trace->{type} eq 'difference') {
	    $topLevel = $trace;
	}
	$trace = $trace->{parent};
    }
    if(!defined($topLevelObjectsMark{$topLevel})) {
	push @topLevelObjects, $topLevel;
	$collectionNo++;
    }
    $p->{collectionNo} = $collectionNo;
    $topLevelObjectsMark{$topLevel} = 1;
}

# Now we can open the output file and start writing to it.

open(SVG,">$filename-$thick.svg");
print SVG '<svg width="200mm" height="200mm" viewBox="0 0 200 200" xmlns="http://www.w3.org/2000/svg" version="1.1">'."\n";

my $identMatrix = [[1,0,0],[0,1,0],[0,0,1]];

for(my $objectNum = 0; $objectNum<=$#topLevelObjects;$objectNum++)
{
    print SVG "<g>\n";
    $latchAxes = [1,1,1];
    my $topLevel1 = $topLevelObjects[$objectNum];

    # For each top-level object, parse the tree and figure out what axes it is planar in
    my $valid = stage2Tree($topLevel1,$identMatrix);

    if($valid)
    {
	print "Object $objectNum is valid and should be projected in ".join(",",@$latchAxes)."\n";
	
        # Now establish a projection matrix from 3D to 2D
	my $project;
	if($latchAxes->[0] == 0 && $latchAxes->[1]==0 && $latchAxes->[2] == 1) {
	    $project = [[1,0,0],[0,1,0],[0,0,0]];
	}
	elsif($latchAxes->[0] == 1 && $latchAxes->[1]==0 && $latchAxes->[2] == 0) {
	    $project = [[0,0,1],[0,1,0],[0,0,0]];
	}
	elsif($latchAxes->[0] == 0 && $latchAxes->[1]==1 && $latchAxes->[2] == 0) {
	    $project = [[1,0,0],[0,0,1],[0,0,0]];
	}
	else
	{
	    print "Unknown projection\n";
	}

        # projectTree will write out the SVG.
	if(defined($project)) {
	    projectTree($topLevel1,$project,[0,0]);
	}
    }
    print SVG "</g>\n";
}
print SVG "</svg>\n";
close(SVG);
