flatten
=======

Perl scripts to unfold 3D OpenSCAD shapes.

Example usage and self test:

./flatten.pl --thickness=3 tests/lifters.csg
./process.pl --kerf=130 tests/lifters.csg-3.svg

diff lifters.csg-3.svg lifters.csg-3.svg.correct
diff lifters.csg-3-processed.svg lifters.csg-3-processed.svg.correct 


