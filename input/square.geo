// Gmsh

L = 1.0;
h = 2.0*L/2.0;

Point(1) = {-L,-L, 0,h};
Point(2) = { L,-L, 0,h};
Point(3) = { L, L, 0,h};
Point(4) = {-L, L, 0,h};

Line(1) = {1,2};
Line(2) = {2,3};
Line(3) = {3,4};
Line(4) = {4,1};

Line Loop(5) = {1, 2, 3, 4};
Plane Surface(1) = {5};

Physical Surface("domain(1.0)::interior") = {1};

Physical Line("known( 1.0)::east") = {2};
Physical Line("known( 0.0)::west") = {4};
Physical Line("empty::north") = {3};
Physical Line("empty::south") = {1};

Physical Point("known( 0.0)::sw") = {1};
Physical Point("known( 1.0)::se") = {2};
Physical Point("known( 1.0)::ne") = {3};
Physical Point("known( 0.0)::nw") = {4};
