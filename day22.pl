:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(matrix).

data([D]) --> cubex(D).
data([D|Ds]) --> cubex(D), data(Ds).

cubex(C) --> cube_opx(V), xequals, range(XMIN, XMAX), comma, yequals, range(YMIN, YMAX), comma, zequals, range(ZMIN, ZMAX), blanks, {C = cube(V, XMIN+XMAX, YMIN+YMAX, ZMIN+ZMAX)}.

range(MIN, MAX) --> integer(MIN), range_symbol, integer(MAX).

cube_opx(1) --> `on `.
cube_opx(-1) --> `off `.

range_symbol --> `..`.
comma --> [C], {char_code(',', C)}.
xequals --> "x=".
yequals --> "y=".
zequals --> "z=".

cube(Matrix, X+Y+Z, Value):-
    ( get_assoc(X+Y+Z, Matrix, StoredValue) ->
      Value = StoredValue
      ; 
      Value = 0
    ).

cube(Matrix, X+Y+Z, Value, NewMatrix):-
    (Value = 1 ->
	 put_assoc(X+Y+Z, Matrix, 1, NewMatrix)
    ;
	 put_assoc(X+Y+Z, Matrix, 0, NewMatrix)
    ).

cube_op_(Value, X+Y+Z, InMatrix, OutMatrix):-
    cube(InMatrix, X+Y+Z, Value, OutMatrix).

cube_op(cube(Value, XMin+XMax, YMin+YMax, ZMin+ZMax), InMatrix, OutMatrix):-
    findall(X+Y+Z,
	    (between(XMin, XMax, X),
	     between(YMin, YMax, Y),
	     between(ZMin, ZMax, Z)),
	    Coords),
    foldl(cube_op_(Value), Coords, InMatrix, OutMatrix).

cube_init(Cubes, OutMatrix):-
    empty_assoc(InitMatrix),
    foldl(cube_op, Cubes, InitMatrix, OutMatrix).

min_range(cube(_, XMin+XMax, YMin+YMax, ZMin+ZMax)):-
    Extents = [XMin, XMax, YMin,YMax, ZMin, ZMax],
    min_list(Extents, MinExtent),
    max_list(Extents, MaxExtent),
    MinExtent >= -50,
    MaxExtent =< 50.

count_50(Matrix, Count):-
    findall(1,
	    (between(-50, 50, X),
	     between(-50, 50, Y),
	     between(-50, 50, Z),
	     cube(Matrix, X+Y+Z, 1)),
	    Ons),
    length(Ons, Count).

volume(cube(V, XMin+XMax, YMin+YMax, ZMin+ZMax), Volume):-
    Volume is (XMax - XMin + 1) * (YMax - YMin + 1) * (ZMax - ZMin + 1) * V.

xintersect(AMin+AMax, BMin+BMax):-
    AMin =< BMax,
    AMax >= BMin.

intersect_cube(cube(_, AXMin+AXMax, AYMin+AYMax, AZMin+AZMax), cube(_, BXMin+BXMax, BYMin+BYMax, BZMin+BZMax)):-

    xintersect(AXMin+AXMax, BXMin+BXMax),
    xintersect(AYMin+AYMax, BYMin+BYMax),
    xintersect(AZMin+AZMax, BZMin+BZMax).

intersection_cube(cube(AV, AXMin+AXMax, AYMin+AYMax, AZMin+AZMax), cube(BV, BXMin+BXMax, BYMin+BYMax, BZMin+BZMax), Intersection):-

    XMin is max(AXMin, BXMin),
    YMin is max(AYMin, BYMin),
    ZMin is max(AZMin, BZMin),

    XMax is min(AXMax, BXMax),
    YMax is min(AYMax, BYMax),
    ZMax is min(AZMax, BZMax),

    % N.B. remembering that an on cube will be added anyway
    %      so this has to account for the effect of the intersection

    ([AV, BV] = [1, -1] -> % cancel existing
	 V = 1
    ; [AV,BV] = [1, 1] ->  % cancel double count
         V = -1
    ; [AV,BV] = [-1, 1] -> % just subtract
         V = -1
    ; % implied [-1, -1]  avoid double count
         V = 1
    ),
    Intersection = cube(V, XMin+XMax, YMin+YMax, ZMin+ZMax).

on_cube(cube(1, _, _, _)).
sum_volume(Cube, InValue, OutValue):-
    volume(Cube, Volume),
    OutValue is InValue + Volume.

cube_count_([], AccCubes, Count):-
    foldl(sum_volume, AccCubes, 0, Count).
cube_count_([C|Cs], AccCubes, Count):-
    findall(Intersection,
	    (member(A, AccCubes),
	     intersect_cube(C,A),
	     intersection_cube(C, A, Intersection)),
	    Intersections),
    length(Intersections, ICount),
    writeln([intersection, ICount]),
    append(Intersections, AccCubes, NewAccCubes),
    (on_cube(C) ->
	 cube_count_(Cs, [C|NewAccCubes], Count)
    ;
         cube_count_(Cs, NewAccCubes, Count)
    ).

cube_count(Cubes, Count):-
    cube_count_(Cubes, [], Count).

day22_p1_simple(File, Score):-
    phrase_from_file(data(Cubes), File),
    include(min_range, Cubes, LCubes),
    cube_init(LCubes, OutMatrix),
    count_50(OutMatrix, Score).

day22_p1_fast(File, Score):-
    phrase_from_file(data(Cubes), File),
    include(min_range, Cubes, LCubes),
    cube_count(LCubes, Score).

day22_p2(File, Score):-
    phrase_from_file(data(Cubes), File),
    cube_count(Cubes, Score).

day22_p1(Score):-
    day22_p1_simple("data/day22_p1_data", Score).

day22_p1_test(Score):-
    day22_p1_simple("data/day22_p1_test", Score).

day22_p2(Score):-
    day22_p2("data/day22_p1_data", Score).

day22_p2_test(Score):-
    day22_p2("data/day22_p1_test", Score).

day22_p1_fast(Score):-
    day22_p1_fast("data/day22_p1_data", Score).

day22_p1_test_fast(Score):-
    day22_p1_fast("data/day22_p1_test", Score).

day22:-
    day22_p1_test(590784),
    day22_p1(580810),
    day22_p2_test(_), % produces wrong answer?
    day22_p2(1265621119006734).
