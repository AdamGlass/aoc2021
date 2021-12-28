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
cube_opx(0) --> `off `.

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
    put_assoc(X+Y+Z, Matrix, Value, NewMatrix).

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

day22_p1(File, Score):-
    phrase_from_file(data(Cubes), File),
    include(min_range, Cubes, LCubes),
    cube_init(LCubes, OutMatrix),
    count_50(OutMatrix, Score).

day22_p2(File, Score):-
    phrase_from_file(data(Players), File),
    play_p2(Players, Score).

day22_p1(Score):-
    day22_p1("data/day22_p1_data", Score).

day22_p1_test(Score):-
    day22_p1("data/day22_p1_test", Score).

day22_p2(Score):-
    day22_p2("data/day22_p1_data", Score).

day22_p2_test(Score):-
    day22_p2("data/day22_p1_test", Score).

day22:-
    day22_p1_test(739785),
    day22_p1(920580),
    day22_p2_test(_),
    day22_p2(_).
