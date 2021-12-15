:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(persistency)).
:- dynamic cost/2.

matrixp(M) --> mapx(M).

mapx([M]) --> mapline(M).
mapx([M|Ms]) --> mapline(M), mapx(Ms).

mapline(M) --> mdigits(M), blank.

mdigits([D]) --> mdigit(D).
mdigits([D|Ds]) --> mdigit(D), mdigits(Ds).

mdigit(V) --> [C], {char_type(C, digit), char_code('0', Zero), V is C - Zero}.

lmatrix(Matrix, X, Y, Value) :-
    nth0(Y, Matrix, Row),
    nth0(X, Row, Value).

lmatrix_matrix(ListMatrix, AssocMatrix):-
    length(ListMatrix, RowCount),
    [Row|_] = ListMatrix,
    length(Row, ColumnCount),
    LY is RowCount - 1,
    LX is ColumnCount - 1,
    findall(X+Y-Value,
	    (between(0, LX, X),
	     between(0, LY, Y),
	     lmatrix(ListMatrix, X, Y, Value)
	    ),
	    KeyValueList),
    ord_list_to_assoc(KeyValueList, AssocMatrix).

matrix(Matrix, X, Y, Value) :-
    get_assoc(X+Y, Matrix, Value).

max_xykey([], AccX, AccY, AccX, AccY).
max_xykey([X+Y|Keys], AccX, AccY, MaxX, MaxY):-
    XMax is max(X, AccX),
    YMax is max(Y, AccY),
    max_xykey(Keys, XMax, YMax, MaxX, MaxY).

matrix_limits(Matrix, XMAX, YMAX):-
    assoc_to_keys(Matrix,XYList),
    max_xykey(XYList, 0, 0, XMAX, YMAX).

matrix_element_transform(Op, Matrix, NewMatrix):-
    assoc_to_list(Matrix, MatrixList),
    maplist(Op, MatrixList, OpList),
    ord_list_to_assoc(OpList, NewMatrix).

matrix_element_add_(Addend, X+Y-Value, X+Y-NewValue):-
    NewValue is Value + Addend.

matrix_element_add(Value, Matrix, NewMatrix):-
    matrix_element_transform(matrix_element_add_(Value), Matrix, NewMatrix).

matrix_transform(Matrix, X, Y, Value, NewMatrix):-
    put_assoc(X+Y, Matrix, Value, NewMatrix).

matrix_xy_transform_(Op, Matrix, X, Y, NewMatrix):-
    matrix(Matrix, X, Y, Value),
    call(Op, Value, NewValue),
    matrix_transform(Matrix, X, Y, NewValue, NewMatrix).

matrix_xy_add(Matrix, X, Y, Addend, NewMatrix):-
    matrix_xy_transform_(plus(Addend), Matrix, X, Y, NewMatrix).

path_least_cost(L, Cost):-
    cost(L, PreviousCost),
    PreviousCost < Cost,
    fail.

path_least_cost(L, Cost):-
    cost(L, PreviousCost),
    PreviousCost > Cost,
    retract(cost(L, PreviousCost)),
    asserta(cost(L, Cost)).

path_least_cost(L, Cost):-
    \+ cost(L, _),
    asserta(cost(L, Cost)).

path_(Map, [End|Path], Cost, Cost, EndPath):-
    End = location(X, Y, _),
    matrix_limits(Map, X, Y),
    EndPath = [End|Path].

path_(Map, [Last|Path], AccCost, Cost, EndPath):-
    Last = location(X,Y, _),
    member(move(DX,DY), [move(-1,0), move(1,0), move(0,-1), move(0,1)]),
    NX is X + DX,
    NY is Y + DY,
    matrix(Map, NX, NY, MoveCost),
    NewCost is AccCost + MoveCost,
    L = location(NX, NY, MoveCost),
    path_least_cost(L, NewCost),
    path_(Map, [location(NX, NY, MoveCost),Last|Path], NewCost, Cost, EndPath).

path_cost(Path, Cost):-
    findall(Cost,
	    member(location(_,_, Cost), Path),
	    CostList),
    sum_list(CostList, Cost).

paths(Map, MinimumPath):-
    findall(Cost-P,
	    path_(Map, [location(0,0, 0)], 0, Cost, P),
	    Paths),
    min_member(MinimumPath, Paths).

day15_p1(File, Score):-
    phrase_from_file(matrixp(LMap), File),
    lmatrix_matrix(LMap, Map),
    paths(Map, Cost-Path),
    writeln(Cost-Path).

day15_p1(Score):-
    day15_p1("data/day15_p1_data", Score).

day15_p1_test(Score):-
    day15_p1("data/day15_p1_test", Score).

day15_p2:-
    day15_p2("data/day15_p1_data").

day15_p2_test:-
    day15_p2("data/day15_p1_test").

day15:-
    day15_p1_test(17),
    day15_p1(720),
    day15_p2_test,
    day15_p2.
