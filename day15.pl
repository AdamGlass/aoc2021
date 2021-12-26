:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(matrix).
:- use_module(astar).
:- dynamic cost/2.

matrixp(M) --> mapx(M).

mapx([M]) --> mapline(M).
mapx([M|Ms]) --> mapline(M), mapx(Ms).

mapline(M) --> mdigits(M), blank.

mdigits([D]) --> mdigit(D).
mdigits([D|Ds]) --> mdigit(D), mdigits(Ds).

mdigit(V) --> [C], {char_type(C, digit), char_code('0', Zero), V is C - Zero}.

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

day15_p1_slow(File, Score):-
    phrase_from_file(matrixp(LMap), File),
    lmatrix_matrix(LMap, Map),
    paths(Map, Score).

traversal_heuristic(_, _, 0).
traversal_heuristic(X+Y, GX+GY, Guestimate):-
    DX is abs(GX-X),
    DY is abs(GY-Y),
    Guestimate is (DX+DY).

traversal_neighbors(CostMatrix, X+Y, NeighborsList):-
    matrix_xy_adjacent_cardinal(CostMatrix, X, Y, NeighborsList).

traversal_cost(CostMatrix, X+Y, Cost):-
    matrix(CostMatrix, X, Y, Cost).

least_cost_traversal(CostMatrix, LeastCost):-
    matrix_limits(CostMatrix, GoalX, GoalY),
    astar(0+0,
	  GoalX+GoalY,
	  traversal_neighbors(CostMatrix),
	  traversal_cost(CostMatrix),
	  traversal_heuristic,
	  LeastCost).

day15_p1_fast(File, Score):-
    phrase_from_file(matrixp(LCostMatrix), File),
    lmatrix_matrix(LCostMatrix, CostMatrix),
    least_cost_traversal(CostMatrix, Score).

% wrong description
add_mod_(Addend, Old, NewValue):-
    Value is Addend + Old,
    (Value =< 9 ->
	 NewValue = Value
	 ;
	 NewValue = 1
    ).

lmatrix_add_row(Value, Row, NewRow):-
    maplist(add_mod_(Value), Row, NewRow).

lmatrix_add(Matrix, Value, NewMatrix):-
    maplist(lmatrix_add_row(Value), Matrix, NewMatrix).

lmatrix_append(Matrix, A, NewMatrix):-
    maplist(append, Matrix, A, NewMatrix).

tile_right_(_, InMatrix+WorkingMatrix, Next):-
    lmatrix_add(InMatrix, 1, NewInMatrix),
    lmatrix_append(WorkingMatrix, NewInMatrix, NewWorkingMatrix),
    Next = NewInMatrix+NewWorkingMatrix.

tile_right(M, NewM):-
    foldl(tile_right_, [1, 2, 3, 4], M+M, _+NewM).

tile_down_(_, InMatrix+WorkingMatrix, Next):-
    lmatrix_add(InMatrix, 1, NewInMatrix),
    append(WorkingMatrix, NewInMatrix, NewWorkingMatrix),
    Next = NewInMatrix+NewWorkingMatrix.

tile_down(M, NewM):-
    foldl(tile_down_, [1, 2, 3, 4], M+M, _+NewM).

matrix_formatter(X,X).

day15_p2_fast(File, Score):-
    phrase_from_file(matrixp(LCostMatrix), File),
    tile_right(LCostMatrix, TiledRight),
    tile_down(TiledRight, LTiledMatrix),
    lmatrix_matrix(LTiledMatrix, CostMatrix),
    least_cost_traversal(CostMatrix, Score).

day15_p1(Score):-
    day15_p1_fast("data/day15_p1_data", Score).

day15_p1_test(Score):-
    day15_p1_fast("data/day15_p1_test", Score).

day15_p2(Score):-
    day15_p2_fast("data/day15_p1_data", Score).

day15_p2_test(Score):-
    day15_p2_fast("data/day15_p1_test", Score).

day15_p1_test2_slow(Score):-
    day15_p1_slow("data/day15_p1_test2", Score).

day15_p1_test2(Score):-
    day15_p1_fast("data/day15_p1_test2", Score).

day15:-
    day15_p1_test(40),
    day15_p1(613),
    day15_p2_test(315),
    day15_p2(2899).
