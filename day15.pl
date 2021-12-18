:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(matrix).
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

astar_known_cost(Costs, X+Y, Cost, NewCosts):-
    put_assoc(X+Y, Costs, Cost, NewCosts).

astar_known_cost(Costs, X+Y, Cost):-
    ( get_assoc(X+Y, Costs, KCost) ->
      KCost = Cost
    ;
      Cost = 100000
    ).

astar_guess_cost(gstate(Costs, _), X+Y, Cost):-
    ( get_assoc(X+Y, Costs, GCost) ->
      Cost = GCost
    ;
      Cost = 100000
    ).

remove(R, List, NewList):-
    partition(=(R), List, _, NewList).

min_guess_(X1+Y1-Cost1, X2+Y2-Cost2, X3+Y3-Cost3):-
    (Cost1 =< Cost2 ->
	 X3 = X1,
	 Y3 = Y1,
	 Cost3 = Cost1
    ;
         X3 = X2,
         Y3 = Y2,
	 Cost3 = Cost2
    ).

min_guess_(_,B, B).

min_guess([Guess], Guess).
min_guess([G|Gs], Guess):-
   foldl(min_guess_, [G|Gs], G, Guess).

%min_guess_([], Acc, Acc).
%min_guess_([G|Gs], Acc, Guess):-
%    G = _+_-GC,
%    Acc = _+_-AC,
%    (GC =< AC ->
%	 min_guess_(Gs, G, Guess)
%    ;
%    min_guess_(Gs, Acc, Guess)
%    ).

%min_guess([Guess], Guess).
%min_guess([G|Gs], Guess):-
%    min_guess_(Gs, G, Guess).

% inefficient

astar_guess_cost(gstate(Costs,Heap), X+Y, Cost, NewGuessCosts):-
    put_assoc(X+Y, Costs, Cost, NewCosts),
    put_assoc(Cost-X+Y, Heap, X+Y, NewHeap),
    NewGuessCosts = gstate(NewCosts, NewHeap).

astar_best_guess(gstate(GuessCosts, Heap), Open, NewOpen, NewGuessCosts, Min):-
%    assoc_to_list(Heap, HeapList),
%    writeln([guesses, HeapList, Open]),
    del_min_assoc(Heap, _-X+Y,_, NewHeap),
    NewGuessCosts = gstate(GuessCosts, NewHeap),
    Min = X+Y,
    remove(Min, Open, NewOpen).
%    writeln(["buestguess?", Min, NewOpen]).

astar_heuristic(_, _, 0).
astar_heuristic(X+Y, GX+GY, Guestimate):-
    DX is abs(GX-X),
    DY is abs(GY-Y),
    Guestimate is (DX+DY).

astar_neighbors(CostMatrix, X+Y, NeighborsList):-
    matrix_xy_adjacent_cardinal(CostMatrix, X, Y, NeighborsList).

astar_cost(CostMatrix, X+Y, Cost):-
    matrix(CostMatrix, X, Y, Cost).

astar_update_neighbor(Goal,CostMatrix, CurrentCost, Neighbor,
		      nstate(GuessCosts, KnownCosts, Open),
		      nstate(NewGuessCosts, NewKnownCosts, NewOpen)):-

%    writeln(["neighborupdate", Neighbor]),
    Neighbor = X+Y,
    astar_cost(CostMatrix, Neighbor, NeighborCost),
    TentativeCost is CurrentCost + NeighborCost,
    astar_known_cost(KnownCosts, X+Y, KnownCost),
%    writeln(["newknown", TentativeCost, KnownCost]),
    (TentativeCost < KnownCost ->
	 astar_known_cost(KnownCosts, X+Y, TentativeCost, NewKnownCosts),
	 astar_heuristic(X+Y, Goal, HCost),
	 NewGuessCost is TentativeCost + HCost,
%	 writeln(["newguesscost", HCost, NewGuessCost]),
	 astar_guess_cost(GuessCosts, X+Y, NewGuessCost, NewGuessCosts),
	 (\+ member(X+Y, Open) ->
	      NewOpen = [X+Y|Open]
	 ;
              NewOpen = Open
	 )
    ;
%         writeln("no update"),
	 nstate(GuessCosts, KnownCosts, Open) = 
	 nstate(NewGuessCosts, NewKnownCosts, NewOpen)
    ).

astar_update_neighbor(_, _, Neighbor, In, In):-
    writeln(["no neighborupdate", Neighbor]).

astar_step(astate(CostMatrix, Open, KnownCosts, Goal, GuessCosts), Next, Cost):-
    statistics(global_stack, Stats),
    writeln(Stats),
%    assoc_to_list(KnownCosts, KnownCostList),
%    writeln(["0astart", KnownCostList, Open]),
%    writeln("1best guess"),
    astar_best_guess(GuessCosts, Open, NewOpen, NewGuessCosts, Current),
%    writeln("1.5best guess"),
    astar_known_cost(KnownCosts, Current, CurrentCost),
%    writeln("1.6known"),
    (Current \= Goal ->
%	 writeln("2neighbors"),
	 astar_neighbors(CostMatrix, Current, Neighbors),
%	 writeln("3fold"),
	 foldl(astar_update_neighbor(Goal, CostMatrix, CurrentCost),
	       Neighbors,
	       nstate(NewGuessCosts, KnownCosts, NewOpen),
	       nstate(NewNewGuessCosts, NewKnownCosts, NewNewOpen)),
%	 writeln("4recurse"),
	 Next = astate(CostMatrix, NewNewOpen, NewKnownCosts, Goal, NewNewGuessCosts)
    ;
%         writeln("5failendpath"),
%	 writeln(KnownCosts),
%	 writeln(GuessCosts),
         astar_known_cost(KnownCosts, Goal, Cost),
         Next = astate(done)
    ).
%astar_(_, astar(_, Open, KnownCosts, Goal, GuessCosts), Cost):-
%    astar_best_guess(GuessCosts, Open, _, Current, _),
%    Current = Goal,
%    astar_known_cost(KnownCosts, Goal, Cost).

astar_(0, _, _).
astar_(Steps, State, Cost):-
%    astate(_, Open, _, _, _) = State,
%    writeln(Open),
    NextSteps is 1000,
    astar_step(State, NewState, Cost),
    !,
    (NewState \= astate(done) ->
	 astar_(NextSteps, NewState, Cost)
    ;
        writeln(["DONE"])
    ).

astar_goal(CostMatrix, GoalX+GoalY):-
    matrix_limits(CostMatrix, GoalX, GoalY).

astar_bootstrap(Start,Goal,KnownCosts, GuessCosts):-
    empty_assoc(InitialCosts),
    astar_known_cost(InitialCosts, Start, 0, KnownCosts),

    empty_assoc(InitialGuessCosts),
    empty_assoc(InitialGuestHeap),
    InitialGuess = gstate(InitialGuessCosts, InitialGuestHeap),
    astar_heuristic(Start, Goal, HCost),
    astar_guess_cost(InitialGuess, Start, HCost, GuessCosts).

astar(CostMatrix, Cost):-
    Start = 0+0,
    astar_goal(CostMatrix,Goal),
    astar_bootstrap(Start, Goal, KnownCosts, GuessCosts),

    astar_(80000, astate(CostMatrix, [Start], KnownCosts, Goal, GuessCosts), Cost).

day15_p1_fast(File, Score):-
    phrase_from_file(matrixp(LCostMatrix), File),
    lmatrix_matrix(LCostMatrix, CostMatrix),
    astar(CostMatrix, Score).

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
    astar(CostMatrix, Score).

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
