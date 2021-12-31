:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).
:- use_module(matrix).
:- use_module(astar).

data(p1data, [o, o, o, o, o, o, o, o, o, o, o], [[b,b], [a,c], [a,d], [d,c]]).
data(p1test, [o, o, o, o, o, o, o, o, o, o, o], [[b,a], [c,d], [b,c], [d,a]]).
data(p1test2,[a, o, o, o, o, o, o, o, o, o, o], [[o,a], [c,d], [b,c], [d,a]]).
data(p2test, [o, o, o, o, o, o, o, o, o, o, o], [[b,d,d,a], [c,c,b,d], [b,b,a,c], [d,a,c,a]]).
data(p2data, [o, o, o, o, o, o, o, o, o, o, o], [[b,d,d,b], [a,c,b,c], [a,b,a,d], [d,a,c,c]]).
data(goal,   [o, o, o, o, o, o, o, o, o, o, o], [[a,a], [b,b], [c,c], [d,d]]).
data(goal2,  [o, o, o, o, o, o, o, o, o, o, o], [[a,a,a,a], [b,b,b,b], [c,c,c,c], [d,d,d,d]]).

move_cost(a, 1).
move_cost(b, 10).
move_cost(c, 100).
move_cost(d, 1000).

target_column(a, 2).
target_column(b, 4).
target_column(c, 6).
target_column(d, 8).

set_hallway(Hallway, X, InMatrix, OutMatrix):-
    nth0(X, Hallway, Value),
    matrix_transform(InMatrix, X, 0, Value, OutMatrix).


set_sideroom([A,B], Index, InMatrix, OutMatrix):-
    matrix_transform(InMatrix, Index, 1, A, AMatrix),
    matrix_transform(AMatrix, Index, 2, B, OutMatrix).

set_sideroom([A,B,C,D], Index, InMatrix, OutMatrix):-
    matrix_transform(InMatrix, Index, 1, A, AMatrix),
    matrix_transform(AMatrix, Index, 2, B, BMatrix),
    matrix_transform(BMatrix, Index, 3, C, CMatrix),
    matrix_transform(CMatrix, Index, 4, D, OutMatrix).

room_init(Hallway, SideRooms, Matrix):-
    matrix_init(InitMatrix),
    A = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9 , 10],
    foldl(set_hallway(Hallway), A, InitMatrix, HallwayMatrix),
    foldl(set_sideroom, SideRooms, [2, 4, 6, 8], HallwayMatrix, Matrix).

between_nc(X, Target, Value):-
    ( X > Target ->
      NewX is X - 1,
      between(Target, NewX, Value)
    ;
      NewX is X + 1,
      between(NewX, Target, Value)
    ).

distance(X+Y, NX+NY, Distance):-
    XDist is abs(X-NX),
    YDist is abs(Y-NY),
    Distance is XDist + YDist.

empty_to_target_column(Matrix, Start, Target):-
    forall(between_nc(Start, Target, Column),
	   matrix(Matrix, Column, 0, o)).

empty_to_target_row(Matrix, Column, Start, Target):-
    forall(between_nc(Start, Target, Row),
	   matrix(Matrix, Column, Row, o)).

destinations_states_costs(Matrix, X+Y, Amphipod, Destinations, DestinationStatesCost):-
    move_cost(Amphipod, CostFactor),
    findall(DestinationState-Cost,
	    (member(DX+DY, Destinations),
	     distance(X+Y, DX+DY, Distance),
	     Cost is Distance*CostFactor,
	     matrix_transform(Matrix, X, Y, o, NewMatrix),
	     matrix_transform(NewMatrix, DX, DY, Amphipod, DestinationState)),
	    DestinationStatesCost).

trim_destinations(Destinations, TrimmedDestinations):-
    sort(Destinations, SortedDestinations),
    subtract(SortedDestinations, [2+0, 4+0, 6+0, 8+0], TrimmedDestinations).

friendly_target_column(Matrix, Amphipod, YMax, Column):-
    forall(between_nc(0, YMax, Row),
	   (matrix(Matrix, Column, Row, o)
	   ;
	   matrix(Matrix, Column, Row, Amphipod)
	   )).

max_depth(X+Y, AX+AY, NX+NY):-
    (Y > AY ->
	NX = X,
	NY = Y
    ;
        NX = AX,
	NY = AY
    ).

max_depth_destinations(Destinations, MaxDepthDestinations):-
    foldl(max_depth, Destinations, 0+0, MaxDepth),
    MaxDepthDestinations = [MaxDepth].

movex(Matrix, X+0, Amphipod, DestinationStatesCost):-
    matrix_limits(Matrix, _, YMax),
    target_column(Amphipod, TargetColumn),
    empty_to_target_column(Matrix, X, TargetColumn),
    friendly_target_column(Matrix, Amphipod, YMax, TargetColumn),
    findall(Destination,
	    (between_nc(0, YMax, Row),
	     forall(between_nc(0, Row, TRow),
		    matrix(Matrix, TargetColumn, TRow, o)),
	     Destination = TargetColumn+Row),
	    Destinations),
    trim_destinations(Destinations, CleanDestinations),
    max_depth_destinations(CleanDestinations, DeepestDestinations),
    destinations_states_costs(Matrix, X+0, Amphipod, DeepestDestinations, DestinationStatesCost).

unfriendly_column(_, Column+_, Amphipod):-
    target_column(Amphipod, TargetColumn),
    TargetColumn \= Column.

unfriendly_column(Matrix, Column+_, Amphipod):-
    matrix_limits(Matrix, _, YMax),
    \+ forall(between(0, YMax, Y),
	      (
		  matrix(Matrix, Column, Y, o)
	      ;
	          matrix(Matrix, Column, Y, Amphipod)
	      )
	     ).

move(Matrix,X+Y, Amphipod, DestinationStatesCost):-
    Y > 0,
    unfriendly_column(Matrix, X+Y, Amphipod),
    empty_to_target_row(Matrix, X, Y, 0),
    findall(Destination,
	    (between_nc(X, 0, DX),
	     empty_to_target_column(Matrix, X, DX),
	     Destination = DX+0),
	    LeftDestinations),
    findall(Destination,
	    (between_nc(X, 10, DX),
	     empty_to_target_column(Matrix, X, DX),
	     Destination = DX+0),
	    RightDestinations),
    append(LeftDestinations, RightDestinations, Destinations),
    trim_destinations(Destinations, CleanDestinations),
    destinations_states_costs(Matrix, X+Y, Amphipod, CleanDestinations, DestinationStatesCost).

move(Matrix,D, Amphipod, DestinationStatesCost):-
    movex(Matrix, D, Amphipod, DestinationStatesCost).

neighbor_states(Matrix, AllNeighborStatesCost):-
    findall(NeighborStatesCost,
	   (matrix_xy(Matrix, X, Y),
	    matrix(Matrix, X, Y, Amphipod),
	    member(Amphipod, [a,b,c,d]),
	    move(Matrix, X+Y, Amphipod, NeighborStatesCost)),
	   NeighborStatesCostList),
    flatten(NeighborStatesCostList, AllNeighborStatesCost).

bad_heuristic(_, _, 1).
heuristic(Matrix, _, Cost):-
    findall(Cost,
	   (matrix_xy(Matrix, X, Y),
	    matrix(Matrix, X, Y, Amphipod),
	    member(Amphipod, [a,b,c,d]),
	    target_column(Amphipod, Column),
	    distance(X+Y, Column+1, Distance),
	    move_cost(Amphipod, AmphipodCost),
	    Cost is Distance * AmphipodCost),
	   CostList),
    sum_list(CostList, Cost).

board_write_side(Matrix, Y, X):-
    matrix(Matrix, X, Y, Value),
    write(Value),
    write(" ").

board_write(Matrix):-
    forall(between(0, 10, X),
	   (matrix(Matrix, X, 0, Value),
	    write(Value))),
    writeln(""),
    \+ forall(between(1, 10, Y),
	   (write("  "),
	    maplist(board_write_side(Matrix, Y), [2,4,6,8]),
	    writeln(""))),
    writeln("").

board_write_cost(State-Cost):-
    writeln([cost, Cost]),
    board_write(State).

room(File, Matrix):-
    data(File, Hallway, SideRooms),
    room_init(Hallway, SideRooms, Matrix).

goal(GoalMatrix):-
    room(goal, GoalMatrix).

goal2(GoalMatrix):-
    room(goal2, GoalMatrix).

day23_p1(File, Score):-
    writeln("Goal"),
    goal(GoalMatrix),
    board_write(GoalMatrix),
    room(File, Matrix),
    board_write(Matrix),
    astar(Matrix,
	  GoalMatrix,
	  neighbor_states,
	  bad_heuristic,
	  Score).

day23_p2(File, Score):-
    writeln("Goal"),
    goal2(GoalMatrix),
    board_write(GoalMatrix),
    room(File, Matrix),
    board_write(Matrix),
    astar(Matrix,
	  GoalMatrix,
	  neighbor_states,
	  heuristic,
	  Score).

day23_p1(Score):-
    day23_p1(p1data, Score).

day23_p1_test(Score):-
    day23_p1(p1test, Score).

day23_p1_test2(Score):-
    day23_p1(p1test2, Score).

day23_p2(Score):-
    day23_p2(p2data, Score).

day23_p2_test(Score):-
    day23_p2(p2test, Score).

day23:-
    day23_p1_test(12521),
    day23_p1(11417),
    day23_p2_test(44169),
    day23_p2(49529).
