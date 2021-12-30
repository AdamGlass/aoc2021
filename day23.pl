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
data(goal,   [o, o, o, o, o, o, o, o, o, o, o], [[a,a], [b,b], [c,c], [d,d]]).

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
    writeln([X+Y, Amphipod, Destinations]),
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
	   
movex(Matrix, X+0, Amphipod, DestinationStatesCost):-
    matrix_limits(Matrix, _, YMax),
    writeln("target"),
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
    writeln([target, CleanDestinations]),
    destinations_states_costs(Matrix, X+0, Amphipod, CleanDestinations, DestinationStatesCost).

move(Matrix,X+Y, Amphipod, DestinationStatesCost):-
    Y > 0,
    empty_to_target_row(Matrix, X, Y, 0),
    findall(Destination,
	    (between_nc(X, 0, DX),
	     empty_to_target_column(Matrix, X, DX),
	     Destination = DX+0),
	    LeftDestinations),
    writeln([X+Y, LeftDestinations]),
    findall(Destination,
	    (between_nc(X, 10, DX),
	     empty_to_target_column(Matrix, X, DX),
	     Destination = DX+0),
	    RightDestinations),
    writeln([X+Y, RightDestinations]),
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
	    writeln(X+Y),
	    move(Matrix, X+Y, Amphipod, NeighborStatesCost)),
	   NeighborStatesCostList),
    flatten(NeighborStatesCostList, AllNeighborStatesCost).

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

neighbor_next(Neighbor-Cost):-
    writeln("start"),
    board_write_cost(Neighbor-Cost),
    writeln("neighbors"),
    neighbor_states(Neighbor, NewNeighbors),
    maplist(board_write_cost, NewNeighbors).

sample_list(List, Count, Samples):-
    length(Samples, Count),
    random_permutation(List, RandomPermutation),
    append(Samples, _, RandomPermutation).

room(File, Matrix):-
    data(File, Hallway, SideRooms),
    room_init(Hallway, SideRooms, Matrix).

goal(GoalMatrix):-
    room(goal, GoalMatrix).
    
day23_p1(File, Score):-
    writeln("Goal"),
    goal(GoalMatrix),
    board_write(GoalMatrix),
    room(File, Matrix),
    board_write(Matrix),
    writeln("Neighbors"),
    neighbor_states(Matrix, Neighbors),
    maplist(board_write_cost, Neighbors),
    writeln("NeighBorNext"),
    maplist(neighbor_next, Neighbors),
    sample_list(Neighbors, 10, NeighborSubset),
    maplist(neighbor_next, NeighborSubset).

%    astar(Mattrix,
%	  GoalMatrix,
%	  successor_states,
%	  state_cost,
%	  heuristic,
%	  Score).
%    writeln(SideRooms).

day23_p1(Score):-
    day23_p1(p1data, Score).

day23_p1_test(Score):-
    day23_p1(p1test, Score).

day23_p1_test2(Score):-
    day23_p1(p1test2, Score).

day23_p2(Score):-
    day23_p2("data/day23_p1_data", Score).

day23_p2_test(Score):-
    day23_p2("data/day23_p1_test", Score).

day23:-
    day23_p1_test(58),
    day23_p1(920580),
    day23_p2_test(_),
    day23_p2(_).
