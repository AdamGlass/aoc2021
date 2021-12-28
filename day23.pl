:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(matrix).
:- use_module(astar).

data(p1data, [[b,b], [a,c], [a,d], [d,c]]).
data(p1test, [[b,a], [c,d], [b,c], [d,a]]).
data(goal,   [[a,a], [b,b], [c,c], [d,d]]).

moves(0+0, [1+0, 2+0, 2+1], [2+1], 3)
moves(0+0, [1+0, 2+0], [2+1], 3)
moves(1+0, [2+0, 2+1, 2+2], [2+2], 3)
moves(1+0, [2+0, 2+1], [2+1], 2)
moves(3+0, [1+0, 2+0, 2+1], [2+1], 3)

move_cost(a, 1).
move_cost(b, 10).
move_cost(c, 100),
move_cost(d, 1000).

set_empty_hallway(X, InMatrix, OutMatrix):-
    matrix_transform(InMatrix, X, 0, o, OutMatrix).

set_sideroom([A,B], Index, InMatrix, OutMatrix):-
    matrix_transform(InMatrix, Index, 1, A, AMatrix),
    matrix_transform(AMatrix, Index, 2, B, OutMatrix).

room_init(SideRooms, Matrix):-
    matrix_init(InitMatrix),
    A = [0, 1, 2, 3, 4, 6, 7, 8, 9 , 10, 11],
    foldl(set_empty_hallway, A, InitMatrix, HallwayMatrix),
    foldl(set_sideroom, SideRooms, [2, 4, 6, 8], HallwayMatrix, Matrix).

move_right_sideroom(Matrix, X+Y, SideRoomIndex):-
    NewX is X + 1,
    forall(between(NewX, SideRoomIndex, X),
	   matrix(Matrix, NewX+Y, o),
	   
	       





successor_states(Matrix, States):-
    findall(NeighborStates,
	    (between(0, 11, X),
	     between(0, 2, Y),
	     matrix(Matrix, X+Y, Value),
	     member(Value, [a,b,c,d]),
	     moves(Matrix, X+Y, NeighborStates)),
	    NeighborStateList).

day23_p1(File, Score):-
    data(File, SideRooms),
    room_init(SideRooms, Matrix),
    data(goal, GoalSideRooms),
    room_init(GoalSideRooms, GoalMatrix),
    writeln(Matrix).
%    astar(Mattrix,
%	  GoalMatrix,
%	  successor_states,
%	  state_cost,
%	  heuristic,
%	  Score).
%    writeln(SideRooms).

day23_p2(File, Score):-
    phrase_from_file(data(Players), File),
    play_p2(Players, Score).

day23_p1(Score):-
    day23_p1(p1data, Score).

day23_p1_test(Score):-
    day23_p1(p1test, Score).

day23_p2(Score):-
    day23_p2("data/day23_p1_data", Score).

day23_p2_test(Score):-
    day23_p2("data/day23_p1_test", Score).

day23:-
    day23_p1_test(58),
    day23_p1(920580),
    day23_p2_test(_),
    day23_p2(_).
