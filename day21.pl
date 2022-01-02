:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(matrix).

data([D]) --> player_start(D).
data([D|Ds]) --> player_start(D), data(Ds).

player_start(PS) --> `Player `, integer(N), ` starting position: `, integer(P), blank, {PS = player(N, P)}.
		 
game_state([player(N1,P1), player(N2,P2)], GameState):-
    GameState = gstate(pstate(N1,P1, 0), pstate(N2,P2, 0)).

die(Steps, Offset, Value):-
    TotalSteps is Steps + Offset,
    Value is mod(TotalSteps, 100) + 1.

play_move(P, Roll, NewP, Score):-
    NewP is mod(P + Roll - 1, 10) + 1,
    Score is NewP.

play_turn(pstate(N, P, Score), Roll, NewState):-
    play_move(P, Roll, NewP, MoveScore),
    NewScore is Score + MoveScore,
    NewState = pstate(N, NewP, NewScore),
    writeln([player, N, Roll, NewP, NewScore]).

roll_det3(Die, Value):-
    V1 is mod(Die, 100) + 1,
    V2 is mod(Die+1, 100) + 1,
    V3 is mod(Die+2, 100) + 1,
    Value is V1+V2+V3.

has_won(pstate(_, _, Score), Threshold):-
    Score >= Threshold.

play_(gstate(P1, P2), Die, Threshold, FinalScore):-
    roll_det3(Die, Roll),
    play_turn(P1, Roll, NewP1),
    NewDie is Die + 3,
    ( has_won(NewP1, Threshold) ->
      P2 = pstate(_,_, LeastScore),
      FinalScore is NewDie * LeastScore
    ;
      play_(gstate(P2, NewP1), NewDie, Threshold, FinalScore)
    ).

play(Players, Score):-
    game_state(Players, GameState),
    play_(GameState, 0, 1000, Score).
    
day21_p1(File, Score):-
    phrase_from_file(data(Players), File),
    play(Players, Score).

day21_p1(Score):-
    day21_p1("data/day21_p1_data", Score).

day21_p1_test(Score):-
    day21_p1("data/day21_p1_test", Score).

day21_p2(Score):-
    day21_p2("data/day21_p1_data", Score).

day21_p2_test(Score):-
    day21_p2("data/day21_p1_test", Score).

day21:-
    day21_p1_test(739785),
    day21_p1(920580),
    day21_p2_test(_),
    day21_p2(_).
