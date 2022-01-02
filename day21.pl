:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(matrix).
:- table play_move/4.

data([D]) --> player_start(D).
data([D|Ds]) --> player_start(D), data(Ds).

player_start(PS) --> `Player `, integer(N), ` starting position: `, integer(P), blank, {PS = player(N, P)}.
		 
game_state([player(N1,P1), player(N2,P2)], GameState):-
    GameState = gstate(pstate(N1,P1, 0), pstate(N2,P2, 0), 1).

die(Steps, Offset, Value):-
    TotalSteps is Steps + Offset,
    Value is mod(TotalSteps, 100) + 1.

play_move(P, Roll, NewP, Score):-
    NewP is mod(P + Roll - 1, 10) + 1,
    Score is NewP.

play_turn(pstate(N, P, Score), Roll, NewState):-
    play_move(P, Roll, NewP, MoveScore),
    NewScore is Score + MoveScore,
    NewState = pstate(N, NewP, NewScore).

roll_det3(Die, Value):-
    V1 is mod(Die, 100) + 1,
    V2 is mod(Die+1, 100) + 1,
    V3 is mod(Die+2, 100) + 1,
    Value is V1+V2+V3.

has_won(pstate(_, _, Score), Threshold):-
    Score >= Threshold.

win(pstate(N, _, _), Universes, UCount, WUniverse):-
    NewUniverses is Universes * UCount,
    (N  =  1 ->
	 WUniverse = NewUniverses-0
    ;
         WUniverse = 0-NewUniverses
    ).

play_universe(gstate(P1, P2, Universes), Threshold, DieUniverses, Roll-UCount, WUniverses):-
    play_turn(P1, Roll, NewP1),
    ( has_won(NewP1, Threshold) ->
      win(NewP1, UCount, Universes, WUniverses)
    ;
      NewUniverse is Universes * UCount,
      playu_split(gstate(P2, NewP1, NewUniverse), Threshold, DieUniverses, WUniverses)
    ).

split_wins(P0-P1, P0, P1).
playu_split(gstate(P1, P2, Universes), Threshold, DieUniverses, WUniverses):-
    maplist(play_universe(gstate(P1, P2, Universes), Threshold, DieUniverses), DieUniverses, Wins),
    maplist(split_wins, Wins, P0Wins, P1Wins),
    sum_list(P0Wins, P0Total),
    sum_list(P1Wins, P1Total),
    WUniverses = P0Total-P1Total.

play_(gstate(P1, P2, 1), Die, Threshold, FinalScore):-
    roll_det3(Die, Roll),
    play_turn(P1, Roll, NewP1),
    NewDie is Die + 3,
    ( has_won(NewP1, Threshold) ->
      P2 = pstate(_,_, LeastScore),
      FinalScore is NewDie * LeastScore
    ;
      play_(gstate(P2, NewP1, 1), NewDie, Threshold, FinalScore)
    ).

play(Players, Score):-
    game_state(Players, GameState),
    play_(GameState, 0, 1000, Score).

die_universes(DieUniverses):-
    findall(Total-1,
            (between(1, 3, D1),
             between(1, 3, D2),
             between(1, 3, D3),
             Total is D1+D2+D3),
	    DiesSummed),
    sort(1, @=<, DiesSummed, SortedDies),
    group_pairs_by_key(SortedDies, SumGroups),
    maplist(count_sums, SumGroups, DieUniverses).

count_sums(A-S, B):-
    length(S, Count),
    B = A-Count.

playu(Players, MaxWins):-
    game_state(Players, GameState),
    die_universes(DieUniverses),
    playu_split(GameState, 21, DieUniverses, P1Wins-P2Wins),
    MaxWins is max(P1Wins, P2Wins).

day21_p1(File, Score):-
    phrase_from_file(data(Players), File),
    play(Players, Score).

day21_p2(File, Score):-
    phrase_from_file(data(Players), File),
    playu(Players, Score).

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
    day21_p2_test(444356092776315),
    day21_p2(647920021341197).
