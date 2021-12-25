:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(matrix).

data([D]) --> player_start(D).
data([D|Ds]) --> player_start(D), data(Ds).

player_start(PS) --> `Player `, integer(N), ` starting position: `, integer(P), blank, {PS = player(N, P)}.
		 
make_pstate(player(N, P), State):-
    State = pstate(N, P, 0).

die(Steps, Offset, Value):-
    TotalSteps is Steps + Offset,
    Value is mod(TotalSteps, 100) + 1.

play_move(P, Roll, NewP, Score):-
    NewP is mod(P + Roll - 1, 10) + 1,
    Score is NewP.

play_turn(pstate(N, P, Score), D1+D2+D3, NewState):-
    Roll is D1 + D2 + D3,
    play_move(P, Roll, NewP, MoveScore),
    NewScore is Score + MoveScore,
    NewState = pstate(N, NewP, NewScore),
    writeln([player, N, Roll, NewP, NewScore]).

roll_detdie(PlayState, Steps, DieList):-
    length(PlayState, PlayerCount),
    DieStepsBase is Steps * PlayerCount *3,
    findall(D1+D2+D3,
	   (nth0(N, PlayState, _),
	    DieSteps is DieStepsBase + N *3,
	    die(DieSteps, 0, D1),
	    die(DieSteps, 1, D2),
	    die(DieSteps, 2, D3)),
	   DieList).

play_turn(PlayState, DieList, NewState, WinScore, Winners):-
    maplist(play_turn, PlayState, DieList, NewState),
    findall(WinState,
	    (member(WinState, NewState),
	     WinState = pstate(_,_, Score),
	     Score >= WinScore),
	    Winners).

score(Steps, Winners, OldState, NewState, FinalScore):-
    length(OldState, PlayerCount),
    DieStepsBase is Steps * PlayerCount *3,
    min_member(Winner, Winners),
    nth0(N, NewState, Winner),
    DieCount is DieStepsBase + N *3 + 3,
    writeln(DieCount),
    ( N > 0 ->
       LoserState = NewState
    ;
       LoserState = OldState
    ),
    findall(LoserScore,
	    (member(pstate(P, _, LoserScore), LoserState),
	     P \= N),
	    LoserScoreList),
    min_list(LoserScoreList, LoserScore),
    FinalScore is DieCount * LoserScore.

play_p1_(State, Steps, FinalScore):-
    roll_detdie(State, Steps, DieList),
    play_turn(State, DieList, NewState, 1000, Winners),
    (length(Winners, 0) ->
	 NewSteps is Steps + 1,
	 play_p1_(NewState, NewSteps, FinalScore)
    ;
         score(Steps, Winners, State, NewState, FinalScore)
    ).

play_p1(Players, Score):-
    length(Players, Count),
    length(State, Count),
    maplist(make_pstate, Players, State),
    play_p1_(State, 0, Score).
    
new_universes_play_turn(Player, Rolls, NewPlayer):-
    Player = pstate(N, P, Score),
    play_move(P, Rolls, NewP, MoveScore),
    NewScore is Score + MoveScore,
    NewPlayer = pstate(N, NewP, NewScore),

play_universes(State, DieList, NewState):-
    findall(NewPlayer,
	    (member(Player, State),
	    member(Rolls, DieList),
	    new_universes_play_turn(Player, Rolls, NewPlayer))
	   NewState).

play_p2_(State, DieUniverses, AccStats, Others):-
    roll_universes(State, DieList, NewState),
    partition_winners_l(NewState, Winners, Others),
    account_winners(W
    (length(Others, 0) ->
       writeln("DONE").
    ;
       
    ).

count_sums(A-S, B):-
    length(S, Count),
    B = A-Count.

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

play_p2(Players, MaxWinUniverses):-
    length(Players, Count),
    length(State, Count),
    maplist(make_pstate, Players, State),
    die_universes(DieUniverses),
    play_p2_(State, DieUniverses, MaxWinUniverses),

day21_p1(File, Score):-
    phrase_from_file(data(Players), File),
    play_p1(Players, Score).

day21_p2(File, Score):-
    phrase_from_file(data(Players), File),
    play_p2(Players, Score).

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
