:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

lines([]) --> eos.
lines([L]) --> line(L).
lines([L|Ls]) --> line(L), blank, lines(Ls).

line(lseg(X1,Y1,X2,Y2)) --> coord(X1,Y1), blank, dashy, blank, coord(X2, Y2).

coord(X,Y) --> integer(X), comma, integer(Y).

comma --> [C], {char_code(',', C)}.
dashy --> [C,D], {char_code('-', C), char_code('>', D)}.

between_no_order(A, B, E):-
    between(A, B, E).
between_no_order(A, B, E):-
    between(B, A, E).

lseg_cross(lseg(X1, Y1, _,   _), X1, Y1).
lseg_cross(lseg(_,   _, X2, Y2), X2, Y2).
lseg_cross(lseg(X1, Y1, X1, Y2), X1, Y):-
    between_no_order(Y1, Y2, Y).
lseg_cross(lseg(X1, Y1, X2, Y1), X, Y1):-
    between_no_order(X1, X2, X).
% reverse line
lseg_cross(lseg(X1, Y1, X2, Y2), X, Y):-
    X1 > X2,
    lseg_cross(lseg(X2, Y2, X1, Y1), X, Y).
lseg_cross(A, X, Y):-
    lseg_cross_diag(A, X, Y).

lseg_cross_diag(lseg(X1, Y1, X2, Y2), X, Y):-
    X1 =< X2,
    between_no_order(X1, X2, X),
    between_no_order(Y1, Y2, Y),
    (Y1 < Y2 ->
         X #= X1 + R,
         Y #= Y1 + R
    ;
         X #= X1 + R,
         Y #= Y1 - R
    ).

crosses([], _, _, AccCount, AccCount).
crosses([L|Ls], X, Y, AccCount, Count):-
    lseg_cross(L, X,Y),!,
    NewCount is AccCount + 1,
    crosses(Ls, X, Y, NewCount, Count).
crosses([_|Ls], X, Y, AccCount, Count):-
    crosses(Ls, X, Y, AccCount, Count).

crosses(L, [X,Y], Count):-
    crosses(L, X, Y, 0, Count).

count_crosses(L, MaxX, MaxY, CrossCount):-
    O = [X,Y],
    findall(O, (between(0, MaxX, X), between(0, MaxY, Y), crosses(L, O,Cross), Cross >= 2), AccCross), length(AccCross, CrossCount).

max_coords([], MaxX, MaxY, MaxX, MaxY).
max_coords([lseg(X1, Y1, X2, Y2)|Ls], AccX, AccY, MaxX, MaxY):-
    max_list([AccX, X1, X2], NewAccX),
    max_list([AccY, Y1, Y2], NewAccY),
    max_coords(Ls, NewAccX, NewAccY, MaxX, MaxY).

non_diag(lseg(X, _, X, _)).
non_diag(lseg(_, Y, _, Y)).

day5_p1(File, Score):-
    phrase_from_file(lines(AllLines), File),
    include(non_diag,AllLines, Lines),
    length(AllLines, AL), length(Lines, LL),
    max_coords(Lines, 0, 0, MaxX, MaxY),
    count_crosses(Lines, MaxX, MaxY, CrossCount),
    Score = CrossCount.

day5_p1(V):-
    day5_p1("data/day5_p1_data", V).

day5_p1_test(V):-
    day5_p1("data/day5_p1_test", V).

day5_p2(File, Score):-
    phrase_from_file(lines(Lines), File),
    max_coords(Lines, 0, 0, MaxX, MaxY),
    count_crosses(Lines, MaxX, MaxY, CrossCount),
    Score = CrossCount.

day5_p2(V):-
    day5_p2("data/day5_p1_data", V).

day5_p2_test(V):-
    day5_p2("data/day5_p1_test", V).

day5:-
    day5_p1_test(5),
    day5_p1(7473),
    day5_p2_test(12),
    day5_p2(24164).
