:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

bingo_data(Draw, Boards) --> draw(Draw), blanks, boards(Boards), blanks.

draw([X]) --> integer(X).
draw([X|Xs]) --> integer(X), comma, draw(Xs), blanks.

comma --> [C], {char_code(',', C)}.

boards([B]) --> board(B).
boards([B|Bs]) --> board(B), blanks, boards(Bs).

board([R0,R1,R2,R3,R4]) --> board_row(R0), blanks, board_row(R1), blanks, board_row(R2), blanks, board_row(R3), blanks, board_row(R4), blanks.

board_row([A,B,C,D,E]) --> blanks, integer(A), blanks, integer(B), blanks, integer(C), blanks, integer(D), blanks, integer(E).

bingo_column(Drawn, [R0, R1, R2, R3, R4], Column):-
    nth0(Column, R0, V0),
    nth0(Column, R1, V1),
    nth0(Column, R2, V2),
    nth0(Column, R3, V3),
    nth0(Column, R4, V4),
    intersection([V0, V1, V2, V3, V4], Drawn, [V0, V1, V2, V3, V4]).

bingo_row(Drawn, Row):-
    list_to_ord_set(Row, RowSet),
    list_to_ord_set(Drawn, DrawnSet),
    ord_intersect(RowSet, DrawnSet, RowSet).

bingo_test(Drawn, [R,_,_,_,_]):-
    bingo_row(Drawn, R).
bingo_test(Drawn, [_,R,_,_,_]):-
    bingo_row(Drawn, R).
bingo_test(Drawn, [_,_,R,_,_]):-
    bingo_row(Drawn, R).
bingo_test(Drawn, [_,_,_,R,_]):-
    bingo_row(Drawn, R).
bingo_test(Drawn, [_,_,_,_,R]):-
    bingo_row(Drawn, R).
bingo_test(Drawn, Board):-
    bingo_column(Drawn, Board, 0).
bingo_test(Drawn, Board):-
    bingo_column(Drawn, Board, 1).
bingo_test(Drawn, Board):-
    bingo_column(Drawn, Board, 2).
bingo_test(Drawn, Board):-
    bingo_column(Drawn, Board, 3).
bingo_test(Drawn, Board):-
    bingo_column(Drawn, Board, 4).

board_bingo_acc(_, Board, [A|As], [A|As]):-
    bingo_test([A|As], Board).
board_bingo_acc([Draw|Draws], Board, AccDrawn, Drawn):-
    board_bingo_acc(Draws, Board, [Draw|AccDrawn], Drawn).

board_bingo(Draw, Board, Drawn):-
    board_bingo_acc(Draw, Board, [], Drawn).

boards_bingo(_, [], []).
boards_bingo(Draw, [B|Bs], [bingo(B,Drawn)|Bingos]):-
    board_bingo(Draw, B, Drawn),
    boards_bingo(Draw, Bs, Bingos).
boards_bingo(Draw, [_|Bs], Bingos):-
    boards_bingo(Draw, Bs, Bingos).

min_game_p(bingo(B1, D1), bingo(_, D2), bingo(B1,D1)):-
    length(D1, D1L), length(D2, D2L), D1L < D2L.
min_game_p(bingo(_, _), bingo(B2, D2), bingo(B2,D2)).
min_game([L|Ls], Min):-
    foldl(min_game_p, Ls, L, Min).

max_game_p(bingo(B1, D1), bingo(_, D2), bingo(B1,D1)):-
    length(D1, D1L), length(D2, D2L), D1L > D2L.
max_game_p(bingo(_, _), bingo(B2, D2), bingo(B2,D2)).
max_game([L|Ls], Max):-
    foldl(max_game_p, Ls, L, Max).


sum_unmarked([R0, R1, R2, R3, R4], Drawn, Sum):-
    subtract(R0, Drawn, S0), sum_list(S0, SL0),
    subtract(R1, Drawn, S1), sum_list(S1, SL1),
    subtract(R2, Drawn, S2), sum_list(S2, SL2),
    subtract(R3, Drawn, S3), sum_list(S3, SL3),
    subtract(R4, Drawn, S4), sum_list(S4, SL4),
    Sum is SL0+SL1+SL2+SL3+SL4.

score(Board, [D|Ds], Score):-
    sum_unmarked(Board, [D|Ds], Sum),
    Score is D * Sum.

day4_bingos(File, Bingos):-
    phrase_from_file(bingo_data(Draw, Boards), File),
    boards_bingo(Draw, Boards, Bingos).

day4_p1(File, Score):-
    day4_bingos(File, Bingos),
    min_game(Bingos, bingo(Board, Drawn)),
    score(Board, Drawn, Score).

day4_p1(V):-
    day4_p1("data/day4_p1_data", V).

day4_p1_test(V):-
    day4_p1("data/day4_p1_test", V).

day4_p2(File, Score):-
    day4_bingos(File, Bingos),
    max_game(Bingos, bingo(Board, Drawn)),
    score(Board, Drawn, Score).

day4_p2(V):-
    day4_p2("data/day4_p1_data", V).

day4_p2_test(V):-
    day4_p2("data/day4_p1_test", V).

day4:-
    day4_p1_test(4512),
    day4_p1(11536),
    day4_p2_test(1924),
    day4_p2(1284).
