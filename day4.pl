:- use_module(library(dcg/basics)).

bingo_data(Draw, Boards) --> draw(Draw), blanks, boards(Boards), blanks.

draw([X]) --> integer(X).
draw([X|Xs]) --> integer(X), comma, draw(Xs), blanks.

comma --> [C], {char_code(',', C)}.

boards([B]) --> board(B).
boards([B|Bs]) --> board(B), blanks, boards(Bs).

board([R0,R1,R2,R3,R4]) --> board_row(R0), blanks, board_row(R1), blanks, board_row(R2), blanks, board_row(R3), blanks, board_row(R4), blanks.

board_row([A,B,C,D,E]) --> blanks, integer(A), blanks, integer(B), blanks, integer(C), blanks, integer(D), blanks, integer(E).

day4_p1(File, V):-
    phrase_from_file(bingo_data(Draw, Boards), File),
    write(Draw), write(Boards),
    V = "notkey".

day4_parse_test(V):-
    phrase_from_file(bingo_data(Draw, Boards), "data/day4_p1_data"), writeln(Draw), writeln(Boards), V=0.

day4_p1_test(V):-
    day4_p1("data/day4_p1_test", V).
