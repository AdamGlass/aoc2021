:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

lines([]) --> eos.
lines([L]) --> line(L).
lines([L|Ls]) --> line(L), blank, lines(Ls).

line(lseg(X1,Y1,X2,Y2)) --> coord(X1,Y1), blank, dashy, blank, coord(X2, Y2).

coord(X,Y) --> integer(X), comma, integer(Y).

comma --> [C], {char_code(',', C)}.
dashy --> [C,D], {char_code('-', C), char_code('>', D)}.

day5_parse_test(V):-
    phrase_from_file(lines(L), "data/day5_p1_data"), write(L), V=0.

