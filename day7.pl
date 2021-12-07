:- use_module(library(dcg/basics)).

crabs([X]) --> integer(X).
crabs([X|Xs]) --> integer(X), comma, crabs(Xs), blanks.

comma --> [C], {char_code(',', C)}.

day7_p1(File, Fuel):-
    phrase_from_file(crabs(Crabs), File),
    length(Crabs, Fuel).

day7_p1(Fuel):-
    day7_p1("data/day7_p1_data", Fuel).

day7_p1_test(Fuel):-
    day7_p1("data/day7_p1_test", Fuel).

day7:-
    day7_p1_test(37).
