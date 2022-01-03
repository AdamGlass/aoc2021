:- use_module(library(dcg/basics)).

depths([D]) --> depth(D).
depths([D|Ds]) --> depth(D), depths(Ds).

depth(D) --> integer(D), blanks.

depth_increase([], 0).
depth_increase([_], 0).
depth_increase([A,B|Tail], Count):-
    B > A,
    depth_increase([B|Tail], NewCount),
    Count is NewCount + 1.
depth_increase([_,B|Tail], Count):-
    depth_increase([B|Tail], Count).

sum3(List, Sums) :- sum3(List, [], Sums).
sum3([_,_], Accum, Sum):-
    reverse(Accum, Sum).
sum3([A,B,C|Tail], Accum, Sum):-
    N is A+B+C,
    sum3([B,C|Tail], [N|Accum], Sum).

day1_p1(File, V):-
    phrase_from_file(depths(Depths), File),
    depth_increase(Depths, V).

day1_p2(File, V):-
    phrase_from_file(depths(Depths), File),
    sum3(Depths, SlidingWindow),
    depth_increase(SlidingWindow, V).

day1_p1_test(V):-
    day1_p1("data/day1_p1_test", V).

day1_p1(V):-
    day1_p1("data/day1_p1_data", V).

day1_p2_test(V):-
    day1_p2("data/day1_p1_test", V).

day1_p2(V):-
    day1_p2("data/day1_p1_data", V).

day1:-
    day1_p1_test(7),
    day1_p1(1482),
    day1_p2_test(5),
    day1_p2(1518).
