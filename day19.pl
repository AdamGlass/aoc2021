:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

scanners(S) --> scanner(S).
scanners([S|Ss]) --> scanner(S), blanks, scanners(Ss).

scanner(S) --> scanner_header(H), beacons(D), { S = scanner(H, D)}.

scanner_header(S) --> scanner_prefix, integer(S), scanner_postfix, blanks.

beacons(B) --> beacon(B).
beacons([B|Bs]) --> beacon(B), beacons(Bs).

beacon(B) --> integer(X), comma, integer(Y), comma, integer(Z), blanks, {B = beacon(X,Y,Z)}.
comma --> [C], {char_code(',', C)}.

scanner_prefix --> `--- scanner `.
scanner_postfix --> ` ---`.
                                       
day19_p1(File, Score):-
    phrase_from_file(scanners(S), File),
    Score = 0,
    writeln(S).

day19_parse:-
    phrase_from_file(scanners(B), "data/day19_parse"),
    writeln(B).

day19_p1(Score):-
    day19_p1("data/day19_p1_data", Score).

day19_p1_test(Score):-
    day19_p1("data/day19_p1_test", Score).

day19_p2(Score):-
    day19_p2("data/day19_p1_data", Score).

day19_p2_test(Score):-
    day19_p2("data/day19_p1_test", Score).

day19:-
    day19_p1_test(4140),
    day19_p1(2541),
    day19_p2_test(3993),
    day19_p2(4647).
