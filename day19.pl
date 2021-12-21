:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

                                       
scanners([S]) --> scanner(S).
scanners([S|Ss]) --> scanner(S), blanks, scanners(Ss).

scanner(S) --> scanner_header(H), beacons(D), { S = scanner(H, D)}.
 
scanner_header(S) --> scanner_prefix, integer(S), scanner_postfix, blanks.

beacons([B]) --> beacon(B).
beacons([B|Bs]) --> beacon(B), beacons(Bs).

beacon(B) --> integer(X), comma, integer(Y), comma, integer(Z), blanks, {B = beacon(X,Y,Z)}.
comma --> [C], {char_code(',', C)}.

scanner_prefix --> `--- scanner `.
scanner_postfix --> ` ---`.
foo(1).

orientation_gen(location(A,B,C), location(O1, O2, O3)):-
    permute([A,B,C], [O1, O2, O3]).

transform_gen(location(BX, BY, BZ), location(OBX, OBY, OBZ):-
    orientation(location(BX, BY, BZ, location(OBX, OBY, OBZ))),
    permutation_gen(A, B, C
    length(L, Beacons),


overlapping_beacons(beacon(A,AB), beacon(B, BB), Beacons):-
    location(AX, AY, AZ),
    location(BX, BY, BZ),
    [AX, AY, AZ, BX, BY, BZ] ins inf..sup,
    AX = AY = AZ = 0,
    member(beacon(RX, RY, RZ),
	   AB),

    location(BX, BY, BZ),
    

    member(Beacon, AB),
    member(
    

overlap_beacons(Scanners):-
    forall((member(A, Scanners),
	    A \= B,
	    member(B, Scanners)),
	   ovelapping_beacons(A, B, Beacons)).

beacon_count(Scanners, BeaconCount):-
    overlap_beacons(Scanners).

day19_p1(File, Score):-
    phrase_from_file(scanners(Scanners), File),
    beacon_count(Scanners, BeaconCount).

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
