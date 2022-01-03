:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).


scanners([S]) --> scanner(S).
scanners([S|Ss]) --> scanner(S), blanks, scanners(Ss).

scanner(S) --> scanner_header(H), beacons(D),
	       { S = scanner(H, D) }.

scanner_header(S) --> scanner_prefix, integer(S), scanner_postfix, blanks.

beacons([B]) --> beacon(B).
beacons([B|Bs]) --> beacon(B), beacons(Bs).

beacon(B) --> integer(X), comma, integer(Y), comma, integer(Z), blanks, {B = beacon(X,Y,Z)}.
comma --> [C], {char_code(',', C)}.

scanner_prefix --> `--- scanner `.
scanner_postfix --> ` ---`.

foo(3).

make_pair(H,A,[H,A]).
%
% scanner_merge(Scanner, Scanners):-
%     pairwise_distances(Scanner, Scanner0Distances),
%     writeln(Scanner0Distances),
%     maplist(pairwise_distances, Scanners, BeaconDistances),
%     findall(N-Commonalities,
% 	    (nth0(N, BeaconDistances, Distances),
% 	     member(Scanner0, Scanner0Distances),
% 	     member(Scanner0, Distances),
% 	     Commonalities = 1),
% 	    Commons),
%     group_pairs_by_key(Commons, GroupedCommons),
%     maplist(count_vals, GroupedCommons, CountedGroupCommons),
%     writeln(CountedGroupCommons).
%
% make_beacon(B):-
%     B = beacon(X,Y,Z),
%     [X,Y,Z] ins inf..sup.
%
% orient(X+Y+Z, OX+OY+OZ):-
%     NX #= -X,
%     NY #= -Y,
%     NZ #= -Z,
%     member(AX, [X, NX]),
%     member(AY, [Y, NY]),
%     member(AZ, [Z, NZ]),
%     permutation([AX,AY,AZ], [OX,OY,OZ]).
%
% identify_beacon_locations(S0, SharedMostScanner, Beacons):-
%     S0 = scanner(_,S0Beacons, _).
%     SharedMostScanner = scanner(_,MBeacons, _).
%     [X,Y,Z] in inf..sup,
%     findall(X+Y+Z
% 		(orient(X+Y+Z, OX+OY+OZ),
% 		 member(
% 	  findall(	      member(beacon(SX, SY, SZ), S0Beacons),
%
% count_vals(X-L, Count):-
%     sum_list(L, Count).


pairs_([], AccPairs, AccPairs).
pairs_([H|T], AccPairs, Pairs):-
    maplist(make_pair(H), T, HPair),
    append(AccPairs, HPair, NewPairs),
    pairs_(T, NewPairs, Pairs).

pairs(L, Pairs):-
    pairs_(L, [], Pairs).

pairwise_distances(Beacons, PairWiseDistances):-
    findall(PairWiseDistance,
	    (pairs(Beacons, BeaconPairs),
	     member([beacon(RX, RY, RZ),beacon(NX, NY, NZ)], BeaconPairs),
	     PairWiseDistance is abs(RX-NX) + abs(RY-NY) + abs(RZ-NZ)),
	   PairWiseDistances),
    writeln(PairWiseDistances).
label_beacons(Scanner, Current, Scanner):-
    Current = scanner(N,_),
    writeln([label_beacons, N]).

shared_distances(AFingerprint, BFingerprint, Count):-
    append(AFingerprint, BFingerprint, Fingerprints),
    sort(Fingerprints, UniqueFingerprints),
    length(Fingerprints, AllFingerprintsCount),
    length(UniqueFingerprints, UniqueFingerprintsCount),
    Count is AllFingerprintsCount - UniqueFingerprintsCount.

fingerprint_beacons(Scanner, Scanner-Fingerprint):-
    Scanner = scanner(_, Beacons),
    pairwise_distances(Beacons, Fingerprint).

identify_shared_most_scanner(GoalFingerprint, ScannerFingerprints, MostSharedScanner-MostSharedFingerprint):-
    findall(Count-Scanner-Fingerprint,
	    (member(Scanner-Fingerprint, ScannerFingerprints),
	     shared_distances(GoalFingerprint, Fingerprint, Count),
	     Count >= 12),
	    CommonDistanceCount),
    max_member(_-MostSharedScanner-MostSharedFingerprint, CommonDistanceCount).

identify_common_beacons(Scanner-_, [], Scanner).
identify_common_beacons(Scanner-Fingerprint, ScannerFingerprints, UnifiedScanner):-
    identify_shared_most_scanner(Fingerprint, ScannerFingerprints, MostSharedScanner-MostSharedFingerprint),
    label_beacons(Scanner, MostSharedScanner, NewScanner),
    fingerprint_beacons(NewScanner, NewScanner-NewFingerprint),
    delete(ScannerFingerprints, MostSharedScanner-MostSharedFingerprint, NewScannerFingerprints),
    identify_common_beacons(NewScanner-NewFingerprint, NewScannerFingerprints, UnifiedScanner).

identify_beacons(Scanners, Beacons):-
    maplist(fingerprint_beacons, Scanners, ScannerFingerprints),
    [S0 | Ss] = ScannerFingerprints,
    identify_common_beacons(S0, Ss, NewS0),
    scanner(_,Beacons) = NewS0.

day19_p1(File, Score):-
    phrase_from_file(scanners(Scanners), File),
    identify_beacons(Scanners, Beacons),
    length(Beacons, Score).

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
