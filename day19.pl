:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- table fingerprint_beacons/2, scanner_distance/2.

scanners([S]) --> scanner(S).
scanners([S|Ss]) --> scanner(S), blanks, scanners(Ss).

scanner(S) --> scanner_header(H), beacons(D),
	       { S = scanner(H, D, [location(0,0,0)]) }.

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
%
% identify_shared_most_scanner(GoalFingerprint, ScannerFingerprints, MostSharedScanner-MostSharedFingerprint):-
%     findall(Count-Scanner-Fingerprint,
% 	    (member(Scanner-Fingerprint, ScannerFingerprints),
% 	     shared_distances(GoalFingerprint, Fingerprint, Count),
% 	     Count >= 66),
% 	    CommonDistanceCount),
%     max_member(_-MostSharedScanner-MostSharedFingerprint, CommonDistanceCount).

% too many (48 instead of 24)
xxx_orientation(Orient):-
    member(AX, [x, -x]),
    member(AY, [y, -y]),
    member(AZ, [z, -z]),
    permutation([AX,AY,AZ], Orient).

orientation(Orient):-
    member(Orient, [
               [x, y, z], [x, z, -y], [x, -y, -z], [x, -z, y],
               [-x, -y, z], [-x, z, y], [-x, y, -z], [-x, -z, -y],
               [y, z, x], [y, x, -z], [y, -z, -x], [y, -x, z],
               [-y, -z, x], [-y, x, z], [-y, z, -x], [-y, -x, -z],
               [z, x, y], [z, y, -x], [z, -x, -y], [z, -y, x],
               [-z, -x, y], [-z, y, x], [-z, x, -y], [-z, -y, -x]
	   ]).

orient_one([X,Y,Z], Orientation, Oriented):-
    (Orientation = x ->
	 Oriented = X
    ;Orientation = -x ->
         NX is -X,
         Oriented = NX
    ;Orientation = y ->
	 Oriented = Y
    ;Orientation = -y ->
         NY is -Y,
         Oriented = NY
    ;Orientation = z ->
	 Oriented = Z
    ; % implied -z
         NZ is -Z,
         Oriented = NZ
    ).

orient(Orient, Incoming, Oriented):-
    maplist(orient_one(Incoming), Orient, Oriented).

pairs_([], AccPairs, AccPairs).
pairs_([H|T], AccPairs, Pairs):-
    maplist(make_pair(H), T, HPair),
    append(AccPairs, HPair, NewPairs),
    pairs_(T, NewPairs, Pairs).

pairs(L, Pairs):-
    pairs_(L, [], Pairs).

manhattan(beacon(RX,RY,RZ), beacon(NX,NY,NZ), Distance):-
    Distance is abs(RX-NX) + abs(RY-NY) + abs(RZ-NZ).

pairwise_distances(Beacons, PairWiseDistances):-
    findall(PairWiseDistance,
	    (pairs(Beacons, BeaconPairs),
	     member([beacon(RX, RY, RZ),beacon(NX, NY, NZ)], BeaconPairs),
	     manhattan(beacon(RX,RY,RZ),beacon(NX,NY,NZ), PairWiseDistance)),
	    PairWiseDistances).

beacon_solve(K, C, Orient, [X,Y,Z]):-
    K = beacon(KX, KY, KZ),
    C = beacon(CX, CY, CZ),
    orient(Orient, [CX, CY, CZ], [NX, NY, NZ]),
    KX #= X + NX,
    KY #= Y + NY,
    KZ #= Z + NZ.

remap_beacon(Location, Orient, C, K):-
    beacon_solve(K, C, Orient, Location).

remap_seen_scanner(Location, Orient, CL, KL):-
    KL = location(KX, KY, KZ),
    CL = location(CX, CY, CZ),
    K = beacon(KX, KY, KZ),
    C = beacon(CX, CY, CZ),
    beacon_solve(K, C, Orient, Location).

merge_scanners(Scanner, Current, NewScanner):-
    Current = scanner(N, CurrentBeacons, CurrentSeenScanners),
    Scanner = scanner(S, KnownBeacons, KnownSeenScanners),
    writeln([label_beacons, S,N]),
    member(C1, CurrentBeacons),
    member(C2, CurrentBeacons),
    C1 \= C2,
    manhattan(C1, C2, Distance),
    member(K1, KnownBeacons),
    member(K2, KnownBeacons),
    K1 \= K2,
    manhattan(K1, K2, Distance),
    orientation(Orient),
    [X,Y,Z] ins inf..sup,
    ( beacon_solve(K1, C1, Orient, [X,Y,Z])
    ;
      beacon_solve(K1, C2, Orient, [X,Y,Z])
    ),
    findnsols(12, Beacon,
	      (member(Beacon, CurrentBeacons),
	       member(K, KnownBeacons),
	       beacon_solve(K, Beacon, Orient, [X,Y,Z])),
	      SharedBeacons),
    length(SharedBeacons, Count),
    Count >= 12,
    writeln([shared, N, Count, [X,Y,Z]]),
    maplist(remap_beacon([X,Y,Z], Orient), CurrentBeacons, RemappedBeacons),
    append(KnownBeacons, RemappedBeacons, AllBeacons),
    sort(AllBeacons, AllUniqueBeacons),
    maplist(remap_seen_scanner([X,Y,Z], Orient), CurrentSeenScanners, RemappedSeenScanners),
    append(KnownSeenScanners, RemappedSeenScanners, AllScanners),
    sort(AllScanners, AllUniqueScanners),
    NewScanner = scanner(S, AllUniqueBeacons, AllUniqueScanners).

shared_distances(AFingerprint, BFingerprint, Count):-
    append(AFingerprint, BFingerprint, Fingerprints),
    sort(Fingerprints, UniqueFingerprints),
    length(Fingerprints, AllFingerprintsCount),
    length(UniqueFingerprints, UniqueFingerprintsCount),
    Count is AllFingerprintsCount - UniqueFingerprintsCount.

fingerprint_beacons(Scanner, Scanner-Fingerprint):-
    Scanner = scanner(_, Beacons, _),
    pairwise_distances(Beacons, Fingerprint).
scanner_distance(Scanners, Distance-NewScanners):-
    Scanners = [S1-S1Fingerprint, S2-S2Fingerprint],
    shared_distances(S1Fingerprint, S2Fingerprint, Distance),
    NewScanners = [S1, S2].

identify_beacons([Scanner], Scanner).
identify_beacons(Scanners, OutScanner):-
    length(Scanners, SCount),
    writeln([len, SCount]),
    !,
    maplist(fingerprint_beacons, Scanners, ScannerFingerprints),
    pairs(ScannerFingerprints, PairedScannerFingerprints),
    maplist(scanner_distance, PairedScannerFingerprints, DistanceScanners),
    member(Distance-[S1,S2], DistanceScanners),
    Distance >= 66,
    writeln([distcount, Distance]),
    merge_scanners(S1, S2, NewScanner),
    subtract(Scanners, [S1, S2], ReducedScanners),
    append(ReducedScanners, [NewScanner], NewScanners),
    identify_beacons(NewScanners, OutScanner).

scanner_location_distance([location(AX, AY, AZ), location(BX, BY, BZ)], Distance):-
    manhattan(beacon(AX, AY, AZ), beacon(BX, BY, BZ), Distance).

day19_core(File, BeaconCount, MaxDistance):-
    phrase_from_file(scanners(Scanners), File),
    identify_beacons(Scanners, OutScanner),
    OutScanner = scanner(_, Beacons, Locations),
    length(Beacons, BeaconCount),
    pairs(Locations, PairedLocations),
    maplist(scanner_location_distance, PairedLocations, Distances),
    max_list(Distances, MaxDistance).

day19_p1(File, Score):-
    day19_core(File, Score, _).

day19_p2(File, MaxDistance):-
    day19_core(File, _, MaxDistance).

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
    day19_p1_test(79),
    day19_p1(438),
    day19_p2_test(3621),
    day19_p2(11985).
