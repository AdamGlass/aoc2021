:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

                                       
scanners([S]) --> scanner(S).
scanners([S|Ss]) --> scanner(S), blanks, scanners(Ss).

scanner(S) --> scanner_header(H), beacons(D), 
	       { (H = 0 ->
		      S = scanner(H, D, 0+0+0)
		 ;
		      S = scanner(H, D, X+Y+Z)
		 )
	       }.
 
scanner_header(S) --> scanner_prefix, integer(S), scanner_postfix, blanks.

beacons([B]) --> beacon(B).
beacons([B|Bs]) --> beacon(B), beacons(Bs).

beacon(B) --> integer(X), comma, integer(Y), comma, integer(Z), blanks, {B = beacon(X,Y,Z)}.
comma --> [C], {char_code(',', C)}.

scanner_prefix --> `--- scanner `.
scanner_postfix --> ` ---`.

pairwise_distances(scanner(_,Beacons, _), PairWiseDistances):-
    findall(PairWiseDistance,
	   (member(R, Beacons),
	    member(N, Beacons),
	    R \= N,
	    R = beacon(RX, RY, RZ),
	    N = beacon(NX, NY, NZ),
	    PairWiseDistance is abs(RX-NX) + abs(RY-NY) + abs(RZ-NZ)),
	   Distances),
    sort(Distances, PairWiseDistances).

count_vals(X-L, Count):-
    sum_list(L, Count).

scanner_merge(Scanner, Scanners):-
    pairwise_distances(Scanner, Scanner0Distances),
    writeln(Scanner0Distances),
    maplist(pairwise_distances, Scanners, BeaconDistances),
    findall(N-Commonalities,
	    (nth0(N, BeaconDistances, Distances),
	     member(Scanner0, Scanner0Distances),
	     member(Scanner0, Distances),
	     Commonalities = 1),
	    Commons),
    group_pairs_by_key(Commons, GroupedCommons),
    maplist(count_vals, GroupedCommons, CountedGroupCommons),
    writeln(CountedGroupCommons).

day19_p1(File, Score):-
    phrase_from_file(scanners(Scanners), File),
    [S|Ss] = Scanners,
    scanner_merge(S, Ss).

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
