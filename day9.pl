:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).

map(M) --> mapx(MapData), 
	   {length(MapData, Y),
	    member(M0, MapData),
	    length(M0, X),
	    M = mapdata(MapData, X, Y)
	   }.

mapx([M]) --> mapline(M).
mapx([M|Ms]) --> mapline(M), mapx(Ms).

mapline(M) --> mdigits(M), blank.

mdigits([D]) --> mdigit(D).
mdigits([D|Ds]) --> mdigit(D), mdigits(Ds).

mdigit(V) --> [C], {char_type(C, digit), char_code('0', Zero), V is C - Zero}.

coord(mapdata(Map, MapWidth, MapHeight), location(X, Y), Value):-
    NWidth is MapWidth - 1,
    NHeight is MapHeight - 1,
    between(0, NWidth, X),
    between(0, NHeight, Y),
    nth0(Y, Map, Row),
    nth0(X, Row, Value).

coord(_,location(X,Y), 9):-
    (X < 0 ; Y < 0).
coord(mapdata(_, MapWidth, MapHeight),location(X,Y), 9):-
    (X >= MapWidth ; Y >= MapHeight).

neighbors(mapdata(_, MapWidth, MapHeight), location(X,Y), NeighborSet):-
    between(-1, MapWidth, X),
    between(-1, MapHeight, Y),
    LX is X - 1,
    RX is X + 1,
    UY is Y - 1,
    DY is Y + 1,
    NeighborList = [location(LX,Y), location(RX, Y), location(X, UY), location(X, DY)],
    list_to_ord_set(NeighborList, NeighborSet).

neighbors(_, location(X,Y), []):-
    (X < 0 ; Y < 0).
neighbors(mapdata(_, MapWidth, MapHeight), location(X,Y), []):-
    (X >= MapWidth ; Y >= MapHeight).

neighbors(_, [], AccSet, AccSet).
neighbors(Map, [N|Ns], AccSet, Set):-
    neighbors(Map, N, NeighborSet),
    ord_union(AccSet, NeighborSet, NewAccSet),
    neighbors(Map, Ns, NewAccSet, Set).

neighbor_heights(Map, L, Heights):-
    neighbors(Map, L, NeighborList),
    findall(Height, 
	    (member(N, NeighborList),
	     coord(Map, N, Height)),
	    Heights).

low_point(Map, L, Risk):-
    coord(Map, L, Height),
    neighbor_heights(Map, L, Heights),
    Risk is Height + 1,
    min_list(Heights, NeighborHeight),
    Height < NeighborHeight.

low_points(mapdata(Map, MapWidth, MapHeight), LowPoints):-
    NMapWidth is MapWidth - 1,
    NMapHeight is MapHeight - 1,
    findall(lowpoint(location(X,Y), Risk), (
		between(0, NMapWidth, X),
		between(0, NMapHeight, Y),
		low_point(mapdata(Map, MapWidth, MapHeight), location(X, Y), Risk)
		),
	    LowPoints).

basin_acc(Map, BasinSet, Threshold, Size):-
    neighbors(Map, BasinSet, [], NeighborSet),
    ord_subtract(NeighborSet, BasinSet, ExploreSet),
    findall(E,
	    (member(E, ExploreSet),
	     coord(Map, E, NeighborHeight),
	     NeighborHeight \= 9,
	     NeighborHeight >= Threshold
	    ),
	    NewBasin),
    ( NewBasin = [] ->
      length(BasinSet, Size)
    ;
      list_to_ord_set(NewBasin, NewBasinSet),
      NewThreshold is Threshold + 1,
      ord_union(BasinSet, NewBasinSet, NextBasinSet),
      basin_acc(Map, NextBasinSet, NewThreshold, Size)
    ).

basin(Map, L, BasinSize):-
    coord(Map, L, Height),
    Threshold is Height + 1,
    basin_acc(Map, [L], Threshold, BasinSize).

day9_p1(File, Score):-
    phrase_from_file(map(Map), File),
    low_points(Map, LowPoints),
    findall(R, member(lowpoint(_,R), LowPoints), Risks),
    sum_list(Risks, Score).

day9_p1(Score):-
    day9_p1("data/day9_p1_data", Score).

day9_p1_test(Score):-
    day9_p1("data/day9_p1_test", Score).

day9_p2(File, Score):-
    phrase_from_file(map(Map), File),
    low_points(Map, LowPoints),
    findall(BasinSize, 
	    (member(lowpoint(L,_), LowPoints),
	     basin(Map, L, BasinSize)),
	    Basins),
    sort(0, @>=, Basins, [A,B,C|_]),
    Score is A * B *C.

day9_p2(File, Score):-
    day9_common(File, crab_p2_fuel, Score).

day9_p2(Score):-
    day9_p2("data/day9_p1_data", Score).

day9_p2_test(Score):-
    day9_p2("data/day9_p1_test", Score).

day9:-
    day9_p1_test(15),
    day9_p1(417),
    day9_p2_test(1134),
    day9_p2(1148965).

