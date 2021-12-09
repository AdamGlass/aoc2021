:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- table crab_p2_fuel/2.
:- table crab_p1_fuel/2.

crabs([X]) --> integer(X).
crabs([X|Xs]) --> integer(X), comma, crabs(Xs), blanks.

comma --> [C], {char_code(',', C)}.

crab_p1_fuel(Distance, Distance).

crab_p2_fuel(Distance, Fuel):-
    findall(X, between(0, Distance, X), FuelSpend), sum_list(FuelSpend, Fuel).

crab_move_acc([], _, _, AccFuel, AccFuel).
crab_move_acc([C|Cs], Location, Fuelp, AccFuel, Fuel):-
    ( Location > C ->
      Distance is Location - C
    ;
      Distance is C - Location
    ),
    call(Fuelp, Distance, FuelConsumed),
    NewAcc is AccFuel + FuelConsumed,
    crab_move_acc(Cs, Location, Fuelp, NewAcc, Fuel).

crab_move(Crabs, Location, Fuelp, LocFuel):-
    crab_move_acc(Crabs, Location, Fuelp, 0, LocFuel).


day7_common(File, Fuelp, Fuel):-
    phrase_from_file(crabs(Crabs), File),
    max_list(Crabs, MaxLocation),
    min_list(Crabs, MinLocation),
    findall(crab_sol(LocFuel, Location), 
	    (between(MinLocation, MaxLocation, Location),
	     crab_move(Crabs, Location, Fuelp, LocFuel)), Sols),
    min_member(crab_sol(Fuel, _), Sols).

day7_p1(File, Fuel):-
    day7_common(File, crab_p1_fuel, Fuel).

day7_p1(Fuel):-
    day7_p1("data/day7_p1_data", Fuel).

day7_p1_test(Fuel):-
    day7_p1("data/day7_p1_test", Fuel).

day7_p2(File, Fuel):-
    day7_common(File, crab_p2_fuel, Fuel).

day7_p2(Fuel):-
    day7_p2("data/day7_p1_data", Fuel).

day7_p2_test(Fuel):-
    day7_p2("data/day7_p1_test", Fuel).

day7:-
    day7_p1_test(37),
    day7_p1(337833),
    day7_p2_test(168),
    day7_p2(96678050).
