:- module(astar, [astar/6]).

remove(R, List, NewList):-
    partition(=(R), List, _, NewList).

astar_known_cost(Costs, State, Cost, NewCosts):-
    put_assoc(State, Costs, Cost, NewCosts).

astar_known_cost(Costs, State, Cost):-
    ( get_assoc(State, Costs, KCost) ->
      KCost = Cost
    ;
      Cost = 100000
    ).

astar_guess_cost(gstate(Costs,Heap), State, Cost, NewGuessCosts):-
    put_assoc(State, Costs, Cost, NewCosts),
    put_assoc(Cost-State, Heap, State, NewHeap),
    NewGuessCosts = gstate(NewCosts, NewHeap).

astar_best_guess(gstate(GuessCosts, Heap), Open, NewOpen, NewGuessCosts, Min):-
    del_min_assoc(Heap, _-State,_, NewHeap),
    NewGuessCosts = gstate(GuessCosts, NewHeap),
    Min = State,
    remove(Min, Open, NewOpen).

astar_update_neighbor(Goal, CostP, HeuristicP, CurrentCost, Neighbor,
		      nstate(GuessCosts, KnownCosts, Open),
		      nstate(NewGuessCosts, NewKnownCosts, NewOpen)):-

    call(CostP, Neighbor, NeighborCost),
    TentativeCost is CurrentCost + NeighborCost,
    astar_known_cost(KnownCosts, Neighbor, KnownCost),
    (TentativeCost < KnownCost ->
	 astar_known_cost(KnownCosts, Neighbor, TentativeCost, NewKnownCosts),
	 call(HeuristicP, Neighbor, Goal, HCost),
	 NewGuessCost is TentativeCost + HCost,
	 astar_guess_cost(GuessCosts, Neighbor, NewGuessCost, NewGuessCosts),
	 (\+ member(Neighbor, Open) ->
	      NewOpen = [Neighbor|Open]
	 ;
              NewOpen = Open
	 )
    ;
	 nstate(GuessCosts, KnownCosts, Open) =
	 nstate(NewGuessCosts, NewKnownCosts, NewOpen)
    ).

astar_update_neighbor(_, _, Neighbor, In, In):-
    writeln(["no neighborupdate", Neighbor]).

astar_step(sstate(Goal, NeighborP, CostP, HeuristicP), astate( Open, KnownCosts, GuessCosts), Next):-
    astar_best_guess(GuessCosts, Open, NewOpen, NewGuessCosts, Current),
    astar_known_cost(KnownCosts, Current, CurrentCost),
    (Current \= Goal ->
	 call(NeighborP, Current, Neighbors),
	 foldl(astar_update_neighbor(Goal, CostP, HeuristicP, CurrentCost),
	       Neighbors,
	       nstate(NewGuessCosts, KnownCosts, NewOpen),
	       nstate(NewNewGuessCosts, NewKnownCosts, NewNewOpen)),
	 Next = astate(NewNewOpen, NewKnownCosts, NewNewGuessCosts)
    ;
         astar_known_cost(KnownCosts, Goal, Cost),
         Next = astate(done, Cost)
    ).

astar_(StaticState, State, LeastCost):-
    astar_step(StaticState, State, NewState),
    !,
    ( NewState = astate(done, Cost) ->
      LeastCost = Cost
    ;
      astar_( StaticState, NewState, LeastCost)
    ).

astar_bootstrap(Start,Goal,Heuristic, KnownCosts, GuessCosts):-
    empty_assoc(InitialCosts),
    astar_known_cost(InitialCosts, Start, 0, KnownCosts),

    empty_assoc(InitialGuessCosts),
    empty_assoc(InitialGuestHeap),
    InitialGuess = gstate(InitialGuessCosts, InitialGuestHeap),
    call(Heuristic, Start, Goal, HCost),
    astar_guess_cost(InitialGuess, Start, HCost, GuessCosts).

astar(Start, Goal, Neighbors, Cost, Heuristic, LeastCost):-
    astar_bootstrap(Start, Goal, Heuristic, KnownCosts, GuessCosts),
    astar_(sstate(Goal, Neighbors, Cost, Heuristic),
	   astate([Start], KnownCosts, GuessCosts),
	   LeastCost).
