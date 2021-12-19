:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

target(T) --> target_area, range(X), spooge, range(Y), blanks, {T = target(X, Y)}.

target_area --> `target area: x=`.
spooge --> `, y=`.		
		
range(R) --> integer(MIN),  range_sep, integer(MAX),
  	     {R = MIN+MAX}.

range_sep --> `..`.

foo(1).

hit_test(target(XMIN+XMAX, YMIN+YMAX), X+Y):-
    between(XMIN, XMAX, X),
    between(YMIN, YMAX, Y).
    
fire_(Steps, Target, X+Y, XV+YV, AccHeight, MaxHeight):-
    NewSteps is Steps - 1,
    NewX is X + XV,
    NewY is Y + YV,
    (XV > 0 ->
	 NewXV is XV - 1
    ; XV < 0 ->
         NewXV is XV + 1
    ;
         NewXV is XV
    ),
    NewYV is YV - 1,
    NewAccHeight is max(AccHeight, NewY),
    ( hit_test(Target, NewX+NewY) ->
        MaxHeight = NewAccHeight
    ; NewSteps > 0 -> 
      fire_(NewSteps, Target, NewX+NewY, NewXV+NewYV, NewAccHeight, MaxHeight)
    ; 
      fail
    ).

fbetween(A, B, X):-
    between(A,B, X).

fire(Target, HitList):-
    Target = target(XMIN+XMAX, YMIN+YMAX),
    YVMax is abs(YMIN),
    YVMin is -abs(YMIN),
    findall(Height-XV+YV,
	    (between(0, XMAX, XV),
	     between(YVMin, YVMax, YV),
	     fire_(1000, Target, 0+0, XV+YV, 0, Height)),
	    HitList).

day17_p1(File, Score):-
    phrase_from_file(target(T), File),
    fire(T, HitList),
    max_member(MaxHeight-XV+YV, HitList),
    Score = MaxHeight.

day17_p2(File, Score):-
    phrase_from_file(target(T), File),
    fire(T, HitList),
    length(HitList, Count),
    Score = Count.

day17_p1(Score):-
    day17_p1("data/day17_p1_data", Score).

day17_p1_test(Score):-
    day17_p1("data/day17_p1_test", Score).

day17_p2(Score):-
    day17_p2("data/day17_p1_data", Score).

day17_p2_test(Score):-
    day17_p2("data/day17_p1_test", Score).

day17:-
    day17_p1_test(45),
    day17_p1(17766),
    day17_p2_test(112),
    day17_p2(1733).
