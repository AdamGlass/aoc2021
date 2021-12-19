:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

spairs([S]) --> spair_line(S).
spairs([S|Ss]) --> spair_line(S), spairs(Ss).

spair_line(L) --> spair(L), blanks.

spair(X) --> left_bracket, num_pair(A), comma, num_pair(B), right_bracket, { X = [A,B]}.

num_pair(X) --> spair(X).
num_pair(X) --> integer(X).
comma --> [C], {char_code(',', C)}.
left_bracket --> [C], {char_code('[', C)}.
right_bracket --> [C], {char_code(']', C)}.

exploded(Right, Acc, Out):-
    reverse(Acc, AccOut),
    append(AccOut, Right, Out).

explode([], Acc, Out):-
    !,
    reverse(Acc, Out).

explode([_+5], Acc, Out):-
    !,
    reverse(Acc, Out).

explode([L+LD,A+5,B+5,R+RD|Es], Acc, Out):-
    LD \= 5,
    RD \= 5,
    !,
    NewR is R+B,
    NewL is L+A,
    exploded(Es, [NewR+RD,0+4,NewL+LD|Acc], Out).

explode([_+5,B+5,R+RD|Es], Acc, Out):-
    RD \= 5,
    !,
    NewR is R+B,
    exploded(Es, [NewR+RD,0+4|Acc], Out).

explode([L+LD,A+5,_+5|Es], Acc, Out):-
    LD \= 5,
    !,
    NewL is L+A,
    exploded(Es, [0+4,NewL+LD|Acc], Out).

explode([E|Es], Acc, Out):-
    !,
    explode(Es, [E|Acc], Out).

explode(A, B):-
    explode(A, [], B).

squish([A,B], Depth, Out):-
    NextDepth is Depth+1,
    squish(A, NextDepth, AOut),
    squish(B, NextDepth, BOut),
    append(AOut, BOut, Out).

squish(A, Depth, Out):-
    \+ is_list(A),
    Out = [A+Depth].

squish(A, B):-
    squish(A, 0, B).

splits([], Acc, Out):-
    reverse(Acc, Out).

splits([A+D|As], Acc, Out):-
    A < 10,
    splits(As, [A+D|Acc], Out).
    
splits([A+AD|As], Acc, Out):-
    A >= 10,
    NewDepth is AD+1,
    NewA is round((A-1)/2),
    NewB is round(A/2),
    reverse([NewB+NewDepth,NewA+NewDepth|Acc],NewAcc),
    append(NewAcc, As, Out).

increase_depth(A+AD, A+CD):-
    CD is AD+1.

adds(A, B, C):-
    maplist(increase_depth, A, NewA),
    maplist(increase_depth, B, NewB),
    append(NewA, NewB, C).

excessive_depth(C):-
    \+ forall(member(_+D, C),
	      D < 5).

big_number(C):-
    \+ forall(member(A+_, C),
	      A < 10).

reduce(A, B):-
    writeln([reduce, A]),
    ( excessive_depth(A) ->
      writeln("exploding"),
      explode(A, [], NewA),
      reduce(NewA, B)
    ; big_number(A) ->
      writeln("splitting"),
      splits(A, [], NewA),
      reduce(NewA, B)
    ;
      B = A
    ).

compare_print(A,B,A):-
    writeln([passed, A, B]).

compare_print(A,B,C):-
    \+ B=C,
    writeln([failed, A, B, C]).

compare_answers(A,B,C):-
    maplist(compare_print, A, B, C).

sums(A, InState, OutState):-
    adds(InState, A, R),
    reduce(R, OutState).

day18_verify(File, P):-
    phrase_from_file(spairs(Inputs), File),
    string_concat(File, "_answers", AnswerFile),
    phrase_from_file(spairs(Outputs), AnswerFile),
    same_length(Inputs, Outputs),
    maplist(squish, Inputs, NewInputs),
    maplist(squish, Outputs, NewOutputs),
    maplist(P, NewInputs, ProposedOutputs),
    ( ProposedOutputs = NewOutputs ->
      writeln("tests passed")
    ;
      compare_answers(NewInputs, NewOutputs, ProposedOutputs)
    ).

day18_sum_verify(File):-
    writeln(File),
    phrase_from_file(spairs(Inputs), File),
    string_concat(File, "_answer", AnswerFile),
    phrase_from_file(spairs([Output|_]), AnswerFile),
    maplist(squish, Inputs, [N|Ns]),
    squish(Output, NewOutput),
    foldl(sums, Ns, N, ProposedOutput),
    (NewOutput = ProposedOutput ->
	 writeln([passed, NewOutput, ProposedOutput])
    ;
	 writeln([failed, NewOutput, ProposedOutput])
    ).         

day18_parse_test:-
    phrase_from_file(spairs(T), "data/day18_explode_answers"),
    writeln(T).


day18_explode_test:-
    day18_verify("data/day18_explode", explode).

day18_reduce_test:-
    day18_verify("data/day18_reduce", reduce).

day18_sums_test:-
    T =  ["data/day18_sum0", "data/day18_sum1","data/day18_sum2","data/day18_sum3","data/day18_sum4"],
    forall(member(F, T),
	   day18_sum_verify(F)).

day18_p1(Score):-
    day18_p1("data/day18_p1_data", Score).

day18_p1_test(Score):-
    day18_p1("data/day18_p1_test", Score).

day18_p2(Score):-
    day18_p2("data/day18_p1_data", Score).

day18_p2_test(Score):-
    day18_p2("data/day18_p1_test", Score).

day18:-
    day18_p1_test(45),
    day18_p1(17766),
    day18_p2_test(112),
    day18_p2(1733).
