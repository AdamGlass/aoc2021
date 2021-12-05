:- use_module(library(dcg/basics)).

binary_strings([]) --> eos.
binary_strings([X]) --> binary_value(X).
binary_strings([X|Xs]) --> binary_value(X), blank, binary_strings(Xs).

binary_value(X) --> binary_string(X).

binary_string([X]) --> binary_digit(X).
binary_string([X|Xs]) --> binary_digit(X),binary_string(Xs).

% binary_digit(D) --> [D], { char_code('0', D)}.
%  binary_digit(D) --> [D], { char_code('1', D)}.

binary_digit(0) --> [C], { char_code('0', C)}.
binary_digit(1) --> [C], { char_code('1', C)}.

added_list([], [], []).
added_list([A|As], [B|Bs], [C|Cs]):-
    C is A+B,
    added_list(As, Bs, Cs).

list_sum_bad([X|Xs], Result):-
    list_sum(Xs, OldResult), added_list(OldResult, X, Result).

list_sum_acc([], Acc, Acc).
list_sum_acc([X|Xs], Acc, Sums):-
    added_list(X, Acc, NewAcc), list_sum_acc(Xs, NewAcc, Sums).

list_sum([X|Xs], Sums):-
    length(X, Lx), length(AccX, Lx), maplist(=(0), AccX),
    list_sum_acc([X|Xs], AccX, Sums).

sum_map([], _, []).
sum_map([L|Ls], Threshold, A):-
    L > Threshold,
    sum_map(Ls, Threshold, NewA),
    A = [1|NewA].
sum_map([L|Ls], Threshold, A):-
    L < Threshold,
    sum_map(Ls, Threshold, NewA),
    A = [0|NewA].

gamma_sum(AL, HalfL, NL):-
   sum_map(AL, HalfL, NL).

binary_build([], _, Acc, Acc).
binary_build([L|Ls], Power, Acc, Val):-
    NewAcc is Acc + Power*L,
    NewPower is Power * 2,
    binary_build(Ls, NewPower, NewAcc, Val).

binary_to_dec(L, Val):-
    reverse(L, RL), binary_build(RL, 1, 0, Val).

invert_val(0, 1).
invert_val(1, 0).
invert(L, IL):-
    maplist(invert_val, L, IL).

compute_gamma_epsilon(BS, Gamma, Epsilon) :-
    list_sum(BS, AL), length(BS, L), HalfL is L / 2,
    gamma_sum(AL, HalfL, GammaL), binary_to_dec(GammaL, Gamma),
    invert(GammaL, EpsilonL), binary_to_dec(EpsilonL, Epsilon).

day3_p1(File, V):-
    phrase_from_file(binary_strings(Diag), File),
    compute_gamma_epsilon(Diag, Gamma, Epsilon),
    V is Gamma * Epsilon.

bitsplit(Bit, L):-
    nth0(Bit, L, 0).

partition_bit(Diag, Bit, Zeros, Ones):-
    partition(bitsplit(Bit), Diag, Zeros, Ones).

most_common_oxy([X], _, X).
most_common_oxy(Diag, Bit,  V):-
    partition_bit(Diag, Bit, Zeros, Ones),
    length(Zeros, LZeros), length(Ones,LOnes),
    NewBit is Bit + 1,
    ( LZeros > LOnes ->
        most_common_oxy(Zeros, NewBit, V)
    ;
        most_common_oxy(Ones, NewBit, V)
    ).

most_common_co2([X], _, X).
most_common_co2(Diag, Bit,  V):-
    partition_bit(Diag, Bit, Zeros, Ones),
    length(Zeros, LZeros), length(Ones,LOnes),
    NewBit is Bit + 1,
    ( LZeros =< LOnes->
        most_common_co2(Zeros, NewBit, V)
    ;
        most_common_co2(Ones, NewBit, V)
    ).

compute_oxygen(Diag, V):-
    most_common_oxy(Diag, 0, V).

compute_co2(Diag, V):-
    most_common_co2(Diag, 0, V).

day3_p2(File, V):-
    phrase_from_file(binary_strings(Diag), File),
    compute_oxygen(Diag, OL), compute_co2(Diag, CL),
    binary_to_dec(OL, O), binary_to_dec(CL, C), V is O*C.

day3_p1_test(V):-
    day3_p1("data/day3_p1_test", V).

day3_p1(V):-
    day3_p1("data/day3_p1_data", V).

day3_p2_test(V):-
    day3_p2("data/day3_p1_test", V).

day3_p2(V):-
    day3_p2("data/day3_p1_data", V).

day3:-
    day3_p1_test(198),
    day3_p1(693486),
    day3_p2_test(230),
    day3_p2(3379326).
