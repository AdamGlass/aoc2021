:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).

entries([ent(P, O)]) --> entry(P,O), eos.
entries([ent(P, O)|Xs]) --> entry(P,O), entries(Xs).

entry(P, O) --> patterns(P), blank, pipe, blank, patterns(O), blanks.
pipe --> [P], {char_code('|', P)}.
space --> [S], {char_code(' ', S)}.

patterns([P]) --> pattern(P).
patterns([P|Ps]) --> pattern(P), space, patterns(Ps).

pattern([P]) --> alpha(P).
pattern([P|Ps]) --> alpha(P), pattern(Ps).

alpha(A) --> [A], {code_type(A, alnum)}.

count_1478(L, Count):-
    findall(C, (member(C, L), length(C, ELength), member(ELength, [2, 3, 4, 7])), Bag), length(Bag, Count).

day8_p1(File, DigitCount):-
    phrase_from_file(entries(E), File),
    findall(Count, (member(ent(_, O), E), count_1478(O, Count)), Counts), sum_list(Counts, DigitCount).

day8_p1(Count):-
    day8_p1("data/day8_p1_data", Count).

day8_p1_test(Count):-
    day8_p1("data/day8_p1_test2", Count).

xdigit(0, [a,b,c,e,f,g]).
xdigit(1, [c,f]).
xdigit(2, [a,c,d,e,g]).
xdigit(3, [a,c,d,f,g]).
xdigit(4, [b,c,d,f]).
xdigit(5, [a,b,d,f,g]).
xdigit(6, [a,b,d,e,f,g]).
xdigit(7, [a,c,f]).
xdigit(8, [a,b,c,d,e,f,g]).
xdigit(9, [a,b,c,d,f,g]).

wiring(Wires):-
    char_code('a', Start),
    char_code('g', End),
    findall(X, between(Start, End, X), OrigWiring),
    permutation(OrigWiring, Wires).

check(Val, Assign):-
    permutation(Val, Assign).

check1(Val, Assign):-
    check(Val, Assign).

check4(Val, Assign):-
    check(Val, Assign).

check7(Val, Assign):-
    check(Val, Assign).

check8(Val, Assign):-
    check(Val, Assign).

check_three(OneTwoThree, [A,B,C]):-
    permutation(OneTwoThree, [X,Y,Z]),
    permutation(X, A),
    permutation(Y, B),
    permutation(Z, C).

decode_output(Output, Decoder, Value):-
    findall(Number, (member(D, Output), permutation(D, DP), nth0(Number, Decoder, DP)), [A,B,C,D]),
    Value is A*1000+B*100+C*10+D.

decode_entry(ent(Pattern, Output), Value):-
    wiring([A,B,C,D,E,F,G]),
    partition([X]>>length(X, 2), Pattern, [One],  Rest),
    check1(One, [C,F]),
    partition([X]>>length(X, 4), Rest, [Four], Rest2),
    check4(Four, [B,C,D,F]),
    partition([X]>>length(X, 3), Rest2, [Seven], Rest3),
    check7(Seven, [A,C,F]),
    partition([X]>>length(X, 7), Rest3, [Eight], Rest4),
    check8(Eight, [A,B,C,D,E,F,G]),
    partition([X]>>length(X, 5), Rest4, TwoThreeFive, Rest5),
    Two = [A,C,D,E,G],
    Three = [A,C,D,F,G],
    Five = [A,B,D,F,G],
    check_three(TwoThreeFive, [Two, Three, Five]),
    partition([X]>>length(X, 6), Rest5, ZeroSixNine, _),
    Zero = [A,B,C,E,F,G],
    Six =  [A,B,D,E,F,G],
    Nine = [A,B,C,D,F,G],
    check_three(ZeroSixNine, [Zero, Six, Nine]),
    Decoder = [Zero, One, Two, Three,Four, Five,Six,Seven,Eight,Nine],
    decode_output(Output, Decoder, Value).

day8_p2(File, Sum):-
    phrase_from_file(entries(Entries), File),
    findall(Number, (member(Entry, Entries),
		     decode_entry(Entry, Number)), NumberList),
    sum_list(NumberList, Sum).

day8_p2(Sum):-
    day8_p2("data/day8_p1_data", Sum).

day8_p2_test(Sum):-
    day8_p2("data/day8_p1_test2", Sum).


day8:-
    day8_p1_test(26),
    day8_p1(488),
    day8_p2_test(61229),
    day8_p2(1040429).

day_profile:-
    profile(day8).
