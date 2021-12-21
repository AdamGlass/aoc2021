:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

stream([P]) --> packet(P).

packets([P]) --> packet(P).
packets([P|Ps]) --> packet(P), packets(Ps).

zeros([]).
zeros([0]) --> bdigit(0).
zeros([0|Zs]) --> bdigit(0), zeros(Zs).

packet(P) --> literal_value(P).
packet(O) --> operator(O).

literal_value(L) --> packet_header(V, 4), pnumber(N, Count),
		     {Bits is 6 + Count*5,
		      L = literal(V, 4, Bits, N)}.

operator(O) --> operator_header(V,T), short_id_type,
		number11(N), packets_count(N, [], P, PBits),
		{Bits is 7 + 11 + PBits,
		 O = op(V,T,Bits, P)
		}.

operator(O) --> operator_header(V,T), long_id_type,
		number15(PBits), packets_bitcount(PBits, [], P),
		{Bits is 7 + 15 + PBits,
		 O = op(V,T,Bits, P)
		}.

operator_header(V,T) --> packet_header(V, T), {T \= 4}.

short_id_type --> bdigit(1).
long_id_type --> bdigit(0).

packet_header(V, I) --> version(V), id(I).

packets_count(0, Acc, P, PBits) --> {reverse(Acc, P), packets_bits_length(P, PBits)}.
packets_count(Count, Acc, P, PBits) -->
    packet(R), {NewCount is Count - 1}, packets_count(NewCount, [R|Acc], P, PBits).

packets_bitcount(0, Acc, P) --> {reverse(Acc, P)}.
packets_bitcount(PBits, Acc, P) -->
    packet(R),
    { packet_bits(R, Bits),
      NewBits is PBits - Bits},
    packets_bitcount(NewBits, [R|Acc], P).

version(V) --> bdigit(A), bdigit(B), bdigit(C), {binary_decimal([A,B,C], V)}.
id(V) --> bdigit(A), bdigit(B), bdigit(C),{binary_decimal([A,B,C], V)}.

pnumber(N, Count) --> padded_group(G), {flatten(G, H), hex_decimal(H,N), length(G, Count)}.

padded_group(G) --> end_group(E), {G = [E]}.
padded_group(G) --> number_groups(N), end_group(E), { append(N, [E], G)}.

number_groups([G]) --> number_group(G).
number_groups([G|Gs]) --> number_group(G), number_groups(Gs).

number_group(G)--> bdigit(1), nibble(G).
end_group(G)--> bdigit(0), nibble(G).

number11(V) --> bnibble(N1), bnibble(N2), bdigit(A), bdigit(B), bdigit(C),
		{ flatten([N1, N2, A, B, C], NB), binary_decimal(NB, V)}.

number15(V) --> bnibble(N1), bnibble(N2), bnibble(N3), bdigit(A), bdigit(B), bdigit(C),
		{ flatten([N1, N2, N3, A, B, C], NB),
		  binary_decimal(NB, V)}.


bnibble(N) --> bdigit(A), bdigit(B), bdigit(C), bdigit(D), {N = [A,B,C,D]}.
nibble(N) --> bnibble(B),{binary_decimal(B, N)}.

bdigit3(B) --> bdigits(B), {length(B, 3)}.
bdigits([B]) --> bdigit(B).
bdigits([B|Bs])--> bdigit(B), bdigits(Bs).

bdigit(0) --> [0].
bdigit(1) --> [1].

hexline(N) --> hex(N), blanks.



hex([H]) --> hexchar(H).
hex([H|Hs]) --> hexchar(H), hex(Hs).

hexchar(V) --> [C], {char_type(C, digit), char_code('0', Zero), V is C - Zero}.
hexchar(V) --> [C], {char_type(C, upper), char_code('A', Zero), V is 10 + C - Zero}.

hex_decimal_([], _, Value, Value).
hex_decimal_([H|Hs], Power , Acc, Value):-
    NewAcc is H*Power + Acc,
    NewPower is Power * 16,
    hex_decimal_(Hs, NewPower, NewAcc, Value).

hex_decimal(HexNumberList, Value):-
    reverse(HexNumberList, R), hex_decimal_(R, 1, 0, Value).

dec_binary_(0, Value, Value).
dec_binary_(Decimal, Acc, Value):-
    NewValue is getbit(Decimal, 0),
    NewAcc = [NewValue|Acc],
    NewDecimal is Decimal >> 1,
    dec_binary_(NewDecimal, NewAcc, Value).

dec_binary_(0, Value, Value).
dec_binary_(Decimal, Acc, Value):-
    NewValue is getbit(Decimal, 0),
    NewAcc = [NewValue|Acc],
    NewDecimal is Decimal >> 1,
    dec_binary_(NewDecimal, NewAcc, Value).

hex_code(0,[0,0,0,0]).
hex_code(1,[0,0,0,1]).
hex_code(2,[0,0,1,0]).
hex_code(3,[0,0,1,1]).
hex_code(4,[0,1,0,0]).
hex_code(5,[0,1,0,1]).
hex_code(6,[0,1,1,0]).
hex_code(7,[0,1,1,1]).
hex_code(8,[1,0,0,0]).
hex_code(9,[1,0,0,1]).
hex_code(10,[1,0,1,0]).
hex_code(11,[1,0,1,1]).
hex_code(12,[1,1,0,0]).
hex_code(13,[1,1,0,1]).
hex_code(14,[1,1,1,0]).
hex_code(15,[1,1,1,1]).

hex_binary_([], Value, Value).
hex_binary_([H|Hs], Acc, Value):-
    hex_code(H, [A,B,C,D]),
    NewAcc = [A,B,C,D|Acc],
    hex_binary_(Hs, NewAcc, Value).

hex_binary(HexNumberList, Value):-
    reverse(HexNumberList, R), hex_binary_(R, [], Value).

binary_decimal_([], _, Value, Value).
binary_decimal_([B|Bs], Power, Acc, Value):-
    NewAcc is B*Power + Acc,
    NewPower is Power * 2,
    binary_decimal_(Bs, NewPower, NewAcc, Value).

binary_decimal(Binary, Decimal):-
    reverse(Binary, BinaryR),
    binary_decimal_(BinaryR, 1, 0, Decimal).

packet_bits(literal(_,_,PBits, _), PBits).
packet_bits(op(_,_,PBits, _), PBits).

packets_bits_length_([], Bits, Bits).
packets_bits_length_([P|Ps], Acc, Bits):-
    packet_bits(P, PBits),
    NewAcc is Acc + PBits,
    packets_bits_length_(Ps, NewAcc, Bits).

packets_bits_length(P, Bits):-
    packets_bits_length_(P, 0, Bits).

version_sum_(literal(Version,_,_, _), _, Version).
version_sum_(op(Version,_,_, Packets), _, Sum):-
    version_sum_(Packets, 0, PVSum),
    Sum is Version + PVSum.

version_sum_([], Sum, Sum).
version_sum_([P|Ps], Acc, Sum):-
    version_sum_(P, 0, PSum),
    NewAcc is Acc+PSum,
    version_sum_(Ps, NewAcc, Sum).

version_sum(Packets, Sum):-
    version_sum_(Packets, 0, Sum).

times(In, State, Out):-
    Out is State * In.

packet_eval(literal(_, _, _, N), N).

% sum
packet_eval(op(_, 0, _, P), Value):-
    maplist(packet_eval, P, Addends),
    sum_list(Addends, Value).

% product
packet_eval(op(_, 1, _, P), Value):-
    maplist(packet_eval, P, Multipliers),
    foldl(times, Multipliers, 1, Value).

% min
packet_eval(op(_, 2, _, P), Value):-
    maplist(packet_eval, P, Mins),
    min_list(Mins, Value).
% max
packet_eval(op(_, 3, _, P), Value):-
    maplist(packet_eval, P, Mins),
    max_list(Mins, Value).
% >
packet_eval(op(_, 5, _, [A,B]), Value):-
    packet_eval(A, AValue),
    packet_eval(B, BValue),
    ( AValue > BValue ->
      Value = 1
    ;
      Value = 0
    ).

% <
packet_eval(op(_, 6, _, [A,B]), Value):-
    packet_eval(A, AValue),
    packet_eval(B, BValue),
    ( AValue < BValue ->
      Value = 1
    ;
      Value = 0
    ).
% =
packet_eval(op(_, 7, _, [A,B]), Value):-
    packet_eval(A, AValue),
    packet_eval(B, BValue),
    ( AValue = BValue ->
      Value = 1
    ;
      Value = 0
    ).

day16_p1(File, Score):-
    phrase_from_file(hexline(HexNumber), File),
    hex_binary(HexNumber, Binary),
    phrase(stream(X), Binary, _),
    version_sum(X, Score).

day16_p2(File, Score):-
    phrase_from_file(hexline(HexNumber), File),
    hex_binary(HexNumber, Binary),
    phrase(stream([X]), Binary, _),
    packet_eval(X, Score).

day16_p1(Score):-
    day16_p1("data/day16_p1_data", Score).

day16_p1_test:-
    Tests = ["data/day16_p1_test1",
	     "data/day16_p1_test2",
	     "data/day16_p1_test3",
	     "data/day16_p1_test4",
	     "data/day16_p1_test5",
	     "data/day16_p1_test6",
	     "data/day16_p1_test7"],
    findall(T-Score,
            (member(T, Tests),
	     day16_p1(T, Score)),
            ScoreList),
    ScoreList = [_,_,_,_-16,_-12, _-23,_-31].

day16_p2_test:-
    Tests = ["data/day16_p2_test1",
	     "data/day16_p2_test2",
	     "data/day16_p2_test3",
	     "data/day16_p2_test4",
	     "data/day16_p2_test5",
	     "data/day16_p2_test6",
	     "data/day16_p2_test7",
	     "data/day16_p2_test8"],
    findall(T-Score,
            (member(T, Tests),
	     day16_p2(T, Score)),
            ScoreList),
    ScoreList = [_-3,_-54,_-9, _-1, _-0, _-0,_-1, _-7].

day16_p2(Score):-
    day16_p2("data/day16_p1_data", Score).

day16_p2_test(Score):-
    day16_p2("data/day16_p1_test", Score).

day16:-
    day16_p1_test,
    day16_p1(971),
    day16_p2_test,
    day16_p2(831996589851).
