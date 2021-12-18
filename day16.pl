:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

tp --> bit1, bit1, bit1.

bit1 --> [1].

stream([P]) --> packet(P), zeros(_).

packets([P]) --> packet(P).
packets([P|Ps]) --> packet(P), packets(Ps).

zeros([]).
zeros([Z]) --> bdigit(Z).
zeros([Z|Zs]) --> bdigit(Z), zeros(Zs).

packet(P) --> literal_value(P).
packet(O) --> operator(O).

literal_value(L) --> packet_header(V, 4), pnumber(N, Count),
		     {Bits is 6 + Count*5,
		      L = literal(V, 4, Bits, N)}.

operator(O) --> operator_header(V,T), short_id_type, 
		number11(N), packets_count(N, P, PBits),
		{Bits is 7 + 11 + PBits,
		 O = op(V,T,Bits, P)
		}.

operator(O) --> operator_header(V,T), long_id_type,
		number15(PBits), packets_bits(PBits, P),
		{Bits is 7 + 15 + PBits,
		 O = op(V,T,Bits, P)
		}.

operator_header(V,T) --> packet_header(V, T), {T \= 4}.

short_id_type --> bdigit(1).
long_id_type --> bdigit(0).

packet_header(V, I) --> version(V), id(I).

packets_count(Count, P, PBits) --> 
    packets(P),
    {length(P, Count), packets_bits_length(P, PBits)}.
packets_bits(PBits, P) --> packets(P),
   { packets_bits_length(P, PBits)}.

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

day16_p1(File, Score):-
    phrase_from_file(hexline(HexNumber), File),
    hex_binary(HexNumber, Binary),
    writeln([HexNumber, Binary]),
    phrase(stream(X), Binary, Rest),
    version_sum(X, Score),
    writeln([result, X, Score, File, Rest]).

day16_parse_test_literal:-
    hex_binary([13,2,15,14,2,8], LitBinary),
    phrase(stream(X), LitBinary, Rest),
    writeln([LitBinary, X, Rest]).

day16_parse_test_op2:-
    hex_binary([14,14,0,0,13,4,0,12,8,2,3,0,6,0], OpBinary2),
    phrase(stream(Y), OpBinary2, Rest2),
    writeln([OpBinary2, Y, Rest2]).

day16_parse_test_op1:-
    hex_binary([3,8,0,0,6,15,4,5,2,9,1,2,0,0], OpBinary),
    writeln(OpBinary),
    phrase(stream(X), OpBinary, Rest),
    writeln([OpBinary, X, Rest]).

day16_p1(Score):-
    day16_p1("data/day16_p1_data", Score).

day16_p1_test(Score):-
    Tests = ["data/day16_p1_test1",
	     "data/day16_p1_test2",
	     "data/day16_p1_test3",
	     "data/day16_p1_test4",
	     "data/day16_p1_test5",
	     "data/day16_p1_test6",
	     "data/day16_p1_test7"],
    forall(member(T, Tests),
	   day16_p1(T, _)).

day16_p2(Score):-
    day16_p2("data/day16_p1_data", Score).

day16_p2_test(Score):-
    day16_p2("data/day16_p1_test", Score).

day16:-
    day16_p1_test(_),
    day16_p1(_),
    day16_p2_test(_),
    day16_p2(_).
