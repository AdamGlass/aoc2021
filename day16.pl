:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

packets([P]) --> packet(P).
packets([P|Ps]) --> packet(P), packets(Ps).

packet(P) --> literal_value(P).
packet(O) --> operator(O).

literal_value(N) --> packet_header(_, 4), pnumber(N).

operator(O) --> operator_header, short_id_type, number15(N), packets(P),
		{O = op(N, P)}.
operator(O) --> operator_header, long_id_type, number11(P), packets(NP), 
		{O = op(N, NP)}.

operator_header --> packet_header(_, X), {X \= 4}.

packet_header(V, I) --> version(V), id(I).

version(V) --> bdigit(A), bdigit(B), bdigit(C), {binary_val([A,B,C], V)}.
id(V) --> bdigit(A), bdigit(B), bdigit(C),{binary_val([A,B,C], V)}.

pnumber(N) --> number_groups(G), end_group(G), zeros, {ngroup(G,N)}.

number_group([G]) --> number_groups(G).
number_group([G|Gs]) --> number_group(G), number_groups(Gs).

number_group(G)--> bdigit(1), nibble(G).
end_group(G)--> bdigit(0), nibble(G).

nibble(N) --> bdigit(A), bdigit(B), bdigit(C), bdigit(D), {hex_value([A, B, C, D], N)}.

bdigit(0) --> `0`.
bdigit(1) --> `1`.

hexline(N) --> hex(N), blanks.

hex([H]) --> hexchar(H).
hex([H|Hs]) --> hexchar(H), hex(Hs).

hexchar(V) --> [C], {char_type(C, digit), char_code('0', Zero), V is C - Zero}.
hexchar(V) --> [C], {char_type(C, upper), char_code('A', Zero), V is 10 + C - Zero}.

hex_to_decimal_([], _, Value, Value).
hex_to_decimal_([H|Hs], Power , Acc, Value):-
    NewAcc is H*Power + Acc,
    NewPower is Power * 16,
    hex_to_decimal_(Hs, NewPower, NewAcc, Value).

hex_to_decimal(HexNumberList, Value):-
    reverse(HexNumberList, R), hex_to_decimal_(R, 1, 0, Value).

dec_binary_(0, Value, Value).
dec_binary_(Decimal, Acc, Value):-
    NewValue is getbit(Decimal, 0),
    NewAcc = [NewValue|Acc],
    NewDecimal is Decimal >> 1,
    dec_binary_(NewDecimal, NewAcc, Value).

dec_binary(Decimal, Value):-
    dec_binary_(Decimal, [], Value).

day16_p1(File, Score):-
    phrase_from_file(hexline(HexNumber), File),
    writeln(HexNumber),
    hex_to_decimal(HexNumber, DecimalNumber),
    writeln(DecimalNumber).

day16_p1(Score):-
    day16_p1("data/day16_p1_data", Score).

day16_p1_test(Score):-
    day16_p1("data/day16_p1_test", Score).

day16_p2(Score):-
    day16_p2("data/day16_p1_data", Score).

day16_p2_test(Score):-
    day16_p2("data/day16_p1_test", Score).

day16:-
    day16_p1_test(_),
    day16_p1(_),
    day16_p2_test(_),
    day16_p2(_).
