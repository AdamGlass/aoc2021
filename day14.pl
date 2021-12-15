:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).

instructions(T, Rules) --> template(T), blanks, rules(Rules), blanks.

template(T) --> ident_string(T).

rules([C]) --> rule(C).
rules([C|Cs]) --> rule(C), rules(Cs).

rule(R) -->
    ident_char(C1), ident_char(C2), blank, arrow, blank, ident_char(C3), blanks,
    {R = rule(C1, C2, C3)}.

arrow --> `->`.

ident_string(S) --> ident(S).

ident([C]) --> ident_char(C).
ident([C|Cs]) --> ident_char(C), ident(Cs).
ident_char(C) --> alpha(A), {code_type(A, upper), char_code(C, A)}.
alpha(A) --> [A], {code_type(A, alpha)}.

polymer_reduce([A,B|Template], Rules, AccPolymer, Polymer):-
    member(rule(A,B,C), Rules),
    polymer_reduce([B|Template],Rules, [C,A|AccPolymer], Polymer).
polymer_reduce([A|Template], Rules, AccPolymer, Polymer):-
    polymer_reduce(Template, Rules, [A|AccPolymer], Polymer).
polymer_reduce([], _, AccPolymer, Polymer):-
    reverse(AccPolymer, Polymer).

polymer_(Polymer, _, 0, Polymer).
polymer_(Template, Rules, Steps, Polymer):-
    polymer_reduce(Template, Rules, [], AccPolymer),
    NewSteps is Steps - 1,!,
    polymer_(AccPolymer, Rules, NewSteps, Polymer).

polymer(Template, Rules, Steps, Polymer):-
   polymer_(Template, Rules, Steps, Polymer).

polymer_histogram(Polymer, Histogram):-
    setof(C-E,
	  (member(E, Polymer),
	   aggregate(count, member(E, Polymer), C)),
	  Histogram).

polymer_histogram_score(HistogramList, Score):-
    min_member(LeastCommon-_, HistogramList),
    max_member(MostCommon-_, HistogramList),
    Score is MostCommon-LeastCommon.

fpolymer_instance_add(Encoding, A+B, Addend, NewEncoding):-
    get_assoc(A+B, Encoding, Value),
    NewValue is Value + Addend,
    put_assoc(A+B, Encoding, NewValue, NewEncoding).
fpolymer_instance_add(Encoding, A+B, Addend, NewEncoding):-
    Addend > 0,
    put_assoc(A+B, Encoding, Addend, NewEncoding).

fpolymer_template_encoding_([], EncodingAcc, EncodingAcc).
fpolymer_template_encoding_([A], EncodingAcc, NewEncoding):-
    fpolymer_instance_add(EncodingAcc, A+eos, 1, NewEncoding).
fpolymer_template_encoding_([A,B|Template], EncodingAcc, Encoding):-
    fpolymer_instance_add(EncodingAcc, A+B, 1, NewEncoding),
    fpolymer_template_encoding_([B|Template], NewEncoding, Encoding).
fpolymer_template_encoding(Template, Encoding):-
    empty_assoc(InitEncoding),
    fpolymer_template_encoding_(Template, InitEncoding, Encoding).

fpolymer_action(substitute(A, B, C, Count), Encoding, NewEncoding):-
    NegCount is 0 - Count,
    fpolymer_instance_add(Encoding, A, NegCount, NewEncodingA),
    fpolymer_instance_add(NewEncodingA, B, Count, NewEncodingB),
    fpolymer_instance_add(NewEncodingB, C, Count, NewEncoding).

fpolymer_reduce(Encoding, Rules, NewEncoding):-
    findall(Action,
	    (gen_assoc(A+B, Encoding, Count),
	     member(rule(A,B,C), Rules),
	     Action = substitute(A+B,A+C,C+B, Count)),
	    Actions),
    foldl(fpolymer_action, Actions, Encoding, NewEncoding).

fpolymer_(Encoding, _, 0, Encoding).
fpolymer_(Encoding, Rules, Steps, Polymer):-
    fpolymer_reduce(Encoding, Rules, NewEncoding),
    NewSteps is Steps - 1,
    fpolymer_(NewEncoding, Rules, NewSteps, Polymer).

fpolymer(Template, Rules, Steps, Polymer):-
    fpolymer_template_encoding(Template, Encoding),
    fpolymer_(Encoding, Rules, Steps, Polymer).

fpolymer_length(Polymer, Length):-
    findall(Count,
	    gen_assoc(_, Polymer, Count),
	    CountList),
    sum_list(CountList, Length).

fpolymer_histogram(Polymer, Histogram):-
    findall(C,
	  (gen_assoc(C+_, Polymer, _)),
	   Encodings),
    sort(Encodings, CharacterSet),
    findall(Count-Encoding,
	    (member(Encoding, CharacterSet),
	     findall(EncodingCount,
		     gen_assoc(Encoding+_, Polymer, EncodingCount),
		     CountList),
	     sum_list(CountList, Count)),
	    Histogram).

fpolymer_score(Polymer, Score, Length):-
    fpolymer_length(Polymer, Length),
    fpolymer_histogram(Polymer, Histogram),
    polymer_histogram_score(Histogram, Score).

polymer_score(Polymer, Score, Length):-
    length(Polymer, Length),
    polymer_histogram(Polymer, Histogram),
    polymer_histogram_score(Histogram, Score).

day14_p1(File, Steps, Score):-
    phrase_from_file(instructions(Template, Rules), File),
    polymer(Template, Rules, Steps, Polymer),
    polymer_score(Polymer, Score, Length).

day14_p2(File, Steps, Score):-
    phrase_from_file(instructions(Template, Rules), File),
    fpolymer(Template, Rules, Steps, Polymer),
    fpolymer_score(Polymer, Score, Length).


day14_p1(Score):-
    day14_p1("data/day14_p1_data", 10, Score).

day14_p1_test(Score):-
    day14_p1("data/day14_p1_test", 10, Score).

day14_p2(Score):-
    day14_p2("data/day14_p1_data", 40, Score).

day14_p2_test(Score):-
    day14_p2("data/day14_p1_test", 10, Score).

day14:-
    day14_p1_test(1588),
    day14_p1(2447),
    day14_p2_test(1588),
    day14_p2(3018019237563).
