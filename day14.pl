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
    writeln(Steps),
    NewSteps is Steps - 1,!,
    polymer_(AccPolymer, Rules, NewSteps, Polymer).

polymer(Template, Rules, Steps, Polymer):-
   polymer_(Template, Rules, Steps, Polymer).

polymer_score(Polymer, Score):-
    setof(C-E,
	  (member(E, Polymer),
	   aggregate(count, member(E, Polymer), C)),
	  HistogramList),
    min_member(LeastCommon-_, HistogramList),
    max_member(MostCommon-_, HistogramList),
    Score is MostCommon-LeastCommon.

day14_p1(File, Steps, Score):-
    phrase_from_file(instructions(Template, Rules), File),
    polymer(Template, Rules, Steps, Polymer),
    polymer_score(Polymer, Score).

day14_p1(Score):-
    day14_p1("data/day14_p1_data", 10, Score).

day14_p1_test(Score):-
    day14_p1("data/day14_p1_test", 10, Score).

day14_p2(Score):-
    day14_p1("data/day14_p1_data", 40, Score).

day14_p2_test:-
    day14_p2("data/day14_p1_test").

day14:-
    day14_p1_test(17),
    day14_p1(720),
    day14_p2_test,
    day14_p2.
