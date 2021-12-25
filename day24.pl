:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(matrix).

program(P) --> ops(P), blanks.

ops([O]) --> opx(O).
ops([O|Os]) --> opx(O), ops(Os).

opx(O) --> operation(O), blanks.

operation(O) --> `inp `, variable(A), { O = operator(inp, A, w)}.
operation(O) --> `add `, variable(A), blank, oparg(B), { O = operator(add, A, B)}.
operation(O) --> `mul `, variable(A), blank, oparg(B), { O = operator(mul, A, B)}.
operation(O) --> `div `, variable(A), blank, oparg(B), { O = operator(div, A, B)}.
operation(O) --> `mod `, variable(A), blank, oparg(B), { O = operator(mod, A, B)}.
operation(O) --> `eql `, variable(A), blank, oparg(B), { O = operator(eql, A, B)}.

oparg(B) --> variable(B).
oparg(B) --> onumber(B).

variable(w) --> `w`.
variable(x) --> `x`.
variable(y) --> `y`.
variable(z) --> `z`.
onumber(B) --> integer(B).

constrain_(R, [], [], R):-
    writeln("exit").

constrain_([CW, CX, CY, CZ], [P|Ps], Input, Results):-
    writeln(P),
    !,
    P = operator(Opname, Target, Argument),
    ( Target = w ->
      Value = CW,
      NewRegs = [TargetOutput, CX, CY, CZ]
    ; Target = x ->
      Value = CX,
      NewRegs = [CW, TargetOutput, CY, CZ]
    ; Target = y ->
      Value = CY,
      NewRegs = [CW, CX, TargetOutput, CZ]
    ;
      Value = CZ,
      NewRegs = [CW, CX, CY, TargetOutput]
    ),
    ( nth0(Index, [w, x, y, z], Argument) ->
      nth0(Index, [CW, CX, CY, CZ], ArgumentValue)
    ;
      ArgumentValue = Argument
    ),
    ( Opname = inp ->
      [I|Is] = Input,
      writeln([digit, I]),
      TargetOutput = I,
      constrain_(NewRegs, Ps, Is, Results)
    ; Opname = mul ->
      TargetOutput #= Value * ArgumentValue,
      constrain_(NewRegs, Ps, Input, Results)
    ; Opname = div ->
      ArgumentValue #\= 0,
      TargetOutput #= Value // ArgumentValue,
      constrain_(NewRegs, Ps, Input, Results)
    ; Opname = mod ->
      Value #>= 0,
      ArgumentValue #> 0,
      TargetOutput #= mod(Value, ArgumentValue),
      constrain_(NewRegs, Ps, Input, Results)
    ; Opname = add ->
      TargetOutput #= Value + ArgumentValue,
      constrain_(NewRegs, Ps, Input, Results)
    ; Opname = eql ->
      (Value #= ArgumentValue ->
	   TargetOutput #= 1
      ;
	   TargetOutput #= 0
      ),
      constrain_(NewRegs, Ps, Input, Results)
    ;
      writeln("badop"),
      fail
    ).

constrain(RegisterStart, Program, Input, Results):-
    RegisterStart ins inf..sup,
    Input ins inf..sup,
    Results ins inf..sup,

    constrain_(RegisterStart, Program, Input, Results).


day24_p1(File, Input, Results):-
    phrase_from_file(program(Program), File),
    length(Results, 4),
    Results ins inf..sup,
    constrain([0,0,0,0], Program, Input, Results).

day24_p1(Score):-
    length(Input, 14),
    Input ins 1..9,
    Score = [_, _, _, 0],
    findall(Input,
	    (day24_p1("data/day24_p1_data", Input, Score),
	     label(Input)),
	    Inputs),
    [Smallest|_] = Inputs,
    reverse(Inputs, RInputs),
    [Biggest|_] = RInputs,
    writeln([Smallest, Biggest]).



day24_p1_test1:-
    length(Input, 1),
    Input ins inf..sup,
    Input = [5],
    day24_p1("data/day24_p1_test1", Input, [0, -5, 0, 0]).

day24_p1_test2:-
    Input = [1, 3],
    day24_p1("data/day24_p1_test2", Input, [0, 3, 0, 1]).

day24_p1_test3:-
    Input = [11],
    day24_p1("data/day24_p1_test3", Input, [1,0,1,1]).

day24_p2(Score):-
    day24_p2("data/day24_p1_data", Score).

day24_p2_test(Score):-
    day24_p2("data/day24_p1_test", Score).

day24:-
    day24_p1_test(739785),
    day24_p1(920580),
    day24_p2_test(_),
    day24_p2(_).
