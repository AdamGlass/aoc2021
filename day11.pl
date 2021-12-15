:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).

matrixp(M) --> mapx(M).

mapx([M]) --> mapline(M).
mapx([M|Ms]) --> mapline(M), mapx(Ms).

mapline(M) --> mdigits(M), blank.

mdigits([D]) --> mdigit(D).
mdigits([D|Ds]) --> mdigit(D), mdigits(Ds).

mdigit(V) --> [C], {char_type(C, digit), char_code('0', Zero), V is C - Zero}.

% increase energy
% energy level > 9 flashes
% energy around increases by 1
% energy level flashes

% easier to parse but clumsy to modify
lmatrix(Matrix, X, Y, Value) :-
    nth0(Y, Matrix, Row),
    nth0(X, Row, Value).

lmatrix_matrix(ListMatrix, AssocMatrix):-
    length(ListMatrix, RowCount),
    [Row|_] = ListMatrix,
    length(Row, ColumnCount),
    LY is RowCount - 1,
    LX is ColumnCount - 1,
    findall(X+Y-Value,
	    (between(0, LX, X),
	     between(0, LY, Y),
	     lmatrix(ListMatrix, X, Y, Value)
	    ),
	    KeyValueList),
    ord_list_to_assoc(KeyValueList, AssocMatrix).

matrix(Matrix, X, Y, Value) :-
    get_assoc(X+Y, Matrix, Value).

max_xykey([], AccX, AccY, AccX, AccY).
max_xykey([X+Y|Keys], AccX, AccY, MaxX, MaxY):-
    XMax is max(X, AccX),
    YMax is max(Y, AccY),
    max_xykey(Keys, XMax, YMax, MaxX, MaxY).

matrix_limits(Matrix, XMAX, YMAX):-
    assoc_to_keys(Matrix,XYList),
    max_xykey(XYList, 0, 0, XMAX, YMAX).

matrix_element_transform(Op, Matrix, NewMatrix):-
    assoc_to_list(Matrix, MatrixList),
    maplist(Op, MatrixList, OpList),
    ord_list_to_assoc(OpList, NewMatrix).

matrix_element_add_(Addend, X+Y-Value, X+Y-NewValue):-
    NewValue is Value + Addend.

matrix_element_add(Value, Matrix, NewMatrix):-
    matrix_element_transform(matrix_element_add_(Value), Matrix, NewMatrix).

matrix_transform(Matrix, X, Y, Value, NewMatrix):-
    put_assoc(X+Y, Matrix, Value, NewMatrix).

matrix_xy_transform_(Op, Matrix, X, Y, NewMatrix):-
    matrix(Matrix, X, Y, Value),
    call(Op, Value, NewValue),
    matrix_transform(Matrix, X, Y, NewValue, NewMatrix).

matrix_xy_add(Matrix, X, Y, Addend, NewMatrix):-
    matrix_xy_transform_(plus(Addend), Matrix, X, Y, NewMatrix).

matrix_xy_adjacent_all(DX, DY):-
    member(dir(DX, DY), [dir(-1, -1), dir(0, -1), dir(1,-1),
                         dir(-1, 0), dir(1, 0),
                         dir(-1, 1), dir(0, 1), dir(1,1)]).

matrix_xy_adjacent_cardinal(DX, DY):-
    member(dir(DX, DY), [dir(0, -1), dir(0, 1),
                         dir(-1, 0), dir(1, 0)]).

matrix_xy_adjacent_all(Matrix, X, Y, AdjacentList):-
    findall(NX+NY,
	    (matrix_xy_adjacent_all(DX, DY),
             NX is X + DX,
             NY is Y + DY,
             matrix(Matrix, NX, NY, _)),
	    AdjacentList).

matrix_xy_adjacent_cardinal(Matrix, X, Y, AdjacentList):-
    findall(NX+NY,
	    (matrix_xy_adjacent_cardinal(DX, DY),
             NX is X + DX,
             NY is Y + DY,
             matrix(Matrix, NX, NY, _)),
	    AdjacentList).

matrix_xy(Matrix, X, Y):-
    matrix_limits(Matrix, XMAX, YMAX),
    between(0, XMAX, X),
    between(0, YMAX, Y).

matrix_write(Matrix, ElementFormatter):-
    matrix_limits(OctoMatrix, XMAX, YMAX),
    forall(between(0, YMAX, Y),
	   (forall(between(0, XMAX, X),
		   (matrix(OctoMatrix, X, Y, Value),
		    call(ElementFormatter,Value,Display),
		    write(Display))),
	    writeln(""))).

matrix_foldl(Op, List, InitialMatrix, OutMatrix):-
    foldl(Op, List, InitialMatrix, OutMatrix).

oct_flash_octopi_(X+Y, Octopi, FlashedOctopi):-
    matrix(Octopi, X, Y, Value),
    NewValue is Value + 1,
    ValueCeiling is min(NewValue, 10),
    matrix_transform(Octopi, X, Y, ValueCeiling, FlashedOctopi).

oct_flash_octopi_done(Octopi, X+Y, DoneOctopi):-
    matrix_transform(Octopi, X, Y, 11, DoneOctopi).

oct_flash_dir(DX, DY):-
    member(dir(DX, DY), [dir(-1, -1), dir(0, -1), dir(1,-1),
                         dir(-1, 0), dir(1, 0),
                         dir(-1, 1), dir(0, 1), dir(1,1)]).

oct_flash_octopi(flash(X,Y), Octopi, FlashDoneOctopi):-
    findall(NX+NY,
	    (oct_flash_dir(DX, DY),
             NX is X + DX,
             NY is Y + DY,
             matrix(Octopi, NX, NY, _)),
	    EnergyAddList),
    foldl(oct_flash_octopi_, EnergyAddList, Octopi, FlashedOctopi),
    writeln("before done"),
    oct_flash_octopi_done(FlashedOctopi, X+Y, FlashDoneOctopi),
    writeln("after done").

oct_flash_(Octopi, FlashingOctopi, PostflashOctopi):-
    foldl(oct_flash_octopi, FlashingOctopi, Octopi, PostflashOctopi).

oct_flash_flashing(Octopi, flash(X,Y)):-
    matrix_limits(Octopi, XMAX, YMAX),
    between(0, XMAX, X),
    between(0, YMAX, Y),
    matrix(Octopi, X, Y, 10).

oct_flash(Octopi, FlashedOctopi):-
    writeln(['startflash']),
    oct_write(Octopi),
    findall(F,
            oct_flash_flashing(Octopi, F),
            Flashers),
    length(Flashers, Count),
    writeln(Flashers),
    (Count > 0 ->
         [NewF|F] = Flashers,
         FlashingOctopi = [NewF],
         oct_flash_(Octopi, FlashingOctopi, StepFlashedOctopi),
         writeln(['flashed', FlashingOctopi]),
         oct_write(StepFlashedOctopi),
         oct_flash(StepFlashedOctopi, FlashedOctopi)
    ;
         writeln("DONE"),
         FlashedOctopi = Octopi
    ).

oct_rollover_octopi(rollover(X,Y), Octopi, AfterOctopi):-
    matrix_transform(Octopi, X, Y, 0, AfterOctopi).

oct_rollover_(Octopi, Rollovers, PostRollover):-
    foldl(oct_rollover_octopi, Rollovers, Octopi, PostRollover).

oct_rollover(Octopi, RolloverOctopi, FlashCount):-
    matrix_limits(Octopi, XMAX, YMAX),
    findall(rollover(X,Y),
            (between(0, XMAX, X),
             between(0, YMAX, Y),
             matrix(Octopi, X, Y, Value),
	     Value > 10
	    ),
            Rollovers),
    length(Rollovers, FlashCount),
    oct_rollover_(Octopi, Rollovers, RolloverOctopi).

oct_value_char(N, V):-
    N < 16,
    nth0(N, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'A', 'B', 'C', 'D','E', 'F'], V).
oct_value_char(N, 'X'):-
    N > 15.

oct_write(OctoMatrix):-
    matrix_limits(OctoMatrix, XMAX, YMAX),
    forall(between(0, YMAX, Y),
	   (forall(between(0, XMAX, X),
		   (matrix(OctoMatrix, X, Y, Value),
		    oct_value_char(Value,Display),
		    write(Display))),
	    writeln(""))).

oct_energize(Octopi,NewOctopi):-
    matrix_element_add(1, Octopi, NewOctopi).

oct_steps_(_, 0,          Flashes, Flashes).
oct_steps_(Octopi, Steps, AccFlashes, Flashes):-
    writeln(["step", Steps]),
    oct_energize(Octopi, EnergizedOctopi),
    writeln(["energized", Steps]),
    oct_write(EnergizedOctopi),
    oct_flash(EnergizedOctopi, FlashedOctopi),
    writeln(["flashed", Steps]),
    oct_write(FlashedOctopi),
    writeln(["rollover", Steps]),
    oct_rollover(FlashedOctopi, NextStepOctopi, FlashedCount),
    writeln(["afterrollover", Steps]),
    oct_write(NextStepOctopi),
    NewSteps is Steps - 1,
    NewFlashes is AccFlashes + FlashedCount,
    oct_steps_(NextStepOctopi, NewSteps, NewFlashes, Flashes).

oct_steps(Octopi, Steps, Flashes):-
    oct_steps_(Octopi, Steps, 0, Flashes).

day11_p1(File, Steps, Flashes):-
    phrase_from_file(matrixp(ListMatrix), File),
    lmatrix_matrix(ListMatrix, Matrix),
    oct_write(Matrix),
    oct_steps(Matrix, Steps, Flashes).

day11_p1(Score):-
    day11_p1("data/day11_p1_data", 100, Score).

day11_p1_test(Score):-
    day11_p1("data/day11_p1_test", 2, Score).

day11_p1_test2(Score):-
    day11_p1("data/day11_p1_test2", 1, Score).

day11_p2(Score):-
    day11_p2("data/day11_p1_data", Score).

day11_p2_test(Score):-
    day11_p2("data/day11_p1_test", Score).

day11:-
    day11_p1_test(_),
    day11_p1(_),
    day11_p2_test(_),
    day11_p2(_).

