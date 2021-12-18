:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(matrix).

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

dwriteln(A).
doct_write(Octopi).

oct_flash_octopi_(X+Y, Octopi, FlashedOctopi):-
    matrix(Octopi, X, Y, Value),
    ( Value < 10 ->
      NewValue is Value + 1,
      matrix_transform(Octopi, X, Y, NewValue, FlashedOctopi)
    ;
      FlashedOctopi = Octopi
    ).

oct_flash_octopi_done(Octopi, X+Y, DoneOctopi):-
    matrix_transform(Octopi, X, Y, 11, DoneOctopi).

oct_flash_octopi(flash(X,Y), Octopi, FlashDoneOctopi):-
    matrix_xy_adjacent_all(Octopi, X, Y, EnergyAddList),
    foldl(oct_flash_octopi_, EnergyAddList, Octopi, FlashedOctopi),
    dwriteln("before done"),
    oct_flash_octopi_done(FlashedOctopi, X+Y, FlashDoneOctopi),
    dwriteln("after done").

oct_flash_(Octopi, FlashingOctopi, PostflashOctopi):-
    foldl(oct_flash_octopi, FlashingOctopi, Octopi, PostflashOctopi).

oct_flash_flashing(Octopi, flash(X,Y)):-
    matrix_xy(Octopi, X, Y),
    matrix(Octopi, X, Y, 10).


oct_flash(Octopi, FlashedOctopi):-
    dwriteln(['startflash']),
    doct_write(Octopi),
    findall(F,
            oct_flash_flashing(Octopi, F),
            Flashers),
    length(Flashers, Count),
    dwriteln(Flashers),
    (Count > 0 ->
         [NewF|F] = Flashers,
         FlashingOctopi = [NewF],
         oct_flash_(Octopi, FlashingOctopi, StepFlashedOctopi),
         dwriteln(['flashed_', FlashingOctopi]),
         doct_write(StepFlashedOctopi),
         oct_flash(StepFlashedOctopi, FlashedOctopi)
    ;
         dwriteln("DONE"),
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

oct_energize(Octopi,NewOctopi):-
    matrix_element_add(1, Octopi, NewOctopi).


oct_step(Steps, Octopi, NextStepOctopi, FlashedCount):-
    dwriteln(["step", Steps]),
    oct_energize(Octopi, EnergizedOctopi),
    dwriteln(["energized", Steps]),
    doct_write(EnergizedOctopi),
    oct_flash(EnergizedOctopi, FlashedOctopi),
    dwriteln(["flashed", Steps]),
    doct_write(FlashedOctopi),
    dwriteln(["rollover", Steps]),
    oct_rollover(FlashedOctopi, NextStepOctopi, FlashedCount),
    dwriteln(["afterrollover", Steps]),
    doct_write(NextStepOctopi).

oct_steps_(_, 0,          Flashes, Flashes).
oct_steps_(Octopi, Steps, AccFlashes, Flashes):-
    NewSteps is Steps - 1,
    oct_step(Steps, Octopi, NextStepOctopi, FlashedCount),
    NewFlashes is AccFlashes + FlashedCount,
    oct_steps_(NextStepOctopi, NewSteps, NewFlashes, Flashes).

oct_steps_sync_(Octopi, AllCount, Step, Steps):-
    oct_step(Steps, Octopi, NextStepOctopi, FlashedCount),
    ( FlashedCount = AllCount ->
      Steps = Step
    ;
      NewSteps is Step + 1,
      oct_steps_sync_(NextStepOctopi, AllCount, NewSteps, Steps)
    ).

oct_steps(Octopi, Steps, Flashes):-
    oct_steps_(Octopi, Steps, 0, Flashes).

oct_steps_sync(Octopi, Steps):-
    matrix_limits(Octopi, XMAX, YMAX),
    AllCount is (XMAX+1)*(YMAX+1),
    oct_steps_sync_(Octopi, AllCount, 1, Steps).

day11_p1(File, Steps, Flashes):-
    phrase_from_file(matrixp(ListMatrix), File),
    lmatrix_matrix(ListMatrix, Matrix),
    doct_write(Matrix),
    oct_steps(Matrix, Steps, Flashes).

day11_p2(File, Steps):-
    phrase_from_file(matrixp(ListMatrix), File),
    lmatrix_matrix(ListMatrix, Matrix),
    doct_write(Matrix),
    oct_steps_sync(Matrix, Steps).

day11_p1(Score):-
    day11_p1("data/day11_p1_data", 100, Score).

day11_p1_test(Score):-
    day11_p1("data/day11_p1_test", 100, Score).

day11_p1_test2(Score):-
    day11_p1("data/day11_p1_test2", 2, Score).

day11_p2(Score):-
    day11_p2("data/day11_p1_data", Score).

day11_p2_test(Score):-
    day11_p2("data/day11_p1_test", Score).

day11:-
    day11_p1_test(1656),
    day11_p1(1717),
    day11_p2_test(195),
    day11_p2(476).
