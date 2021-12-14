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

dmatrix_ones(M):-
    L = 10,
    length(M, L),
    length(Columns, L),
    maplist(=(1), Columns),
    maplist(=(Columns), M).

dmatrix_zeros(M):-
    L = 10,
    length(M, L),
    length(Columns, L),
    maplist(=(0), Columns),
    maplist(=(Columns), M).

dmatrix_add(M1, M2, M3):-
    maplist(maplist(plus), M1, M2, M3).

dmatrix_inf_xy(M, X, Y, NV):-
    matrix(M, X, Y, Value),
    Value < 10,
    findall(1, (
                between(-1, 1, DX),
                between(-1, 1, DY),
                DX \= 0,
                DY \= 0,
                NX is X + DX,
                NY is Y + DY,
                matrix(M, NX, NY, V),
                V = 10
            ),
            NeighborFlash),
    length(NeighborFlash, Flashes),
    (Flashes > 0 ->
         NV = Flashes,
         write(["UP", X,Y, NV])
    ;
         NV = 0,
         write(["UP0", X,Y, NV])
    ).

dmatrix_inf_xy(M, X, Y, 1):-
    matrix(M, X, Y, 10).

dmatrix_inf_xy(M, X, Y, 0):-
    matrix(M, X, Y, 11).

dmatrix_same_size(M, S):-
    same_length(M, S),
    transpose(M, TM),
    transpose(S, TS),
    same_length(TM, TS).


dflashed_done(10, 1).
dflashed_done(X, X).

doct_flash_(M, Y, AccMatrix, IM):-
    length(M, RowCount),
    RowCount > Y,
    length(Row, RowCount),
    LX is RowCount - 1,
    findall(V,
            (between(0, LX, X),
             matrix_inf_xy(M, X, Y, V)),
            Row),
    reverse(Row, NewRow),
    NewY is Y + 1,
    oct_flash_(M, NewY, [NewRow|AccMatrix], IM).

doct_flash_(M, _, AccM, IM):-
    same_length(M, AccM),
    reverse(AccM, IM).

doct_flash(M, IM):-
    writeln("flash"),
    oct_write(M),
    oct_flash_(M, 0, [], AM),
    writeln("adder"),
    oct_write(AM),
    matrix_zeros(Zeros),
    (AM \= Zeros ->
         matrix_add(M, AM, NewM),
         oct_flash(NewM, AM)
    ;
         M = IM
    ).

oct_deflash_row([], []).
oct_deflash_row([R|Rs], [N|Ns]):-
    R >= 10,
    N = 0,
    oct_deflash_row(Rs, Ns).

oct_deflash_row([R|Rs], [N|Ns]):-
    R < 10,
    N = R,
    oct_deflash_row(Rs, Ns).

oct_deflash(FlashedOctopi, PostFlash):-
    maplist(oct_deflash_row, FlashedOctopi, PostFlash).

oct_flash_accounting(FlashedOctopi, Count, PostFlash):-
    length(FlashedOctopi, RowCount),
    LX is RowCount - 1,
    findall(1,
            (between(0, LX, X),
             between(0, LX, Y),
             matrix(FlashedOctopi, X, Y, V),
             V > 10),
            Flashed),
    length(Flashed, Count),
    oct_deflash(FlashedOctopi, PostFlash).

oct_flash_octopi_(X+Y, Octopi, FlashedOctopi):-
    matrix_xy_add(Octopi, X, Y, 1, FlashedOctopi).

oct_flash_octopi_(X+Y, Octopi, FlashedOctopi):-
    matrix(Octopi, X, Y, Value),

    matrix_transform(Octopi, X, Y, 
    matrix_xy_add(Octopi, X, Y, 1, FlashedOctopi).

oct_flash_octopi(flash(X,Y), Octopi, FlashedOctopi):-
    findall(NX+NY,
	    (between(-1, 1, DX),
             between(-1, 1, DY),
             NX is X + DX,
             NY is Y + DY,
             matrix(Octopi, NX, NY, _)
            ),
	    EnergyAddList),
    foldl(oct_flash_octopi_, EnergyAddList, Octopi, FlashedOctopi).

oct_flash_(Octopi, FlashingOctopi, PostflashOctopi):-
    foldl(oct_flash_octopi, FlashingOctopi, Octopi, PostflashOctopi).

oct_flash(Octopi, FlashedOctopi):-
    matrix_limits(Octopi, XMAX, YMAX),
    writeln("before find"),
    findall(flash(X,Y),
	    (between(0, XMAX, X),
	     between(0, YMAX, Y),
	     matrix(Octopi, X, Y, 10)),
	    FlashingOctopi),
    writeln("after find"),
    writeln(['flashing', FlashingOctopi]),
    FlashingOctopi \= [],
    oct_flash_(Octopi, FlashingOctopi, StepFlashedOctopi),
    oct_flash(StepFlashedOctopi, FlashedOctopi).
oct_flash(Octopi, Octopi).

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

oct_not_yet:-
    halt,
    oct_flash(EOctopi, FlashedOctopi),
    oct_flash_accounting(FlashedOctopi, FlashCount, PostFlash),
    NewFlashes is FlashCount + AccFlashes,
    NewSteps is Steps - 1,
    oct_write(PostFlash),
    oct_steps_(PostFlash, NewSteps, NewFlashes, Flashes).

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
    day11_p1("data/day11_p1_test2", 2, Score).

day11_p2(Score):-
    day11_p2("data/day11_p1_data", Score).

day11_p2_test(Score):-
    day11_p2("data/day11_p1_test", Score).

day11:-
    day11_p1_test(_),
    day11_p1(_),
    day11_p2_test(_),
    day11_p2(_).

