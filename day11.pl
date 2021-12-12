:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).

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

matrix(Matrix, I, J, Value) :-
    nth0(I, Matrix, Row),
    nth0(J, Row, Value).

matrix_ones(M):-
    L = 10,
    length(M, L),
    length(Columns, L),
    maplist(=(1), Columns),
    maplist(=(Columns), M).

matrix_zeros(M):-
    L = 10,
    length(M, L),
    length(Columns, L),
    maplist(=(0), Columns),
    maplist(=(Columns), M).


matrix_add(M1, M2, M3):-
    maplist(maplist(plus), M1, M2, M3).

matrix_inf_xy(M, X, Y, NV):-
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

matrix_inf_xy(M, X, Y, 1):-
    matrix(M, X, Y, 10).

matrix_inf_xy(M, X, Y, 0):-
    matrix(M, X, Y, 11).

matrix_same_size(M, S):-
    same_length(M, S),
    transpose(M, TM),
    transpose(S, TS),
    same_length(TM, TS).


flashed_done(10, 1).
flashed_done(X, X).

octo_flash_(M, Y, AccMatrix, IM):-
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
    octo_flash_(M, NewY, [NewRow|AccMatrix], IM).

octo_flash_(M, _, AccM, IM):-
    same_length(M, AccM),
    reverse(AccM, IM).

octo_flash(M, IM):-
    writeln("flash"),
    octo_write(M),
    octo_flash_(M, 0, [], AM),
    writeln("adder"),
    octo_write(AM),
    matrix_zeros(Zeros),
    (AM \= Zeros ->
         matrix_add(M, AM, NewM),
         octo_flash(NewM, AM)
    ;
         M = IM
    ).

octo_deflash_row([], []).
octo_deflash_row([R|Rs], [N|Ns]):-
    R >= 10,
    N = 0,
    octo_deflash_row(Rs, Ns).

octo_deflash_row([R|Rs], [N|Ns]):-
    R < 10,
    N = R,
    octo_deflash_row(Rs, Ns).

octo_deflash(FlashedOctopi, PostFlash):-
    maplist(octo_deflash_row, FlashedOctopi, PostFlash).

octo_flash_accounting(FlashedOctopi, Count, PostFlash):-
    length(FlashedOctopi, RowCount),
    LX is RowCount - 1,
    findall(1,
            (between(0, LX, X),
             between(0, LX, Y),
             matrix(FlashedOctopi, X, Y, V),
             V > 10),
            Flashed),
    length(Flashed, Count),
    octo_deflash(FlashedOctopi, PostFlash).

octo_write(Octopi):-
    forall(member(X, Octopi),
           writeln(X)).

oct_steps_(_, 0, Flashes, Flashes).
oct_steps_(Octopi, Steps, AccFlashes, Flashes):-
    writeln(["step", Steps]),
    matrix_ones(Ones),
    matrix_add(Octopi, Ones, EOctopi),
    octo_flash(EOctopi, FlashedOctopi),
    octo_flash_accounting(FlashedOctopi, FlashCount, PostFlash),
    NewFlashes is FlashCount + AccFlashes,
    NewSteps is Steps - 1,
    octo_write(PostFlash),
    oct_steps_(PostFlash, NewSteps, NewFlashes, Flashes).

oct_steps(Octopi, Steps, Flashes):-
    oct_steps_(Octopi, Steps, 0, Flashes).

day11_p1(File, Steps, Flashes):-
    phrase_from_file(matrixp(M), File),
    octo_write(M),
    oct_steps(M, Steps, Flashes).

day11_p1(Score):-
    day11_p1("data/day11_p1_data", 100, Score).

day11_p1_test(Score):-
    day11_p1("data/day11_p1_test", 2, Score).

day11_p2(Score):-
    day11_p2("data/day11_p1_data", Score).

day11_p2_test(Score):-
    day11_p2("data/day11_p1_test", Score).

day11:-
    day11_p1_test(_),
    day11_p1(_),
    day11_p2_test(_),
    day11_p2(_).

