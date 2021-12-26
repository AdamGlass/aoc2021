:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(matrix).

data(F) --> floor(F).

floor([A]) --> floor_line(A).
floor([A|As]) --> floor_line(A), floor(As).

floor_line(L) --> bitstream(L), blank.

bitstream([A]) --> bdigit(A).
bitstream([A|As]) --> bdigit(A), bitstream(As).

bdigit(V) --> [C], {char_code('v', C), V = 1}.
bdigit(V) --> [C], {char_code('>', C), V = 2}.
bdigit(V) --> [C], {char_code('.', C), V = 0}.


floor_formatter(0, '.').
floor_formatter(1, 'v').
floor_formatter(2, '>').

limits(138, 136).

floor_xy(Matrix, X+Y, Value):-
    limits(XMax, YMax),
    ( X > XMax ->
      get_assoc(0+Y, Matrix, Result)
    ; Y > YMax ->
      get_assoc(X+0, Matrix, Result)
    ;
      get_assoc(X+Y, Matrix, Result)
    ),
    Value = Result.

floor_xy_value(Matrix, X+Y, Value, NewMatrix):-
    limits(XMax, YMax),
    ( X > XMax ->
      put_assoc(0+Y, Matrix, Value, NewMatrix)
    ; Y > YMax ->
      put_assoc(X+0, Matrix, Value, NewMatrix)
    ;
      put_assoc(X+Y, Matrix, Value, NewMatrix)
    ).

floor_write(Matrix):-
    matrix_write(Matrix, floor_formatter).

move_right(X+Y, InMatrix, OutMatrix):-
    floor_xy_value(InMatrix, X+Y, 0, CleanedMatrix),
    NewX is X + 1,
    floor_xy_value(CleanedMatrix, NewX+Y, 2, OutMatrix).

move_down(X+Y, InMatrix, OutMatrix):-
    floor_xy_value(InMatrix, X+Y, 0, CleanedMatrix),
    NewY is Y + 1,
    floor_xy_value(CleanedMatrix, X+NewY, 1, OutMatrix).

sea_march(Matrix, Step, Count):-
    writeln([step, Step]),
%    floor_write(Matrix),
    NewStep is Step + 1,
    findall(X+Y,
	    (matrix_xy(Matrix, X, Y),
	     floor_xy(Matrix, X+Y, 2),
	     NewX is X+1,
	     floor_xy(Matrix, NewX+Y, 0)),
	    RightList),
%    writeln([right, RightList]),
    foldl(move_right, RightList, Matrix, PostRight),
%    writeln([posright]),
%    floor_write(PostRight),
    findall(X+Y,
	    (matrix_xy(PostRight, X, Y),
	     floor_xy(PostRight, X+Y, 1),
	     NewY is Y+1,
	     floor_xy(PostRight, X+NewY, 0)),
	    DownerList),
%    writeln([down, DownerList]),
    foldl(move_down, DownerList, PostRight, PostDown),
%    floor_write(PostDown),
    ( RightList = [], DownerList = [] ->
      Step = Count
    ;
      sea_march(PostDown, NewStep, Count)
    ).
    
sea_march(Matrix, Count):-
    sea_march(Matrix, 1, Count).

day25_p1(File, Score):-
    phrase_from_file(data(LFloor), File),
    lmatrix_matrix(LFloor, Matrix),
    writeln([XMax, YMax]),
    sea_march(Matrix, Count),
    writeln(Count).

day25_p2(File, Score):-
    phrase_from_file(data(Players), File),
    play_p2(Players, Score).

day25_p1(Score):-
    day25_p1("data/day25_p1_data", Score).

day25_p1_test(Score):-
    day25_p1("data/day25_p1_test", Score).

day25_p2(Score):-
    day25_p2("data/day25_p1_data", Score).

day25_p2_test(Score):-
    day25_p2("data/day25_p1_test", Score).

day25:-
    day25_p1_test(58),
    day25_p1(920580),
    day25_p2_test(_),
    day25_p2(_).
