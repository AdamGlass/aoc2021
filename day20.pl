:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(matrix).

data(A,I) --> algorithm(A), blanks, image(I).

algorithm(A) --> bitstream(A), blanks, {length(A, 512)}.

image([A]) --> image_line(A).
image([A|As]) --> image_line(A), image(As).

image_line(L) --> bitstream(L), blank.

bitstream([A]) --> bdigit(A).
bitstream([A|As]) --> bdigit(A), bitstream(As).

bdigit(V) --> [C], {char_code('#', C), V = 1}.
bdigit(V) --> [C], {char_code('.', C), V = 0}.


% N.B. avoid use as a test of valid X, Y as its infinite
image(Image, X, Y, Background, Value):-
    (matrix(Image, X, Y, R) ->
      Value = R
    ;
      Value = Background
    ).

image_limits(Matrix, NXMin+NXMax, NYMin+NYMax):-
    assoc_to_keys(Matrix,XYList),
    findall(X,
	    member(X+_, XYList),
	    XList),
    findall(Y,
	    member(_+Y, XYList),
	    YList),
    min_list(XList, XMin),
    max_list(XList, XMax),
    min_list(YList, YMin),
    max_list(YList, YMax),
    NXMin is XMin - 2,
    NYMin is YMin - 2,
    NXMax is XMax + 2,
    NYMax is YMax + 2.

image_xy(Matrix, X, Y):-
    image_limits(Matrix, NXMin+NXMax, NYMin+NYMax),
    between(NXMin, NXMax, X),
    between(NYMin, NYMax, Y).

image_xy_adjacent_all_inclusive(DX, DY):-
    member(dir(DX, DY), [dir(-1, -1), dir(0, -1), dir(1,-1),
                         dir(-1, 0), dir(0,0), dir(1, 0),
                         dir(-1, 1), dir(0, 1), dir(1,1)]).

binary_decimal_([], _, Value, Value).
binary_decimal_([B|Bs], Power, Acc, Value):-
    NewAcc is B*Power + Acc,
    NewPower is Power * 2,
    binary_decimal_(Bs, NewPower, NewAcc, Value).

binary_decimal(Binary, Decimal):-
    reverse(Binary, BinaryR),
    binary_decimal_(BinaryR, 1, 0, Decimal).

convolve_(Image, Algorithm, Background, X+Y, InMatrix, OutMatrix):-
    findall(Value,
	    (image_xy_adjacent_all_inclusive(DX,DY),
	     BX is X + DX,
	     BY is Y + DY,
	     image(Image, BX, BY, Background, Value)),
	    Bits),
    length(Bits, 9),
    binary_decimal(Bits, Index),
    nth0(Index, Algorithm, NewValue),
    matrix_transform(InMatrix, X, Y, NewValue, OutMatrix).

convolve(Image, Algorithm, Background, Output):-
    matrix_init(InitMatrix),
    findall(X+Y,
	    image_xy(Image, X, Y),
	    Coords),
    foldl(convolve_(Image, Algorithm, Background), Coords, InitMatrix, Output).

count_lit_pixels(Image, Count):-
    findall(1,
	    (image_xy(Image, X, Y),
	     image(Image, X, Y, 0, 1)),
	    Pixels),
    length(Pixels, Count).

image_formatter(0,'.').
image_formatter(1,'#').

image_write(Image, Background):-
    image_limits(Image, XMIN+XMAX, YMIN+YMAX),
    forall(between(YMIN, YMAX, Y),
	   (forall(between(XMIN, XMAX, X),
		   (image(Image, X, Y, Background, Value),
		    image_formatter(Value,Display),
		    write(Display))),
	    writeln(""))).

convolves(Image, _, _, 0, Image).
convolves(Image, Algorithm, Background, Steps, OutImage):-
    NewSteps is Steps - 1,
    convolve(Image, Algorithm, Background, NewImage),
    (Background = 0 ->
	 nth0(0, Algorithm, NewBackground)
    ;
         nth0(511, Algorithm, NewBackground)
    ),
    convolves(NewImage, Algorithm, NewBackground, NewSteps, OutImage).

day20_p1(File, Score):-
    phrase_from_file(data(Algorithm,LImage), File),
    lmatrix_matrix(LImage, Image),
    convolves(Image, Algorithm, 0, 2, NewImage),
    count_lit_pixels(NewImage, Score).

day20_p2(File, Score):-
    phrase_from_file(data(Algorithm,LImage), File),
    lmatrix_matrix(LImage, Image),
    convolves(Image, Algorithm, 0, 50, NewImage),
    count_lit_pixels(NewImage, Score).

day20_p1(Score):-
    day20_p1("data/day20_p1_data", Score).

day20_p1_test(Score):-
    day20_p1("data/day20_p1_test", Score).

day20_p2(Score):-
    day20_p2("data/day20_p1_data", Score).

day20_p2_test(Score):-
    day20_p2("data/day20_p1_test", Score).

day20:-
    day20_p1_test(35),
    day20_p1(5498),
    day20_p2_test(3351),
    day20_p2(16014).
