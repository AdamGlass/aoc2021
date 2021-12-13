:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- use_module(library(assoc)).

origami(T,F) --> tdots(T), blanks, tfolds(F), blanks.

tdots([T]) --> tdot(T).
tdots([T|Ts]) --> tdot(T), tdots(Ts).

tdot(D) --> integer(X), comma, integer(Y), blanks, {D = tdot(X,Y)}.

comma --> [C], {char_code(',', C)}.
equal --> `=`.

tfolds([F]) --> tfold(F).
tfolds([F|Fs]) --> tfold(F), tfolds(Fs).

tfold(F) -->  tfold_prefix, blank, axis(A), equal, integer(L), blanks, {F = ofold(A,L)}.

tfold_prefix --> `fold along`.

axis(x) --> `x`.
axis(y) --> `y`.

max_xykey([], AccX, AccY, AccX, AccY).
max_xykey([X+Y|Keys], AccX, AccY, MaxX, MaxY):-
    XMax is max(X, AccX),
    YMax is max(Y, AccY),
    max_xykey(Keys, XMax, YMax, MaxX, MaxY).

amatrix_limits(Matrix, XMAX, YMAX):-
    assoc_to_keys(Matrix,XYList),
    max_xykey(XYList, 0, 0, XMAX, YMAX).

amatrix(Matrix, X, Y, Value) :-
    get_assoc(X+Y, Matrix, Value).
amatrix(Matrix, X, Y, 0):-
    \+ get_assoc(X+Y, Matrix, _).

matrix(Matrix, X, Y, Value):-
    amatrix(Matrix, X, Y, Value).

matrix_limits(Matrix, XMAX, YMAX):-
    amatrix_limits(Matrix, XMAX, YMAX).

matrix_transform(Matrix, X, Y, Value, NewMatrix):-
    put_assoc(X+Y, Matrix, Value, NewMatrix).
    
matrix_write(Matrix):-
    amatrix_write(Matrix).

tpaper_matrix_(tdot(X,Y), Matrix, NewMatrix):-
    matrix_transform(Matrix, X, Y, 1, NewMatrix).
    
tpaper_write(Matrix):-
    matrix_limits(Matrix, XMAX, YMAX),
    forall(between(0, YMAX, Y),
	   (forall(between(0, XMAX, X),
		   (matrix(Matrix, X, Y, Value),
		    (Value = 1 ->
			 write('#')
		    ;
		         write('.')
		    ))),
	    writeln(""))).

tpaper_dot_count(Matrix, Count):-
    matrix_limits(Matrix, XMAX, YMAX),
    findall(1,
	    (between(0, YMAX, Y),
	     between(0, XMAX, X),
	     matrix(Matrix, X, Y, 1)),
	    Dots),
    length(Dots, Count).

tpaper_matrix(Dots, Matrix):-
    empty_assoc(InitMatrix),
    foldl(tpaper_matrix_, Dots, InitMatrix, Matrix).

tpaper_folded(Matrix, ofold(x, Line), FoldedMatrix):-
    findall(Dot,
	    (gen_assoc(X+Y, Matrix, 1),
	     X < Line,
	     Dot = tdot(X,Y)),
	    SavedDots),
    findall(Dot,
	    (gen_assoc(X+Y, Matrix, 1),
	     X > Line,
	     FoldX is X - (X-Line)*2,
	     Dot = tdot(FoldX,Y)),
	    ReflectedDots),
    append(SavedDots, ReflectedDots, AllDots),
    tpaper_matrix(AllDots, FoldedMatrix).

tpaper_folded(Matrix, ofold(y, Line), FoldedMatrix):-
    findall(Dot,
	    (gen_assoc(X+Y, Matrix, 1),
	     Y < Line,
	     Dot = tdot(X,Y)),
	    SavedDots),
    findall(Dot,
	    (gen_assoc(X+Y, Matrix, 1),
	     Y > Line,
	     FoldY is Y - (Y-Line)*2,
	     Dot = tdot(X,FoldY)),
	    ReflectedDots),
    append(SavedDots, ReflectedDots, AllDots),
    tpaper_matrix(AllDots, FoldedMatrix).

tpaper_fold(Fold, Matrix, FoldedMatrix):-
    tpaper_folded(Matrix, Fold, FoldedMatrix),
    writeln(["FOLD", Fold]).

day13_common(Dots, Folds, FoldedMatrix):-
    tpaper_matrix(Dots, Matrix),
    foldl(tpaper_fold, Folds, Matrix, FoldedMatrix).

day13_p1(File, Score):-
    phrase_from_file(origami(Dots, [Fold|_]), File),
    day13_common(Dots, [Fold], FoldedMatrix),
    tpaper_dot_count(FoldedMatrix, Score).

day13_p2(File):-
    phrase_from_file(origami(Dots, Folds), File),
    day13_common(Dots, Folds, FoldedMatrix),
    tpaper_write(FoldedMatrix).

day13_p1(Score):-
    day13_p1("data/day13_p1_data", Score).

day13_p1_test(Score):-
    day13_p1("data/day13_p1_test", Score).

day13_p2:-
    day13_p2("data/day13_p1_data").

day13_p2_test:-
    day13_p2("data/day13_p1_test").

day13:-
    day13_p1_test(17),
    day13_p1(720),
    day13_p2_test,
    day13_p2.
