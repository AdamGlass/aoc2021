:- module(matrix, [lmatrix/4,
		   lmatrix_matrix/2,
		   lmatrix_limits/3,
		   matrix/4,
		   matrix_limits/3,
		   matrix_element_transform/3,
		   matrix_element_add/3,
		   matrix_transform/5,
		   matrix_xy_add/5,
		   matrix_xy_adjacent_all/4,
		   matrix_xy_adjacent_cardinal/4,
		   matrix_xy/3,
		   matrix_xy_adjacent_all/2,
		   matrix_write/2,
		   matrix_foldl/4,
		   matrix_init/1
		  ]).
		   
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

lmatrix_limits(ListMatrix, XMax, YMax):-
    length(ListMatrix, Rows),
    YMax is Rows - 1,
    nth0(0, ListMatrix, Row),
    length(Row, Columns),
    XMax is Columns - 1.

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
    matrix_limits(Matrix, XMAX, YMAX),
    forall(between(0, YMAX, Y),
	   (forall(between(0, XMAX, X),
		   (matrix(Matrix, X, Y, Value),
		    call(ElementFormatter,Value,Display),
		    write(Display))),
	    writeln(""))).

matrix_foldl(Op, List, InitialMatrix, OutMatrix):-
    foldl(Op, List, InitialMatrix, OutMatrix).
	      
matrix_init(InitMatrix):-
    empty_assoc(InitMatrix).
