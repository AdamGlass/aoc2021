:- use_module(library(dcg/basics)).

commands([C]) --> command(C).
commands([C|Cs]) --> command(C), commands(Cs).

command(C) --> cop(C), blanks.

cop(sop(forward, Amount)) --> forward, amount(Amount).
cop(sop(up, Amount)) --> up, amount(Amount).
cop(sop(down, Amount)) --> down, amount(Amount).

forward --> `forward `.
down --> `down `.
up --> `up `.

amount(X) --> integer(X).

move(sop(forward, Amount), X, Y, NewX, NewY):-
    NewX is X + Amount,
    NewY = Y.
move(sop(up, Amount), X, Y, NewX, NewY):-
    NewY is Y - Amount,
    NewX = X.
move(sop(down, Amount), X, Y, NewX, NewY):-
    NewY is Y + Amount,
    NewX = X.

compute_movement([], AccX, AccY, AccX, AccY).
compute_movement([C|Cs], AccX, AccY, X, Y):-
    move(C, AccX, AccY, NewX, NewY),
    compute_movement(Cs, NewX, NewY, X, Y).

move_aim(sop(forward, Amount), X, Y, Aim, NewX, NewY, Aim):-
    NewX is X + Amount,
    NewY is Y + Aim * Amount.
move_aim(sop(up, Amount), X, Y, Aim, X, Y, NewAim):-
    NewAim is Aim - Amount.
move_aim(sop(down, Amount), X, Y, Aim, X, Y, NewAim):-
    NewAim is Aim + Amount.

compute_movement_aim([], AccX, AccY, AccAim, AccX, AccY, AccAim).
compute_movement_aim([C|Cs], AccX, AccY, AccAim, X, Y, Aim):-
    move_aim(C, AccX, AccY, AccAim, NewX, NewY, NewAim),
    compute_movement_aim(Cs, NewX, NewY, NewAim, X, Y, Aim).

day2_p1(File, Mul):-
    phrase_from_file(commands(Data), File),
    compute_movement(Data, 0, 0, X,Y),
    Mul is X * Y.

day2_p2(File, Mul):-
    phrase_from_file(commands(Data), File),
    compute_movement_aim(Data, 0, 0, 0, X,Y, _),
    Mul is X * Y.

day2_p1_test(V):-
    day2_p1("data/day2_p1_test", V).

day2_p1(V):-
    day2_p1("data/day2_p1_data", V).

day2_p2_test(V):-
    day2_p2("data/day2_p1_test", V).

day2_p2(V):-
    day2_p2("data/day2_p1_data", V).

day2 :-
    day2_p1_test(150),
    day2_p1(1746616),
    day2_p2_test(900),
    day2_p2(1741971043).
