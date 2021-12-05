:- use_module(library(dcg/basics)).
:- include("data/day2_p1_test.pl").
:- include("data/day2_p1_data.pl").

commands([C]) --> command(C), blank.
commands([C|Cs]) --> command(C), blank, commands(Cs).

command(sop(forward, Amount)) --> forward, blank, amount(Amount).
command(sop(up, Amount)) --> up, blank, amount(Amount).
command(sop(down, Amount)) --> down, blank, amount(Amount).

forward --> [forward].
down --> [down].
up --> [up].

amount(X) --> integer(X).

move(sop(forward, Amount), X, Y, NewX, NewY) :- NewX is X + Amount, NewY = Y.
move(sop(up, Amount), X, Y, NewX, NewY) :- NewY is Y - Amount, NewX = X.
move(sop(down, Amount), X, Y, NewX, NewY) :- NewY is Y + Amount, NewX = X.

compute_movement([], AccX, AccY, AccX, AccY).
compute_movement([C|Cs], AccX, AccY, X, Y):-
    move(C, AccX, AccY, NewX, NewY), compute_movement(Cs, NewX, NewY, X, Y).

day2_p1(Data, Mul) :-
    compute_movement(Data, 0, 0, X,Y), Mul is X * Y.

move_aim(sop(forward, Amount), X, Y, Aim, NewX, NewY, Aim) :- NewX is X + Amount, NewY is Y + Aim * Amount.
move_aim(sop(up, Amount), X, Y, Aim, X, Y, NewAim) :- NewAim is Aim - Amount.
move_aim(sop(down, Amount), X, Y, Aim, X, Y, NewAim) :- NewAim is Aim + Amount.

compute_movement_aim([], AccX, AccY, AccAim, AccX, AccY, AccAim).
compute_movement_aim([C|Cs], AccX, AccY, AccAim, X, Y, Aim):-
    move_aim(C, AccX, AccY, AccAim, NewX, NewY, NewAim), compute_movement_aim(Cs, NewX, NewY, NewAim, X, Y, Aim).

day2_p2(Data, Mul) :-
    compute_movement_aim(Data, 0, 0, 0, X,Y, _), Mul is X * Y.

day2_p1_test(Mul) :-
    day2_p1_testdata(Data), day2_p1(Data, Mul).
day2_p1(Mul) :-
    day2_p1_data(Data), day2_p1(Data, Mul).

day2_p2_test(Mul) :-
    day2_p1_testdata(Data), day2_p2(Data, Mul).
day2_p2(Mul) :-
    day2_p1_data(Data), day2_p2(Data, Mul).

day2 :-
    day2_p1_test(150), day2_p1(1746616),
    day2_p2_test(900), day2_p2(1741971043).

