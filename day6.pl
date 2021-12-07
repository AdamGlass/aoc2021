:- use_module(library(dcg/basics)).

fish([X]) --> integer(X).
fish([X|Xs]) --> integer(X), comma, fish(Xs), blanks.

comma --> [C], {char_code(',', C)}.

fish_day([], AccFish, YFish, NewFish):-
    reverse(AccFish, NewAcc),
    append(NewAcc, YFish, NewFish).
fish_day([0|Fs], AccFish, YFish, NewFish):-
    fish_day(Fs, [6|AccFish], [8|YFish], NewFish).
fish_day([F|Fs], AccFish, YFish, NewFish):-
    NF is F - 1,
    fish_day(Fs, [NF|AccFish], YFish, NewFish).

frepro(Fish, 0, Fish).
frepro(Fish, Days, NewFish):-
    fish_day(Fish, [], [], AccFish),
    NewDays is Days - 1,
    frepro(AccFish, NewDays, NewFish).

fish_repro(Fish, Days, NewFish):-
    frepro(Fish, Days, NewFish).
    
count_fish_stage(Fish, Stage, Count):-
    include(=(Stage), Fish, StageFish), length(StageFish, Count).

fast_fish(Fish, NewFish):-
    findall(X,
            (between(0,8, Stage),
             count_fish_stage(Fish, Stage, X)
             ),
            NewFish).

day6_p1(File, Days, Score):-
    phrase_from_file(fish(Fish), File),
    fish_repro(Fish, Days, NewFish),
    length(NewFish, Score).


day6_p1(Days, Score):-
    day6_p1("data/day6_p1_data", Days, Score).

day6_p1_test(Days, Score):-
    day6_p1("data/day6_p1_test", Days, Score).

%day6_p2(File, Score).

ffish_day([F0, F1, F2, F3, F4, F5, F6, F7, F8], NewFish):-
    NewF6 is F7+F0,
    NewFish = [F1, F2, F3, F4, F5, F6, NewF6, F8, F0].

ffrepro(Fish, 0, Fish).
ffrepro(Fish, Days, NewFish):-
    ffish_day(Fish, AccFish),
    NewDays is Days - 1,
    ffrepro(AccFish, NewDays, NewFish).

day6_p2(File, Days, Score):-
    phrase_from_file(fish(SlowFish), File),
    fast_fish(SlowFish, Fish),
    ffrepro(Fish, Days, NewFish),
    sum_list(NewFish, Score).

day6_p2(Days,Score):-
    day6_p2("data/day6_p1_data", Days, Score).

day6_p2_test(Days, Score):-
    day6_p2("data/day6_p1_test", Days, Score).

day6:-
    day6_p1_test(18, 26),
    day6_p1_test(80, 5934),
    day6_p1(80, 352195),
    day6_p2_test(18, 26),
    day6_p2_test(80, 5934),
    day6_p2_test(256, 26984457539),
    day6_p2(256, 1600306001288).
