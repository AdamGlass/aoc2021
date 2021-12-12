:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).
:- table cave_next/3.

connects([C]) --> connect(C).
connects([C|Cs]) --> connect(C), connects(Cs).

connect(C) --> cave(X), dash, cave(Y), blank, {C = connect(X,Y)}.
dash --> [C], {char_code('-', C)}.

cave(C) --> bigcave_ident(I), {string_codes(S, I), C = cave(S, big)}.
cave(C) --> smallcave_ident(I), {string_codes(S, I), C = cave(S, small)}.

bigcave_ident([C]) --> bigcave_char(C).
bigcave_ident([C|Cs]) --> bigcave_char(C), bigcave_ident(Cs).

smallcave_ident([C]) --> smallcave_char(C).
smallcave_ident([C|Cs]) --> smallcave_char(C), smallcave_ident(Cs).

smallcave_char(A) --> alpha(A), {code_type(A, lower)}.
bigcave_char(A) --> alpha(A), {code_type(A, upper)}.

alpha(A) --> [A], {code_type(A, alpha)}.

cave_both_(connect(_, Next), Next).
cave_next_(connect(Next, _), Next).


cave_next_(C1, connect(C1, Next), Next).
cave_next_(C1, connect(Next, C1), Next).

cave_next(CaveNetwork, StartCave, NextCaves):-
    findall(PotentialNext,
            (member(Connection, CaveNetwork),
             cave_next_(StartCave, Connection, PotentialNext)),
            NextCaves).

p1_visit(Current, VisitedSmall):-
    \+ member(Current, VisitedSmall).

p2_visit(Current, Current, []).
p2_visit(_, Current, VisitedSmall):-
    \+ member(Current, VisitedSmall).
p2_visit(DoubleOk, Current, VisitedSmall):-
    DoubleOk = Current,
    findall(1,
            member(Current, VisitedSmall),
            Visited),
    length(Visited, 1).

cave_path(_, cave(End, small), cave(End, small), _, _, Traveled, Path):-
    Path = [cave(End, small)|Traveled].

cave_path(CaveNetwork, Current, End, VisitP, VisitedSmall, Traveled, Path):-
    Current \= End,
    call(VisitP, Current, VisitedSmall),
    Current = cave(_, Size),
    (Size = small ->
         NewVisitedSmall = [Current|VisitedSmall]
    ;
         NewVisitedSmall = VisitedSmall
    ),
    NewTraveled = [Current|Traveled],
    cave_next(CaveNetwork, Current, NextCaves),
    member(Next, NextCaves),
    cave_path(CaveNetwork, Next, End, VisitP, NewVisitedSmall, NewTraveled, Path).

day12_path_report(Paths):-
    forall(member(P, Paths),
           (reverse(P, ForwardPath),
            forall(member(cave(Name, _), ForwardPath),
                   (write(Name), write("-"))),
            writeln(""))).

day12_common(CaveNetwork, Paths, VisitP):-
    findall(Path,
            cave_path(CaveNetwork, cave("start", small), cave("end", small), VisitP, [], [], Path),
            Paths).

day12_p1(File, Score):-
    phrase_from_file(connects(CaveNetwork), File),
    day12_common(CaveNetwork, Paths, p1_visit),
    day12_path_report(Paths),
    length(Paths, Score).

day12_p2(File, Score):-
    phrase_from_file(connects(CaveNetwork), File),
    findall(SmallCave,
            (member(Connection, CaveNetwork),
             cave_both_(Connection, cave(Name, small)),
             Name \= "start",
             Name \= "end",
             SmallCave = cave(Name, small)),
            SmallCavesList),
    list_to_set(SmallCavesList, SmallCavesSet),
    writeln(SmallCavesSet),
    findall(Paths,
            (member(DoubleSmallCave, SmallCavesSet),
             day12_common(CaveNetwork, Paths, p2_visit(DoubleSmallCave))),
            TotalPaths),
    forall(member(P, TotalPaths),
           day12_path_report(P)),
    findall(Count,
            (member(P, TotalPaths),
             length(P, Count)),
            PathCounts),
    writeln(PathCounts).

day12_p1(Score):-
    day12_p1("data/day12_p1_data", Score).

day12_p1_test(Score):-
    day12_p1("data/day12_p1_test1", Score).

day12_p1_test2(Score):-
    day12_p1("data/day12_p1_test2", Score).

day12_p1_test3(Score):-
    day12_p1("data/day12_p1_test3", Score).

day12_p2(Score):-
    day12_p2("data/day12_p1_data", Score).

day12_p2_test(Score):-
    day12_p2("data/day12_p1_test1", Score).

day12_p2_test2(Score):-
    day12_p2("data/day12_p1_test2", Score).

day12_p2_test3(Score):-
    day12_p2("data/day12_p1_test3", Score).

day12:-
    day12_p1_test(_),
    day12_p1(_),
    day12_p2_test(_),
    day12_p2(_).

