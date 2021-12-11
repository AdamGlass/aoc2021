:- use_module(library(dcg/basics)).
:- use_module(library(tabling)).
:- use_module(library(clpfd)).

file_line(File, Line) :-
    setup_call_cleanup(open(File, read, In),
        stream_line(In, Line),
        close(In)).

stream_line(In, Line) :-
    repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  Line0 = Line
    ;   !,
        fail
    ).


% valid_line(Input, Stack, Retired, Result)

match(<, >).
match('[', ']').
match('{', '}').
match('(', ')').

points(')', 3).
points(']', 57).
points('}', 1197).
points(>, 25137).

p2_points(A,P):-
    nth1(P,[')',']', '}', >], A).

valid_line([], [], _, valid).
valid_line([], Stacks, _, incomplete(Stacks)):-
    length(Stacks, L),
    L > 0.
valid_line([Closer|Inputs], [Opener|Stacks], Retired, Result):-
    match(Opener, Closer),
    valid_line(Inputs, Stacks, [Closer|Retired], Result).
valid_line([Closer|_], [Opener|_], Retired, Result):-
    match(_, Closer),
    match(Opener, ShouldClose),
    Closer \= ShouldClose,
    length(Retired, L),
    Result = corrupt(L, Closer).
valid_line([Opener|Inputs], Stacks, Retired, Result):-
    match(Opener,_),
    valid_line(Inputs, [Opener|Stacks], [Opener|Retired], Result).

parse_line(Line, Result):-
    string_chars(Line, In),
    valid_line(In, [], [], Result).

day10_p1(File, Score):-
    findall(Points, (
		file_line(File, L),
		parse_line(L, corrupt(_,C)),
		points(C, Points)
	    ),
	    PointsList),
    sum_list(PointsList, Score).

day10_p1(Score):-
    day10_p1("data/day10_p1_data", Score).

day10_p1_test(Score):-
    day10_p1("data/day10_p1_test", Score).

score_stack_acc([], AccScore, AccScore).
score_stack_acc([S|Ss], AccScore, Score):-
    p2_points(S, Points),
    NewScore is AccScore * 5  + Points,
    score_stack_acc(Ss, NewScore, Score).

score_stack(Stack, Score):-
    findall(Closer,
	 (member(Opener, Stack),
	  match(Opener, Closer)),
	 CompleterStack),
    score_stack_acc(CompleterStack, 0, Score).

day10_p2(File, Score):-
    findall(S, (
		file_line(File, L),
		parse_line(L, incomplete(Stack)),
		score_stack(Stack, S)
	    ),
	    ScoreList),
    sort(ScoreList, SortedList),
    length(L, Len), length(ML, Len),
    append([L, [M], ML], SortedList),
    Score = M.

day10_p2(Score):-
    day10_p2("data/day10_p1_data", Score).

day10_p2_test(Score):-
    day10_p2("data/day10_p1_test", Score).

day10:-
    day10_p1_test(26397),
    day10_p1(343863),
    day10_p2_test(288957),
    day10_p2(2924734236).

day10_profile:-
    profile(day10).
