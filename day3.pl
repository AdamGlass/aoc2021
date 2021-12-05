:- use_module(library(dcg/basics)).

binary_strings([]) --> eos.
binary_strings([X]) --> binary_value(X).
binary_strings([X|Xs]) --> binary_value(X), blank, binary_strings(Xs).

binary_value(X) --> binary_string(X).

binary_string([X]) --> binary_digit(X).
binary_string([X|Xs]) --> binary_digit(X),binary_string(Xs).

% binary_digit(D) --> [D], { char_code('0', D)}.
%  binary_digit(D) --> [D], { char_code('1', D)}.

binary_digit(0) --> [C], { char_code('0', C)}.
binary_digit(1) --> [C], { char_code('1', C)}.
