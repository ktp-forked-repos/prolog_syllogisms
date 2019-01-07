%% syllogism
%% definite clause grammer and meta-interpreter exercise.

%% step 1
opposite([A, B, is | T], [some, B, is, not | T]):-
  A = every; A = a,
  \+ member(not, T).
opposite([no, B, is | T], [some, B, is | T]):-
  \+ member(not, T).
opposite([some, B, is | T], [no, B, is | T]):-
  \+ member(not, T).
opposite([some, B, is, not | T], [a, B, is | T]).

%% step 2.1
/*
syllogism --> [a], noun, [is], objective.
syllogism --> [some], noun, [is], objective.
syllogism --> [no], noun, [is], objective.
syllogism --> [some], noun, [is], [not], objective.

objective --> determinant, noun.
determinant --> [a].
determinant --> [].
noun --> [_].
*/

%% step 2.2
syllogism([ (B :- C) ]) --> [a], noun(B), [is], determinant, noun(C).
syllogism([ (false :- B, C) ]) --> [no], noun(B), [is], determinant, noun(C).
syllogism([ (B1 :- true), (C1 :- true) ]) --> [some], [B], [is], determinant, [C], {combine1(B, C, B1, C1)}.
syllogism([ (B1 :- true), (false :- C1) ]) --> [some], [B], [is], [not], determinant, [C], {combine2(B, C, B1, C1)}.

combine1(B, C, B1, C1):-
  B1 =.. [B, T],
  T =.. [some, B, C],
  C1 =.. [C, T].
combine2(B, C, B1, C1):-
  B1 =.. [B, T],
  C1 =.. [C, T],
  T =.. [some, B, T2],
  T2 =.. [not, C].

determinant --> [a].
determinant --> [].
noun(B) --> [X], {tr(X, B)}.
tr(X, B):-
  B =.. [X, _V].

%% step 3


%% ***************** test *********************
:- use_module(library(plunit)).
/*
:- begin_tests(opposite).
test('1', true(Opp == [some,cat,is,not,a,mammal])):-
  opposite([a,cat,is,a,mammal],Opp).
test('2', true(Opp == [some,cat,is,not,a,mammal])):-
  opposite([every,cat,is,a,mammal],Opp).
test('3', true(Opp == [no,cat,is,a,mammal])):-
  opposite([some,cat,is,a,mammal],Opp).
test('4', true(Opp == [a,cat,is,a,mammal])):-
  opposite([some,cat,is,not,a,mammal],Opp).
test('5', true(Opp == [some,banker,is,not,greedy])):-
  opposite([a,banker,is,greedy],Opp).
test('6', true(Opp == [some,banker,is,not,greedy])):-
  opposite([every,banker,is,greedy],Opp).
test('7', true(Opp == [some,banker,is,greedy])):-
  opposite([no,banker,is,greedy],Opp).
test('8', true(Opp == [a,banker,is,greedy])):-
  opposite([some,banker,is,not,greedy],Opp).
:- end_tests(opposite).
?- run_tests(opposite).
*/
