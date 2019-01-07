%% syllogism
%% definite clause grammer and meta-interpreter exercise.

:- include(arguments).
:- dynamic cl/3.

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
syllogism([ (C :- B) ]) --> [a], noun(B), [is], determinant, noun(C).
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
translate(N):-
  p(N, L1),
  p(N, L2),
  L1 \= L2, !,
  c(N, L),
  opposite(L, L3),
  phrase(syllogism(Clause1), L1),
  phrase(syllogism(Clause2), L2),
  phrase(syllogism(Clause3), L3),
  assertall(N, Clause1),
  assertall(N, Clause2),
  assertall(N, Clause3).

%% step 4
eval(_N, true):- !.
eval(N, (H1, H2)):-
  !,
  eval(N, H1),
  eval(N, H2).
eval(N, H):-
  clause(cl(N, H, B), true),
  eval(N, B).

/*
| ?- clause(cl(1, bird(X), B),L).
B = robin(_A),
L = true ? ;
no

| ?- clause(cl(1, robin(X), B),true).
X = some(robin,reptile),
B = true ? ;
no
*/

valid(N):-
  eval(N, false).

invalid(N):-
  cl(N, _, _),
  \+ eval(N, false).

%% step 5
show_syllogism(N):-
  p(N, P1),
  p(N, P2),
  P1 \= P2, !,
  c(N, C),
  write('   '),
  show_list(P1),
  write('   '),
  show_list(P2),
  write('=>'),
  nl,
  write('   '),
  show_list(C).

show_list([H|T]):-
  write(H),
  write(' '),
  show_list(T).
show_list([]):-
  nl.
%% *************** utilities ********************

% the following is built-in in some Prologs but not Sicstus

forall(C1,C2) :- \+ (call(C1), \+ call(C2)).


% the following can be used by your test/1 program to display the clauses 
% you generate for the N'th syllogism stored as facts of the form cl(N,Clause).


show_clauses(N) :-
  forall(
    cl(N,H,B),
    (
      write('   '),
      write((H:-B)),
      write('.'),
      nl
    )
  ).

/* The above could also be written using format/2 (see manual)

show_clauses(N) :-
  forall(cl(N,H,B),
         format("   ~w.~n",[(H:-B)] ).

*/


% The following can be used to assert each clause (H:-B) of the list of clauses 
% produced by parsing a syllogism sentence list number N  as facts 
% of the form cl(N,H,B)

assertcl(N,(H:-B)) :- !, assert(cl(N,H,B)).
assertcl(N,H) :- assert(cl(N,H,true)). 

assertall(_,[]).  
assertall(N,[C|Cs]) :-
  assertcl(N,C),
  assertall(N,Cs).


% You might find the following useful

writeL([]).
writeL([W|Ws]) :-
  write(W),
  write(' '),
  writeL(Ws).


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

