%%% instahub tests.

% see report for a pretty drawing of this graph.
g1([person(a, [b, d]),
    person(b, [a, d, e]),
    person(c, [d, e]),
    person(d, [b, c, g]),
    person(e, [d]),
    person(f, [i, j]),
    person(g, [d, e, h]),
    person(h, []),
    person(i, [f]),
    person(j, [i]),
    person(k, [])]).

:- begin_tests(level0_tests).

% follows/3
test("multiple incoming follows",  set(X = [a, b, c, e, g])) :-
  g1(G), follows(G, X, d).
test("multiple outgoing follows", set(X = [d, b])) :-
  g1(G), follows(G, a, X).
test("no outgoing follows.",      set(X = [])) :-
  g1(G), follows(G, h, X).
test("no incoming follows.",      set(X = [])) :-
  g1(G), follows(G, X, k).
test("Y follows non-existing person", set(Y = [])) :-
  g1(G), follows(G, Y, anders).
test("non-existing person follows Y", set(Y = [])) :-
  g1(G), follows(G, anders, Y).

% ignores/3
test("multiple incoming ignores", set(X = [e, h])) :-
  g1(G), ignores(G, X, g).
test("multiple outgoing ignores", set(X = [b, c, g])) :-
  g1(G), ignores(G, e, X).
test("Y ignores non-existing person", set(Y = [])) :-
  g1(G), ignores(G, Y, anders).
test("non-existing person ignores Y", set(Y = [])) :-
  g1(G), ignores(G, anders, Y).

:- end_tests(level0_tests).



:- begin_tests(level1_tests).
% popular/2

test("all populars", set(X = [d, h, i, k])) :-
  g1(G), popular(G, X).
test("all outcasts", set(X = [e, h, j, k])) :-
  g1(G), outcast(G, X).
test("all friendlies", set(X = [a, b, c, f, g, k])) :-
  g1(G), friendly(G, X).
test("all hostiles", set(X = [e, h, j, k])) :-
  g1(G), hostile(G, X).


test("popular and outcast", set(X = [h, k])) :-
  g1(G), popular(G, X), outcast(G, X).
test("popular and friendly", set(X = [k])) :-
  g1(G), popular(G, X), friendly(G, X).
test("popular and hostile", set(X = [h, k])) :-
  g1(G), popular(G, X), hostile(G, X).
test("outcast and friendly", set(X = [k])) :-
  g1(G), outcast(G, X), friendly(G, X).
test("outcast and hostile", set(X = [e, h, j, k])) :-
  g1(G), outcast(G, X), hostile(G, X).
test("friendly and hostile", set(X = [k])) :-
  g1(G), friendly(G, X), hostile(G, X).

test("popular, outcast, friendly, and hostile", set(X = [k])) :-
  g1(G), popular(G, X), outcast(G, X), friendly(G, X), hostile(G, X).

test("popularity of non-existing person", fail) :-
  g1(G), popular(G, anders).
test("isolarity of non-existing person", fail) :-
  g1(G), outcast(G, anders).
test("friendliness of non-existing person", fail) :-
  g1(G), friendly(G, anders).
test("hostility of non-existing person", fail) :-
  g1(G), hostile(G, anders).


:- end_tests(level1_tests).



:- begin_tests(level2_tests).
/*
 * aware tests
 */
test("irreflexitivity of awareness", set(X = [])) :-
  g1(G), aware(G, X, X).
test("transitive awareness", nondet) :-
  g1(G), aware(G, a, d), aware(G, d, h), aware(G, a, h).
test("awareness of non-existing person", set(X = [])) :-
  g1(G), aware(G, X, anders).

test("multiple incoming awarenesses", set(X = [e, c, a, b, g])) :-
  g1(G), aware(G, X, d).
test("multiple outgoing awarenesses", set(X = [b, c, d, e, g, h])) :-
  g1(G), aware(G, a, X).

test("awareness in a disconnected graph 1", set(X = [i, j])) :-
  g1(G), aware(G, f, X).
test("awareness in a disconnected graph 2", set(X = [])) :-
  g1(G), aware(G, f, X), aware(G, d, X).
test("awareness in a disconnected graph 3", set(X = [])) :-
  g1(G), aware(G, X, a), aware(G, X, k).
test("awareness in a disconnected graph 4", set(X = [])) :-
  g1(G), aware(G, X, f), aware(G, X, k).



/*
 * ignorant tests
 */
test("irreflexitivity of ignorance", set(X = [])) :-
  g1(G), ignorant(G, X, X).
test("symmetric ignorance", set( Y = [f, i, j, k])) :-
  g1(G), ignorant(G, a, Y), ignorant(G, Y, a).
test("transitive ignorance", set(Y = [k])) :-
  g1(G), ignorant(G, a, Y), ignorant(G, Y, f), ignorant(G, a, f).


test("multiple incoming ignorances", set(X = [f, i, h, j, k])) :-
  g1(G), ignorant(G, X, d).
test("multiple outgoing ignorances", set(X = [f, i, j, k])) :-
  g1(G), ignorant(G, e, X).


test("ignorance in a disconnected graph 1", set(X = [a, b, e, c, d, g, h, k])) :-
  g1(G), ignorant(G, i, X).
test("ignorance in a disconnected graph 2", set(X = [a, b, e, c, d, g, h, k])) :-
  g1(G), ignorant(G, f, X).
test("ignorance in a disconnected graph 3", set(X = [k])) :-
  g1(G), ignorant(G, f, X), ignorant(G, d, X).
test("ignorance in a disconnected graph 4", set(X = [h])) :-
  g1(G), ignorant(G, X, a), ignorant(G, X, k), ignorant(G, X, f).
test("ignorance of non-existing person", set(X = [])) :-
  g1(G), ignorant(G, anders, X).


:- end_tests(level2_tests).
