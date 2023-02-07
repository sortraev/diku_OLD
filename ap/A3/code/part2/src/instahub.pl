%%% instahub

/*
 * level 0
 */
follows(G, X, Y) :-
  adj(G, X, X_adj),
  elem(Y, X_adj).

ignores(G, X, Y) :-
  follows(G, Y, X),
  notFollows(G, X, Y).

notFollows(G, X, Y) :-
  adj(G, X, X_adj),
  notElem(G, Y, X_adj).


/*
 * level 1
 */
popular(G, X) :-
  adj(G, X, X_adj),
  allFollow(G, X_adj, X).

outcast(G, X) :-
  adj(G, X, X_adj),
  noneFollow(G, X_adj, X).

friendly(G, X) :-
  inAdj(G, X, X_inAdj),
  followsAll(G, X_inAdj, X).

hostile(G, X) :-
  inAdj(G, X, X_inAdj),
  followsNone(G, X_inAdj, X).


/*
 * level 2
 */
% My `aware/3` simply encodes the transitive closure of the `following`
% relation. To avoid infinite recursion, I keep track of previously visited
% persons.
aware(G, X, Y) :-
  different(G, X, Y),
  reachable(G, X, Y, []).

reachable(G, X, Y, _Visited) :-
  follows(G, X, Y).
reachable(G, X, Y, Visited) :-
  notElem(G, X, Visited),
  follows(G, X, Z),
  reachable(G, Z, Y, [X | Visited]).



% ignorant uses a slightly different tactic than aware; first, `reachables`
% performs a breadth-first search to find all nodes reachable from X; then, Y is
% asserted to not be a member of the set of nodes reachable from X.
ignorant(G, X, Y) :-
  different(G, X, Y),
  reachables(G, [X], [], Reachable_from_X),
  notElem(G, Y, Reachable_from_X).


% reachables maintains a list of nodes which at this point are known to be
% reachable from X but which are to be recursively processed for reachability.
reachables(G, [X | Todo], Visited, Final) :-
  notElem(G, X, Visited),
  adj(G, X, X_adj),
  concat(Todo, X_adj, Todo_), % BFS; flip Todo and X_adj to get DFS.
  reachables(G, Todo_, [X | Visited], Final).

reachables(G, [X | Todo], Visited, Final) :-
  elem(X, Visited),
  reachables(G, Todo, Visited, Final).

%% no more unprocessed nodes? copy visited nodes to final!
reachables(_G, [], Visited, Visited).


/*
 * level 3 - nope! not attempted.
 */
% same_world(G, H, K)
% different_world(G, H)


/*
 * level 1 specific helper predicates
 */
allFollow(_, [], _).
allFollow(G, [Y | T], X) :-
  follows(G, Y, X),
  allFollow(G, T, X).

noneFollow(_, [], _).
noneFollow(G, [Y | T], X) :-
  notFollows(G, Y, X),
  noneFollow(G, T, X).

followsAll(_, [], _).
followsAll(G, [Y | T], X) :-
  follows(G, X, Y),
  followsAll(G, T, X).

followsNone(_, [], _).
followsNone(G, [Y | T], X) :-
  notFollows(G, X, Y),
  followsNone(G, T, X).


/*
 * helper predicates
 */
elem(X, [X | _]).
elem(X, [_ | T]) :- elem(X, T).

% notElem(G, X, Gsubset) succeeds if X is *not* in Gsubset. Needs G to determine
% if X is different from other persons.
notElem(_, _, []).
notElem(G, X, [Y | T]) :-
  different(G, X, Y),
  notElem(G, X, T).

% different(G, X, Y) succeeds if X and Y are persons of the graph G and X != Y.
different(G, X, Y) :-
  removeFirst(G, person(X, _), G_minus_X),
  elem(person(Y, _), G_minus_X).

% removeFirst(X, G, G_minus_X) removes first occurence of X in G, storing
% resulting resulting graph in G_minus_X.
removeFirst([X | T], X, T).
removeFirst([Y | T], X, [Y | T_minus_X]) :-
  removeFirst(T, X, T_minus_X).


% adj(G, X, X_adj) extracts X's adjacency list in the graph G if X is the name
% of a person in G.
adj([person(X, X_adj) | _], X, X_adj).
adj([_ | T], X, X_adj) :-
  adj(T, X, X_adj).


% inAdj(G, G, X, X_inAdj) extracts the list of X's incoming adjacents in the
% graph G if X is the name of a person in G. I don't know how to make the
% predicate work without first asserting X as a member of G.
inAdj(G, X, X_inAdj) :-
  elem(person(X, _), G),
  inAdj_(G, G, X, X_inAdj).

inAdj_(_, [], _, []).
inAdj_(G, [person(_, Y_adj) | T], X, X_inAdj) :-
  notElem(G, X, Y_adj),
  inAdj_(G, T, X, X_inAdj).
inAdj_(G, [person(Y, Y_adj) | T], X, [Y | X_inAdj]) :-
  elem(X, Y_adj),
  inAdj_(G, T, X, X_inAdj).


concat([], SomeList, SomeList).
concat([Head | L1], L2, [Head | L3]) :-
  concat(L1, L2, L3).

/*
 * sample network (the one used for testing).
 */
% g2([person(a, [b, d]),
%     person(b, [a, d, e]),
%     person(c, [d, e]),
%     person(d, [b, c, g]),
%     person(e, [d]),
%     person(f, [i, j]),
%     person(g, [d, e, h]),
%     person(h, []),
%     person(i, [f]),
%     person(j, [i]),
%     person(k, [])]).
