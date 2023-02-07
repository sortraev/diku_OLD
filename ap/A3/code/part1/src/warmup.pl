% Advanced Programming Assignment 3


/* move one layer of s() from X to
 * Y each iteration until X is z.
 */
add(z, Res, Res).
add(s(X), Y, Res) :-
  add(X, s(Y), Res).


/* recurse to the bottom of X; then
 * add Y to Acc at each recursion level.
 */
mult(z, _, z).
mult(s(X), Y, Res) :-
  mult(X, Y, Acc),
  add(Y, Acc, Res).


/* repeatedly remove one s() from X and
 * Y until either (or both) becomes z.
 */
comp(s(_), z,    gt).
comp(z,    s(_), lt).
comp(z,    z,    eq).
comp(s(X), s(Y), R) :- comp(X, Y, R).


insert(N, leaf, node(N, leaf, leaf)).

insert(N, node(M, X, Y), node(M, X, Y)) :-
  comp(N, M, eq).

insert(N, node(M, T1, T2), node(M, T1_, T2)) :-
  comp(N, M, lt),
  insert(N, T1, T1_).

insert(N, node(M, T1, T2), node(M, T1, T2_)) :-
  comp(N, M, gt),
  insert(N, T2, T2_).

insertlist([], T_in, T_in).

insertlist([Head | Rest], T_in, T_out) :-
  insert(Head, T_in, T_tmp),
  insertlist(Rest, T_tmp, T_out).

