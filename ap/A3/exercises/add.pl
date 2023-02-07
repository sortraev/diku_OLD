
add(z, R, R).        % move result from second to third argument.
add(s(X), Y, R) :-
  add(X, s(Y), R).   % move one s() from X to Y.

mul(z, _, z).
mul(s(X), Y, R) :-
  mul(X, Y, Z),      % recurse to the bottom of X;
  add(Y, Z, R).      % then add Y to Z at each recursion level.


/* repeatedly remove one s() from X and
 * Y until either (or both) becomes z.
 */
comp(s(_), z,    gt).
comp(z,    s(_), lt).
comp(z,    z,    eq).
comp(s(X), s(Y), R)  :- comp(X, Y, R).
