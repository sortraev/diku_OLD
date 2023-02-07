nat(z).
nat(s(X)) :- nat(X).

add(z, X, X)      :- nat(X).
add(s(X), Y, Res) :- add(X, s(Y), Res).





known_to_be_older(christian, frederik).
known_to_be_older(frederik, knud).
known_to_be_older(knud, margrethe).
known_to_be_older(margrethe, benedikte).

is_older(X, Y) :- known_to_be_older(X, Y).
is_older(X, Y) :- known_to_be_older(Z, Y), is_older(X, Z).


last(X, [X | []]).
last(X, [_ | Tail]) :- last(X, Tail).


reverse(X, Y) :- rev_inner(X, Y, []).


rev_inner([], X, X).
rev_inner([X | Xt], Y, Res) :-
  rev_inner(Xt, Y, [X | Res]).


trump(X) :- trump(s(X)).
