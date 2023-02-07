taller(fedejeppe, malthe).
taller(malthe, anders).
taller(anders, nyejeppe).
taller(nyejeppe, fedejeppe).

is_taller(X, Y) :- taller(X, Y).
is_taller(X, Y) :- taller(X, Z), is_taller(Z, Y).
