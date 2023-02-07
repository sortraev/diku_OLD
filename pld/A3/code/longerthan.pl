list(nil).
list(cons(_, As)) :- list(As).


% if both L1 and L2 have a tail, then L1 is longer than T2 if T1's tail is
% longer than T2's tail.
longerthan(cons(_, T1), cons(_, T2)) :- longerthan(T1, T2).

% if L1 has a tail and L2 is nil, then L1 is longer than L2. however, we must
% also assert that L1 is, in fact, a well-formed list.
longerthan(cons(_, T1), nil) :- list(T1).



% for testing purposes.
my_list(cons(1, cons(3, cons(3, cons(7, nil))))).

my_other_list(cons(4, cons(2, nil))).

my_invalid_list(cons(57, cons(0, cons(-42, cons(9001, cons(99, oops)))))).
