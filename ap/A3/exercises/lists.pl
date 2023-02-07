concat([], SomeList, SomeList).
concat([Head | L1], L2, [Head | L3]) :-
  concat(L1, L2, L3).


reverse([], []).
reverse([H | T], Res) :-
  reverse(T, TRev), concat(TRev, [H], Res).


duplicate([], []).
duplicate([H | T], Res) :-
  duplicate(T, Tmp),
  concat([H, H], Tmp, Res).


palindrome(X) :-
  reverse(X, X).


sublist([], [_|_]). % the empty list is a sublist of any list.

sublist([X | Xt], [X | Yt]) :-
  sublist(Xt, Yt), !.

sublist([X | Xt], [_ | Yt]) :-
  sublist([X | Xt], Yt).
  

