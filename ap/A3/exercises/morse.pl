translate(a, [., -]).
translate(b, [-, ., ., .]).
translate(c, [-, ., -, .]).
translate(d, [-, ., .]).
translate(e, [.]).
translate(f, [., ., -, .]).
translate(g, [-, -, .]).
translate(h, [., ., ., .]).
translate(i, [., .]).
translate(j, [., -, -, -]).
translate(k, [-, ., -]).
translate(l, [., -, ., .]).
translate(m, [-, -]).
translate(n, [-, .]).
translate(o, [-, -, -]).
translate(p, [., -, -, .]).
translate(q, [-, -, ., -]).
translate(r, [., -, .]).
translate(s, [., ., .]).
translate(t, [-]).
translate(u, [., ., -]).
translate(v, [., ., ., -]).
translate(w, [., -, -]).
translate(x, [-, ., ., -]).
translate(y, [-, ., -, -]).
translate(z, [-, -, ., .]).

encode([], []).
encode([Head | Rest], [HeadMorse | RestMorse]) :-
  translate(Head, HeadMorse),
  encode(Rest, RestMorse).
