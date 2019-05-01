spot(a, 6, 10, culinary).
spot(b, 7, 11, culinary).
spot(c, 10, 11, historical).
spot(d, 12, 11, historical).
spot(e, 13, 18, culinary).
edge(a, b, 1).
edge(b, c, 2).
edge(c, d, 1).
edge(b, d, 4).
edge(c, e, 1).
edge(e, a, 10).
edge(e, d, 6).

path(X, Y, Z, Input) :-  edge(X,Y, Z), spot(X, _ , _, Input) , spot(Y, _, _, Input).

go :- read(Input),path(X, Y, Z, Input), write(X), nl, write(Y), nl, write(Z).
