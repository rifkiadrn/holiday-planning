/** spot/4 is the predicate for a spot object
* first param is the spot name(unique), second param is the opening time,
* third param is the closing time, and fourth param is the category
*/
spot(a, 6, 10, culinary).
spot(b, 7, 11, culinary).
spot(c, 10, 11, culinary).
spot(d, 12, 11, culinary).
spot(e, 13, 18, culinary).
spot(f, 14, 15, culinary).

/** edge/3 is the predicate to simulate the weighted graph
* first param is the source node, second param is the destination node (vice versa/undirected graph),
* third param is the weight \/ edge of the graph representing the time needed to journey to the end node
*/
edge(a, b, 1).
edge(b, c, 2).
edge(c, d, 1).
edge(b, d, 4).
edge(c, e, 1).
edge(e, f, 6).

path(X, Y, Res, Input, [X|Ys]):- spot(X, _, _, Input), edge(X, Z, Weight), spot(Z, _, _, Input), path(Z, Y, Weight2, Input, Ys), Res is Weight + Weight2.
path(X, X, 0, Input, [X]):- spot(X, _, _, Input).

%starting the program with predicate go/0
% go :- read(Input),path(X, Y, Res, Input), write(X), nl, write(Y), nl, write(Res).
