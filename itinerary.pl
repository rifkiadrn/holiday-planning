/** spot/4 is the predicate for a spot object
* first param is the spot name(unique), second param is the opening time,
* third param is the closing time, and fourth param is the category
*/

:- dynamic totalVisit/1.

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

edge(home, a, 1).
edge(home, c, 3).
edge(home, e, 5).
edge(a, b, 1).
edge(b, c, 2).
edge(c, d, 1).
edge(b, d, 1).
edge(c, e, 1).
edge(e, f, 2).


/**
* print visited spot
*/
visit(Destination,Weight, CurrentTime):- write(CurrentTime), write(' - '), UpdatedWithJourneyTime is CurrentTime + Weight, write(UpdatedWithJourneyTime), write(': Journey to Vacation Spot '), write(Destination), nl, UpdatedWithVisitTime is UpdatedWithJourneyTime + 1, write(UpdatedWithJourneyTime), write(' - '), write(UpdatedWithVisitTime), write(': Vacation Spot '), write(Destination), nl. 

/**
* update total visit in db
*/
incrementTotalVisit:- \+ totalVisit(_), !, assert(totalVisit(1)).
incrementTotalVisit:- totalVisit(X), !, NewX is X + 1, retract(totalVisit(_)), assert(totalVisit(NewX)).

/**
* traverse the graph
*/
path(X, Y, Input, [Z|Ys], CurrentTime):- edge(X, Z, Weight), spot(Z, _, _, Input), visit(Z, Weight, CurrentTime), UpdatedTime is CurrentTime + 1 + Weight, incrementTotalVisit, path(Z, Y, Input, Ys, UpdatedTime).
path(X, _, Input, [], _):- spot(X, _, _, Input).

%starting the program with predicate go/0
go :- read(Input), retractall(totalVisit(_)), path(home, _, Input, SPOT, 8), write(SPOT), write('Total: '), totalVisit(Total), write(Total).
