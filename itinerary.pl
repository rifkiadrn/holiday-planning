/** spot/4 is the predicate for a spot object
* first param is the spot name(unique), second param is the opening time,
* third param is the closing time, and fourth param is the time spent,
* and the fifth param is the category
*/

:- dynamic totalVisit/1.

spot(a, 6, 10, 1, culinary).
spot(b, 8, 11, 1, culinary).
spot(c, 10, 14, 1, culinary).
spot(d, 12, 16, 3, culinary).
spot(e, 13, 18, 2, culinary).
spot(f, 14, 18, 1, culinary).

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
visit(Destination, Weight, TimeSpent, CurrentTime, UpdatedWithVisitTime):- spot(Destination, OpeningTime, ClosingTime, TimeSpent, _),
 UpdatedWithJourneyTime is CurrentTime + Weight, UpdatedWithJourneyTime >= OpeningTime, UpdatedWithJourneyTime =< ClosingTime,
 UpdatedWithVisitTime is UpdatedWithJourneyTime + TimeSpent, UpdatedWithVisitTime =< 18,
 ampm(CurrentTime, ConvertedCurrentTime, MeridiemStatus), ampm(UpdatedWithJourneyTime, ConvertedUpdatedWithJourneyTime, MeridiemStatus2),
 write(ConvertedCurrentTime), write(MeridiemStatus), write(' - '),  write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),
 write(': Journey to Vacation Spot '), write(Destination), nl, ampm(UpdatedWithVisitTime, ConvertedUpdatedWithVisitTime, MeridiemStatus3),
 write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),write(' - '), write(ConvertedUpdatedWithVisitTime), write(MeridiemStatus3),
 write(': Vacation Spot '), write(Destination), nl.

/**
* AM PM checker
*/
ampm(Hour, NewHour, MeridiemStatus):- Hour >= 12, MeridiemStatus = 'PM', convertHour(Hour, NewHour).
ampm(Hour, NewHour, MeridiemStatus):- Hour < 12, MeridiemStatus = 'AM', convertHour(Hour, NewHour).

convertHour(12, 12).
convertHour(Hour, NewHour):- Hour > 12, NewHour is Hour - 12, !.
convertHour(Hour, NewHour):- NewHour = Hour.


/**
* update total visit in db
*/
incrementTotalVisit:- \+ totalVisit(_), !, assert(totalVisit(1)).
incrementTotalVisit:- totalVisit(X), !, NewX is X + 1, retract(totalVisit(_)), assert(totalVisit(NewX)).

/**
* traverse the graph
*/
path(X, Y, Category, [Z|Ys], CurrentTime):- edge(X, Z, Weight), spot(Z, _, _, TimeSpent, Category),
 visit(Z, Weight, TimeSpent, CurrentTime, UpdatedTime), incrementTotalVisit, path(Z, Y, Category, Ys, UpdatedTime).
path(X, X, Category, [], _):- spot(X, _, _, _, Category), printTotal.


printTotal:- write('Total: '), totalVisit(Total), write(Total).
%starting the program with predicate go/0
go :- read(Category), string_lower(Category, CategoryToLower), retractall(totalVisit(_)), path(home, _, CategoryToLower, SPOT, 8), write(SPOT).
