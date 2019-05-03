/** spot/4 is the predicate for a spot object
* first param is the spot name(unique), second param is the opening time,
* third param is the closing time, and fourth param is the time spent,
* and the fifth param is the category
*/

:- dynamic currentSolution/2.

spot(a, 6, 10, 1, culinary).
spot(b, 8, 11, 1, culinary).
spot(c, 10, 14, 1, historical).
spot(d, 12, 16, 3, culinary).
spot(e, 13, 18, 2, historical).
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
visit(Destination, Weight, TimeSpent, CurrentTime, UpdatedWithVisitTime):- spot(Destination, OpeningTime, ClosingTime, TimeSpent, _),
 UpdatedWithJourneyTime is CurrentTime + Weight, UpdatedWithJourneyTime >= OpeningTime, UpdatedWithJourneyTime =< ClosingTime,
 UpdatedWithVisitTime is UpdatedWithJourneyTime + TimeSpent, UpdatedWithVisitTime =< 18,
 ampm(CurrentTime, ConvertedCurrentTime, MeridiemStatus), ampm(UpdatedWithJourneyTime, ConvertedUpdatedWithJourneyTime, MeridiemStatus2),
 write(ConvertedCurrentTime), write(MeridiemStatus), write(' - '),  write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),
 write(': Journey to Vacation Spot '), write(Destination), nl, ampm(UpdatedWithVisitTime, ConvertedUpdatedWithVisitTime, MeridiemStatus3),
 write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),write(' - '), write(ConvertedUpdatedWithVisitTime), write(MeridiemStatus3),
 write(': Vacation Spot '), write(Destination), nl.
*/

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
*
incrementTotalVisit:- \+ totalVisit(_), !, assert(totalVisit(1)).
incrementTotalVisit:- totalVisit(X), !, NewX is X + 1, retract(totalVisit(_)), assert(totalVisit(NewX)).
*/
/**
* traverse the graph
*/
/**path(X, Y, Category, [Z|Ys], CurrentTime, Vertices):- \+member(Z,Vertices), edge(X, Z, Weight), spot(Z, _, _, TimeSpent, Category),
* visit(Z, Weight, TimeSpent, CurrentTime, UpdatedTime), incrementTotalVisit, path(Z, Y, Category, Ys, UpdatedTime).
*path(X, X, Category, [], _):- spot(X, _, _, _, Category), printTotal.
*/


visit(Destination, Weight, TimeSpent, CurrentTime, UpdatedWithVisitTime):- spot(Destination, OpeningTime, ClosingTime, TimeSpent, _),
 UpdatedWithJourneyTime is CurrentTime + Weight, UpdatedWithJourneyTime >= OpeningTime, UpdatedWithJourneyTime =< ClosingTime,
 UpdatedWithVisitTime is UpdatedWithJourneyTime + TimeSpent, UpdatedWithVisitTime =< 18.


path(X, Y, Categories, [Z|Ys], CurrentTime, Vertices, Length):- edge(X, Z, Weight), spot(Z, _, _, TimeSpent, Category), member(Category, Categories),\+ member(Z,Vertices), 
 visit(Z, Weight, TimeSpent, CurrentTime, UpdatedTime), path(Z, Y, Categories, Ys, UpdatedTime, [Z|Vertices], Length).
path(X, _, Categories, [], _, Vertices, Length):- spot(X, _, _, _, Category), member(Category, Categories), list_length(Vertices, Length), insertToSolution(Length, Vertices).

printPath([Source, Destination|Vertices], Length, CurrentTime):- edge(Source, Destination, Weight), spot(Destination, OpeningTime, ClosingTime, TimeSpent, _), UpdatedWithJourneyTime is CurrentTime + Weight,
 ampm(CurrentTime, ConvertedCurrentTime, MeridiemStatus), ampm(UpdatedWithJourneyTime, ConvertedUpdatedWithJourneyTime, MeridiemStatus2),
 UpdatedWithVisitTime is UpdatedWithJourneyTime + TimeSpent, write(ConvertedCurrentTime), write(MeridiemStatus), write(' - '),  write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),
 write(': Journey to Vacation Spot '), write(Destination), nl, ampm(UpdatedWithVisitTime, ConvertedUpdatedWithVisitTime, MeridiemStatus3),
 write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),write(' - '), write(ConvertedUpdatedWithVisitTime), write(MeridiemStatus3),
 write(': Vacation Spot '), write(Destination), nl, !, printPath([Destination|Vertices], Length, UpdatedWithVisitTime).
printPath([Destination|[]], Length, CurrentTime):- write('Total: '), write(Length).

printPathHelper([H,T|_]):- printPath([home|T], H, 8).

insertToSolution(Length, Vertices):- currentSolution(CurrentHighest, CurrentHighestVertices), CurrentHighest =< Length, assert(currentSolution(Length, Vertices)).
insertToSolution(Length, Vertices):- \+ currentSolution(_, _), assert(currentSolution(Length, Vertices)).

list_length([], 0).
list_length([H|T], Length):-  list_length(T, Length2), Length is 1 + Length2.

%starting the program with predicate go/0
go :- retractall(currentSolution(_,_)), write('Input: '), nl, write('Category:'), read(Category), string_lower(Category, CategoryToLower),
 atomic_list_concat(Categories,',', CategoryToLower), findall([Length,SPOT], path(home, _, Categories, SPOT, 8, [], Length), [H|T]), currentSolution(Length, Vertices), write(H), nl, printPathHelper(H).
