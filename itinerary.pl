/** spot/4 is the predicate for a spot object
* first param is the spot name(unique), second param is the opening time,
* third param is the closing time, and fourth param is the time spent,
* and the fifth param is the category
*/

:- dynamic currentSolution/2.

spot(a, 8, 16, 1, culinary).
spot(b, 8, 16, 1, culinary).
spot(c, 8, 16, 1, historical).
spot(d, 8, 16, 1, culinary).
spot(e, 8, 18, 2, historical).
spot(f, 14, 18, 1, culinary).

/** edge/3 is the predicate to simulate the weighted graph
* first param is the source node, second param is the destination node (vice versa/undirected graph),
* third param is the weight \/ edge of the graph representing the time needed to journey to the end node
*/

edge(home, a, 1).
edge(home, c, 1).
edge(a, b, 1).
edge(c, d, 1).
edge(d, e, 1).



/**
* AM PM checker
*/
ampm(Hour, NewHour, MeridiemStatus):- Hour >= 12, MeridiemStatus = 'PM', convertHour(Hour, NewHour).
ampm(Hour, NewHour, MeridiemStatus):- Hour < 12, MeridiemStatus = 'AM', convertHour(Hour, NewHour).

convertHour(12, 12).
convertHour(Hour, NewHour):- Hour > 12, NewHour is Hour - 12, !.
convertHour(Hour, NewHour):- NewHour = Hour.


/**
* check if visit is feasible
*/
visit(Destination, Weight, TimeSpent, CurrentTime, UpdatedWithVisitTime):- spot(Destination, OpeningTime, ClosingTime, TimeSpent, _),
 UpdatedWithJourneyTime is CurrentTime + Weight, UpdatedWithJourneyTime >= OpeningTime, 
 UpdatedWithVisitTime is UpdatedWithJourneyTime + TimeSpent, UpdatedWithVisitTime =< ClosingTime,
 UpdatedWithVisitTime =< 18.

/**
* traverse the graph
*/
path(X, Y, Categories, [Z|Ys], CurrentTime, Vertices, Length):- edge(X, Z, Weight), spot(Z, _, _, TimeSpent, Category), member(Category, Categories),\+ member(Z,Vertices), 
 visit(Z, Weight, TimeSpent, CurrentTime, UpdatedTime), path(Z, Y, Categories, Ys, UpdatedTime, [Z|Vertices], Length).
path(X, _, Categories, [], _, Vertices, Length):- spot(X, _, _, _, Category), member(Category, Categories), list_length(Vertices, Length), reverse(Vertices, Res), insertToSolution(Length, Res).


/**
* print the route of the solutions
*/
printPath([Source, Destination|Vertices], Length, CurrentTime):- edge(Source, Destination, Weight), spot(Destination, _, _, TimeSpent, _), UpdatedWithJourneyTime is CurrentTime + Weight,
 ampm(CurrentTime, ConvertedCurrentTime, MeridiemStatus), ampm(UpdatedWithJourneyTime, ConvertedUpdatedWithJourneyTime, MeridiemStatus2),
 UpdatedWithVisitTime is UpdatedWithJourneyTime + TimeSpent, write(ConvertedCurrentTime), write(MeridiemStatus), write(' - '),  write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),
 write(': Journey to Vacation Spot '), write(Destination), nl, ampm(UpdatedWithVisitTime, ConvertedUpdatedWithVisitTime, MeridiemStatus3),
 write(ConvertedUpdatedWithJourneyTime), write(MeridiemStatus2),write(' - '), write(ConvertedUpdatedWithVisitTime), write(MeridiemStatus3),
 write(': Vacation Spot '), write(Destination), nl, !, printPath([Destination|Vertices], Length, UpdatedWithVisitTime).
printPath([_|[]], Length, _):- write('Total: '), write(Length),nl,nl.


printDB:- \+ currentSolution(_,_).
printDB:- currentSolution(Length,Vertices), printPath([home|Vertices], Length, 8), retract(currentSolution(Length,Vertices)),printDB.

/**
* append  with difference list
*/
append_dl(A-B, B-C, A-C).

append_dl_helper([],B,Res):- append_dl(L-L, [B|T]-T, Res-[]).
append_dl_helper(Vertices, B, Res):- append_dl([Vertices|B] - B, [B|T]-T, Res-[]).

/**
* insert current best solution to DB
*/
insertToSolution(Length, Vertices):- currentSolution(CurrentHighest, _), CurrentHighest = Length, assert(currentSolution(Length, Vertices)).
insertToSolution(Length, Vertices):- currentSolution(CurrentHighest, _), CurrentHighest < Length, retractall(currentSolution(_,_)), assert(currentSolution(Length, Vertices)).
insertToSolution(Length, Vertices):- \+ currentSolution(_, _), assert(currentSolution(Length, Vertices)).

/**
* Get length of list
*/
list_length([], 0).
list_length([_|T], Length):-  list_length(T, Length2), Length is 1 + Length2.

/**
* starting the program with predicate go/0
*/
go :- retractall(currentSolution(_,_)), write('Input: '), nl, write('Category:'), read(Category), string_lower(Category, CategoryToLower),
 atomic_list_concat(Categories,',', CategoryToLower), findall([Length,SPOT], path(home, _, Categories, SPOT, 8, [], Length), _), printDB.
