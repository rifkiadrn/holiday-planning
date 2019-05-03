%% Input: culinary, historical

edge(home, a, 1).
edge(home, c, 2).
edge(home, b, 1).
edge(a, c, 3).
edge(b, c, 1).
edge(e, b, 1).
edge(e, d, 4).
edge(b, d, 3).
edge(c, d, 2).
edge(b, d, 3).

spot(a, 15, 16, 1,culinary).
spot(c, 10, 15, 2,culinary).
spot(b, 11, 15, 1,historical).
spot(d, 12, 16, 1,historical).
spot(e, 12, 15, 1,culinary).
