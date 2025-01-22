board([
    ['r', 'r', 'r', 'r', 'r', 'e', 'e'],
    ['e', 'r', 'r', 'r', 'e', 'e', 'e'],
    ['e', 'e', 'r', 'r', 'e', 'e', 'e'],
    ['e', 'e', 'r', 'r', 'e', 'e', 'e'],
    ['e', 'e', 'r', 'r', 'e', 'e', 'e'],
    ['e', 'e', 'r', 'e', 'e', 'e', 'e']
]).
   
find_lowest_empty_square(B, Col, S) :-
    find_lowest_empty_square(B, Col, 1, S).

find_lowest_empty_square(Board, Col, Row, S) :-
    Row =< 6,  % Ensure we do not go beyond the last row
    nth1(Row, Board, RowList),  % Get the list representing the current row
    nth1(Col, RowList, 'e'),  % Check if the square in the given column is empty
    S = Row, !.

find_lowest_empty_square(B, Col, Row, S) :-
    Row < 6,  % Move to the next row if the current one is not empty
    NextRow is Row + 1,
    find_lowest_empty_square(B, Col, NextRow, S).  

moves(B, L) :-
    findall(Col, (
        between(1, 7, Col),                   % Iterate through columns (1–7).
        find_lowest_empty_square(B, Col, S) % Check for valid (row, column).
    ), L),
    L \= []
    .

random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

dumbAI(B,S) :-
    moves(B, L),
    length(L, N),
    random_int_1n(N, Random),
    nth1(Random, L, S)
    .