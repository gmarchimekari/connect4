%defining the board
initialize :-
    retractall(board(_)),
    asserta( board([
        ['x', 'x', 'x', 'x', 'e', 'e', 'e'],
        ['e', 'x', 'x', 'x', 'e', 'e', 'e'],
        ['e', 'e', 'x', 'x', 'e', 'e', 'e'],
        ['e', 'e', 'x', 'x', 'e', 'e', 'e'],
        ['e', 'e', 'x', 'x', 'e', 'e', 'e'],
        ['e', 'e', 'x', 'e', 'e', 'e', 'e']
    ])). 

find_lowest_empty_square(B, Col, S) :-
    find_lowest_empty_square(B, Col, 1, S).

find_lowest_empty_square(Board, Col, Row, S) :-
    Row =< 6,  % Ensure we do not go beyond the last row
    nth1(Row, Board, RowList),  % Get the list representing the current row. rowList = Board[Row]
    nth1(Col, RowList, 'e'),  % Check if the square in the given column is empty
    S = Row, !.

find_lowest_empty_square(B, Col, Row, S) :-
    Row < 6,  % Move to the next row if the current one is not empty
    NextRow is Row + 1,
    find_lowest_empty_square(B, Col, NextRow, S).  

run :-
    initialize,
    board(B),
    find_lowest_empty_square(B, 4, S),
    write(S).

