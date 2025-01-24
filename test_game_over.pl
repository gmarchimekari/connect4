%defining the board

initialize :-
    retractall(board(_)),
    asserta(board([
            ['x', 'x', 'x', 'x', 'o', 'x', 'x'],
            ['x', 'x', 'x', 'o', 'o', 'x', 'o'],
            ['x', 'o', 'o', 'o', 'x', 'x', 'x'],
            ['x', 'x', 'o', 'x', 'x', 'x', 'x'],
            ['x', 'o', 'x', 'x', 'x', 'x', 'o'],
            ['e', 'e', 'e', 'e', 'e', 'e', 'e']
        ])).



opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').


player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').  

blank_mark('e').        %%% the mark used in an empty square



find_lowest_empty_square(B, Col, E, S) :-
    find_lowest_empty_square(B, Col, 1, E, S).

find_lowest_empty_square(Board, Col, Row, E, S) :-
    Row =< 6,  % Ensure we do not go beyond the last row
    nth1(Row, Board, RowList),  % Get the list representing the current row. rowList = Board[Row]
    nth1(Col, RowList, E),  % Check if the square in the given column is empty
    S = Row, !.

find_lowest_empty_square(B, Col, Row, E, S) :-
    Row < 6,  % Move to the next row if the current one is not empty
    NextRow is Row + 1,
    find_lowest_empty_square(B, Col, NextRow, E, S). 


% Check if the square at (Row, Col) is part of a winning sequence
check_win(Board, Row, Col, E) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Player),
    Player \= E,  % Ensure the square is not empty
    (check_horizontal(Board, Row, Col, Player), !;
     check_vertical(Board, Row, Col, Player), !;
     check_diagonal1(Board, Row, Col, Player), !;
     check_diagonal2(Board, Row, Col, Player), !).

% Check for a horizontal win
check_horizontal(Board, Row, Col, Player) :-
    findall(P, (between(1, 7, C), nth1(Row, Board, RowList), nth1(C, RowList, P)), Line),
    consecutive_four(Player, Line).

% Check for a vertical win
check_vertical(Board, Row, Col, Player) :-
    findall(P, (between(1, 6, R), nth1(R, Board, RowList), nth1(Col, RowList, P)), Line),
    consecutive_four(Player, Line).

% Check for a diagonal win (top-left to bottom-right)
check_diagonal1(Board, Row, Col, Player) :-
    findall(P, (between(-3, 3, Offset), R is Row + Offset, C is Col + Offset, nth1(R, Board, RowList), nth1(C, RowList, P)), Line),
    consecutive_four(Player, Line).

% Check for a diagonal win (bottom-left to top-right)
check_diagonal2(Board, Row, Col, Player) :-
    findall(P, (between(-3, 3, Offset), R is Row - Offset, C is Col + Offset, nth1(R, Board, RowList), nth1(C, RowList, P)), Line),
    consecutive_four(Player, Line).

% Check for four consecutive marks
consecutive_four(P, List) :-
    append(_, [P, P, P, P | _], List).



board_full(Board, E) :-
    findall(P, (nth1(Row, Board, RowList), nth1(Col, RowList, P), P = E), EmptySquares),
    EmptySquares = [].  % The board is full if there are no empty squares left


% Check if the game is over
game_over(P, Col, B) :-
    blank_mark(E),
    (board_full(B, E) -> true;  % If the board is full, the game is over
    (find_lowest_empty_square(B, Col, E, S),
    player_mark(P, M),
    S1 is S - 1,
    check_win(B, S1, Col, 'e'))).  % Check if the last player who played has won

run :-
    initialize,
    board(B),
    game_over(1, 7, B),
    write('Game over!').
