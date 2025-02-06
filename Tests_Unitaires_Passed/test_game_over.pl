%defining the board

initialize :-
    retractall(board(_)),
    asserta( board([
        ['o', 'o', 'o', 'x', 'e', 'e', 'e'],
        ['o', 'o', 'x', 'e', 'e', 'e', 'e'],
        ['o', 'o', 'o', 'e', 'e', 'e', 'e'],
        ['x', 'o', 'e', 'e', 'e', 'e', 'e'],
        ['x', 'e', 'e', 'e', 'e', 'e', 'e'],
        ['x', 'e', 'e', 'e', 'e', 'e', 'e']
        ])).

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').

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
    find_lowest_empty_square(B, Col, NextRow, E, S), !.

find_lowest_empty_square(B, Col, Row, E, S) :-
    S = 7. 


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
game_over(P, Col, B, W) :-
    blank_mark(E),
    (find_lowest_empty_square(B, Col, E, S),
    player_mark(P, M),
    S1 is S - 1,
    (check_win(B, S1, Col, 'e') -> W = P;  % If the last player who played has won, set W to the player
    (board_full(B, E) -> W = 0;  % If the board is full and no one has won, set W to 0
    W = -1))).  % If the board is not full and no one has won, set W to -1

output_board :-
    board(B),
    reverse(B, RB),
    output_rows(RB).

output_rows([]).
output_rows([Row|Rest]) :-
    write('\033[34m|'),  % Left border in blue
    write('\033[0m '),   % Reset color for the row
    output_row(Row),
    write('\033[34m|'),  % Left border in blue
    write('\033[0m '),   % Reset color for the row
    nl,
    output_rows(Rest)
    .

output_row([]).
output_row([Square|Rest]) :-
    output_square(Square),
    write(' '),
    output_row(Rest).

output_square(E) :-
    blank_mark(E),
    write('.'), !.  %%% if square is empty, output a dot

output_square('x') :-
    ansi_format([fg(yellow)], 'x', []), !.  %%% print x in blue

output_square('o') :-
    ansi_format([fg(red)], 'o', []), !.  %%% print o in red

output_square(M) :-
    write(M), !.  %%% if square is marked, output the mark

blank_mark('e').        %%% the mark used in an empty square

run :-
    initialize,
    board(B),
    output_board,
    game_over(2, 2, B, W),
    write(W), nl,
    (W = 1 -> write('Player X won'), nl;
     W = 2 -> write('Player O won'), nl;
     W = 0 -> write('It\'s a draw'), nl;
     write('Game continues'), nl),
    write('Game over!').
