initialize :-
    retractall(board(_)),
    asserta( board([
        ['o', 'x', 'o', 'x', 'x', 'o', 'o'],
        ['o', 'o', 'o', 'x', 'x', 'o', 'x'],
        ['x', 'x', 'x', 'o', 'x', 'o', 'o'],
        ['x', 'o', 'x', 'x', 'o', 'x', 'o'],
        ['o', 'x', 'x', 'o', 'o', 'x', 'o'],
        ['e', 'e', 'o', 'x', 'o', 'o', 'x']
        ])).

maximizing('x').
minimizing('o').

inverse_mark('x', 'o').
inverse_mark('o', 'x').

utility(B,U) :-
    (between(1, 7, Col), 
    find_lowest_empty_square(B, Col, 'e', Row),
    RowNE is Row - 1,
    RowNE > 0,
    check_win(B, RowNE, Col, 'e'),
    nth1(RowNE, B, RowList), 
    nth1(Col, RowList, M),
    maximizing(M) -> U = 100000 ; fail),
    !
    .



utility(B,U) :-
    (between(1, 7, Col), 
    find_lowest_empty_square(B, Col, 'e', Row),
    RowNE is Row - 1,
    RowNE > 0,
    check_win(B, RowNE, Col, 'e'),
    nth1(RowNE, B, RowList), 
    nth1(Col, RowList, M),
    minimizing(M) -> U = (-100000) ; fail),
    !
    .

utility(B,U) :-
    U = 0
    .


blank_mark('e').

run:-
    initialize,
    board(B),
    output_board,
    utility(B, U),
    write(U)
    .


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
    append(Prefix, [P, P, P, P | _], List). % Find four consecutive Ps