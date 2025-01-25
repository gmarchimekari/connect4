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
    find_lowest_empty_square(B, 3, S),
    write(S).

