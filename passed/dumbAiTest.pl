%defining the board
initialize :-
    retractall(board(_)),
    asserta( board([
    ['x', 'x', 'x', 'x', 'x', 'o', 'e'],
    ['o', 'x', 'x', 'x', 'o', 'o', 'e'],
    ['o', 'o', 'x', 'x', 'o', 'o', 'e'],
    ['o', 'o', 'x', 'x', 'o', 'o', 'e'],
    ['o', 'o', 'x', 'x', 'o', 'o', 'e'],
    ['o', 'o', 'x', 'o', 'o', 'o', 'e']
])).
   
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


moves(B, L) :-
    findall(Col, (
        between(1, 7, Col),                   % Iterate through columns (1–7).
        find_lowest_empty_square(B, Col, 'e', S), % Check for valid (row, column).
        S \= 7
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

blank_mark('e'). 

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

run :-
    initialize,
    board(B),
    output_board,
    dumbAI(B, S),
    write('Selected column: '), write(S), nl
    .