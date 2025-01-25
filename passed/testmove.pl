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



set_item(B, Col, V, B2) :-
    find_lowest_empty_square(B, Col, 'e', Row),
    set_item2(B, Col, Row, V, 1, B2).

set_item2([], _Col, _Row, _V, _A, []) :- !.

set_item2([H|T], Col, Row, V, A, [H2|T2]) :-
    A == Row,
    set_column_item(H, Col, V, H2),
    A1 is A + 1,
    set_item2(T, Col, Row, V, A1, T2).

set_item2([H|T], Col, Row, V, A, [H|T2]) :-
    A1 is A + 1,
    set_item2(T, Col, Row, V, A1, T2).

set_column_item([_|T], 1, V, [V|T]) :- !.
set_column_item([H|T], Col, V, [H|T2]) :-
    Col1 is Col - 1,
    set_column_item(T, Col1, V, T2).

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
    output_board,
    read(S),
    board(B),
    set_item(B, S, 'o', B2),
    retract(board(_)),
    asserta(board(B2)),
    output_board.