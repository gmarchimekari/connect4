%defining the board
initialize :-
    retractall(board(_)),
    asserta( board([
        ['x', 'o', 'x', 'o', 'x', 'o', 'x'],
        ['o', 'x', 'o', 'x', 'o', 'x', 'o'],
        ['x', 'o', 'x', 'o', 'x', 'o', 'x'],
        ['o', 'x', 'o', 'x', 'o', 'x', 'o'],
        ['x', 'o', 'x', 'o', 'x', 'o', 'x'],
        ['o', 'x', 'o', 'x', 'o', 'x', 'o']
    ])). 


board_full(Board, E) :-
    findall(P, (nth1(Row, Board, RowList), nth1(Col, RowList, P), P = E), EmptySquares),
    EmptySquares = [].  % The board is full if there are no empty squares left

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
    board_full(B, 'e').

