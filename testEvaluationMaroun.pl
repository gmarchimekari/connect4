initialize :-
    retractall(board(_)),
    asserta( board([
        ['o', 'x', 'o', 'x', 'x', 'o', 'o'],
        ['o', 'o', 'o', 'x', 'x', 'o', 'x'],
        ['x', 'x', 'x', 'o', 'x', 'x', 'o'],
        ['x', 'o', 'x', 'x', 'o', 'x', 'o'],
        ['e', 'x', 'o', 'o', 'o', 'x', 'o'],
        ['e', 'e', 'o', 'x', 'o', 'o', 'x']
        ])).

maximizing('x').
minimizing('o').

inverse_mark('x', 'o').
inverse_mark('o', 'x').

blank_mark('e').

evaluator(B, U) :-
    


run:-
    initialize,
    board(B),
    output_board,
    evaluator(B, U),
    write('U = '), write(U)
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


