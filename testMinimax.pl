initialize :-
    retractall(board(_)),
    asserta( board([
        ['o', 'x', 'o', 'x', 'x', 'o', 'o'],
        ['o', 'o', 'o', 'x', 'x', 'o', 'x'],
        ['x', 'x', 'x', 'o', 'x', 'x', 'o'],
        ['x', 'o', 'x', 'x', 'o', 'x', 'o'],
        ['e', 'x', 'x', 'o', 'o', 'x', 'o'],
        ['e', 'e', 'o', 'x', 'o', 'o', 'x']
        ])).

maximizing('x').
minimizing('o').

utility(B,U) :-
    (between(1, 7, Col), 
    find_lowest_empty_square(B, Col, 'e', Row),
    RowNE is Row - 1,
    RowNE > 0,
    check_win(B, RowNE, Col, 'e'),
    nth1(RowNE, B, RowList), 
    nth1(Col, RowList, M),
    maximizing(M) -> U = 1 ; fail),
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
    minimizing(M) -> U = (-1) ; fail),
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
    %minimax(0, B, 'x', S, U),
    write(U)
    .

switch_player('x', 'o').
switch_player('o', 'x').

random_int_1n(N, R) :- R is random(N) + 1.


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




moves(B, L) :-
    findall(Col, (
        between(1, 7, Col),                   % Iterate through columns (1–7).
        find_lowest_empty_square(B, Col, 'e', S), % Check for valid (row, column).
        S \= 7
    ), L),
    L \= []
    .

is_empty(Board) :-
    forall(member(Row, Board), forall(member(Cell, Row), Cell = 0)).

% Base case: If the board is empty, pick a random column
minimax(D, Board, M, S, U) :-
    is_empty(Board),                     
    random_int_1n(7, S),                 
    !.

% If valid moves exist, recursively determine the best move
minimax(D, Board, M, S, U) :-
    D2 is D + 1,
    moves(Board, Moves),  
    Moves \= [],              
    best(D2, Board, M, Moves, S, U),  
    !.

% If no moves are available, return the utility value
minimax(D, Board, M, S, U) :-
    utility(Board, U),
    !.

%--------------------------------------
% Finding the Best Move
%--------------------------------------

% If only one move left
best(D, Board, M, [Col], S, U) :-
    move(Board, Col, M, NewBoard),   
    switch_player(M, Opponent),    
    minimax(D, NewBoard, Opponent, _S, U),  
    S = Col,
    !.

% If multiple moves exist, evaluate each recursively
best(D, Board, M, [Col|Rest], S, U) :-
    move(Board, Col, M, NewBoard),  
    switch_player(M, Opponent),
    minimax(D, NewBoard, Opponent, _S, U1),  
    best(D, Board, M, Rest, S2, U2),  
    better(D, M, Col, U1, S2, U2, S, U).  

%--------------------------------------
% Choosing the Best Move
%--------------------------------------

better(D, max, S1, U1, S2, U2, S, U) :-
    U1 > U2, S = S1, U = U1, !.
better(D, min, S1, U1, S2, U2, S, U) :-
    U1 < U2, S = S1, U = U1, !.
better(_, _, S1, U1, S2, U2, S, U) :-
    U1 == U2, random_int_1n(10, R), better2(D, R, S1, U1, S2, U2, S, U), !.
better(_, _, _, _, S2, U2, S, U) :- S = S2, U = U2, !.

better2(_, R, S1, U1, S2, U2, S, U) :- R < 6, S = S1, U = U1, !.
better2(_, _, S1, U1, S2, U2, S, U) :- S = S2, U = U2, !.

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



move(B, Col, V, B2) :-
    set_item(B, Col, V, B2)
    .


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
