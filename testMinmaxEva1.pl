initialize :-
    retractall(board(_)),
    asserta( board([
        ['o', 'x', 'o', 'x', 'x', 'o', 'o'],
        ['o', 'o', 'o', 'x', 'x', 'o', 'x'],
        ['x', 'x', 'x', 'o', 'o', 'x', 'o'],
        ['x', 'o', 'x', 'x', 'o', 'x', 'o'],
        ['e', 'x', 'e', 'e', 'o', 'x', 'o'],
        ['e', 'e', 'e', 'e', 'e', 'e', 'x']
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
    minimax(0, B, 'x', S, U), nl,
    write('S = '), write(S), write(', U = '), write(U)
    .


random_int_1n(N, R) :- R is random(N) + 1.


bestEstimate(B, M, L, S, U) :-
    findall(U1-S1, (
        member(S1, L),
        move(B, S1, M, B2),
        evaluate_board(B2, U1)
    ), Scores),
    ( maximizing(M) -> max_member(U-S, Scores) ; min_member(U-S, Scores) ).

minimax(D, B, M, S, U) :-
    D2 is D + 1,
    moves(B, L),          %%% get the list of available moves
    !,
    (D2 < 5 -> best(D2, B, M, L, S, U) ; bestEstimate(B, M, L, S, U)),  %%% recursively determine the best available move or use bestEstimate
    !
    .

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax(D, B, M, S, U) :-
    utility(B, U)
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    move(B,S1,M,B2),             %%% apply the move to the board,
    inverse_mark(M,M2), 
    !,
    (utility(B2, U1), U1 \= 0 ->  %%% if the utility is not 0, use it directly
        (S = S1, U = U1),
        output_value(D,S,U)
    ;
        minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
        S = S1, !,
        output_value(D,S,U),
        !
    ).


% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    (utility(B2, U1), U1 \= 0 ->  %%% if the utility is not 0, use it directly
        best(D,B,M,T,S2,U2),     %%% determine the best move of the remaining moves,
        output_value(D,S1,U1),      
        better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    ;
        minimax(D,B2,M2,_S,UMinMax),  %%% otherwise, recursively search for the utility value of that move, continue en profondeur
        best(D,B,M,T,S2,U2),     %%% determine the best move of the remaining moves, continue en largeur
        output_value(D,S1,UMinMax),      
        better(D,M,S1,UMinMax,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    )
    .

%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,S1,U1,S2,U2,     S,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    S = S1,
    U = U1, 
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(100,R),               %%% then pick one of them at random
    better2(D,R,M,S1,U1,S2,U2,S,U),    
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-        %%% otherwise, second move is better
    S = S2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    R < 51,
    S = S1,
    U = U1, 
    !
    .

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    S = S2,
    U = U2,
    !
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

output_value(D,S,U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(D,S,U) :- 
    true
    .



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















% Retrieve all consecutive 4 cells in the board
find_consecutive_4(Lists, B) :-
    findall(Seq, (
        (horizontal(B, Seq);
         vertical(B, Seq);
         diagonal1(B, Seq);
         diagonal2(B, Seq)
         ),
        length(Seq, 4)  % Ensure sequences are of length 4
    ), Lists).

% Horizontal sequences
horizontal(Board, Seq) :-
    member(Row, Board),
    sublist(Row, Seq).

% Vertical sequences
vertical(Board, Seq) :-
    transpose(Board, TransposedBoard),
    horizontal(TransposedBoard, Seq).

% Diagonal sequences (top-left to bottom-right)
diagonal1(Board, Seq) :-
    length(Board, Rows),
    nth1(1, Board, FirstRow),
    length(FirstRow, Cols),
    between(1, Rows, Row),
    between(1, Cols, Col),
    RowEnd is Row + 3,
    ColEnd is Col + 3,
    RowEnd =< Rows,
    ColEnd =< Cols,
    get_diagonal(Board, Row, Col, 1, 1, Seq).

% Diagonal sequences (bottom-left to top-right)
diagonal2(Board, Seq) :-
    length(Board, Rows),
    nth1(1, Board, FirstRow),
    length(FirstRow, Cols),
    between(1, Rows, Row),
    between(1, Cols, Col),
    RowEnd is Row - 3,
    ColEnd is Col + 3,
    RowEnd >= 1,
    ColEnd =< Cols,
    get_diagonal(Board, Row, Col, -1, 1, Seq).

% Helper to get diagonal sequences
get_diagonal(Board, Row, Col, RowStep, ColStep, [Cell|Rest]) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Cell),
    NextRow is Row + RowStep,
    NextCol is Col + ColStep,
    get_diagonal(Board, NextRow, NextCol, RowStep, ColStep, Rest).
get_diagonal(_, _, _, _, _, []).

% Helper: Extract sublists of a given list
sublist(List, SubList) :-
    append(_, Suffix, List),
    append(SubList, _, Suffix).

% Helper: Transpose a matrix
transpose([], []).
transpose([[]|_], []).
transpose(Matrix, Transposed) :-
    Matrix = [Row|_],  % Get the first row
    length(Row, N),  % Find the number of columns
    transpose_helper(Matrix, 1, N, Transposed).

transpose_helper(_, Index, N, []) :- Index > N, !.
transpose_helper(Matrix, Index, N, [Column|Columns]) :-
    findall(Element, (
        member(Row, Matrix),
        nth1(Index, Row, Element)
    ), Column),
    NextIndex is Index + 1,
    transpose_helper(Matrix, NextIndex, N, Columns).

evaluate_board(Board, TotalScore) :-
    find_consecutive_4(Lists, Board),
    findall(Score, (
        member(Seq, Lists),
        evaluate_sequence(Seq, Score)
    ), Scores),
    sum_list(Scores, TotalScore).

evaluate_sequence(Seq, Score) :-
    count_marks(Seq, 'x', XCount),
    count_marks(Seq, 'o', OCount),
    ( OCount > 0, XCount > 0 -> Score = 0  % Nullify score if there are both 'x' and 'o' Marks
    ; XCount == 4 -> Score = 10000
    ; XCount == 3, OCount == 0 -> Score = 300
    ; XCount == 2, OCount == 0 -> Score = 100
    ; XCount == 1, OCount == 0 -> Score = 10
    ; OCount == 4 -> Score = -10000
    ; OCount == 3, XCount == 0 -> Score = -300
    ; OCount == 2, XCount == 0 -> Score = -100
    ; OCount == 1, XCount == 0 -> Score = -10
    ; Score = 0  % Default case
    ).

count_marks(List, Mark, Count) :-
    findall(Mark, member(Mark, List), Marks),
    length(Marks, Count).

