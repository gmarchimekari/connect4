% Initialize the board
initialize :-
    retractall(board(_)),
    asserta(board([
        ['o', 'x', 'o', 'o', 'x', 'e', 'e'],
        ['x', 'x', 'o', 'x', 'e', 'e', 'e'],
        ['o', 'o', 'x', 'o', 'e', 'e', 'e'],
        ['x', 'x', 'e', 'e', 'e', 'e', 'e'],
        ['e', 'e', 'e', 'e', 'e', 'e', 'e'],
        ['e', 'e', 'e', 'e', 'e', 'e', 'e']
    ])).



    

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

evaluate_board_lineaire(Board, TotalScore) :-
    find_consecutive_4(Lists, Board),
    findall(Score, (
        member(Seq, Lists),
        evaluate_sequence_lineaire(Seq, Score)
    ), Scores),
    sum_list(Scores, TotalScore).

evaluate_board_lineaire_3inRow(Board, TotalScore) :-
    find_consecutive_4(Lists, Board),
    findall(Score, (
        member(Seq, Lists),
        evaluate_sequence_lineaire_3inRow(Seq, Score)
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

evaluate_sequence_lineaire(Seq, Score) :-
    count_marks(Seq, 'x', XCount),
    count_marks(Seq, 'o', OCount),
    ( OCount > 0, XCount > 0 -> Score = 0  % Nullify score if there are both 'x' and 'o' Marks
    ; XCount == 4 -> Score = 10000
    ; XCount == 3, OCount == 0 -> Score = 3
    ; XCount == 2, OCount == 0 -> Score = 2
    ; XCount == 1, OCount == 0 -> Score = 1
    ; OCount == 4 -> Score = -10000
    ; OCount == 3, XCount == 0 -> Score = -3
    ; OCount == 2, XCount == 0 -> Score = -2
    ; OCount == 1, XCount == 0 -> Score = -1
    ; Score = 0  % Default case
    ).

evaluate_sequence_lineaire_3inRow(Seq, Score) :-
    count_marks(Seq, 'x', XCount),
    count_marks(Seq, 'o', OCount),
    ( OCount > 0, XCount > 0 -> Score = 0  % Nullify score if there are both 'x' and 'o' Marks
    ; XCount == 4 -> Score = 10000
    ; XCount == 3, OCount == 0 -> Score = 1
    ; OCount == 4 -> Score = -10000
    ; OCount == 3, XCount == 0 -> Score = -1
    ; Score = 0  % Default case
    ).

count_marks(List, Mark, Count) :-
    findall(Mark, member(Mark, List), Marks),
    length(Marks, Count).








% Output the board
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
    output_rows(Rest).

output_row([]).
output_row([Square|Rest]) :-
    output_square(Square),
    write(' '),
    output_row(Rest).

output_square(E) :-
    blank_mark(E),
    write('.'), !.  %%% if square is empty, output a dot

output_square('x') :-
    ansi_format([fg(yellow)], 'x', []), !.  %%% print x in yellow

output_square('o') :-
    ansi_format([fg(red)], 'o', []), !.  %%% print o in red

output_square(M) :-
    write(M), !.  %%% if square is marked, output the mark

blank_mark('e').        %%% the mark used in an empty square

% Print the board and calculate score
run :-
    initialize,
    board(B),
    output_board,
    evaluate_board(B, S),
    write(S), nl.
