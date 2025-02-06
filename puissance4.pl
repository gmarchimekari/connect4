%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A Prolog Implementation of Connect Four
%%% using the minimax strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Single letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
A - an accumulator
H - the head of a list
T - the tail of a list

For this implementation, these single letter variables represent:

P - a player number (1 or 2)
B - the board (a 42 item list representing a 6x7 matrix)
    each "square" on the board can contain one of 3 values: x, o, or e (for empty)
S - the number of a square on the board (1 - 42)
M - a mark on a square (x or o)
E - the mark used to represent an empty square ('e').
U - the utility value of a board position
R - a random number
D - the depth of the minimax search tree (for outputting utility values, and for debugging)

Variables with a numeric suffix represent a variable based on another variable.
(e.g. B2 is a new board position based on B)

For predicates, the last variable is usually the "return" value.
(e.g. opponent_mark(P,M), returns the opposing mark in variable M)

Predicates with a numeric suffix represent a "nested" predicate.

e.g. myrule2(...) is meant to be called from myrule(...) 
     and myrule3(...) is meant to be called from myrule2(...)

There are only two assertions that are used in this implementation

asserta( board(B) ) - the current board 
asserta( player(P, Type, Level) ) - indicates which players are human/computer. When Type is human level is 0
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position

corner_square(1, 7).    %%% map corner squares to board squares
corner_square(6, 7).
corner_square(7, 6).
corner_square(7, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run :-
    hello,          %%% Display welcome message, initialize game

    play(1, W),        %%% Play the game starting with player 1

    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
%    cls,
    nl,
    nl,
    nl,
    write('Welcome to Connect 4.'), nl,
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
    asserta( board([[E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E], 
                    [E,E,E,E,E,E,E]]) )  %%% create a blank board
    .

goodbye :-
    board(B),
    nl,
    output_board(B),
    nl,

    (between(1, 7, Col), 
    find_lowest_empty_square(B, Col, 'e', Row),
    RowNE is Row - 1,
    RowNE > 0,
    check_win(B, RowNE, Col, 'e') -> 
        (player_mark(P, M), 
        (M == 'x' -> W = 1 ; W = 2)) ; 
        W = -1),

    (W == 1 -> write('Player X won'), nl;
     W == 2 -> write('Player O won'), nl;
     W == 0 -> write('It\'s a draw'), nl;
     write('Game continues'), nl),
    
    retract(board(_)),
    retract(player(_,_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

read_play_again(V) :-
    nl,
    nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter Y or N.'),
    read_play_again(V)
    .

read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :- 

    write('Level of Difficulty (1, 2, 3 or 4) of player 1 (X)? '), nl,
    read(N1),
    write('Level of Difficulty (1, 2, 3 or 4) of player 2 (O)? '), nl,
    read(N2),
    asserta( player(1, computer, N1) ),
    asserta( player(2, computer, N2) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human, 0) ),
    asserta( player(2, human, 0) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .

human_playing(M) :- 
    (M == 'x' ; M == 'X'),

    write('Level of Difficulty (1, 2, 3 or 4) of player 2 (O)? '), nl,
    read(N2),

    asserta( player(1, human, 0) ),
    asserta( player(2, computer, N2) ), !
    .

human_playing(M) :- 
    (M == 'o' ; M == 'O'),

    write('Level of Difficulty (1, 2, 3 or 4) of player 1 (X)? '), nl,
    read(N1),

    asserta( player(1, computer, N1) ),
    asserta( player(2, human, 0) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .

play(P, W) :-
    board(B), !,
    output_board(B), !,
    make_move(P, B, B2, Col), !,
    game_over(P, Col, B2, W), !,  % Check if the game is over

    (W = 1 -> write('Player X won'), nl;
     W = 2 -> write('Player O won'), nl;
     W = 0 -> write('It\'s a draw'), nl;
     write('Game continues'), nl),

    (W = -1 ->
        next_player(P, P2), !,
        play(P2, W); % Continue with the next player if game is not over
        true). % Otherwise, stop the game


%.......................................
% find lowest empty square
%.......................................

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

%.......................................
% win
%.......................................
% 
% determines if a player has won the game

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
    append(Prefix, [P, P, P, P | _], List). % Find four consecutive P's

%.......................................
% check if the board is full
%.......................................

board_full(Board, E) :-
    findall(P, (nth1(Row, Board, RowList), nth1(Col, RowList, P), P = E), EmptySquares),
    EmptySquares = [].  % The board is full if there are no empty squares left

%.......................................
% move
%.......................................
% applies a move on the given board
% (put mark M in square S on board B and return the resulting board B2)
%

move(B, Col, V, B2) :-
    set_item(B, Col, V, B2), !
    .


%.......................................
% game_over
%.......................................
% determines when the game is over
%

% Check if the game is over
game_over(P, Col, B, W) :-
    blank_mark(E),
    (find_lowest_empty_square(B, Col, E, S),
    player_mark(P, M),
    S1 is S - 1,
    (check_win(B, S1, Col, 'e') -> W = P;  % If the last player who played has won, set W to the player
    (board_full(B, E) -> W = 0;  % If the board is full and no one has won, set W to 0
    W = -1))).  % If the board is not full and no one has won, set W to -1



%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%

make_move(P, B, B2, Col) :-
    player(P, Type, AI),

    make_move2(Type, AI, P, B, B2, Col),

    retractall( board(_) ),
    asserta( board(B2) )
    .

make_move2(human, AI, P, B, B2, Col) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(S),

    Col is S,

    blank_mark(E),
    player_mark(P, M),
    move(B, Col, M, B2), !
    .

make_move2(human, AI, P, B, B2) :-
    nl,
    nl,
    write('Please select a column.'),
    make_move2(human,AI,P,B,B2)
    .

make_move2(computer, AI, P, B, B2, Col) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    blank_mark(E),
    (AI == 1 -> dumbAI(B, S) ; minimax(0, AI, B, M, S, U)),
    Col is S,
    move(B, Col, M, B2),

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(S),
    write('.'),
    nl, !.


%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(B, L) :-
    findall(Col, (
        between(1, 7, Col),                   % Iterate through columns (1–7).
        find_lowest_empty_square(B, Col, 'e', S), % Check for valid (row, column).
        S \= 7
    ), L),
    L \= []
    .

%.......................................
% utility
%.......................................
% determines the value of a given board position
%
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
%.......................................
% Dumb AI algorithm
%.......................................
% The algorithm chooses a random square from the list of available moves.

dumbAI(B,S) :-
    moves(B, L),
    length(L, N),
    random_int_1n(N, Random),
    nth1(Random, L, S)
    .

%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree 
% by simply selecting a random square.

is_board_empty(B, E) :-
    findall(S, (between(1, 7, Col), find_lowest_empty_square(B, Col, E, S)), Squares),
    forall(member(S, Squares), S = 1).


minimax(D, AI, B,M,S,U) :-   
    blank_mark(E),
    is_board_empty(B, E),
    random_int_1n(7,S),
    !
    .

bestEstimate(B, AILvl, M, L, S, U) :-
    findall(U1-S1, (
        member(S1, L),
        move(B, S1, M, B2),
        (AILvl == 4 -> evaluate_board(B2, U1);
         AILvl == 3 -> evaluate_board_lineaire(B2, U1);
         AILvl == 2 -> evaluate_board_lineaire_3inRow(B2, U1))
    ), Scores),
    ( maximizing(M) -> max_member(U-S, Scores) ; min_member(U-S, Scores) ).

minimax(D, AI, B, M, S, U) :-
    D2 is D + 1,
    moves(B, L),          %%% get the list of available moves
    !,
    (D2 < 5 -> best(D2, AI, B, M, L, S, U) ; bestEstimate(B, AI, M, L, S, U)),  %%% recursively determine the best available move or use bestEstimate
    !
    .

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax(D, AI, B, M, S, U) :-
    utility(B, U)
    .

%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D, AI, B,M,[S1],S,U) :-
    move(B,S1,M,B2),             %%% apply the move to the board,
    inverse_mark(M,M2), 
    !,
    (utility(B2, U1), U1 \= 0 ->  %%% if the utility is not 0, use it directly
        (S = S1, U = U1),
        output_value(D,S,U)
    ;
        minimax(D,AI,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
        S = S1, !,
        output_value(D,S,U),
        !
    ).


% if there is more than one move in the list...

best(D, AI, B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    (utility(B2, U1), U1 \= 0 ->  %%% if the utility is not 0, use it directly
        best(D,AI,B,M,T,S2,U2),     %%% determine the best move of the remaining moves,
        output_value(D,S1,U1),      
        better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    ;
        minimax(D,AI,B2,M2,_S,UMinMax),  %%% otherwise, recursively search for the utility value of that move,
        best(D,AI,B,M,T,S2,U2),     %%% determine the best move of the remaining moves,
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :- 
    nl,
    player(1, V1, AI1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),
    (V1 == computer -> write(' with difficulty '), write(AI1) ; true),

    nl,
    player(2, V2, AI2),
    write('Player 2 is '),   %%% either human or computer
    write(V2),
    (V2 == computer -> write(' with difficulty '), write(AI2) ; true),
    nl,
    !
    .


output_winner(W) :-
    W == 1,
    write('X wins.'), nl,
    !
    .

output_winner(W) :-
    W == 2,
    write('O wins.'), nl,
    !
    .

output_winner(W) :-
    write('No winner.'), nl
    .


output_board(B) :-
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PSEUDO-RANDOM NUMBERS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% random_seed
%.......................................
% Initialize the random number generator...
% If no seed is provided, use the current time
%

random_seed :-
    random_seed(_),
    !
    .

random_seed(N) :-
    nonvar(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

random_seed(N) :-
    var(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N), 
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Second,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Second+1) * (Tick+1)),
    randomize(N), 
    !
    .

******************************************/



%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_int_1n(N, V) :-
    R is random,
    V2 is (R * N) - 0.5,           
    float_text(V2,V3,fixed(0)),
    int_text(V4,V3),
    V is V4 + 1,
    !
    .

******************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_member([V|_], V).
my_member([_|T], V) :- my_member(T, V).

my_append([], L, L).
my_append([H|T1], L2, [H|T3]) :- my_append(T1, L2, T3).


%.......................................
% set_item
%.......................................
% Given a board B, replace the item at coulmn Col with V
% return the new list in borad B2
%

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


%.......................................
% get_item
%.......................................
% Given a list L, retrieve the item at position N and return it as value V
%

get_item(Board, Row, Col, V) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%










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




evaluate_board_lineaire(Board, TotalScore) :-
    find_consecutive_4(Lists, Board),
    findall(Score, (
        member(Seq, Lists),
        evaluate_sequence_lineaire(Seq, Score)
    ), Scores),
    sum_list(Scores, TotalScore).

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




evaluate_board_lineaire_3inRow(Board, TotalScore) :-
    find_consecutive_4(Lists, Board),
    findall(Score, (
        member(Seq, Lists),
        evaluate_sequence_lineaire_3inRow(Seq, Score)
    ), Scores),
    sum_list(Scores, TotalScore).


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


