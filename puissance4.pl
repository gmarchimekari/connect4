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
asserta( player(P, Type) ) - indicates which players are human/computer.
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
    retract(board(_)),
    retract(player(_,_)),
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
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .

human_playing(M) :- 
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :- 
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
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
    set_item(B, Col, V, B2)
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

game_over(Board, Winner) :-
    (check_win_board(Board, 'x') -> Winner = 1;
     check_win_board(Board, 'o') -> Winner = 2;
     board_full(Board, 'e') -> Winner = 0;
     Winner = -1).

check_win_board(Board, Mark) :-
    between(1, 6, Row),
    between(1, 7, Col),
    (check_line(Board, Row, Col, Mark, horizontal, 4);
     check_line(Board, Row, Col, Mark, vertical, 4);
     check_line(Board, Row, Col, Mark, diagonal_down, 4);
     check_line(Board, Row, Col, Mark, diagonal_up, 4)).
%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%

make_move(P, B, B2, Col) :-
    player(P, Type),

    make_move2(Type, P, B, B2, Col),

    retractall( board(_) ),
    asserta( board(B2) )
    .

make_move2(human, P, B, B2, Col) :-
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

make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Please select a column.'),
    make_move2(human,P,B,B2)
    .

make_move2(computer, P, B, B2, Col) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(B, 5, -10000, 10000, M, Col, _),  % Depth-limited search
    find_lowest_empty_square(B, Col, 'e', Row),
    move(B, Col, M, B2),
    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(Col),
    write('.'),
    nl, !.



minimax(Board, Depth, Alpha, Beta, PlayerMark, BestCol, BestScore) :-
    (game_over(Board, Winner); Depth = 0),
    !,
    evaluate_board(Board, PlayerMark, Score),
    BestScore = Score,
    BestCol = -1.

minimax(Board, Depth, Alpha, Beta, PlayerMark, BestCol, BestScore) :-
    findall(Col, valid_column(Board, Col), Cols),
    (maximizing_player(PlayerMark) -> 
        evaluate_columns_max(Board, Cols, Depth, Alpha, Beta, -10000, BestCol, BestScore, PlayerMark)
    ;
        evaluate_columns_min(Board, Cols, Depth, Alpha, Beta, 10000, BestCol, BestScore, PlayerMark)
    ).

evaluate_columns_max(_, [], _, _, _, BestScore, BestCol, BestScore, _) :- BestCol = -1.
evaluate_columns_max(Board, [Col|Cols], Depth, Alpha, Beta, CurrentBest, BestCol, BestScore, PlayerMark) :-
    make_temp_move(Board, Col, PlayerMark, NewBoard),
    NewDepth is Depth - 1,
    inverse_mark(PlayerMark, OpponentMark),
    minimax(NewBoard, NewDepth, Alpha, Beta, OpponentMark, _, Score),
    (Score > CurrentBest ->
        NewCurrentBest = Score,
        NewAlpha is max(Alpha, Score),
        (NewAlpha >= Beta ->
            BestCol = Col,
            BestScore = NewAlpha
        ;
            evaluate_columns_max(Board, Cols, Depth, NewAlpha, Beta, NewCurrentBest, TempCol, TempScore, PlayerMark),
            (TempScore > NewCurrentBest ->
                BestCol = TempCol,
                BestScore = TempScore
            ;
                BestCol = Col,
                BestScore = NewCurrentBest
            )
        )
    ;
        evaluate_columns_max(Board, Cols, Depth, Alpha, Beta, CurrentBest, BestCol, BestScore, PlayerMark)
    ).

evaluate_columns_min(_, [], _, _, _, BestScore, BestCol, BestScore, _) :- BestCol = -1.
evaluate_columns_min(Board, [Col|Cols], Depth, Alpha, Beta, CurrentBest, BestCol, BestScore, PlayerMark) :-
    make_temp_move(Board, Col, PlayerMark, NewBoard),
    NewDepth is Depth - 1,
    inverse_mark(PlayerMark, OpponentMark),
    minimax(NewBoard, NewDepth, Alpha, Beta, OpponentMark, _, Score),
    (Score < CurrentBest ->
        NewCurrentBest = Score,
        NewBeta is min(Beta, Score),
        (NewBeta =< Alpha ->
            BestCol = Col,
            BestScore = NewBeta
        ;
            evaluate_columns_min(Board, Cols, Depth, Alpha, NewBeta, NewCurrentBest, TempCol, TempScore, PlayerMark),
            (TempScore < NewCurrentBest ->
                BestCol = TempCol,
                BestScore = TempScore
            ;
                BestCol = Col,
                BestScore = NewCurrentBest
            )
        )
    ;
        evaluate_columns_min(Board, Cols, Depth, Alpha, Beta, CurrentBest, BestCol, BestScore, PlayerMark)
    ).



evaluate_board(Board, PlayerMark, Score) :-
    (check_win(Board, PlayerMark) -> Score = 1000
    ; check_win(Board, OpponentMark) -> Score = -1000
    ; board_full(Board, 'e') -> Score = 0
    ;
        count_potential_lines(Board, PlayerMark, PlayerScore),
        inverse_mark(PlayerMark, OpponentMark),
        count_potential_lines(Board, OpponentMark, OpponentScore),
        Score is PlayerScore - OpponentScore * 2  % Prioritize blocking opponent
    ).

count_potential_lines(Board, Mark, Score) :-
    findall(Strength, (
        between(1, 6, Row),
        between(1, 7, Col),
        check_line_strength(Board, Row, Col, Mark, Strength)
    ), Strengths),
    sum_list(Strengths, Score).

check_line_strength(Board, Row, Col, Mark, Strength) :-
    (check_line(Board, Row, Col, Mark, horizontal, 4) -> Strength = 100
    ; check_line(Board, Row, Col, Mark, vertical, 4) -> Strength = 100
    ; check_line(Board, Row, Col, Mark, diagonal_down, 4) -> Strength = 100
    ; check_line(Board, Row, Col, Mark, diagonal_up, 4) -> Strength = 100
    ; check_line(Board, Row, Col, Mark, horizontal, 3) -> Strength = 50
    ; check_line(Board, Row, Col, Mark, vertical, 3) -> Strength = 50
    ; check_line(Board, Row, Col, Mark, diagonal_down, 3) -> Strength = 50
    ; check_line(Board, Row, Col, Mark, diagonal_up, 3) -> Strength = 50
    ; check_line(Board, Row, Col, Mark, horizontal, 2) -> Strength = 10
    ; check_line(Board, Row, Col, Mark, vertical, 2) -> Strength = 10
    ; check_line(Board, Row, Col, Mark, diagonal_down, 2) -> Strength = 10
    ; check_line(Board, Row, Col, Mark, diagonal_up, 2) -> Strength = 10
    ; Strength = 0
    ).

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     IMPROVED WIN CHECKING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_line(Board, Row, Col, Mark, Direction, Length) :-
    (Direction = horizontal ->
        ColEnd is Col + Length - 1,
        ColEnd =< 7,
        check_consecutive(Board, Row, Col, 0, Length, Mark, 0, 1)
    ; Direction = vertical ->
        RowEnd is Row + Length - 1,
        RowEnd =< 6,
        check_consecutive(Board, Row, Col, 0, Length, Mark, 1, 0)
    ; Direction = diagonal_down ->
        RowEnd is Row + Length - 1,
        ColEnd is Col + Length - 1,
        RowEnd =< 6, ColEnd =< 7,
        check_consecutive(Board, Row, Col, 0, Length, Mark, 1, 1)
    ; Direction = diagonal_up ->
        RowEnd is Row - Length + 1,
        ColEnd is Col + Length - 1,
        RowEnd >= 1, ColEnd =< 7,
        check_consecutive(Board, Row, Col, 0, Length, Mark, -1, 1)
    ).

check_consecutive(_, _, _, Count, Length, _, _, _) :-
    Count >= Length, !.

check_consecutive(Board, Row, Col, Count, Length, Mark, DR, DC) :-
    get_item(Board, Row, Col, Cell),
    (Cell == Mark ->
        NewCount is Count + 1,
        NextRow is Row + DR,
        NextCol is Col + DC,
        check_consecutive(Board, NextRow, NextCol, NewCount, Length, Mark, DR, DC)
    ;
        check_consecutive(Board, Row, Col, Count, Length, Mark, DR, DC)
    ).

    valid_column(Board, Col) :-
    between(1, 7, Col),
    find_lowest_empty_square(Board, Col, 'e', Row),
    Row \= 7.

make_temp_move(Board, Col, Mark, NewBoard) :-
    find_lowest_empty_square(Board, Col, 'e', Row),
    set_item(Board, Col, Mark, NewBoard).

maximizing_player('x').
minimizing_player('o').
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
/*
utility(B,U) :-
    win(B,'x'),
    U = 1, 
    !
    .

utility(B,U) :-
    win(B,'o'),
    U = (-1), 
    !
    .

utility(B,U) :-
    U = 0
    .
*/
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

minimax(D,[E,E,E, E,E,E, E,E,E],M,S,U) :-   
    blank_mark(E),
    random_int_1n(9,S),
    !
    .

minimax(D,B,M,S,U) :-
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,S,U),  %%% recursively determine the best available move
    !
    .

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax(D,B,M,S,U) :-
    utility(B,U)      
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    minimax(D,B2,M2,_S,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),      
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
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
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2), 
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

member([V|T], V).
member([_|T], V) :- member(T,V).

append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).


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