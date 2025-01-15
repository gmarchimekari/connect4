%defining the board
board([
    ['r', 'r', 'r', 'r', 'y', 'e', 'r'],
    ['e', 'r', 'r', 'y', 'y', 'e', 'y'],
    ['e', 'y', 'y', 'y', 'e', 'e', 'r'],
    ['e', 'r', 'y', 'e', 'e', 'e', 'r'],
    ['e', 'y', 'e', 'e', 'e', 'e', 'y'],
    ['e', 'e', 'e', 'e', 'e', 'e', 'e']
]).

% Check if the square at (Row, Col) is part of a winning sequence
check_win(Board, Row, Col) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Player),
    Player \= 'e',  % Ensure the square is not empty
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
    append(_, [P, P, P, P | _], List).