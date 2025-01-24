%defining the board
board([
    ['r', 'r', 'r', 'r', 'r', 'r', 'r'],
    ['r', 'r', 'r', 'r', 'r', 'r', 'r'],
    ['r', 'y', 'r', 'r', 'r', 'r', 'r'],
    ['r', 'y', 'y', 'r', 'r', 'r', 'r'],
    ['r', 'r', 'r', 'r', 'r', 'r', 'r'],
    ['r', 'r', 'r', 'y', 'y', 'r', 'r']
]).


board_full(Board, E) :-
    findall(P, (nth1(Row, Board, RowList), nth1(Col, RowList, P), P = E), EmptySquares),
    EmptySquares = [].  % The board is full if there are no empty squares left

