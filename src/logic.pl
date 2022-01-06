:-ensure_loaded('utils.pl').

% initial_state(+Size, +FirstToPlay, -GameState)
initial_state(Size, FirstToPlay, [FirstToPlay,Board]):-
    Size >= 5,
    Size =< 9,
    Size mod 2 =:= 1,
    (FirstToPlay = w; FirstToPlay = b),
    MiddleSize is Size - 2,
    replicate(Size, o, MiddleRow),
    replicate(MiddleSize, MiddleRow, MiddleBoard),
    replicate(Size, w, WhiteRow),
    replicate(Size, b, BlackRow),
    append([BlackRow|MiddleBoard], [WhiteRow], Board).

% move(+GameState, +Move, -NewGameState)
move([CurrentToPlay,Board], Move, [NextToPlay,NewBoard]):-
    length(Board, BoardSize),
    parse_move(Move, BoardSize, StartColumnNumber, StartRowNumber, EndColumnNumber, EndRowNumber),
    nth0(StartRowNumber, Board, StartRow),
    nth0(StartColumnNumber, StartRow, Piece),
    Piece = CurrentToPlay,
    nth0(EndRowNumber, Board, EndRow),
    nth0(EndColumnNumber, EndRow, _),
    is_knight_move(StartColumnNumber, StartRowNumber, EndColumnNumber, EndRowNumber),
    next_to_play(CurrentToPlay, NextToPlay).

% parse_move(+Move, +BoardSize, -Column, -Row)
parse_move(StartSquare-EndSquare, BoardSize, StartColumn, StartRow, EndColumn, EndRow):-
    atom_chars(StartSquare, StartSquareString),
    atom_chars(EndSquare, EndSquareString),
    parse_square(StartSquareString, BoardSize, StartColumn, StartRow),
    parse_square(EndSquareString, BoardSize, EndColumn, EndRow).

% parse_square(+AlgebraicNotationString, +BoardSize, -Column, -Row)
parse_square([H|T], BoardSize, Column, Row):-
    char_code('a', ACharCode),
    char_code(H, ColumnCharCode),
    Column is ColumnCharCode - ACharCode,
    digits_value(T, InvertedRow),
    Row is BoardSize - InvertedRow.

% next_to_play(+Current, -Next)
next_to_play(w,b).
next_to_play(b,w).

% is_knight_move(+StartColumn, +StartRow, +EndColumn, +EndRow)
is_knight_move(StartColumn, StartRow, EndColumn, EndRow):-
    (EndColumn is StartColumn + 1; EndColumn is StartColumn - 1),!,
    (EndRow is StartRow - 2; EndRow is StartRow + 2).
is_knight_move(StartColumn, StartRow, EndColumn, EndRow):-
    (EndColumn is StartColumn + 2; EndColumn is StartColumn - 2),
    (EndRow is StartRow - 1; EndRow is StartRow + 1).
