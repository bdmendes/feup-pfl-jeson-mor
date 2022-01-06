/* :-set_prolog_flag(toplevel_print_options,
    [quoted(true), portrayed(true), max_depth(0)]). */
:-ensure_loaded('utils.pl').

% initial_state(+Size, +FirstToPlay, -GameState)
initial_state(Size, FirstToPlay, [FirstToPlay,Board]):-
    Size >= 5,
    Size =< 15,
    Size mod 2 =:= 1,
    (FirstToPlay = w; FirstToPlay = b),
    MiddleSize is Size - 2,
    replicate(Size, o, MiddleRow),
    replicate(Size, w, WhiteRow),
    replicate(Size, b, BlackRow),
    assemble_empty_board(MiddleSize, MiddleSize, MiddleRow, [], MiddleBoard),
    append([BlackRow|MiddleBoard], [WhiteRow], Board).
    
% assemble_empty_board(+Size, +NRows, +Row, +Acc, -Board)
assemble_empty_board(_, 0, _, Board, Board):- !.
assemble_empty_board(Size, NRows, Row, Acc, Board):-
    Remaining is NRows - 1,
    assemble_empty_board(Size, Remaining, Row, [Row|Acc], Board).

% move(+GameState, +Move, -NewGameState)
move([CurrentToPlay,Board], Move, [NextToPlay,NewBoard]):-
    length(Board, BoardSize),
    parse_move(Move, BoardSize, ColumnNumber, RowNumber),
    nth0(RowNumber, Board, Row),
    nth0(ColumnNumber, Row, Piece),
    Piece = CurrentToPlay.

% parse_move(+Move, +BoardSize, -Column, -Row)
parse_move([H|T], BoardSize, Column, Row):-
    char_code('a', ACharCode),
    char_code(H, ColumnCharCode),
    Column is ColumnCharCode - ACharCode,
    digits_value(T, InvertedRow),
    Row is BoardSize - InvertedRow.
