% display_game(+GameState)
display_game([CurrentToPlay,Board,_]):-
    length(Board,Size),
    BoardWidth is Size*2 + 3,
    ColumnCoordsWidth is Size*2,
    write('\n    '),
    char_code('a',CharCode),
    display_column_coords(ColumnCoordsWidth, CharCode),
    write('\n  '),
    display_separator(BoardWidth),
    put_char('\n'),
    display_board(Board),
    write('  '),
    display_separator(BoardWidth),
    write('\n\n'),
    (CurrentToPlay = b -> write('Black to play\n'); write('White to play\n')),
    put_char('\n').

% display_board(+Board)
display_board([]).
display_board([H|T]):-
    length([H|T],RowNumber),
    write(RowNumber),
    write(' |'),
    display_row(H),
    write(' |\n'),
    display_board(T).

% display_row(+Row)
display_row([]).
display_row([H|T]):-
    put_char(' '),
    put_char(H),
    display_row(T).

% display_separator(+Size)
display_separator(0):- !.
display_separator(Size):-
    put_char('-'),
    NSize is Size - 1,
    display_separator(NSize).

% display_bottom_coords(+Width, +Char):-
display_column_coords(0, _):- !.
display_column_coords(Width, CharCode):-
    (Width mod 2 =:= 0 ->
        put_code(CharCode), NCharCode is CharCode + 1;
        put_char(' '), NCharCode is CharCode),
    NWidth is Width - 1,
    display_column_coords(NWidth, NCharCode).
