:-set_prolog_flag(toplevel_print_options,
    [quoted(true), portrayed(true), max_depth(0)]).
:-ensure_loaded('utils.pl').

% initial_state(+Size, +FirstToPlay, -GameState)
initial_state(Size, FirstToPlay, [FirstToPlay|Board]):-
    Size >= 5,
    Size =< 15,
    Size mod 2 =:= 1,
    (FirstToPlay = w; FirstToPlay = b),
    replicate(Size, o, Row),
    assemble_empty_board(Size, Size, Row, [], Board).

% assemble_empty_board(+Size, +NRows, +Row, +Acc, -Board)
assemble_empty_board(_, 0, _, Board, Board):- !.
assemble_empty_board(Size, NRows, Row, Acc, Board):-
    Remaining is NRows - 1,
    assemble_empty_board(Size, Remaining, Row, [Row|Acc], Board).
