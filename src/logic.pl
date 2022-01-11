:-ensure_loaded('utils.pl').

% initial_state(+Size, +FirstToPlay, -GameState)
initial_state(S, FP, [FP,B,B]):-
    member(S, [5,7,9]),
    (FP = w; FP = b),
    MS is S - 2,
    replicate_nested(MS, S, o, MB),
    replicate(S, w, WR),
    replicate(S, b, BR),
    append([BR|MB], [WR], B).

% move(+GameState, +Move, -NewGameState)
move([CP,CB,_], M, [NP,NB,CB]):-
    length(CB, BS),
    parse_move(M, BS, SC, SR, EC, ER),
    nth0_nested(SR, SC, CB, P),
    P = CP,
    nth0_nested(ER, EC, CB, _),
    is_knight_move(SC, SR, EC, ER),
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB),
    next_to_play(CP, NP).

% parse_move(+Move, +BoardSize, -Column, -Row)
parse_move(SS-ES, BS, SC, SR, EC, ER):-
    atom_chars(SS, SSString),
    atom_chars(ES, ESString),
    parse_square(SSString, BS, SC, SR),
    parse_square(ESString, BS, EC, ER).

% parse_square(+AlgebraicNotationString, +BoardSize, -Column, -Row)
parse_square([H|T], BoardSize, Column, Row):-
    char_code('a', ACharCode),
    char_code(H, ColumnCharCode),
    Column is ColumnCharCode - ACharCode,
    digits_value(T, InvertedRow),
    Row is BoardSize - InvertedRow.

% next_to_play(+CurrentToPlay, -Next)
next_to_play(w,b).
next_to_play(b,w).

% is_knight_move(+StartColumn, +StartRow, +EndColumn, +EndRow)
is_knight_move(StartColumn, StartRow, EndColumn, EndRow):-
    (EndColumn is StartColumn + 1; EndColumn is StartColumn - 1),
    (EndRow is StartRow - 2; EndRow is StartRow + 2).
is_knight_move(StartColumn, StartRow, EndColumn, EndRow):-
    (EndColumn is StartColumn + 2; EndColumn is StartColumn - 2),
    (EndRow is StartRow - 1; EndRow is StartRow + 1).

% is_center_square(+BoardSize, +Row, +Column)
is_center_square(BoardSize, Row, Column):-
    Row =:= BoardSize div 2,
    Column =:= BoardSize div 2.

% digits_value(+List, -Value)
digits_value(String, Value) :-
    digits_value_aux(String,0,Value).
digits_value_aux([], Value, Value).
digits_value_aux([H|T], Acc, Value):-
    char_code('0', ZeroCharCode),
    char_code(H, HeadCharCode),
    CharCodeDiff is HeadCharCode - ZeroCharCode,
    Nacc is 10*Acc + CharCodeDiff,
    digits_value_aux(T, Nacc, Value).