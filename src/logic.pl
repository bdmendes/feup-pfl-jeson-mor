:-ensure_loaded('utils.pl').

% initial_state(+Size, +FirstToPlay, -GameState)
initial_state(S, FP, [FP,B,B]):-
    member(S, [5,7,9]),
    (FP = w; FP = b),
    MS is S - 2,
    replicate_nested(MS, S, o, MB),
    replicate(S, w, WR),
    replicate(S, b, BR),
    append([BR|MB],[WR], B).

% move(+GameState, ?Move, ?NewGameState)
move([CP,CB,_], SC-SR-EC-ER, [NP,NB,CB]):-
    can_move([CP,CB,_], SC-SR-EC-ER),
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB),
    next_to_play(CP, NP).

% can_move(+GameState, ?Move)
can_move([CP,CB,_], SC-SR-EC-ER):-
    nth0_nested(SR, SC, CB, P),
    P = CP,
    nth0_nested(ER, EC, CB, _),
    knight_move(SC-SR-EC-ER).

% parse_move(+AlgebraicNotation, +Board, -Move)
parse_move(SS-ES, B, SC-SR-EC-ER):-
    length(B, BS),
    atom_chars(SS, SS_),
    atom_chars(ES, ES_),
    parse_square(SS_, BS, SC-SR),
    parse_square(ES_, BS, EC-ER).

% parse_square(+AlgebraicNotation, +BoardSize, ?Square)
parse_square([H|T], BS, C-R):-
    char_code('a', AC),
    char_code(H, CC),
    C is CC - AC,
    catch(number_chars(IR,T), _, fail),
    R is BS - IR.

% next_to_play(?CurrentToPlay, ?Next)
next_to_play(w,b).
next_to_play(b,w).

% knight_move(+Move)
knight_move(SC-SR-EC-ER):-
    member([DC,DR],[[1,-2],[1,2],[-1,-2],[-1,2],[2,-1],[2,1],[-2,-1],[-2,1]]),
    EC is SC + DC,
    ER is SR + DR.

% center_square(+BoardSize, ?Row, ?Column)
center_square(BS, R, C):-
    K is BS div 2,
    R = K,
    C = K.

% game_over(+GameState, ?Winner)
game_over([CP,CB,_], W):-
    valid_moves([CP,CB,_], []),!,
    next_to_play(CP,W).
game_over([_,CB,OB], W):-
    length(CB, BS),
    center_square(BS, R, C),
    nth0_nested(R, C, CB, o),
    nth0_nested(R, C, OB, W),
    W \= o.

% valid_moves(+GameState, ?ListOfMoves)
valid_moves([CP,CB,_], L):-
    findall(SC-SR-EC-ER, can_move([CP,CB,_], SC-SR-EC-ER), L).
