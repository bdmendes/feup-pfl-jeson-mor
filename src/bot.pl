:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

% material_count(+Player, +Board, -Material)
material_count(P, B, M):-
    findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).

% knights_position_score(+Player, +Board, -Score)
knights_position_score(P, B, S):-
    findall(C-R, nth0_nested(R, C, B, P), K),
    length(B, BS),
    map_center_square_hops(BS, K, KH),
    maplist(knight_position_score, KH, KSL),
    sumlist(KSL, S).

% knight_hops(+BoardSize, ?StartPosition, ?EndPosition, ?Hops)
:- dynamic knight_hops/4.
knight_hops(BS, SC-SR, EC-ER, H):-
    path(BS, SC-SR, EC-ER, P),
    length(P, S),
    H is S-1,
    asserta(( knight_hops(BS, SC-SR, EC-ER, H):- ! )),
    asserta(( knight_hops(BS, EC-ER, SC-SR, H):- ! )).

% path(+BoardSize, ?StartPosition, ?EndPosition, ?Path)
path(BS, SC-SR, EC-ER, P):-
    bfs_path(BS, SC-SR, [[EC-ER]], P).

% bfs_path(+BoardSize, +EndPosition, +Queue, ?Path)
bfs_path(_, EC-ER, [[EC-ER|T]|_], [EC-ER|T]):- !.
bfs_path(BS, EC-ER, [V|Q], P):-
    V = [CC-CR|T],
    findall([NC-NR, CC-CR|T], (knight_move(CC-CR-NC-NR), NC >= 0, NC < BS, NR >= 0, NR < BS, \+member(NC-NR, [CC-CR|T])), L),
    append(Q, L, NQ),
    bfs_path(BS, EC-ER, NQ, P).

% value(+GameState, +Player, -Value)
value([CP,CB,OB], P, V):-
    board_evaluation([CP,CB,OB],E),
    (P = w -> V is E; V is -E).

% board_evaluation(+GameState, -Evaluation)
board_evaluation([CP,CB,OB], 9999):-
    game_over([CP,CB,OB], w),!.
board_evaluation([CP,CB,OB], -9999):-
    game_over([CP,CB,OB], b),!.
board_evaluation([CP,CB,_], E):-
    material_count(w, CB, WM),
    material_count(b, CB, BM),
    WM > 0, BM > 0,
    knights_position_score(w, CB, WKS),
    knights_position_score(b, CB, BKS),
    attacking_knights([w,CB,_], WAK),
    attacking_knights([b,CB,_], BAK),
    ((CP = w, WAK > 0) -> PBAK is BAK - 1; PBAK is BAK),
    ((CP = b, BAK > 0) -> PWAK is WAK - 1; PWAK is WAK),
    E is WM + 0.25*(WKS/WM) + 0.25*PWAK - BM - 0.25*(BKS/BM) - 0.25*PBAK.

% attacking_knights(+GameState, -NumberOfAttacks)
attacking_knights([P,CB,_], NA):-
    findall(C-R, nth0_nested(R, C, CB, P), K),
    next_to_play(P, NP), 
    filter_attacking_knights([P,CB,_], NP, K, [], FM),
    length(FM, NA).

% to_opponent_square_aux(+GameState, +NextPlayer, +KnightPositions, +Acc, -FilteredMoves)
filter_attacking_knights(_, _, [], FM, FM).
filter_attacking_knights([CP,CB,_], NP, [SC-SR|T], A, FM):-
    findall(EC-ER, (can_move([CP,CB,_], SC-SR-EC-ER), nth0_nested(ER,EC,CB,NP)), L),
    length(L, LS),
    LS > 0, !,
    filter_attacking_knights([CP,CB,_], NP, T, [SC-SR|A], FM).
filter_attacking_knights([CP,CB,_], NP, [_|T], A, FM):-
    filter_attacking_knights([CP,CB,_], NP, T, A, FM).

% map_center_square_hops(+BoardSize, +PositionList, -Hops)
map_center_square_hops(BS, L, H):-
    center_square(BS, CR, CC),
    map_center_square_hops_aux(CR-CC, BS, L, [], H).

% map_center_square_hops_aux(+CenterSquare, +BoardSize, +PositionList, +Acc, -Hops)
map_center_square_hops_aux(_, _, [], H, H).
map_center_square_hops_aux(CR-CC, BS, [C-R|T], A, H):-
    knight_hops(BS, C-R, CR-CC, N),
    map_center_square_hops_aux(CR-CC, BS, T, [N|A], H).

% knight_position_score(+CenterHops, -Score)
knight_position_score(0, 1).
knight_position_score(H, S):-
    H > 0,
    S is 1/H.

% choose_move(+GameState, +Level, -Move)
choose_move([CP,CB,_], 1, M):-
    valid_moves([CP,CB,_], L),
    random_member(M, L).
choose_move([CP,CB,_], 2, M):-
    valid_moves([CP,CB,_], L),
    map_evaluate([CP,CB,_], L, E),
    min_element_index(E, I),
    nth0(I, L, M).

% map_evaluate(+GameState, +Moves, +Evaluations)
map_evaluate([CP,CB,_], [H|T], E):-
    map_evaluate_aux([CP,CB,_], [H|T], [], RE),
    reverse(RE,E).

% map_evaluate_aux(+GameState, +Moves, +Acc, +Evaluations)
map_evaluate_aux(_, [], E, E).
map_evaluate_aux([CP,CB,_], [H|T], A, E):-
    H = SC-SR-EC-ER,
    move([CP,CB,_], SC-SR-EC-ER, [NP, NB, OB]),
    value([NP, NB, OB], NP, V),
    map_evaluate_aux([CP,CB,_], T, [V|A], E).