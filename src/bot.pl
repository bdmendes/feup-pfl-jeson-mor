:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

% material_count(+Player, +Board, -Material)
material_count(P, B, M):-
    findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).

% knight_hops(+Board, ?StartPosition, ?EndPosition, ?Hops)
knight_hops(B, SC-SR, EC-ER, H):-
    path(B, SC-SR, EC-ER, P),
    length(P, S),
    H is S-1.

% path(+Board, ?StartPosition, ?EndPosition, ?Path)
path(B, SC-SR, EC-ER, P):-
    bfs_path(B, SC-SR, [[EC-ER]], P).

% bfs_path(+Board, +EndPosition, +Queue, ?Path)
bfs_path(_, EC-ER, [[EC-ER|T]|_], [EC-ER|T]):- !.
bfs_path(B, EC-ER, [V|Q], P):-
    V = [CC-CR|T],
    findall([NC-NR, CC-CR|T], (can_move([_,B,_], CC-CR-NC-NR), \+member(NC-NR, [CC-CR|T])), L),
    append(Q, L, NQ),
    bfs_path(B, EC-ER, NQ, P).

% value(+GameState, +Player, -Value)
value([_,CB,OB], P, V):-
    board_evaluation([_,CB,OB],E),
    (P = w -> V is E; V is -E).

% board_evaluation(+GameState, -Evaluation)
board_evaluation([CP,CB,OB], 9999):-
    game_over([CP,CB,OB], w),!.
board_evaluation([CP,CB,OB], -9999):-
    game_over([CP,CB,OB], b),!.
board_evaluation([_,CB,_], E):-
    findall(C-R, nth0_nested(R, C, CB, w), WK),
    findall(C-R, nth0_nested(R, C, CB, b), BK),
    length(WK, WM),
    length(BK, BM),
    map_center_square_hops(CB, WK, WKH),
    map_center_square_hops(CB, BK, BKH),
    maplist(knight_position_score, WKH, WKSL),
    maplist(knight_position_score, BKH, BKSL),
    sumlist(WKSL, WKS),
    sumlist(BKSL, BKS),
    E is WM + WKS - BM - BKS.

% map_center_square_hops(+Board, +PositionList, -Hops)
map_center_square_hops(B, L, H):-
    length(B, BS),
    center_square(BS, CR, CC),
    map_center_square_hops_aux(CR-CC, B, L, [], H).

% map_center_square_hops_aux(+CenterSquare, +Board, +PositionList, +Acc, -Hops)
map_center_square_hops_aux(_, _, [], H, H).
map_center_square_hops_aux(CR-CC, B, [C-R|T], A, H):-
    knight_hops(B, C-R, CR-CC, N),
    map_center_square_hops_aux(CR-CC, B, T, [N|A], H).

% knight_position_score(+CenterHops, -Score)
knight_position_score(0, 0).
knight_position_score(H, S):-
    H > 0,
    S is 1/H.

% choose_move(+GameState, +Level, -Move)
choose_move([CP,CB,_], 0, M):-
    valid_moves([CP,CB,_], L),
    random_member(M, L).
