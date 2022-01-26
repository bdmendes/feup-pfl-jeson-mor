:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

%%%% Heuristic 1: Material count %%%%

%% material_count(+Player, +Board, -Material)
%
% Counts the knights available for a given player
%
% @param Player whose material we want to count
% @param Board to count from
% @param Number of pieces in play
material_count(P, B, M):-
    findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).


%%%% Heuristic 2: Positional advantage: distance to center %%%%

%% knights_position_score(+Player, +Board, -Score)
%
% Scores the player's knight positions
%
% @param Player to score 
% @param Board to evaluate
% @param Center distance score
knights_position_score(P, B, S):-
    findall(C-R, nth0_nested(R, C, B, P), K),
    length(B, BS),
    map_center_square_hops(BS, K, KH),
    maplist(knight_position_score, KH, KSL),
    sumlist(KSL, S).

%% knight_position_score(+CenterHops, -Score)
%
% Adjusts the positional score
%
% @param Knight hops needed to reach the center position
% @param Score attributed to the distance
knight_position_score(0, 1).
knight_position_score(H, S):-
    H > 0,
    S is 1/H.

%% map_center_square_hops(+BoardSize, +PositionList, -Hops)
%
% Lists the needed knight hops to reach the center square for each knight
%
% @param Board size
% @param List of knight positions
% @param List of hops needed to reach the center position for each knight
map_center_square_hops(BS, L, H):-
    center_square(BS, CR, CC),
    map_center_square_hops_aux(CR-CC, BS, L, [], H).

%% map_center_square_hops_aux(+CenterSquare, +BoardSize, +PositionList, +Acc, -Hops)
%
% Auxiliary function to list the needed knight hops to reach the center for each knight
%
% @param Center position coordinates
% @param Board size
% @param List of knight positions
% @param Accumulator
% @param List of hops needed to reach the center position for each knight
map_center_square_hops_aux(_, _, [], H, H).
map_center_square_hops_aux(CR-CC, BS, [C-R|T], A, H):-
    knight_hops(BS, C-R, CR-CC, N),
    map_center_square_hops_aux(CR-CC, BS, T, [N|A], H).

%% knight_hops(+BoardSize, ?StartPosition, ?EndPosition, ?Hops)
%
% Function to get the needed hops to reach an end position starting from another position,
%   using bfs.
%
% @param Board size
% @param Start position
% @param End position
% @param Hops needed to go from the start to end position
:- dynamic knight_hops/4.
knight_hops(BS, SC-SR, EC-ER, H):-
    bfs_path(BS, SC-SR, [[EC-ER]], P),
    length(P, S),
    H is S-1,
    asserta(( knight_hops(BS, SC-SR, EC-ER, H):- ! )),
    asserta(( knight_hops(BS, EC-ER, SC-SR, H):- ! )).

%% bfs_path(+BoardSize, +EndPosition, +Queue, ?Path)
%
% BFS algotithm to search the possible moves tree to move from start postion to end position
%
% @param Board size
% @param End position
% @param Queue of positions
% @param Path taken
bfs_path(_, EC-ER, [[EC-ER|T]|_], [EC-ER|T]):- !.
bfs_path(BS, EC-ER, [V|Q], P):-
    V = [CC-CR|T],
    findall([NC-NR, CC-CR|T], (knight_move(CC-CR-NC-NR), NC >= 0, NC < BS, NR >= 0, NR < BS, \+member(NC-NR, [CC-CR|T])), L),
    append(Q, L, NQ),
    bfs_path(BS, EC-ER, NQ, P).


%%%% Heuristic 3: Tactical opportunities: knights attacking others %%%%

%% attacking_knights(+GameState, -NumberOfAttacks)
%
% Gets the number of possible moves resulting in attack 
%
% @param Game state
% @param Number of knights attacking the opponent
attacking_knights([P,CB,_], NA):-
    findall(C-R, nth0_nested(R, C, CB, P), K),
    next_to_play(P, NP), 
    filter_attacking_knights([P,CB,_], NP, K, [], FM),
    length(FM, NA).

% to_opponent_square_aux(+GameState, +NextPlayer, +KnightPositions, +Acc, -FilteredMoves)
%
% Filters the moves resulting in attack
%
% @param Game state
% @param Next player
% @param List containg current player's knight positions
% @param Accumulator
% @param Moves resulting in attack 
filter_attacking_knights(_, _, [], FM, FM).
filter_attacking_knights([CP,CB,_], NP, [SC-SR|T], A, FM):-
    findall(EC-ER, (can_move([CP,CB,_], SC-SR-EC-ER), nth0_nested(ER,EC,CB,NP)), L),
    length(L, LS),
    LS > 0, !,
    filter_attacking_knights([CP,CB,_], NP, T, [SC-SR|A], FM).
filter_attacking_knights([CP,CB,_], NP, [_|T], A, FM):-
    filter_attacking_knights([CP,CB,_], NP, T, A, FM).


%%%% Static board evaluation using heuristics %%%%

%% value(+Player, +GameState, -Value)
%
% Evaluates the board from the prespective of the player. 
% Positive for player leverage, negative for opponent leverage.
%
% @param Player from which the evaluation is made
% @param Game state
% @value Evaluation value
value(P, [CP,CB,OB], V):-
    board_evaluation([CP,CB,OB],E),
    (P = w -> V is E; V is -E).

%% board_evaluation(+GameState, -Evaluation)
%
% Statically evaluates the board using chess notation.
% White leverage postive, black leverage negative.
%
% @param Game state
% @param Board Evaluation
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


%%%% Computer algortihms %%%%

%% choose_move(+GameState, +Level, -Move)
%
% Chooses a computer move. For level 1 chooses a random valid play.
% Larger level values are met with the minimax algorithm with depth
%   minimum depth 1. 
%
% @param Game state
% @param Inteligence level
% @param Move to make
choose_move([CP,CB,_], 1, M):-
    valid_moves([CP,CB,_], L),
    random_member(M, L).
choose_move([CP,CB,_], L, M):-
    L > 1,
    D is L - 1,
    minimax(D, [CP,CB,_], M).

%% minimax(+Depth, +GameState, -Move)
%
% Uses the minimax algorith with the given depth to determine the best move.
%
% @param Depth of the algorithm
% @param Game state
% @param Resultant move
minimax(D, [CP,CB,_], M):-
    minimax_aux(D, [CP,CB,_], M-_).

%% minimax_aux(+Depth, +GameState, -MoveScore)
%
% Executes the minimax algorithm itself
%
% @param Depth of the algorithm
% @param Game state
% @param Resultant move and corresponding score
minimax_aux(D, [CP,CB,OB], _-E):-
    game_over([CP,CB,OB],_), !, % cut branch if game is over
    value(CP, [CP,CB,OB], E_),
    (D mod 2 =:= 0 -> E is -E_; E is E_). % simulate this being the base case
minimax_aux(D, [CP,CB,OB], M-E):-
    D > 1, D mod 2 =\= 0, % only jump if base case is same player
    value(CP, [CP,CB,OB], E_),
    (E_ > 2; E_ < -2), !, % cut branch if static board evaluation is obvious
    minimax_aux(1, [CP,CB,OB], M-E).
minimax_aux(1, [CP,CB,_], M-E):-
    valid_moves([CP,CB,_], ML),
    maplist(move([CP,CB,_]), ML, NGS),
    maplist(value(CP), NGS, EL),
    max_element(EL, I, _), % depth 1 is always the maximizer
    nth0(I, ML, M),
    nth0(I, EL, E).
minimax_aux(D, [CP,CB,_], M-E):-
    D > 1,
    ND is D - 1,
    valid_moves([CP,CB,_], ML_),
    maplist(move([CP,CB,_]), ML_, NGS),
    maplist(minimax_aux(ND), NGS, MSL),
    maplist(filter_score_minimax, MSL, SL),
    % odd depths lead to us being the maximizer at the base case
    % even depths lead to the opponent
    (D mod 2 =:= 0 -> min_element(SL, I, E2); max_element(SL, I, E2)),
    nth0(I, NGS, GS_),
    next_to_play(CP, NP),
    (D mod 2 =:= 0 -> value(NP, GS_, E1); value(CP, GS_, E1)),
    E is E1 + E2,
    nth0(I, ML_, M).

% filter_score_minimax(+MoveScore, -Score)
%
% Filters a move score
%
% @param Resultant move and corresponding score
% @param Score only
filter_score_minimax(_-E, E).
