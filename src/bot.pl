:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

% material_count(+Player, +Board, -MaterialNr)
material_count(P, B, M):-
findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).

% knight_hops(?StartPos, ?EndPos, ?Hops)
knight_hops_needed([SP , EP, H):-
    knight_hops_needed_aux([SP], EP, L, []),
    length(L, H).

% DFS
knight_hops_needed_aux(EP, EP, L, L).
knight_hops_needed_aux(CP, EP, L, V):-
    \+member(CP, V), 
    CC-CR = CP,
    CC > 0,
    CC =< 5,
    CR > 0,
    CR =< 5,
    knight_move(CC-CR-NC-NR),
    knight_hops_needed_aux(NC-NR, EP, L, [CP|V]).



    