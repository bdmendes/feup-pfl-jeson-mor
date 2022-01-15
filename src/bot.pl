:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

% material_count(+Player, +Board, -MaterialNr)
material_count(P, B, M):-
findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).

% knight_hops(?StartPos, ?EndPos, ?Hops)
knight_hops_needed(SP, EP, H):-
    knight_hops_needed_aux(SP, EP, 0/0, H, 0).

knight_hops_needed_aux(CP, CP, _,  Acc, Acc).
knight_hops_needed_aux(CC/CR, EP, LC/LR, H, Acc):-
    CC > 0,
    CC =< 5,
    CR > 0,
    CR =< 5, 
    knight_move(CC-CR-EC-ER),
    EC \= LC,
    ER \= LR,
    NAcc is Acc + 1,
    knight_hops_needed_aux(EC/ER, EP, CC/CR, H, NAcc).