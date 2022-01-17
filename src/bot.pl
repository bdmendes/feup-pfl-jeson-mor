:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

% material_count(+Player, +Board, -MaterialNr)
material_count(P, B, M):-
findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).

% knight_hops(?StartPos, ?EndPos, ?Hops)
knight_hops_needed(CB,SC-SR, EP, H):-
    find_path(CB, [[SC-SR]], EP, M),
    length(M, N),
    H is N-1.

find_path(_, [[EP|T]|_], EP, [EP|T]).
find_path(CB, [[CC-CR|T]|LM], EP, M):-
    findall([NC-NR, CC-CR|T], (can_move([_,CB,_], CC-CR-NC-NR), \+member(NC-NR, [CC-CR|T])), NM1),
    append(LM, NM1, NM2),
    find_path(CB, NM2, EP, M).
