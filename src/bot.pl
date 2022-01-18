:-ensure_loaded('utils.pl').
:-ensure_loaded('logic.pl').

% material_count(+Player, +Board, -Material)
material_count(P, B, M):-
    findall(_, nth0_nested(_, _, B, P), L),
    length(L, M).

% knight_hops(+Board, ?StartPosition, ?EndPosition, ?Hops)
knight_hops(B, SC-SR, EC-ER, H):-
    path(B, [[SC-SR]], EC-ER, P),
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
