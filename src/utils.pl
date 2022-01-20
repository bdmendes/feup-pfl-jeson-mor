:-use_module(library(lists)).
:-use_module(library(random)).

% replicate_nested(?Height, ?Width, ?Elem, ?List)
replicate_nested(H, W, E, L):-
    replicate(W, E, R),
    replicate(H, R, L).

% replicate(?Size, ?Elem, ?List)
replicate(S, E, L):-
    length(L, S),
    maplist(=(E), L).

% nth0_nested(?Row, ?Col, ?List, ?Elem)
nth0_nested(R, C, L, E):-
    nth0(R, L, F),
    nth0(C, F, E).

% replace(?Index, ?List, ?Elem, ?NewList)
replace(I, L, E, NL):-
    nth0(I, L, _, R),
    nth0(I, NL, E, R).

% replace_nested(?Row, ?Column, ?List, ?Elem, ?NewList)
replace_nested(R, C, L, E, NL):-
    nth0(R, L, F),
    replace(C, F, E, NF),
    nth0(R, L, _, K),
    nth0(R, NL, NF, K).

% min_element_index(+List, -Index)
min_element_index([H|T], I):-
    min_element_index_aux([H|T], H, 0, 0, I).

% min_element_index_aux(+List, +CurrMax, +CurrIndex, +CurrMaxIndex, -Index)
min_element_index_aux([], _, _, CMI, CMI).
min_element_index_aux([H|T], CM, CI, _, I):-
    H < CM, !,
    NI is CI + 1,
    min_element_index_aux(T, H, NI, CI, I).
min_element_index_aux([_|T], CM, CI, CMI, I):-
    NI is CI + 1,
    min_element_index_aux(T, CM, NI, CMI, I).
