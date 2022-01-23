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

% max_element(+List, -Index, -Element)
max_element([H|T], I, E):-
    element_aux([H|T], >, H, 0, 0, I, E).

% max_element(+List, -Index, -Element)
min_element([H|T], I, E):-
    element_aux([H|T], <, H, 0, 0, I, E).

% element_aux(+List, +Op, +CurrBest, +CurrIndex, +CurrBestIndex, -Index, -Element)
element_aux([], _, CM, _, CMI, CMI, CM).
element_aux([H|T], OP, CM, CI, _, I, E):-
    H = CM, random_member(1, [0,1]), !,
    NI is CI + 1,
    element_aux(T, OP, H, NI, CI, I, E).
element_aux([H|T], OP, CM, CI, _, I, E):-
    call(OP,H,CM), !,
    NI is CI + 1,
    element_aux(T, OP, H, NI, CI, I, E).
element_aux([_|T], OP, CM, CI, CMI, I, E):-
    NI is CI + 1,
    element_aux(T, OP, CM, NI, CMI, I, E).
