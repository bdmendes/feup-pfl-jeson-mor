:-use_module(library(lists)).
:-use_module(library(random)).

%% replicate_nested(?Height, ?Width, ?Elem, ?List)
%
% Creates/verifies 2 dimensional lists with a given element
%
% @param Height
% @param Width 
% @param Elem
% @param List
replicate_nested(H, W, E, L):-
    replicate(W, E, R),
    replicate(H, R, L).

% replicate(?Size, ?Elem, ?List)
%
% Creates/verifies a list with a given size filled with a given element
%
% @param Size
% @param Elem
% @param List
replicate(S, E, L):-
    length(L, S),
    maplist(=(E), L).

%% nth0_nested(?Row, ?Col, ?List, ?Elem)
%
% Executes the nth0 predicate in a 2 dimensional list, allowing
%   for verification of elements.
%
% @param Row
% @param Col
% @param List
% @param Elem 
nth0_nested(R, C, L, E):-
    nth0(R, L, F),
    nth0(C, F, E).

%% replace(?Index, ?List, ?Elem, ?NewList)
%
% Replaces/verifies a given element
%
% @param Index
% @param List
% @param Elem
% @param NewList
replace(I, L, E, NL):-
    nth0(I, L, _, R),
    nth0(I, NL, E, R).

%% replace_nested(?Row, ?Column, ?List, ?Elem, ?NewList)
%
% Replaces/verifies a element in a 2 diemnsional list
%
% @param Row
% @param Column
% @param List
% @param Elem
% @param NewList
replace_nested(R, C, L, E, NL):-
    nth0(R, L, F),
    replace(C, F, E, NF),
    nth0(R, L, _, K),
    nth0(R, NL, NF, K).

%% max_element(+List, -Index, -Element)
%
% Finds the greater element in a List and its index
%
% @param List
% @param Index
% @param Element
max_element([H|T], I, E):-
    element_aux([H|T], >, H, 0, 0, I, E).

%% min_element(+List, -Index, -Element)
%
% Finds the smaller element in a List and its index
%
% @param List
% @param Index
% @param Element
min_element([H|T], I, E):-
    element_aux([H|T], <, H, 0, 0, I, E).

%% element_aux(+List, +Op, +CurrBest, +CurrIndex, +CurrBestIndex, -Index, -Element)
%
% Auxiliary function to find the max/min elements in a list. In case of multiple equal 
%   max/min elements, chooses a random one.
%
% @param List
% @param Op
% @param CurrBest
% @param CurrIndex
% @param CurrBestIndex
% @param Index
% @param Element
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
