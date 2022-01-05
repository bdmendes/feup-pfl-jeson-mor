:-use_module(library(lists)).

% replicate(+Size, +Elem, ?List)
replicate(Size, Elem, List):-
    Size >= 0,
    replicate_aux(Size, Elem, List, []).
replicate_aux(0,_,List,List).
replicate_aux(Size, Elem, List, Acc):-
    NSize is Size - 1,
    replicate_aux(NSize, Elem, List, [Elem|Acc]).
