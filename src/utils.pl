:-use_module(library(lists)).

% replicate(+Size, +Elem, ?List)
replicate(Size, Elem, List):-
    Size >= 0,
    replicate_aux(Size, Elem, List, []).
replicate_aux(0,_,List,List).
replicate_aux(Size, Elem, List, Acc):-
    NSize is Size - 1,
    replicate_aux(NSize, Elem, List, [Elem|Acc]).

% digits_value(+List, -Value)
digits_value(String, Value) :-
    digits_value_aux(String,0,Value).
digits_value_aux([], Value, Value).
digits_value_aux([H|T], Acc, Value):-
    char_code('0', ZeroCharCode),
    char_code(H, HeadCharCode),
    CharCodeDiff is HeadCharCode - ZeroCharCode,
    Nacc is 10*Acc + CharCodeDiff,
    digits_value_aux(T, Nacc, Value).
