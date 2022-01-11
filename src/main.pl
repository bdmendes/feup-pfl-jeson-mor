:-ensure_loaded('logic.pl').
:-ensure_loaded('view.pl').

play:-
    initial_state(5,w,GS),
    play_aux(GS).

play_aux(GS):-
    game_over(GS, Winner),!,
    format("~w has won the game!\n", [Winner]).

play_aux([CP,CB,OB]):-
    display_game([CP,CB,OB]),
    read(X),
    parse_move(X, CB, M),
    (move([CP,CB,OB], M, NGS)
        -> write('Valid move!\n'), play_aux(NGS);
        write('Invalid move!\n'), play_aux([CP,CB,OB])).
