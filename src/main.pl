:-ensure_loaded('logic.pl').
:-ensure_loaded('view.pl').

play:-
    initial_state(5,b,GameState),
    play_aux(GameState).

play_aux(GameState):-
    display_game(GameState),
    read(X),
    (move(GameState, X, NewGameState)
        -> write('Valid move!\n'), play_aux(NewGameState);
        write('Invalid move!\n'), play_aux(GameState)).
