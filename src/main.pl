:-ensure_loaded('logic.pl').
:-ensure_loaded('view.pl').

play:-
    initial_state(5,b,GameState),
    repeat,
    display_game(GameState),
    read(X),
    (move(GameState, X, NewGameState) -> write('Valid move!\n'); write('Invalid move!\n')),
    fail.
