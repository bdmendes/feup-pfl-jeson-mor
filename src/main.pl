:-ensure_loaded('game_logic.pl').
:-ensure_loaded('view.pl').

play:-
    initial_state(5,b,GameState),
    repeat,
    display_game(GameState),
    read(X),
    atom_chars(X, Move),
    (move(GameState, Move, A) -> write('Valid move!\n'); write('Invalid move!\n')),
    fail.
