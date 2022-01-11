:-ensure_loaded('logic.pl').
:-ensure_loaded('view.pl').

play:-
    initial_state(5,w,GameState),
    play_aux(GameState).

/* play_aux([CurrentToPlay, _]):-
    has_won(CurrentToPlay, Name),!,
    format("~w has won the game!\n", [Name]).
 */
play_aux(GameState):-
    display_game(GameState),
    read(X),
    (move(GameState, X, NewGameState)
        -> write('Valid move!\n'), play_aux(NewGameState);
        write('Invalid move!\n'), play_aux(GameState)).
