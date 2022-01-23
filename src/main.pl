:-ensure_loaded('logic.pl').
:-ensure_loaded('view.pl').    

%%% Validations %%% 

valid_player_mode(M):-
   nonvar(M), M >= 1, M =< 4.

valid_board_size(BS, M):-
    BS >= 5, BS =< M, BS mod 2 =\= 0.


%%% Main menu %%%
menu:-
    switch_color(default),
    display_greeting,
    get_player_mode(w, MW),
    assertz(player(w, MW)),
    get_player_mode(b, MB),
    assertz(player(b, MB)),
    get_board_size(BS, 9),
    initial_state(BS, GS),
    assertz(state(GS)).

get_player_mode(P, M):-
    repeat,
    display_player_modes(P),
    read(M),
    catch(char_code(_, M), _, fail),
    valid_player_mode(M).

get_board_size(BS, M):-
    repeat,
    display_board_size_message(M),
    read(BS),
    catch(char_code(_, BS), _, fail),
    valid_board_size(BS,M).

:- dynamic player/2, state/1.
play:-
    retractall(player(_,_)),
    retractall(state(_)),
    menu,
    skip_line,
    repeat,
    loop.

loop:-
    state(GS),
    game_over(GS, Winner), !,
    display_winner_greeting(Winner),
    retractall(state(_)),
    retractall(player(_,_)).
loop:-
    retract(state(GS)),
    display_game(GS),
    make_a_move(GS, NGS),
    assertz(state(NGS)),
    fail.

try_move([CP,CB,OB], M, NGS):-
    (move([CP,CB,OB], M, NGS)-> (write('Valid move!\n'), !) ; (write('Invalid move!\n'), fail)).

make_a_move([CP,CB,OB], NGS):-
    player(CP, 4),
    choose_move([CP,CB,OB], 4, M),
    display_computer_move([CP,CB,OB], M),
    skip_line,
    try_move([CP,CB,OB], M, NGS), !.

make_a_move([CP,CB,OB], NGS):-
    player(CP, L),
    L_ is L -1,
    choose_move([CP,CB,OB], L_, M),
    display_computer_move([CP,CB,OB], M),
    skip_line,
    try_move([CP,CB,OB], M, NGS).

make_a_move([CP,CB,OB], NGS):-
    player(CP, 1),
    display_computer_hint([CP,CB,OB]),
    repeat,
    read_move([CP,CB,OB], M),
    try_move([CP,CB,OB], M, NGS),
    skip_line, !.
