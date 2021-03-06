:-ensure_loaded('logic.pl').
:-ensure_loaded('view.pl').    

%%% Validations %%% 

%% valid_player_mode(+Mode)
%
% Checks if the mode corresponds to a valid player mode
%
% @param Mode
valid_player_mode(M):-
   nonvar(M), M >= 1, M =< 4.

%% valid_board_size(+BoasdSize, +Max)
%
% Checks if BoardSize is smaller or the same as Max and if it is odd.
%
% @param Board size
% @param M max size
valid_board_size(BS, M):-
    BS >= 5, BS =< M, BS mod 2 =\= 0.


%%% Main menu %%%
%% menu
%
% Greets the user and gathers game settings, adding them to the knowlegde base
%
:- dynamic player/2, state/1.
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

%% get_player_mode(+Player, -Mode)
%
% Retrieves the game mode for a Player from the user
%
% @param Player to assign mode
% @param Mode choosen
get_player_mode(P, M):-
    repeat,
    display_player_modes(P),
    read(M),
    catch(char_code(_, M), _, fail),
    valid_player_mode(M).

%% get_board_size(-BoardSize, +Max)
%
% Retrieves the bard size from the user
%
% @param BoardSize choosen
% @param Max board size
get_board_size(BS, M):-
    repeat,
    display_board_size_message(M),
    read(BS),
    catch(char_code(_, BS), _, fail),
    valid_board_size(BS,M).

%% play
%
% Clears the knowledge base of previous playthroughts and controls flow with fail based loop
%
play:-
    retractall(player(_,_)),
    retractall(state(_)),
    menu,
    skip_line,
    repeat,
    loop.

%% loop
%
% Predicates to call on loop, verifying for game ending and retriving moves from the player with turn,
%   advancing through states.
% In game ending displays winner greeting and clears knowledge base of asserted prediacates.
%
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


%% read_move(+GameState, -Move)
%
% Reads a Move with valid notation from input and parses it. Called recursively until succeds
%
% @param GameState to supply the game Board to parse the move
% @param Move with valid notation
read_move([CP,CB,OB], M):-
    read(X),
    (parse_move(X, CB, M) -> ! ; (write('Invalid algebraic notation. Try again\n'), read_move([CP,CB,OB], M))).

%% try_move(+GameState, +Move, -NewGameState)
%
% Tries to execute the desired move. If successful outputs a new sate, fails otherwise
%
% @param GameState to apply Move
% @param Move to execute
% @param New game state generated by Move
try_move([CP,CB,OB], M, NGS):-
    (move([CP,CB,OB], M, NGS)-> ! ; (write('Invalid move!\n'), fail)).

%% make_a_move(+GameState, -NewGameState)
%
% Retrieve a Move from the player and execute it. Loops if Human player input is invalid.
% 
%
% @param Game State
% @param New game state
make_a_move([CP,CB,OB], NGS):-
    player(CP, 1), % human player
    display_computer_hint([CP,CB,OB]),
    repeat,
    catch((read_move([CP,CB,OB], M)),_,(write('Invalid input. Try again\n'), fail)),
    try_move([CP,CB,OB], M, NGS),
    skip_line, !.
make_a_move([CP,CB,OB], NGS):-
    player(CP, L), % computer player
    (L = 4 -> L_ is 4; L_ is L - 1),
    choose_move([CP,CB,OB], L_, M),
    display_computer_move([CP,CB,OB], M),
    skip_line,
    try_move([CP,CB,OB], M, NGS).
