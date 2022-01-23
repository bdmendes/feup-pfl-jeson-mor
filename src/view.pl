:-ensure_loaded('bot.pl').

%% display_game(+GameState)
%
% Displays the CurentBoard in a game state and its evaluation. 
%
% @param Game state
display_game([CP,B,OB]):-
    length(B,BS),
    BW is BS*2 + 3,
    CW is BS*2,
    write('\n    '),
    char_code('a',AC),
    display_column_coords(CW, AC),
    write('\n  '),
    display_separator(BW),
    put_char('\n'),
    display_board(B),
    write('  '),
    display_separator(BW),
    write('\n\n'),
    board_evaluation([CP,B,OB],E),
    format('Computer board evaluation: ~2F\n', [E]),
    (CP = b -> write('Black to play\n\n'); write('White to play\n\n')).

%% display_board(+Board)
%
% Displays a game board
%
% @param Board to display
display_board([]).
display_board([H|T]):-
    length([H|T],RN),
    write(RN),
    write(' |'),
    display_row(H),
    write(' |\n'),
    display_board(T).

%% display_row(+Row)
%
% Displays a row of the game board
%
% @param Row in a game board
display_row([]).
display_row([w|T]):-
    put_char(' '),
    switch_color(blue),
    put_char('w'),
    switch_color(default),
    display_row(T), !.
display_row([b|T]):-
    put_char(' '),
    switch_color(red),
    put_char('b'),
    switch_color(default),
    display_row(T), !.
display_row([H|T]):-
    put_char(' '),
    put_char(H),
    display_row(T).

%% display_separator(+Size)
%
% Displays a horizontal separator of the specified width
%
% @param Size
display_separator(0):- !.
display_separator(S):-
    put_char('-'),
    NS is S - 1,
    display_separator(NS).

%% display_column_coords(+Width, +Char):-
%
% Display column coordinate in algebraic notation
%
% @param Width of the board
% @param First char 
display_column_coords(0, _):- !.
display_column_coords(W, C):-
    (W mod 2 =:= 0 ->
        put_code(C), NC is C + 1;
        put_char(' '), NC is C),
    NW is W - 1,
    display_column_coords(NW, NC).

%% display_greeting
% 
% Displays the game logo and contact info
%
display_greeting:-
    write('           _                    __  __         \n'),
    write('        _ | |___ ___ ___ _ _   |  \\/  |___ _ _ \n'),
    write('       | || / -_|_-</ _ \\ \' \\  | |\\/| / _ \\ \'_|\n'),
    write('        \\__/\\___/__/\\___/_||_| |_|  |_\\___/_|  \n\n'),
    write(' Bruno Mendes                up201906166@edu.fe.up.pt\n'),
    write(' Jose Costa                  up201907216@edu.fe.up.pt\n').

%% display_player_modes(+Color)
%
% Display player mode choice prompt
%
% @param Color of the player this prompt refers to
display_player_modes(K):-
    write('\n'),
    write(' ( Input the option number to select it ) \n'),
    (K == 'w' -> M = 'White'; M = 'Black'),
    format(" Choose the type of player for ~w: \n", [M]),
    write('       1. Human Player                           \n'),
    write('       2. Sloppy Computer           \n'),
    write('       3. Greedy Computer           \n'),
    write('       4. Smart Computer            \n').

%% display_board_size_message(+Max)
%
% Display board size choice prompt
%
% @param Max board size
display_board_size_message(M):-
    format(" Input the desired board size ( max.~w, must be odd )\n",[M]).

%% display_computer_hint(+GameState)
%
% Displays the move the greedy algorithm would make to aid the player in its move decision
%
% @param Game state
display_computer_hint([CP,CB,_]):-
    choose_move([CP,CB,_], 2, M),
    parse_move(PM,CB,M),
    format('Greedy Computer would play: ~w', [PM]),nl,
    write('( Input your move in algebraic notation as above )'),nl.

%% display_computer_move(+GameState, +Move)
%
% Displays the move the computer player will perform
%
% @param Game state
display_computer_move([_,B,_], M):-
    parse_move(PM,B,M),
    format('Computer will play: ~w\n', [PM]),
    write('(Press return)\n').

%% display_winner_greeting(+Winner)
%
% Displays a string announcing the game winner and the game greeting
%
% @param Winner to congratulate
display_winner_greeting(Winner):-
    nl,nl,
    (Winner == 'w'-> W = 'White' ; W = 'Black'),
    format("       ~w has won the game! Congratulations!\n", [W]),
    display_greeting, nl, nl.

%% switch_color(+Color)
%
% Switches the characters color
%
% @param Color to change to
switch_color(blue):- write('\33\[1;34m').
switch_color(red):- write('\33\[1;31m').
switch_color(default):- write('\33\[0m').
