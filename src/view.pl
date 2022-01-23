:-ensure_loaded('bot.pl').

% display_game(+GameState)
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

% display_board(+Board)
display_board([]).
display_board([H|T]):-
    length([H|T],RN),
    write(RN),
    write(' |'),
    display_row(H),
    write(' |\n'),
    display_board(T).

% display_row(+Row)
display_row([]).
display_row([H|T]):-
    put_char(' '),
    put_char(H),
    display_row(T).

% display_separator(+Size)
display_separator(0):- !.
display_separator(S):-
    put_char('-'),
    NS is S - 1,
    display_separator(NS).

% display_bottom_coords(+Width, +Char):-
display_column_coords(0, _):- !.
display_column_coords(W, C):-
    (W mod 2 =:= 0 ->
        put_code(C), NC is C + 1;
        put_char(' '), NC is C),
    NW is W - 1,
    display_column_coords(NW, NC).

display_greeting:-
    write('           _                    __  __         \n'),
    write('        _ | |___ ___ ___ _ _   |  \\/  |___ _ _ \n'),
    write('       | || / -_|_-</ _ \\ \' \\  | |\\/| / _ \\ \'_|\n'),
    write('        \\__/\\___/__/\\___/_||_| |_|  |_\\___/_|  \n\n'),
    write(' Bruno Mendes                up201906166@edu.fe.up.pt\n'),
    write(' Jose Costa                  up201907216@edu.fe.up.pt\n').


display_white_knights_message:-
    write(' Chose the type of player for the white knights: \n').

display_black_knights_message:-
    write(' Chose the type of player for the black knights: \n').

display_player_modes(K):-
    write('\n'),
    write(' ( Input the option number to select it ) \n'),
    (K == 'w' -> display_white_knights_message; display_black_knights_message),
    write('       1. Human Player                           \n'),
    write('       2. Sloppy Computer           \n'),
    write('       3. Greedy Computer           \n'),
    write('       4. Smart Computer            \n').


display_board_size_message(M):-
    format(" Input the desired board size (max.~w, should be odd)\n",[M]).

display_computer_hint([CP,CB,_]):-
    choose_move([CP,CB,_], 2, M),
    parse_move(PM,CB,M),
    format('Computer would play: ~w', [PM]),nl.

display_computer_move([_,B,_], M):-
    parse_move(PM,B,M),
    format('Computer will play: ~w\n', [PM]),
    write('(Press any key)\n').

display_winner_greeting(Winner):-
    nl,nl,
    (Winner == 'w'-> W = 'White' ; W = 'Black'),
    format("       ~w has won the game! Congratulations!\n", [W]),
    display_greeting, nl, nl.