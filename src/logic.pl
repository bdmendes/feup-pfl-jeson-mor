:-ensure_loaded('utils.pl').

%% initial_state(+Size, -GameState)
%
% Generates an initial game state, with maximum size 9.
% Uses a list containing 3 elements: Current player, Current board and Old board.
% Old board is initialized as the same as the current board.
% A board is a 2 dimensional list conataining "w's" representing the white knights
%   and "b's" representing black knights and "o's" representing blank spaces.
%
% @param Board size
% @param Initial game state
initial_state(S, [w,B,B]):-
    member(S, [5,7,9]),
    (FP = w; FP = b),
    MS is S - 2,
    replicate_nested(MS, S, o, MB),
    replicate(S, w, WR),
    replicate(S, b, BR),
    append([BR|MB],[WR], B).

%% move(+GameState, ?Move, ?NewGameState)
%
% Executes a move using a game state and returning a new state.
% A move has the form CurrentPosition-NewPosition
% A positon has the form Column-Row.
%
%
% @param Game state
% @param Move to execute
% @param Resulting game state
move([CP,CB,_], SC-SR-EC-ER, [NP,NB,CB]):-
    can_move([CP,CB,_], SC-SR-EC-ER),
    replace_nested(ER, EC, CB, CP, NB_),
    replace_nested(SR, SC, NB_, o, NB),
    next_to_play(CP, NP).

%% can_move(+GameState, ?Move)
%
% Checks if a move is possible given the current game state
%
% @param Game state
% @param Move to verify
can_move([CP,CB,_], SC-SR-EC-ER):-
    nth0_nested(SR, SC, CB, CP),
    nth0_nested(ER, EC, CB, NS),
    NS \= CP,
    knight_move(SC-SR-EC-ER).

% parse_move(?AlgebraicNotation, +Board, ?Move)
%
% Parses a move in algebraic notation (chess like) to a game move, 
%   given a game board, and vice-versa.
%
% @param Algebraic notation
% @param Game board
% @param Move
parse_move(SS-ES, B, SC-SR-EC-ER):-
    nonvar(SS), nonvar(ES), !,
    length(B, BS),
    atom_chars(SS, SS_),
    atom_chars(ES, ES_),
    parse_square(SS_, BS, SC-SR),
    parse_square(ES_, BS, EC-ER).
parse_move(SS-ES, B, SC-SR-EC-ER):-
    length(B,BS),
    char_code('a', AC),
    char_code('1', ZC),
    ISR is BS - SR - 1,
    IER is BS - ER - 1,
    SCC is AC + SC,
    ECC is AC + EC,
    SRC is ZC + ISR,
    ERC is ZC + IER,
    atom_codes(SS, [SCC,SRC]),
    atom_codes(ES, [ECC,ERC]).

%% parse_square(+AlgebraicNotation, +BoardSize, ?Square)
%
%  Parses a move in algebraic notation (chess like) to a game move, 
%   given a game board size.
%
% @param Algebraic notation
% @param Board size
% @param Square positon
parse_square([H|T], BS, C-R):-
    char_code('a', AC),
    char_code(H, CC),
    C is CC - AC,
    catch(number_chars(IR,T), _, fail),
    R is BS - IR.

%% next_to_play(?CurrentToPlay, ?Next)
%
% Indicates who is the next player given the current player and
%   vice-versa.
%
% @param Current player
% @param Next player
next_to_play(w,b).
next_to_play(b,w).

%% knight_move(+Move)
%
% Checks if the given move is a knight move
%
% @param Move
knight_move(SC-SR-EC-ER):-
    member([DC,DR],[[1,-2],[1,2],[-1,-2],[-1,2],[2,-1],[2,1],[-2,-1],[-2,1]]),
    EC is SC + DC,
    ER is SR + DR.

%% center_square(+BoardSize, ?Row, ?Column)
%
% Indicates/verifies is the coordinates for the center square
%
% @param Board size
% @param Row
% @param Column

center_square(BS, R, C):-
    K is BS div 2,
    R = K,
    C = K.

%% game_over(+GameState, ?Winner)
%
% Indicates/verifies the if the given state corresponds to a game over state with the respective winner.
%
% @param Game state
% @param Winner
game_over([CP,CB,_], W):-
    valid_moves([CP,CB,_], []),!,
    next_to_play(CP,W).
game_over([_,CB,OB], W):-
    nonvar(CB), nonvar(OB),
    length(CB, BS),
    center_square(BS, R, C),
    nth0_nested(R, C, CB, o),
    nth0_nested(R, C, OB, W),
    W \= o.

%% valid_moves(+GameState, ?ListOfMoves)
%
% Indicates all the valid knight moves for a given game state.
%
% @param Game state
% @param List of valid moves
valid_moves([CP,CB,_], L):-
    findall(SC-SR-EC-ER, can_move([CP,CB,_], SC-SR-EC-ER), L).
