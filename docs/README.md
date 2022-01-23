# Jeson Mor - SICStus Prolog implementation
## Group Information
### Jeson Mor_1
- Bruno Mendes - [up201906166](mailto:up201906166@edu.fe.up.pt)
- José Costa - [up201907216](mailto:201907216@edu.fe.up.pt)

## Contributions

| Name         | Contribution |
|--------------|--------------|
| Bruno Mendes | 50%          |
| José Costa   | 50%          |

## Motivation
This project aims to provide an implementation of the chess variant `Jeson Mor` using the SICStus Prolog language distribution.

## Install and execution
Apart from the standard SICStus Prolog installation, you should run the consult command on the `main.pl` file to load the game predicates.
To run the game simply execute the `play` predicate.

Load the predicates:
```prolog
consult('main.pl').
```

Run the game:
```prolog
play.
```
## Game description
The Jeson Mor chess variant, also known as `nine horses` is typically played in a 9x9 checkboard. Each piece is, as implied by the name, a horse, moving like the chess knight.

The goal of this mongolia originated game is to move one horse to the middle cell and then move it out to any other cell. A plauer can also win by capturing all the opponent's horses.

This information was gathered in the `boardgamegeek` website, available in [this location](https://boardgamegeek.com/boardgame/37917/jeson-mor).

## Implementation
### Internal game representation

At each point, the game is represented by a state, a list with 3 elements [`CurrentPlayer`, `CurrentBoard`, `OldBoard`]. This list is refered throughout the code and documentation as `GameState`.

The `CurrentPlayer` element is an atom comprising either `w` or `b`, identifing the player turn.

The `CurrentBoard` element is a bi-demensional list containing the game board and pieces position's. The board rows are saved as elements of the outer list and the columns as elements of each row list. Column lists are saved from top to bottom and row lists left to right, just as they are printed. The pieces inside the board are represented by either `w` or `b` atoms, while empty cells are represented by an `o` atom.

The `OldBoard` element saves the board in the last turn, being useful for determining the game ending.

Below are some examples of `GameStates` in various situations:

### Prolog `GameState` representations:
#### Initial state:

```
[w, 
    [
        [b,b,b,b,b,b,b,b,b],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [w,w,w,w,w,w,w,w,w]
    ], 
    [
        [b,b,b,b,b,b,b,b,b],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [w,w,w,w,w,w,w,w,w]
    ]
]
```

#### Mid game:

```
[b,
    [
        [b,o,o,o,o,o,b,o,b],
        [o,b,o,b,o,b,o,o,o],
        [o,o,o,b,o,b,o,o,o],
        [o,o,b,o,o,o,o,o,o],
        [o,o,o,o,w,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [w,o,o,o,o,o,w,o,w],
        [w,w,o,w,w,o,o,w,o]
    ],
    [
        [b,o,o,o,o,o,b,o,b],
        [o,b,o,b,o,b,o,o,o],
        [o,o,o,b,o,b,o,o,o],
        [o,o,b,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,w,o,o,o],
        [w,o,o,o,o,o,w,o,w],
        [w,w,o,w,w,o,o,w,o]
        ]
]
```

#### Final state:

```
[w,
    [
        [b,o,o,o,o,o,b,o,b],
        [o,b,o,b,o,b,o,o,o],
        [o,o,o,b,o,b,o,o,o],
        [o,o,b,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,w,o,o,o,o],
        [w,o,o,o,o,o,o,o,w],
        [w,w,o,w,w,o,o,w,o]
    ],
    [
        [b,o,o,o,o,o,b,o,b],
        [o,b,o,b,o,b,o,o,o],
        [o,o,o,o,o,b,o,o,o],
        [o,o,b,o,o,o,o,o,o],
        [o,o,o,o,b,o,o,o,o],
        [o,o,o,o,o,o,o,o,o],
        [o,o,o,o,w,o,o,o,o],
        [w,o,o,o,o,o,o,o,w],
        [w,w,o,w,w,o,o,w,o]
    ]
]

```
## Game display

User interaction happens in 3 cases.  First for gathering game settings, then to interact with the board and lastly the game ending.

### Input

Input happens in the first 2 cases, in game the settings, before the game loop, and then in the game itself.

Input is achieved by using predicates with this structure: 
```prolog
some_predicate:-
    repeat,
    display_prompt,
    read(Something),
    validate(Something)
```

When asking for atom representations this verification is made to ensure the input is treated as such:
```prolog
    catch(char_code(_, Something), _, fail),
```

#### Movement parser

Moves are read from input using algebraic notations (chess like), and then parsed to an internal move representation. The internal representation of a position is given by `Column-Row` (The row with index 0 in the board is the first row displayed). The representation of a move is given by `StartPosition-EndPosition`.

The conversion between I/O and move is achieved using the `parse_move` predicate and between move and positions is made using `parse_square`.

### Output

The several messages throughout tJogadashe game are made relying on the `write` and `format` predicates.

Game settings are diplayed as such:
```
           _                    __  __         
        _ | |___ ___ ___ _ _   |  \/  |___ _ _ 
       | || / -_|_-</ _ \ ' \  | |\/| / _ \ '_|
        \__/\___/__/\___/_||_| |_|  |_\___/_|  

 Bruno Mendes                up201906166@edu.fe.up.pt
 Jose Costa                  up201907216@edu.fe.up.pt

 ( Input the option number to select it ) 
 Choose the type of player for the white knights: 
       1. Human Player                           
       2. Sloppy Computer           
       3. Greedy Computer           
       4. Smart Computer            
|: <your-input>.

 ( Input the option number to select it ) 
 Choose the type of player for the black knights: 
       1. Human Player                           
       2. Sloppy Computer           
       3. Greedy Computer           
       4. Smart Computer            
|: <your-input>.
 Input the desired board size (max.9, should be odd)
|: <your-input>.

```

The board is displayed in the following configuration:
```
  ---------------------
9 | b o o o o o b o b |
8 | o b o b o b o o o |
7 | o o o o o b o o o |
6 | o o b o o o o o o |
5 | o o o o b o o o o |
4 | o o o o o o o o o |
3 | o o o o o o o o o |
2 | w o o o o o w o w |
1 | w w o w w o o w o |
  ---------------------

Computer board evaluation: -1.07
White to play

```
The board display predicate, `display_game(+GameState)`, is flexible printing boards as big as the boards inside the game state. The white pieces are represented in blue and the black pieces in red, whem in game. 

Following it is displayed a prompt that varies depending on the player nature (Human or Computer)
Human:
```
Computer would play: c1-a2
|: <your-input>.
Valid move! / Invalid move|
```

Computer:
```
Computer will play: d1-c3
(Press any key)
```

The games finishes with the following display:
```
       Black has won the game! Congratulations!
           _                    __  __         
        _ | |___ ___ ___ _ _   |  \/  |___ _ _ 
       | || / -_|_-</ _ \ ' \  | |\/| / _ \ '_|
        \__/\___/__/\___/_||_| |_|  |_\___/_|  

 Bruno Mendes                up201906166@edu.fe.up.pt
 Jose Costa                  up201907216@edu.fe.up.pt


yes
```

The first `GameState` is generated by the `initial_state(+Size, -GameState)` predicate that, similarly to the `display_game` predicate, also generates a new state with a board as big as we might wish, even though we limited it to only print boards as big as 9x9 cells, due to performance issues when using bots in the game. The new state has the first player as the white knights, as it is known by the chess rules.


## Move execution
After correctly parsed by the `parse_move` predicate, the move is fed to the `move(+GameState, +Move, -NewGameState)` predicate that executes the move, relying on the `can_move` predicate to verify the validity of the move and in the `replace_nested` predicate to actually generate a new `Board` with the correct atoms in place. The new `GameState` is then assembled.

### Valid moves
Making use of the same predicate as the `can_move` predicate uses to verify if the  move of that piece is valid, we can generate a list of valid moves. 

The `knight_move(+Move)` predicate generates all the possible moves or possible origins of a piece in a position by using backtrack. That is exactly what the `valid_moves(+GameState, -ListOfMoves)` predicate does, lsiting all the possible moves for a given `GameState`.

## BOTS

## Execution flow
The program, after being laucnhed with the `play` predicate, uses the `retractall` predicate for `player/2` and `state/1` to posssibly clear data from previous games. 

Then summons the menu that prompts the user for the game settings (Nature of each player, Human or Computer) and board size.
To keep this choices and save the initial game state generated, makes use of `assertz` predicates.

From here sets up a fail based loop with `repeat`. The loop that will be responsible for checking for the game ending (and if so, for displaying the congratulations message and clearing the knowledge base of asserted predicates, exiting the loop and allowing for progam termination), and performing operations to control the game, generating a new `GameState`, retracting (`retract`) the old state from the knowledge base and saving the new state (`assert`), before failing (`fail`) and using backtrack to reenter the loop.

The operations of the game are displaying the board to the user and retrieving moves (in case of a Human player) or confirmation (in case of a Computer player).

## Conclusion
For impertive minded programmers Prolog proves to be a healthy challenge, forcing us to focus on the problem instead of in the steps to solve it. This isn't always easy, and we found ourselves battling many times for thinking imperatively, gasping for cycles, when declarative programming offered us a compact and logical solution. More often than not, long hours translated in few but powerful lines of code.

### Known Issues
- Sensitive input - Our mechanism for treating input isn't fool-proof. Should be fixed for improved user experience.

