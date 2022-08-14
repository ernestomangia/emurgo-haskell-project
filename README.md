# Minesweeper

Minesweeper game implemented using Haskell with Cabal. This is a mini-project that I made for the 'EMURGO Cardano Developer Associate' course.

The game consists of a board of selectable cells, where some of the cells contain hidden mines. The objective is to clear the board without detonating any mines. 

For a quick overview, check the [Google Minesweeper](https://g.co/kgs/rUvkYU) online.

**Workflow**

1. Make Config
   1. Ask for player name 
   2. Ask for difficulty 
2. Make Game
   1. Make board
      1. Generate random positions for mines
      2. Make cell
      3. Calculate adjacent mines for each cell
3. Run Game
   1. Check game state
      1. If Lost or Won, then
         1. Uncover board
         2. Show board -> Game ends
      2. If On, then 
         1. Show board
         2. Ask for position
         3. Update board
         4. Update game state
         5. Run game (loop)

**Example**
    
    1) Init game - Easy (5 x 5)
    
    Generated board:                            Board displayed to user:
          1     2     3     4     5                   1     2     3     4     5   
                                                                                  
    1     0  |  0  |  1  |  1  |  1  |          1     -  |  -  |  -  |  -  |  -  |
        ------------------------------              ------------------------------
    2     0  |  0  |  1  |  *  |  1  |          2     -  |  -  |  -  |  -  |  -  |
        ------------------------------              ------------------------------
    3     0  |  1  |  1  |  2  |  1  |          3     -  |  -  |  -  |  -  |  -  |
        ------------------------------              ------------------------------
    4     0  |  1  |  *  |  1  |  0  |          4     -  |  -  |  -  |  -  |  -  |
        ------------------------------              ------------------------------
    5     0  |  1  |  1  |  1  |  -  |          5     -  |  -  |  -  |  -  |  -  |
        ------------------------------              ------------------------------

**Legend**

| Display | Description                                    | Cell state    |
| :---    | :---                                           | :---          |
| '-'     | Covered cell                                   | Mine or [0,8] |
| '*'     | Uncovered mine cell                            | Mine          |
| ' '     | Uncovered empty cell with no surrounding mines | 0             |
| [1,8]   | Uncovered empty cell with N surrounding mines  | [1,8]         |

## Getting Started

1. Clone this repo 
2. Open a cmd shell
3. Run `cabal update && cabal install --only-dependencies`

### Docker
Instead of setting up Haskell/Cabal in your local system, you can make use of this [Haskell - Official Image](https://hub.docker.com/_/haskell/) as your development environment.

1. Clone this repo
2. Open a cmd shell
3. Run:
	```bash
	# Creates a container using the haskell:latest official image
	# Mounts the repo folder into the '/app' folder inside the container
	# Opens a bash terminal inside de container
	docker run --name <container name> -it -v <repo absolute path>:/app haskell:latest /bin/bash

	# Example:
	# docker run --name minesweeper-game -it -v H:\haskell-minesweeper:/app haskell:latest /bin/bash
	```
4. Inside the container terminal, run `cd /app` to move to the repo folder
5. Run `cabal update && cabal install --only-dependencies`

All done! Your docker container is ready. Run `cabal run` to play the game.

Read [Usage > Docker](#docker-1) section to know how to start the container once exited.

## Usage
* Run `cabal run` to play the game

### Docker
1. Run `docker start minesweeper-game` to start the container  
2. Run `docker exec -ti minesweeper-game /bin/bash` to enter into the container
3. Once inside, run `cd /app` 
4. Run `cabal run` 

## Development
Some useful commands:
* Run `cabal repl --build-depends random` to load the `random` lib into ghci
* Run `cabal build` to build the game

## License
[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)