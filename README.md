# Minesweeper

Minesweeper game implemented using Haskell with Cabal. This is a mini-project that I made for the 'EMURGO Cardano Developer Associate' course.

The game consists of a board of selectable cells, where some of the cells contain hidden mines. The objective is to clear the board without detonating any mines. 

For a quick overview, check the [Google Minesweeper](https://g.co/kgs/rUvkYU) online.
  
    Example of a 5x5 board:

    1) Init game

    Generated board:        Board displayed to user:
       1 2 3 4 5               1 2 3 4 5
    1  0 0 1 1 1            1  - - - - -
    2  0 0 1 * 1            2  - - - - -
    3  0 1 1 2 1            3  - - - - -
    4  0 1 * 1 0            4  - - - - -
    5  0 1 1 1 0            5  - - - - -

    2) User selects cell (1, 5)

       1 2 3 4 5
    1  - - - - 1
    2  - - - - -
    3  - - - - -
    4  - - - - -
    5  - - - - -
    
    3) User selects cell (2, 1)

       1 2 3 4 5
    1  - - - - 1
    2    - - - -
    3  - - - - -
    4  - - - - -
    5  - - - - -

    4) ...

| Cell value   | Display value  | Description                                             |
| :---         | :---           | :---                                                    |
| [0,9]        | '-'            | Covered cell                                            |
| 0            | ' '            | Uncovered empty cell with no surrounding mines          |
| [1,8]        | [1,8]          | Uncovered empty cell with N surrounding mines (up to 8) |
| 9            | '*'            | Uncovered mine cell                                     |

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

Read [Usage using Docker](#usage-using-docker) to know how to start the container once exited.

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