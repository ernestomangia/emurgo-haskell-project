# Minesweeper

Minesweeper game implemented using Haskell with Cabal. 

This is a mini-project that I made for the 'EMURGO Cardano Developer Associate' course.

## Setup

1. Clone this repo 
2. Open a cmd shell
3. Run `cabal update && cabal install --only-dependencies`

### Setup using Docker
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

### Usage using Docker
1. Run `docker start minesweeper-game` to start the container  
2. Run `docker exec -ti minesweeper-game /bin/bash` to enter into the container
3. Once inside, run `cd /app` 
4. Run `cabal run` 

## License
[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0)