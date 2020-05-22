# Haskell-Project

## Description 
Bouncing Betty is a Pong like game developed by two CS undergraduates from the University of Kansas. Players attempt to maximize the number of times they can bounce a ball off any surface without touching the ground. To adjust the platform use the left and right arrow keys. The game keeps track of how many times the ball bounces off the boundaries. 

## Future Development
* Extending the code base to make it compatible with the chrome web browser 
* Set up a database to keep track of users scores
* Different game modes to allow for custom mods
* Difficulty levels or AI difficulty adjusting based on game play 

## Local Development
### Installation 
1. Install Haskell https://www.haskell.org/downloads/
2. Install required dependencies
```sh
cabal install gloss
cabal install random
```
3. Compile
```sh
ghc main.hs
```
4. Run 
```sh
./main
```
5. Left and right arrow keys to control paddle.

## License 
Miller Bath & Devin Suttles

Licensed under the MIT License; you may not use this file except in compliance with the License. You may obtain a copy of the License at
https://opensource.org/licenses/MIT
