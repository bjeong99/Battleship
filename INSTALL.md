# Battleship
## Final project for CS 3110

---

## System Requirements:

- Graphics, Emoji, ANSITerminal and OUnit are required packages
- XQuartz for MacOS is also required to work with Graphics
- X11 (I hope) works for windows

## To extract docs
- make docs

## To Compile:
- make build

## To Test:
- make test

## To Play standard game that we demoed in section
- make play

## To Play gui game (Easter egg)
- make visual

## Features:

### AI:
- Player can choose if they want to play against another human or an AI
  - type in yes or no or quit
- Four difficulties if AI is chosen - easy, medium, hard and insane
  - type in easy, medium, hard or insane 

### Ship Placement Phase:
- Player can place ship where they want
- Player can remove ship they placed at any point
- Player can randomly place ships at any point in placement if they feel lazy 
   (though they cannot remove those ships that are randomly placed)
- Command to allow player to indicate he/she is finished placing ships

- type in manually e.g.
  [x] [y] down battleship 
  for example 1 1 down aircraftcarrier
  or 2 5 right submarine 
  or A 6 up cruiser
  or h 9 left destroyer

  (No commas required)

- when you type in manually, type
  finish 
  to complete 

- to remove ships randomly (which you cannot do with random)
  random [ship] where ship could be 
    battleship, cruiser, destroyer, aircraftcarrier

- type in random to randomly lay down ships (at any point)
  e.g. just random

- type in clear to clear
  - two clears required to move to next player

- AI will always lay down ships randomly on map

### Targeting Phase (Classic):
- Player can target anywhere on the map

- If Easy AI is playing, it targets randomly always
- If Medium AI is playing, it will target in large squares around hit locations
- If Hard AI is playing, it will target in linear patterns based on its hits,
   minimizing the amount of targets needed to sunk ship
- If Insane AI is playing, it will probabilistically "hunt" for ships,
   and then switch to the Hard AI algorithm when it finds a ship hit
   We attribute the insane strategy to http://datagenetics.com/blog/december32011/index.html

- displays victory message when one player's ships all sunk

### Targeting Phase (Upgraded):
#### Powerups to target ships
- Square shot to target multiple squares
- Rehit to target on multiple turns
- Instakill to sink a ship immediately if hit

- to use powerups, say:
  use [powerup] at [x] [y] 
  where powerup is one of "rehit", "square", "instakill"
  and x and y is some valid coordinate

### GUI (Easter egg)
- used OCaml Graphics module to implement gui for battleship
- only functionality to lay down ships randomly, no AI, no powerups
- players 1 and 2 keep guessing until one of the players destroys all the others ships
- no warning of when ship is sunk
- Press enter to advance turn as a next button
- press on the left hand side to target grid as player 2 and press on grid on right hand side to target as player 1
- cannot target two locations in one turn
- red displays a hit for player 1
- magenta is a hit for player 2
- white is a miss
- currently, 2.0 second time delay implemented to change players between targeting phases, if you want to change, change the delay to whatever you want by changing the c_TIME_DELAY constant at the top of visual.ml to whatever value
- displays victory message for winner

## Design Philosophy (Module Layout):

### AI:
- records state information about where AI has targeted and which locations it has not targeted
- different targeting strategies depending on the AI difficulty

### State:
- records hits on ship and changes on the game state
- keeps tracks of which player's turn it is (player 1 or player 2 and determines what gets changed)
- coordinates with AI state information to keep game in sync

### Command:
- Game controller
- parses various commands
- fire, quit, place (x,y, orientation), remove, random, remaining (remaining players on own side and enemy side)

### Main:
- This is view in the MVC paradigm
- run actual game and read from terminal
- when first start off game, it will call "place_ship "player 1" and will only change to "player 2" when the player one inventory is empty and when player inputs "finish"

### Battleship:
- everything constant
- ship locations
   - when inserting, check if Out of Bounds

### Visual: 
- independent of all the other modules
- works with battleship
- the visual model is both a model (for state) and a view

