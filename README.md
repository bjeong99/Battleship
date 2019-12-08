# Battleship
## Final project for CS 3110

---

## Features:

### AI:
- Player can choose if they want to play against another human or an AI
- Three difficulties if AI is chosen - easy, medium, hard and insane

### Ship Placement Phase:
- Player can place ship where they want
- Player can remove ship they placed at any point
- Player can randomly place ships at any point in placement if they feel lazy 
   (though they cannot remove those ships that are randomly placed)
- Command to allow player to indicate he/she is finished placing ships

- AI will always lay down ships randomly on map

### Targeting Phase (Classic):
- Player can target anywhere on the map

- If Easy AI is playing, it targets randomly always
- If Medium AI is playing, it will target in large squares around hit locations
- If Hard AI is playing, it will target in linear patterns based on its hits,
   minimizing the amount of targets needed to sunk ship
- If Insane AI is playing, it will probabilistically "hunt" for ships,
   and then switch to the Hard AI algorithm when it finds a ship hit

- displays victory message when one player's ships all sunk

### Targeting Phase (Upgraded):
- Powerups to target ships
- Cannonballs to target multiple squares
- Possibility of targeting multiple times per turn
- ...

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
- when first start off game, it will call "place_ship "player 1"" and will only change to "player 2" when the player one inventory is empty and when player inputs "finish"

### Battleship:
- everything constant
- ship locations
   - when inserting, check if Out of Bounds

