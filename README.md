# Battleship
## Final project for CS 3110

---

### State:
- records hits on ship and changes on the game state
- keeps tracks of which player's turn it is (player 1 or player 2 and determines what gets changed)

### Command:
- parses various commands
- fire, quit, place (x,y, orientation), remove, random, remaining (remaining players on own side and enemy side)

### Main:
- run actual game and read from terminal

### Battleship:
- everything constant
- ship locations
   - when inserting, check if Out of Bounds
