haskellGames4iPad
=================

Small collection of games written in "Raskell"'s iPad environment.

All games were written on iPad2 using the iPad application named "Raskell". They run on my my iPad2 and iPhone 4S with Raskell version from 1.0.11 (references local ressources and imports local modules), tested with 1.0.12 and 1.0.13. They may run on any iPad / iPhone / iPod touch where "Raskell" is installed and possibly on any computer inside a Hugs interpreter. As of this writing, Hugs inside "Raskell" was using Haskell98, not Haskell2010. Tests inside ghc did work, but getChar needs an annoying additional enter after every keypress. Use start.sh to start one of the games and change terminal settings before and reset afterwards.

To configure, create Tools.hs and LocalSettings.hs in your "Raskell" environment's root folder / desktop. Edit LocalSttings.hs to adjust to your screen size.

Labyrinth.hs and Numbers.hs only need one file, Labyrinth3d.hs needs Labyrinth3d.map as input file. To start, load the hs file in "Raskell" and press "play" button and eventually select the stdin window, so keypresses are recognized. Every game is controlled with

 i
j k
 m

for up, left, right and down. 'x' exits.

Labyrinth: Navigate the small snake to the bottom line, exiting the maze.

Labyrinth3d: Navigate the "ship" through the maze. It only moves forwards and backwards, turn left or rigt to change direction.

Numbers: Order the numbers from 1 to fifteen. Use navigation to push a number to left / right / up / down into the empty space (the two dots).

