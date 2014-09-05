haskellGames4iPad
=================

Small collection of games written in "Raskell"'s iPad environment.

All games were written on iPad2 using the iPad application named "Raskell". They run on my my iPad2 with the version of Raskell current at the 5th of September 2014 and the version before that. They may run on any iPad / iPhone / iPod touch where "Raskell" is installed and possibly on any computer insid a Hugs interpreter. As of this writing, Hugs inside "Raskell" was using Haskell98, not Haskell2010.

To configure, create Tools.hs and LocalSettings.hs in your "Raskell" environment's root folder / desktop. Edit LocalSttings.hs to adjust to your screen size.

Labyrinth.hs and Numbers.hs only need one file, Labyrinth3d.hs needs Labyrinth3d.map as input file. To start, load the hs file in "Raskell" and press "play" button and eventually select the stdin window, so keypresses are recognized. Every game is controlled with

 i
j k
 m

for up, left, right and down. 'x' exits.

Labyrinth: Navigate the small snake to the bottom line, exiting the maze.

Labyrinth3d: Navigate the "ship" through the maze. It only moves forwards and backwards, turn left or rigt to change direction.

Numbers: Order the numbers from 1 to fifteen. Use navigation to push a number to left / right / up / down into the empty space (the two dots).

