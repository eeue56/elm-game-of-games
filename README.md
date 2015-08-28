# elm-game-of-games

Game of life in Elm, written to test three things:

- Elm's data structures for storing large amount of values (arrays vs lists)
- Elm's canvas drawing speed at both naive levels of drawing (drawing everything) and non-naive (filtering)
- Interaction with images at a pixel level

So far, only the first two have been done. The final step will be done through drawing an image to a hidden canvas, getting the content data array and passing it back to Elm to work with. Native and ports will both be compared for simplicity of usage.

Features at the moment:

    * Press n to go through the next iteration of the board
    * Press p to pause and play the iteration at an interval of 500ms
    * Press s to save the current board as the init board
    * Press r to reset the board to init
    * Click to select places
    * Shift-click to unselect places
    * When the mouse is down, then pause the game until the mouse is up

Planned features:
    
    * Press shift s to save the current board as a "stamp"
    * Choose from a list of stamps and click to place
    * Import images to use as heatmaps 
    * Display the current number of ticks
    * Mutli-coloured based automata 
    * Enable jumping back to the start of the current "run" (different from resets)

See a demo at http://eeue56.github.io/projects/elm/game-of-games/index.html