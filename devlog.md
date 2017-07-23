30.06.2017
    * Draw both types of tiles

1.07.2017
    * Try to make real walls: fighting with coordinates and 'walls are not working' :( Mixing rows and columns, trying to understand why Steve Losh map creation does not work.

2.07.2017
    ~~TODO: find a way to move colors out of the game.clj module~~
    TODO: unify player and objects, find a way to move player via the :objects list - is it really needed?
    * Moved out colors from the game.clj, tune some colors

06.07.2017:
    + Fixed a bunch of stupid bugs in rendering and printing

07.07.2017:
    + Added a proper room generation with corridors between, looks good enough

11.07.2017:
    + Add simple torch lighting
    + Make room gen create a nice closed dungeon

15.07.2017:
    + Attempt to add raycasting for FOV (http://www.roguebasin.com/index.php?title=Simple_and_accurate_LOS_function_for_BlitzMax ), so much bugs
    = Raycasting to nearby cells produces funky results (indexes on path)

16.07.2017:
    + Fix raycasting (result is probable, but some artifacts remain)
    = Add exploration

18.07.2017:
    + Added exploration, tiles will become slightly visible when are seen once
    + Refactor FOV handling, make it more functional and reduce overhead

22.07.2017:
    + Add more monster types, generate one monster per room
    + Make monsters block player`s movement

23.07.2017
    + Refactor code into smaller chunks
