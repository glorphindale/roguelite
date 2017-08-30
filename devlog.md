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

25.07.2017
    + Add a crude component system
        * monsters now yell and roam around (sometimes occupying a same space)

09.08.2017
    + Refactor visibility calculation to a proper way
    + Make 'wait' step do proper waiting

10.08.2017
    + Add Dwarf Fortress tileset - looks nicer
    * Refactor old components into data-based approach

11.08.2017
    * Add simple combat allowing player to slay monsters
    * Monsters now strike back
    * Add simple UI for showing player stats
    * Game over state added

12.08.2017
    * Add 8-directional movement
    * Add a sketch to test worldgen
    * Revamped worldgen to get rid of the bugs
    * Tune UI

13.08.2017
    * Add corpses and objects that can be walked over

17.08.2017
    * Tune UI to make it more pleasant
    * Fix NullPointer bug when being slain
    * Add mouselook

21.08.2017
    * Refactored components and moved to a proper file
    * Add primitive inventory with potion chugging

26.08.2017
    * Removed potions from inventory after use
    * Created potions on the map
    * Show potions on the map
    * Allowed picking potions up
    * Improved UI: some help text and shuffling around
    * Add more potions
    * Proper names for items

27.08.2017
    * Add monsters that hunt for player

29.08.2017
    * Broke half the game when trying to add scrolls (I hate that map converts vectors to lazy sequences)
    * Added three mysterious scrolls

30.08.2017
    * Make lightning bolt scroll do actual lightning strikes
    * Refactor message manipulation
    * Fix bugs and some structure refactoring
