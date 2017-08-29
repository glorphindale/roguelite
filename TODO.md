Code
    * Refactor this mess
    * calling refresh-visibility on new world creation is kinda hacky - should be included into the world generation

Worldgen
    * A lot of duplication - there should be a function spawn-entity that accepts a dict of probabilities {:rat 3 :troll 2 :ogre 1} and spawns accordingly
Entities
    * using posx/posy for GameObject is inconvenient, should be abstrated to 'position'

Inventory
    * Items should start at 1


