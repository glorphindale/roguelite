Code
    * Refactor this mess

Worldgen
    * A lot of duplication - there should be a function spawn-entity that accepts a dict of probabilities {:rat 3 :troll 2 :ogre 1} and spawns accordingly
Entities
    * using posx/posy for GameObject is inconvenient, should be abstrated to 'position'

UI
    * Add mouse targeting for spells - unclear how to implement properly
    * Add visual feedback for combat - blood splatters, lightning effects

Inventory

Balance
    * EXP gain formula is bad
