# General game solver in Haskell

Games are instances of the class `GameState`. The minimal complete definition for this class must implement the following function:

* `firstplayer` - this Boolean is set to `True` if it's the first (maximizing) player's turn.
* `terminal` - this must be set to `Nothing` for all terminal nodes, and to `Just val` where `val` is the outcome of the game.
* `actions` - these is a list of pairs `(name, result)` where `name` is the move's name, and `branch` is the is sub branch of the game that is reached with the move.

All instances of `GameState` are also `Show`, and their `show` value is a presentation of the game's state to the user.



Better documentation will perhaps happen some day in the future.
