# Purpose

Each day I'll complete some small, targetted coding exercises in Haskell.

By "small" I mean that they'll be more along the lines of "implement Functor"
than "Conway's Game of Life".

By "targetted" I mean that they'll be tasks that are near the edge of my current
understanding of Haskell. It will take some time to figure out where the right
target is for me.

The intention of doing these exercises is to increase muscle memory around
fundamental Haskell coding tasks.

Over time it would be nice to extract both a more detailed description of this
practice and a library of these exercises that can be used by others.

# Current workout detail

- Write `fmap` for a list. A function that goes from `(a -> b) -> [a] -> [b]`
- Write `fmap` for `Maybe`. A function that goes from `(a -> b) -> [a] -> [b]`
- Create an Algebraic Data Type (ADT)
- Write a function that goes from that ADT to a `String` (with unique values for
  each)
- Write a polymorphic ADT
- Write a function to extract the wrapped value from that ADT
- Implement my own version of Maybe
- Implement Functor for that type
- Implement my own version of List
- Implement Functor for that type

### See [ChangeLog.md](ChangeLog.md) for history
