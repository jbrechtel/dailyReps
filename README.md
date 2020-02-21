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

- Write `foldl` for a list. A function that goes from `(b -> a -> b) -> b -> [a] -> b`
- Write a polymorphic ADT
- Write a function to extract the wrapped value from that ADT
- Implement my own version of Maybe
- Implement Functor for that type
- Implement Applicative for that type
- Implement Alternative for that type
- Implement Foldable for that type
- Implement Semigroup for that type
- Implement Monoid for that type

### See [ChangeLog.md](ChangeLog.md) for history
