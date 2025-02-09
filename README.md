# 🧶🧵🧶 zwirn 🧶🧵🧶 

zwirn is a live coding language for playing with nested functions of time which contain both continuous and discrete information (values and triggers)

# inspiration

zwirn is an experiment in making the pattern language [TidalCycles](https://tidalcycles.org/) into a small functional language of it's own. while zwirn's internals are quite different from Tidal's, zwirns design owes almost everything to tidals design by [Alex McLean](https://slab.org/).

the internal representation of signals of time was implemented together (and parallel) with [Julian Rohrhuber](https://wertlos.org/~rohrhuber/), in an effort to port tidal to SuperCollider. This can be found in the seperate haskell library [zwirn-core](https://lab.al0.de/martin/zwirn-core).

the implementation of the compiler is inspired by the excellent [Write You a Haskell](https://github.com/sdiehl/write-you-a-haskell) by Stepehn Diehl.

## Installing zwirn

There are currently two ways to play with zwirn:
  * [zwirnzi](https://github.com/polymorphicengine/zwirnzi) - the zwirn zompiler-interpreter
  * [zwirn-loom](https://github.com/polymorphicengine/zwirn-loom) - a compiler-interpreter for zwirn with an experimental editor interface

Zwirnzi is meant to serve as a way to play with zwirn in an editor of your choice, currently there are no official editor extensions - but it shouldn't be too hard to implement one.
