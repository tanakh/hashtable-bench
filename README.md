hashtable-bench
===============

Benchmarks of some haskell's (and some others) hashtable implementations.

It containes benchmarks of:

* containers (Data.Map.Strict)
* unordered-containers (Data.HashMap.Strict)
* base (Data.HashTable, **Deprecated**)
* hashtables (Data.Hashtable.IO.*)
* C++ std::map and std::unordered_map

How to Execute
==============

~~~ {.bash}
$ cabal update
$ cabal install --only-dependencies --enable-benchmark
$ cabal configure --enable-benchmark
$ cabal build --ghc-options='-rtsopts -O3 -fllvm'
$ cabal bench --benchmark-options='+RTS -K16m -RTS -oresult.html'
~~~

It generates the graphical report `result.html`.
