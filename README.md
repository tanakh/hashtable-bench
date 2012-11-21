hashtable-bench
===============

Benchmarks of some haskell's (and some others) hashtable implementations.

It containes benchmarks of:

* [containers](http://hackage.haskell.org/package/containers) (Data.Map.Strict)
* [unordered-containers](http://hackage.haskell.org/package/unordered-containers) (Data.HashMap.Strict)
* [base](http://hackage.haskell.org/package/base) (Data.HashTable, **Deprecated**)
* [hashtables](http://hackage.haskell.org/package/hashtables) (Data.Hashtable.IO.*)
* [hashmap](http://hackage.haskell.org/package/hashmap) (Data.HashMap)
* [Judy](http://hackage.haskell.org/package/judy) (Data.Judy)
    * binding to C library libjudy
* C++ std::map and std::unordered_map

How to Execute
==============

~~~ {.bash}
$ cabal update
$ cabal install --only-dependencies --enable-benchmark --ghc-options='-O3 -fllvm' -fsse41
$ cabal configure --enable-benchmark
$ cabal build --ghc-options='-rtsopts -O3 -fllvm'
$ cabal bench --benchmark-options='+RTS -K16m -RTS -oresult.html'
~~~

It generates the graphical report `result.html`.
