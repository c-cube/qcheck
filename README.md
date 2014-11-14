QCheck
======

QuickCheck inspired property-based testing for OCaml, and combinators to
generate random values to run tests on.

The documentation can be found [here](http://cedeela.fr/~simon/software/qcheck).

Note that @gasche's [generator library](http://gasche.github.io/random-generator/doc/Generator.html)
can be useful too, for generating random instances.

## Use

See the documentation. I also wrote
[a blog post](http://cedeela.fr/quickcheck-for-ocaml.html) that explains
how to use it and some design choices.

## Build

    $ make

You can use opam:

    $ opam install qcheck

## License

The code is released under the BSD license (see the `LICENSE` file).
