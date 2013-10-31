QCheck
======

QuickCheck inspired property-based testing for OCaml, and combinators to
generate random values to run tests on.

The documentation can be found [here](http://cedeela.fr/~simon/software/qcheck).

The documentation for `Generator` can be found
[here](http://gasche.github.io/random-generator/doc/Generator.html).

## Use

See the documentation. I also wrote
[a blog post](http://cedeela.fr/quickcheck-for-ocaml.html) that explains
how to use it and the design choices.

## Build

First you need to download the submodule `Generator`. Type in a shell:

    $ make submodules

Then:

    $ make

## License

The code is released under the BSD license (see the `LICENSE` file).
