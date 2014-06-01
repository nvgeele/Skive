# Skive

Skive is a parallel, compiled Scheme dialect with single assignment
semantics, implemented in [Racket](http://racket-lang.org/) on the
[SISAL](https://en.wikipedia.org/wiki/SISAL) platform.

The language was designed and implemented for the final bachelor's
project of Nils Van Geele at the Vrije Universiteit Brussel.

More details are available in the
[literature study](http://skive.nvgeele.be/litstudy.pdf) or in the
[final thesis](http://skive.nvgeele.be/thesis.pdf).

Code and results of benchmarks are also
[available](http://skive.nvgeele.be/benchmarks.tgz).

# Directory Structure
* **src** contains the source files of the compiler and Racket
  integration layer
* **tests** contains all tests
* **examples** contains some Skive and Racket integration examples
* **benchmarks** contains Skive and SISAL code use for benchmarks, as
  well as the benchmark results
