# Skive
Skive is a parallel, compiled Scheme dialect with single assignment
semantics, implemented in [Racket](http://racket-lang.org/) on the
[SISAL](https://en.wikipedia.org/wiki/SISAL) platform.

The language was designed and implemented for the final bachelor's
project of Nils Van Geele at the Vrije Universiteit Brussel.

Details on the rationale behind the language can be found in the
[literature study](http://skive.nvgeele.be/litstudy.pdf). Details
on the implementation, benchmarks, installation instructions, and
so forth can be found in the
[final thesis](http://skive.nvgeele.be/thesis.pdf).

# Directory Structure
* **src** contains the source files of the compiler and Racket
  integration layer
* **tests** contains all tests
* **examples** contains some Skive and Racket integration examples
* **benchmarks** contains Skive and SISAL code use for benchmarks, as
  well as the benchmark results and accompanying Mathenatica notebook

# Downloads
The latest version of the SISAL compiler and runtime are available
on the [SISAL site](http://sisal.sourceforge.net/). The version
used for development is 14.1, is mirrored
[here](http://skive.nvgeele.be/sisal-14.1.0.tgz).

Version 13 of the Optimizing SISAL Compiler, including many SISAL
code examples, the SISAL reference manual, and other documents
are also available [here](http://skive.nvgeele.be/osc-13.0.4.tgz).

The "IF1: An Intermediate Form for Applicative Languages." paper by
Skedzielewski and Glauert, containing valuable information on IF1,
is made available [here](http://skive.nvgeele.be/if1.pdf). Other
documents on SISAL are freely available on the internet.
