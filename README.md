# Program Analysis Examples
**CIS 700 (Fall '19) by Kris Micinski at Syracuse U**
**Notes for "Program Analysis: Foundations and Applications"**

This repository contains a broad array of abstract machines and
abstract interpreters, along with conversion routines (e.g., ANF/CPS
conversion) for a variety of languages (mostly the lambda calculus
extended with various primitives).

- `anf-convert.rkt` - Sample ANF converter over a subset of Scheme.
- `cek.rkt` - A simple CEK machine, done via ANF and non-ANF
- `church.rkt` - A church-encoder
- `cps-machine` - Concrete interpreter for CPS lambda calculus
- `cps-convert.rkt` - A CPS converter for a Scheme subset
- `cesk.rkt` - A CESK machine for direct-style lambda calculus
