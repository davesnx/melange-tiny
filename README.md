# melange-tiny

Small replacements of OCaml stdlib modules for [Melange](https://melange.re).
They keep JavaScript bundles small. They also run on native OCaml, so the same
code works on the server and in the browser.

| Library | Module | Replaces |
| --- | --- | --- |
| `melange-tiny.printf` | `Tiny_printf.Printf` | `Stdlib.Printf` |
| `melange-tiny.hashtbl` | `Tiny_hashtbl.Hashtbl` | `Stdlib.Hashtbl` |
| `melange-tiny.random` | `Tiny_random.Random` | `Stdlib.Random` |
| `melange-tiny.md5` | `Tiny_md5.Digest` | `Stdlib.Digest` |

Each module has the name of the stdlib module that it replaces. Write
`open Tiny_printf` and `Printf` is the small one.

## Why

On Melange, one call to a stdlib module can pull in a large part of the
runtime. For example, `Printf.sprintf` brings `CamlinternalFormat`. This table
shows the size of a small program that uses each module. Each program is
bundled with `esbuild --bundle --minify`, then compressed with gzip level 9.
Sizes are in bytes.

| Library | stdlib raw | stdlib gzip | tiny raw | tiny gzip |
| --- | ---: | ---: | ---: | ---: |
| printf | 48535 | 11652 | 4483 | 1856 |
| hashtbl | 64542 | 16970 | 2829 | 1018 |
| random | 56462 | 14756 | 2003 | 969 |
| md5 | 54625 | 13932 | 3149 | 1743 |

The programs are in `size/`. Run `make size` to measure again.

## Install

```sh
opam pin add melange-tiny https://github.com/davesnx/melange-tiny.git
```

## Use with dune

`printf`, `hashtbl` and `random` have one implementation for native
and one for Melange. The native one is the default. The Melange one has the
same name with `.js` at the end.

`md5` has one implementation for both. It has no `.js` library.

### Native

List the library. Dune uses the native implementation.

```dune
(executable
 (name main)
 (libraries melange-tiny.printf melange-tiny.hashtbl))
```

### Melange

In a library, list the library, as on native:

```dune
(library
 (name app)
 (modes melange)
 (libraries melange-tiny.printf melange-tiny.hashtbl))
```

In the `melange.emit` stanza, list the `.js` library for each one:

```dune
(melange.emit
 (target output)
 (libraries app melange-tiny.printf.js melange-tiny.hashtbl.js))
```

A `melange.emit` that uses a module directly can list only the `.js`
library:

```dune
(melange.emit
 (target output)
 (libraries melange-tiny.random.js))
```

If you forget the `.js` library, dune stops with an error that says the
native library is not compatible with Melange.

For code that builds on both, use `(modes :standard melange)` in the
library. The `example/` folder has a library that uses all four modules, a
native test and a Melange program. The test checks that both print the same
text.

## Layout

Each library is a folder. `md5/` has one implementation. The other four
are dune virtual libraries:

```
hashtbl/
  hashtbl.mli         the public module, Tiny_hashtbl.Hashtbl
  hashtbl.ml          the same code for native and Melange
  hashtbl_impl.mli    the virtual module: the part that is not the same
  native/             melange-tiny.hashtbl.native, the default
  js/                 melange-tiny.hashtbl.js, for Melange
  test/               test against Stdlib.Hashtbl on native
  test/js/            the same test, compiled by Melange and run with node
```

The virtual module is `Printf_impl`, `Hashtbl_impl` or `Random_impl`.
Do not use it directly. The public module is not virtual, so a
library that uses it builds on Melange before the emit picks the
implementation.

## Libraries

Each module has a subset of the stdlib module. A function in the subset has
the same name, type and result as in the stdlib, except for the differences
below. The `.mli` files have the full details.

### printf

`Stdlib.Printf` reads the format string at run time. This module has no
format strings. It has:

- `int`, `hex ?upper ?width`, `float_g p`, `char_literal`: one conversion
  each, for use with `^`. They give `%d`, `%x`, `%X`, `%0Nx`, `%.*g` and
  `%C`.
- `lit`, `s`, `d`, `conv f` and `^^`, to build a format. `sprintf` and
  `ksprintf` use it.

```ocaml
open Tiny_printf

let message msg i = Printf.sprintf Printf.(s ^^ lit " at " ^^ d) msg i
```

Differences:

- A format is built from functions, not written as a string.
- No flags except zero padding. No width on `%s` and `%d`. No `%f`, `%e`,
  `%S`, `%B`, `%c`, `%i`, `%u`, `%o`.
- No output to channels: no `printf`, `eprintf`, `fprintf`, `bprintf`.
- `float_g` raises `Invalid_argument` if the precision is negative.
- On Melange, `float_g` uses `toExponential` and `toPrecision`. It gives the
  same text as Melange's `Printf` up to precision 21. Above 21, a number at or
  above `1e21` is in decimal form, as on native.

### hashtbl

`create`, `clear`, `reset`, `replace`, `find`, `find_opt`, `mem`, `remove`,
`length`, `iter`, `fold`. On Melange, the table is a JavaScript `Map`.

Differences:

- Keys must be strings, ints, chars, bools, floats or constant constructors.
  On Melange, other keys are found only if they are the same physical value.
- One binding per key. There is no `add`. Use `replace`.
- `iter` and `fold` visit bindings in insertion order.
- `create` has no `?random` argument.
- On Melange, a `-0.` key comes back as `0.`.

### random

`init`, `self_init`, `bits`, `int`, `float`, `bool`. The generator is
xorshift128 on 32-bit words.

Differences:

- The numbers are not the same as the stdlib numbers for the same seed. They
  are the same on native and on Melange for a seed in the 32-bit range.
- One global state. It is not safe to use from two domains.
- `init` folds the seed to 32 bits.
- On Melange, `self_init` takes its seed from `Math.random`.
- Not for cryptography.

### md5

`string`, `substring`, `to_hex`, `of_hex`, `from_hex`, `compare`, `equal`.
The digest is MD5, as in `Stdlib.Digest`.

Differences:

- No `bytes`, channels, files or BLAKE2 modules.
- On Melange, the input must be shorter than 256 MB.
- On Melange, `Stdlib.Digest.substring` gives a wrong digest when the offset
  is not 0. `Tiny_md5.Digest.substring` gives the correct one.

## Tests

Each library has a test that compares it with the stdlib module. The test
runs on native, and Melange compiles the same test to JavaScript and runs it
with Node.js.

```sh
make test
```

## Develop

```sh
make init    # local opam switch with OCaml 5.4.0, opam and npm dependencies
make build   # dune build @all
make test    # dune build @runtest, needs node
make size    # prints the size table, needs esbuild (npm install)
```

Set `ESBUILD` to use an `esbuild` binary that is not in `node_modules`.

## License

MIT. See [LICENSE.md](LICENSE.md).
