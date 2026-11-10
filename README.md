# melange-tiny

Small drop-in replacements of OCaml stdlib modules for
[Melange](https://melange.re). Each module has the signature and the behaviour
of the stdlib module. Only the size of the JavaScript bundle is different.

| Library | Module | Replaces |
| --- | --- | --- |
| `melange-tiny.printf` | `Tiny_printf.Printf` | `Stdlib.Printf` |
| `melange-tiny.hashtbl` | `Tiny_hashtbl.Hashtbl` | `Stdlib.Hashtbl` |
| `melange-tiny.random` | `Tiny_random.Random` | `Stdlib.Random` |
| `melange-tiny.md5` | `Tiny_md5.Digest` | `Stdlib.Digest` |

Each module has the name of the stdlib module that it replaces. Write
`open Tiny_printf` and `Printf` is the small one. No other change to your code
is necessary.

```ocaml
open Tiny_printf

let message msg i = Printf.sprintf "%s at %d" msg i
```

## Why

On Melange, one call to a stdlib module can pull in a large part of the
runtime. For example, `Printf.sprintf` brings `CamlinternalFormat`, and
`Hashtbl` brings `Domain`, `Random` and then `Printf`. This table shows the
size of a small program that uses each module. The stdlib program and the tiny
program have the same source, except the module path. Each program is bundled
with `esbuild --bundle --minify`, then compressed with gzip level 9. Sizes are
in bytes.

| Library | stdlib raw | stdlib gzip | tiny raw | tiny gzip |
| --- | ---: | ---: | ---: | ---: |
| printf | 48535 | 11652 | 15400 | 4838 |
| hashtbl | 64542 | 16970 | 8591 | 2961 |
| random | 56462 | 14756 | 4568 | 2340 |
| md5 | 54625 | 13932 | 3650 | 1858 |

The programs are in `size/`. Run `make size` to measure again.

## Same behaviour

- On native OCaml, each module is the stdlib module (`include Stdlib.Printf`).
- On Melange, each module is a small implementation with the same results as
  the Melange stdlib module: the same output text, the same exceptions, the
  same `Hashtbl` iteration order, the same `Random` numbers for a seed.
- The signature is the signature of the stdlib of the platform. The Melange
  stdlib and the native stdlib do not have the same functions in all cases
  (for example `Random.split` is native only), and each platform gets its own
  set.
- The stdlib gives different results on native and on Melange in some cases
  (`Random` numbers, 32-bit integers). The tiny modules do the same.

A test for each module runs on native and in Node.js. In Node.js it compares
the tiny module with the Melange stdlib module call by call. A signature check
fails the build if a signature is different from the stdlib one.

There is one difference. On Melange, `Stdlib.Digest.substring` and
`Stdlib.Digest.subbytes` give a wrong digest when the offset is not 0.
`Tiny_md5.Digest` gives the correct digest, the same as on native.

## Install

```sh
opam pin add melange-tiny https://github.com/davesnx/melange-tiny.git
```

## Use with dune

Each library has one implementation for native and one for Melange. The native
one is the default. The Melange one has the same name with `.js` at the end.

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

Each library is a folder and a dune virtual library:

```
hashtbl/
  hashtbl.mli         the public module, Tiny_hashtbl.Hashtbl
  hashtbl.ml          include Hashtbl_impl
  hashtbl_impl.mli    the virtual module, with the stdlib signature
  native/             melange-tiny.hashtbl.native, the default: Stdlib.Hashtbl
  js/                 melange-tiny.hashtbl.js, the small one for Melange
  test/               test against Stdlib.Hashtbl
  test/js/            the same test, compiled by Melange and run with node
```

The virtual module is `Printf_impl`, `Hashtbl_impl`, `Random_impl` or
`Digest_impl`. Do not use it directly. The public module is not virtual, so a
library that uses it builds on Melange before the emit picks the
implementation.

## How each one is small

- `printf`: reads the format value that the compiler makes from the format
  string, without `CamlinternalFormat`.
- `hashtbl`: the stdlib algorithm without `Domain` and `Random`. With
  `~random:true` or after `randomize`, the seed comes from `Math.random`.
- `random`: the generator of the Melange stdlib with its own small MD5 for the
  seed. The 64-bit runtime is in the bundle only when you call `int64` or
  `bits64`.
- `md5`: MD5 in OCaml, without the stdlib `Digest` module and channels.

## Tests

```sh
make test
```

## Develop

```sh
make init    # local opam switch with OCaml 5.4.0, opam and npm dependencies
make build   # dune build @all
make test    # dune build @runtest, needs node
make size    # prints the size table, needs esbuild
```

Set `ESBUILD` to use an `esbuild` binary that is not in `node_modules`.

## License

MIT. See [LICENSE.md](LICENSE.md).
