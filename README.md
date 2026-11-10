# melange-tiny

A prototype. It answers one question: how small can `Printf`, `Hashtbl`,
`Random` and `Digest` be in a [Melange](https://melange.re) bundle, with the
signature and the behaviour of the stdlib modules?

It is an experiment, not a library to depend on. It is not on opam, it has no
releases, and it can change or stop at any time. The useful parts are the
numbers, the tests that compare each module with the stdlib, and the notes on
where the bytes go. They show what to change in the Melange stdlib.

## Result

On Melange, one call to a stdlib module can pull in a large part of the
runtime. For example, `Printf.sprintf` brings `CamlinternalFormat`, and
`Hashtbl` brings `Domain`, `Random` and then `Printf`.

This table shows the size of a small program that uses each module. The stdlib
program and the tiny program have the same source, except the module path.
Each program is bundled with `esbuild --bundle --minify`, then compressed with
gzip level 9. Sizes are in bytes.

| Module | stdlib raw | stdlib gzip | tiny raw | tiny gzip |
| --- | ---: | ---: | ---: | ---: |
| `Printf` | 48535 | 11652 | 15400 | 4838 |
| `Hashtbl` | 64542 | 16970 | 8591 | 2961 |
| `Random` | 56462 | 14756 | 4568 | 2340 |
| `Digest` | 54625 | 13932 | 3650 | 1858 |

Measured with Melange 7.0.1-54 and OCaml 5.4.0. The programs are in `size/`.
Run `make size` to measure again.

## How each one is small

- `Printf`: reads the format value that the compiler makes from the format
  string, without `CamlinternalFormat`.
- `Hashtbl`: the stdlib algorithm without `Domain` and `Random`. With
  `~random:true` or after `randomize`, the seed comes from `Math.random`.
- `Random`: the generator of the Melange stdlib with its own small MD5 for the
  seed. The 64-bit runtime is in the bundle only when you call `int64` or
  `bits64`.
- `Digest`: MD5 in OCaml, without the stdlib `Digest` module and channels.

## Same behaviour

The experiment has a value only if the small modules are true replacements.

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

## Run it

```sh
make init    # local opam switch with OCaml 5.4.0, opam and npm dependencies
make build   # dune build @all
make test    # dune build @runtest, needs node
make size    # prints the size table, needs esbuild
```

Set `ESBUILD` to use an `esbuild` binary that is not in `node_modules`.

## Layout

Each module is a folder and a dune virtual library:

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

`printf/`, `random/` and `md5/` have the same shape. `size/` has the programs
of the size table. `example/` has a library that uses all four modules, a
native test and a Melange program. The test checks that both print the same
text.

## Try it in a project

To measure the effect on your own bundle, pin the repository:

```sh
opam pin add melange-tiny https://github.com/davesnx/melange-tiny.git
```

| Library | Module | Replaces |
| --- | --- | --- |
| `melange-tiny.printf` | `Tiny_printf.Printf` | `Stdlib.Printf` |
| `melange-tiny.hashtbl` | `Tiny_hashtbl.Hashtbl` | `Stdlib.Hashtbl` |
| `melange-tiny.random` | `Tiny_random.Random` | `Stdlib.Random` |
| `melange-tiny.md5` | `Tiny_md5.Digest` | `Stdlib.Digest` |

Each module has the name of the stdlib module that it replaces. Write
`open Tiny_printf` and `Printf` is the small one.

```ocaml
open Tiny_printf

let message msg i = Printf.sprintf "%s at %d" msg i
```

Each library has one implementation for native and one for Melange. The native
one is the default. The Melange one has the same name with `.js` at the end.
List the library in your `library` or `executable` stanza, and add the `.js`
library in the `melange.emit` stanza:

```dune
(library
 (name app)
 (modes melange)
 (libraries melange-tiny.printf melange-tiny.hashtbl))

(melange.emit
 (target output)
 (libraries app melange-tiny.printf.js melange-tiny.hashtbl.js))
```

If you forget the `.js` library, dune stops with an error that says the
native library is not compatible with Melange.

## License

MIT. See [LICENSE.md](LICENSE.md).
