(** A small replacement for a part of [Stdlib.Printf].

    [Stdlib.Printf] reads a typed format string at run time with
    [CamlinternalFormat]. On Melange that module is about 29 KB of minified
    JavaScript. This module does not use format strings. It gives:

    - functions that each do one conversion ([int], [hex], [float_g],
      [char_literal]), for use with [^];
    - typed format combinators ([lit], [s], [d], [conv], [^^]) with
      [sprintf] and [ksprintf], for code that wants the shape of
      [Printf.sprintf].

    The same code runs on native and on Melange, except [float_g]. The
    native implementation ([melange-tiny.printf.native], the default) calls
    the [caml_format_float] primitive, as [Stdlib.Printf] does. The Melange
    implementation ([melange-tiny.printf.js]) uses the JavaScript
    [toExponential] and [toPrecision] methods, so the bundle does not get
    [caml_format.js]. A [melange.emit] stanza must list
    [melange-tiny.printf.js] in its [libraries].

    Supported subset, with the [Stdlib.Printf] equivalent:
    - [%s]: [s], or the string itself.
    - [%d]: [d], or [int].
    - [%x], [%X], [%0Nx], [%0NX]: [hex].
    - [%.Ng], [%.*g]: [float_g].
    - [%C]: [char_literal].
    - [%%] and other literal text: [lit], or a string literal.
    - [%a] and [%t]: [conv] with your own printer.

    Not supported: flags other than zero padding ([-], [+], space, [#]),
    widths on [%s] and [%d], [%i], [%u], [%o], [%f], [%e], [%F], [%h],
    [%S], [%B], [%c], [%l], [%n], [%L], [%!], [%,], format string
    concatenation of [Stdlib] formats, and all output channels
    ([printf], [eprintf], [fprintf], [bprintf], [kprintf], [ifprintf]). *)

(** {1 Conversions} *)

val int : int -> string
(** [int n] is [Printf.sprintf "%d" n]. *)

val hex : ?upper:bool -> ?width:int -> int -> string
(** [hex ?upper ?width n] is [Printf.sprintf "%0*x" width n], or
    [Printf.sprintf "%0*X" width n] when [upper] is [true]. [upper] is [false]
    and [width] is [0] by default. A negative [n] is shown as an unsigned
    number of the platform word size: 63 bits on native, 32 bits on Melange,
    as [Printf] does. *)

val float_g : int -> float -> string
(** [float_g p f] is [Printf.sprintf "%.*g" p f]: [f] with [p] significant
    digits, trailing zeros removed, in exponent form when the exponent is
    below [-4] or at or above [p]. [nan], [infinity] and [neg_infinity] give
    ["nan"], ["inf"] and ["-inf"]. On native the output is the same as
    [Printf]. On Melange it is the same as the Melange [Printf] for [p <= 21].
    For [p > 21], a number at or above [1e21] is in decimal form, as on
    native; the Melange [Printf] keeps it in exponent form. The two backends
    differ from each other as [Printf] does: on Melange, [-0.] gives ["0"]
    (native: ["-0"]), and a decimal tie with many digits rounds away from
    zero (native: to even). On Melange, a [p] above [100] is used as [100].
    @raise Invalid_argument if [p] is negative. [Printf] does not check. *)

val char_literal : char -> string
(** [char_literal c] is [Printf.sprintf "%C" c]: [c] as an OCaml character
    literal, with quotes and escapes. *)

(** {1 Format combinators}

    A format is a value of type [('a, 'r) t]. ['a] is the type of the
    function that [sprintf] returns, and ['r] is its final result. For
    example, [Printf.sprintf "%s at %d" msg i] becomes
    [sprintf (s ^^ lit " at " ^^ d) msg i].

    Differences from [Stdlib.Printf]:
    - A format is built from functions, not written as a string literal.
    - A format that is the result of [^^] and is bound with [let] at top
      level has a weak type variable for ['r]: it is used with one result
      type only. A [Stdlib] format literal is polymorphic. *)

type ('a, 'r) t
(** A format that takes the arguments in ['a] and gives ['r]. *)

val lit : string -> ('r, 'r) t
(** [lit text] adds [text] and takes no argument. *)

val s : (string -> 'r, 'r) t
(** [s] takes a string and adds it, as [%s] does. *)

val d : (int -> 'r, 'r) t
(** [d] takes an int and adds it in decimal, as [%d] does. *)

val conv : ('x -> string) -> ('x -> 'r, 'r) t
(** [conv f] takes an ['x] and adds [f x]. For example
    [conv (hex ~upper:true ~width:2)] is [%02X], and [conv (float_g 15)] is
    [%.15g]. *)

val ( ^^ ) : ('a, 'b) t -> ('b, 'r) t -> ('a, 'r) t
(** [f ^^ g] takes the arguments of [f], then the arguments of [g]. *)

val sprintf : ('a, string) t -> 'a
(** [sprintf fmt] takes the arguments of [fmt] and returns the text. *)

val ksprintf : (string -> 'r) -> ('a, 'r) t -> 'a
(** [ksprintf k fmt] takes the arguments of [fmt] and returns [k] applied to
    the text. *)
