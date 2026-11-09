external random : unit -> float = "random" [@@mel.scope "Math"]

let seed () = int_of_float (random () *. 4294967296.)
