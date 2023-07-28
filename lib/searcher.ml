module type S = sig
  val search : pat:string -> string -> int option
  val name : string
end

module KMP : S = struct
  let name = "Knuth-Morris-Pratt algorithm"

  let preprocess pat =
    let p = String.length pat in
    let lpp = Array.make p 0 in
    let rec loop len i =
      if i < p then
        if pat.[len] = pat.[i] then (
          lpp.(i) <- len + 1;
          loop (len + 1) (i + 1))
        else if len = 0 then (
          lpp.(i) <- 0;
          loop len (i + 1))
        else loop lpp.(len - 1) i
    in
    loop 0 1;
    lpp

  let search ~pat str =
    let p = String.length pat in
    let s = String.length str in
    let lpp = preprocess pat in
    let rec loop x y =
      if s - x < p - y then None
      else if y = p then Some (x - y)
      else if pat.[y] = str.[x] then loop (succ x) (succ y)
      else if y = 0 then loop (succ x) y
      else loop x lpp.(y - 1)
    in
    loop 0 0
end

module Opti : S = struct
  let name = "Optimized naive algorithm"

  let search ~pat str =
    let p = String.length pat in
    let s = String.length str in
    let rec loop x =
      let rec inner y =
        if y = p then true
        else if pat.[y] = str.[x + y] then inner (succ y)
        else false
      in
      if p + x > s then None else if inner 0 then Some x else loop (succ x)
    in
    loop 0
end

module Naive : S = struct
  let name = "Naive algorithm"

  let search ~pat str =
    let p = String.length pat in
    let s = String.length str in
    let rec loop x =
      let rec inner y =
        if y = p then true
        else if x + y < s && pat.[y] = str.[x + y] then inner (succ y)
        else false
      in
      if x > s then None else if inner 0 then Some x else loop (succ x)
    in
    loop 0
end
