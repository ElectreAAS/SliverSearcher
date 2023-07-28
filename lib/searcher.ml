module type S = sig
  val search : pat:string -> string -> int option
  val name : string
end

let pp_partial_match pat =
  let p = String.length pat in
  let table = Array.make p 0 in
  let rec loop len i =
    if i < p then
      if pat.[i] = pat.[len] then (
        table.(i) <- len + 1;
        loop (len + 1) (i + 1))
      else if len = 0 then (
        table.(i) <- 0;
        loop len (i + 1))
      else loop table.(len - 1) i
  in
  loop 0 1;
  table

let pp_bad_char pat =
  let bad_char = Array.make 256 None in
  String.iteri (fun i c -> bad_char.(Char.code c) <- Some i) pat;
  bad_char

module KMP : S = struct
  let name = "Knuth-Morris-Pratt algorithm"

  let search ~pat text =
    let p = String.length pat in
    let s = String.length text in
    let partial_match = pp_partial_match pat in
    let rec loop x y =
      if s - x < p - y then None
      else if y = p then Some (x - y)
      else if pat.[y] = text.[x] then loop (succ x) (succ y)
      else if y = 0 then loop (succ x) y
      else loop x partial_match.(y - 1)
    in
    loop 0 0
end

module Opti : S = struct
  let name = "Optimized naive algorithm"

  let search ~pat text =
    let p = String.length pat in
    let s = String.length text in
    let rec loop x =
      let rec inner y =
        if y = p then true
        else if pat.[y] = text.[x + y] then inner (succ y)
        else false
      in
      if p + x > s then None else if inner 0 then Some x else loop (succ x)
    in
    loop 0
end

module Naive : S = struct
  let name = "Naive algorithm"

  let search ~pat text =
    let p = String.length pat in
    let s = String.length text in
    let rec loop x =
      let rec inner y =
        if y = p then true
        else if x + y < s && pat.[y] = text.[x + y] then inner (succ y)
        else false
      in
      if x >= s then None else if inner 0 then Some x else loop (succ x)
    in
    loop 0
end

module BMH : S = struct
  let name = "Boyer-Moore-Horspool algorithm"

  let search ~pat text =
    let p = String.length pat in
    let s = String.length text in
    let bad_char = pp_bad_char pat in
    let rec loop x y =
      if y < 0 then Some x
      else if x + y >= s then None
      else
        let char = text.[x + y] in
        if pat.[y] = char then loop x (pred y)
        else
          match bad_char.(Char.code char) with
          | None -> loop (x + y + 1) (pred p)
          | Some last_pos -> loop (x + y - last_pos) (pred p)
    in
    loop 0 (pred p)
end
