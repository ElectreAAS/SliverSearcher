module PP = Preprocess

module type S = sig
  val search : needle:string -> haystack:string -> int option
  val name : string
end

module KMP : S = struct
  let name = "Knuth-Morris-Pratt"

  let search ~needle ~haystack =
    let n = String.length needle in
    let h = String.length haystack in
    let partial_match = PP.partial_match_table needle in
    let rec loop x y =
      if h - x < n - y then None
      else if y = n then Some (x - y)
      else if needle.[y] = haystack.[x] then loop (succ x) (succ y)
      else if y = 0 then loop (succ x) y
      else loop x partial_match.(y - 1)
    in
    loop 0 0
end

module Opti : S = struct
  let name = "Optimized naive"

  let search ~needle ~haystack =
    let n = String.length needle in
    let h = String.length haystack in
    let rec loop x =
      let rec inner y =
        if y = n then true
        else if needle.[y] = haystack.[x + y] then inner (succ y)
        else false
      in
      if n + x > h then None else if inner 0 then Some x else loop (succ x)
    in
    loop 0
end

module Naive : S = struct
  let name = "Naive"

  let search ~needle ~haystack =
    let n = String.length needle in
    let h = String.length haystack in
    let rec loop x =
      let rec inner y =
        if y = n then true
        else if x + y < h && needle.[y] = haystack.[x + y] then inner (succ y)
        else false
      in
      if x + n > h then None else if inner 0 then Some x else loop (succ x)
    in
    loop 0
end

module BMH : S = struct
  let name = "Boyer-Moore-Horspool"

  let search ~needle ~haystack =
    let n = String.length needle in
    let h = String.length haystack in
    let bad_char = PP.bad_char_table needle in
    let rec loop x y =
      if y < 0 then Some x
      else if x + y >= h then None
      else
        let char = haystack.[x + y] in
        if needle.[y] = char then loop x (pred y)
        else
          let shift =
            match bad_char.(Char.code char) with
            | None -> y + 1
            | Some pos when pos < y -> y - pos
            | Some _too_big_to_be_useful -> 1
          in
          loop (x + shift) (pred n)
    in
    loop 0 (pred n)
end

module BM : S = struct
  let name = "Boyer-Moore"

  let search ~needle ~haystack =
    let n = String.length needle in
    let h = String.length haystack in
    let bc_table = PP.bad_char_table needle in
    let gs_table = PP.good_suffix_table needle in
    let rec loop x y =
      if y < 0 then Some x
      else if x + y >= h then None
      else
        let char = haystack.[x + y] in
        if needle.[y] = char then loop x (pred y)
        else
          let shift =
            match bc_table.(Char.code char) with
            | Some pos when pos < y -> min (y - pos) gs_table.(y)
            | _ -> gs_table.(y)
          in
          (* TODO: if we shift with an offset provided by good_suffix,
             we can also learn that characters ????????????????
             have already been matched with the current alignment, and don't need to be checked again...
          *)
          loop (x + shift) (pred n)
    in
    loop 0 (pred n)
end
