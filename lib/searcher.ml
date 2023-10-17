module PP = Preprocess

module type S = sig
  val search : needle:string -> haystack:string -> int option
  val search_count : needle:string -> haystack:string -> int
  val name : string
  val index : int
end

module Naive : S = struct
  let name = "Naive"

  let search ?(count = false) ~needle ~haystack () =
    let n = String.length needle in
    let h = String.length haystack in
    let counter = ref 0 in
    let rec loop x =
      let rec inner y =
        if y = n then true
        else if x + y < h then (
          incr counter;
          if needle.[y] = haystack.[x + y] then inner (succ y) else false)
        else false
      in
      if x + n > h then None else if inner 0 then Some x else loop (succ x)
    in
    let res = loop 0 in
    if count then Some !counter else res

  let search_count ~needle ~haystack =
    Option.get @@ search ~count:true ~needle ~haystack ()

  let search = search ()
  let index = 0
end

module Opti : S = struct
  let name = "Optimized"

  let search ?(count = false) ~needle ~haystack () =
    let n = String.length needle in
    let h = String.length haystack in
    let counter = ref 0 in
    let rec loop x =
      let rec inner y =
        if y = n then true
        else (
          incr counter;
          if needle.[y] = haystack.[x + y] then inner (succ y) else false)
      in
      if n + x > h then None else if inner 0 then Some x else loop (succ x)
    in
    let res = loop 0 in
    if count then Some !counter else res

  let search_count ~needle ~haystack = Option.get @@ search ~needle ~haystack ()
  let search = search ()
  let index = 1
end

module KMP : S = struct
  let name = "Knuth-Morris-Pratt"

  let search ?(count = false) ~needle ~haystack () =
    let n = String.length needle in
    let h = String.length haystack in
    let counter = ref 0 in
    let partial_match = PP.partial_match_table needle in
    let rec loop x y =
      if h - x < n - y then None
      else if y = n then Some (x - y)
      else (
        incr counter;
        if needle.[y] = haystack.[x] then loop (succ x) (succ y)
        else if y = 0 then loop (succ x) y
        else loop x partial_match.(y - 1))
    in
    let res = loop 0 0 in
    if count then Some !counter else res

  let search_count ~needle ~haystack =
    Option.get @@ search ~count:true ~needle ~haystack ()

  let search = search ()
  let index = 2
end

module BMH : S = struct
  let name = "Boyer-Moore-Horspool"

  let search ?(count = false) ~needle ~haystack () =
    let n = String.length needle in
    let h = String.length haystack in
    let bad_char = PP.bad_char_table needle in
    let counter = ref 0 in
    let rec loop x y =
      if y < 0 then Some x
      else if x + y >= h then None
      else
        let char = haystack.[x + y] in
        incr counter;
        if needle.[y] = char then loop x (pred y)
        else
          let shift = max 1 (y - bad_char.(Char.code char)) in
          loop (x + shift) (pred n)
    in
    let res = loop 0 (pred n) in
    if count then Some !counter else res

  let search_count ~needle ~haystack =
    Option.get @@ search ~count:true ~needle ~haystack ()

  let search = search ()
  let index = 3
end

module BM : S = struct
  let name = "Boyer-Moore"

  let search ?(count = false) ~needle ~haystack () =
    let n = String.length needle in
    let h = String.length haystack in
    let bc_table = PP.bad_char_table needle in
    let gs_table = PP.good_suffix_table needle in
    let counter = ref 0 in
    let rec loop h_index n_index =
      if n_index < 0 then Some (h_index + 1)
      else if h_index >= h then None
      else
        let char = haystack.[h_index] in
        incr counter;
        if needle.[n_index] = char then loop (pred h_index) (pred n_index)
        else
          let pos = bc_table.(Char.code char) in
          let shift = max (max 1 (n_index - pos)) gs_table.(n_index) in
          (* TODO: if we shift with an offset provided by good_suffix,
             we can also learn that characters ????????????????
             have already been matched with the current alignment, and don't need to be checked again...
          *)
          loop (h_index + shift) (pred n)
    in
    let res = loop (pred n) (pred n) in
    if count then Some !counter else res

  let search_count ~needle ~haystack =
    Option.get @@ search ~count:true ~needle ~haystack ()

  let search = search ()
  let index = 4
end
