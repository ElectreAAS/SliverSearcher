module type S = sig
  val search : needle:string -> haystack:string -> int option
  val name : string
end

let partial_match_table needle =
  let n = String.length needle in
  let table = Array.make n 0 in
  let rec loop len i =
    if i < n then
      if needle.[i] = needle.[len] then (
        table.(i) <- len + 1;
        loop (len + 1) (i + 1))
      else if len = 0 then (
        table.(i) <- 0;
        loop len (i + 1))
      else loop table.(len - 1) i
  in
  loop 0 1;
  table

let bad_char_table needle =
  let n = String.length needle in
  let bad_char = Array.make 256 None in
  for i = 0 to n - 2 do
    let c = Char.code needle.[i] in
    bad_char.(c) <- Some i
  done;
  bad_char

(** Is [needle.\[i..\]] a prefix of needle? *)
let is_prefix needle i =
  let open String in
  let prefix = sub needle i (length needle - i) in
  starts_with ~prefix needle

(** If there is a substring [needle.\[x..i\]] that is also a suffix of [needle],
    what is the maximum length [i-x]? *)
let suffix_length needle i =
  let n = String.length needle in
  assert (i >= 0 && i < n);
  if i = n - 1 then Some n
  else if needle.[i] <> needle.[n - 1] then None
  else
    let rec loop j =
      if i - j < 0 then i + 1
      else if needle.[i - j] = needle.[n - 1 - j] then loop (succ j)
      else j
    in
    Some (loop 1)

let good_suffix_table needle =
  let n = String.length needle in
  let table = Array.make n n in
  let last_prefix = ref None in
  (* First loop, assumes result of second loop doesn't get erased *)
  for i = n - 1 downto 0 do
    Option.iter (fun pref -> table.(i) <- pref) !last_prefix;
    if is_prefix needle i then last_prefix := Some i
  done;
  (* Second loop: TODO explain *)
  for i = 0 to n - 2 do
    match suffix_length needle i with
    | None -> ()
    | Some suf_len ->
        let table_i = n - 1 - suf_len in
        table.(table_i) <- n - i - 1
  done;
  table

module KMP : S = struct
  let name = "Knuth-Morris-Pratt"

  let search ~needle ~haystack =
    let n = String.length needle in
    let h = String.length haystack in
    let partial_match = partial_match_table needle in
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
    let bad_char = bad_char_table needle in
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
    let bc_table = bad_char_table needle in
    let gs_table = good_suffix_table needle in
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
