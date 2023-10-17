let ( >>! ) x f = Option.iter f x

(** TODO: doc here *)
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

(** For each character in the alphabet, [bad_char.\[char\]] is
    the rightmost position of [char] in [needle].
    If the character isn't present, we return [-1]. *)
let bad_char_table needle =
  let n = String.length needle in
  let table = Array.make 256 (-1) in
  for i = 0 to n - 2 do
    let c = Char.code needle.[i] in
    table.(c) <- i
  done;
  Format.printf "Bad char table for %s:@." needle;
  Array.iter
    (function -1 -> Format.printf "_ " | i -> Format.printf "%d " i)
    table;
  table

(** Is [needle.\[i..\]] a prefix of needle? *)
let is_prefix needle i =
  if i = 0 then true
  else
    let n = String.length needle in
    let rec loop k =
      if k + i >= n then true
      else if needle.[k] = needle.[i + k] then loop (succ k)
      else false
    in
    loop 0

(** What is the length of the largest substring of [needle] ending at position
   [i], that is also a suffix of [needle]? *)
let suffix_length needle i =
  let n = String.length needle in
  assert (i >= 0 && i < n);
  if i = n - 1 then n
  else if needle.[i] <> needle.[n - 1] then 0
  else
    let rec loop j =
      if i - j < 0 then i + 1
      else if needle.[i - j] = needle.[n - 1 - j] then loop (succ j)
      else j
    in
    loop 1

(** TODO: doc here *)
let good_suffix_table needle =
  let n = String.length needle in
  let table = Array.make n n in
  let last_prefix = ref (n - 1) in
  (* First loop, assumes result of second loop doesn't get erased *)
  for i = n - 1 downto 0 do
    if is_prefix needle (i + 1) then last_prefix := i + 1;
    table.(i) <- !last_prefix + n - 1 - i
  done;
  (* Second loop: TODO explain *)
  for i = 0 to n - 2 do
    let suf_len = suffix_length needle i in
    let table_i = n - 1 - suf_len in
    assert (suf_len > i || needle.[i - suf_len] <> needle.[table_i]);
    table.(table_i) <- n - 1 - i + suf_len
  done;
  table
