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

(** TODO: doc here *)
let bad_char_table needle =
  let n = String.length needle in
  let table = Array.make 256 None in
  for i = 0 to n - 2 do
    let c = Char.code needle.[i] in
    table.(c) <- Some i
  done;
  table

(** Is [needle.\[i..\]] a prefix of needle? *)
let is_prefix needle i =
  let open String in
  let prefix = sub needle i (length needle - i) in
  starts_with ~prefix needle

(** If there is a substring [needle.\[x..i\]] that is also a suffix of [needle],
    what is the maximum length [i-x]?
    TODO: reword this *)
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

(** TODO: doc here *)
let good_suffix_table needle =
  let n = String.length needle in
  let table = Array.make n n in
  let last_prefix = ref None in
  (* First loop, assumes result of second loop doesn't get erased *)
  for i = n - 1 downto 0 do
    !last_prefix >>! Array.set table i;
    if is_prefix needle i then last_prefix := Some i
  done;
  (* Second loop: TODO explain *)
  for i = 0 to n - 2 do
    suffix_length needle i >>! fun suf_len ->
    let table_i = n - 1 - suf_len in
    table.(table_i) <- n - i - 1
  done;
  table
