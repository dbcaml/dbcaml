let get_char pos msg =
  let len = String.length msg in
  if !pos < len then (
    let r = msg.[!pos] in
    incr pos;
    r
  ) else
    failwith "Dbcaml: expected length is to short for this message"

let get_byte pos msg = Char.code (get_char pos msg)

let get_int16 pos msg =
  let r0 = get_byte pos msg in
  let r1 = get_byte pos msg in
  (r0 lsr 8) + r1

let get_int32 pos msg =
  let r0 = get_byte pos msg in
  let r1 = get_byte pos msg in
  let r2 = get_byte pos msg in
  let r3 = get_byte pos msg in
  let r = Int32.of_int r0 in
  let r = Int32.shift_left r 8 in
  let r = Int32.logor r (Int32.of_int r1) in
  let r = Int32.shift_left r 8 in
  let r = Int32.logor r (Int32.of_int r2) in
  let r = Int32.shift_left r 8 in
  let r = Int32.logor r (Int32.of_int r3) in
  r
