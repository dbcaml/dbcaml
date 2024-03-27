(* Function to encode an int32 to a 4-byte string *)
let int32 i =
  let b = Bytes.create 4 in
  Bytes.set b 0 (char_of_int (Int32.to_int (Int32.shift_right i 24) land 0xFF));
  Bytes.set b 1 (char_of_int (Int32.to_int (Int32.shift_right i 16) land 0xFF));
  Bytes.set b 2 (char_of_int (Int32.to_int (Int32.shift_right i 8) land 0xFF));
  Bytes.set b 3 (char_of_int (Int32.to_int i land 0xFF));
  b

let string s =
  let length = String.length s in
  let b = Bytes.create length in
  Bytes.blit_string s 0 b 0 length;
  b

let bool b =
  Bytes.make
    1
    (if b then
       '\001'
     else
       '\000')

let float f =
  let b = Bytes.create 8 in
  let i = Int64.bits_of_float f in
  for j = 0 to 7 do
    Bytes.set
      b
      j
      (char_of_int (Int64.to_int (Int64.shift_right i (8 * (7 - j))) land 0xFF))
  done;
  b

let int i =
  let b = Bytes.create 4 in
  Bytes.set b 0 (char_of_int ((i lsr 24) land 0xFF));
  Bytes.set b 1 (char_of_int ((i lsr 16) land 0xFF));
  Bytes.set b 2 (char_of_int ((i lsr 8) land 0xFF));
  Bytes.set b 3 (char_of_int (i land 0xFF));
  b

let int64 i =
  let b = Bytes.create 8 in
  Bytes.set b 0 (char_of_int (Int64.to_int (Int64.shift_right i 56) land 0xFF));
  Bytes.set b 1 (char_of_int (Int64.to_int (Int64.shift_right i 48) land 0xFF));
  Bytes.set b 2 (char_of_int (Int64.to_int (Int64.shift_right i 40) land 0xFF));
  Bytes.set b 3 (char_of_int (Int64.to_int (Int64.shift_right i 32) land 0xFF));
  Bytes.set b 4 (char_of_int (Int64.to_int (Int64.shift_right i 24) land 0xFF));
  Bytes.set b 5 (char_of_int (Int64.to_int (Int64.shift_right i 16) land 0xFF));
  Bytes.set b 6 (char_of_int (Int64.to_int (Int64.shift_right i 8) land 0xFF));
  Bytes.set b 7 (char_of_int (Int64.to_int i land 0xFF));
  b
