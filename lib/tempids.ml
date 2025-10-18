let tempidcounter = ref 0
let counter = ref 0

let make_temp_id () =
  let n = !tempidcounter in
  tempidcounter := n + 1;
  "tmp-id" ^ string_of_int n

let make_label prefix =
  let n = !counter in
  counter := n + 1;
  prefix ^ "." ^ Int.to_string n

let make_named_temporary = make_label
