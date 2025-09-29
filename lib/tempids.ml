let tempidcounter = ref 0

let make_temp_id () =
  let n = !tempidcounter in
  tempidcounter := n + 1;
  "tmp-id" ^ string_of_int n
