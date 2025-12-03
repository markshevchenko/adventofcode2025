let read_first_line filename =
  let ic = open_in filename in
  let line = input_line ic in
  close_in ic;
  line

let parse s = Scanf.sscanf s "%Ld-%Ld" (fun a b -> (a, b))

let is_invalid1 n =
  if n < 0L then false
  else if n < 100L then (Int64.div n 10L = Int64.rem n 10L)
  else if n < 1000L then false
  else if n < 10000L then (Int64.div n 100L = Int64.rem n 100L)
  else if n < 100000L then false
  else if n < 1000000L then (Int64.div n 1000L = Int64.rem n 1000L)
  else if n < 10000000L then false
  else if n < 100000000L then (Int64.div n 10000L = Int64.rem n 10000L)
  else if n < 1000000000L then false
  else if n < 10000000000L then (Int64.div n 100000L = Int64.rem n 100000L)
  else if n < 100000000000L then false
  else if n < 1000000000000L then (Int64.div n 1000000L = Int64.rem n 1000000L)
  else if n < 10000000000000L then false
  else if n < 100000000000000L then (Int64.div n 10000000L = Int64.rem n 10000000L)
  else if n < 1000000000000000L then false
  else if n < 10000000000000000L then (Int64.div n 100000000L = Int64.rem n 100000000L)
  else if n < 100000000000000000L then false
  else if n < 1000000000000000000L then (Int64.div n 1000000000L = Int64.rem n 1000000000L)
  else false

let range start finish =
  Seq.unfold (fun i -> if i <= finish then Some (i, Int64.add i 1L) else None) start

let () =
  "prod.txt"
  |> read_first_line
  |> String.split_on_char ','
  |> List.map parse
  |> List.concat_map (fun (a, b) -> range a b |> Seq.filter is_invalid1 |> List.of_seq)
  |> List.fold_left Int64.add Int64.zero
  |> Printf.printf "%Ld\n"
