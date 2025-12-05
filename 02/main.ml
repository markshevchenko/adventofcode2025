let read_first_line filename =
  let ic = open_in filename in
  let line = input_line ic in
  close_in ic;
  line

let parse s = Scanf.sscanf s "%Ld-%Ld" (fun a b -> (a, b))

let is_invalid1 n =
  let check divisor = (Int64.div n divisor = Int64.rem n divisor) in
  if n < 0L then false
  else if n < 100L then check 10L
  else if n < 1000L then false
  else if n < 10000L then check 100L
  else if n < 100000L then false
  else if n < 1000000L then check 1000L
  else if n < 10000000L then false
  else if n < 100000000L then check 10000L
  else if n < 1000000000L then false
  else if n < 10000000000L then check 100000L
  else failwith "too big number"

let unfolder divisor i =
  if i = 0L then None
  else Some (Int64.rem i divisor, Int64.div i divisor)


let is_invalid2 n =
  let check divisor =
    let parts = Seq.unfold (unfolder divisor) n |> List.of_seq in
    List.length parts >= 2 && List.for_all ((=) (List.hd parts)) (List.tl parts) in
  if n < 10L then false
  else if n < 100L then check 10L
  else if n < 1000L then check 10L
  else if n < 10000L then check 10L || check 100L
  else if n < 100000L then check 10L
  else if n < 1000000L then check 10L || check 100L || check 1000L
  else if n < 10000000L then check 10L
  else if n < 100000000L then check 10L || check 100L || check 10000L
  else if n < 1000000000L then check 10L || check 1000L
  else if n < 10000000000L then check 10L || check 100L || check 100000L
  else failwith "too big number"

let range64 start finish =
  Seq.unfold (fun i -> if i <= finish then Some (i, Int64.add i 1L) else None) start

let () =
  "prod.txt"
  |> read_first_line
  |> String.split_on_char ','
  |> List.map parse
  |> List.concat_map (fun (a, b) -> range64 a b |> Seq.filter is_invalid2 |> List.of_seq)
  |> List.fold_left Int64.add Int64.zero
  |> Printf.printf "%Ld\n"
