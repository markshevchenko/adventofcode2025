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

let exp10 n = Seq.repeat 10L |> Seq.take n |> Seq.fold_left Int64.mul 1L

let split number size =
  let base = exp10 size in
  let step i = if i = 0L then None else Some (Int64.rem i base, Int64.div i base) in
  Seq.unfold step number

let digits_count n = n |> Int64.to_float |> Stdlib.log10 |> Float.to_int |> (+) 1

let are_same (s : 'a Seq.t) : bool =
  match s () with
  | Seq.Nil -> true
  | Seq.Cons (head, tail) -> Seq.for_all (Int64.equal head) tail

let range start finish =
  Seq.ints start |> Seq.take_while ((>=) finish)

let check n size =
  let parts = split n size in
  let result = (are_same parts && Seq.length parts > 1) in
  if result then Printf.printf "%Ld:" n; Seq.iter (Printf.printf " %Ld") parts; Printf.printf " - %B\n" result;
  result

let is_invalid2 n =
  range 1 (digits_count n)
  |> Seq.map (check n)
  |> Seq.fold_left (||) false

let range64 start finish =
  Seq.unfold (fun i -> if i <= finish then Some (i, Int64.add i 1L) else None) start

let () =
  "test.txt"
  |> read_first_line
  |> String.split_on_char ','
  |> List.map parse
  |> List.concat_map (fun (a, b) -> range64 a b |> Seq.filter is_invalid2 |> List.of_seq)
  |> List.fold_left Int64.add Int64.zero
  |> Printf.printf "%Ld\n"
