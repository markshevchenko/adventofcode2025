let lines_of_file filename =
  let ic = In_channel.open_text filename in
  let rec next_line () =
    try
      Some (input_line ic, ())
    with End_of_file ->
      close_in ic;
      None
  in
  Seq.unfold next_line ()

let parse lines =
  let lines = List.of_seq lines in
  let empty_string_index = lines |> List.find_index ((=) "") |> Option.get in
  let intervals = lines |> List.take empty_string_index |> List.map (fun s -> Scanf.sscanf s "%Ld-%Ld" (fun a b -> (a, b))) in
  let ids = lines |> List.drop (empty_string_index + 1) |> List.map (fun s-> Scanf.sscanf s "%Ld" Fun.id) in
  (intervals, ids)

let strategy1 (intervals, ids) =
  ids
  |> List.filter (fun id -> List.exists (fun (lo, hi) -> id >= lo && id <= hi) intervals)
  |> List.length
  |> Int64.of_int

let folder (count, (lo0, hi0)) (lo1, hi1) =
  if hi0 >= lo1
  then (count, (lo0, hi1))
  else (Int64.add count (Int64.add (Int64.sub hi0 lo0) 1L), (lo1, hi1))

let strategy2 (intervals, _) =
  let sorted_intervals = List.sort (fun (a, _) (b, _) -> Int64.compare a b) intervals in
  let first = List.hd sorted_intervals in
  let (count, (lo, hi)) = List.fold_left folder (0L, first) (List.tl sorted_intervals) in
  Int64.add count (Int64.add (Int64.sub hi lo) 1L)

let () =
  "prod.txt"
  |> lines_of_file
  |> parse
  |> strategy2
  |> Printf.printf "%Ld\n"
