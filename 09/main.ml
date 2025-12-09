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

let pairwise l1 l2 =
  List.concat (List.map (fun e1 ->
    List.map (fun e2 -> (e1, e2)) l2
  ) l1)

let square ((x1, y1), (x2, y2)) =
  let width = if x1 < x2 then x2 - x1 + 1 else x1 - x2 + 1 in
  let height = if y1 < y2 then y2 - y1 + 1 else y1 - y2 + 1 in
  width * height

let list_max = function
  | [] -> failwith "empty list"
  | x :: xs -> List.fold_left Int.max x xs

let solution1 tiles =
  pairwise tiles tiles
  |> List.map square
  |> list_max

let () =
  "prod.txt"
  |> lines_of_file
  |> Seq.map (fun line -> Scanf.sscanf line "%d,%d" (fun x y -> (x, y)))
  |> List.of_seq
  |> solution1
  |> Printf.printf "%d\n"
