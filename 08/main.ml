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

let square_distance (x1, y1, z1) (x2, y2, z2) =
  let square x = x * x in
  square (x2 - x1) + square (y2 - y1) + square (z2 - z1)

let distance_compare (p1, p2) (p3, p4) =
  Int.compare (square_distance p1 p2) (square_distance p3 p4)

let make_pairs points =
  let pairs_length = Array.length points * (Array.length points - 1) / 2 in
  let pairs = Array.make pairs_length ((0,0,0), (0,0,0)) in
  let index = ref 0 in
  for i = 0 to Array.length points - 2 do
    for j = i + 1 to Array.length points - 1 do
      pairs.(!index) <- (points.(i), points.(j));
      index := !index + 1;
    done
  done;
  Array.sort distance_compare pairs;
  pairs

let solution1 connections points =
  let pairs = make_pairs points in
  let circuits = Hashtbl.create connections in
  let cycles = Hashtbl.create connections in
  for i = 0 to connections - 1 do
    let point1 = fst pairs.(i) in
    let point2 = snd pairs.(i) in
    match (Hashtbl.find_opt circuits point1), (Hashtbl.find_opt circuits point2) with
    | None, None ->
      Hashtbl.add circuits point1 i;
      Hashtbl.add circuits point2 i;
      Hashtbl.add cycles i (point1::[point2]);
    | None, Some connection ->
      Hashtbl.add circuits point1 connection;
      Hashtbl.replace cycles connection (point1::Hashtbl.find cycles connection);
    | Some connection, None  ->
      Hashtbl.add circuits point2 connection;
      Hashtbl.replace cycles connection (point2::Hashtbl.find cycles connection);
    | Some connection1, Some connection2 ->
      if connection1 != connection2 then begin
        let points1 = Hashtbl.find cycles connection1 in
        let points2 = Hashtbl.find cycles connection2 in
        List.iter (fun point -> Hashtbl.replace circuits point connection1) points2;
        Hashtbl.replace cycles connection1 (points1 @ points2);
        Hashtbl.remove cycles connection2;
      end
  done;

  cycles
  |> Hashtbl.to_seq_values
  |> Seq.map (fun list -> List.length list)
  |> List.of_seq
  |> List.sort_uniq (fun a b -> -Int.compare a b)
  |> List.take 3
  |> List.fold_left ( * ) 1

let x_of (x, _, _) = x
let print_points (x1, y1, z1) (x2, y2, z2) =
  Printf.printf "(%d, %d, %d) - (%d, %d, %d)\n" x1 y1 z1 x2 y2 z2

let solution2 points =
  let point_set =
    points
    |> Array.to_seq
    |> Seq.map (fun point -> (point, ()))
    |> Hashtbl.of_seq in
  let pairs = make_pairs points in
  let circuits = Hashtbl.create 1000 in
  let cycles = Hashtbl.create 1000 in
  let i = ref 0 in
  while Hashtbl.length point_set > 0 do
    let point1 = fst pairs.(!i) in
    let point2 = snd pairs.(!i) in
    match (Hashtbl.find_opt circuits point1), (Hashtbl.find_opt circuits point2) with
    | None, None ->
      Hashtbl.add circuits point1 !i;
      Hashtbl.add circuits point2 !i;
      Hashtbl.add cycles !i (point1::[point2]);
      Hashtbl.remove point_set point1;
      Hashtbl.remove point_set point2;
    | None, Some connection ->
      Hashtbl.add circuits point1 connection;
      Hashtbl.replace cycles connection (point1::Hashtbl.find cycles connection);
      Hashtbl.remove point_set point1;
    | Some connection, None  ->
      Hashtbl.add circuits point2 connection;
      Hashtbl.replace cycles connection (point2::Hashtbl.find cycles connection);
      Hashtbl.remove point_set point2;
    | Some connection1, Some connection2 ->
      if connection1 != connection2 then begin
        let points1 = Hashtbl.find cycles connection1 in
        let points2 = Hashtbl.find cycles connection2 in
        List.iter (fun point -> Hashtbl.replace circuits point connection1) points2;
        Hashtbl.replace cycles connection1 (points1 @ points2);
        Hashtbl.remove cycles connection2;
      end;
    i := !i + 1;
  done;

  x_of (fst pairs.(!i)) * x_of (snd pairs.(!i))


let () =
  "prod.txt"
  |> lines_of_file
  |> Seq.map (fun line -> Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x, y, z)))
  |> Array.of_seq
  |> solution2
  |> Printf.printf "%d\n"
