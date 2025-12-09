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

let rec build_pairs element list accumulator =
  match list with
  | [] -> accumulator
  | x::xs -> build_pairs element xs ((element, x)::accumulator)

let rec diagonal_pairwise list accumulator =
  match list with
  | [] -> failwith "empty list"
  | [_] -> failwith "too short list"
  | x1::[x2] -> (x1, x2)::accumulator
  | x::xs -> diagonal_pairwise xs (build_pairs x xs accumulator)

let pairwise list =
  diagonal_pairwise list []

let compare ((x11, y11, z11), (x12, y12, z12)) ((x21, y21, z21), (x22, y22, z22)) =
  let d1 = (x12 - x11) * (x12 - x11) + (y12 - y11) * (y12 - y11) + (z12 - z11) * (z12 - z11) in
  let d2 = (x22 - x21) * (x22 - x21) + (y22 - y21) * (y22 - y21) + (z22 - z21) * (z22 - z21) in
  d1 - d2

let group_by_key f lst =
  List.fold_left (fun acc x ->
    let key = f x in
    match List.assoc_opt key acc with
    | Some vals -> (key, x :: vals) :: List.remove_assoc key acc
    | None -> (key, [x]) :: acc
  ) [] lst

let solution1 max_connections points =
  let pairs =
    pairwise points
    |> List.filter (fun ((x1, y1, z1), (x2, y2, z2)) -> x1 != x2 || y1 != y2 || z1 != z2)
    |> List.sort compare in
  let cirtuits = Hashtbl.create 1000 in
  let result () =
    let l1 = cirtuits
    |> Hashtbl.to_seq_values
    |> List.of_seq in
    List.iter (Printf.printf " %d") l1;
    Printf.printf "\n";
    let l2 = group_by_key (Fun.id) l1
    |> List.map (fun (_, list) -> List.length list) in
    List.iter (Printf.printf " %d") l2;
    Printf.printf "\n";
    let l3 = List.sort_uniq (fun a b -> b - a) l2 in
    List.iter (Printf.printf " %d") l3;
    Printf.printf "\n";
    l3 |> List.take 3 |> List.fold_left ( * ) 1
  in
  let set_connections p1 p2 current_connection =
    match (Hashtbl.find_opt cirtuits p1, Hashtbl.find_opt cirtuits p2) with
    | (None, None) ->
        Printf.printf "both: %d\n" current_connection;
        Hashtbl.add cirtuits p1 current_connection;
        Hashtbl.add cirtuits p2 current_connection
    | (Some connection, None) ->
        Printf.printf "right: %d\n" connection;
        Hashtbl.add cirtuits p2 connection
    | (None, Some connection) ->
        Printf.printf "left: %d\n" connection;
        Hashtbl.add cirtuits p1 connection
    | (Some connection1, Some connection2) ->
        Printf.printf "none: %d, %d, %d\n" connection1 connection2 current_connection;
        ()
  in
  let rec process current_connection = function
    | [] -> result ()
    | ((x1, y1, z1), (x2, y2, z2))::ps ->
      Printf.printf "(%d, %d, %d)-(%d, %d, %d)\n" x1 y1 z1 x2 y2 z2;
      if current_connection > max_connections + 1 then
        result ()
      else begin
        set_connections (x1, y1, z1) (x2, y2, z2) current_connection;
        process (current_connection + 1) ps
      end
  in
  process 1 pairs

let () =
  "test.txt"
  |> lines_of_file
  |> Seq.map (fun line -> Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x, y, z)))
  |> List.of_seq
  |> solution1 10
  |> Printf.printf "%d\n"