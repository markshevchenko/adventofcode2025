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


let can_access matrix (row, column) =
  let paper i j =
    let has_piper = i >= 0 && i < Array.length matrix
                 && j >= 0 && j < Array.length matrix.(i)
                 && matrix.(i).(j) = '@' in
    if has_piper then 1 else 0 in
  let pipers = paper (row - 1) (column - 1)
             + paper (row - 1) column
             + paper (row - 1) (column + 1)
             + paper row (column - 1)
             + paper row (column + 1)
             + paper (row + 1) (column - 1)
             + paper (row + 1) column
             + paper (row + 1) (column + 1) in
  matrix.(row).(column) = '@' && pipers < 4

let strategy1 matrix =
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in
  Seq.product (Seq.init height Fun.id) (Seq.init width Fun.id)
  |> Seq.filter (can_access matrix)
  |> Seq.length

let strategy2 matrix =
  let height = Array.length matrix in
  let width = Array.length matrix.(0) in
  let indices = Seq.product (Seq.init height Fun.id) (Seq.init width Fun.id) |> List.of_seq in
  let rec step sum =
    let accessed = List.filter (can_access matrix) indices in
    let count = List.length accessed in
    List.iter (fun (i, j) -> matrix.(i).(j) <- '.') accessed;
    if count = 0
    then sum
    else step (sum + count) in
  
    step 0

let () =
  let matrix =
    "prod.txt"
    |> lines_of_file
    |> Seq.map (fun s -> Array.init (String.length s) (String.get s))
    |> Array.of_seq in
  strategy2 matrix |> Printf.printf "%d\n"
