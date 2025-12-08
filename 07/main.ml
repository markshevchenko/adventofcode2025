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

let solution1 field =
  let rec process row column split_count =
    if row = Array.length field then
      split_count
    else if column = Array.length field.(row) then
      process (row + 1) 0 split_count
    else if field.(row - 1).(column) = '|' then
      if field.(row).(column) = '^' then begin
        field.(row).(column - 1) <- '|';
        field.(row).(column + 1) <- '|';
        process row (column + 2) (split_count + 1)
      end else begin
        field.(row).(column) <- '|'; process row (column + 1) split_count
      end
    else process row (column + 1) split_count
  in
  let start = Array.find_index ((=) 'S') field.(0) |> Option.get in
  field.(1).(start) <- '|';
  process 2 0 0

let solution2 field =
  let splits = Hashtbl.create 1000 in
  let rec process row column =
    if Hashtbl.mem splits (row, column) then
      Hashtbl.find splits (row, column)
    else
      if row = Array.length field then
        0
      else if field.(row).(column) = '^' then
        let left_count = process (row + 2) (column - 1) in
        let right_count = process (row + 2) (column + 1) in
        let current_split = left_count + right_count + 1 in
        Hashtbl.add splits (row, column) current_split;
        current_split
      else
        let current_split = process (row + 2) column in
        Hashtbl.add splits (row, column) current_split;
        current_split

  in
  let start = Array.find_index ((=) 'S') field.(0) |> Option.get in
  1 + process 2 start


let () =
  "prod.txt"
  |> lines_of_file
  |> Seq.map (fun line -> Array.init (String.length line) (String.get line))
  |> Array.of_seq 
  |> solution2
  |> Printf.printf "%d"
