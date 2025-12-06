#load "str.cma";;

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

let solution1 lines =
  let delimiter = Str.regexp " +" in
  let lines = lines |> Seq.map (fun line -> Str.split delimiter line |> Array.of_list)
                    |> Array.of_seq in
  let height = Array.length lines in
  let width = Array.length lines.(0) in
  let operators = lines.(height - 1) in
  let accumulators = Array.init width (fun column -> Int64.of_string lines.(0).(column)) in
  let matrix = Array.init_matrix (height - 2) width (fun row column -> Int64.of_string lines.(row + 1).(column)) in
  for row = 0 to height - 3 do
    for column = 0 to width - 1 do
      if operators.(column) = "+"
      then accumulators.(column) <- Int64.add accumulators.(column) matrix.(row).(column)
      else if operators.(column) = "*" 
      then accumulators.(column) <- Int64.mul accumulators.(column) matrix.(row).(column)
      else failwith "unknown operator"
    done;
  done;

  Array.fold_left Int64.add 0L accumulators

let () =
  "prod.txt"
  |> lines_of_file
  |> solution1
  |> Printf.printf "%Ld\n"