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

let solution2 lines =
  let matrix =
    lines
    |> Seq.map (fun s -> Array.init (String.length s) (String.get s))
    |> Array.of_seq in
  let rec collect_number column row accumulator =
    if row = Array.length matrix - 1
    then accumulator
    else
      let c = matrix.(row).(column) in
      if c < '0' || c > '9'
      then collect_number column (row + 1) accumulator
      else
        let digit = Char.code c - Char.code '0'in
        let next_accumulator = Int64.add (Int64.mul accumulator 10L) (Int64.of_int digit) in
        collect_number column (row + 1) next_accumulator
    in
  let operators = matrix.(Array.length matrix - 1) in
  let rec get_final_sum column final_sum sources =
    if column < 0
    then final_sum
    else
      let operator = operators.(column) in
      let number = collect_number column 0 0L in
      if operator = ' '
      then get_final_sum (column - 1) final_sum (number::sources)
      else if operator = '+'
      then
        let sum = List.fold_left Int64.add 0L (number::sources) in
        get_final_sum (column - 2) (Int64.add final_sum sum) []
      else if operator = '*'
      then
        let prod = List.fold_left Int64.mul 1L (number::sources) in
        get_final_sum (column - 2) (Int64.add final_sum prod) []
      else failwith "invalid operator"
    in
  get_final_sum (Array.length matrix.(0) - 1) 0L []

let () =
  "prod.txt"
  |> lines_of_file
  |> solution2
  |> Printf.printf "%Ld\n"