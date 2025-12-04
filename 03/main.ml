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

let extract_first s =
    let rec find_max i max_digit max_i =
        if i = String.length s - 1 then (max_digit, max_i)
        else if max_digit < s.[i] then find_max (i + 1) s.[i] i
        else find_max (i + 1) max_digit max_i in
    
    find_max 1 s.[0] 0

let extract_second s first_index =
    let rec find_max i max_digit max_i =
        if i = String.length s then max_digit
        else if max_digit < s.[i] then find_max (i + 1) s.[i] i
        else find_max (i + 1) max_digit max_i in

    find_max (first_index + 2) s.[first_index + 1] (first_index + 1)

let max_joltage1 s =
    let d1, i1 = extract_first s in
    let d2 = extract_second s i1 in
    10 * (Char.code d1 - Char.code '0') + (Char.code d2 - Char.code '0')

let extract_next s start last =
    let rec find_max i max_digit max_i =
        if i = String.length s - (last - 1) then (max_digit, max_i)
        else if max_digit < s.[i] then find_max (i + 1) s.[i] i
        else find_max (i + 1) max_digit max_i in

    find_max (start + 1) s.[start] start

let folder s (sum, current_index) last_index =
    let max_digit, max_index = extract_next s current_index last_index in
    (10 * sum + Char.code max_digit - Char.code '0', max_index + 1)

let max_joltage2 s =
    Seq.init 12 (fun i -> 12 - i)
    |> Seq.fold_left (folder s) (0, 0)
    |> fst

let () =
  "prod.txt"
  |> lines_of_file
  |> Seq.map max_joltage2
  |> Seq.fold_left (+) 0
  |> Printf.printf "\n%d\n"