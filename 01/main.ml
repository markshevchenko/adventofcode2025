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

type command =
  | Left of int
  | Right of int

let to_str = function
  | Left ticks -> Printf.sprintf "L %d" ticks
  | Right ticks -> Printf.sprintf "R %d" ticks

let make_command c i =
  match c with 
  | 'L' -> Left i
  | 'R' -> Right i
  | _ -> failwith "unknown command"

let parse s = Scanf.sscanf s "%c%d" make_command

let strategy1 (old_dial, old_counter) command =
  let new_dial =
    match command with
    | Left ticks ->
      let new_dial = old_dial - ticks mod 100 in
      if new_dial < 0 then new_dial + 100 else new_dial
    | Right ticks ->
      (old_dial + ticks) mod 100
    in
    let new_counter = old_counter + if new_dial = 0 then 1 else 0 in
    (new_dial, new_counter)

let strategy2 (old_dial, old_counter) command =
  let (new_dial, round_counter, pass_counter) =
    match command with
    | Left ticks ->
      let round_counter = ticks / 100 in
      let new_dial = old_dial - ticks mod 100 in
      if new_dial < 0
      then (new_dial + 100, round_counter, if old_dial > 0 then 1 else 0)
      else (new_dial, round_counter, 0)
    | Right ticks ->
      let round_counter = ticks / 100 in
      let new_dial = old_dial + ticks mod 100 in
      if new_dial >= 100
      then (new_dial - 100, round_counter, if new_dial > 100 then 1 else 0)
      else (new_dial, round_counter, 0)
    in
    let dial_counter = if new_dial = 0 then 1 else 0 in
    let new_counter = old_counter + round_counter + pass_counter + dial_counter in
    (*Printf.printf "command: %s, dial: (old%d new%d), counter: (old%d round%d pass%d dial%d new%d)\n" (to_str command) old_dial new_dial old_counter round_counter pass_counter dial_counter new_counter;*)
    (new_dial, new_counter)

let () =
  "prod.txt"
  |> lines_of_file
  |> Seq.map parse
  |> Seq.fold_left strategy2 (50, 0)
  |> snd
  |> Printf.printf "%d\n"
