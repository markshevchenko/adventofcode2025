let read_lines_from_file (filename : string) : string list =
  In_channel.with_open_text filename In_channel.input_lines

let parse s =
  Scanf.sscanf s "%c%d" (fun c i -> (c, i))

let execute_command dial (c, i) =
  match c with
  | 'L' -> dial - i
  | 'R' -> dial + i
  | _' -> failwith "Unknown command"

let ediv a b =
  if b = 0 then failwith "Division by zero"
  else
    let q = a / b in
    let r = a mod b in
    if (r < 0 && b > 0) || (r > 0 && b < 0) then q - 1 else q

let erem a b =
  if b = 0 then failwith "Division by zero"
  else
    let r = a mod b in
    if (r < 0 && b > 0) || (r > 0 && b < 0) then r + abs b else r

let fix_dial counter dial =
  let new_dial = erem dial 100 in
  let diff = ediv dial 100 in
  let counter_offset = if diff < 0 then -diff else diff in
  Printf.printf "dial %d, new_dial %d, diff %d, counter %d, counter_offset %d\n" dial new_dial diff counter counter_offset;
  new_dial, counter + counter_offset

let rec execute_commands commands dial counter =
  match commands with
  | [] -> counter
  | c::cs ->
    let new_dial, new_counter = execute_command dial c |> fix_dial counter in
    execute_commands cs new_dial new_counter

let () =
  let commands = "input.txt" |> read_lines_from_file |> List.map parse in
  let result = execute_commands commands 50 0 in

  Printf.printf "%d\n" result
