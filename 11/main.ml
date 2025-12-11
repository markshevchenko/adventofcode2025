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

let parse_line line =
  let head = String.sub line 0 3 in
  let targets = String.sub line 5 (String.length line - 5) |> String.split_on_char ' ' in
  (head, targets)

let solution1 lines =
  let devices = Hashtbl.create 1000 in
  Seq.iter (fun (head, targets) -> Hashtbl.add devices head targets) lines;
  let rec count_paths device =
    if device = "out" then
      1
    else
      (Hashtbl.find devices device)
      |> List.map count_paths
      |> List.fold_left (+) 0 in
  count_paths "you"

let solution2 lines =
  let devices = Hashtbl.create 1000 in
  let counted = Hashtbl.create 1000 in
  Seq.iter (fun (head, targets) -> Hashtbl.add devices head targets) lines;
  let check_device device flags =
    if device = "dac" then
      flags lor 1
    else if device = "fft" then
      flags lor 2
    else flags in
  let rec count_paths device flags =
    if Hashtbl.mem counted (device, flags) then
      Hashtbl.find counted (device, flags)
    else if device = "out" then
      let result = if flags = 3 then 1 else 0 in
      Hashtbl.add counted (device, flags) result;
      result
    else
      let result =
        (Hashtbl.find devices device)
        |> List.map (fun device -> count_paths device (check_device device flags))
        |> List.fold_left (+) 0 in
      Hashtbl.add counted (device, flags) result;
      result
    in
  count_paths "svr" 0

let () =
  "prod.txt"
  |> lines_of_file
  |> Seq.map parse_line
  |> solution2
  |> Printf.printf "%d\n"
