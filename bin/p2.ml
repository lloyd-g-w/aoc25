open Scanf

type range = { l : int; r : int }

let rec read_lines fd arr =
  try
    let line = input_line fd in
    sscanf line "%d-%d," (fun l r -> Dynarray.add_last arr { l; r });
    read_lines fd arr
  with _ -> close_in_noerr fd

let rec main ?(curr_line = 0) ?(res = 0) lines =
  if curr_line = Dynarray.length lines then Printf.printf "%d\n" res else ()

let () =
  let fd = open_in "in/2" in
  let lines = Dynarray.create () in
  read_lines fd lines;
  main lines
