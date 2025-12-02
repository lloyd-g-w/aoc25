type line = { left : bool; count : int64 }

let counts = Array.make 100 0

let rec read_lines fd arr =
  try
    let line = input_line fd in
    let parsed =
      {
        left = line.[0] = 'L';
        count = Int64.of_string (String.sub line 1 (String.length line - 1));
      }
    in
    Dynarray.add_last arr parsed;
    read_lines fd arr
  with _ -> close_in_noerr fd

let calc_left acc count =
  let v = Int64.rem (Int64.sub acc count) 100L in
  if v < 0L then Int64.add v 100L else v

let calc_right acc count = Int64.rem (Int64.add acc count) 100L

let calc_left2 acc count =
  let vraw = Int64.sub acc count in
  let times =
    if vraw > 0L then 0
    else
      let wraps = Int64.to_int (Int64.div (Int64.neg vraw) 100L) in
      if acc = 0L then wraps else wraps + 1
  in
  let v = Int64.rem vraw 100L in
  counts.(0) <- counts.(0) + times;
  if v < 0L then Int64.add v 100L else v

let calc_right2 acc count =
  let vraw = Int64.add acc count in
  let times = Int64.to_int (Int64.div vraw 100L) in
  counts.(0) <- counts.(0) + times;
  Int64.rem vraw 100L

let rec main lines curr (acc : int64) l r =
  if curr = Dynarray.length lines then Printf.printf "%d\n" counts.(0)
  else
    let line = Dynarray.get lines curr in
    let new_acc = if line.left then l acc line.count else r acc line.count in
    let idx = Int64.to_int new_acc in
    if idx != 0 || l != calc_left2 then counts.(idx) <- counts.(idx) + 1;
    main lines (curr + 1) new_acc l r

let () =
  let fd = open_in "in/1" in
  let lines = Dynarray.create () in
  read_lines fd lines;
  main lines 0 50L calc_left calc_right;
  Array.fill counts 0 (Array.length counts) 0;
  main lines 0 50L calc_left2 calc_right2
