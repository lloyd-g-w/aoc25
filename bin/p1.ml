type line = { left : bool; count : int64 }

let counts = Array.make 99 0

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
  let mod_count = Int64.rem count 99L in
  if Int64.sub acc mod_count < 0L then Int64.add (Int64.sub acc mod_count) 99L
  else Int64.sub acc mod_count

let calc_right acc count =
  let mod_count = Int64.rem count 99L in
  if Int64.add acc mod_count > 99L then Int64.sub (Int64.add acc mod_count) 99L
  else Int64.add acc mod_count

let rec main lines curr (acc : int64) =
  if curr = Dynarray.length lines - 1 then print_string (Int64.to_string acc)
  else
    let line = Dynarray.get lines curr in
    let new_acc =
      if line.left then calc_left acc line.count else calc_right acc line.count
    in
    let idx = (Int64.to_int new_acc mod 99) - 1 in
    print_int idx;
    counts.(idx) <- counts.(idx) + 1;
    main lines (curr + 1) new_acc

let () =
  let fd = open_in "in/1" in
  let lines = Dynarray.create () in
  read_lines fd lines;
  main lines 0 50L
