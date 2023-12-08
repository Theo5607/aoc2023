let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let hash_instr file =
  let h = Hashtbl.create 42 in
  for i = 2 to Array.length file - 1 do
    Hashtbl.add h (String.sub file.(i) 0 3 |> String.trim) (String.sub file.(i) 7 3 |> String.trim, String.sub file.(i) 12 3 |> String.trim)
  done;
  h

let res file s =
  let h = hash_instr file in
  let cur = ref s in
  let k = ref 0 in
  let c = ref 0 in
  while not (String.contains !cur 'Z') do
    if !k = String.length file.(0) then (k := 0; print_char '\n');
    cur := if file.(0).[!k] = 'L' then (Hashtbl.find h !cur |> fst)
    else (Hashtbl.find h !cur |> snd);
    incr c;
    incr k;
  done;
  !c

let rec pgcd n m =
  if n > m then pgcd m n
  else if n = 0 then m
       else let p = m / n
            and r = m mod n in
            pgcd r n;;

let res2 file =
  let l = ref [] in
  for i = 2 to Array.length file - 1 do
    if file.(i).[2] = 'A' then l := (res file (String.sub file.(i) 0 3))::(!l)
  done;
  List.fold_left (fun acc e -> (acc*e)/(pgcd acc e)) 1 !l

    

let main =
  let file = read_lines (open_in "day8.txt") |> Array.of_list in
  res2 file 
