let rec read_lines f =
  try
    let line = input_line f in
    line :: read_lines f
  with End_of_file -> []

let soc c s =
  String.split_on_char c s |> Array.of_list

let string_to_tab s =
  let t = Array.make 14 0 in
  for i = 0 to 4 do
    match s.[i] with
    | 'B' -> t.(9) <- t.(9) + 1
    | '0' -> t.(10) <- t.(10) + 1
    | 'C' -> t.(11) <- t.(11) + 1
    | 'D' -> t.(12) <- t.(12) + 1
    | 'X' -> t.(13) <- t.(13) + 1
    | _ -> t.((int_of_char s.[i]) - 49) <- t.((int_of_char s.[i]) - 49) + 1
  done;
  t

let convert_int_l i =
  match i with
  | 9 -> "B"
  | 10 -> "0"
  | 11 -> "C"
  | 12 -> "D"
  | 13 -> "X"
  | 0 -> "1"
  | 1 -> "2"
  | 2 -> "3"
  | 3 -> "4"
  | 4 -> "5"
  | 5 -> "6"
  | 6 -> "7"
  | 7 -> "8"
  | 8 -> "9"

let trans s =
  let t = string_to_tab s in
  let maxv = ref min_int in
  let max_ind = ref 0 in
  for i = 13 downto 0 do
    if t.(i) > !maxv && i <> 10 then (maxv := t.(i); max_ind := i)
  done;
  t.(!max_ind) <- t.(!max_ind) + t.(10);
  t.(10) <- 0;
  let ns = ref "" in
  for i = 0 to 4 do
    if s.[i] = '0' then ns := !ns ^ (convert_int_l !max_ind)
    else ns := !ns ^ (String.make 1 s.[i])
  done;
  print_string !ns; print_char '\n'; !ns

let replace s =
    let ns = ref "" in
    for j = 0 to 4 do
      match s.[j] with
      | 'A' -> ns := !ns ^ "X"
      | 'K' -> ns := !ns ^ "D"
      | 'Q' -> ns := !ns ^ "C"
      | 'J' -> ns := !ns ^ "0"
      | 'T' -> ns := !ns ^ "B"
      | _ -> ns := !ns ^ (String.make 1 s.[j])
    done;
    !ns 

let split_hand_bid file =
  file |> Array.map (soc ' ') |> Array.map (fun e -> (e.(0), int_of_string e.(1))) |> Array.map (fun (a, b) -> (replace a, b))

let type_main lmb =
  let l = ref [] in
  let main = lmb |> fst in
  for i = 0 to  (main |> String.length) - 1 do
    if not (!l |> List.mem main.[i]) then l := (main.[i])::(!l)
  done;
  !l |> List.rev

let is_brelan s =
  let m1 = trans s in
  print_char '\n';
  let tbl = Hashtbl.create 42 in
  for i = 0 to 4 do
    if Hashtbl.mem tbl m1.[i] then Hashtbl.replace tbl m1.[i] ((Hashtbl.find tbl m1.[i]) + 1)
    else Hashtbl.add tbl m1.[i] 1
  done;
  let maxhtbl = ref min_int in
  for i = 0 to 4 do
    let e = Hashtbl.find tbl m1.[i] in
    if e > !maxhtbl then maxhtbl := e
  done;
  if !maxhtbl = 3 then true else false

let is_carre s =
  let m1 = trans s in
  print_char '\n';
  let tbl = Hashtbl.create 42 in
  for i = 0 to 4 do
    if Hashtbl.mem tbl m1.[i] then Hashtbl.replace tbl m1.[i] ((Hashtbl.find tbl m1.[i]) + 1)
    else Hashtbl.add tbl m1.[i] 1
  done;
  let maxhtbl = ref min_int in
  for i = 0 to 4 do
    let e = Hashtbl.find tbl m1.[i] in
    if e > !maxhtbl then maxhtbl := e
  done;
  if !maxhtbl = 4 || !maxhtbl = 5 then true else false

let cmp lmb1 lmb2 =
  let n1 = ref (lmb1 |> type_main |> List.length) in
  let n2 = ref (lmb2 |> type_main |> List.length) in
  if (lmb1 |> type_main <> ['0']) && (lmb1 |> type_main |> List.mem '0') then (decr n1);
  if (lmb2 |> type_main <> ['0']) && (lmb2 |> type_main |> List.mem '0') then (decr n2);
  if !n1 < !n2 then 1
  else if !n1 > !n2 then -1
  else (
    if is_carre (lmb1 |> fst) && not (is_carre (lmb2 |> fst)) then 1
    else if is_carre (lmb2 |> fst) && not (is_carre (lmb1 |> fst)) then -1
    else if is_brelan (lmb1 |> fst) && not (is_brelan (lmb2 |> fst)) then 1
    else if is_brelan (lmb2 |> fst) && not (is_brelan (lmb1 |> fst)) then -1
    else (
    let i = ref 0 in
    while (lmb1 |> fst).[!i] = (lmb2 |> fst).[!i] do
      incr i
    done;
    if (lmb1 |> fst).[!i] < (lmb2 |> fst).[!i] then -1 else 1
    ))

let res file =
  let c = ref 0 in
  let l = List.sort cmp (file |> Array.to_list) |> Array.of_list in
  for i = Array.length file - 1 downto 0 do
    c := !c + (l.(i) |> snd)*(i + 1)
  done;
  !c

    

let main =
  let file = read_lines (open_in "day7.txt") |> Array.of_list in
  res ((split_hand_bid file) |> Array.map (fun (a, b) -> (replace a, b)))
  (*let l = List.sort cmp ((split_hand_bid file) |> Array.map (fun (a, b) -> (replace a, b)) |> Array.to_list) |> List.rev |> Array.of_list in*)
