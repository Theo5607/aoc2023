let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []

let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l

let is_digit c =
  let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|] in
  Array.mem c chiffres

let is_symbol c =
  let symb = [|'+';'*';'$';'&';'#';'/';'@';'=';'%';'-'|] in
  Array.mem c symb

let add_tuple x y =
  let a, b = x in
  let a', b' = y in
  (a + a', b + b')
  

let res tab = 
  let inds = ref [] in
  let dir = [|(1, 1); (0, 1); (1, 0); (-1, 1); (1, -1); (-1, 0); (0, -1);(-1, -1)|] in
  Array.iteri (fun i e -> String.iteri (fun j e' -> if e' = '*' then inds := (i, j)::(!inds)) e) tab;
  let c = ref 0 in
  List.iter (fun e ->
    let ind_ban = ref [] in
    let compt = ref 0 in
    let prod = ref 1 in
    for i = 0 to 7 do
      if not (List.mem (add_tuple e dir.(i)) !ind_ban) then (
      let x, y = add_tuple e dir.(i) in
      try
      if is_digit tab.(x).[y] then (
        let nb = ref "" in
        let k = ref y in
        while is_digit tab.(x).[!k] do
          nb := !nb ^ (String.make 1 tab.(x).[!k]);
          ind_ban := (x, !k)::(!ind_ban);
          incr k
        done;
        k := y - 1;
        while is_digit tab.(x).[!k] do
          nb := (String.make 1 tab.(x).[!k]) ^ !nb;
          ind_ban := (x, !k)::(!ind_ban);
          decr k
        done;
        incr compt;
        prod := !prod*(int_of_string !nb);
        if !compt = 2 then c := !c + !prod;)
      with Invalid_argument _ -> ())
    done;) !inds;
  !c

let main = 
  let fichier = read_lines (open_in "day3.txt") in
  let tab' =  Array.of_list fichier in
  let tab = Array.map (fun e -> "." ^ e ^ ".") tab' in
  print_string "resultat";
  res tab
