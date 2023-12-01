let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []

let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l

let res tab = 
  let c = ref 0 in
  let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|] in
  for i = 0 to Array.length tab - 1 do
    let c1 = ref ' ' in
    let c2 = ref ' ' in
    for j = 0 to String.length tab.(i) - 1 do
      for k = 0 to Array.length chiffres - 1 do
        if tab.(i).[j] = chiffres.(k) && !c1 = ' ' then c1 := chiffres.(k)
      done;
    done;
    for j = String.length tab.(i) - 1 downto 0 do
      for k = 0 to Array.length chiffres - 1 do
        if tab.(i).[j] = chiffres.(k) && !c2 = ' ' then c2 := chiffres.(k)
      done;
    done;
      c := !c + (int_of_char !c1 - 48)*10 + (int_of_char !c2 - 48)
  done;
  !c

let res2_v2 tab =
  let c = ref 0 in
  let chiffres = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|] in
  let t = [|"zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"|] in
  for i = 0 to Array.length tab - 1 do
    let min1 = ref 0 in
    let max1 = ref 0 in
    let flag_deb = ref true in
    let flag_fin = ref true in
    for j = 0 to String.length tab.(i) - 1 do
      if Array.mem tab.(i).[j] chiffres then begin min1 := int_of_char tab.(i).[j] - 48; flag_deb := false end
      else begin Array.iteri (fun k e -> if String.starts_with e (String.sub tab.(i) j (String.length tab.(i) - j)) then min1 := k; flag_deb := false) t end
    done;
    for j = String.length tab.(i) - 1 downto 0 do
      if Array.mem tab.(i).[j] chiffres then begin max1 := int_of_char tab.(i).[j] - 48; flag_fin := false end
      else begin Array.iteri (fun k e -> if String.ends_with e (String.sub tab.(i) 0 (j + 1)) then max1 := k; flag_fin := false) t end
    done;
    print_int !max1;
    print_int !min1;
    print_char '\n';
    c := !c + 10*(!max1) + !min1
  done;
  !c

let main = 
  let fichier = read_lines (open_in "day1.txt") in
  let tab =  Array.of_list fichier in
  print_string "le résultat est : ";
  print_int (res2_v2 tab);
