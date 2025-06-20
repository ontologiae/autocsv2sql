open Batteries

module L = BatList;;


type typPsql = Int | Numeric | Text;;


let typPsqlToString e =
       match e with
        | Int -> "Int"
        | Numeric -> "Numeric"
        | Text  -> "Text";;         

(* val get_separator : string -> char = <fun>*) 
let get_separator line =
  let separators = [',';';';'\t'] in
  let counts = L.map (fun sep -> BatString.split_on_char sep line |> L.length) separators in
  let max_count = L.fold_left max 0 counts in
  let index = L.findi (fun _ -> fun valu -> valu = max_count) counts |> fst in
  L.nth separators index;;



let remove_quotes str =
  let len = BatString.length str in
  let res = if len >= 2 && (str.[0] = '"' || str.[0] = '\'') && (str.[len-1] = '"' || str.[len-1] = '\'') then
    BatString.sub str 1 (len - 2) |> BatString.replace_chars ( function '\'' -> "''" 
                                                                | c     -> BatString.of_char c)
  else
          str |> BatString.replace_chars ( function '\'' -> "''" 
                                                                | c     -> BatString.of_char c)
  in (*Printf.eprintf "removequote : %s\n%!" res;*) res
;; 



(*val get_field_names : string -> char -> string list = <fun>*)
let get_field_names line separator =
        BatString.split_on_char separator line |> L.map remove_quotes;;



(*val detect_field_type : string -> typPsql = <fun>*)
let detect_field_type value =
  let is_int str =
    try ignore (int_of_string str); true with _ -> false
  in
  if is_int value then Int
  else if try ignore (float_of_string value); true with _ -> false then Numeric
  else Text;;



(*val analyze_data_lines : string -> typPsql list = <fun>*)

(*La fonction analyze_data_lines est algotrithmiquement totalement fausse.
Tu vas la réécrire en suivant ces instructions.
En paramètre le separateur et ic de type BatInnerIO.input
Tu créé d'abord un BatArray typArray de taille égale au nombre de colone.
Tu parcours ensuite la ligne n°2, qui est la première ligne de données, la 1ière étant les entêtes de colonnes
Tu utilises la fonction detect_field_type pour typer chacune des colonnes en renseignant le BatArray typArray
Tu parcours ensuite chaque ligne en analysant chaque colonne.
Sur chaque colonne, tu utilises detect_field_type. On procède ensuite à la mutation des types comme suit :
Si detect_field_type de la ième colonne est Text alors que la ième case de typArray est Int ou Numeric, alors la case deviens Text
Si detect_field_type de la ième colonne est Numeric alors que la ième case de typArray est Int , alors la case deviens Numeric
Si detect_field_type de la ième colonne est Text alors que la ième case de typArray est Int ou Numeric alors la case deviens Text
Tu rajouteras un affichage du pourcentage de progression par rapport au nombre de lignes totales*)




let replace_non_alnum_with_underscore str =
  let is_valid_char c = BatChar.is_letter c || BatChar.is_digit c in
  Filename.basename str |> BatString.map (fun c -> if is_valid_char c then c else '_');;


let analyze_data_lines separator ic =
  let header = input_line ic in
  let field_names = BatString.split_on_char separator header in
  let num_columns = L.length field_names in
  let typArray = BatArray.make num_columns Int in
  let count = ref 0 in
  let notFinished = ref true in
  while (!notFinished) do
    try
      let line = input_line ic  in
      let values = BatString.split_on_char separator line in
      BatArray.iteri (fun i col_type ->
        if i < L.length values then (
          let value = L.nth values i |> remove_quotes in
          let new_type = detect_field_type value in
          match col_type with
           | Text -> ()
          | Int -> typArray.(i) <- new_type 
          | Numeric -> typArray.(i) <- (match new_type with
                         | Int | Numeric -> Numeric
                         | Text -> Text)
        )
      ) typArray;
      count := !count + 1;
    with End_of_file -> notFinished := false; close_in ic;
  done;
  typArray,!count;;

(*val generate_create_table_statement :
  string -> string list -> typPsql list -> unit = <fun>*)

let generate_create_table_statement table_name field_names field_types =
  let zip = try L.combine field_names field_types with e -> failwith "Seems that the columns definition is not consistent with datas" in
  let field_declarations = L.map (fun (name, ftype) ->
          let nameOk = name |> remove_quotes in
    match ftype with
    | Numeric -> "\"" ^ nameOk ^ "\" NUMERIC"
    | Int ->  "\"" ^ nameOk ^ "\" INT"
    | Text ->  "\"" ^ nameOk ^ "\" TEXT"
  ) zip in
  Printf.printf "CREATE TABLE %s (%s);\n" table_name (BatString.join ", " field_declarations);;





(*val generate_insert_statements :
  string -> string -> string list -> typPsql list -> char -> unit = <fun>*)
let generate_insert_statements file_name table_name field_names field_types separator nbligne =
  let ic = open_in file_name in
  let _ = input_line ic in (*first line columns*)
  let cpt = ref 0 in
  let count = ref 0 in
  let values_str = ref "" in
  let  read_lines () =
    try
      while true do
        let line = input_line ic  in
        cpt := !cpt + 1;
        count := !count + 1;
        let columns = BatString.split_on_char separator line in
        let insert_values = L.map2 (fun column_ ftype ->
                let column = column_ |> remove_quotes in
                match ftype with
                        | Numeric -> column
                        | Int -> column
                        | Text -> "'" ^ column ^ "'"
        ) columns field_types in
        values_str := BatString.concat ", " insert_values; 
        match !cpt with
        | 1 -> Printf.printf "\nINSERT INTO %s (%s) \nVALUES (%s),\n%!" table_name ("\"" ^ (BatString.concat "\", \"" field_names)^"\"") !values_str
        | 999 -> cpt := 0; Printf.printf "(%s);\n%!"   !values_str
        | _ when  !count >= (nbligne+1)  -> Printf.printf "(%s);\n%!"  !values_str
        | _   -> Printf.printf "(%s),\n%!"  !values_str 
      done
    with 
    | End_of_file -> close_in ic
    | _ -> Printf.eprintf "Erreur sur la ligne : %s\n%!" (!values_str)
  in
  read_lines ();;



let process_csv_file file_name insert_only =
  let ic = open_in file_name in
  let separator = ref ',' in
  let field_names = ref [] in
  let nom_table = replace_non_alnum_with_underscore file_name in
  try
    let first_line = input_line ic in
    separator := get_separator first_line;
    let _ = Printf.eprintf "Separator found : %c\n%!" (!separator) in
    field_names := get_field_names first_line (!separator);
    let field_types_, nbligne = analyze_data_lines  (!separator) ic  in
    let field_types = field_types_ |> BatArray.to_list in
    if not insert_only then generate_create_table_statement nom_table !field_names field_types;
    generate_insert_statements file_name nom_table !field_names field_types (!separator) nbligne;

    close_in ic;
  with
  | End_of_file ->
    close_in ic;
    Printf.printf "Error: Empty CSV file\n";;


let copy_with_psql file_name insert_only =
  let ic = open_in file_name in
  let separator = ref ',' in
  let field_names = ref [] in
  let nom_table = replace_non_alnum_with_underscore file_name in
  try
    let first_line = input_line ic in
    separator := get_separator first_line;
    let _ = Printf.eprintf "Separator found : %c\n%!" (!separator) in
    field_names := get_field_names first_line (!separator);
    let field_types_, nbligne = analyze_data_lines  (!separator) ic  in
    let field_types = field_types_ |> BatArray.to_list in
    if not insert_only then generate_create_table_statement nom_table !field_names field_types;
	(*génération de l'instruction copy*)
    let p = Printf.printf "COPY %s(%s) FROM '%s' DELIMITER '%s' CSV HEADER;\n%!" in
    p nom_table (BatString.join "," !field_names) file_name (BatString.of_char !separator) ;
   close_in ic;
  with
  | End_of_file ->
    close_in ic;
    Printf.printf "Error: Empty CSV file\n";;


let main () =
 if (Array.length Sys.argv == 2 && (BatString.count_string Sys.argv.(1) "--" > 0) ) || Array.length Sys.argv < 2 then
	 begin 
		print_endline "usage: autocsv2sql file [--insert-only] [--copy-psql] > sqlfile"; exit 0; 
	 end;
 match Sys.argv with
	 | [| _; "--help" |] | [| _; "-h" |] ->
	 print_endline "usage: autocsv2sql file [--insert-only] [--copy-psql] > sqlfile"
	 | argv ->
		 let file_name = ref "" in
		 let insert_only = ref false in
		 let copy_psql = ref false in
		 Array.iter (fun arg ->
			 match arg with
			 | "--insert-only" -> insert_only := true
			 | "--copy-psql"   -> copy_psql := true
			 | _ when BatString.count_string arg "--" == 0 -> file_name := arg
			 (*| _ when !(insert_only) -> print_endline "Argument inconnu"*)
			 | _ -> ()
		    ) argv;
		 if !file_name <> "" then begin
			if !copy_psql then copy_with_psql !file_name !insert_only
			else process_csv_file !file_name !insert_only;
	 		end
	 	else
		 print_endline "usage: autocsv2sql file [--insert-only] [--copy-psql] > sqlfile";;

let () = main ()

