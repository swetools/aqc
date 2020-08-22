type status = Ok | Not_ok
type directive = Todo of string
               | Skip of string

let plan n_tests =
  print_string "1..";
  print_int n_tests;
  print_newline ()
    
let test ?ord ?description ?directive status =
  (match status with
   | Ok -> print_string "ok"
   | Not_ok -> print_string "not ok");
  (match ord with
   | None -> ()
   | Some n -> print_char ' '; print_int n);
  (match description with
   | None -> ()
   | Some descr -> print_char ' '; print_string descr);
  (match directive with
   | None -> ()
   | Some (Todo s) -> print_string " # TODO "; print_string s
   | Some (Skip s) -> print_string " # SKIP "; print_string s);
  print_newline ()
       
              
let comment msg =
  print_string "# ";
  print_endline msg

let bailout msg =
  print_string "Bail out! ";
  print_endline msg
