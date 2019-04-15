open Scraper
open Command

let main () =
  ANSITerminal.(print_string [red]
  "\nWelcome to snake sim.\n");
  ANSITerminal.(print_string [green] "> ");  
  let input = read_line () in
  try 
    match parse (input) with 
    | Buy phrase -> failwith "sdklfjsd"
    | _ -> failwith "sdf"
  with 
    | Empty -> ANSITerminal.(print_string [green] "empty command")
    | Malformed -> ANSITerminal.(print_string [green] ("malformed" ^ input))

(* Execute the game engine. *)
let () = main ()