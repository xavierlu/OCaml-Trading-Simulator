open Scraper
open Command
open Trade

type stock_array = {
  ticker : string;
  open_prices : float array;
  high_prices: float array;
  low_prices: float array;
  close_prices : float array;
  volumes: float array;
  start_date: string
}

let (current_state:Trade.state) = {balance = 1000.; portfolio = []; value = 0.; day = 0}

(* turn all lists in stock type to arrays *)
let list_to_array stocks = List.map 
    (fun ({ticker = t;
           open_prices = op;
           high_prices = high;
           low_prices = low;
           close_prices = close;
           volumes = vol;
           start_date = date;} : Scraper.stock)
      -> {ticker = t;
          open_prices = Array.of_list op;
          high_prices = Array.of_list high;
          low_prices = Array.of_list low;
          close_prices = Array.of_list close;
          volumes = Array.of_list vol; start_date = date}) stocks 

let string_of_state state = 
  "Balance: $" ^ string_of_float state.balance ^ "\nPortfolio: " ^ 
  List.fold_left (fun x y -> x ^ fst y ^ ": " ^ string_of_int (snd y) ^ "  ") "" state.portfolio 
  ^ "\nValue: $" ^ string_of_float state.value ^ "\nDays: " ^ string_of_int state.day ^ "\n"

let rec parse_next state stocks path =  
  ANSITerminal.(print_string [red] "Please enter a command, or type help for a list of commands");
  ANSITerminal.(print_string [] "\n> ");
  let input = read_line () in
  try 
    match parse input path with 
    | Buy phrase -> let next_state = buy state stocks (String.uppercase_ascii (List.hd phrase))
                        (int_of_string (List.nth phrase 1)) in 
      ANSITerminal.(print_string [green] (string_of_state next_state)); 
      parse_next next_state stocks path
    | Sell phrase -> let next_state = sell state stocks (List.hd phrase) 
                         (int_of_string (List.nth phrase 1)) in 
      ANSITerminal.(print_string [green] (string_of_state next_state)); 
      parse_next next_state stocks path
    | Quit -> ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")
    | Help -> ANSITerminal.(print_string [blue] "\n\tYou can say
      \n\tbuy [ticker] [vol]\n\tsell [ticker] [vol]\n\tvolatility\n\n"); parse_next state stocks path
    | View -> ANSITerminal.(print_string [green] (string_of_state state)); parse_next state stocks path
    | Volatility phrase -> failwith "sdf"
    | Next phrase -> ANSITerminal.(print_string [green] (string_of_float state.balance ^ "\n" ^ string_of_float state.value))
    | _ -> failwith "unimplemented"
  with 
  | Empty -> ANSITerminal.(print_string [green] "empty command\n")
  | Malformed -> ANSITerminal.(print_string [green] ("Not a valid command: " ^ input ^ "\n")); parse_next state stocks path


let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to snake sim.\nPlease enter the folder of price data to be used\n");
  ANSITerminal.(print_string [] "> ");
  let path = read_line () in
  ANSITerminal.(print_string [blue] "\n\tLoading Stock Data...\n\n");
  match path with
  | exception End_of_file -> ()
  | file_name -> let stocks = (Scraper.get_data path) in
    ANSITerminal.(print_string [blue] "\tFile Successfully Loaded!\n\n");
    parse_next current_state stocks path 

let () = main ()