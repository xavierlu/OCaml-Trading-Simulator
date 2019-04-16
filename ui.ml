open Scraper
open Command

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
  day : int
}

type stock_array = {
  ticker : string;
  open_prices : float array;
  high_prices: float array;
  low_prices: float array;
  close_prices : float array;
  volumes: float array;
  start_date: string
}

(* turn all lists in stock type to arrays *)
let stocks_array stocks = List.map 
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

let rec parse_next stocks =  
  ANSITerminal.(print_string [red] "Please enter a command, or type help for a list of commands");
  ANSITerminal.(print_string [] "\n> ");
  let input = read_line () in
  try 
    match parse (input) with 
    | Buy phrase -> let _ = Trade.buy 14 4 in parse_next stocks
    | Sell phrase -> failwith "sdklfjsd"
    | Quit -> ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")
    | Volatility phrase -> failwith "sdf"
    | Next phrase -> failwith "sdfd"
  with 
  | Empty -> ANSITerminal.(print_string [green] "empty command\n")
  | Malformed -> ANSITerminal.(print_string [green] ("malformed: " ^ input ^ "\n"))


let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to snake sim.\nPlease enter the folder of price data to be used\n");
  ANSITerminal.(print_string [] "> ");
  let input = read_line () in
  ANSITerminal.(print_string [blue] "\n\tLoading Stock Data...\n\n");
  match input with
  | exception End_of_file -> ()
  | file_name -> let stocks = Scraper.get_data input in
    ANSITerminal.(print_string [blue] "\tFile Successfully Loaded!\n\n");
    parse_next stocks

let () = main ()