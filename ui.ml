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

let path = "quantquote_daily_sp500_83986/daily"

let stocks = Scraper.get_data path

(* turn all lists in stock type to arrays *)
let stocks_array = List.map (fun ({ticker = t;
  open_prices = op;
  high_prices = high;
  low_prices = low;
  close_prices = close;
  volumes = vol;
  start_date = date;} : Scraper.stock)  -> {ticker = t;
  open_prices = Array.of_list op;
  high_prices = Array.of_list high;
  low_prices = Array.of_list low;
  close_prices = Array.of_list close;
  volumes = Array.of_list vol; start_date = date}) stocks 

let main () =
  ANSITerminal.(print_string [red]
  "\nWelcome to snake sim.\n");
  ANSITerminal.(print_string [green] "> ");  
  let input = read_line () in
  try 
    match parse (input) with 
    | Buy phrase -> failwith "be happy"
    | Sell phrase -> failwith "sdklfjsd"
    | Quit -> failwith "dsf"
    | Volatility phrase -> failwith "sdf"
    | Next phrase -> failwith "sdfd"
  with 
    | Empty -> ANSITerminal.(print_string [green] "empty command")
    | Malformed -> ANSITerminal.(print_string [green] ("malformed: " ^ input))

let () = main ()