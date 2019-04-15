open Scraper
open Command

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
}

type stock_array = {
  ticker : string;
  open_prices : float array;
  high_prices: float array;
  low_prices: float array;
  close_prices : float array;
  volumes: float array;
}

let path = "quantquote_daily_sp500_83986/daily"

let stocks = Scraper.get_data path

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