open Scraper
open Command
open Trade
open Analysis 

let sos_helper x y =
  if snd y <> 0 then x ^ fst y ^ ": " ^ string_of_int (snd y) ^ "  " else x ^ ""

let string_of_state state = 
  "Balance: $" ^ string_of_float state.balance ^ "\nPortfolio: " ^ 
  List.fold_left sos_helper "" state.portfolio 
  ^ "\nValue: $" ^ string_of_float state.value ^ "\nDays: " ^ state.day ^ "\n"

let get_ticker stocks ticker = 
  List.find (fun item -> item.ticker = ticker) stocks

let get_price stocks ticker state =
  print_endline ticker;
  let obj = get_ticker stocks (String.uppercase_ascii ticker) in
  List.assoc state.day obj.close_prices


let date_cmp d1 d2 =
  if int_of_string d1 > int_of_string d2 then 1 else 
  if int_of_string d1 = int_of_string d2 then 0 else -1

let rec add_dates stock_price_data earliest = 
  match stock_price_data with 
  |[] -> []
  |h::t -> if date_cmp (fst h) earliest = -1 then (fst h)::(add_dates t earliest)
    else add_dates [] earliest

let rec dates_helper (stocks:stock list) earliest = 
  match stocks with
  | [] -> []
  | h::t -> 
    if date_cmp h.start_date earliest = -1 then (add_dates h.open_prices earliest)@(dates_helper t h.start_date)
    else dates_helper t earliest

let rec check_valid_date dates =
  ANSITerminal.(print_string [red] "Please enter a start date.");
  ANSITerminal.(print_string [] "\n> ");
  let date = read_line () in
  if date = "quit" then failwith "byeeee"
  else if List.mem date dates then date else check_valid_date dates

let rec get_valid_stocks stocks date valid inv =
  match stocks with
  | [] -> (valid, inv)
  | h::t -> if (date_cmp h.start_date date) <= 0 
    then get_valid_stocks t date (h::valid) inv
    else get_valid_stocks t date valid (h::inv)


let rec parse_next state stocks (s_stocks:Scraper.stock list * Scraper.stock list) path =  
  ANSITerminal.(print_string [red] "Please enter a command, or type help for a list of commands");
  ANSITerminal.(print_string [] "\n> ");
  let input = read_line () in
  try 
    match parse input (fst s_stocks) with 
    | Buy phrase -> let next_state = buy state (fst s_stocks) (String.uppercase_ascii (List.hd phrase))
                        (int_of_string (List.nth phrase 1)) in 
      ANSITerminal.(print_string [green] (string_of_state next_state)); 
      parse_next next_state stocks s_stocks path
    | Sell phrase -> let next_state = sell state (fst s_stocks) (String.uppercase_ascii (List.hd phrase))
                         (int_of_string (List.nth phrase 1)) in 
      ANSITerminal.(print_string [green] (string_of_state next_state)); 
      parse_next next_state stocks s_stocks path
    | Quit -> ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")
    | Help -> ANSITerminal.(print_string [blue] "\n\tYou can say
      \n\tbuy [ticker] [vol]\n\tsell [ticker] [vol]\n\tvolatility\n\n"); parse_next state stocks s_stocks path
    | View -> ANSITerminal.(print_string [green] (string_of_state state)); parse_next state stocks s_stocks path
    | Volatility phrase -> failwith "unimplemented"
    | Price phrase -> ANSITerminal.(print_string [green] ((string_of_float (get_price stocks (List.hd phrase) state)) ^ "\n")); 
      parse_next state stocks s_stocks path
    | Next phrase -> let next_state = next state stocks (int_of_string (List.hd phrase)) in 
      parse_next (next_state) stocks (get_valid_stocks stocks next_state.day [] []) path 
    | SMA phrase -> 
      ANSITerminal.(print_string [green] (string_of_float 
                                            (sma (get_ticker stocks (List.hd phrase)) (int_of_string (List.nth phrase 1)) state.day)));
    | _ -> failwith "unimplemented"
  with 
  | Empty -> ANSITerminal.(print_string [green] "empty command\n"); parse_next state stocks s_stocks path
  | Malformed -> ANSITerminal.(print_string [green] ("Not a valid command: " ^ input ^ "\n")); parse_next state stocks s_stocks path
  | Broke -> ANSITerminal.(print_string [green] "It smells like broke in here\n"); parse_next state stocks s_stocks path
  | EndOfSim final_state -> 
    ANSITerminal.(print_string [green] "End of simulation.\n");
    ANSITerminal.(print_string [green] ("You started with $10000, now you have:\n" ^ (string_of_state final_state) ^ "\n"));
    ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")

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
    let dates = dates_helper stocks "999999999" in
    let date = check_valid_date dates in
    let separated_stocks = get_valid_stocks stocks date [] [] in 
    let (start_state:Trade.state) = {balance = 10000.; portfolio = []; value = 0.; day = date; dates = dates} in 
    parse_next start_state stocks separated_stocks path 

let () = main ()