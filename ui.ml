open Scraper
open Command
open Trade
open Analysis 

(** [sos_helper x y] concatenates a list of stocks for the portfolio, omitting
    the stock's quantity is 0*)
let sos_helper x y =
  if snd y <> 0 then x ^ fst y ^ ": " ^ string_of_int (snd y) ^ "  " else x ^ ""

(** [string_of_state state] converts a state into a printable string*)
let string_of_state state = 
  "Balance: $" ^ string_of_float state.balance ^ "\nPortfolio: " ^ 
  List.fold_left sos_helper "" state.portfolio 
  ^ "\nValue: $" ^ string_of_float state.value ^ "\nDays: " ^ state.day ^ "\n" ^
  "Short Positions: " ^ 
  (List.fold_left (fun x (a,b,c) -> 
       x ^ a ^ " " ^ b ^ " " ^ (string_of_int c) ^ ", ") "" state.short_positions) ^ "\n"

(** [get_ticker] returns a stock associated with [ticker] in [stocks] *)
let get_ticker stocks ticker = 
  List.find (fun item -> item.ticker = ticker) stocks

(** [get_price] returns the price associated with [ticker] in the list [stocks]
    on the day specified in [state]*)
let get_price stocks ticker state =
  let obj = get_ticker stocks (String.uppercase_ascii ticker) in
  List.assoc state.day obj.close_prices

(** [date_cmp d1 d2] compares two string representations of dates, returning 1
    if d1 is after d2, -1 if d1 is before d2, and 0 if they are the same*)
let date_cmp d1 d2 =
  if int_of_string d1 > int_of_string d2 then 1 else 
  if int_of_string d1 = int_of_string d2 then 0 else -1

(** [add_dates] creates a list from the earliest date in [stock_price_data], 
    using the trading dates for that stock up until [earliest]*)
let rec add_dates stock_price_data earliest = 
  match stock_price_data with 
  |[] -> []
  |h::t -> if date_cmp (fst h) earliest = -1
    then (fst h)::(add_dates t earliest)
    else add_dates [] earliest

(** [dates_helper] creates a list of dates based on [stocks], where the 
    first date in the output is the earliest date any of the stocks in 
    [stocks] and the last ate is the last date of the simulation*)
let rec dates_helper (stocks:stock list) earliest = 
  match stocks with
  | [] -> []
  | h::t -> 
    if date_cmp h.start_date earliest = -1 
    then (add_dates h.open_prices earliest)@(dates_helper t h.start_date)
    else dates_helper t earliest

(** [check_valid_date] repeatedly prompts the user for a date until they input
    a valid date in [dates]. Can be exited if the user types "quit"*)
let rec check_valid_date dates =
  ANSITerminal.(print_string [red] "Please enter a start date (YYYYMMDD).");
  ANSITerminal.(print_string [] "\n> ");
  let date = read_line () in
  if date = "quit" then failwith "byeeee"
  else if List.mem date dates then date 
  else 
    let _ = ANSITerminal.
              (print_string [red] "Invalid Date. Are you sure that's not a weekend or a federal holiday?\n") in
    check_valid_date dates

(** [get_valid_stocks] returns a tuple where the first element is the subset of
    [stocks] that traded on [date] and the second element is the subset of
    [stocks] that did not trade on [date]*)
let rec get_valid_stocks stocks date valid inv =
  match stocks with
  | [] -> (valid, inv)
  | h::t -> if (date_cmp h.start_date date) <= 0 
    then get_valid_stocks t date (h::valid) inv
    else get_valid_stocks t date valid (h::inv)

(** [parse_next] prompts the user for a command, processes it according to the
    output of [Command.parse] and acts accordingly*)
let rec parse_next state stocks s_stocks path =  
  ANSITerminal.(print_string [red] 
                  "Please enter a command, or type help for a list of commands");
  ANSITerminal.(print_string [] "\n> ");
  let input = read_line () in
  try 
    match parse input (fst s_stocks) with 
    | Buy phrase -> let next_state = buy state (fst s_stocks) 
                        (String.uppercase_ascii (List.hd phrase))
                        (int_of_string (List.nth phrase 1)) in 
      ANSITerminal.(print_string [green] (string_of_state next_state)); 
      parse_next next_state stocks s_stocks path
    | Sell phrase -> let next_state = sell state (fst s_stocks) 
                         (String.uppercase_ascii (List.hd phrase))
                         (int_of_string (List.nth phrase 1)) in 
      ANSITerminal.(print_string [green] (string_of_state next_state)); 
      parse_next next_state stocks s_stocks path
    | Quit -> ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")
    | Help -> ANSITerminal.(print_string [blue] "\n\tYou can say: \n\t
      buy [ticker] [vol]\n\t
      sell [ticker] [vol]\n\t
      sma [ticker] [period]\n\t
      price [ticker]\n\t
      momentum [ticker] [period]\n\t
      roc [ticker] [period]\n\t
      skew [ticker]\n\t
      view\n\t
      next [optional:length]\n\t
      quit\n\n"); 
      parse_next state stocks s_stocks path
    | View -> ANSITerminal.(print_string [green] (string_of_state state)); 
      parse_next state stocks s_stocks path
    | Volatility phrase -> 
      ANSITerminal.(print_string [green]  
                      ("Volatility: " ^ string_of_float 
                         (vol (get_ticker stocks (String.uppercase_ascii (List.hd phrase)))
                            (int_of_string (List.nth phrase 1)) state.day) ^ "\n"));
      parse_next state stocks s_stocks path
    | Price phrase -> ANSITerminal.(print_string [green] ((string_of_float (get_price stocks (List.hd phrase) state)) ^ "\n")); 
      parse_next state stocks s_stocks path
    | Next phrase -> let next_state = next state stocks 
                         (int_of_string (List.hd phrase)) in 
      ANSITerminal.(print_string [green] ("Date: " ^ (next_state.day) ^ "\n")) ;
      parse_next (next_state) stocks 
        (get_valid_stocks stocks next_state.day [] []) path 
    | SMA phrase -> 
      ANSITerminal.(print_string [green] 
                      ("Moving Average: " ^ string_of_float 
                         (sma (get_ticker stocks (String.uppercase_ascii (List.hd phrase)))
                            (int_of_string (List.nth phrase 1)) state.day) ^ "\n"));
      parse_next state stocks s_stocks path
    | Skew phrase -> 
      ANSITerminal.(print_string [green] ("Skew: " ^ string_of_float 
                                            (skew (get_ticker stocks (String.uppercase_ascii (List.hd phrase))) 
                                               state.day)^ "\n"));
      parse_next state stocks s_stocks path
    | Momentum phrase -> 
      ANSITerminal.(print_string [green] 
                      ("Momentum: " ^ string_of_float 
                         (momentum (get_ticker stocks (String.uppercase_ascii (List.hd phrase)))
                            (int_of_string (List.nth phrase 1)) state.day) ^ "\n"));
      parse_next state stocks s_stocks path
    | ROC phrase -> 
      ANSITerminal.(print_string [green] 
                      ("Rate of change: " ^ string_of_float 
                         (rate_of_change (get_ticker stocks (String.uppercase_ascii (List.hd phrase)))
                            (int_of_string (List.nth phrase 1)) state.day) ^ "\n"));
      parse_next state stocks s_stocks path
    | Short phrase -> 
      let next_state = short state  (fst s_stocks) 
          (String.uppercase_ascii (List.hd phrase))
          (int_of_string (List.nth phrase 1)) in 
      parse_next next_state stocks s_stocks path
    | Close phrase -> 
      let next_state = close state  (fst s_stocks) 
          (String.uppercase_ascii (List.hd phrase))
          (List.nth phrase 1) in 
      parse_next next_state stocks s_stocks path
    | _ -> failwith "unimplemented"
  with 
  | Empty -> ANSITerminal.(print_string [green] "empty command\n"); 
    parse_next state stocks s_stocks path
  | Malformed -> ANSITerminal.(print_string [green] 
                                 ("Not a valid command: " ^ input ^ "\n")); 
    parse_next state stocks s_stocks path
  | Broke -> ANSITerminal.(print_string [green] 
                             "It smells like broke in here\n"); 
    parse_next state stocks s_stocks path
  | EndOfSim final_state -> 
    ANSITerminal.(print_string [green] "End of simulation.\n");
    ANSITerminal.(print_string [green] ("You started with $10000, 
    now you have:\n" ^ (string_of_state final_state) ^ "\n"));
    ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")

let make_state stocks bal portfolio short_pos value day dates =
  match dates with
  | [] -> let new_dates = dates_helper stocks "999999999" in
    {
      balance = bal;
      portfolio = portfolio;
      short_positions = short_pos;
      value = value;
      day = day;
      dates = new_dates;
    }
  | _::_ -> { 
      balance = bal;
      portfolio = portfolio;
      short_positions = short_pos;
      value = value;
      day = day;
      dates = dates;
    }


(** [main] initializes the game, prompting the user to enter the database file
    they wish to use and creating a valid list of dates and stocks based on 
    that file *)
let main () =
  ANSITerminal.(print_string [red]
                  "\nWelcome to Snake Sim.\nPlease enter the folder of price data to be used\n");
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
    let start_state = make_state stocks 10000.0 [] [] 0.0 date dates in                          
    parse_next start_state stocks separated_stocks path 

(*let () = main ()*)