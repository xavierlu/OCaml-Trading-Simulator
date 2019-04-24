
open Trade
open Command
open Scraper

(** [rule] represents a valid trading rule. Everything is straightfoward,
    "all" is represented as [amt] = -1 *)
type rule = {
  verb: string;
  ticker: string;
  amt: int;
  freq: string;
  measure: string; (*measure type? *)
  argument: int; (* the args that needed to pass into the measure analysis *)
  gtlt: char;
  number: float;
}

let rule_list = ref []

(** [removeEmpty list] takes in a string list and returns a new list containing
    the same elements with the empty string elements removed. *)
let rec removeEmpty list=
  match list with
  | [] -> []
  | h::t -> if h = "" then removeEmpty (t) else h::(removeEmpty(t))

(** [removeFirst list] takes in a list and returns the same list without its
    first element. *)
let removeFirst list=
  match list with
  | [] -> []
  | h::t -> t

(** [get_ticker] returns a stock associated with [ticker] in [stocks] *)
let get_ticker (stocks:Scraper.stock list) ticker = 
  List.find (fun (item:stock) -> item.ticker = ticker) stocks

(** [getTickers stocks] takes a list of stocks and returns a list of their
    corresponding tickers *)
let getTickers stocks = 
  List.map (fun {ticker = tick ; open_prices = _ ; 
                 high_prices = _ ; low_prices = _;
                 close_prices = _; volumes = _ } -> tick) stocks

(** checks if string represents a ticker in S&P 500 *)
let isTicker str valid= 
  List.mem (String.uppercase_ascii str) (getTickers valid)

(** checks if a string is either "<" or ">" *)
let isCompare str =
  if (String.equal str "<") || (String.equal str ">") then true else false

(** [isNum str] checks if [str] is a string representation of a number *)
let isNum str = 
  let r = Str.regexp"[0-9]+$" in 
  let r2 = Str.regexp "[-][0-9]+$" in
  (Str.string_match r str 0 || Str.string_match r2 str 0)

(** [isMeasure str] checks if [str] is the name of a measure function in
    analysis.ml *)
let isMeasure str = 
  match str with
  | "momentum" | "rate_of_change" | "sma"  | "vol"  | "get_mean" | "skew" 
    -> true
  | _ -> false

let evaluate_measure str argument (stock : Scraper.stock) state =
  match str with
  | "momentum" -> Analysis.momentum stock argument state.day
  | "rate_of_change" -> Analysis.rate_of_change stock argument state.day
  | "sma" -> Analysis.sma stock argument state.day
  | "vol" -> Analysis.vol stock argument state.day
  (*| "get_mean" -> 
    Analysis.get_mean ((sublist stock.close_prices state.day) 30)*)
  | "skew" -> Analysis.skew stock state.day
  | _ -> raise Empty

(** [isValidTrade lst valid] checks if the list of strings [lst]
    comprising of a routine for the simulator is in the correct format *)
let isValid lst valid = 
  (List.length lst = 7 || List.length lst = 8)
  && (String.equal (List.nth lst 0) "sell" || 
      String.equal (List.nth lst 0) "buy")
  && (isTicker (String.uppercase_ascii (List.nth lst 1)) valid )
  && (isNum (List.nth lst 2) || ((String.equal (List.nth lst 0) "sell")
                                 && (String.equal (List.nth lst 2) "all")))
  &&  (String.equal (List.nth lst 3) "whenever" || 
       String.equal (List.nth lst 3) "once" )
  && (isMeasure (List.nth lst 4))
  && (isCompare (List.nth lst 5) || isCompare (List.nth lst 6))
  && (isNum (List.nth lst 6) || isNum (List.nth lst 7))

(** [amt_helper s] returns -1 iff s = "all", otherwise returning [s] as an
    int. If [s] is a negative number then failwith "negative shares" *)
let amt_helper s = 
  if s = "all" 
  then -1 
  else if (int_of_string s > 0) 
  then int_of_string s 
  else failwith "negative shares"

(** [date_list stock] creates a list of every day [stock] has traded *)
let date_list stock = 
  let lst = stock.close_prices in
  let rec helper lst =
    match lst with
    | [] -> []
    | (x, y)::t -> x::(helper t)
  in helper lst

(** [date_helper d sd ticker] checks if [d] is between the start date of [stock]
    and [sd], which is the start date of the simulation *)
let date_helper d sd (stock:Scraper.stock) =
  let stock_sd = stock.start_date |> int_of_string in
  if (int_of_string d <= int_of_string sd && int_of_string d >= stock_sd )
  then List.mem d (date_list stock) else false

(** [get_stock_list s] takes a string [s] of the format "ticker num;ticker num" 
    and creates a list of [string * int] tuples representing those stocks *)
let get_stock_list s stocks =
  let raw_list = String.split_on_char ';' s in
  let rec helper lst =
    match lst with
    | [] -> []
    | h::t ->
      let list_of_holding = String.split_on_char ' ' h in
      let ticker = List.nth list_of_holding 0 in
      if isTicker ticker stocks = false then failwith "invalid ticker" else
        let amt = List.nth list_of_holding 1 |> amt_helper in
        (ticker, amt)::helper t
  in helper raw_list

(** [get_portfolio s] takes a string s representing stock holdings separated
    by semicolons and turns them into a list of [string * int] pairs if they are
    all valid tickers in [stocks]*)
let get_portfolio s stocks = 
  let start = (String.index s ' ') + 1 in
  let len = String.length s - start in
  get_stock_list (String.sub s start len) stocks

let get_short_list s stocks sd = 
  let raw_list = String.split_on_char ';' s in
  let rec helper lst =
    match lst with
    | [] -> []
    | h::t -> 
      let list_of_short = String.split_on_char ' ' h in
      let ticker = List.nth list_of_short 0 in
      if isTicker ticker stocks = false then failwith "invalid ticker" else
        let stock = get_ticker stocks ticker in
        let date = List.nth list_of_short 1 in
        if date_helper date sd stock = false then failwith "invalid date" else
          let amt = List.nth list_of_short 2 |> amt_helper in
          (ticker, date, amt)::helper t
  in helper raw_list


(** [get_short_post s] takes a string s representing short positions separated
    by semicolons and turns them into a list of [string * strint * int] pairs 
    if they are all valid tickers in [stocks]*)
let get_short_pos s stocks sd =
  let start = (String.index s ' ') + 1 in
  let len = String.length s - start in
  get_short_list (String.sub s start len) stocks sd

(** [get_init_state path stocks] returns an initial state for the simulation
    obtained by parsing the file specified by [path]*)
let get_init_state path stocks =
  let unfiltered = Scraper.get_rules path in
  let rec filter lst in_init = 
    match lst with
    | [] -> []
    | h::t ->
      if h = "INIT: {" then filter t true else
      if h = "}" then [] else
      if in_init then
        h::(filter t true) else filter t false
  in let raw_init = filter unfiltered false in
  let balance = List.nth (List.nth raw_init 0 |> String.split_on_char ' ') 1 |> float_of_string in
  let start_date = List.nth (List.nth raw_init 3 |> String.split_on_char ' ') 1 in
  let portfolio = get_portfolio (List.nth raw_init 1) stocks in
  let short_pos = get_short_pos (List.nth raw_init 2) stocks start_date in
  let value = Trade.update_val portfolio short_pos start_date stocks in
  Ui.make_state stocks balance portfolio short_pos value start_date [] (*still need to implement short_pos value calcuation into here but proof of concept *)


(** [get_rules_list path stocks] returns a [rule list] obtained from parsing
    the strings in the file specified by [path] *)
let get_rules_list path stocks =  
  let unfiltered = Scraper.get_rules path in
  let rec filter lst = 
    match lst with
    | [] -> []
    | h::t ->
      let list_of_rule = String.split_on_char ' ' h in
      if (isValid list_of_rule stocks) 
      then (
        let new_rule = {
          verb = List.nth list_of_rule 0;
          ticker = List.nth list_of_rule 1;
          amt = amt_helper (List.nth list_of_rule 2);
          freq = List.nth list_of_rule 3;
          measure = List.nth list_of_rule 4;
          argument = if List.nth list_of_rule 4 <> "skew"
            then int_of_string (List.nth list_of_rule 5) else -1;
          gtlt = 
            String.get (List.nth list_of_rule (if List.nth list_of_rule 4 <> "skew" then 6 else 5)) 0;
          number = 
            float_of_string (List.nth list_of_rule (if List.nth list_of_rule 4 <> "skew" then 7 else 6));
        } in new_rule::(filter t) )
      else filter t
  in filter unfiltered

let extract_gtlt rule = 
  match rule.gtlt with 
  |'<' -> (<)
  |'>' -> (>)
  |_ -> failwith "not an equality operator"

(** [execute_rule] evaluates [rule] and returns the corresponding command
    Raises None if no command needs to be executed *)
let execute_rule state rule dir =
  let comp = extract_gtlt rule in 
  if comp (evaluate_measure rule.measure rule.argument (Trade.get_ticker dir (String.uppercase_ascii rule.ticker)) state) rule.number then 
    let phrase = [rule.ticker; string_of_int rule.amt] in 
    match rule.verb with 
    | "buy" -> Buy (phrase)
    | "sell" -> Sell (phrase)
    | "short" -> Short (phrase)
    | "close" -> Close (phrase)
    |_ -> failwith "not a verb or something"
  else None


(** [date_cmp d1 d2] compares two string representations of dates, returning 1
    if d1 is after d2, -1 if d1 is before d2, and 0 if they are the same*)
let date_cmp d1 d2 =
  if int_of_string d1 > int_of_string d2 then 1 else 
  if int_of_string d1 = int_of_string d2 then 0 else -1

(** [get_valid_stocks] returns a tuple where the first element is the subset of
    [stocks] that traded on [date] and the second element is the subset of
    [stocks] that did not trade on [date]*)
let rec get_valid_stocks stocks date valid inv =
  match stocks with
  | [] -> (valid, inv)
  | h::t -> if (date_cmp h.start_date date) <= 0 
    then get_valid_stocks t date (h::valid) inv
    else get_valid_stocks t date valid (h::inv)

let rec execute_trades state rule_lst valid stocks = 
  match rule_lst with 
  |[] -> state
  |rule::t -> 
    match execute_rule state rule (fst valid) with
    | Buy phrase -> 
      let next_state = buy state stocks
          (String.uppercase_ascii (List.hd phrase))
          (int_of_string (List.nth phrase 1)) in 
      if rule.verb = "buy" && rule.freq = "once" 
      then 
        rule_list := List.filter (fun r -> r <> rule) !rule_list;
      execute_trades next_state t valid stocks
    | Sell phrase -> let next_state = sell state stocks
                         (String.uppercase_ascii (List.hd phrase))
                         (int_of_string (List.nth phrase 1)) in 
      if rule.verb = "sell" && rule.freq = "once" 
      then 
        rule_list := List.filter (fun r -> r <> rule) !rule_list;
      execute_trades next_state t valid stocks
    | Short phrase -> 
      let next_state = short state  stocks
          (String.uppercase_ascii (List.hd phrase))
          (int_of_string (List.nth phrase 1)) in 
      if rule.verb = "short" && rule.freq = "once" 
      then 
        rule_list := List.filter (fun r -> r <> rule) !rule_list;
      execute_trades next_state t valid stocks
    | Close phrase -> 
      let next_state = close state  stocks
          (String.uppercase_ascii (List.hd phrase))
          (List.nth phrase 1) in 
      if rule.verb = "close" && rule.freq = "once" 
      then 
        rule_list := List.filter (fun r -> r <> rule) !rule_list;
      execute_trades next_state t valid stocks
    | None -> execute_trades state rule_lst valid stocks 

    | _ -> failwith "unimplemented"


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

let rec step state stocks =
  let validated_stocks = get_valid_stocks stocks state.day [] [] in
  try 
    let new_state = execute_trades state !rule_list validated_stocks stocks in
    let new_day = next new_state stocks 1 in
    step new_day stocks 
  with 
  | EndOfSim final_state -> 
    ANSITerminal.(print_string [green] "End of simulation.\n");
    ANSITerminal.(print_string [green] ("You started with $10000, 
    now you have:\n" ^ (string_of_state final_state) ^ "\n"));
    ANSITerminal.(print_string [blue] "\n\tGoodbye\n\n")



let main_sim () = 

  ANSITerminal.(print_string [red]
                  "\nWelcome to Snake Sim Auto-Trader.\nPlease enter the folder of price data to be used\n");
  ANSITerminal.(print_string [] "> ");
  let path = read_line () in
  ANSITerminal.(print_string [blue] "\n\tLoading Stock Data...\n\n");
  match path with
  | exception End_of_file -> ()
  | file_name -> let stocks = (Scraper.get_data path) in
    ANSITerminal.(print_string [blue] "\tFile Successfully Loaded!\n\n");
    ANSITerminal.(print_string [red]
                    "\nPlease enter the name of your trade file.\n");
    ANSITerminal.(print_string [] "> ");
    let path2 = read_line () in
    ANSITerminal.(print_string [blue] "\n\tLoading Simulation...\n\n");
    match path2 with
    | exception End_of_file -> ()
    | file_name -> let init_state = get_init_state path2 stocks in 
      rule_list := get_rules_list path2 stocks; 
      ANSITerminal.(print_string [green] (string_of_int (List.length !rule_list))) ;
      step init_state stocks
(* unfinished, still need to get the init state and then pass them on to the function that will run the sim with those two *)

let rec pre_main () =
  ANSITerminal.(print_string [] "\n> "); 
  let response = read_line () in
  if String.uppercase_ascii response = "SIM" then main_sim ()
  else if String.uppercase_ascii response = "UI" then Ui.main ()
  else if response = "quit" then failwith "byeeee"
  else (ANSITerminal.(print_string [red] "Invalid entry. Type \"ui\" for the interactive tool or type \"sim\" for the simulator."); 
        pre_main () )


let () = 
  ANSITerminal.(print_string [red]
                  "\nWelcome to Snake Sim. Type \"ui\" for the interactive tool or type \"sim\" for the simulator.");
  pre_main ()

