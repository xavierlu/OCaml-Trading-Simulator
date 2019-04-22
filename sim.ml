
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
  gtlt: char;
  number: float;
}

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

(** [isValidTrade lst valid] checks if the list of strings [lst]
    comprising of a routine for the simulator is in the correct format *)
let isValid lst valid = 
  List.length lst = 7 
  && (String.equal (List.nth lst 0) "sell" || 
      String.equal (List.nth lst 0) "buy")
  && (isTicker (String.uppercase_ascii (List.nth lst 1)) valid )
  && (isNum (List.nth lst 2) || ((String.equal (List.nth lst 0) "sell")
                                 && (String.equal (List.nth lst 2) "all")))
  &&  (String.equal (List.nth lst 3) "whenever" || 
       String.equal (List.nth lst 3) "once" ||
       String.equal (List.nth lst 3) "twice")
  && (isMeasure (List.nth lst 4))
  && (isCompare (List.nth lst 5))
  && (isNum (List.nth lst 6))

(** [amt_helper s] returns -1 iff s = "all", otherwise returning [s] as an
    int. If [s] is a negative number then failwith "negative shares" *)
let amt_helper s = 
  if s = "all" 
  then -1 
  else if (int_of_string s > 0) 
  then int_of_string s 
  else failwith "negative shares"

(** [get_stock_list s] takes a string [s] of the format "ticker num;ticker num" 
    and creates a list of [string * int] tuples representing those stocks *)
let get_stock_list s stocks =
  let raw_list = String.split_on_char ';' s in
  let rec helper lst =
    match lst with
    | [] -> []
    | h::t ->
      print_endline h;
      let list_of_holding = String.split_on_char ' ' h in
      let ticker = List.nth list_of_holding 0 in
      if isTicker ticker stocks = false then failwith "invalid ticker" else
        let amt = List.nth list_of_holding 1 |> amt_helper in
        (ticker, amt)::helper t
  in helper raw_list

(** [get_portfolio s] takes a string s representing stock holdings separated
    by commas and turns them into a list of [string * int] pairs if they are
    all valid tickers in [stocks]*)
let rec get_portfolio s stocks = 
  let start = (String.index s ' ') + 1 in
  let len = String.length s - start in
  get_stock_list (String.sub s start len) stocks

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
        (print_endline h;
         h::(filter t true) )else filter t false
  in let raw_init = filter unfiltered false in
  let balance = List.nth (List.nth raw_init 0 |> String.split_on_char ' ') 1 |> float_of_string in
  let portfolio = get_portfolio (List.nth raw_init 1) stocks in
  let start_date = List.nth (List.nth raw_init 2 |> String.split_on_char ' ') 1 in
  Ui.make_state stocks balance portfolio [] 0.0 start_date (*still need to implement short_pos value calcuation into here but proof of concept *)


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
          gtlt = String.get (List.nth list_of_rule 5) 0;
          number = float_of_string (List.nth list_of_rule 6);
        } in new_rule::(filter t) )
      else filter t
  in filter unfiltered

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
      let rule_list = get_rules_list path2 stocks in ()
(* unfinished, still need to get the init state and then pass them on to the function that will run the sim with those two *)




let () = main_sim ()
