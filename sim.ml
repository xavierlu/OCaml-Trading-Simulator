
open Trade
open Command
open Scraper

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

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
  | "momentum" -> true
  | "rate_of_change" -> true
  | "sma" -> true
  | "vol" -> true
  | "get_mean" -> true
  | "skew" -> true
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

let get_rules_list path stocks =  
  let unfiltered = Scraper.get_rules path in
  let rec filter lst = 
    match lst with
    | [] -> []
    | h::t ->
      let list_of_rule = String.split_on_char ' ' h in
      if (isValid list_of_rule stocks) 
      then 
        (print_endline h;
         h::(filter t))
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
    | file_name -> let rule_list = get_rules_list path2 stocks in ()
(* unfinished, still need to get the init state and then pass them on to the function that will run the sim with those two *)




let () = main_sim ()
