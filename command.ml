
open Str
open Scraper 

type phrase = string list 

type command = 
  | Buy of phrase
  | Sell of phrase
  | Quit 
  | Volatility of phrase
  | SMA of phrase 
  | Skew of phrase 
  | Analysis of phrase
  | Next of phrase
  | Help 
  | View 
  | Price of phrase

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

let getTickers stocks = 
  List.map (fun {ticker = tick ; open_prices = _ ; 
                 high_prices = _ ; low_prices = _;
                 close_prices = _; volumes = _ } -> tick) stocks

(** checks if string represents a ticker in S&P 500 *)
let isTicker str path = 
  List.mem str (getTickers (Scraper.get_data path))

let isNum str = 
  let r = Str.regexp"[0-9]+$" in 
  Str.string_match r str 0

(** does some regex to check if valid trade *)
let isValidTrade lst path = 
  isTicker (String.uppercase_ascii (List.nth lst 1)) path && isNum (List.nth lst 2)


let parse str path = 
  let strlst = String.split_on_char ' ' str in
  let finalLst = removeEmpty strlst in 
  if (List.length finalLst = 0) then raise Empty else
  if (List.nth finalLst 0 = "buy" && List.length finalLst = 3  && isValidTrade finalLst path) then
    Buy (removeFirst finalLst)
  else if (List.nth finalLst 0 = "sell" && List.length finalLst = 3 && isValidTrade finalLst path) then 
    Sell (removeFirst finalLst)
  else if (List.nth finalLst 0 = "quit" && List.length finalLst = 1) then
    Quit
  else if (List.nth finalLst 0 = "view" && List.length finalLst = 1) then
    View
  else if (List.nth finalLst 0 = "volatility" && List.length finalLst = 2 && 
           isTicker (List.nth finalLst 1) path) then
    Volatility (removeFirst finalLst)
  else if ((List.nth finalLst 0 = "next" || List.nth finalLst 0 = "sleep") 
           && (List.length finalLst = 1 || (List.length finalLst = 2 && 
                                            isNum (List.nth finalLst 1)))) then
    Next (removeFirst finalLst)
  else if (List.nth finalLst 0 = "sma" && List.length finalLst = 3 && 
           isTicker (List.nth finalLst 1) path && isNum (List.nth finalLst 2)) then
    SMA (removeFirst finalLst)
  else if (List.nth finalLst 0 = "skew" && List.length finalLst = 2 
           && isTicker (List.nth finalLst 1) path) then
    Skew (removeFirst finalLst)
  else if (List.nth finalLst 0 = "analysis" && List.length finalLst = 2 
           && isTicker (List.nth finalLst 1) path) then
    Analysis (removeFirst finalLst)
  else if (List.nth finalLst 0 = "price") then
    Price (removeFirst finalLst)
  else if (List.nth finalLst 0 = "help") then Help 
  else raise Malformed



