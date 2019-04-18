open Scraper

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
  day: string;
  dates: string list
}

(** TODO - maybe change to of string to better represent errors *)
exception Broke

exception EndOfSim of state

(** [index_of] returns the indes of a specific day in a state*)
let rec index_of state dates = 
  match dates with 
  |[] -> failwith "date not found"
  |h::t -> if h = state.day then 0 else 1 + index_of state t

(** [get_ticker] returns a stock associated with [ticker] in [stocks] *)
let get_ticker stocks ticker = 
  List.find (fun item -> item.ticker = ticker) stocks

(** [update_val] updates the state with the new prices from [new_day] *)
let rec update_val portfolio new_day stocks =
  match portfolio with 
  |[]-> 0.0
  |h::t -> let ticker_obj = get_ticker stocks (fst h) in
    let price = List.assoc new_day ticker_obj.close_prices  in
    price *. (float_of_int (snd h)) +. update_val t new_day stocks

let next state stocks steps =
  try 
    let new_day = List.nth state.dates (index_of state state.dates + steps) in
    ANSITerminal.(print_string [green] ("Date: " ^ new_day ^ "\n")) ;
    {balance = state.balance;
     portfolio = state.portfolio;
     value = update_val state.portfolio new_day stocks;
     day = new_day;
     dates = state.dates}
  with _ -> raise (EndOfSim 
                     {balance = state.balance;
                      portfolio = state.portfolio;
                      value = update_val state.portfolio "20130809" stocks;
                      day = "20130809";
                      dates = state.dates})


let buy (state:state) stocks ticker amt = 
  let ticker_obj = get_ticker stocks ticker in
  let price = List.assoc state.day ticker_obj.close_prices  in
  if state.balance >= (price *. float_of_int amt) then
    {
      balance = state.balance -. (price *. float_of_int amt);
      portfolio = if List.mem_assoc ticker state.portfolio 
        then List.map (fun item -> 
            if fst item = ticker 
            then (fst item, snd item + amt) else item) state.portfolio
        else (ticker, amt) :: state.portfolio;
      value = state.value +. (price *. float_of_int amt); 
      day = state.day;
      dates = state.dates;
    }
  else 
    raise Broke


let sell state stocks ticker amt = 
  let ticker_obj = get_ticker stocks ticker in
  let price = List.assoc state.day ticker_obj.close_prices  in
  if List.mem_assoc ticker state.portfolio 
  && List.assoc ticker state.portfolio >= amt then
    {
      balance = state.balance +. (price *. float_of_int amt);
      portfolio = List.map (fun item -> 
          if fst item = ticker 
          then (fst item, snd item - amt) else item) state.portfolio;
      value = state.value -. (price *. float_of_int amt); 
      day = state.day;
      dates = state.dates;
    }
  else 
    raise Broke


