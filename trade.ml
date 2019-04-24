open Scraper

type state = {
  balance: float;
  portfolio: (string * int) list;
  short_positions: (string * string * int) list;
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

let second = function (x,y,z) -> y
let first = function (x,y,z) -> x
let third = function (x,y,z) -> z

let rec short_list short_positions = 
  match short_positions with 
  |[] -> []
  |(x,y,z)::t -> (x,y)::(short_list t)

(** [update_val] updates the state with the new prices from [new_day] *)
let rec update_val portfolio short new_day stocks =
  match (portfolio,short) with 
  |([],[])-> 0.0
  |(h1::t1, h2::t2) -> let ticker_obj = get_ticker stocks (fst h1) in
    let short_obj = get_ticker stocks (first h2) in
    let short_value = List.assoc (second h2) short_obj.close_prices -. List.assoc new_day short_obj.close_prices in
    let price = List.assoc new_day ticker_obj.close_prices  in
    price *. (float_of_int (snd h1)) +. short_value *. (float_of_int (third h2)) +. update_val t1 t2 new_day stocks
  |(h::t, _) -> let ticker_obj = get_ticker stocks (fst h) in
    let price = List.assoc new_day ticker_obj.close_prices  in
    price *. (float_of_int (snd h)) +. update_val t [] new_day stocks
  |(_, h::t) ->  
    let short_obj = get_ticker stocks (first h) in
    let short_value = List.assoc (second h) short_obj.close_prices -. List.assoc new_day short_obj.close_prices in
    short_value *. (float_of_int (third h)) +. update_val [] t new_day stocks

let next state stocks steps =
  try 
    let new_day = List.nth state.dates (index_of state state.dates + steps) in
    {balance = state.balance;
     portfolio = state.portfolio;
     value = update_val state.portfolio state.short_positions new_day stocks;
     short_positions = state.short_positions;
     day = new_day;
     dates = state.dates}
  with _ -> raise (EndOfSim 
                     {balance = state.balance;
                      portfolio = state.portfolio;
                      short_positions = state.short_positions;
                      value = update_val state.portfolio state.short_positions "20130809" stocks;
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
      short_positions = state.short_positions;
      value = state.value +. (price *. float_of_int amt); 
      day = state.day;
      dates = state.dates;
    }
  else 
    state

let short (state:state) stocks ticker amt = 
  {
    balance = state.balance;
    portfolio = state.portfolio;
    short_positions = (ticker, state.day, amt) :: state.short_positions;
    value = state.value; 
    day = state.day;
    dates = state.dates;
  }

let rec short_checker short_positions ticker date = 
  match short_positions with 
  |[] -> false
  |(x,y,z)::t -> if x = ticker && y = date then true else short_checker t ticker date

let rec remove_short ticker date short_positions = 
  match short_positions with
  |[] -> []
  |(x,y,z)::t -> if x = ticker && y = date then t
    else (x,y,z)::(remove_short ticker date t)

let rec get_short ticker date short_positions = 
  match short_positions with 
  |[] -> failwith "short position does not exist"
  |(x,y,z)::t -> if x = ticker && y = date then (x,y,z) else get_short ticker date t

let rec extract_tick_num ticker shortpositions = 
match shortpositions with
|[] -> failwith "position does not exist"
|(x,y,z)::t -> if x = ticker then (x,y,z) else extract_tick_num ticker t

let rec close state stocks ticker date = 
  let ticker_obj = get_ticker stocks ticker in
  let price = List.assoc state.day ticker_obj.close_prices in
  if short_checker state.short_positions ticker date  && date <> "-1" then
    let short_obj = get_ticker stocks ticker in 
    let amt = float_of_int (third (get_short ticker date state.short_positions) ) in 
    let profit = (List.assoc date short_obj.close_prices -. price) *. amt in 
    {
      balance = state.balance +. profit;
      portfolio = state.portfolio;
      short_positions = remove_short ticker date state.short_positions;
      value = state.value -. profit; 
      day = state.day;
      dates = state.dates;
    }
  else if List.mem_assoc ticker (short_list state.short_positions) && date = "-1" then 
    let short_obj = get_ticker stocks ticker in 
    let short_boi = extract_tick_num ticker state.short_positions in
    let amt = float_of_int (third (short_boi) ) in 
    let profit = (List.assoc (second short_boi) short_obj.close_prices -. price) *. amt in 
    let iter_state = {
      balance = state.balance +. profit;
      portfolio = state.portfolio;
      short_positions = remove_short ticker (second short_boi) state.short_positions;
      value = state.value -. profit; 
      day = state.day;
      dates = state.dates;
    } in close iter_state stocks ticker date 
  else 
    state


let sell state stocks ticker amt = 
  let ticker_obj = get_ticker stocks ticker in
  let price = List.assoc state.day ticker_obj.close_prices  in
  if List.mem_assoc ticker state.portfolio 
  && (List.assoc ticker state.portfolio >= amt) && amt > 0 then
    {
      balance = state.balance +. (price *. float_of_int amt);
      portfolio = List.map (fun item -> 
          if fst item = ticker 
          then (fst item, snd item - amt) else item) state.portfolio;
      short_positions = state.short_positions;
      value = state.value -. (price *. float_of_int amt); 
      day = state.day;
      dates = state.dates;
    }
  else if List.mem_assoc ticker state.portfolio && amt = -1 then 
  let quantity = List.assoc ticker state.portfolio in
  {
      balance = state.balance +. (price *. float_of_int quantity);
      portfolio = List.remove_assoc ticker state.portfolio;
      short_positions = state.short_positions;
      value = state.value -. (price *. float_of_int quantity); 
      day = state.day;
      dates = state.dates;
    }
  else 
    state


