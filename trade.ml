open Scraper

type state = {
  balance: float;
  portfolio: (string * int) list;
  value: float; 
  day: string;
  dates: string list
}

let get_ticker stocks ticker = 
  List.find (fun item -> item.ticker = ticker) stocks

let buy (state:state) stocks ticker amt = 
  let ticker_obj = get_ticker stocks ticker in
  let price = List.assoc state.day ticker_obj.close_prices  in
  if state.balance >= (price *. float_of_int amt) then
    {
      balance = state.balance -. (price *. float_of_int amt);
      portfolio = if List.mem_assoc ticker state.portfolio 
        then List.map (fun item -> if fst item = ticker then (fst item, snd item + amt) else item) state.portfolio
        else (ticker, amt) :: state.portfolio;
      value = state.value +. (price *. float_of_int amt); 
      day = state.day;
      dates = state.dates;
    }
  else 
    failwith "ur broke fam"


let sell state stocks ticker amt = 
  let ticker_obj = get_ticker stocks ticker in
  let price = List.assoc state.day ticker_obj.close_prices  in
  if List.mem_assoc ticker state.portfolio && List.assoc ticker state.portfolio >= amt then
    {
      balance = state.balance +. (price *. float_of_int amt);
      portfolio = List.map (fun item -> if fst item = ticker then (fst item, snd item - amt) else item) state.portfolio;
      value = state.value -. (price *. float_of_int amt); 
      day = state.day;
      dates = state.dates;
    }
  else 
    failwith "fam why u tryna sell more than what u have"

