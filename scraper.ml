open Unix

type stock = {
  ticker : string;
  open_prices : float list;
  close_prices : float list
}

let file_crawler file filename = 
  let separator = ',' in
  let rec crawler file (op: float list) (cp: float list) = 
    try
      let next_line = input_line file in
      let separated = String.split_on_char separator next_line in
      crawler file (float_of_string (List.nth separated 2)::op) (float_of_string (List.nth separated 5)::cp)
    with _ ->
      let ticker_length = (String.index filename '.') - 6 in
      let ticker = String.uppercase_ascii (String.sub filename 6 ticker_length) in
      {ticker = ticker; open_prices = op; close_prices = cp} 
  in
  crawler file [] []


let rec directory_crawler dir path lst = 
  let next_file_name = readdir dir in
  if Filename.check_suffix next_file_name ".csv" then
    let file = open_in (path ^ next_file_name) in
    file_crawler file next_file_name
  else directory_crawler dir path lst

(** [get_data] crawls through the files in directory  [path] 
    and outputs a stock list containing the stock data found in
    the directory
    Requires: csv files have proper formatting and name formatting*)
let get_data (path : string) =
  let dir = opendir path in 
  directory_crawler dir path []