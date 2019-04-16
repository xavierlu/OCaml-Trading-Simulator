open Scraper

let buy ticker amt = 
  ANSITerminal.(print_string [] "\nyou bought a thing\n");
  ticker
