
open Trade
open Command






let main_sim () = 

 ANSITerminal.(print_string [red]
                  "\nWelcome to Snake Sim Auto-Trader.\nPlease enter the name of your trade file\n");
  ANSITerminal.(print_string [] "> ");
  (*let path = read_line () in *)
  ANSITerminal.(print_string [blue] "\n\tLoading Simulation...\n\n")

  

let () = main_sim ()
