KTK_players <- all_matches %>% 
  filter(Format=="KTK") %>% 
  pull(Player) %>% 
  unique

DFT_players <- all_matches %>% 
  filter(Format=="DFT") %>% 
  pull(Player) %>% 
  unique

sum(!(DFT_players %in% 
  KTK_players)) + 6
