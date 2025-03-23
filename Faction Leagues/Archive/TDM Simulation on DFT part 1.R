#simulate clan allocation in DFT

#create win rates for each player

all_matches <- read_csv("all_matches.csv")

win_rates <- all_matches %>% 
  filter(Format %in% c("KTK","MKM","OTJ","MH3","BLB","DSK","FDN","PIO")) %>% 
  group_by(Player) %>% 
  summarise(win_rate = mean(Result=="Win"))

win_rate_new_players <- 0.46 #placeholder value

DFT_players <- all_matches %>% 
  filter(Format=="DFT") %>% 
  select(Player) %>% 
  unique() %>% 
  left_join(win_rates) %>% 
  replace_na(list(win_rate=win_rate_new_players))

write_csv(DFT_players,"Faction Leagues/DFT_players_historicalwinrate.csv")

#draft here: https://docs.google.com/spreadsheets/d/1t2svbvisHgW7WJqNvWMBk2vakHfmg8cOXWf6iF6IrIY