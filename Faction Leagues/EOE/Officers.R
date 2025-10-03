EOE_players <- read_csv("Faction Leagues/EOE/Sheet.csv") %>% 
  select(Name,Captain,Rookie) %>% 
  drop_na()

EOE_officers <- EOE_players %>% 
  filter(Captain==FALSE) %>% 
  filter(Rookie==FALSE) %>% 
  rename(Player=Name)

win_rates <- all_matches %>% 
  filter(Format %in% c("DSK","FDN","PIO","DFT","NXT","TDM","PLG","FIN")) %>% 
  group_by(Player) %>% 
  summarise(win_rate=sum(n_wins)/sum(n_matches))

EOE_officers <- EOE_officers %>% 
  left_join(win_rates) %>% 
  arrange(desc(win_rate)) %>% 
  head(47)
#Ike P = 0.58387800
#Oceane P = 0.2573529