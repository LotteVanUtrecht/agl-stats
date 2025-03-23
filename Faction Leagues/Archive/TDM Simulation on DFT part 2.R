#continued from here: https://docs.google.com/spreadsheets/d/1t2svbvisHgW7WJqNvWMBk2vakHfmg8cOXWf6iF6IrIY

DFT_clans <- read_csv("Faction Leagues/TDM Simulation on DFT - Download.csv",col_names = c("Player","HistoricalWinRate","Clan"))


DFT_matches <- all_matches %>% 
  filter(Format=="DFT") %>% 
  left_join(DFT_clans,by='Player')



points <- data.frame(Week=rep(1:6,rep(5,6)),
                     Rank=rep(1:5,6),
                     Points=c(
                       25,20,15,10,5, #Week 1 points
                       30,24,18,12,6, #Week 2 points
                       35,28,21,14,7, #Week 3 points
                       40,32,24,16,8, #Week 4 points
                       40,32,24,16,8, #Week 5 points
                       0,0,0,0,0 #Week 6 points
                     ))


DFT_points <- DFT_matches %>% 
  group_by(Clan,Week) %>% 
  summarise(win_rate=mean(Result=="Win"),.groups="drop") %>% 
  complete(Week,Clan,fill=list(win_rate=0)) %>% 
  group_by(Week) %>% 
  arrange(desc(win_rate)) %>% 
  mutate(Rank=1:5) %>% 
  left_join(points,by = join_by(Week, Rank))
