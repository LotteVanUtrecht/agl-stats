all_matches <- read_csv("all_matches.csv")

n_12_3 <- all_matches %>% 
  filter(n_matches==15) %>% 
  filter(n_wins>=12) %>% 
  nrow()

perc_12_3 <- n_12_3/(all_matches %>% 
  filter(last_match==T) %>% 
  nrow)

perc_12_3*135
