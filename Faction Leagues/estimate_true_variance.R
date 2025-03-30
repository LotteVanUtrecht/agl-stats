all_matches <- read_csv("all_matches.csv",col_types = "cfiTffiiilcil")

#Get first league data
all_results <- all_matches %>% 
  filter(last_match==T) %>% 
  mutate(win_rate=n_wins/n_matches,
         value=n_wins-n_losses)

prolific_results <- all_results %>% 
  filter(Format %in% c("KTK","MKM",'OTJ','MH3','BLB','DSK','FDN','PIO')) %>% 
  filter(n_matches>5) %>% 
  group_by(Player) %>% 
  mutate(n=n()) %>% 
  filter(n==8) %>% 
  summarise(mean=mean(win_rate),
            var=var(win_rate),
            normalitypval=shapiro.test(win_rate)[["p.value"]])

est_true_variance <- mean(prolific_results$var * 9/8)
