library("spatstat")

all_matches <- read_csv("all_matches.csv",col_types = "cfiTffiiilcil")

#Get first league data
first_league_results <- all_matches %>% 
  filter(last_match==T) %>% 
  mutate(win_rate=n_wins/n_matches,
         value=n_wins-n_losses) #%>% 
  #mutate(win_rate=log(win_rate/(1-win_rate))) %>% 
  #mutate(win_rate=case_when(win_rate==-Inf ~ -2.5,
  #                          win_rate==Inf ~ 2.5,
  #                          .default = win_rate)) # could use log odds because to makes our distribution more nice maybe?

first_league_results <- first_league_results[!duplicated(first_league_results$Player),]

#filter for our period
first_league_results <- first_league_results %>% 
  filter(n_matches>5) %>% #filter out people with only a couple matches, cutoff at min 6 is pretty arbitrary
  filter(Format %in% c("KTK","MKM",'OTJ','MH3','BLB','DSK','FDN','PIO')) #add DFT before TDM calculatons

#mean first player league performance is 0.4275739
#sd in first player league performance is 0.1488202

prior <- density(first_league_results$win_rate, kernel="biweight")


first_league_results %>% 
  ggplot(aes(x=win_rate)) + 
  geom_density(kernel="biweight")
