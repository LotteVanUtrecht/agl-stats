#depends on estimate_true_variance and first_league_result_and_priors

recent_results <- all_results %>% 
  filter(Format %in% c("KTK","MKM",'OTJ','MH3','BLB','DSK','FDN','PIO')) %>% 
  filter(n_matches>5)

posteriors <- list()

recent_players <- unique(recent_results$Player)

likelihoods <- function(win_rate){
  
  llhoods <- dnorm(x=win_rate,
                   mean=seq(0,1,by=0.005),
                   sd=sqrt(est_true_variance))
  
}

for (player in recent_players){
  
  observed_win_rates <- recent_results %>% 
    filter(Player==player) %>% 
    pull(win_rate)
  
  
  llhoods <- sapply(observed_win_rates, likelihoods,simplify = "array")
  
  posterior <- posterior_first_league
  
  for (i in 1:ncol(llhoods)){
    posterior$y <- posterior$y * llhoods[,i]
    posterior$y <- 201*posterior$y/sum(posterior$y)
  }
  
  posteriors[[player]] <- posterior
  
}

rm(observed_win_rates,llhoods,posterior,player)
