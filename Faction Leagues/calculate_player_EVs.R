get_player_ev <- function(distribution){
  
  league_winrates <- quantile(distribution,seq(0.005,0.995,by=0.01),names=F)
  
  EV <- function(winrate){
    probs <- c(dbinom(0:19,size=10+(0:19),prob=winrate)*(1-winrate),
               dbinom(20:30,size = 30,prob = winrate))
    
    value <- c(0:19-11,20:30-10:0) #wins-losses
    
    expected_value <- sum(probs*value)
    
    return(expected_value)
  }
  
  
  player_EV <- league_winrates %>% 
  sapply(EV) %>% 
  mean()
  
  return(player_EV)
  
}
