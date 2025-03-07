#simulate clan win rates based on BLB

#create win rates for each player

win_rates <- all_matches %>% 
  filter(Format %in% c("KTK","MKM","OTJ","MH3")) %>% 
  group_by(Player) %>% 
  summarise(win_rate = mean(Result=="Win"))

win_rate_new_players <- mean((all_matches %>% 
                                filter(Format=="BLB") %>% 
                                filter(!Player %in% win_rates$Player) %>% 
                                pull(Result))=="Win") #about 0.45, use as fictional win rate for new players

BLB_players <- all_matches %>% 
  filter(Format=="BLB") %>% 
  select(Player) %>% 
  unique() %>% 
  left_join(win_rates) %>% 
  replace_na(list(win_rate=win_rate_new_players))

#########Create points

points <- data.frame(Week=rep(1:6,rep(5,6)),
                     Rank=rep(1:5,6),
                     Points=c(
                       25,15,10,5,0, #Week 1 points
                       30,20,10,5,0, #Week 2 points
                       40,30,20,10,0, #Week 3 points
                       50,30,20,10,0, #Week 4 points
                       50,30,20,10,0, #Week 5 points
                       0,0,0,0,0 #Week 6 points
                     ))



#########START SIMULATION HERE




n <- 1000

simulation_data <- data.frame(Iteration=1:n,
                              week1leader=NA,
                              week6leader=NA,
                              winner_KTK_style=NA)

for (i in 1:n){
  
    #assign clans at random
  

    clans <- c()
    for (x in 1:25){clans <- c(clans,sample(c("Jeskai","Sultai","Abzan","Temur","Mardu")))}
    clans <- c(clans,sample(c("Jeskai","Sultai","Abzan","Temur","Mardu"),1))
    
    #generate simulation data
    
    BLB_players$Clan <- as.factor(clans) 
    
    BLB_matches <- all_matches %>% 
      filter(Format=="BLB") %>% 
      left_join(BLB_players,by="Player")
    
    BLB_points <- BLB_matches %>% 
      group_by(Clan,Week) %>% 
      summarise(win_rate=mean(Result=="Win"),.groups="drop") %>% 
      complete(Week,Clan,fill=list(win_rate=0)) %>% 
      group_by(Week) %>% 
      arrange(desc(win_rate)) %>% 
      mutate(Rank=1:5) %>% 
      left_join(points,by = join_by(Week, Rank))

    
    #analyses
    
    simulation_data$week1leader[[i]] <- BLB_points %>% 
      filter(Week<=1) %>% 
      group_by(Clan) %>% 
      summarise(Points=sum(Points)) %>% 
      filter(Points==max(Points)) %>% 
      pull(Clan) %>% 
      as.character()
    
    week6leader <- BLB_points %>% 
      filter(Week<=6) %>% 
      group_by(Clan) %>% 
      summarise(Points=sum(Points)) %>% 
      filter(Points==max(Points)) %>% 
      pull(Clan)
    
    simulation_data$week6leader[[i]] <- if_else(length(week6leader) == 1,week6leader[1],"Tie")
    
    winner_KTK_style <- BLB_matches %>% 
      filter(Result=="Win") %>% 
      group_by(Clan) %>% summarise(n_wins=n())  %>% 
      filter(n_wins==max(n_wins)) %>% 
      pull(Clan)
    
    simulation_data$winner_KTK_style[[i]] <- if_else(length(winner_KTK_style) == 1,winner_KTK_style[1],"Tie")

}


simulation_data <- simulation_data %>% 
  mutate(early_leader=(week1leader==week6leader),
         early_leader_KTK_style=(week1leader==winner_KTK_style))
