

get_pbs <- function(data=all_matches){

    format <- levels(data$Format) %>% tail(n=1)

    PBs <- data %>% 
      filter(last_match == T) %>% 
      group_by(Player) %>% 
      mutate(personal_best=(n_wins==max(n_wins))) %>% 
      filter(personal_best==T) %>%
      group_by(Player) %>% 
      summarise(Format=head(Format,1),n_wins=mean(n_wins),n_losses=mean(n_losses),n=n()) %>% 
      filter(n==1) %>% 
      filter(Format==format) %>% 
      select(Format,Player,n_wins,n_losses) %>% 
      arrange(desc(n_wins))
    
    return(PBs)}
PBs <- get_pbs()
