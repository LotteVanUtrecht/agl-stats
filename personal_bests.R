

get_pbs <- function(data=all_matches){

    format <- levels(data$Format) %>% tail(n=1)

    PBs <- data %>% 
      filter(last_match == T) %>% 
      group_by(Player) %>% 
      mutate(personal_best=(n_wins==max(n_wins))) %>% 
      filter(personal_best==T) %>% 
      filter(Format==format) %>% 
      select(Format,Player,n_wins,n_losses,personal_best) %>% 
      arrange(desc(n_wins))
    
    return(PBs)}
