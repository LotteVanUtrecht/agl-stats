top8_cutoffs <- data.frame(Format=all_matches$Format %>% unique,cutoff=numeric(length(all_matches$Format %>% unique())))

for (format in top8_cutoffs$Format){
  places <- all_matches %>% 
    filter(Format==format,last_match) %>% 
    arrange(desc(n_wins)) %>% 
    pull(n_wins)
  
  if (places[8]==places[9]){
    top8_cutoffs$cutoff[top8_cutoffs$Format==format] <- places[8]
  } else {
    top8_cutoffs$cutoff[top8_cutoffs$Format==format] <- places[8]-0.5
  }
  
}

all_matches <- all_matches %>% 
  left_join(top8_cutoffs)
rm(format,formats,places)
