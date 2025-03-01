install.packages("tidyverse")
library(tidyverse)

#download data 
raw_data <- list()

formats <- list.files("Data/") %>% substr(1,3)

for (format in formats){
  raw_data[[format]] <- read_csv(paste0("Data/",format,".csv"))
}

#create a single table
all_matches <- data.frame(Format=character(),
                          Timestamp=character(),
                          `Winner Name`=character(),
                          `Loser Name`=character(),
                          Score=character())

for (format in formats){
  
  tmp <- raw_data[[format]] %>% 
    select(1:4) %>% 
    mutate(Format=format)
  
  all_matches <- rbind(all_matches,tmp)

}

rm(tmp) #remove temp

#data cleaning

all_matches <- all_matches %>% 
  rename(Time=Timestamp,Winner=`Winner Name`,Loser=`Loser Name`) %>% 
  filter(!is.na(`Winner`)) %>% 
  mutate(Time=mdy_hms(Time),
         Winner=Winner %>% sub(pattern=" - ",replacement="@") %>% str_extract(pattern="^([^@])+"),
         Loser=Loser %>% sub(pattern=" - ",replacement="@") %>% str_extract(pattern="^([^@])+")
         ) %>% 
  arrange(Time) %>% 
  mutate(Score=case_when(Result == "2-0" ~ "2-0",
                         Result == "2-1" ~ "2-1",
                         .default = NA), #handles bo5 matches in KLR
         Format=factor(Format,levels=unique(Format))) %>%  #this puts the formats in chronological order
  select(-Result)
  
formats <- levels(all_matches$Format)

#add weeks and match code
all_matches <- all_matches %>% 
  group_by(Format) %>% 
  mutate(start_time=min(Time) %>% floor_date("hour")) %>% 
  mutate(Week=interval(start_time,Time) %>% 
           as.duration() %>% 
           as.numeric("weeks") %>% 
           ceiling()) %>% 
  select(-start_time) %>% 
  filter(Week<7) %>% #removes a test Johnny put in for DBL
  group_by(Format,Week) %>% 
  mutate(MatchCode=1:n()) %>%
  ungroup() %>% 
  mutate(MatchCode = paste0(Format,Week,str_pad(MatchCode,width=3,side="left",pad="0")))
  
#pivot

all_matches <- all_matches %>% 
  pivot_longer(cols=c(Winner,Loser),names_to = "Result", values_to="Player") %>% 
  mutate(Player=as.factor(Player),
         Result=if_else(Result=="Winner","Win","Loss"))

#add n_matches, n_wins, n_losses

all_matches <- all_matches %>% 
  group_by(Player,Format) %>% 
  mutate(n_matches=1:n(),
         last_match=(n_matches==n())) %>% 
  group_by(Player,Format,Result) %>% 
  mutate(n_wins=1:n(),
         n_losses=1:n()) %>% 
  mutate(n_wins=if_else(Result=="Win",n_wins,n_matches-n_losses),
         n_losses=if_else(Result=="Win",n_losses,n_matches-n_wins)) %>% 
  ungroup()

#finishing touches
all_matches <- all_matches %>% 
  relocate(MatchCode,Format,Week,Time,Player,Result,n_matches,n_wins,n_losses,last_match,Score) %>% 
  mutate(
    weeks_in_league = if_else(Format %in% c("DBL","ELD","SET","SIR","NEp","XLR","CH3"),3,6),
    team_league = if_else(Format %in% c("DBL","SET","NEp","CH3"),3,6)
  )
