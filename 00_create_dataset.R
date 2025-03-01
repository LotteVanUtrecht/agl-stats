install.packages("tidyverse")
library(tidyverse)

#initialize 
raw_data <- list()

formats <- list.files("Data/") %>% substr(1,3)

for (format in formats){
  raw_data[[format]] <- read_csv(paste0("Data/",format,".csv"))
}

