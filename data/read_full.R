library(tidyverse)

#read in full data
full <- read_csv('Most-Recent-Cohorts-All-Data-Elements.csv')

#filter by four year schools that receive Title IV funding
fouryr <- filter(full, HIGHDEG >= 3 & OPEFLAG == 1)

write.csv(fouryr, file = "fouryr.csv")
