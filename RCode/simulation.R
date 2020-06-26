#City and Protest data
rm(list=ls())
library(readr)
library(tidyverse)
# Import Count Love Data


count_love <- read_csv("data/simulation/protest/count_love/count_love_racial_injustice.csv")

census<- read_csv("data/simulation/protest/census_city_data/sub-est2019_all.csv")


census<- census[,c(9,10,22)]

#Change City Districts to Cities
library(qdap)
count_love$NAME<-char2end(count_love$City, ", ")
count_love$STNAME<-count_love$`State Name`

census$NAME <- gsub(" city", "", census$NAME)
# Left Join by State and City

join<-left_join(count_love,census,by=c("STNAME","NAME"))

final<- join %>% 
  distinct(Date, Location, Attendees, Source, .keep_all = TRUE)


#save csv

write.csv(final,"data/simulation/protest/protest_population.csv")
