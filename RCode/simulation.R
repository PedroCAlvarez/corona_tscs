#City and Protest data

library(readr)
library(tidyverse)
# Import Count Love Data


count_love <- read_csv("data/simulation/protest/count_love/count_love_racial_injustice.csv")

census<- read_csv("data/simulation/protest/census_city_data/sub-est2019_all.csv")

#Change City Districts to Cities
library(qdap)
count_love$NAME<-char2end(count_love$City, ", ")
count_love$STNAME<-count_love$`State Name`

# Left Join by State and City

test<-left_join(count_love,census,by=c("STNAME","NAME"))

table(census$STNAME)
table(count_love$STNAME) 

count_love$STNAME %in% census$STNAME 

count_love$NAME %in% census$NAME 
