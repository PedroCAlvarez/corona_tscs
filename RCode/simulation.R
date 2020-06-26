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
census$NAME <- gsub(" township", "", census$NAME)
census$NAME <- gsub(" town", "", census$NAME)
census$NAME <- gsub(" borough", "", census$NAME)
census$NAME <- gsub(" County", "", census$NAME)
census$NAME <- gsub(" municipality", "", census$NAME)
census$NAME <- gsub(" village", "", census$NAME)

census[which(census$NAME=="Louisville/Jefferson metro government (balance)"),"NAME"]  = "Louisville"



# Left Join by State and City

join<-left_join(count_love,census,by=c("STNAME","NAME"))

final<- join %>% 
  distinct(Date, Location, Attendees, Source, .keep_all = TRUE)


#save csv

write.csv(final,"data/simulation/protest/protest_population.csv")


# Unemployment Data
unemployment <- read.csv("data/simulation/unemployment/unemployment_raw.csv",head = TRUE, sep=";")

unemployment<-unemployment%>% mutate_if(is.factor, as.character)
unemployment$Initial.Claims <- gsub(",","",unemployment$Initial.Claims)
unemployment$Continued.Claims <- gsub(",","",unemployment$Continued.Claims)
unemployment$Covered.Employment <- gsub(",","",unemployment$Covered.Employment)

names(unemployment)
unemployment$Filed.week.ended <- as.Date(unemployment$Filed.week.ended , "%m/%d/%Y")
unemployment$Reflecting.Week.Ended <- as.Date(unemployment$Reflecting.Week.Ended, "%m/%d/%Y")
unemployment$Initial.Claims <- as.numeric(unemployment$Initial.Claims)
unemployment$Continued.Claims <- as.numeric(unemployment$Continued.Claims)
unemployment$Covered.Employment <- as.numeric(unemployment$Covered.Employment)

write.csv(unemployment,"data/simulation/unemployment/unemployment.csv")



