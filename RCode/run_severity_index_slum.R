# Run idealstan model on the combined dataset
# Create a activity index from the policies
# Robert Kubinec
# April 4th, 2020

require(idealstan)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)
require(readr)
require(stringr)
require(parallel)

# load cleaned file

clean <- read_csv("data/CoronaNet/coronanet_release.csv") %>% 
  filter(init_country_level=="National",
         type!="Other Policy Not Listed Above") %>% 
  select(entry_type,update_type,type,country,
         date_updated,
         compliance,date_announced,
         target_geog_level,
         type_sub_cat,
         target_country,
         target_direction,
         travel_mechanism,
         target_who_what,
         date_end,
         date_start,
         record_id) %>% 
  distinct

# need to make sub-categories

# clean$type_preschool <- grepl(x=clean$type_sub_cat,pattern="Preschool")
# clean$type_primaryschool <- grepl(x=clean$type_sub_cat,pattern="Primary")
# clean$type_secondschool <- grepl(x=clean$type_sub_cat,pattern="Secondary Schools")
# clean$type_highered <- grepl(x=clean$type_sub_cat,pattern="Higher education")
# clean$type_self_quarantine <- grepl(x=clean$type_sub_cat,pattern="Self-Quarantine")
# clean$type_bars <- grepl(x=clean$type_sub_cat,pattern="Restaurants/Bars")
# clean$type_retail <- grepl(x=clean$type_sub_cat,pattern="Retail Businesses")
# clean$type_shopping <- grepl(x=clean$type_sub_cat,pattern="Shopping Centers")
# clean$type_grooming <- grepl(x=clean$type_sub_cat,pattern="Personal Grooming")
# clean$type_advisory <- grepl(x=clean$type_sub_cat,pattern="Travel Advisory")
# clean$type_health_staff <- grepl(x=clean$type_sub_cat,pattern="Doctors|Nurses|Health Staff|Hospitals")
# clean$type_health_ppe <- grepl(x=clean$type_sub_cat,pattern="Personal Protective Equipment")
# clean$type_health_masks <- grepl(x=clean$type_sub_cat,pattern="Masks")
# clean$type_govt_quar <- grepl(x=clean$type_sub_cat,pattern="Government Quarantine")
# clean$type_quar_restrict <- grepl(x=clean$type_sub_cat,pattern="Quarantine only applies to people of certain ages")
# clean$type_sanitizer <- grepl(x=clean$type_sub_cat,pattern="Hand Sanitizer")
# clean$type_screenings <- grepl(x=clean$type_sub_cat,pattern="Health Screenings|Health Certificates")
# clean$type_health_other <- grepl(x=clean$type_sub_cat,pattern="Health Volunteers|Health Research|Health Infrastructure|Health Materials")
# clean$type_testing <- grepl(x=clean$type_sub_cat,pattern="Public Testing|Test Kits")
# clean$type_temporary <- grepl(x=clean$type_sub_cat,pattern="Temporary")

# gather to make new sub-category with new recordIDs

# clean <- select(clean,-type_sub_cat) %>% gather(key="extra_cat",value=value_cat,type_preschool:type_temporary) %>% 
#   group_by(record_id) %>% 
#   mutate(combine_type=ifelse(value_cat,paste0(type,"_",extra_cat),
#                              type)) %>% 
#   select(-value_cat,-extra_cat) %>% 
#   distinct %>% 
#   group_by(combine_type) %>% 
#   mutate(combine_type2=ifelse(n()>2,combine_type,type)) %>% 
#   ungroup %>% 
#   select(-combine_type,combine_type="combine_type2")


clean <- clean %>% 
  mutate(compliance=as.numeric(grepl(x=compliance,pattern="Mandatory")),
    # compliance=case_when(grepl(x=compliance,pattern="Mandatory")~2,
    #                           grepl(x=compliance,pattern="Voluntary")~1,
    #                           TRUE~2),
         date_start=ymd(date_start),
         date_end=ymd(date_end)) %>% 
  filter(!is.na(date_start),
         !is.na(type)) %>% 
  mutate(target_geog_level=case_when(target_geog_level %in% c("One or more regional groupings",
                                                              "All countries",
                                                              "One or more countries and one or more regional groupings",
                                                              "One or more countries, but not all countries") &
                                       !grepl(x=type,pattern="External")~"country",
                                     TRUE~target_geog_level),
    target_geog_level=case_when(grepl(x=type,pattern="External")~as.numeric(forcats::fct_collapse(factor(target_geog_level,levels=c("A geographical or administrative unit within a country",
                                                             "One or more countries, but not all countries",
                                                             "One or more countries and one or more regional groupings",
                                                             "One or more regional groupings",
                                                             "All countries")),
                                                             region=c("One or more countries and one or more regional groupings",
                                                                      "One or more regional groupings",
                                                                      "One or more countries, but not all countries"))),
                                TRUE~as.numeric(factor(target_geog_level,levels=c("A geographical or administrative unit within a country",
                                                                                  "country")))),
    type_sub_cat=recode(type_sub_cat,
                        `Self-Quarantine (i.e. quarantine at home)`="type_self_quarantine",
                        `Secondary Schools (generally for children ages 10 to 18)`="type_secondschool",
                        `Primary Schools (generally for children ages 10 and below)`="type_primaryschool",
                        `Higher education (i.e. degree granting institutions)`="type_highered",
                        `Government Quarantine (i.e. quarantine at a government hotel or facility)`="type_govt_quar",
                        `Preschool or childcare facilities (generally for children ages 5 and below)`="type_preschool",
                        `Restaurants/Bars`="type_bars",
                        `Non-Essential Commercial Businesses`="type_commerce",
                        `Retail Businesses`="type_retail",
                        `Travel History Form (e.g. documents where traveler has recently been)`="travel_hist",
                        `Other Businesses`="other",
                        `Shopping Centers`="type_shopping",
                        `Quarantine outside the home or government facility (i.e. quarantine in a hotel)`="hotel_quar",
                        `Health Screenings (e.g. temperature checks)`="type_screenings",
                        `Personal Grooming Businesses (e.g. hair salons)`="type_grooming",
                        `Other Health Materials`="other",
                        `Masks`="type_health_masks",
                        `Unspecified Health Staff`="type_health_staff",
                        `Unspecified Health Materials`="type_health_other",
                        `Other`="other",
                        `Other Health Infrastructure`="other_facilities",
                        `Other Heath Staff`="type_health_staff",
                        `Temporary Quarantine Centers`="quar_facility",
                        `Test Kits`="type_testing",
                        `Public Testing Facilities (e.g. drive-in testing for COVID-19)`="public_tests",
                        `Temporary Medical Centers`="type_temporary",
                        `Health Certificates`="health_cert",
                        `Health Research Facilities`="health_research",
                        `Quarantine only applies to people of certain ages. Please note the age restrictions in the text box.`="type_quar_restrict",
                        `Ventilators`="ventilators",
                        `Health Volunteers`="volunteers",
                        `Hand Sanitizer`="type_sanitizer"),
    type_sub_cat=case_when(type=="Closure of Schools" & type_sub_cat %in% c("type_govt_quar",
                                                                            "type_self_quarantine")~"",
                           type=="External Border Restrictions" & type_sub_cat %in% c("type_govt_quar",
                                                                                      "type_self_quarantine",
                                                                                      "other",
                                                                                      "Other")~"",
                            type=="Health Monitoring" & type_sub_cat %in% c("other","type_screenings",
                                                                            "type_self_quarantine")~"",
                            type=="Health Testing" & type_sub_cat %in% c("travel_hist",
                                                                         "type_testing",
                                                                         "Hospitals",
                                                                         "type_screenings")~"",
                           type=="Health Testing" & type_sub_cat=="health_research"~"",
                            type=="Hygiene"~"",
                            type=="Internal Border Restrictions"~"",
                            type=="New Task Force, Bureau or Administrative Configuration"~"",
                            type=="Public Awareness Measures"~"",
                            type=="Quarantine/Lockdown" & type_sub_cat %in% c("type_screenings",
                                                                              "type_health_other",
                                                                              "travel_hist")~"",
    type=="Restriction of Non-Essential Businesses" & type_sub_cat %in% c("Yes","type_self_quarantine")~"",
    type=="Restrictions of Mass Gatherings"~"",
    type=="Social Distancing"~"",
    type_sub_cat %in% c("Yes","No","Vaccines","markets)","All or unspecified non-essential businesses",
                        "government buildings","Health Insurance","Other Areas Hygiene Measures Applied",
                        "Public areas (e.g. mosques",
                        "schools)",
                        "All non-essential government services restricted",
                        "Attendance at religious services prohibited (e.g. mosque/church closings)",
                        "Cancellation of an annually recurring event (e.g. election",
                        "Inside public or commercial building (e.g. supermarkets)",
                        "national festival)",
                        "New Task Force or Bureau (i.e. establishment of a temporary body)",
                        "Disseminating information related to COVID-19 to the public that is reliable and factually accurate",
                        "Keeping a distance of at least 6 feet or 1.5 meters apart",
                        "Commercial areas (e.g. shopping malls",
                        "other","Other",
                        "markets)"~"")~'',
    is.na(type_sub_cat)~"",
    TRUE~type_sub_cat),
    combine_type=paste0(type,"_",type_sub_cat)) %>% 
  # sum target countries matrix
  group_by(record_id) %>% 
  mutate(target_country_num=case_when(all(target_country==unique(country),na.rm=T)~NA_real_,
                                      target_country==""~NA_real_,
                                      "All countries" %in% target_country~216,
                                   TRUE~as.numeric(length(unique(target_country))))) %>% 
  ungroup %>% 
       # mutate(target_country_num=(target_country_num-min(target_country_num,na.rm=T))/(max(target_country_num,na.rm=T)-min(target_country_num,na.rm=T))) %>% 
  distinct(country,target_country_num,combine_type,date_start,date_end,target_geog_level,compliance,update_type,date_updated) %>% 
  filter(!combine_type %in% c("Closure of Schools_","Anti-Disinformation Measures_","Hygiene_"))


# complete the matrix

# function that takes a vector and increases over time values

set_deck <- function(x) {
  for(i in 2:length(x)) {
    if(!is.na(x[i-1])) {
      if(!is.na(x[i])) {
        if(x[i-1]>x[i]) {
          x[i] <- x[i-1]
        } else {
          x[i] <- x[i]
        }
      } else {
        x[i] <- x[i-1]
      }
      
    } else {
      x[i] <- x[i]
    }
  }
  return(x)
}

clean_comp <- clean %>% complete(country,combine_type,date_start) %>% group_by(country,combine_type) %>% 
  arrange(country,combine_type,date_start) %>% 
  mutate(compliance=set_deck(compliance),
         target_geog_level=set_deck(target_geog_level)) %>% 
  ungroup %>% 
  mutate(target_country_num=coalesce(target_country_num,0),
         compliance=coalesce(compliance,0),
         target_geog_level=ifelse(compliance==1,target_geog_level,0),
         target_geog_level=ifelse(is.na(target_geog_level) & compliance==1,2,target_geog_level),
         compliance=ifelse(combine_type=="Restriction of Non-Essential Businesses_type_bars",
                           recode(compliance,`2`=0,
                           `0`=2),compliance),
         combine_disc=ifelse(grepl(x=combine_type,pattern="External"),
                             target_geog_level,
                             compliance)) %>% 
  group_by(country) %>% 
  filter(date_start<today(),!all(compliance==0)) %>% 
  ungroup %>% 
  mutate(country=factor(country),
         country=relevel(country,"United States of America")) %>% 
  group_by(date_start) %>% 
  mutate(total_day=sum(compliance)) %>% 
  ungroup %>% 
  # group_by(country,combine_type) %>% 
  # arrange(country,combine_type,date_start) %>% 
  # mutate(target_country_num=cumsum(target_country_num),
  #        target_country_num=ifelse(target_country_num>216,216,target_country_num)) %>% 
  filter(!is.na(country)) %>% 
  mutate(model_id=3,
         #target_country_num=as.numeric(scale(target_country_num)),
         ordered_id=ifelse(grepl(x=combine_type,pattern="External"),4,3))

# get rid of newer records

clean_comp <- filter(clean_comp,date_start<ymd("2020-04-30"))

# get rid of days where there were no changes (will screw with the time series)

comp_days <- distinct(clean_comp,date_start,total_day) %>% 
  arrange(date_start) %>% 
  mutate(diff = total_day - dplyr::lag(total_day)) 

# do preliminary analysis

countries <- c("United States of America","Germany","Brazil","Switzerland","Israel")

# need to cycle over each time point
# oh hurray

over_time_pts <- mclapply(unique(clean_comp$date_start), function(d) {
  
  to_make <- ungroup(clean_comp) %>% 
    distinct %>% 
    filter(date_start==d) %>% 
    id_make(outcome_disc="combine_disc",
            person_id="country",
            ordered_id="ordered_id",
            item_id="combine_type",time_id="date_start")
  
  # note no missing data :)
  
  activity_fit <- id_estimate(to_make,vary_ideal_pts="none",ncores=1,nchains=1,niters=500,
                              within_chain="threads",
                              warmup=300,
                              fixtype="prefix",
                              restrict_ind_high="Quarantine/Lockdown_type_self_quarantine",
                              restrict_ind_low="Restriction of Non-Essential Businesses_type_bars",
                              id_refresh = 10,
                              const_type="items")
  
  saveRDS(activity_fit,paste0("/scratch/rmk7/sev_fit_",d,".rds"))
  
})

