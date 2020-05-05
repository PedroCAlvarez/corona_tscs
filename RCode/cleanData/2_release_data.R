# Use this code to generate data for the visualization
# First run cleanQualtrics_short.R
# Bob Kubinec

require(dplyr)
require(readr)
require(readxl)
require(stringr)
require(idealstan)
require(lubridate)
require(tidyr)
require(missRanger)
require(qualtRics)

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
'%!in%' <- function(x,y)!('%in%'(x,y))

# update all githubs

system2("git",args=c("-C ~/covid-tracking-data","pull"))
system2("git",args=c("-C ~/covid-19-data","pull"))
system2("git",args=c("-C ~/covid19_tests","pull"))
system2("git",args=c("-C ~/COVID-19","pull"))

# covid test data

covid_test <- read_csv("~/covid19_tests/data_snapshots/covid_tests_last.csv") %>% 
  select(ISO3,Date,tests_raw="Tests_raw",
         test_source="Source",
         test_notes="Notes",
         tests_date="Date_scraped",
         tests_daily_or_total="Daily_or_total") %>% 
  group_by(Date,ISO3,tests_daily_or_total) %>% 
  summarize(tests_raw=mean(tests_raw,na.rm=T))

# cases/deaths

cases <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  select(-Lat, -Long,country="Country/Region") %>% 
  gather(key="date_start",value="confirmed_cases",-`Province/State`,-country) %>% 
  mutate(date_start=mdy(date_start),
         country=recode(country,Czechia="Czech Republic",
                        `Hong Kong`="China",
                        `US`="United States of America",
                        `Taiwan*`="Taiwan",
                        `Bahamas`="The Bahamas",
                        `Tanzania`="United Republic of Tanzania",
                        `North Macedonia`="Macedonia",
                        `Micronesia`="Federated States of Micronesia",
                        `Burma`="Myanmar",
                        `Tanzania`="United Republic of Tanzania",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Korea, South`="South Korea",
                        `Timor-Leste`="East Timor",
                        `Congo (Brazzaville)`="Republic of Congo",
                        `Congo (Kinshasa)`="Democratic Republic of the Congo",
                        `Cabo Verde`="Cape Verde",
                        `West Bank and Gaza`="Palestine",
                        `Eswatini`="Swaziland")) %>% 
  group_by(date_start,country) %>% 
  summarize(confirmed_cases=sum(confirmed_cases,na.rm=T))
deaths <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
  select(-Lat, -Long,country="Country/Region") %>% 
  gather(key="date_start",value="deaths",-`Province/State`,-country) %>% 
  mutate(date_start=mdy(date_start),
         country=recode(country,Czechia="Czech Republic",
                        `Hong Kong`="China",
                        `US`="United States of America",
                        `Taiwan*`="Taiwan",
                        `Bahamas`="The Bahamas",
                        `Tanzania`="United Republic of Tanzania",
                        `North Macedonia`="Macedonia",
                        `Micronesia`="Federated States of Micronesia",
                        `Burma`="Myanmar",
                        `Tanzania`="United Republic of Tanzania",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Korea, South`="South Korea",
                        `Timor-Leste`="East Timor",
                        `Congo (Brazzaville)`="Republic of Congo",
                        `Congo (Kinshasa)`="Democratic Republic of the Congo",
                        `Cabo Verde`="Cape Verde",
                        `West Bank and Gaza`="Palestine",
                        `Eswatini`="Swaziland")) %>% 
  group_by(date_start,country) %>% 
  summarize(deaths=sum(deaths,na.rm=T))
recovered <- read_csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
  select(-Lat, -Long,country="Country/Region") %>% 
  gather(key="date_start",value="recovered",-`Province/State`,-country) %>% 
  mutate(date_start=mdy(date_start),
         country=recode(country,Czechia="Czech Republic",
                        `Hong Kong`="China",
                        `US`="United States of America",
                        `Taiwan*`="Taiwan",
                        `Bahamas`="The Bahamas",
                        `Tanzania`="United Republic of Tanzania",
                        `North Macedonia`="Macedonia",
                        `Micronesia`="Federated States of Micronesia",
                        `Burma`="Myanmar",
                        `Tanzania`="United Republic of Tanzania",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Korea, South`="South Korea",
                        `Timor-Leste`="East Timor",
                        `Congo (Brazzaville)`="Republic of Congo",
                        `Congo (Kinshasa)`="Democratic Republic of the Congo",
                        `Cabo Verde`="Cape Verde",
                        `West Bank and Gaza`="Palestine",
                        `Eswatini`="Swaziland")) %>% 
  group_by(date_start,country) %>% 
  summarize(recovered=sum(recovered,na.rm=T))

# niehaus data

niehaus <- read_csv("data/data_niehaus/03_21_20_0105am_wep.csv") %>% 
  group_by(country) %>% 
  fill(pop_WDI_PW:EmigrantStock_EMS,.direction="down") %>% 
  slice(n()) %>% 
  filter(!is.na(ifs)) %>% 
  select(country,ccode,ifs,pop_WDI_PW:EmigrantStock_EMS)


# load cleaned data
 
clean_data <- readRDS("data/CoronaNet/coranaNetData_clean_wide.rds")

# activity index

get_est <- readRDS("data/get_est.rds")


get_est_sum <- get_est %>%
  ungroup %>%
  mutate(estimate=(estimate-min(estimate))/(max(estimate)-min(estimate))*100,
         date_start=ymd(as.character(date_announced))) %>%
  group_by(country,date_start) %>%
  summarize(index_med_est=median(estimate),
            index_high_est=quantile(estimate,.95),
            index_low_est=quantile(estimate,.05)) %>%
  group_by(date_start) %>%
  mutate(index_country_rank=rank(index_med_est))

# select only columns we need

release <- filter(clean_data,!is.na(init_country),is.na(init_other),is.na(target_other) | target_other=="",
                  validation) %>% 
              select(record_id,policy_id,entry_type,event_description,country="init_country",
                     date_announced,
                     date_start,
                     date_end,
                     init_country_level,
                     domestic_policy,
                     province="init_prov",
                     city="init_city",
                     matches("type"),
                     target_country="target_country",
                     target_geog_level,
                     target_region,
                     target_province,
                     target_city,
                     target_other,
                     target_who_what,
                     recorded_date="RecordedDate",
                     target_direction,
                     travel_mechanism,
                     compliance,
                     enforcer,
                     link="sources_matrix_1_2") %>% 
  select(-matches("TEXT"))

# move visa restrictions from travel_mechanism to type_ext_restrict 
release$type_ext_restrict = ifelse(grepl("Visa restrictions", release$travel_mechanism), 
                                   ifelse(is.na(release$type_ext_restrict),  'Visa restrictions (e.g. suspend issuance of visa)', paste(release$type_ext_restrict, 'Visa restrictions (e.g. suspend issuance of visa)', sep = ',')),release$type_ext_restrict )
 
# make distinct 'other' category for each relevant broad policy type
release$type_quarantine = gsub('Other', 'Other Quarantine', release$type_quarantine)
release$type_ext_restrict = gsub('Other', 'Other External Border Restriction', release$type_ext_restrict)
release$type_soc_distance = gsub('Other', 'Other Mask Wearing Policy', release$type_soc_distance)

# make distinct 'unspecifed' category
release$type_soc_distance = gsub('Unspecified', 'Unspecified Mask Wearing Policy', release$type_soc_distance)

# iterate and capture unique vars
type_vars <- names(release)[grepl(x=names(release),
                                  pattern="type\\_")]

unique_vars <- do.call(rbind, lapply(type_vars, function(c) {
  tibble(type_var=c,
         vals=unique(release[[c]]))
}))
 

# separate out free text entry
free_text <- filter(unique_vars,grepl(x=type_var,pattern="other|num"),!is.na(vals))
cats <- filter(unique_vars,!grepl(x=type_var,pattern="other|num"),!is.na(vals)) %>% 
  mutate(vals=str_replace(vals,
                     "Personal Protective Equipment \\(e\\.g\\. gowns, goggles\\)",
                     "Personal Protective Equipment")) %>% 
  mutate(vals=str_replace(vals,
                          "Cancellation of an annually recurring event \\(e\\.g\\. election, national festival\\)",
                          "Cancellation of an annually recurring event")) %>% 
  mutate(vals=str_replace(vals,
                          "Postponement of an annually recurring event \\(e\\.g\\. election, national festival\\)",
                          "Postponement of an annually recurring event")) %>% 
  mutate(vals=str_replace(vals,
                          "Cancellation of a recreational or commercial event \\(e\\.g\\. sports game, music concert\\)",
                          "Cancellation of a recreational or commercial event"))%>% 
  mutate(vals=str_replace(vals,
                          "Postponement of a recreational or commercial event \\(e\\.g\\. sports game, music concert\\)",
                          "Postponement of a recreational or commercial event"))%>% 
  mutate(vals=str_replace(vals,
                          "Commercial areas \\(e\\.g\\. shopping malls,markets\\)",
                          "Hygiene measures for commercial areas"))%>% 
  mutate(vals=str_replace(vals,
                          "Public areas \\(e\\.g\\. mosques, government buildings, schools\\)",
                          "Hygiene measures for public areas"))%>% 
  mutate(vals=str_replace(vals,
                          "Public Transport \\(e\\.g\\. subways,trains\\)",
                          "Hygiene measures for public transport"))



cats <- lapply(1:nrow(cats), function(i) {
  this_data <- slice(cats,i)
  tibble(orig_val=this_data$vals,
         type_var=this_data$type_var,
         vals=(str_split(this_data$vals,pattern=",")[[1]]))
}) %>% bind_rows

 

# assign unique IDs

cats <- mutate(cats,
               vals_id=as.numeric(factor(vals)))

all_let <- expand.grid(letters,LETTERS) %>% 
  mutate(new_id=paste0(as.character(Var2),
                       as.character(Var1)),
         vals_id=1:n())

# merge back in to cats

cats <- left_join(cats,select(all_let,new_id,vals_id),
                  by="vals_id") %>% 
  distinct

 


# now merge back in to regular data 
release = release %>% 
  mutate(type_health_resource=str_replace(type_health_resource,
                          "Personal Protective Equipment \\(e\\.g\\. gowns, goggles\\)",
                          "Personal Protective Equipment")) %>% 
  mutate(type_mass_category=str_replace(type_mass_category,
                          "Cancellation of an annually recurring event \\(e\\.g\\. election, national festival\\)",
                          "Cancellation of an annually recurring event")) %>% 
  mutate(type_mass_category=str_replace(type_mass_category,
                          "Postponement of an annually recurring event \\(e\\.g\\. election, national festival\\)",
                          "Postponement of an annually recurring event")) %>% 
  mutate(type_mass_category=str_replace(type_mass_category,
                          "Cancellation of a recreational or commercial event \\(e\\.g\\. sports game, music concert\\)",
                          "Cancellation of a recreational or commercial event"))%>% 
  mutate(type_mass_category=str_replace(type_mass_category,
                          "Postponement of a recreational or commercial event \\(e\\.g\\. sports game, music concert\\)",
                          "Postponement of a recreational or commercial event"))%>% 
  mutate(type_hygiene=str_replace(type_hygiene,
                          "Commercial areas \\(e\\.g\\. shopping malls,markets\\)",
                          "Hygiene measures for commercial areas"))%>% 
  mutate(type_hygiene=str_replace(type_hygiene,
                          "Public areas \\(e\\.g\\. mosques, government buildings, schools\\)",
                          "Hygiene measures for public areas"))%>% 
  mutate(type_hygiene=str_replace(type_hygiene,
                          "Public Transport \\(e\\.g\\. subways,trains\\)",
                          "Hygiene measures for public transport"))

release_long <- gather(release,key="discard",value="type_text",
                       unique(free_text$type_var)) %>% 
  gather(key="extra",value="type_sub_cat",unique(cats$type_var))

# merge in new IDs

release_long <- left_join(release_long,select(cats,-vals_id),by=c(extra="type_var",
                                                 "type_sub_cat"="orig_val"))


# merge back down

release_long <- distinct(release_long,record_id,policy_id,new_id,.keep_all = T) %>% 
  ungroup %>% 
  mutate(record_id=paste0(record_id,new_id)) %>% 
  select(-new_id,-discard,-extra,-type_sub_cat) %>% 
  select(everything(),type_sub_cat="vals") %>% 
  mutate(type_sub_cat=na_if(type_sub_cat,"None of the above"))

release_long <- release_long %>% 
  mutate(province=ifelse(country=="Hong Kong","Hong Kong",province),
         province=ifelse(country=="Macau","Macau",province),
         init_country_level=recode(init_country_level,`No, it is at the national level`="National",
                                   `Yes, it is at the city/municipal level`="Municipal",
                                   `Yes, it is at the city/municipal level`="Provincial"),
         date_announced=lubridate::mdy(date_announced),
         date_start=lubridate::mdy(date_start),
         date_end=lubridate::mdy(date_end),
         entry_type=recode(entry_type,
                           `Correction to Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="correction",
                           `Update on Existing Entry (type in Record ID in text box)`="update",
                           `Update on Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="update",
                           `New Entry`="new_entry")) %>% 
  filter(!is.na(date_start),
         recorded_date<(today()-days(5))) %>% 
  mutate(type_sub_cat=ifelse(type_sub_cat=="None of the above",NA,type_sub_cat))


## records that have a type_sub_cat are still 'duplicated'
# e.g. if a policy sub type is 'Health screenings (e.g. temperature checks)" it has
# two record_id: '[record_id]Ag' and '[record_id]NA'
# remove the second one ('[record_id]NA')
release_long = release_long %>% 
  group_by(policy_id) %>%
  filter(if (any(!is.na(type_sub_cat)))
    !is.na(type_sub_cat)
    else is.na(type_sub_cat)) %>%
  ungroup()


# recode records
release_long$country <- recode(release_long$country,Czechia="Czech Republic",
                            `Hong Kong`="China",
                             Macau="China",
                            `United States`="United States of America",
                            `Bahamas`="The Bahamas",
                            `Tanzania`="United Republic of Tanzania",
                            `North Macedonia`="Macedonia",
                            `Micronesia`="Federated States of Micronesia",
                            `Timor Leste`="East Timor",
                            `Republic of the Congo`="Republic of Congo",
                            `Cabo Verde`="Cape Verde",
                            `Eswatini`="Swaziland",
                            `Serbia`="Republic of Serbia",
                            `Guinea-Bissau`="Guinea Bissau")

release_long$target_country <- recode(release_long$target_country,Czechia="Czech Republic",
                                  `Hong Kong`="China",
                                  `Serbia`="Republic of Serbia",
                                  `United States`="United States of America",
                                  `Bahamas`="The Bahamas",
                                  `Tanzania`="United Republic of Tanzania",
                                  `North Macedonia`="Macedonia",
                                  `Micronesia`="Federated States of Micronesia",
                                  `Timor Leste`="East Timor",
                                  `Republic of the Congo`="Republic of Congo",
                                  `Cabo Verde`="Cape Verde",
                                  `Eswatini`="Swaziland",
                                  `Guinea-Bissau`="Guinea Bissau")

release_long <- mutate(release_long,init_country_level=ifelse(province %in% c("Hong Kong","Macau"),"No, it is at the national level",
                                                    init_country_level))

# country names

country_names <- read_xlsx("data/ISO WORLD COUNTRIES.xlsx",sheet = "ISO-names")

# try a simple join

release_long <- left_join(release_long,country_names,by=c("country"="ADMIN"))

missing <- filter(release_long,is.na(ISO_A2))

# we will get a warning because of the European Union

if(nrow(missing)>0 && !(all(missing$country=="European Union"))) {
  
  warning("Country doesn't match ISO data.")
  
}


# remove illogical type_sub_cat - type combinations
# note these combinations are technically possible because RAs are given the 'backtrack' option in the Qualtrics survey

release_long = release_long %>%
mutate_cond( type %in% c('Declaration of Emergency',
              'Internal Border Restrictions',
              'Curfew',
              'Health Testing',
              'Health Monitoring',
              'Other Policy Not Listed Above') & !is.na(type_sub_cat),
              type_sub_cat = NA) %>%
mutate_cond( type == "Quarantine/Lockdown" &
              type_sub_cat %!in% c("Self-Quarantine (i.e. quarantine at home)",
              "Government Quarantine (i.e. quarantine at a government hotel or facility)",
              "Quarantine outside the home or government facility (i.e. quarantine in a hotel)",
              "Quarantine only applies to people of certain ages. Please note the age restrictions in the text box.",
              "Other Quarantine"),
              type_sub_cat = NA) %>%
mutate_cond( type == "External Border Restrictions" &
            type_sub_cat %!in% c("Health Screenings (e.g. temperature checks)" ,
            "Health Certificates",
            "Travel History Form (e.g. documents where traveler has recently been)",
            "Visa restrictions (e.g. suspend issuance of visa)",
            "Visa extensions (e.g. visa validity extended)",
            "Other External Border Restriction"  ),
type_sub_cat = NA) %>%
mutate_cond( type == "Restrictions of Mass Gatherings"  &
            type_sub_cat %!in% c("Cancellation of an annually recurring event" ,
            "Postponement of an annually recurring event",
            "Cancellation of a recreational or commercial event",
            "Postponement of a recreational or commercial event",
            "Attendance at religious services prohibited (e.g. mosque/church closings)",
            "Prison population reduced (e.g. early release of prisoners)"),
            type_sub_cat = NA) %>%
mutate_cond( type == "Social Distancing" & 
             type_sub_cat %!in% c("All public spaces / everywhere",
             "Inside public or commercial building (e.g. supermarkets)",
              "Unspecified Mask Wearing Policy",
              "Other Mask Wearing Policy",
              "Wearing masks"),
              type_sub_cat = NA) %>%
mutate_cond(type =="Closure of Schools"  &
            type_sub_cat %!in% c("Preschool or childcare facilities (generally for children ages 5 and below)" ,
            "Primary Schools (generally for children ages 10 and below)"  ,
            "Secondary Schools (generally for children ages 10 to 18)"  ,
            "Higher education (i.e. degree granting institutions)" ),
            type_sub_cat = NA)  %>%  
mutate_cond(type == "Restriction of Non-Essential Government Services"  &
            type_sub_cat %!in% c("Restricted issuing of permits/certificates and/or processing of government documents"  ,
            "Beaches"  ,
            "Campsites",
            "Parks",
            "Tourist Sites",
            "Unspecified outdoor spaces",
            "Other public outdoor spaces",
            'Public libraries',
            'Public museums/galleries',
            'Unspecified public facilities',
            'Other public facilities',
            'Restricted government working hours (e.g. work from home policies for government workers)',
            'All non-essential government services restricted'),
            type_sub_cat = NA) %>%
mutate_cond(type =="Restriction of Non-Essential Businesses" &
            type_sub_cat %!in% c("Retail Businesses",
            "Restaurants/Bars" ,
            "Shopping Centers" ,
            "Non-Essential Commercial Businesses" ,
            "Personal Grooming Businesses (e.g. hair salons)",
            "Other Businesses",
            "All or unspecified non-essential businesses"),
            type_sub_cat = NA)  %>%  
mutate_cond(type =="Health Resources" &
            type_sub_cat %!in% c("Masks",
            "Ventilators",
            "Personal Protective Equipment",
            "Hand Sanitizer" ,
            "Test Kits" ,
            "Vaccines",
            "Unspecified Health Materials",
            "Other Health Materials",
            "Hospitals",
            "Temporary Quarantine Centers",
            "Temporary Medical Centers",
            "Public Testing Facilities (e.g. drive-in testing for COVID-19)" ,
            "Health Research Facilities",
            "Unspecified Health Infrastructure",
            "Other Health Infrastructure",
            "Doctors" ,
            "Nurses" ,
            "Health Volunteers" ,
            "Unspecified Health Staff" ,
            "Other Heath Staff",
            "Health Insurance"),
            type_sub_cat = NA) %>%  
mutate_cond( type =="Hygiene" &
             type_sub_cat %!in% c("Hygiene measures for commercial areas"  ,
             "Hygiene measures for public areas"   ,
             "Hygiene measures for public transport"  ,
             "Burial procedures",
             "Other Areas Hygiene Measures Applied" ),
             type_sub_cat = NA)  %>%
mutate_cond( type == "Public Awareness Measures"  &
            type_sub_cat %!in% c("Disseminating information related to COVID-19 to the public that is reliable and factually accurate"  ,
            "Gathering information related to COVID-19 from the public"  ,
            "Both Disseminating and Gathering information related to COVID-19"),
            type_sub_cat = NA) %>%
mutate_cond(type == "New Task Force, Bureau or Administrative Configuration"  &
            type_sub_cat %!in% c("New Task Force or Bureau (i.e. establishment of a temporary body)"  ,
            "Existing government entity given new powers"  ,
            "Cooperation among different jurisdictional entities (e.g. treaties or agreements among countries or provinces)",
            "Other Administrative Configurations"),
            type_sub_cat = NA)

# add in update information

update_orig <- qualtRics::read_survey("data/CoronaNet/RA/ra_update_first.csv")
update_current <- read_survey("data/CoronaNet/RA/ra_update.csv")

# need to iterate over these and create new columns for update records

split_orig <- str_split(update_orig$policy_update,",",simplify = T)

all_orig_un <- unique(c(split_orig))
all_orig_un <- all_orig_un[!is.na(all_orig_un) & all_orig_un!=""]
all_orig_un <- data_frame(type=all_orig_un)

split_data_orig <- apply(split_orig,1,function(c) {
  left_join(all_orig_un,data_frame(type=c,present=1),by="type") %>% 
    spread(key="type",value="present",fill=0)
}) %>% bind_rows

split_data_orig$country <- update_orig$assign_country
split_data_orig$date_updated <- ymd("2020-04-10")

split_curr <- str_split(update_current$country,",",simplify = T)

all_curr_cu <- unique(c(split_curr))
all_curr_cu <- all_curr_cu[!is.na(all_curr_cu) & all_curr_cu!=""]
all_curr_cu <- data_frame(type=all_curr_cu)

split_data_curr <- apply(split_curr,1,function(c) {
  left_join(all_curr_cu,data_frame(type=c,present=1),by="type") %>% 
    spread(key="type",value="present",fill=0)
}) %>% bind_rows

split_data_curr$country <- update_current$init_country
split_data_curr$date_updated <- as_date(ymd_hms(update_current$EndDate))

# separate rows

split_data_orig <- separate_rows(split_data_orig,country,sep=",") %>% 
  filter(!is.na(country))
split_data_curr <- separate_rows(split_data_curr,country,sep=",") %>% 
  filter(!is.na(country))

combine_update <- bind_rows(list(orig_update=split_data_orig,
                                 curr_update=split_data_curr),
                            .id="update_type")

# should be easy to collapse data now

combine_update <- gather(combine_update,key="type",value="updated",-country,-date_updated) %>% 
  group_by(type,country) %>% 
  summarize(updated=any(updated==1,na.rm = T),
            date_updated=max(date_updated,na.rm=T)) %>% 
  mutate(date_updated=ifelse(updated,as.character(date_updated),NA_character_))

# recode country

combine_update$country <- recode(combine_update$country,Czechia="Czech Republic",
                               `Hong Kong`="China",
                               Macau="China",
                               `United States`="United States of America",
                               `Bahamas`="The Bahamas",
                               `Tanzania`="United Republic of Tanzania",
                               `North Macedonia`="Macedonia",
                               `Micronesia`="Federated States of Micronesia",
                               `Timor Leste`="East Timor",
                               `Republic of the Congo`="Republic of Congo",
                               `Cabo Verde`="Cape Verde",
                               `Eswatini`="Swaziland",
                               `Serbia`="Republic of Serbia",
                               `Guinea-Bissau`="Guinea Bissau")

# merge updated variable

release_long <- left_join(release_long,distinct(select(combine_update,country,date_updated,
                                                       type)),
                          by=c("type","country"))

# now we can fill in missing values, if they exist, with recorded dates

release_long$date_updated <- ifelse(is.na(release_long$date_updated),as.character(as_date(release_long$recorded_date)),
                                    release_long$date_updated)

# we can also update if record is newer

release_long$date_updated <- ifelse(ymd(release_long$date_updated)<release_long$recorded_date,
                                    as.character(as_date(release_long$recorded_date)),
                                   release_long$date_updated)

release_long$date_updated <- ymd(release_long$date_updated)

# need to collapse data due to possible duplicate records


# Add in activity index

release_long <- left_join(release_long,get_est_sum,by=c("country","date_start"))

release_long <- select(release_long,record_id,policy_id,recorded_date,date_updated,date_announced,date_start,date_end,
                  entry_type,event_description,domestic_policy,type,type_sub_cat,type_text,
                  index_high_est,
                  index_med_est,
                  index_low_est,
                  index_country_rank,
                  everything()) %>% 
  select(-link_type)

# now output raw data for sharing

#write_csv(release_long,"../CoronaNet/data/coronanet_release_long.csv")
write_csv(release_long,"data/CoronaNet/coronanet_release.csv")

# merge with other files

release_long_combined <- left_join(cases,deaths, by=c("country","date_start")) %>% 
  left_join(recovered,by=c("country","date_start")) %>% 
  full_join(release_long,by=c("country","date_start")) %>% 
  left_join(covid_test,by=c(ISO_A3="ISO3",
                                                      date_start="Date")) %>% 
  left_join(niehaus,by=c("country"))

write_csv(release_long_combined,"data/CoronaNet/coronanet_release_allvars.csv")

# let's impute the buggers

# release_long_combined <- group_by(release_long_combined,country) %>% 
#   mutate(miss_test=all(is.na(tests_raw)))
# 
# imputed_release_long <- lapply(c(7583,
#                             1999332,
#                             747352,
#                             99226,
#                             1884630,
#                             19945817,
#                             856397,
#                             885773,
#                             1994005,
#                             8847736),function(i) {
#   missRanger(ungroup(release_long_combined),formula= FarRight_IO + 
#                ExternalLaborOpenness_IO + eco_glob_KOF + 
#                soc_glob_KOF + 
#                cult_prox_KOF +
#                poli_glob_KOF +
#                overallGlob_index_KOF +
#                news_WB +
#                disap_FA +
#                polpris_FA +
#                latentmean_FA +
#                transparencyindex_HR +
#                state_IDC +
#                muni_IDC +
#                dispersive_IDC +
#                constraining_IDC +
#                inclusive_IDC +
#                Rank_FP +
#                Score_FP +
#                sfi_SFI +
#                ti_cpi_TI +
#                v2x_polyarchy_VDEM +
#                EmigrantStock_EMS ~.-c(record_id,policy_id,miss_test),
#              pmm.k=10,
#              seed=i) %>% 
#     mutate(imputation_seed=i)
# })
# 
# imputed_release_long <- lapply(imputed_release_long, mutate,tests_daily_or_total=ifelse(miss_test,NA,
#                                                                               tests_daily_or_total),
#                           tests_raw=ifelse(miss_test,NA,
#                                            tests_raw))


#write_csv(release_combined,"../CoronaNet/data/coronanet_release_allvars.csv")

# copy raw data over

#system("cp data/CoronaNet/coranaNetData_clean.rds ../CoronaNet/data/coranaNetData_clean.rds")
