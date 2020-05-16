# RA stats

require(dplyr)
require(ggplot2)
require(readr)
require(tidyr)
require(googlesheets4)
require(lubridate)
require(stringr)

start_week <- ymd("2020-05-11")
end_week <- ymd("2020-05-19")

#sheets_auth()

export <- read_csv("data/CoronaNet/RA/ra_data_pull.csv") %>% 
  slice(-c(1:2)) %>% 
  mutate(RecordedDate=lubridate::ymd_hms(RecordedDate),
         record_date_day=lubridate::as_date(RecordedDate),
         Progress=as.numeric(Progress)) %>% 
mutate(entry_type=recode(entry_type,
                         `1`="new_entry",
                         `New Entry`="new_entry",
                         `Correction to Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="correction",
                         `Update on Existing Entry (type in Record ID in text box)`="update",
                         `Update on Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="update")) %>% 
  filter(Progress>98)

# need to so some more work on this to calculate policy IDs
# and policy types

export$correct_record_match = ifelse(
  grepl(x=export$entry_type,pattern='correction'),
  trimws(export$entry_type_2_TEXT),
  export$record_id
)

export <- group_by(export, record_id) %>% mutate(
  policy_id=case_when(entry_type=="update" & !is.na(entry_type_3_TEXT)~entry_type_3_TEXT,
                      entry_type=="correction" & !is.na(entry_type_2_TEXT)~entry_type_2_TEXT,
                      TRUE~record_id)) %>% 
  ungroup %>% 
  select(-record_id) %>% 
  select(record_id="ResponseId",everything())

miss_vars <- names(export)[sapply(export, function(c) any(is.na(c)))]

miss_vars <- miss_vars[miss_vars!="correct_record_match"]

export <- group_by(export,policy_id) %>% 
  arrange(policy_id,StartDate) %>% 
  fill(all_of(miss_vars),.direction=c("down"))%>% 
  ungroup()

  
  names(export)[which(names(export) %in% c("type_quarantine_days", "type_mass_gathering"))] = c('type_num_quarantine_days', 'type_num_mass_gathering')
names(export)[names(export) %in% c("type_health_resource_1_TEXT",
                                         "type_health_resource_2_TEXT",
                                         "type_health_resource_3_TEXT",
                                         "type_health_resource_18_TEXT",
                                         "type_health_resource_19_TEXT",
                                         "type_health_resource_21_TEXT",
                                         "type_health_resource_13_TEXT",
                                         "type_health_resource_4_TEXT",
                                         "type_health_resource_5_TEXT",
                                         "type_health_resource_6_TEXT",
                                         "type_health_resource_7_TEXT",
                                         "type_health_resource_8_TEXT",
                                         "type_health_resource_15_TEXT",
                                         "type_health_resource_9_TEXT",
                                         "type_health_resource_10_TEXT",
                                         "type_health_resource_11_TEXT",
                                         "type_health_resource_17_TEXT",
                                         "type_health_resource_20_TEXT")] = c(
                                           'type_num_masks',
                                           'type_num_ventilators',
                                           'type_num_ppe',
                                           'type_num_hand_sanit',
                                           'type_num_test_kits',
                                           'type_num_vaccines',
                                           'type_other_health_materials',
                                           'type_num_hospitals',
                                           'type_num_quaranCen',
                                           'type_num_medCen',
                                           'type_num_pubTest',
                                           'type_num_research',
                                           'type_other_health_infra',
                                           'type_num_doctors',
                                           'type_num_nurses',
                                           'type_num_volunt',
                                           'type_other_health_staff',
                                           'type_num_health_insurance'
                                         )


# recode provinces

country_regions = read_csv(file = 'data/regions/country_region_clean.csv')
regions_df = read_csv(file = 'data/regions/country_regional_groups_concordance.csv')

province_out <- select(ungroup(export), matches("init\\_prov")) %>% 
  as.matrix %>% 
  apply(1,function(c) {
    if(all(is.na(c))) {
      return(NA) 
      } else {
        return(str_extract(c[!is.na(c)],"[0-9]+" ))
      }
    })

export$index_prov <- paste0(export$init_country,province_out)

country_regions_long = country_regions %>%
  gather(key="p_num",value="init_2prov",-Country,-ISO2) %>% 
  mutate(p_num = as.numeric(p_num)+1) %>%
  mutate(index_prov=paste0(Country,p_num)) %>%
  filter(!is.na(init_2prov)) %>%
  select(-p_num)

# match province code to actual province name


export <- left_join(export,country_regions_long,by="index_prov") %>% 
  select(-matches("init\\_prov|province\\_coop"),
         init_prov="init_2prov")

export <- select(export,ra_name,
         recorded_date="RecordedDate",
         record_id,policy_id,entry_type,event_description,country="init_country",
         date_announced,
         date_start,
         date_end,
         init_country_level,
         province="init_prov",
         city="init_city",
         target_country="target_country",
         target_geog_level,
         target_region,
         target_province,
         target_city,
         target_other,
         target_who_what,
         target_direction,
         travel_mechanism,
         compliance,
         enforcer,
         matches("type"),
         link="sources_matrix_1_2") %>% 
  select(-matches("TEXT")) %>% 
  arrange(country,policy_id,date_start)

# need to get rid of/change corrections

export <- mutate(export,corrected=(entry_type=="correction")) %>% 
  group_by(policy_id) %>% 
  arrange(policy_id,recorded_date) %>% 
  mutate(entry_type=case_when(entry_type=="correction" & recorded_date==min(recorded_date)~"new entry",
                              TRUE~entry_type))

export %>% 
  filter(entry_type!="correction") %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  group_by(record_date_day) %>% 
  count %>% 
  ggplot(aes(y=n,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Updates/New Entry Records\nin CoronaNet Database Since March 26th")

ggsave("date_performance.png",width = 6,height=3)

export %>% 
  filter(entry_type!="correction)") %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  group_by(record_date_day) %>% 
  count %>% 
  ungroup %>% 
  arrange(record_date_day) %>% 
  mutate(n_cum=cumsum(n)) %>% 
  ggplot(aes(y=n_cum,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Updates/New Entry Records\nin CoronaNet Database Since March 26th")

ggsave("date_performance_cumsum.png",width = 6,height=3)

# country coverage by days

export %>% 
  filter(entry_type!="correction") %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  group_by(country,record_date_day) %>% 
  mutate(n_exists=ifelse(n()>1,1,NA)) %>% 
  ungroup %>% 
  complete(country,record_date_day,fill=list(n_exists=NA)) %>% 
  group_by(country) %>% 
  arrange(country,record_date_day) %>% 
  fill(n_exists,.direction="down") %>% 
  mutate(n_exists=coalesce(n_exists,0)) %>% 
  distinct(record_date_day,country,n_exists) %>% 
  group_by(record_date_day) %>% 
  summarize(n=sum(n_exists)) %>% 
  ggplot(aes(y=n,x=record_date_day)) +
  geom_area(fill="blue",alpha=0.5) +
  geom_hline(yintercept=219,linetype=2) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Countries") +
  xlab("") +
  ggtitle("Number of Countries Covered\nin CoronaNet Database Since March 26th")

ggsave("country_cov.png",width = 6,height=3)

# house competition info

hogwarts <- sheets_get("https://docs.google.com/spreadsheets/d/1nhPGi7GD6RwsI2pZ5SOCRHg4mICqc5nByWZl7UgDjys/edit?usp=sharing") %>% 
  sheets_read(sheet="Sheet1")

# need to produce leader board

leaders <- export %>% 
  group_by(ra_name) %>% 
  filter(entry_type!="correction",
         recorded_date>start_week,
         recorded_date<end_week) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  select(Name="ra_name",`Count of Records`="n") 
  
leaders <- leaders %>% left_join(hogwarts,by=c("Name"="ra_name")) 

leaders %>% write_sheet(ss=sheets_get("https://docs.google.com/spreadsheets/d/1INmpDvIne76qqmlucMABaJxeaZ-xpoypc0s_S0tUyto/edit?usp=sharing"),
                                                                        sheet="Leaderboard")

# add up point totals from leader board

lead_pts <- slice(ungroup(leaders),c(1:10)) %>% 
  mutate(points=c(50,25,15,rep(10,7))) %>% 
  group_by(house) %>% 
  summarize(lead_pts=sum(points,na.rm=T))

# see how many we can match

hogwarts <- left_join(hogwarts,export)

# records by day by house

hogwarts %>% 
  ungroup %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  filter(entry_type!="correction",
         recorded_date>start_week,
         recorded_date<end_week,
         !is.na(house)) %>% 
  group_by(record_date_day,house) %>% 
  count %>% 
  ungroup %>% 
  group_by(house) %>% 
  arrange(record_date_day) %>% 
  mutate(n_cum=cumsum(n)) %>% 
  ggplot(aes(y=n_cum,x=record_date_day)) +
  geom_line(aes(colour=house)) +
  theme_minimal() +
  theme(panel.grid=element_blank()) +
  ylab("Count of Records (Excluding Corrections)") +
  xlab("") +
  ggtitle("Number of Records by Hogwarts House")

ggsave('hogwarts.png')

# calculate overall standings

standings <- group_by(hogwarts,house) %>% 
  filter(entry_type!="correction",
         recorded_date>start_week,
         recorded_date<end_week,
         !is.na(house)) %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  summarize(count_policies=length(unique(record_id)))

# addin point bonus

standings <- left_join(standings,lead_pts,by="house") %>% 
  mutate(Total=count_policies+lead_pts)

standings %>% 
  select(House="house",
         `Count of Policies`="count_policies",
         `Player Bonus`="lead_pts",
         Total) %>% 
  write_sheet(ss=sheets_get("https://docs.google.com/spreadsheets/d/1INmpDvIne76qqmlucMABaJxeaZ-xpoypc0s_S0tUyto/edit?usp=sharing"),
              sheet="Overall Standings")


# need 

# output to record sheet

current_sheet <- sheets_get("https://docs.google.com/spreadsheets/d/183lWnJH7rSVkOTiuCXt9D7uCwS1SdJSOleXPpFkj2us/edit#gid=0")

export %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  select(-record_date_day) %>% 
  ungroup %>% 
  mutate(link_correct=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",record_id,
                            "&Q_R_DEL=1&record_id=",policy_id,"&link_type=C"),
         link_update=ifelse(entry_type=="new_entry",paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",record_id,
                                                           "&record_id=",policy_id,"&link_type=U"),"")) %>% 
  select(link_correct,link_update,everything()) %>% 
  arrange(country,policy_id,date_announced) %>% 
  sheets_write(ss=current_sheet,sheet="Sheet1")

# save and read out

export %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  ungroup %>% 
  select(-record_date_day) %>% 
  mutate(link_correct=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",record_id,
                             "&Q_R_DEL=1&record_id=",policy_id,"&link_type=C"),
         link_update=ifelse(entry_type=="new_entry",paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",record_id,
                            "&record_id=",policy_id,"&link_type=U"),"")) %>% 
  select(link_correct,link_update,everything()) %>% 
  arrange(country,date_announced) %>% 
  write_csv("data/CoronaNet/RA/ra_data_pull_purified.csv")

# upload to postgres

con <- dbConnect("PostgreSQL",user="tariff",password="6235$$Wa",port=5432,
                 dbname="master",
                 host="niehaususer.ccecwurg6k9l.us-east-2.rds.amazonaws.com")

export %>% 
  mutate(record_date_day=lubridate::as_date(recorded_date)) %>% 
  ungroup %>% 
  select(-record_date_day) %>% 
  mutate(link_correct=paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",record_id,
                             "&Q_R_DEL=1&record_id=",policy_id,"&link_type=C"),
         link_update=ifelse(entry_type=="new_entry",paste0("https://tummgmt.eu.qualtrics.com/jfe/form/SV_bf6YMWbTpYJAW4l?Q_R=",record_id,
                                                           "&record_id=",policy_id,"&link_type=U"),"")) %>% 
  select(link_correct,link_update,everything()) %>% 
  arrange(country,policy_id,date_announced) %>% 
  dbWriteTable(con,"cordata",value=.,append=F,overwrite=T)


