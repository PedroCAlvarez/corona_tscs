# This code transforms the raw survey qualtrics data
# for the CoronaNet Project into a long format
# the output for this code is currently saved under 'coranaNetData_clean.rds' in the Data/CoronaNet folder of the Dropbox

## the code should transform the data such that:
# for the initiating policy actor, there is:
# one column for the country: init_country
# one column for the initiating province, where applicable: init_province
# one column for the initiating city, where applicable: int_city
# one column for the initiating other body, where applicable: init_other_level
# for the geographical target of the policy actors, there is:
# one column for the targeted country/regional grouping  (see note below in line 336 about breaking out the countries in regional groupings): target_country_region
# one column for the targeted province, where applicable: target_province
# one column for the targeted city, where applicable: target_city
# for the type of policy, there is:
# one column for the broad type of policy: type
# one column for the sub-category of a policy: type_sub_cat
# one column for the 'other' text entries of a sub-category of a policy: type_sub_cat_other
# one column for the number of a policy (e.g. how many masks) where applicable: type_sub_num
# one column for other broad policy types: type_other
# there is one column for the who/what target of a policy: target_who_what
# there is one column for the travel mechanism of a policy: travel_mechanism
# there is one column for the compliance of a policy: compliance
# there is one column for the enforcer of a policy: enforcer
# for the timing of a policy there is
# one column for date announced: date_announced
# one column for date policy implemented: date_start
# one column for date when policy ends, if found: date_end

# note that the 'other columns' where they exist, correspond to the above, with a TEXT at the end, unless otherwise specified above
# Data Recoding File
# Cindy Cheng with edits by Bob Kubinec
# April 4th, 2020

# To-do List:

# 1) Finish resolving/checking for when the 'other'/text_entry category is selected for all applicable variables
# I tried to mark where this still needs to be done with the following comments: "!!! NOTE "
# Note, for now, I've left the 'other' text entry vars as their own columns, we may want to combine some of these 'other' text entries with the main variable for some of these variables at some point
# 2) check 'notes' section and address issues there accordingly
# 3) ?organize sources (rename?)
# 4) ?relabel some of the variable names for text entry vars to make them more intuitive
# 5) at the very end, download latest version of dataset and make sure everything still works

# setup -----------------------------

## load packages and functions

library(tidyr)
library(magrittr)
library(dplyr)
library(readr)
library(qualtRics)
library(stringr)

# whether to make a row for each policy 

long <- F

'%!in%' <- function(x,y)!('%in%'(x,y))

capwords <- function(s, strict = FALSE) {
  cap <- function(s)
    paste(toupper(substring(s, 1, 1)),
          {
            s <- substring(s, 2)
            if (strict)
              tolower(s)
            else
              s
          },
          sep = "", collapse = " ")
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# findAllLinkedRecords() function finds all records that are linked together
findAllLinkedRecords = function(correct_id, data = qualtrics){
  # find each record_id, policy_id or correct_record_match which match with a particular 'corrected_id'
   correct_id_new = data %>% filter(record_id %in% c(correct_id)|
                                    policy_id %in% c(correct_id)|
                                    correct_record_match %in% c(correct_id)) %>% 
                    select(policy_id, record_id, correct_record_match) %>% unlist() %>% unique()
   
   # call this function recursively until you find all of them
   if(length(correct_id_new)>length(correct_id))
    {
    return(findAllLinkedRecords(correct_id_new))
    } else{
    return(correct_id_new) %>% sort()
   }
}

# the linkCorrectedEntries() function addresses the issue where some records are linked to the same original id in a daisy chain, but 
    # they currently do not have the same correct_record_match by:
  # i) linking all the records in this chain to have the same correct_record_match
  # ii) linking all the records in this chain to have the same entry type
linkCorrectedEntries = function(correct_id, data = qualtrics){

  # (A) identify the entry_type associated with a particular correct_record_match id
  corr_slice = data %>% filter(correct_record_match %in% c(correct_id))   %>% 
        select(record_id, policy_id, correct_record_match, correct_dum, entry_type, link_type) 

  # (B1) link all the records in this chain to have the same correct_record_match
  # (B2) link all the records in this chain to have the same entry type
  clean_slice = apply(corr_slice , 1, function(x){
 
  # (1) if the entry_type is a correction
    if( x['entry_type'] == 'correction'){
     
  # (2) find out if there is :
      # a) another entry with the same record_id (Y) as the correct_record match in (1) 
      # b) that itself is not a correction
     if( which(corr_slice$record_id %in% x['correct_record_match'] & corr_slice$entry_type !='correction')%>% length() >0  ) {  

  # (3) if so, then change the correct_record_match in (1) to be the correct_record_match of policy_id (Y) in (2)
      x['entry_type'] = corr_slice$entry_type[which(corr_slice$record_id %in% x['correct_record_match'] & corr_slice$entry_type !='correction')] %>% unique()
      x['correct_record_match'] = corr_slice$correct_record_match[which(corr_slice$record_id %in% x['correct_record_match'] & corr_slice$entry_type !='correction')] %>% unique()
    } }
  return(x)
  }) %>% t() %>% data.frame(., stringsAsFactors = FALSE)  %>% tibble()

  # clean strings  
  clean_slice = clean_slice %>% mutate(record_id = str_trim(record_id),
                                        policy_id= str_trim(policy_id),
                                        correct_record_match = str_trim(correct_record_match))

  # call this function recursively until you link all the corrections to the 'original' entries
  if(any(clean_slice$entry_type == 'correction')){
    linkCorrectedEntries(clean_slice$correct_record_match, data = clean_slice)
  }else{
     return(clean_slice )
   }

 
}

# link updates addresses the issue where some updates are linked to the same original id in a daisy chain, but 
  # they do not have the same policy id
  # by linking all the updates in this chain to the same policy id
linkUpdates = function(p_id, data = slice){
  update_slice = data  %>% 
                select(record_id, policy_id, correct_record_match, correct_dum, entry_type, link_type) 

  if (which(update_slice$entry_type == 'new_entry' & update_slice$correct_dum == 'original')%>% length() ==1){
      orig_policy_id = update_slice$policy_id[which(update_slice$entry_type == 'new_entry' & update_slice$correct_dum == 'original')] %>% unique()
    } else if (which(update_slice$link_type == 'C' & update_slice$entry_type == 'new_entry') %>% length() == 1){
     orig_policy_id = update_slice$policy_id[which(update_slice$entry_type == 'new_entry' & update_slice$link_type == 'C')] %>% unique()}

  update_slice$policy_id =   orig_policy_id 

  return(update_slice)
}

## load data

# note we can't connect to qualtrics via API
# because the TUM has not purchased one
# .csv downloads it is!

country_regions = read_csv(file = 'data/regions/country_region_clean.csv')
regions_df = read_csv(file = 'data/regions/country_regional_groups_concordance.csv')
qualtrics = read_survey('data/CoronaNet/RA/ra_data_pull.csv') %>% 
  mutate(entry_type=recode(entry_type,
                           `1`="new_entry",
                           `New Entry`="new_entry",
                           `Correction to Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="correction",
                           `Update on Existing Entry (type in Record ID in text box)`="update",
                           `Update on Existing Entry for record ID ${e://Field/record_id} (<- if no record ID listed, type in Record ID in text box)`="update")) %>% 
  filter(Progress>98)


 
# text entry cleaning ----------------------------------
## !!! NOTE need to  do this for all text entries
qualtrics$target_city[which(qualtrics$target_city == 'bogota')] = "Bogota"
 

# This script filters out bad records (need to remove/fix these manually so we don't do this)
source("RCode/validation/filter_bad_records.R")

# This script manually recodes values
# Also should be fixed so we don't do this
 
source("RCode/validation/recode_records.R")


# replace entries with documented corrected entries as necessary ----------------------------------

# make vector of ids that need to be corrected
correction_record_ids = qualtrics[which(qualtrics$entry_type == 'correction'), 'entry_type_2_TEXT']

# note that there are some un-matched records; check them out later
matched_corrections = qualtrics$record_id[which(qualtrics$record_id %in% correction_record_ids$entry_type_2_TEXT)]
unmatched_corrections = setdiff(correction_record_ids$entry_type_2_TEXT, matched_corrections)

# make a variable called correct_record_match: if entry is corrected, fill in the corresponding record id entered in entry_type_2_TEXT,
#  if an entry was not corrected, fill in with original record id
qualtrics$correct_record_match = ifelse(
  # if correction is made manually, use record_id written in entry_type_2_TEXT
  grepl(x=qualtrics$entry_type,pattern='correction') & is.na(qualtrics$link_type),
  trimws(qualtrics$entry_type_2_TEXT), 

  qualtrics$record_id
)

# make a variable called correct_dum: dummy variable for if entry is corrected or not
qualtrics$correct_dum = ifelse(qualtrics$entry_type == 'correction', 'correction', 'original')

# link updated policy(ies) with original entry with variable 'policy_id' ----------------------------------
updated_record_ids = qualtrics[which(qualtrics$entry_type == 'update'), 'entry_type_3_TEXT']

matched_updates = qualtrics$record_id[which(qualtrics$record_id %in% updated_record_ids$entry_type_3_TEXT)]
(unmatched_updates = setdiff(updated_record_ids$entry_type_3_TEXT, matched_updates)) # need to take a closer look later


qualtrics <- group_by(qualtrics, record_id) %>% 
              mutate(policy_id=case_when(entry_type=="update" & !is.na(entry_type_3_TEXT)~entry_type_3_TEXT,
                                        TRUE~record_id)) %>% 
              ungroup()

#qualtrics= qualtrics[-which(is.na(qualtrics$policy_id)),]
 
# note making manual corrections of the following type should not be necessary after adding in appropriate question to survey
  # note this can't go in record_records because policy_id and correction_record_ids is not defined until later
qualtrics  = qualtrics %>% mutate(entry_type = ifelse(record_id ==4068919 & policy_id == 4068919 & correct_record_match == 4068919, 'new_entry', entry_type ))

# it would appear that the random generator radnomly generated these record_ids twice
  # which seems somewhat improbable but don't want to spend too much time on this
  # prob would be good to get another pair of eyes on this
  # but for now, just change the record_id
  # for more info see entries that correspond to record_id %in% c("4807248", "9299202", "9329898") | policy_id %in% c("4807248", "9299202", "9329898") |correct_record_match %in% c("4807248", "9299202", "9329898") 
qualtrics = qualtrics %>% mutate(record_id = ifelse(record_id ==9299202 & policy_id == 9299202 & correct_record_match == 9299202, 92992021, record_id ),
                                 policy_id = ifelse(record_id ==92992021 & policy_id == 9299202 & correct_record_match == 9299202, 92992021, policy_id ),
                                 correct_record_match = ifelse(record_id ==92992021 & policy_id == 92992021 & correct_record_match == 9299202, 92992021, correct_record_match ))

# for more info see entries that correspond to record_id %in% c("1251472", "5290875", "7925660", "9238095")| policy_id %in% c("4807248", "9299202", "9329898")|correct_record_match %in%c("4807248", "9299202", "9329898")
qualtrics = qualtrics %>% mutate(record_id = ifelse(record_id ==9238095 & policy_id == 9238095 & correct_record_match == 9238095, 92380951, record_id ),
                                 policy_id = ifelse(record_id ==92380951 & policy_id == 9238095 & correct_record_match == 9238095, 92380951, policy_id ),
                                 correct_record_match = ifelse(record_id ==92380951 & policy_id == 92380951 & correct_record_match == 9238095, 92380951, correct_record_match ))

#for more info see entries that correspond to record_id %in% c("2812219", "3742052")| policy_id %in% c("2812219", "3742052")|correct_record_match %in%c("2812219", "3742052")
qualtrics = qualtrics %>% mutate(record_id = ifelse(record_id ==2812219 & init_country == 'Japan', 28122191, record_id ),
                                 policy_id = ifelse(record_id ==28122191 & init_country == 'Japan', 28122191,  policy_id ),
                                 correct_record_match = ifelse(record_id ==28122191 & init_country == 'Japan', 28122191, correct_record_match ))

 
# Sort out how corrections/updates related to each other 
  # given the move to links for updating/correcting entries, entries that require this kind of cleaning should not grow over time

# identify all of the records that are related to each other
correction_record_ids_manual = qualtrics$correct_record_match[which(qualtrics$correct_record_match != qualtrics$record_id)]
allLinkedRecordsList = unique(lapply(correction_record_ids_manual, function(x){
                              findAllLinkedRecords(x)}))

# then clean up how the corrections/updates are linked
cleanAllLinkedRecords = do.call(rbind,lapply(allLinkedRecordsList , function(x){

  # for each bundle of records that are related to each other
  slice = qualtrics %>% 
          filter(record_id %in% x|policy_id %in% x | correct_record_match %in% x) %>%
          select(record_id, policy_id, correct_record_match, correct_dum, entry_type, link_type)  

  # if you cannot find a 'new_entry' entry for a set of linked policies, declare them 'orphaned' and ignore
  slice = slice %>% mutate(orphanDum = ifelse(any(entry_type == 'new_entry') == FALSE, 1, 0))
  
  # if a set of linked policies are not orphaned
  if(all(slice$orphanDum==0)){

  ## First link all the corrections together into the same 'correct_record_match'
  # if all the correct_record_matches are the same, then it is straightforward to fill in the appropriate entry_type
    if(length(unique(slice$correct_record_match))==1){
      slice = slice %>% mutate(entry_type = 
                        ifelse(entry_type=='correction', 
                               entry_type[which(entry_type!='correction')], 
                               entry_type))
    } else {

  # if all the correct_record_matches are not the same, replace the correct_record_matched iteratively until they are and fill in the appropriate entry_type
    slice = linkCorrectedEntries(slice$correct_record_match)}
   
    # Then link all the updates together into the same 'policy_id'
    slice = linkUpdates(slice$policy_id, slice)
    slice = slice %>% mutate(orphanDum = 0)}
      
  return(slice)}))

# then replace old records with cleaned records back 
cleanAllLinkedRecords = cleanAllLinkedRecords %>% mutate(new_id = paste0(record_id, link_type))
qualtrics = qualtrics %>% mutate(new_id = paste0(record_id, link_type))

varsToMatch = c('record_id',
             'policy_id',
             'correct_record_match',
             'correct_dum',
             'entry_type')


qualtrics = data.frame(qualtrics)
qualtrics[match(cleanAllLinkedRecords$new_id, qualtrics$new_id),varsToMatch]  = cleanAllLinkedRecords[varsToMatch]
qualtrics = tibble(qualtrics)
 
# check only for vars with missing
 
qualtrics<- filter(qualtrics, !is.na(record_id))
 
miss_vars <- names(qualtrics)[sapply(qualtrics, function(c) any(is.na(c)))]

miss_vars <- miss_vars[miss_vars!="correct_record_match"]

qualtrics <- qualtrics %>% 
  group_by(correct_record_match) %>% 
  arrange(correct_record_match,RecordedDate) %>% 
  fill(miss_vars,.direction="down")%>%
  ungroup()%>%
  group_by(policy_id) %>% 
  arrange(policy_id,RecordedDate) %>% 
  fill(all_of(miss_vars),.direction=c("down"))%>% 
  ungroup() 
  
  # remove old entries
  #qualtrics = qualtrics %>% filter(!record_id %in% correction_record_ids)
  
qualtrics <- group_by(qualtrics,correct_record_match) %>% 
  arrange(correct_record_match,desc(RecordedDate)) %>% 
  slice(1) %>%
  ungroup()
  

  # will need to exclude these dud records for now
  
  qualtrics <- filter(qualtrics, !is.na(type),
                      !is.na(init_country))


# there's still a few entries that are 'corrections' when they should be either an update or new entry
    # note, can't run this code before you've untangled the hot mess above

# remove corrections that have no match -- look into this later
qualtrics  = qualtrics %>% filter(entry_type != 'correction')
 

# rename variables ----------------------------------

# rename all type questions that ask extra detail about the 'number' of a policy using the same variable name format
names(qualtrics)[which(names(qualtrics) %in% c("type_quarantine_days", "type_mass_gathering"))] = c('type_num_quarantine_days', 'type_num_mass_gathering')
names(qualtrics)[names(qualtrics) %in% c("type_health_resource_1_TEXT",
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

# let's do this in tidy format
# slower but easier to debug

old_row_num <- nrow(qualtrics)
 
qualtrics <- gather(qualtrics,key="province",value="prov_num",matches("init\\_province")) %>% 
  mutate(index_prov=ifelse(is.na(str_extract(prov_num,"[0-9]+")),
                           "",
                           paste0(init_country,str_extract(prov_num,"[0-9]+")))) %>% 
  select(-prov_num,-province)

 
# reshape country_regions data to long format
country_regions_long = country_regions %>%
  gather(key="p_num",value="init_prov",-Country,-ISO2) %>% 
  mutate(p_num = as.numeric(p_num)+1) %>%
  mutate(index_prov=paste0(Country,p_num)) %>%
  filter(!is.na(init_prov)) %>%
  select(-p_num)

# match province code to actual province name
qualtrics <- left_join(qualtrics,country_regions_long,by="index_prov")

# remove province records that don't match anything

qualtrics <- distinct(qualtrics)

## records that have a province match are still 'duplicated'
# e.g. if a policy comes out of Alaska it currently has
# two index_provs: 'United States3' and 'United StatesNA'
# remove the second one ('United StatesNA')
qualtrics = qualtrics %>% 
  group_by(record_id) %>%
    filter(if (any(!is.na(init_prov)))
      !is.na(init_prov)
  else is.na(init_prov)) %>%
  ungroup()

 
# if(!(old_row_num==nrow(qualtrics))) {
#   stop("You done screwed up the merge moron.")
# }
 
# combining info on target countries --------------------

# replace 'target_country' with 'target_country_sub' when target_country_sub has a value, then remove 'target_country_sub'
# this is because 'target_country_sub' records the country for when a policy is targeted toward a region inside a country
qualtrics[which(qualtrics$target_country_sub != ""), 'target_country'] = qualtrics[which(qualtrics$target_country_sub !=
                                                                                           ""), 'target_country_sub']
qualtrics = qualtrics[, -which(names(qualtrics) == 'target_country_sub')]

qualtrics$target_country[qualtrics$target_geog_level == "All countries"] <- "All countries"



# double check to make sure 'target_country' is empty when 'target_country_sub' has a value
if (length(qualtrics[which(qualtrics$target_country_sub != ""), 'target_country'] %>% table()) == 0) {
  
  print('All Good')
} else{
  stop("Error: Problem with target_country recoding.")
}

qualtrics[which(qualtrics$target_country_sub != ""), 'target_country'] %>% table()
qualtrics[which(qualtrics$target_geog_level == "All countries"), 'target_country'] %>% table()
# double check to make sure 'target_country' is empty when 'all countries' is selected
# if (all (is.na(qualtrics[which(qualtrics$target_geog_level == "All countries"), 'target_country']))) {
# 
#   print('All Good')
# } else{
#   stop(
#     "Error: double check to make sure 'target_country' is empty when 'all countries' is selected"
#   )
# }
 
# saveRDS(distinct(qualtrics),
#         file = paste0(path, "/data/CoronaNet/coranaNetData_recode_records_countries.rds"))

source("RCode/validation/recode_records_countries.R")

 
# check/clean monadic variables to be strictly monadic

# create var domestic_policy which shows if init_country == target_country 
# !!! NOTE: 'domestic_policy' is still has NA fields which need to be fixed
# but for the purposes of cleaning/checking monadic variables, it is fine
qualtrics = qualtrics %>% 
  mutate(domestic_policy = case_when(type=="External Border Restrictions"~0,
                                     init_country == target_country~1,
                                     is.na(target_country) & is.na(target_geog_level)~1,
                                     type=="Internal Border Restrictions"~1,
                                           TRUE~0),
         target_country=ifelse(domestic_policy,init_country,target_country))

monad_types = c('Closure of Schools',
                'Curfew',
                'Social Distancing',
                'Internal Border Restrictions',
                'Restrictions of Mass Gatherings',
                "Restriction of Non-Essential Government Services",
                "Restriction of Non-Essential Businesses",
                "New Task Force or Bureau",
                'Declaration of Emergency') 
 
 
  qualtrics <- mutate(qualtrics,
                      target_country = ifelse(type %in% monad_types & domestic_policy==0 & !is.na(target_country), init_country, target_country),
                      target_who_what = ifelse(type %in% monad_types & domestic_policy == 1, "All Residents (Citizen Residents +Foreign Residents)", target_who_what),
                      target_who_what_10_TEXT = ifelse(type %in% monad_types, NA, target_who_what_10_TEXT),
                      target_direction = ifelse(type %in% monad_types, NA, target_direction ),
                      travel_mechanism = ifelse(type %in% monad_types, NA, travel_mechanism),
                      travel_mechanism_9_TEXT = ifelse(type %in% monad_types, NA, travel_mechanism_9_TEXT)) 

  # qualtrics = qualtrics %>% mutate(target_geog_level = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_geog_level),
  #                                  target_country = ifelse(type %in% monad_types, init_country, target_country),
  #                                  target_country_197_TEXT = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_country_197_TEXT),
  #                                  target_region = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_region),
  #                                  target_region_14_TEXT = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_region_14_TEXT),
  #                                  target_geog_sublevel = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_geog_sublevel),
  #                                  target_province = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_province),
  #                                  target_city = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_city), 
  #                                  target_other = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_other),
  #                                  target_intl_org = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_intl_org),
  #                                  target_who_what = ifelse(type %in% monad_types & domestic_policy == 1, "All Residents (Citizen Residents +Foreign Residents)", target_who_what),
  #                                  target_who_what_10_TEXT = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_who_what_10_TEXT),
  #                                  target_direction = ifelse(type %in% monad_types & domestic_policy == 1, NA, target_direction ),
  #                                  travel_mechanism = ifelse(type %in% monad_types & domestic_policy == 1, NA, travel_mechanism),
  #                                  travel_mechanism_9_TEXT = ifelse(type %in% monad_types & domestic_policy == 1, NA, travel_mechanism_9_TEXT)) 

  
  # if (nrow(qualtrics %>% filter(type %in% monad_types & domestic_policy == 0)) >0){
  #   stop("Error: Problem with target countries for monadic policy types")
  # } 

 
# code certain policy types to always have mandatory enforcement with legal penalties
mandatory_types = c("Declaration of Emergency",
                    "New Task Force or Bureau")

qualtrics = qualtrics %>%
      mutate(compliance = ifelse(type %in% mandatory_types, "Mandatory (Unspecified/Implied)", compliance))


# save as wide clean file ---------------

# remove records with duplicate provinces per record ID. 
# these are some issue we haven't yet resolved

# qualtrics <- group_by(qualtrics,record_id) %>% 
#                         filter(n()==1) %>%
#                           ungroup()
                


saveRDS(distinct(qualtrics),
        file = "data/CoronaNet/coranaNetData_clean_wide.rds")
 
#### Clean the 'other countries' text entries
## !!! NOTE that until April 2, it wasn't possible to do text entry for this, so we've lost this data for now
# its only 4 entries however, and should be straightforward to look in the original sources to get that info
# but still need to do this
# add select all/deselect all button; too tired now to trust myself not to fuck it up, do it in the morning
#qualtrics$target_country[grepl('Other',  qualtrics$target_country)]

## Clean the 'other regions' text
# !!! NOTE  haven't done this yet, should do this at some point
#qualtrics[which(qualtrics$target_region == "Other Regions (please specify below)"), 'target_region_14_TEXT']


# add in additional rows for target areas as needed ----------------------------------

# currently, multiple targets are grouped together in one cell if the policy is the same on all dimensions for all targets

# Disaggregate regions (e.g. Schengen Area) into the relevant component countries
# !!! NOTE: still think about how we want to deal with the 'All' countries entry

# drop observations where coder chose more than 3 regions; CHECK later
qualtrics = qualtrics  %>% filter(unlist(lapply(stringr:::str_split(qualtrics$target_region, ','), function(x) length(x[!is.na(x)]))) <4)


# if coder selected multiple regions, separate them out into different columns
num_region_columns = max(unlist(lapply(stringr:::str_split(qualtrics$target_region, ','), function(x) length(x[!is.na(x)]))))
qualtrics = qualtrics  %>% separate(target_region, c(paste0("regions", 1:num_region_columns)), sep = ',', remove = FALSE)

# match regions to the countries that comprise them
qualtrics[, paste0("regions", 1:num_region_columns)]  = data.frame(apply(qualtrics [, paste0("regions", 1:num_region_columns)], 2, function(x){
  regions_df$country[match(x, regions_df$regions)]}), stringsAsFactors = FALSE)
 

# recollapse separate columns into one column
qualtrics  = qualtrics  %>% 
  mutate_at(vars(starts_with("regions")),
            list( ~ replace(., is.na(.), ""))) %>%
  unite(target_regions_disagg, contains('regions'), sep = ',') %>%
 mutate(target_regions_disagg = gsub('\\,\\,|\\,$', "", target_regions_disagg))


# replace  empty rows in [target_country] with disagregated 'All countries' from [target_geog_level]
# qualtrics[which(qualtrics$target_country == 'All'), 'target_country'] = paste(country_regions$Country, collapse = ',')
# qualtrics[which(qualtrics$target_country == 'All'), 'target_region'] = 'All countries'

# separate out disaggregated target countries from 'All countries' into separate rows 
# qualtrics  = qualtrics %>% separate_rows(target_country, sep = ',')

qualtrics$target_country = str_trim(qualtrics$target_country)

  ### for first version, don't break out target provinces/cities into separate rows
  # but code for doing so is below
  
  ## add in additional rows for target countries as needed
  # qualtrics = qualtrics %>% separate_rows(target_province, sep = ';')
  
  # add in additional rows for target cities as needed
  #qualtrics = qualtrics %>% separate_rows(target_city, sep = ';')
  
  # check target_other variable text entries are standardized
  #  !!!! NOTE haven't done this yet, not necessary for formatting data
  # but we should def do this at some point
  # table(qualtrics$target_other) %>% names() %>%  unique() %>% length()
  
  # note that Palestine/Gaza wasn't an option in the target_country/target_province survey before
  
  
  
  # check target_who_what variable text entries are standardized
  #  !!!! NOTE haven't done this yet, not necessary for formatting data
  # but we should def do this
  
  # table(qualtrics$target_who_what)
  # unique(qualtrics$target_who_what_10_TEXT)
  
  
  # get a separate row for every policy sub-type --------------------------------------
  #
  
  
  
  ###------ combining info on QUANTITY of policy type, where applicable----- ###
  
  # the policies for which the 'quantity' of a policy type apply are:
  # the health resources variables (type_num_health_[health resource]), the number of quarantine days (type_num_quarantine_days), and number of poeple restricted from gathering (type_num_mass_gathterings)
  
  # what makes making it so difficult to turn the health resources data into long format in particular is that:
  # some, but not all health resources have a text entry to code the number of resources (e.g. masks)
  # and some, but not all text entries are filled up
  # at the end of the day, you want one column with the subtype (quality) of the health resource and another column with the number (quantity) of each health resource
  # but these do not overlap perfectly in the data as currently formatted
  
  # to solve this, in the below, (A) first we make a column for the number of health policies (type_sub, and type_sub_num) ( we also throw in days quarantine and number of people restricted from meeting)
  #  (B) then a column for the type of health policies (type_health_policies)
  # type_sub accounts for all sub policies that have a number associated with them (e.g. number of masks, number of quarantine days); type_sub_num gives the actual numbers
  # type_health_resource accounts for all policies that were selected but don't have a number associated with them (e.g. the policy was about masks but the source didn't specify how many)
  # sometimes type_sub and type_health coincide, sometimes they don't
  # (C) then we resolve any duplicates that arise from doing (A) and (B)
  
  # (A) add column names of each health resource to each row that has a text entry
  qualtrics[, grep('type_num', names(qualtrics))] =  data.frame(t(apply(select(qualtrics, contains('type_num')), 1, function(x) {
    ifelse(is.na(x), x, paste(names(x), x, sep = "@"))
  })), stringsAsFactors = FALSE)
  
  qualtrics = qualtrics %>%
    # first change all of the NA's in the relevant columns that code for the number of policies to ""
    mutate_at(vars(starts_with("type_num")),
              list( ~ coalesce(.,""))) %>%
    # then combine all the type_num columns together into one column called 'type_num'
    # !!! note should probably create code to clean all !'s from text entries before hand
    unite(type_sub_num, contains('type_num'), sep = '!') %>%
    # replace extraneous !
    mutate_at(vars(starts_with("type_sub_num")),
              list( ~ gsub("[[:punct:]]+$|^[[:punct:]]+", "", .))) %>%
    mutate_at(vars(starts_with("type_sub_num")),
              list( ~ gsub("!+", "!", .))) %>%
    # remove type_num_[health resource] columns now that they are made redundant by the 'health_num' variable
    select(-contains("type_num"))  %>%
    # separate entries for each health resource
    separate_rows(type_sub_num, sep = '!') %>%
    # separate columns for type of health resource and number of health resource
    # !!! note should probably create code to clean all @'s from text entries before hand
    separate(type_sub_num, c("type_sub", 'type_sub_num'),  "@", fill = 'right')
  
 
  # clean names so that they match what they originally were in the codebook
  qualtrics$type_sub  = gsub('type_num_', '', qualtrics$type_sub) %>% capwords()
  qualtrics$type_sub = qualtrics$type_sub %>% recode(
    MedCen = "Temporary Medical Centers",
    Ppe = "Personal Protective Equipment (e.g. gowns; goggles)",
    Hand_sanit = "Hand Sanitizer",
    Test_kits  = "Test Kits",
    QuaranCen = "Temporary Quarantine Centers",
    Research = "Health Research Facilities",
    PubTest = "Public Testing Facilities (e.g. drive-in testing for COVID-19)",
    Health_insurance = 'Health Insturance'
  )
  
  # !!! NOTE still need to check if all the text entries for the number of a policy type make sense/find a standard format for them as much as possible
  # for the purposes of formatting the data however, not necessary
  
  ###------ making additional rows for QUALITY of policy type, where applicable----- ###
  
  ### (B) Health resources
  
  # clean name so you can separate on ',' without problems
  qualtrics$type_health_resource = gsub(
    "Personal Protective Equipment \\(e.g. gowns, goggles\\)",
    "Personal Protective Equipment (e.g. gowns; goggles)",
    qualtrics$type_health_resource
  )
  
  # this allows you to capture multiple health resources that may not have a number value attached to it
  # e.g. the event is about doctors, but the source does not say how many doctors
  # remember if the event is about doctors and the source says how many, this is already captured in the typ_sub/type_sub_num variables
  qualtrics = qualtrics %>% separate_rows(type_health_resource, sep = ',')
  
  
  # note that when you separate the rows in the above, qualtrics duplicates all of the type_sub/type_sub_num
  # such that you get logical inconsistencies when there is a number value associated with the policy
  #; e.g. a row where the type_health_resource is 'hospitals' and the type_sub is doctors;
  # to fix this you should make those entries NA
  qualtrics[which(qualtrics$type_health_resource != qualtrics$type_sub), c('type_sub', 'type_sub_num')] = NA
  
  
  # (C) Resolve duplicates between number of health policies (type_sub) and quality of health policies (type_health_resource)
  qualtrics = qualtrics %>%
    group_by(record_id, type, type_health_resource) %>%
    filter(if (all(is.na(type_sub)))
      row_number() == 1
      else!is.na(type_sub)) %>%
    ungroup()
  
  # delete all of the health resources options (keep quarantine days and mass gathering) from the type_sub variable
  # this is so you don't get duplicates when you unite all of the 'quality/kind' variables below (e.g. when uniting sub types for biz, health, schools etc)
  qualtrics[which(qualtrics$type_sub == qualtrics$type_health_resource), 'type_sub'] = NA
  
  
  ### make all 'health other texts' into one column
  # the relevant variables are: type_other_health_infra, type_other_health_infra, type_other NOT mutually exclusive
  
  # first clean text entries to make them consistent
  ## !!! NOTE STILL NEED TO DO THIS, but for the purposes of formatting the data, not a priority
  
  # then unite 'other' health categories into one
  # and then separate them into separate rows
  qualtrics = qualtrics %>%
    mutate_at(vars(
      c(
        type_other_health_infra,
        type_other_health_materials,
        type_other_health_staff
      )
    )  ,
    list( ~ replace(., is.na(.), ""))) %>%
    unite(
      type_other_health,
      c(
        type_other_health_infra,
        type_other_health_materials,
        type_other_health_staff
      ),
      sep = "@"
    ) %>%
    
    # replace extraneous @
    mutate_at(vars(type_other_health),
              list( ~ gsub("@+$|^@+", "", .))) %>%
    separate_rows(type_other_health, sep = "@")
  
  # note that when you separate the rows in the above, qualtrics duplicates all of the type_other_health_mat/staff/infra
  # such that you get logical inconsistencies when there is an 'other' text associated with the policy
  #; e.g. a row where the type_health_resource is 'other health materials' and the type_other is masks;
  # to fix this you should make those entries NA
  qualtrics[which(qualtrics$type_health_resource != 'Other Health Materials'), 'type_other_health'] = NA
  
  
  #### Restrictions
  # make a seprate row for each external border restriction sub-category
  qualtrics  = qualtrics %>% separate_rows(type_ext_restrict, sep = ',')
  qualtrics[which(qualtrics$type_ext_restrict == 'None of the above'), 'type_ext_restrict'] = "None of the given external border restrictions measures"
  
  #### Quarantine
  # make a seprate row for each quarantine sub-category
  qualtrics = qualtrics %>% separate_rows(type_quarantine, sep = ',')
  qualtrics[which(qualtrics$type_quarantine == 'Other'), 'type_quarantine'] = 'Other Quarantine'
  
  # delete all quarantine days from the sub type variable when there is a corresponding entry in the type_quarantine var
  # this is so you don't get duplicates when you unite all of the 'quality/kind' variables below
  qualtrics[which(qualtrics$type_sub == "Quarantine_days" &
                    !is.na(qualtrics$type_quarantine)), c('type_sub')] = NA
  
  
  ### Restriction on businesses
  # make a seprate row for each restriction on businesses sub-category
  qualtrics = qualtrics %>% separate_rows(type_business, sep = ',')
  qualtrics[which(qualtrics$type_business == 'Other'), 'type_business'] = 'Other Restricted Businesses'
  
  ### Schools
  # make a seprate row for each school sub-category
  qualtrics  = qualtrics %>% separate_rows(type_schools, sep = ',')
  
  
  ##### unite all the quality/kind sub type variables together in one variable called type_sub_cat ####
  qualtrics = qualtrics %>%
    mutate_at(vars(
      c(
        type_sub,
        type_ext_restrict,
        type_schools,
        type_business,
        type_health_resource,
        type_quarantine
      )
    )  ,
    list( ~ replace(., is.na(.), ""))) %>%
    unite(
      type_sub_cat,
      c(
        type_sub,
        type_ext_restrict,
        type_schools,
        type_business,
        type_health_resource,
        type_quarantine
      ),
      sep = ""
    )
  
  
  # clean up 'other' text entries and put into one column -----------------------
  
  
  ### quarantine other texts
  # type_quarantine_4_TEXT and type_quarantine_5_TEXT NOT mutually exclusive
  # the following code in this section:
  # 1) cleans the text entries
  # 2) combines them into one column
  # 3) separates out the rows
  
  #1) clean text entries
  # !!! NOTE: I only did this for up to the March 30 version of the data, still more to resolve for newer entries
  # table(qualtrics$type_quarantine_4_TEXT)
  # table(qualtrics$type_quarantine_5_TEXT)
  
  qualtrics[which(qualtrics$type_quarantine_4_TEXT %in% c("over 70 years", "70+")), 'type_quarantine_4_TEXT'] = ">70"
  qualtrics[which(qualtrics$type_quarantine_5_TEXT == "not specified, no information found"), 'type_quarantine_5_TEXT'] = "Not specified"
  qualtrics[which(qualtrics$type_quarantine_5_TEXT == "based on gender"), 'type_quarantine_5_TEXT'] = "Based on Gender"
  
  
  qualtrics = qualtrics %>%
    mutate_at(vars(c(
      type_quarantine_4_TEXT, type_quarantine_5_TEXT
    ))  ,
    list( ~ replace(., is.na(.), ""))) %>%
    
    # 2) combine them into one column
    unite(type_quarantine_text,
          c(type_quarantine_4_TEXT, type_quarantine_5_TEXT),
          sep = "@") %>%
    
    # replace extraneous @
    mutate_at(vars(starts_with("type_quarantine_text")),
              list( ~ gsub("@+$|^@+", "", .))) %>%
    # 3) separate the rows
    separate_rows(type_quarantine_text, sep = "@")
  
  
  # note that when you separate the rows in the above, qualtrics duplicates all of the type_quarantine_4_TEXT, type_quarantine_5_TEXT
  # such that you get logical inconsistencies when there is an 'other' text associated with the policy
  #; e.g. a row where the type_sub_cat is 'self quarantine' and the type_quarantine_text is an age limit, when it should be NA;
  # to fix this you should make those entries NA
  quar_text_cats = c(
    'Quarantine only applies to people of certain ages. Please note the age restrictions in the text box.',
    'Other Quarantine'
  )
  qualtrics[-which(qualtrics$type_sub_cat %in% quar_text_cats), c('type_quarantine_text')] = NA
  
  
  ### busines other texts
  # clean/standardize entries
  
  # !!! NOTE STILL HAVEN"T DONE THIS, but for purposes of formatting data to long version, not an issue
  # table(qualtrics$type_business_6_TEXT)
  
  
  #### Finally, unite all the text entries for the 'other' variables
  # they should all also be mutually exclusive but CHECK
  qualtrics = qualtrics %>%
    mutate_at(vars(
      c(
        type_quarantine_text,
        type_business_6_TEXT,
        type_other_health
      )
    )  ,
    list( ~ replace(., is.na(.), ""))) %>%
    unite(
      type_sub_cat_other,
      c(
        type_quarantine_text,
        type_business_6_TEXT,
        type_other_health
      ),
      sep = ""
    )
  
  
  # note that when you separate the rows in the above, qualtrics duplicates all of the 'other' text entries
  # such that you get logical inconsistencies when there is an 'other' text associated with the policy
  #; e.g. a row where the type_sub_cat is 'shopping centers' and the business is the text entry type_sub_cat_other is e.g. tattoo parlos;
  # to fix this you should make those entries NA
  qualtrics[which(
    qualtrics$type == "Restriction of Non-Essential Businesses" &
      qualtrics$type_sub_cat != "Other Restricted Businesses"
  ), c('type_sub_cat_other')] = NA
  
  
  # add one row for each target country
  
  # separate out disaggregated target countries into separate rows
  qualtrics  = qualtrics %>% separate_rows(target_regions_disagg, sep = ',')
  
  ## add additional rows for target countries/regional groupings
  qualtrics = qualtrics %>%
    mutate_at(vars("target_country", "target_regions_disagg"),
              list( ~ replace(., is.na(.), ""))) %>%
    unite(target_country, c(target_country, target_regions_disagg), sep = ',') %>%
    mutate(target_country = gsub("\\,$|^\\,", "", target_country)) %>%
    separate_rows(target_country, sep = ',')
  
  saveRDS(distinct(qualtrics),
          file = "data/CoronaNet/coranaNetData_clean_long.rds")
  





