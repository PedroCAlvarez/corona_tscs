# send RA update emails

require(googlesheets4)
require(dplyr)
require(blastula)
require(ggplot2)
require(readr)
require(stringr)
require(lubridate)
require(qualtRics)


# get list of RA emails

ra_data <- qualtRics::read_survey("data/CoronaNet/RA/ra_data_pull_purified.csv")

# jataware

#sheets_auth()

jat_direct <- sheets_get("https://docs.google.com/spreadsheets/d/1EW-Vsue2fQ7k41gC31hWjIUr290VPDO-0w-Ig_Wh71E/edit?ts=5e865e7d#gid=0") %>% 
  sheets_read()


# need to merge this back to our data

country_merge <- distinct(select(ra_data,ra_name,country)) %>% 
  mutate(country=recode(country,Czechia="Czech Republic",
                             `United Republic of Tanzania`="Tanzania",
                             `Micronesia`="F.S. Micronesia",
                             `Timor Leste`="East Timor",
                             `Democratic Republic of the Congo`="DR Congo",
                             `Republic of the Congo`="Congo",
                             `Cabo Verde`="Cape Verde",
                             `Sao Tome and Principe`="São Tomé and Príncipe"))
         

jat_direct <- left_join(jat_direct,country_merge,by=c(Country="country"))

# save this for use

saveRDS(jat_direct,"data/CoronaNet/RA/jat_direct.rds")

# loop and download each dataset

lapply(unique(jat_direct$Country), function(c) {
  jat_data <- try(sheets_get(unique(jat_direct$`Tracker URL`[jat_direct$Country==c])) %>% 
    sheets_read() %>% 
    mutate(publish_date=mdy_hms(publish_date)))
  Sys.sleep(30)
  
  if('try-error' %in% class(jat_data)) {
    jat_data <- sheets_get(unique(jat_direct$`Tracker URL`[jat_direct$Country==c])) %>% 
      sheets_read() %>% 
      mutate(publish_date=mdy_hms(publish_date))
  }
  
  saveRDS(jat_data,paste0("data/CoronaNet/RA/jat_",c,".rds"))
  
})

# now send emails 

ra_emails <- read_csv("data/CoronaNet/RA/ra_data_pull.csv") %>% 
  slice(-c(1:2)) %>% 
  select(ra_name,RecipientEmail,init_country) %>% 
  distinct(RecipientEmail,.keep_all = T) %>% 
  mutate(ra_name=coalesce(ra_name,"")) %>% 
  filter(ra_name!="") %>% 
  mutate(init_country=recode(init_country,Czechia="Czech Republic",
                             `United Republic of Tanzania`="Tanzania",
                             `Micronesia`="F.S. Micronesia",
                             `Timor Leste`="East Timor",
                             `Democratic Republic of the Congo`="DR Congo",
                             `Republic of the Congo`="Congo",
                             `Cabo Verde`="Cape Verde",
                             `Sao Tome and Principe`="São Tomé and Príncipe"))

country_merge <- filter(country_merge,!is.na(country),country!="European Union")

country_out <- lapply(unique(country_merge$country), function(i) {
  print(i)
  rmarkdown::render("RCode/validation/RA_update_website.Rmd",
                    output_file=paste0("~/saudiwin.github.io/content/countrysite/",
                substr(str_extract(i,"[A-Za-z]+"),1,10),".html"),
                params=list(country=i))
  # system(paste0("cp RCode/validation/RA_update_website.html ~/saudiwin.github.io/content/countrysite/",
  #               substr(str_extract(i,"[A-Za-z]+"),1,10),
  #               ".html"))
  # system(paste0("cp -R RCode/validation/RA_update_website.html ~/saudiwin.github.io/content/countrysite/",
  #               substr(str_extract(i,"[A-Za-z]+"),1,10),
  #               ".html"))
})

system("git -C ~/saudiwin.github.io pull")

system2("hugo",args=c("-s ~/saudiwin.github.io"))

# copy dirs as hugo won't

#system("cp -R ~/saudiwin.github.io/content/countrysite/*files* ~/saudiwin.github.io/public/countrysite/")


#system("git -C ~/saudiwin.github.io add public/*")
system("git -C ~/saudiwin.github.io add content/countrysite/*")
system("git -C ~/saudiwin.github.io commit -m 'website country dashboard update'")
system("git -C ~/saudiwin.github.io push")

# get list of country/html links

country_links <- lapply(unique(country_merge$country),
                        function(i) paste0("http://www.robertkubinec.com/countrysite/",substr(str_extract(i,"[A-Za-z]+"),1,10),".html"))

country_links <- lapply(1:length(unique(country_merge$country)), function(c) {
  this_var <- country_links[[c]]
  names(this_var) <- unique(country_merge$country)[c]
  this_var
})

names(country_links) <- unique(country_merge$country)

email_out <- lapply(country_links, function(c) {
  this_data <- filter(ra_emails,init_country==names(c),
                      !is.na(RecipientEmail))
  
  this_email <- render_email("RCode/validation/RA_update_email.Rmd",
                             render_options=list(params=list(country=unique(this_data$init_country),
                                                             country_link=c)))
  print(paste0("Sending to ",this_data$RecipientEmail))
  
  if(!all(is.na(this_data$RecipientEmail))) {
    check <- try(smtp_send(this_email,to=unique(this_data$RecipientEmail),
                           from="admin@coronanet-project.org",
                           subject=paste0("Weekly Update Email from CoronaNet for Country",names(c)),
                           credentials=creds_file("aws_cred"),verbose=T))
    
    while('try-error' %in% class(check)) {
      Sys.sleep(20) 
      check <- try(smtp_send(this_email,to=unique(this_data$RecipientEmail),
                             from="admin@coronanet-project.org",
                             subject=paste0("Weekly Update Email from CoronaNet for Country",names(c)),
                             credentials=creds_file("aws_cred"),verbose=T))
    }
    return(check)
  } else {
    print("Recipient email address is missing.")
  }
  
  
})

