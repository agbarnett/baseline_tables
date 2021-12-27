# 0_country_email.R
# get country code emails and US states. Used for affiliation data
# August 2021
library(rvest)
library(dplyr)
library(countrycode) # for list of countries for affiliation

# get list of countries
countries = c(countrycode::codelist$country.name.en, 'UK', 'USA', 'Korea','Republic of Korea') # add a few common abbreviations and alternatives

# get email extensions from Wikipedia
url = "https://en.wikipedia.org/wiki/Country_code_top-level_domain"
page = read_html(url)
table = page %>% 
  xml_nodes("table") %>%
  .[2] %>% # 
  html_table(header = TRUE, fill = TRUE) 
emails = table[[1]] %>%
  select("Name[7]","Entity") %>%
  rename('email' = 'Name[7]',
         'country' = 'Entity') %>%
  mutate(email = paste('\\', email, sep='')) # to make '.'

# get US states 
url = 'https://en.wikipedia.org/wiki/List_of_U.S._state_and_territory_abbreviations'
page = read_html(url)
table = page %>% 
  xml_nodes("table") %>%
  .[1] %>% # 
  html_table(header = FALSE, fill = TRUE) # ignore header
states = table[[1]] %>%
  filter(X2=='State') %>%
  select(X1, X7) %>%
  rename('state' = 'X1',
         'postal' = 'X7')

# get capital cities worldwide
url = 'https://en.wikipedia.org/wiki/List_of_national_capitals'
page = read_html(url)
table = page %>% 
  xml_nodes("table") %>%
  .[2] %>% # 
  html_table(header = TRUE, fill = TRUE) #
capitals = table[[1]] %>%
  janitor::clean_names() %>%
  rename('country' = 'country_territory') %>%
  select(-notes)

# save
save(emails, states, countries, capitals, file='data/emails_and_states.RData')
