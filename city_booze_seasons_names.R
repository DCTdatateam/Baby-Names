library(tidyverse)
library(stringr)
rm(list = ls())
## import latest full list data


full_list <- read_csv("https://www.nrscotland.gov.uk/files//statistics/babies-names/23/babies-first-names-all-names-all-years.csv")

full_list <- full_list %>% 
  rename(rank = Rank, position = Position, yr = 'Registration Year', FirstForename = 'First forename', number = 'Number of babies', sex = Sex)

##WORLD BABIES


#Upload recent version of Simple Maps World City Database
countries <- read_csv("worldcities.csv") %>% 
  arrange(desc(population)) %>%  
  distinct(city_ascii, .keep_all = TRUE) 

world_babies <- full_list %>% 
  group_by(FirstForename, sex) %>% 
  summarise(`All time number` = sum(`number`)) %>% 
  pivot_wider(names_from = sex, 
              values_from = 'All time number', 
              values_fill = 0) %>% 
  rename('Boys' = B,
         'Girls' = G) 

world_babies <- world_babies %>% 
  mutate(`All time total` = Boys + Girls) %>% 
  mutate(`% Male` = (Boys / `All time total`) * 100) %>% 
  mutate( `% Female` = (Girls / `All time total`) * 100) %>% 
  mutate(`Primary Gender` = case_when( `% Female` > `% Male` ~ "Mostly girls",
                                       `% Female` < `% Male` ~ "Mostly boys", 
                                       `% Female` == `% Male` ~ "Equally split")) %>% 
  rename(city_ascii = FirstForename)

world_final <- inner_join(countries, world_babies)

write.csv(world_final, "Worldbabynames.csv", row.names = FALSE)


##SEASONAL BABIES

seasonal <- full_list %>% 
  group_by(FirstForename) %>% 
  summarise(`All time number` = sum(`number`)) %>% 
  filter(FirstForename == 'Summer' | FirstForename == 'Autumn' | FirstForename == 'Winter' | FirstForename == 'Spring') 


##Monthly babies

months <- data.frame(FirstForename = c('January', 'February', 'March', 'April', 
                                       'May', 'June', 'July', 'August', 'September', 
                                       'October', 'November', 'December'))

monthly_babies <- full_list %>%
  group_by(FirstForename) %>%
  summarise(`All time number` = sum(`number`)) %>%
  right_join(months, by = "FirstForename") %>%
  mutate(`All time number` = ifelse(is.na(`All time number`), 0, `All time number`)) %>%
  arrange(match(FirstForename, c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')))



## BOOZY BABIES 
alcohol_update <- data.frame(FirstForename = c('Syrah', 'Stella', 'Gin', 'Jack-Daniel', 'Asahi', 'Tequilla-Paris', 'Brandy', 
                                               'Sherry', 'Foster', 'Jameson'))

booze <- full_list %>% 

  

  

         