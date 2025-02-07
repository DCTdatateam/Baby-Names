library(tidyverse)
library(stringr)
rm(list = ls())
## import latest full list data

full_list <- read_csv("https://www.nrscotland.gov.uk/files//statistics/babies-names/23/babies-first-names-all-names-all-years.csv")

full_list <- full_list %>% 
  rename(rank = Rank, position = Position, yr = 'Registration Year', FirstForename = 'First forename', number = 'Number of babies', sex = Sex)

peak_boys <- full_list %>%
  filter(sex == "B") %>% 
  group_by(FirstForename) %>% 
  summarise(`Peak number in year` = max(`number`),
            `Highest rank` = min(rank)) %>% 
  mutate(Topten = ifelse(`Highest rank` <=10, "TRUE", "FALSE"),
         `Name has ever reached the top spot?`= ifelse(`Highest rank` ==1, "Yes", "No"),
         `Ever 100+`= ifelse(`Peak number in year` >=100, "TRUE", "FALSE"))

peak_girls <- full_list %>%
  filter(sex == "G") %>% 
  group_by(FirstForename) %>% 
  summarise(`Peak number in year` = max(`number`),
            `Highest rank` = min(rank)) %>% 
  mutate(Topten = ifelse(`Highest rank` <=10, "TRUE", "FALSE"),
         `Name has ever reached the top spot?`= ifelse(`Highest rank` ==1, "Yes", "No"),
         `Ever 100+`= ifelse(`Peak number in year` >=100, "TRUE", "FALSE"))

full_list_boys <-  full_list %>% 
  left_join(peak_boys)

full_list_girls <- full_list %>% 
  left_join(peak_girls)

## boys bar chart race

boys_bar_chart_race <- full_list_boys %>% 
  filter(sex == "B",
         Topten == "TRUE") %>% 
  select(c("FirstForename", "Name has ever reached the top spot?", "sex", "yr", "number")) %>% 
  group_by(FirstForename) %>% 
  pivot_wider(names_from = yr, values_from = number) %>% 
  rename(Name = FirstForename)

## girls bar chart race

girls_bar_chart_race <- full_list_girls %>% 
  filter(sex == "G",
         Topten == "TRUE") %>% 
  select(c("FirstForename", "Name has ever reached the top spot?", "sex", "yr", "number")) %>% 
  group_by(FirstForename) %>% 
  pivot_wider(names_from = yr, values_from = number) %>% 
  rename(Name = FirstForename)


## export tables

write.csv(boys_bar_chart_race, "boys_race.csv", row.names = FALSE)
write.csv(girls_bar_chart_race, "girls_race.csv", row.names = FALSE)
