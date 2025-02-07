library(tidyverse)
library(stringr)

rm(list = ls())

full_list <- read_csv("https://www.nrscotland.gov.uk/files//statistics/babies-names/23/babies-first-names-all-names-all-years.csv") 

##EXTINCT NAMES
## N.B. The below code is dependent on the data being sorted from 
# oldest year to most recent

##Column renaming for 2023

full_list <- full_list %>% 
  rename(rank = Rank, position = Position, yr = 'Registration Year', FirstForename = 'First forename', number = 'Number of babies', sex = Sex)

#Remove not needed columns and reshape data
boys_wide <- full_list %>% 
  filter(sex == "B") %>% 
  select(!c(rank, position, sex)) %>% 
  pivot_wider(names_from = 'FirstForename',
              values_from = 'number', 
              values_fill = 0) %>% 
  select(where(~any(. >= 100)))

#Separate out only those with 0 in the last year
  
boys_last_year <- boys_wide %>% 
  select(c(yr, where(~last(.) == 0)))


#Same for girls

girls_wide <- full_list %>% 
  filter(sex == "G") %>% 
  select(!c(rank, position, sex)) %>% 
  pivot_wider(names_from = 'FirstForename',
              values_from = 'number', 
              values_fill = 0) %>% 
  select(where(~any(. >= 100)))

#Separate out only those with 0 in the last year

girls_last_year <- girls_wide %>% 
  select(c(yr, where(~last(.) == 0)))


#Join boys and girls 

extinct_unfilter <- full_join(boys_last_year, girls_last_year)





last_row <- extinct_unfilter %>% 
  slice(n())

without_last <-extinct_unfilter %>% 
  slice(-n())

extinct_final <- without_last %>% 
  select(c(yr,where(~!any(. == 0)))) %>% 
  bind_rows(last_row) %>% 
  select(where(~!any(is.na(.))))
##just boys

last_row_b <- boys_last_year %>% 
  slice(n())
without_last_b <- boys_last_year %>% 
  slice(-n())

extinct_final_b <- without_last_b %>% 
  select(c(yr,where(~!any(. == 0)))) %>% 
  bind_rows(last_row) %>% 
  select(where(~!any(is.na(.))))


##just girls

last_row_g <- girls_last_year %>% 
  slice(n())
without_last_g <- girls_last_year %>% 
  slice(-n())

extinct_final_g <- without_last_g %>% 
  select(c(yr,where(~!any(. == 0)))) %>% 
  bind_rows(last_row) %>% 
  select(where(~!any(is.na(.))))

write.csv(extinct_final, "extinct_names.csv", row.names = FALSE)


##TOP FOR THE FIRST TIME

top_boys <- full_list %>% 
  filter(sex == "B") %>% 
  select(!c(rank, position, sex)) %>% 
  pivot_wider(names_from = 'FirstForename',
              values_from = 'number', 
              values_fill = 0) %>% 
  select(c(yr, where(~last(.) >= 100)))



top_girls <- full_list %>% 
  filter(sex == "G") %>% 
  select(!c(rank, position, sex)) %>% 
  pivot_wider(names_from = 'FirstForename',
              values_from = 'number', 
              values_fill = 0) %>% 
  select(c(yr, where(~last(.) >= 100)))

top_joint <- full_join(top_boys,top_girls)

top_last_row <- top_joint %>% 
  slice(n())

top_without_last <-top_joint %>% 
  slice(-n())

top_final <- top_without_last %>% 
  select(c(yr, where(~!any(. >= 100)))) %>% 
  bind_rows(top_last_row) %>% 
  select(where(~!any(is.na(.))))

write.csv(top_final, "newly_trending_names.csv", row.names = FALSE)
    
    