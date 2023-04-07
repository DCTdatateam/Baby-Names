library(tidyverse)
library(stringr)
  
## import latest full list data

full_list <- read_csv("https://www.nrscotland.gov.uk/files//statistics/babies-names/22/babies-first-names-all-names-all-years.csv") %>% 
  mutate(`Unique in year` = ifelse(number ==1, "TRUE", "FALSE")) %>% 
  rename(Year = yr,
         `Number in year` = number) %>% 
  mutate(Length = nchar(FirstForename)) %>% 
  mutate(Hyphenated = if_else(grepl("-", FirstForename), "TRUE", "FALSE")) %>% 
  mutate(Hyphens = str_count(FirstForename,"\\-")) %>% 
  mutate(`Ends with hyphen` = if_else(str_ends(FirstForename, '\\-'), "TRUE", "FALSE"))

first_appears <- full_list %>% 
  group_by(FirstForename) %>% 
  summarise(Appears = min(Year))

full_list <- full_list %>%
  left_join(first_appears)


## totals and highest rank etc 

all_time_count_all <- full_list %>% 
  group_by(FirstForename) %>% 
  summarise(`All time number` = sum(`Number in year`),
            `Peak number in year` = max(`Number in year`),
            `Highest rank` = min(rank),
            `Lowest number in year` = min(`Number in year`),
            `Lowest rank` = max(rank))

all_time_count_boys <- full_list %>% 
  filter(sex == "B") %>% 
  group_by(FirstForename) %>% 
  summarise(`All time number boys` = sum(`Number in year`),
            `Peak number in year boys` = max(`Number in year`),
            `Highest rank boys` = min(rank),
            `Lowest number in year boys` = min(`Number in year`),
            `Lowest rank boys` = max(rank))



all_time_count_girls <- full_list %>% 
  filter(sex == "G") %>% 
  group_by(FirstForename) %>% 
  summarise(`All time number girls` = sum(`Number in year`),
            `Peak number in year girls` = max(`Number in year`),
            `Highest rank girls` = min(rank),
            `Lowest number in year girls` = min(`Number in year`),
            `Lowest rank girls` = max(rank))

full_list_all <- full_list %>% 
  left_join(all_time_count_all) %>%
  left_join(all_time_count_boys) %>% 
  left_join(all_time_count_girls) %>% 
  mutate(`Unique all time` = ifelse(`All time number` ==1, "TRUE", "FALSE")) %>% 
  mutate(`Unique all time for boys` = ifelse(`All time number boys` ==1, "TRUE", "FALSE")) %>%
  mutate(`Unique all time for girls` = ifelse(`All time number girls` ==1, "TRUE", "FALSE"))



## unique names in their own year totals

unique_names_yr_all <- full_list_all %>% 
  filter(`Number in year` == 1) %>% 
  group_by(Year) %>% 
  summarise(`Unique names` = sum(`Number in year`))

unique_names_yr_boys <- full_list %>% 
  filter(sex == "B") %>% 
  filter(`Number in year` == 1) %>% 
  group_by(Year) %>% 
  summarise(`Unique names` = sum(`Number in year`))

unique_names_yr_girls <- full_list %>% 
  filter(sex == "G") %>% 
  filter(`Number in year` == 1) %>% 
  group_by(Year) %>% 
  summarise(`Unique names` = sum(`Number in year`))

## all time unique names - with having to be unique within gender

unique_names_all_time_all <- full_list_all %>% 
  filter(`Unique all time` == "TRUE") %>% 
  group_by(Year) %>% 
  summarise(`Still unique` = sum(`Number in year`))

unique_names_all_time_boys <- full_list_all %>% 
  filter(sex == "B") %>% 
  filter(`Unique all time for boys` == "TRUE") %>% 
  group_by(Year) %>% 
  summarise(`Still unique` = sum(`Number in year`))

unique_names_all_time_girls <- full_list_all %>% 
  filter(sex == "G") %>% 
  filter(`Unique all time for girls` == "TRUE") %>% 
  group_by(Year) %>% 
  summarise(`Still unique` = sum(`Number in year`))

## all time unique names totals - with having to be totally unique
Tunique_names_all_time_all <- full_list_all %>% 
  filter(`Unique all time` == "TRUE") %>% 
  group_by(Year) %>% 
  summarise(`Still unique` = sum(`Number in year`))


Tunique_names_all_time_boys <- full_list_all %>% 
  filter(`Unique all time` == "TRUE") %>% 
  filter(sex == "B") %>% 
  group_by(Year) %>% 
  summarise(`Still unique` = sum(`Number in year`))

Tunique_names_all_time_girls <- full_list_all %>% 
  filter(sex == "G") %>% 
  filter(`Unique all time` == "TRUE") %>% 
  group_by(Year) %>% 
  summarise(`Still unique` = sum(`Number in year`))


## BN Rate per population girls

girls_rate <- full_list %>% 
  filter(sex == "G") %>% 
  group_by(Year, FirstForename, `Number in year`) %>% 
  filter(rank == 1)  

years_at_top <- girls_rate %>% 
  group_by(FirstForename) %>% 
  summarise(`Total years at top`=sum(rank))

total_registered_g <- full_list %>% 
  filter(sex == "G") %>% 
  group_by(Year) %>% 
  summarise(`Out of` =sum(`Number in year`))  


girls_rate <- girls_rate %>% 
  left_join(total_registered_g) %>%
  mutate(`Rate per 1,000` = (`Number in year`/`Out of`)*1000) %>% 
  left_join(years_at_top)

## BN Rate per population boys

boys_rate <- full_list %>% 
  filter(sex == "B") %>% 
  group_by(Year, FirstForename, `Number in year`) %>% 
  filter(rank == 1)  

years_at_top <- boys_rate %>% 
  group_by(FirstForename) %>% 
  summarise(`Total years at top`=sum(rank))

total_registered_b <- full_list %>% 
  filter(sex == "B") %>% 
  group_by(Year) %>% 
  summarise(`Out of` =sum(`Number in year`))  


boys_rate <- boys_rate %>% 
  left_join(total_registered_b) %>%
  mutate(`Rate per 1,000` = (`Number in year`/`Out of`)*1000) %>% 
  left_join(years_at_top)
  


## BN unique names stacked area 


All <- full_list %>% 
  group_by(Year) %>% 
  summarise(`Total babies registered` = sum(`Number in year`)) %>% 
  left_join(unique_names_yr_all) %>% 
  left_join(unique_names_all_time_all) %>% 
  mutate(`Not unique` = `Total babies registered`-`Unique names`) %>% 
  mutate(`% unique`=round(`Unique names`/`Total babies registered`*100, digit=1)) %>% 
  mutate(Filter = "All", .after = Year) %>% 
  mutate(`Pop up` = "babies", .before = `Total babies registered`) 

Boys  <- full_list %>% 
  group_by(Year) %>% 
  filter(sex == "B") %>% 
  summarise(`Total babies registered` = sum(`Number in year`)) %>% 
  left_join(unique_names_yr_boys) %>% 
  left_join(unique_names_all_time_boys) %>% 
  mutate(`Not unique` = `Total babies registered`-`Unique names`) %>% 
  mutate(`% unique`=round(`Unique names`/`Total babies registered`*100, digit=1)) %>% 
  mutate(Filter = "Boys", .after = Year) %>% 
  mutate(`Pop up` = "boys", .before = `Total babies registered`) 

Girls  <- full_list %>% 
  group_by(Year) %>% 
  filter(sex == "G") %>% 
  summarise(`Total babies registered` = sum(`Number in year`)) %>% 
  left_join(unique_names_yr_girls) %>% 
  left_join(unique_names_all_time_girls) %>% 
  mutate(`Not unique` = `Total babies registered`-`Unique names`) %>% 
  mutate(`% unique`=round(`Unique names`/`Total babies registered`*100, digit=1)) %>% 
  mutate(Filter = "Girls", .after = Year) %>% 
  mutate(`Pop up` = "girls", .before = `Total babies registered`) 

area_chart <- All %>% 
  rbind(Boys, Girls)

## Boys all time unique names table

boys_table <- full_list_all %>% 
  filter(`Unique all time` == "TRUE", sex == "B") %>% 
  select(Year,FirstForename) %>% 
  rename(`First forename` = FirstForename)

## Girls all time unique names table
girls_table <- full_list_all %>% 
  filter(`Unique all time` == "TRUE", sex == "G") %>% 
  select(Year,FirstForename) %>% 
  rename(`First forename` = FirstForename)

## BN Longest names bubble chart

longest_names <- full_list_all %>% 
  slice_max(Length, n = 20)

## Multiple hyphens

multiple_hyphen <- full_list_all %>%
  filter(Hyphens >= 3)

# Ends with hyphen

ends_hyphen <- full_list_all %>% 
  filter(`Ends with hyphen` == "TRUE")

## BN Hyphenated names

girls_hyphenated <- full_list_all %>% 
  filter(Hyphenated == "TRUE", sex == "G") %>% 
  group_by(Year) %>% 
  summarise(Girls = sum(`Number in year`))

boys_hyphenated <- full_list_all %>% 
  filter(Hyphenated == "TRUE", sex == "B") %>% 
  group_by(Year) %>% 
  summarise(Boys = sum(`Number in year`))

hyphenated_chart <- girls_hyphenated %>% 
  left_join(boys_hyphenated) %>% 
  mutate(Total = Girls + Boys)

## Top 25 boys names time series

top_boys <- full_list_all %>% 
  filter(sex == "B", `Highest rank boys` <= 25) %>% 
  select(Year, FirstForename, rank) %>% 
  pivot_wider(names_from = FirstForename, values_from = rank)


top_boys_colours <- full_list_all %>% 
  filter(sex == "B", rank <= 25) %>% 
  select(FirstForename, `Highest rank boys`) %>% 
  distinct(FirstForename, .keep_all = TRUE) %>% 
  mutate(Colour = case_when(`Highest rank boys` <= 25  & `Highest rank boys` > 5 ~ '#d1c5a0',
                              `Highest rank boys` <= 5  & `Highest rank boys` > 1 ~ '#8c6d12',
                              `Highest rank boys` <= 1  ~ '#435a6a')) %>% 
  mutate(Flourish = paste(FirstForename, ": ", Colour, sep = ""))



## Top 25 girls names time series
top_girls <- full_list_all %>% 
  filter(sex == "G", `Highest rank girls` <= 25) %>% 
  select(Year, FirstForename, rank) %>% 
  pivot_wider(names_from = FirstForename, values_from = rank)

top_girls_colours <- full_list_all %>% 
  filter(sex == "G", rank <= 25) %>% 
  select(FirstForename, `Highest rank girls`) %>% 
  distinct(FirstForename, .keep_all = TRUE) %>% 
  mutate(Colour = case_when(`Highest rank girls` <= 25  & `Highest rank girls` > 5 ~ '#c09baf',
                            `Highest rank girls` <= 5  & `Highest rank girls` > 1 ~ '#610536',
                            `Highest rank girls` <= 1  ~ '#f2b705')) %>% 
  mutate(Flourish = paste(FirstForename, ": ", Colour, sep = "")) 

## Registered for the first time in latest year

latest_year <- max(full_list$Year)

new_names_boys <- full_list_all %>% 
  filter(Appears == latest_year, sex == "B") %>% 
  select(Year, sex, FirstForename,`Number in year`, rank, `Unique in year`, Length,
         Hyphenated, Hyphens, Appears, `All time number boys`)

new_names_girls <- full_list_all %>% 
  filter(Appears == latest_year, sex == "G") %>% 
  select(Year, sex, FirstForename,`Number in year`, rank, `Unique in year`, Length,
         Hyphenated, Hyphens, Appears, `All time number girls`)
  
## summary table

boys_summary <- Boys %>% 
  select(Year, `Total babies registered`, `Unique names`, `% unique`, `Still unique`) %>% 
  rename(`Boys registered` = `Total babies registered`,
         `Boys unique names` = `Unique names`,
         `Boys % unique` = `% unique`,
         `Boys still unique` = `Still unique`)

girls_summary <- Girls %>% 
  select(Year, `Total babies registered`, `Unique names`, `% unique`, `Still unique`) %>% 
  rename(`Girls registered` = `Total babies registered`,
         `Girls unique names` = `Unique names`,
         `Girls % unique` = `% unique`,
         `Girls still unique` = `Still unique`)

  summary_table <- All %>% 
    select(Year, `Total babies registered`, `Unique names`, `% unique`, `Still unique`) %>% 
    left_join(boys_summary) %>% 
    left_join(girls_summary)



## export data
write.csv(summary_table, "summary_table.csv", row.names = FALSE)
write.csv(girls_rate, "girls_rate.csv", row.names = FALSE)
write.csv(boys_rate, "boys_rate.csv", row.names = FALSE)
write.csv(area_chart, "area_chart.csv", row.names = FALSE)
write.csv(boys_table, "boys_table.csv", row.names = FALSE)
write.csv(girls_table, "girls_table.csv", row.names = FALSE)
write.csv(longest_names, "longest_names.csv", row.names = FALSE)
write.csv(ends_hyphen, "ends_hyphen.csv", row.names = FALSE)
write.csv(hyphenated_chart, "hyphenated_chart.csv", row.names = FALSE)
write.csv(top_boys, "top_boys.csv", row.names = FALSE)
write.csv(top_boys_colours, "top_boys_colours.csv", row.names = FALSE)
write.csv(top_girls, "top_girls.csv", row.names = FALSE)
write.csv(top_girls_colours, "top_girls_colours.csv", row.names = FALSE)
write.csv(new_names_boys, "new_names_boys.csv", row.names = FALSE)
write.csv(new_names_girls, "new_names_girls.csv", row.names = FALSE)
