# libraries ---------------------------------------------------------------
library(tidyverse)
library(knitr)

# Question 1 --------------------------------------------------------------
# vector survey_responses contains 25 elements 
survey_responses <- c(NA, 3, 4, 4, 5, 2, 4, NA, 6, 3, 5, 4, 0, 5, 2, 5, NA, 5, 
                      2, 4, NA, 3, 3, 5, NA)


# 1.1 ---------------------------------------------------------------------
survey_responses_NA_ommited <- survey_responses %>%
  na.omit()
 
survey_responses <- c(NA, 3, 4, 4, 5, 2, 4, NA, 6, 3, 5, 4, 0, 5, 2, 5, NA, 5, 
                      2, 4, NA, 3, 3, 5, NA)





# 1.2 ---------------------------------------------------------------------
which(survey_responses %in% 5:7)

# Question 2 --------------------------------------------------------------
# 2.1 ---------------------------------------------------------------------
# loads the tidyverse
library(tidyverse)
#loads the knitr 
library(knitr)
# installs the data palmerpenguins 
install.packages("palmerpenguins")
#loads the data palmerpenguins
library(palmerpenguins)

# 2.2 ---------------------------------------------------------------------
# Starts from the entire dataset
palmerpenguins::penguins %>%
  # Selects only the necessary columns
  dplyr::select(species, island, bill_length_mm, body_mass_g
  ) %>%
  # Retain only rows representing the Gentoo species 
  dplyr::filter(species == "Gentoo"
  ) %>%
  # Sort by descending body mass in g
  dplyr::arrange(desc(body_mass_g)) 

# 2.3 ---------------------------------------------------------------------
# Starts from the entire data set
palmerpenguins::penguins %>%
  # Selects only the necessary columns
  dplyr::select(bill_length_mm, island) %>%
  # Grouped by island
  dplyr::group_by(island) %>%
  # Drops rows containing NAs in the bill_length_mm column
  # otherwise the mean function will return NA
  dplyr::filter(!is.na(bill_length_mm)) %>%
  # Calculates the average of bill_length_mm
  dplyr::summarise(average_bill_length = mean(bill_length_mm)) %>%
  # Ordered by descending average_bill_length
  dplyr::arrange(desc(average_bill_length)) %>%
  # kable improves tibble format 
  knitr::kable()

# 2.4 ---------------------------------------------------------------------
# Starts from the entire data set
palmerpenguins::penguins %>%
  # Selects only the necessary columns
  dplyr::select(species, bill_length_mm, bill_depth_mm) %>%
  # Grouped by species
  dplyr::group_by(species) %>%
  # Drops rows containing NAs in the bill_length_mm column
  # otherwise the mean function will return NA
  dplyr::filter(!is.na(bill_length_mm))%>%
  # Drops rows containing NAs in the bill_depth_mm column
  # otherwise the mean function will return NA
  dplyr::filter(!is.na(bill_depth_mm)) %>%
  # Calculates the bill length to bill depth proportion  
  dplyr::summarise(Proportion_Bill_length_to_depth = 
                     (bill_length_mm/bill_depth_mm)) %>%
  # using summariase again the minimum, median and maximum for each species can be calculated 
  dplyr::summarise(min(Proportion_Bill_length_to_depth), 
                   median(Proportion_Bill_length_to_depth),
                   max(Proportion_Bill_length_to_depth)) %>%
  # Using the function kable formats the table 
  knitr::kable()
  

# Question 3 --------------------------------------------------------------

# 3.1 ---------------------------------------------------------------------
#using readr (part of tidyverse)
library(readr)
# reads the .CSV file with the correct directory
# Imports covid19_cases_20200301_20201017.csv and assings to a new variable
#covid_data 
covid_data <-readr::read_csv("covid19_cases_20200301_20201017.csv")

# 3.2 ---------------------------------------------------------------------
# create a complete table containg a row for each day and area, replace Na
# with the value available for the previous date
# Resulting table will be stored in the new variable 
# brentwood_complete_covide_data 
Brentwood_complete_covid_data <-covid_data %>%
  #selects extracts wanted columns 
  dplyr::select(specimen_date, area_name, newCasesBySpecimenDate, 
                cumCasesBySpecimenDate)%>%
  # group by specimen_data & area_name leads to each area_name having a row 
  # per specimen data  
  dplyr::group_by(specimen_date, area_name) %>%
  # mutates specimen_date from date to character value 

  # tidyr :: fill replace NA values with the values from the previous row 
  # default direction is down
  tidyr::fill(newCasesBySpecimenDate, cumCasesBySpecimenDate) %>%
  # replace_na replaces any reamining NA with 0 
  tidyr::replace_na()%>%
  # dplyr :: filter subsets the area_name to Bentwood 
  dplyr::filter(area_name == "Brentwood")%>%
  # converting to a data.frame as initally when trying to drop area_name using
  # dplyr::select (-area_number) got an error message 'adding missing grouping 
  # variables error: area_number so I converted it to a data frame
  data.frame()%>%
  # then drop area_name using select 
  dplyr::select(-area_name) %>%
  # converts specimen_date to character to enable table join in question 3.3.
  # Then converted back to a tibble 
  as_tibble() 

print(Brentwood_complete_covid_data)
# 3.3 ---------------------------------------------------------------------

# load library lubridate
library("lubridate")

# start from copy Brentwood_day_before 
Brentwood_day_before <- Brentwood_complete_covid_data %>%
  # mutate specimen_date using lubridate 
  # format of ymd is specified then - 1 day 
  # as.character converts date into character string 
  dplyr::mutate(day_before = as.character(ymd(specimen_date - 1 ))) %>%
  # drop specimen_date and cumCasesBySpecimenDate columns 
  dplyr::select(-specimen_date, -cumCasesBySpecimenDate) %>%
  # Rename newCasesBySpecimenDate to newCases_day_before 
  dplyr::rename(newCases_day_before = newCasesBySpecimenDate) 
# join Brentwood_day_before with Brentwood_complete_covid_data
# where specimen_date is equal to day_before using inner_join
 
 
Brentwood_complete_covid_data %>%
  dplyr::mutate(specimen_date = as.character(specimen_date))
  

# join Brentwood_day_before with Brentwood_complete_covid_data
# where specimen_date is equal to day_before using left_join
Brentwood_covid_development <-dplyr::left_join(Brentwood_day_before, 
                                               Brentwood_complete_covid_data,
                                               by =c("day_before"= 
                                                       "specimen_date")) %>%
  # new column containing the number of new cases as a percentage of the number
  # of new  cases of the day before 
  dplyr::mutate(percentage_new_cases = 
                  ((newCasesBySpecimenDate / newCases_day_before)*100)) 


ggplot(Brentwood_covid_development, mapping = aes(x = day_before, 
                                                  y = cumCasesBySpecimenDate)) +  
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))



# question 4.1 --------------------------------------------------------------
# import lad19_population data and assigned to a new variable LAD_pop 
LAD_pop<- readr::read_csv("lad19_population.csv")
# import COVID19 case data and assigned to a new variable LAD_covid_cases 
LAD_covid_cases <- readr::read_csv("covid19_cases_20200301_20201017.csv")


# need to rename LAD_pop column lad19_area_name to area_name to match
# LAD_covid_cases table 
covid_cases_lad_pop <- dplyr::rename(LAD_pop,area_name = lad19_area_name) %>%
# table join between LAD_pop and LAD_covid_cases 
dplyr::left_join(LAD_covid_cases, LAD_pop, by = "area_name")





