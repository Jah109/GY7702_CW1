# libraries ---------------------------------------------------------------
library(tidyverse)
library(knitr)

# Question 1 --------------------------------------------------------------
# vector survey_responses contains 25 elements 
survey_responses <- c(NA, 5, 4, 4, 5, 2, 4, NA, 6, 3, 5, 7, 1, 5, 5, 5, NA, 5, 
                      2, 4, NA, 3, 5, 5, NA)


# 1.1 ---------------------------------------------------------------------
survey_responses %>%
  na.omit() %>%
  (function(x) x %in% c(1,7))
  
  
  
  na.omit(survey_responses) %in% c(1, 7)



   
 
 






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
  dplyr::arrange(desc(body_mass_g))%>%
  #shows 10 lines 
  print(n=10)

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
  


# question 3------------------------------------------------------------
library(tidyverse)
# 3.1 ---------------------------------------------------------------------
covid_data <- readr::read_csv("covid19_cases_20200301_20201017.csv")

# 3.2 ---------------------------------------------------------------------
Brentwood_complete_covid_data <- covid_data %>%
  dplyr::select(specimen_date, area_name, newCasesBySpecimenDate,
                cumCasesBySpecimenDate) %>%
  dplyr::arrange(specimen_date, area_name) %>%
  tidyr::fill("newCasesBySpecimenDate", "cumCasesBySpecimenDate") %>%
  tidyr::replace_na(list("newCasesBySpecimenDate" = 0,
                         "cumCasesBySpecimenDate"= 0)) %>%
  dplyr::filter(area_name == "Brentwood") %>%
  dplyr::select(-area_name) %>%
  print(n= 8)
  
  
print(Brentwood_complete_covid_data)
 
print(Brentwood_complete_covid_data)
  
across()
# 3.3 ---------------------------------------------------------------------
library(lubridate)
2020-03-03

Brentwood_day_before <- Brentwood_complete_covid_data %>%
  dplyr::mutate(day_before = as.character(ymd(specimen_date -1))) %>%
  dplyr::select(-specimen_date, -cumCasesBySpecimenDate) %>%
  dplyr::rename(newCases_day_before = newCasesBySpecimenDate) 

Brentwood_complete_covid_data_converted <- Brentwood_complete_covid_data %>%
  dplyr::mutate(across(where(is.Date), as.character)) 


Brentwood_covid_development <- dplyr::left_join( Brentwood_day_before,
                                                 Brentwood_complete_covid_data_converted,
                                                by =c("day_before"= "specimen_date")) %>%
  dplyr::mutate((newCasesBySpecimenDate - newCases_day_before)/
  ((newCasesBySpecimenDate + newCases_day_before)/2)*100) 
  tidyr::replace_na(list(percentage_new_cases = 0)) 

# have a think about this percentage change question. 

(percentage_new_cases = 
    ((newCasesBySpecimenDate / newCases_day_before)*100))

print(Brentwood_covid_development, n= 40)





# import data  --------------------------------------------------------------
# import lad19_population data and assigned to a new variable LAD_pop 
LAD_pop<- readr::read_csv("lad19_population.csv")
# import COVID19 case data and assigned to a new variable LAD_covid_cases 
LAD_covid_cases <- readr::read_csv("covid19_cases_20200301_20201017.csv")


# need to rename LAD_pop column lad19_area_name to area_name to match
# LAD_covid_cases table 


# population search -------------------------------------------------------
LAD_selection <- LAD_pop %>%
  dplyr::arrange(area_population, area_code)


# create comparison tables -------------------------------------------------


# Brentwood ---------------------------------------------------------------

Brentwood_covid_data <- covid_data %>%
  dplyr::select(specimen_date, area_name, newCasesBySpecimenDate,
                cumCasesBySpecimenDate) %>%
  dplyr::arrange(specimen_date, area_name) %>%
  tidyr::fill("newCasesBySpecimenDate", "cumCasesBySpecimenDate") %>%
  tidyr::replace_na(list("newCasesBySpecimenDate" = 0,
                         "cumCasesBySpecimenDate"= 0)) %>%
  dplyr::filter(area_name == "Brentwood") %>%
  dplyr::select(-area_name) %>%
  dplyr::rename("Brentwood_Cumulative_cases" = "cumCasesBySpecimenDate", 
                "Brentwood_new_cases" = "newCasesBySpecimenDate" )


# Rossendale --------------------------------------------------------------


Rossendale_covid_data <- covid_data %>%
  dplyr::select(specimen_date, area_name, newCasesBySpecimenDate,
                cumCasesBySpecimenDate) %>%
  dplyr::arrange(specimen_date, area_name) %>%
  tidyr::fill("newCasesBySpecimenDate", "cumCasesBySpecimenDate") %>%
  tidyr::replace_na(list("newCasesBySpecimenDate" = 0,
                         "cumCasesBySpecimenDate"= 0)) %>%
  dplyr::filter(area_name == "Rossendale") %>%
  dplyr::select(-area_name) %>%
  dplyr::rename("Rossendale_Cumulative_cases" = "cumCasesBySpecimenDate", 
                "Rossendale_new_cases" = "newCasesBySpecimenDate")


# table join  -------------------------------------------------------------

Rossendale_Brentwood_Comparison <- dplyr::full_join(Rossendale_covid_data, Brentwood_covid_data) %>%
  dplyr::select(specimen_date, Rossendale_Cumulative_cases, Brentwood_Cumulative_cases) %>%
  dplyr::arrange(specimen_date) %>%
  dplyr::mutate(daily_percentage_difference = (Rossendale_Cumulative_cases - Brentwood_Cumulative_cases)/
                  ((Rossendale_Cumulative_cases +Brentwood_Cumulative_cases)/2)*100)
                  
                  
  
  


  
  











