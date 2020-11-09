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