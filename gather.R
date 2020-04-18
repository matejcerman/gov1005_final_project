# Load packages

library(tidyverse)
library(janitor)
library(readxl)

# This function allows me to read in csv files compiled by INEKO, the think-tank
# whose data I'm using. It uses international encoding that recognizes special
# signs in the Slovak alphabet (like á, č, ä, ô, etc.) and uses a decimal comma
# instead of a decimal point

read_slovak_csv <- function(path, ...) {
  read_delim(file = path, 
             delim = ";", 
             escape_double = FALSE, 
             locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"), 
             trim_ws = TRUE,
             ...) %>%
    clean_names()
}

# Load in the ratings of all primary and high schools while translating column
# names to make the analysis more legible

school_ratings <- read_slovak_csv("raw_data/INEKO/recent/celkove_hodnotenie_2018-19.csv",
                                      col_types = 'dffffffccffddddddddddddd') %>%
  rename(id = id,
         region = kraj,
         county = okres,
         school_board = zriadovatel,
         school_category = druh_skoly,
         language = jazyk,
         type = typ_skoly,
         name = nazov,
         street = ulica,
         town = obec,
         zip_code = psc,
         overall_rating = celkove_hodnotenie,
         math = matematika,
         first_language = vyucovaci_jazyk,
         foreign_languages = cudzie_jazyky,
         special_achievements = mimoriadne_vysledky,
         alumni_unemployment = nezamestnanost_absolventov,
         inspection_results = vysledky_inspekcie,
         competition_results = ucasti_na_sutaziach,
         college_admissions = prijimanie_na_vs,
         teachers = pedagogicky_zbor,
         financial_resources = financne_zdroje)

# Load in the two forms of economic data for all counties and manipulate the
# county names so that they match. I also remove iformation about regions that
# are not counties from the unemployment dataset

wages <- read_slovak_csv("raw_data/Government/wages.csv",
                             skip = 3,
                             col_names = c("county_broken", "type", "avg_wage"),
                             col_types = 'ffd') %>%
  select(county, avg_wage) %>%
  mutate(county = str_remove(county_broken, pattern = "Okres "))
  

unemployment <- read_xlsx("raw_data/Government/unemployment.xlsx",
                              sheet = 3,
                              skip = 9) %>%
  clean_names() %>%
  rename(county = x1,
         unemployment_rate = x15) %>%
  select(county, unemployment_rate) %>%
  filter(!str_detect(county, "kraj"),
         !str_detect(county, 'Slovensko'),
         !str_detect(county, 'Pozn'))

# Join the two datasets into one tibble that will store both kinds of economic
# data

ec_data <- unemployment %>%
  bind_cols(wages) %>%
  mutate(county = as_factor(county)) %>%
  select(-county1)

# Change the factor levels for county names to remove typos and make sure they
# match county names in the economic data tibble

levels(school_ratings$county) <- levels(ec_data$county)

# Join the school ratings with economic data to prepare the full dataset for
# analysis

schools_ec <- school_ratings %>%
  full_join(ec_data, by = "county")
