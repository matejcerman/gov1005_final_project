# Load packages

library(tidyverse)
library(janitor)
library(readxl)

prep_data <- function() {

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
# names to make the analysis more legible. Also load in the actual indicators
# that make up the final rating for each school and its geographical coordinates

school_ratings <- read_slovak_csv("raw_data/celkove_hodnotenie_2018-19.csv",
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

school_indicators <- read_slovak_csv(
  "raw_data/udaje_2018-19.csv",
  col_types = 'dffffffccffdddddddddddddddddddddddddddddddddd'
  ) %>%
  select(-id, -kraj, -okres, - zriadovatel, - druh_skoly, -jazyk,
         -typ_skoly, -nazov, -ulica, -obec, -psc)

school_coords <- read_slovak_csv(
  'raw_data/zoznam_skol.csv',
  
  col_types = 'dffffffccffdddddddccccdfffff'
) %>%
  select(sur_x, sur_y) %>%
  rename(
    lng = sur_x,
    lat = sur_y
  )

# Load in the two forms of economic data for all counties and manipulate the
# county names so that they match. I also remove iformation about regions that
# are not counties from the unemployment dataset

density <- read_slovak_csv('raw_data/density.csv',
                           col_names = c('county', 'indicator', 'value'),
                           col_types = 'fcd',
                           skip = 1
                           ) %>%
  mutate(county = str_remove(county, 'Okres '))

density$indicator[seq(1, nrow(density), 2)] <- 'pop_density'
density$indicator[seq(2, nrow(density), 2)] <- 'pop_total'

for (i in 1:nrow(density)) {
  if (is.na(density[i,1])) {
    density[i,1] <- density[i-1,1]
  }
}

density <- density %>%
  pivot_wider(names_from = 'indicator', values_from = 'value')

wages <- read_slovak_csv("raw_data/wages.csv",
                             skip = 3,
                             col_names = c("county_broken", "type", "avg_wage"),
                             col_types = 'ffd') %>%
  select(county_broken, avg_wage) %>%
  mutate(county = str_remove(county_broken, pattern = "Okres ")) %>%
  select(county, avg_wage)
  
wages_density <- wages %>%
  full_join(density, by = 'county')

unemployment <- read_xlsx(
  "raw_data/unemployment.xlsx",
  sheet = 3,
  skip = 9,
  col_names = c("x1", "x2", "x3", "x4", "x5",
                "x6", "x7", "x8", "x9", "x10",
                "x11", "x12", "x13", "x14", "x15")
  ) %>%
  rename(county = x1,
         unemployment_rate = x15) %>%
  select(county, unemployment_rate) %>%
  filter(!str_detect(county, "kraj"),
         !str_detect(county, 'Slovensko'),
         !str_detect(county, 'Pozn'))

# Join the two datasets into one tibble that will store both kinds of economic
# data

ec_data <- unemployment %>%
  bind_cols(wages_density) %>%
  mutate(county = as_factor(county)) %>%
  select(-county1)

# Change the factor levels for county names to remove typos and make sure they
# match county names in the economic data tibble

levels(school_ratings$county) <- levels(ec_data$county)

# Join the school information with economic data to prepare the full dataset for
# analysis

schools_ec <- school_ratings %>%
  full_join(ec_data, by = "county") %>%
  bind_cols(school_indicators) %>%
  bind_cols(school_coords)

schools_ec <- schools_ec %>%
  mutate(
    super_region = case_when(
      region == 'Banskobystrický' | region == 'Žilinský' ~ 'Central',
      region == 'Košický' | region == 'Prešovský' ~ 'Eastern',
      TRUE ~ 'Western'
    ),
    pub_pri = case_when(
      school_board %in% c("Krajský úrad, Okresný úrad",
                          "Obec",
                          "Samosprávny kraj") ~ 'Public',
      school_board %in% c("Súkromník",
                          "Cirkev, cirkevné spoloèenstvo",
                          "Obèianske združenia") ~ 'Private',
      TRUE ~ 'Misc'
    )
  )

return(schools_ec)
}