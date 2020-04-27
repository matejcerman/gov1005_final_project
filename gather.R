# Load packages

library(tidyverse)
library(janitor)
library(readxl)
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)
library(broom)

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

return(schools_ec)
}

# Prepare the datasets with summary data

schools_ec <- prep_data()

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
    ),
    mat_aj = rowMeans(
      select(., mat_ajb1, mat_ajb2, mat_ajc1),
      na.rm = T
    ),
    avg_wage = 12*avg_wage/1000,
    pop_total = pop_total/1000,
    log_dens = log(pop_density)
  ) %>%
  rename(avg_income = avg_wage)

levels(schools_ec$region)[3] <- 'Trenčiansky'
levels(schools_ec$type)[c(2,3,4)] <- c('Primary', 'College preparatory', 'Vocational')

pr <- schools_ec %>%
  filter(type == 'Primary')

hs <- schools_ec %>%
  filter(type == 'Vocational' | type == 'College preparatory')


regional_pr <- pr %>%
  group_by(region, county) %>%
  summarise(
    overall_rating = mean(overall_rating, na.rm = T),
    testovanie9 = mean(testovanie9, na.rm = T),
    t9_sj = mean(t9_sj, na.rm = T),
    t9_m = mean(t9_m, na.rm = T),
    t9_mj = mean(t9_mj, na.rm = T),
    t9_s_ja_sl = mean(t9_s_ja_sl, na.rm = T),
    teachers = mean(teachers, na.rm = T),
    pop_total = mean(pop_total),
    pop_density = mean(pop_density),
    log_dens = mean(log_dens),
    avg_income = mean(avg_income),
    unemployment_rate = mean(unemployment_rate)
  )

regional_hs <- hs %>%
  group_by(region, county) %>%
  summarise(
    overall_rating = mean(overall_rating, na.rm = T),
    maturity = mean(maturity, na.rm = T),
    mat_sj = mean(mat_sj, na.rm = T),
    mat_m = mean(mat_m, na.rm = T),
    mat_mj = mean(mat_mj, na.rm = T),
    mat_s_ja_sl = mean(mat_s_ja_sl, na.rm = T),
    mat_aj = mean(mat_aj, na.rm = T),
    mat_ajb1 = mean(mat_ajb1, na.rm = T),
    mat_ajb2 = mean(mat_ajb2, na.rm = T),
    mat_ajc1 = mean(mat_ajc1, na.rm = T),
    teachers = mean(teachers, na.rm = T),
    pop_total = mean(pop_total),
    pop_density = mean(pop_density),
    log_dens = mean(log_dens),
    avg_income = mean(avg_income),
    unemployment_rate = mean(unemployment_rate)
  )

# Create datasets for mapping

create_mapping_data <- function(data_by_county) {
  geo <- st_read('raw_data/shapefiles/SVK_adm2.shp') %>%
    mutate(NAME_2 = as.character(NAME_2)) %>%
    mutate(NAME_2 = case_when(
      NAME_2 == 'Bytca' ~ 'Bytča',
      NAME_2 == 'Cadca' ~ 'Čadca',
      NAME_2 == 'Turcianske Teplice' ~ 'Turčianske Teplice',
      NAME_2 == 'Lucenec' ~ 'Lučenec',
      NAME_2 == 'Šala' ~ 'Šaľa',
      NAME_2 == 'Topolcany' ~ 'Topoľčany',
      NAME_2 == 'Levoca' ~ 'Levoča',
      NAME_2 == 'Stará Lubovna' ~ 'Stará Ľubovňa',
      NAME_2 == 'Trencín' ~ 'Trenčín',
      NAME_2 == 'Pieštany' ~ 'Piešťany',
      NAME_2 == 'Velký Krtíš' ~ 'Veľký Krtíš',
      NAME_2 == 'Rožnava' ~ 'Rožňava',
      NAME_2 == 'Vranov nad Toplou' ~ 'Vranov nad Topľou',
      NAME_2 == 'Košice-okolie' ~ 'Košice - okolie',
      TRUE ~ NAME_2
    )) %>%
    mutate(county = as.factor(NAME_2)) %>%
    full_join(data_by_county, by = 'county')
  return(geo)
}

geo_pr <- create_mapping_data(regional_pr)
geo_hs <- create_mapping_data(regional_hs)