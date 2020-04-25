library(shiny)
library(shinythemes)
library(sf)
library(leaflet)

source('gather.R')
source('helpers.R')

# Prepare the datasets with summary data

schools_ec <- prep_data()

pr <- schools_ec %>%
    filter(type == 'Základná škola')

hs <- schools_ec %>%
    filter(type == 'Stredná odborná škola' | type == 'Gymnázium')


regional_pr <- pr %>%
    group_by(region, county) %>%
    summarise(
        overall = mean(overall_rating, na.rm = T),
        testovanie9 = mean(testovanie9, na.rm = T),
        t9_sj = mean(t9_sj, na.rm = T),
        t9_m = mean(t9_m, na.rm = T),
        t9_mj = mean(t9_mj, na.rm = T),
        t9_s_ja_sl = mean(t9_s_ja_sl, na.rm = T),
        pop = mean(pop_total),
        dens = mean(pop_density),
        log_dens = log(dens),
        income = mean(avg_wage),
        unempl = mean(unemployment_rate)
    )
    
regional_hs <- hs %>%
    group_by(region, county) %>%
    summarise(
        overall = mean(overall_rating, na.rm = T),
        maturity = mean(maturity, na.rm = T),
        mat_sj = mean(mat_sj, na.rm = T),
        mat_m = mean(mat_m, na.rm = T),
        mat_mj = mean(mat_mj, na.rm = T),
        mat_s_ja_sl = mean(mat_s_ja_sl, na.rm = T),
        mat_ajb1 = mean(mat_ajb1, na.rm = T),
        mat_ajb2 = mean(mat_ajb2, na.rm = T),
        mat_ajc1 = mean(mat_ajc1, na.rm = T),
        pop = mean(pop_total),
        dens = mean(pop_density),
        log_dens = log(dens),
        income = mean(avg_wage),
        unempl = mean(unemployment_rate),
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

# Shiny web application UI

ui <- navbarPage("Performance of Primary and Secondary Schools in Slovakia",
                 
    tabPanel('Overview'),
    
    tabPanel('Regional Inequalities',
             
        sidebarLayout(
            
            sidebarPanel(
                selectInput('indicator1', "Select a statistic to display",
                            choices = c('Average monthly salary (€)' = 'income',
                                           'Unemployment rate (%)' = 'unempl',
                                           'Population density (logged)' = 'log_dens',
                                           'Total population' = 'pop')
                            )
                ),
            
            mainPanel(
                leafletOutput('rin_map')
            )
          )
        ),
    
    navbarMenu('School Performance',
               
        tabPanel('Maps'),
        
        tabPanel('Visualizations')
    ),
    
    tabPanel('Model'),
    
    tabPanel('About')
)

server <- function(input, output) {
    
    output$rin_map <- renderLeaflet({
        
        pal_rin <- colorNumeric(
            palette = "plasma",
            reverse = TRUE,
            domain = pull(geo_hs, input$indicator1))
        
        leaflet(geo_hs) %>%
            addTiles() %>%
            addPolygons(
                stroke = F,
                fillOpacity = 0.9,
                smoothFactor = 0.3,
                fillColor = ~pal_rin(geo_pr %>% pull(input$indicator1)),
                label = ~paste(county, ': ', round(pull(geo_hs, input$indicator1), 2), sep = '')
            ) %>%
            addLegend(
                position = 'bottomleft',
                pal = pal_rin,
                values = ~pull(geo_hs, input$indicator1),
                title = FALSE,
                )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)






