library(shiny)
library(shinythemes)
library(sf)
library(leaflet)

source('gather.R')
source('helpers.R')

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
        )
    )

levels(schools_ec$region)[3] <- 'Trenčiansky'

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
        mat_aj = mean(mat_aj, na.rm = T),
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
             
        titlePanel('Regional Disparities in Demographic Indicators'),
             
        sidebarLayout(
            
            sidebarPanel(
                selectInput('indicator1', "Select a statistic to display",
                            choices = c('Unemployment rate (%)' = 'unempl',
                                        'Average monthly salary (€)' = 'income',
                                        'Population density (logged)' = 'log_dens',
                                        'Total population' = 'pop'),
                            selected = 'unempl'
                            )
                ),
            
            mainPanel(
                leafletOutput('rin_map')
            )
          )
        ),
    
    navbarMenu('School Performance',
               
        tabPanel('Maps',
            titlePanel('Average Educational Outcomes in Each County'),
            sidebarLayout(
                sidebarPanel(
                    selectInput('level1', 'Select school level',
                                choices = c('Primary schools (grade 1-9)' = 'pr',
                                            'Secondary schools (grade 10-13)' = 'hs'),
                                selected = 'pr'),
                    radioButtons('indicator2', 'Select performance indicator',
                                 choices = school_map_indicators)
                ),
                mainPanel(
                    leafletOutput('schin_map')
                )
            )
        ),

        tabPanel('Visualizations',
            titlePanel('Distributions of Educational Outcomes'),
            sidebarLayout(
                sidebarPanel(
                    selectInput('level2', 'Select school level',
                                choices = c('Primary schools (grade 1-9)' = 'pr',
                                            'Secondary schools (grade 10-13)' = 'hs'),
                                selected = 'pr'),
                    selectInput('indicator3', 'Select performance indicator',
                                choices = school_gen_indicators,
                                selected = 'overall_rating'),
                    checkboxGroupInput('reg1', 'Select regions',
                                       choices = c('All of Slovakia' = "svk", levels(schools_ec$region))),
                    conditionalPanel(
                        condition = "input.reg1 == 'svk'",
                        selectInput('colind1', 'Color by',
                                    choices = c('none', 'lol'))
                    )
                ),
                mainPanel(
                    
                )
            )
        )
    ),
    
    tabPanel('Model'),
    
    tabPanel('About')
)

server <- function(input, output, session) {
    
    output$rin_map <- renderLeaflet({
        
        pal_rin <- colorNumeric(
            palette = "plasma",
            reverse = TRUE,
            domain = pull(geo_hs, input$indicator1))
        
        leaflet(geo_hs) %>%
            addTiles() %>%
            addPolygons(
                stroke = T,
                weight = 0.5,
                fillOpacity = 0.9,
                smoothFactor = 0.3,
                fillColor = ~pal_rin(geo_pr %>% pull(input$indicator1)),
                label = ~paste(county, ': ', round(pull(geo_hs, input$indicator1), 2), sep = ''),
                highlightOptions = highlightOptions(color = "white", weight = 0.8,
                                         bringToFront = TRUE)
            ) %>%
            addLegend(
                position = 'bottomleft',
                pal = pal_rin,
                values = ~pull(geo_hs, input$indicator1),
                title = F,
                )
    })
    
    schin1 <- reactive({
       if(input$level1 == 'pr') school_map_indicators[8:13]
        else if(input$level1 == 'hs') school_map_indicators[1:7]
    })
    
    output$schin_map <- renderLeaflet({
        
       if(input$level1 == 'pr') {
        pal_schin <- colorNumeric(
            palette = "YlGn",
            reverse = F,
            domain = pull(geo_pr, input$indicator2)
        )
        leaflet(geo_pr) %>%
            addTiles() %>%
            addPolygons(
                stroke = T,
                color = 'black',
                weight = 0.5,
                opacity = 1,
                fillOpacity = 0.9,
                smoothFactor = 0.3,
                fillColor = pal_schin(geo_pr %>% pull(input$indicator2)),
                label = ~paste(county, ': ', 
                               round(pull(geo_pr, input$indicator2), 2), sep = ''),
                highlightOptions = highlightOptions(color = "white", weight = 0.8,
                                                    bringToFront = TRUE)
            ) %>%
            addLegend(
                position = 'bottomleft',
                pal = pal_schin,
                values = ~pull(geo_pr, input$indicator2),
                title = F,
            )
       }
        
        else if(input$level1 == 'hs') {
            pal_schin <- colorNumeric(
                palette = "YlOrRd",
                reverse = F,
                domain = pull(geo_hs, input$indicator2)
            )
            leaflet(geo_hs) %>%
                addTiles() %>%
                addPolygons(
                    stroke = T,
                    color = 'black',
                    weight = 0.5,
                    opacity = 1,
                    fillOpacity = 0.9,
                    smoothFactor = 0.3,
                    fillColor = pal_schin(geo_hs %>% pull(input$indicator2)),
                    label = ~paste(county, ': ', 
                                   round(pull(geo_hs, input$indicator2), 2), sep = ''),
                    highlightOptions = highlightOptions(color = "white", weight = 0.8,
                                                        bringToFront = TRUE)
                ) %>%
                addLegend(
                    position = 'bottomleft',
                    pal = pal_schin,
                    values = ~pull(geo_hs, input$indicator2),
                    title = F,
                )
        }
    })
        
       schin2 <- reactive({
            if(input$level2 == 'pr') school_gen_indicators[9:15]
            else school_gen_indicators[1:8]
        })
        
    
    observe({
        updateRadioButtons(session, 'indicator2', choices = schin1())
        updateSelectInput(session, 'indicator3', choices = schin2())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)






