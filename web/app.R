# Load packages

library(tidyverse)
library(janitor)
library(readxl)
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)
library(broom)

load('data.RDATA')
source('helpers.R')



# Shiny web application UI

ui <- navbarPage('Regional Inequality in Slovak Education',
                 theme = shinytheme('flatly'),
    tabPanel('Regional Inequalities',
        titlePanel('Regional Disparities in Demographic Indicators'),
        sidebarLayout(
            sidebarPanel(
                selectInput('indicator1', "Select a statistic to display",
                            choices = c('Unemployment rate (%)' = 'unemployment_rate',
                                        'Average gross income (in € thousands)' = 'avg_income',
                                        'Population density (logged)' = 'log_dens',
                                        'Total population (in 1000s)' = 'pop_total'),
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
                    selectInput('reg1', 'Overall or regional data',
                                choices = c('All Slovakia' = 'svk', 'Select regions' = 'regs'),
                                selected = 'svk'),
                    conditionalPanel(
                        condition = "input.reg1 == 'regs'",
                        checkboxGroupInput('reg2', 'Select regions',
                                           choices = levels(schools_ec$region))),
                    conditionalPanel(
                        condition = "input.reg1 == 'svk'",
                        selectInput('colind1', 'Color by',
                                    choices = c('None' = 'no',
                                                'Major region' = 'super_region',
                                                'Administrative region' = 'region',
                                                'School type' = 'type',
                                                'Public or private' = 'pub_pri'),
                                    selected = 'no')
                    )
                ),
                mainPanel(
                    plotOutput('schplot')
                )
            )
        )
    ),
    
    tabPanel('Model',
        headerPanel('Visualize simple linear models'),
        fluidRow(
            column(3,
                   selectInput('level3', 'Select school level',
                               choices = c('Primary schools (grade 1-9)' = 'pr',
                                           'Secondary schools (grade 10-13)' = 'hs'),
                               selected = 'pr'),
                   selectInput('scope1', 'County averages or individual schools',
                               choices = c('Counties' = 'county', 'Schools'= 'schools')),
                   selectInput('resp1', 'Select outcome variable',
                               choices = school_model_indicators,
                               selected = 'overall_rating'),
                   radioButtons('expl1', 'Select explanatory variable',
                               choices = c('Average gross income (in € thousands)' = 'avg_income',
                                           'County unemployment rate (%)' = 'unemployment_rate',
                                           'Logged county population density (per km^2)' = 'log_dens',
                                           'Total population (in 1000s)' = 'pop_total',
                                           'Teachers per 100 students' = 'teachers'),
                               selected = 'avg_income')
                ),
            column(6,
                plotOutput('modplot1')
            ),
            column(3,
                tableOutput('tab1')
            )
        )
    ),
    
    tabPanel('About',
        h1('Regional Inequality in Slovak Education', align = "center"),
        fluidRow(column(2), column(8,
            h2('Context'),
                                   
            "Slovakia (a Central European country) operates on 13-year pre-college educational
            system which is typically split into a 9-year long primary school and 4 years of high
            school. In 9th grade, students choose whether to apply to college-preparatory high schools
            that teach a general curriculum or vocational schools which provide practical education for
            a specific job (students from these schools typically don't continue to college).
            Students take two national standardized tests in this time: a native language
            and math test in 9th grade and a leaving examination in the final year of high school.
            Both of these tests report raw percentages as their final outcome, wihtout any curving
            or scoring formulas.",
            
            "There are significant regional disparities between the 79 counties (organized into 8 regions)
            that consitute Slovakia. Cities tend to be significantly wealthier than rural areas,
            and unemployment rates in southeastern and eastern Slovakia remain far above the national average.",
            
            h2('About this project'),
            
            'This project attempts to visualize, map, and find relationships between the performance,
            of schools in various metrics and the socioeconomic conditions that their students and staff
            live in. Importantly, it does not try to uncover causality either way, since the evaluated
            data does not come from a controlled experiment, and in some cases contains limited sample
            sizes. Both directions of causality (and a myriad of potential confounding variables) 
            can be plausibly imagined: students from more advantaged backgrounds might have more resources
            to succeed academically, but better schools might also contribute to an economic upliftment
            in their region. In the future, I would like to include time series data for the relevant
            metrics to observe trends over time.',
            
            h2('Data'),
            
            'The project relies on', tags$a(href = 'http://skoly.sme.sk/metodika/#data',
                                           'data from INEKO,'),
            "an NGO which rates all primary and secondary schools in Slovakia (provided that
            they hava sufficient number of students, meaning that extremely small schools are
            excluded from the data. INEKO's methodology uses a weigted average of several metrics,
            including standardized testing performance, alumni job prospects, and the school's resources.
            Schools are compared against other schools of the same type and a formula is used to produce
            a final 0-10 rating. The socioeconomic indicators that the project looks at come from the 
            Slovak Government's offices:",
            tags$a(href = 'https://bit.ly/2Kw4OLl',
                   'population density data'),
            'and',
            tags$a(href = 'https://bit.ly/2Y4xsv1',
                   'average monthly salary data'),
            'from the Slovak Statistical Office,',
            tags$a(href = 'https://www.upsvr.gov.sk/statistiky/nezamestnanost-mesacne-statistiky/2020.html?page_id=971502',
                   'and unemployment rates'),
            'from the Bureau of Labor. They are available under the',
            tags$a(href = 'http://creativecommons.org/licenses/by/4.0/',
                   'Cerative Commons 4.0 Attribution International'),
            'licence.',
            h2('About me'),
            "My name is Matej Cerman and I'm a Harvard University student hailing from Slovakia. I
            study Applied Math and Economics, hoping to use quantitative approaches to find new insights
            into social science problems. Please contact me with any feedback or suggestions at 
            matej_cerman@college.harvard.edu. You can find the code for this project on",
            tags$a(href = 'https://github.com/matejcerman/gov1005_final_project',
                   'my GitHub.'),
            br(),
            br(),
            br(),
            br(),
            br()
        )
        )
    )
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
       
    output$schplot <- renderPlot({
        if (input$level2 == 'pr'){
            if(input$reg1 == 'svk') {
                if(input$colind1 == 'no') {
                    pr %>%
                        ggplot(aes_string(x = input$indicator3)) +
                        geom_histogram()
                }
                    else if(!input$colind1 == 'no') {
                        pr %>%
                            ggplot(aes_string(x = input$indicator3, fill = input$colind1)) +
                            geom_histogram()
                    }
            }
            else if(input$reg1 == 'regs') {
                req(input$reg2)
                pr %>%
                    filter(region %in% input$reg2) %>%
                    ggplot(aes_string(x = input$indicator3)) +
                    geom_histogram(aes(fill = region)) +
                    facet_wrap(~region)
                }
            }
                    
        else if(input$level2 == 'hs') {
            if(input$reg1 == 'svk') {
                if(input$colind1 == 'no') {
                    hs %>%
                        ggplot(aes_string(x = input$indicator3)) +
                        geom_histogram()
                }
                else if(!input$colind1 == 'no') {
                    hs %>%
                        ggplot(aes_string(x = input$indicator3, fill = input$colind1)) +
                        geom_histogram()
                }
            }
            else if(input$reg1 == 'regs') {
                req(input$reg2)
                hs %>%
                    filter(region %in% input$reg2) %>%
                    ggplot(aes_string(x = input$indicator3)) +
                    geom_histogram(aes(fill = region)) +
                    facet_wrap(~region)
            }
        }
    })
    
    schin3 <- reactive({
        if(input$level3 == 'pr') school_model_indicators[8:13]
        else school_gen_indicators[1:7]
    })
    
    output$modplot1 <- renderPlot({
        if (input$level3 == 'pr') {
            if (input$scope1 == 'county') {
                regional_pr %>%
                    ggplot(aes_string(input$expl1, input$resp1)) +
                    geom_point(aes(color = region), alpha = 0.8, size = 3) +
                    geom_smooth(method = 'lm', se = F)
            }
            else {
                pr %>%
                    ggplot(aes_string(input$expl1, input$resp1)) +
                    geom_point(aes(color = region), alpha = 0.6, size = 1.5) +
                    geom_smooth(method = 'lm', se = F)
            }
        }
        else {
            if (input$scope1 == 'county') {
                regional_hs %>%
                    ggplot(aes_string(input$expl1, input$resp1)) +
                    geom_point(aes(color = region), alpha = 0.8, size = 3) +
                    geom_smooth(method = 'lm', se = F)
            }
            else {
                hs %>%
                    ggplot(aes_string(input$expl1, input$resp1)) +
                    geom_point(aes(color = region), alpha = 0.6, size = 1.5) +
                    geom_smooth(method = 'lm', se = F)
            }
        }
    })
    
    output$tab1 <- renderTable({
        
        if (input$level3 == 'pr') {
            if (input$scope1 == 'county') {
                regional_pr %>%
                    lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                    tidy(conf.int = T) %>%
                    select(term, estimate, std.error)
            }
            else {
                pr %>%
                    lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                    tidy(conf.int = T) %>%
                    select(term, estimate, std.error)
            }
        }
        else {
            if (input$scope1 == 'county') {
                regional_hs %>%
                    lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                    tidy(conf.int = T) %>%
                    select(term, estimate, std.error)
            }
            else {
                hs %>%
                    lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                    tidy(conf.int = T) %>%
                    select(term, estimate, std.error)
            }
        }
    })
    
    observe({
        updateRadioButtons(session, 'indicator2', choices = schin1())
        updateSelectInput(session, 'indicator3', choices = schin2())
        updateSelectInput(session, 'resp1', choices = schin3())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)






