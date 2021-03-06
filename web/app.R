# Load all packages

library(tidyverse)
library(janitor)
library(readxl)
library(shiny)
library(shinythemes)
library(sf)
library(leaflet)
library(broom)
library(scales)

# Load in the data saved by the gather.R file to make all environment objects
# available for the shiny app

load('data.RDATA')

# Load in the objects that serve as menu choices. I stored them in a separate
# file to unclutter the UI code.

source('helpers.R')

# Shiny UI consisting of a menu bar at the top. Using the Flatly theme for now,
# might change to something else in the future

ui <- navbarPage('Regional Inequality in Slovak Education',
                 theme = shinytheme('flatly'),
                 
# First panel contains a sidebar menu which allows the user to select a
# demographic indicator that they want to visualize on a map. The main panel
# shows a colored map of Slovakia for the appropriate statistic
                 
    tabPanel('Regional Inequalities',
        titlePanel('Regional Disparities in Demographic Indicators'),
        sidebarLayout(
            sidebarPanel(
                selectInput('indicator1', "Select a statistic to display",
                            choices = c('Unemployment rate (%)' = 'unemployment_rate',
                                        'Average gross income (in € thousands)' = 'avg_income',
                                        'Population density (per km^2, logged)' = 'log_dens',
                                        'Total population (in 1000s)' = 'pop_total'),
                            selected = 'unempl'
                            ),
                'The map  on the right shows the value of the selected indicator in all 79 
                Slovak counties. Hover over a county to see its name and the exact value.'
                ),
            mainPanel(
                leafletOutput('rin_map')
            )
          )
        ),
    
# Second button on the menu allows the user to select from two sub-panels

    navbarMenu('School Performance',
               
# Sidebar menu lets the user choose between primary and secondary schools
# (dropdown menu) and then which school quality metric they want to visualize
# (buttons). Main panel shown a map of the chosen metric

        tabPanel('Maps',
            titlePanel('Average Educational Outcomes in Each County'),
            sidebarLayout(
                sidebarPanel(
                    selectInput('level1', 'Select school level',
                                choices = c('Primary schools (grade 1-9)' = 'pr',
                                            'Secondary schools (grade 10-13)' = 'hs'),
                                selected = 'pr'),
                    radioButtons('indicator2', 'Select performance indicator',
                                 choices = school_map_indicators),
                    'The map on the right displays the per-county averages of different
                    measures of educational outcomes. You can select whether to look at
                    primary or secondary schools in the menu at the top. Hover over the
                    map to see the name of a county and the average value.'
                ),
                mainPanel(
                    leafletOutput('schin_map')
                )
            )
        ),

# Sidebar menu gives the user several choices. First of all, the user chooses
# between primary and secondary schools, and then a quality metric. If the user
# decides to create a plot for all of Slovakia, the app allows for coloring by
# muliple categories. If the user wants to see specific regions, this is not
# possible (too few data points for meaningful categorization), but checkboxes
# appear, allowing the user to select any combination of the 8 regions

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
                                    selected = 'no'),
                    'View histograms of the distributions of different educational outcomes. You
                    can choose whether to look at primary or secondary schools and also whether to
                    only consider some of the 8 administrative regions.'
                    )
                ),
                
# Main panel shows a histogram with the parameters inputted by the user
                
                mainPanel(
                    plotOutput('schplot')
                )
            )
        )
    ),

# Last data panel shows bivariate models. I'm planning to add models other than
# simple linear regression in the future

    tabPanel('Model',
        headerPanel('Visualize simple linear models'),
        fluidRow(
            
# The user is presented with a menu where they can select between primary and
# secondary schools and then choose whether they want individual datapoints to
# represent individual schools or county averages. After this, the user chooses
# an explanatory variable (socioeconomic factor) and a reponse variable (school
# quality metric)
            
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
                               selected = 'avg_income'),
                   'The scatterplot shows the explanatory variable on the x-axis and the outcome variable 
                   on the y-axis. The coefficients of a simple linear regression model between the two
                   variables are displayed on the right.'
                ),
            
# Output shows a scatterplot with a best-fit line for the chosen variables.
            
            column(6,
                plotOutput('modplot1')
            ),

# Output shows a table with regression coefficients.

            column(3,
                tableOutput('tab1')
            )
        )
    ),
    
# Panel with explanatory text

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
            excluded from the data). INEKO's methodology uses a weigted average of several metrics,
            including standardized testing performance, alumni job prospects, and the school's resources.
            Schools are compared against other schools of the same type and a formula is used to produce
            a final 0-10 rating. The data on schools comes from the 2018/2019 academic year.
            The socioeconomic indicators that the project looks at come from the 
            Slovak Government's offices:",
            tags$a(href = 'https://bit.ly/2Kw4OLl',
                   'population density data (2019)'),
            'and',
            tags$a(href = 'https://bit.ly/2Y4xsv1',
                   'average monthly salary data (2019)'),
            'from the Slovak Statistical Office,',
            tags$a(href = 'https://www.upsvr.gov.sk/statistiky/nezamestnanost-mesacne-statistiky/2020.html?page_id=971502',
                   'and unemployment rates (March 2020)'),
            'from the Bureau of Labor. They are available under the',
            tags$a(href = 'http://creativecommons.org/licenses/by/4.0/',
                   'Cerative Commons 4.0 Attribution International'),
            'licence.',
            'The geospatial data for Slovak counties comes from',
            tags$a(href = 'https://www.diva-gis.org/datadown', 'DIVA-GIS.'),
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

# The server of the shiny app

server <- function(input, output, session) {
    
# Map of the chosen socioeconomic indicator. First, it creates a color pallette
# scaled for the appropriate variable and then applies it to a leaflet map with
# Slovak counties as polygons
    
    output$rin_map <- renderLeaflet({
        
        pal_rin <- colorNumeric(
            palette = "plasma",
            reverse = TRUE,
            domain = pull(geo_hs, input$indicator1))
        
        suf1 <- case_when(
          input$indicator1 == 'unemployment_rate' ~ '%',
          input$indicator1 == 'avg_income' ~ " 000 €",
          input$indicator1 == 'log_dens' ~ "( /km^2, logged)",
          input$indicator1 == 'pop_total' ~ " 000"
        )
        
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
                labFormat = labelFormat(
                  suffix = suf1
                )
            )
    })
    
# Choose appropriate school quality indicators depending on whether the user
# select primary or secondary schools
    
    schin1 <- reactive({
       if(input$level1 == 'pr') school_map_indicators[8:13]
        else if(input$level1 == 'hs') school_map_indicators[1:7]
    })
    
# Render a leaflet county-level map with county averages of school performance
# metrics for primary schools.
    
    output$schin_map <- renderLeaflet({
      
      # Dynamically change the suffix of the legend to have correct units
      
      suf2 <- case_when(
        input$indicator2 == 'overall_rating' ~ '',
        input$indicator2 == 'maturity' ~ '',
        input$indicator2 == 'mat_sj' ~ '%',
        input$indicator2 == 'mat_m' ~ '%',
        input$indicator2 == 'mat_aj' ~ '%',
        input$indicator2 == 'mat_mj' ~ '%',
        input$indicator2 == 'mat_s_ja_sl' ~ '%',
        input$indicator2 == 'testovanie9' ~ '',
        input$indicator2 == 't9_sj' ~ '%',
        input$indicator2 == 't9_m' ~ '%',
        input$indicator2 == 't9_mj' ~ '%',
        input$indicator2 == 't9_s_ja_sl' ~ '%'
      )
        
       if(input$level1 == 'pr') {
         
        req(input$indicator2 %in% colnames(geo_pr))
         
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
                labFormat = labelFormat(
                  suffix = suf2
                )
            )
       }
        
# Render a leaflet county-level map with county averages of school performance
# metrics for secondary schools.
        
        else if(input$level1 == 'hs') {
          
          req(input$indicator2 %in% colnames(geo_hs))
          
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
                    labFormat = labelFormat(
                      suffix = suf2
                    )
                )
        }
    })
      
# Choose appropriate school quality indicators depending on whether the user
# select primary or secondary schools
      
    schin2 <- reactive({
            if(input$level2 == 'pr') school_gen_indicators[9:15]
            else school_gen_indicators[1:8]
        })
    
# Histograms of school performance metrics
    
    output$schplot <- renderPlot({
      
      
# Dynamically change the x-label and the label for fill color
      
      xlab <- case_when(
        input$indicator3 == 'overall_rating' ~ 'Overall school rating (0-10)',
        input$indicator3 == 'maturity' ~ 'Leaving examination preformance rating (0-10)',
        input$indicator3 == 'mat_sj' ~ 'Average leaving exam score: Slovak language (%)',
        input$indicator3 == 'mat_m' ~ 'Average leaving exam score: Math (%)',
        input$indicator3 == 'mat_aj' ~ 'Average leaving exam score: English language (%)',
        input$indicator3 == 'mat_mj' ~ 'Average leaving exam score: Hungarian language  (%)',
        input$indicator3 == 'mat_s_ja_sl' ~ 'Average leaving exam score: Slovak as second language (%)',
        input$indicator3 == 'teachers' ~ 'Teachers per 100 students',
        input$indicator3 == 'testovanie9' ~ 'Grade 9 standardized test performance rating (0-10)' ,
        input$indicator3 == 't9_sj' ~ 'Grade 9 test score: Slovak Language (%)',
        input$indicator3 == 't9_m' ~ 'Grade 9 test score: Math (%)',
        input$indicator3 == 't9_mj' ~ 'Grade 9 test score: Hungarian Language (%)',
        input$indicator3 == 't9_s_ja_sl' ~ 'Grade 9 test score: Slovak as second language (%)'
      )
      
      filllab <- case_when(
        input$colind1 == 'super_region' ~ 'Major region',
        input$colind1 == 'region' ~ 'Administrative region',
        input$colind1 == 'type' ~ 'School type',
        input$colind1 == 'pub_pri' ~ 'Public or private'
      )
      
# If the user wants to see plots for primary schools for all of Slovakia, show
# either a plain histogram or color it by their chosen category
        
        if (input$level2 == 'pr'){
            if(input$reg1 == 'svk') {
                if(input$colind1 == 'no') {
                  pr %>%
                    ggplot(aes_string(x = input$indicator3)) +
                    geom_histogram() +
                    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
                    theme_classic() +
                    labs(
                      x = xlab,
                      y = 'Count'
                    )
                }
                    else if(!input$colind1 == 'no') {
                      pr %>%
                        ggplot(aes_string(x = input$indicator3, fill = input$colind1)) +
                        geom_histogram() +
                        theme_classic() +
                        labs(
                          x = xlab,
                          y = 'Count',
                          fill = filllab
                        )
                    }
            }
            
# If the user wants to see regional plots, color and facet the histograms by
# region after filtering the data for only selected regions
            
            else if(input$reg1 == 'regs') {
              req(input$reg2)
              pr %>%
                filter(region %in% input$reg2) %>%
                ggplot(aes_string(x = input$indicator3)) +
                geom_histogram(aes(fill = region)) +
                facet_wrap(~region) +
                labs(
                  x = xlab,
                  y = 'Count'
                )
            }
        }
        
# If the user wants to see plots for secondary schools for all of Slovakia, show
# either a plain histogram or color it by their chosen category
                    
        else if(input$level2 == 'hs') {
            if(input$reg1 == 'svk') {
                if(input$colind1 == 'no') {
                  hs %>%
                    ggplot(aes_string(x = input$indicator3)) +
                    geom_histogram() +
                    theme_classic() +
                    labs(
                      x = xlab,
                      y = 'Count'
                    )
                }
                else if(!input$colind1 == 'no') {
                  hs %>%
                    ggplot(aes_string(x = input$indicator3, fill = input$colind1)) +
                    geom_histogram() +
                    theme_classic() +
                    labs(
                      x = xlab,
                      y = 'Count',
                      fill = filllab
                    )
                }
            }
            
# If the user wants to see regional plots, color and facet the histograms by
# region after filtering the data for only selected regions
            
            else if(input$reg1 == 'regs') {
                req(input$reg2)
                hs %>%
                    filter(region %in% input$reg2) %>%
                    ggplot(aes_string(x = input$indicator3)) +
                    geom_histogram(aes(fill = region)) +
                    facet_wrap(~region) +
                  labs(
                    x = xlab,
                    y = 'Count'
                  )
            }
        }
    })
    
# Update choices based on whether the user wants to see primary or secondary
# schools
    
    schin3 <- reactive({
        if(input$level3 == 'pr') school_model_indicators[8:13]
        else school_gen_indicators[1:7]
    })
    
# Display scatterplots
    
    output$modplot1 <- renderPlot({
      
# Dynamic x and y labels
      
      xlab <- case_when(
        input$expl1 == 'avg_income' ~ "Average gross income ('000 €)",
        input$expl1 == 'unemployment_rate' ~ 'County unemployment rate (%)',
        input$expl1 == 'log_dens' ~ 'County population density (/km^2, logged)',
        input$expl1 == 'pop_total' ~ "Total county population ('000)",
        input$expl1 == 'teachers' ~ 'Teachers per 100 students'
      )
      
      ylab <- case_when(
        input$resp1 == 'overall_rating' ~ 'Overall school rating (0-10)',
        input$resp1 == 'maturity' ~ 'Leaving examination preformance rating (0-10)',
        input$resp1 == 'mat_sj' ~ 'Average leaving exam score: Slovak language (%)',
        input$resp1 == 'mat_m' ~ 'Average leaving exam score: Math (%)',
        input$resp1 == 'mat_aj' ~ 'Average leaving exam score: English language (%)',
        input$resp1 == 'mat_mj' ~ 'Average leaving exam score: Hungarian language  (%)',
        input$resp1 == 'mat_s_ja_sl' ~ 'Average leaving exam score: Slovak as second language (%)',
        input$resp1 == 'testovanie9' ~ 'Grade 9 standardized test performance rating (0-10)' ,
        input$resp1 == 't9_sj' ~ 'Grade 9 test score: Slovak Language (%)',
        input$resp1 == 't9_m' ~ 'Grade 9 test score: Math (%)',
        input$resp1 == 't9_mj' ~ 'Grade 9 test score: Hungarian Language (%)',
        input$resp1 == 't9_s_ja_sl' ~ 'Grade 9 test score: Slovak as second language (%)'
      )
        
# If the user wants to see primary school data, then show a scatterplot of their
# chosen explanatory and response variable for either individual schools or
# county averages. Add a best-fit line to the data and color the points by
# region

        if (input$level3 == 'pr') {
            if (input$scope1 == 'county') {
              
              req(input$expl1 %in% colnames(regional_pr))
              req(input$resp1 %in% colnames(regional_pr))
              
              regional_pr %>%
                ggplot(aes_string(input$expl1, input$resp1)) +
                geom_point(aes(color = region), alpha = 0.8, size = 3) +
                geom_smooth(method = 'lm', color = 'indianred') +
                scale_x_continuous(breaks = pretty_breaks(n = 8)) +
                labs(
                  x = xlab,
                  y = ylab,
                  color = 'Administrative region'
                ) +
                theme_bw()
            }
            else {
              
              req(input$expl1 %in% colnames(pr))
              req(input$resp1 %in% colnames(pr))
              
              pr %>%
                ggplot(aes_string(input$expl1, input$resp1)) +
                geom_point(aes(color = region), alpha = 0.6, size = 1.5) +
                geom_smooth(method = 'lm', color = 'indianred') +
                scale_x_continuous(breaks = pretty_breaks(n = 8)) +
                labs(
                  x = xlab,
                  y = ylab,
                  color = 'Administrative region'
                ) +
                theme_bw()
            }
        }
        
# If the user wants to see secondary school data, then show a scatterplot of
# their chosen explanatory and response variable for either individual schools
# or county averages. Add a best-fit line to the data and color the points by
# region
        
        else {
            if (input$scope1 == 'county') {
              
              req(input$expl1 %in% colnames(regional_hs))
              req(input$resp1 %in% colnames(regional_hs))
              
              regional_hs %>%
                ggplot(aes_string(input$expl1, input$resp1)) +
                geom_point(aes(color = region), alpha = 0.8, size = 3) +
                geom_smooth(method = 'lm', color = 'indianred') +
                labs(
                  x = xlab,
                  y = ylab,
                  color = 'Administrative region'
                ) +
                theme_bw()
            }
            else {
              
              req(input$expl1 %in% colnames(hs))
              req(input$resp1 %in% colnames(hs))
              
              hs %>%
                ggplot(aes_string(input$expl1, input$resp1)) +
                geom_point(aes(color = region), alpha = 0.6, size = 1.5) +
                geom_smooth(method = 'lm', color = 'indianred') +
                labs(
                  x = xlab,
                  y = ylab,
                  color = 'Administrative region'
                ) +
                theme_bw()
            }
        }
    })
    
# Render a table with regression coefficients
    
    output$tab1 <- renderTable({
        
# Include either school-level or county level data for primary schools, create a
# simple linear model based on the user's selected variables.
        
        if (input$level3 == 'pr') {
            if (input$scope1 == 'county') {
              
              req(input$expl1 %in% colnames(regional_pr))
              req(input$resp1 %in% colnames(regional_pr))
              
              regional_pr %>%
                lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                tidy(conf.int = T) %>%
                select(term, estimate, std.error) %>%
                rename(
                  Variable = term,
                  `Estimated coefficient` = estimate,
                  `Standard error` = std.error
                )
            }
            else {
              
              req(input$expl1 %in% colnames(pr))
              req(input$resp1 %in% colnames(pr))
              
              pr %>%
                lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                tidy(conf.int = T) %>%
                select(term, estimate, std.error) %>%
                rename(
                  Variable = term,
                  `Estimated coefficient` = estimate,
                  `Standard error` = std.error
                )
            }
        }
        
# Include either school-level or county level data for secondary schools, create a
# simple linear model based on the user's selected variables.
        
        else {
            if (input$scope1 == 'county') {
              
              req(input$expl1 %in% colnames(regional_hs))
              req(input$resp1 %in% colnames(regional_hs))
              
              regional_hs %>%
                lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                tidy(conf.int = T) %>%
                select(term, estimate, std.error) %>%
                rename(
                  Variable = term,
                  `Estimated coefficient` = estimate,
                  `Standard error` = std.error
                )
            }
            else {
              
              req(input$expl1 %in% colnames(hs))
              req(input$resp1 %in% colnames(hs))
              
              hs %>%
                lm(as.formula(paste(input$resp1, '~', input$expl1)), data = .) %>%
                tidy(conf.int = T) %>%
                select(term, estimate, std.error) %>%
                rename(
                  Variable = term,
                  `Estimated coefficient` = estimate,
                  `Standard error` = std.error
                )
            }
        }
    })
    
# Function updates the menus of performance indicators based on the user's
# choice of primary or secondary schools.
    
observe({
        updateRadioButtons(session, 'indicator2', choices = schin1())
        updateSelectInput(session, 'indicator3', choices = schin2())
        updateSelectInput(session, 'resp1', choices = schin3())
        })
    
}

# Run the shiny app (yay!) 
shinyApp(ui = ui, server = server)






