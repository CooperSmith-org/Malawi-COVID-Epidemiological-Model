##------------------------------------------------##
##--Updated version: 19 November, 2020------------##
##--In this version, the model run for 1 year----##
##--or 90 days after today-----------------------##
##-----------------------------------------------##

##--Libraries----
library(shinydashboard)
library(shiny)
library(shinyFeedback)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(tidyverse)
library(scales)
library(htmlwidgets)
library(jsonlite)

source('../epi_stepwise.R')

##--Loading data frames for baseline simulations 

n_days <- max(365, difftime((today() + days(90)), as.Date("2020-04-01"), units = "days"))

df_params <- read_csv('../inputs/params_inits_template.csv', col_types=cols())
df_distancing <- read_csv('../inputs/reductionScenarios/current.csv', col_types=cols())
df_masking <- read_csv('../inputs/masking/masking_compliance.csv', col_types=cols())
df_locations <- read_csv('../inputs/MW COVID Inputs.csv', col_types=cols())
df_seed <- read_csv('../inputs/simulation-seeddates-ta-20200910.csv', col_types=cols())

hosp_time <- 1 / df_params$tau
crit_time <- 1 / df_params$tau2

# Add date to existing masking and distancing files and write

masking = tibble::rowid_to_column(df_masking, 'time')
masking$date = as.Date("2020-04-01") + (masking$time - 1)
write.csv(masking, 'out/masking_with_date.csv')

current = tibble::rowid_to_column(df_distancing, 'time')
current$date = as.Date("2020-04-01") + (current$time - 1)
write.csv(current, 'out/distancing_with_date.csv')

#masking <- read_csv("masking-mar2021.csv", col_types=cols()) #read_csv("masking_with_date.csv")
#current <- read_csv("distancing-mar2021.csv", col_types = cols(date = col_date(format = "%Y-%m-%d"))) #read_csv("current_with_date.csv", 

districts_names <- read_csv("in/districts_names.csv", col_types=cols())
ta_names <- read_csv("in/tas_names.csv", col_types=cols())
tas_names2 <- read_csv("in/tas_names2.csv", col_types=cols())
df_country_dash <- read_csv('in/saved-initial-dashboard-state.csv', col_types=cols()) #read_csv("df_country_dash_initial.csv")

# Simulation function
runModel <- function(df_distancing, df_masking, df_locations, df_params, df_seed) {
  model_results <- run_stepwise(df_params, df_locations, df_masking, df_distancing, df_seed, n_days)
  
  df_country <- model_results$country
  df_district <- model_results$district
  df_ta <- model_results$ta
  
  ##--Adding dates
  ##--Country
  df_country_spread <- spread(df_country, key = State, value = People) 
  df_country_spread$date <- seq(from = as.Date("2020-04-01"),
                                by = "day", length.out = n_days)
  
  ##--District
  df_district_spread <- spread(df_district, key = State, value = People) 
  districts <- df_district_spread$Lvl3 %>% unique()
  df_district_spread$date <- rep(seq(from = as.Date("2020-04-01"),
                                     by = "day", length.out = n_days), length(districts))
  
  ##--TA
  df_ta2 <- df_ta 
  df_ta2$date <- rep(seq(from = as.Date("2020-04-01"), 
                         by = "day", length.out = n_days), dim(df_ta)[1]/n_days)  
  
  ##--Organizing the outputs in a list
  list_output_sim <- list(country = df_country_spread, district = df_district_spread, ta = df_ta2)
  return(list_output_sim)
}

## 1. Header----------------------------
header <- dashboardHeader()


## 2. Sidebar----------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",
             tabName = "home",
             icon = icon('dashboard')
    ),
    menuItem("User guide",
             tabName = "user_guide",
             icon = icon('user')),
    menuItem("Technical infrastructure guide",
             tabName = "technical_guide",
             icon = icon("keyboard"))
  )##--end of sidebarMenu
)##--end of sidebar

## 3. Body----------------------------
body <- dashboardBody(
  tabItems(
    ##--Home
    tabItem(tabName = "home",
            fluidRow(
              column(width =11, offset = 0.5,
                     h1(strong("Epidemiological Model for COVID-19 - Malawi"))
              )
            ),
            ##--3.1 Plots UI----------------
            fluidRow(
              br(),
              br(),
              column(width = 11, offset = 0.75,
                     uiOutput("national_title"),
                     uiOutput("district_title"),
                     uiOutput("ta_title"),
              ),
              br(),
              
              column(width = 7, 
                     ##--Widgets UI------------
                     uiOutput("widgets_national"),
                     uiOutput("widgets_district"),
                     uiOutput("widgets_tas"),
                     #"Plots"
                     column(width = 6,
                            uiOutput("national_ui"),
                            uiOutput("district_ui_plot1"),
                            uiOutput("ta_plot1")
                     ),
                     column(width = 6,
                            uiOutput("national_ui2"),
                            uiOutput("district_ui_plot2"),
                            uiOutput("ta_plot2")
                     ),
                     
                     column(width = 6,
                            br(),
                            br(),
                            uiOutput("national_ui3"),
                            uiOutput("district_ui_plot3"),
                            uiOutput("ta_plot3")
                     ),
                     column(width = 6,
                            br(),
                            br(),
                            uiOutput("national_ui4"),
                            uiOutput("district_ui_plot4"),
                            uiOutput("ta_plot4")
                     )#--end column 6
              ),
              column(width=5,
                     #"Police levers"
                     #--3.2 Policy Levers-------
                     h2(strong("Policy Levers")),
                     tags$hr(),
                     column(width = 4.0, h5(strong("% Masking")),
                            
                            tags$h5("Current: 15 %"),
                            shinyFeedback::useShinyFeedback(),
                            numericInput('mask_perc', 
                                         label = "New",
                                         value = 15, 
                                         min = 15,
                                         max = 100),
                            textOutput("warning_masking"),
                            shinyFeedback::useShinyFeedback(),
                            uiOutput("masking_intervention"),
                            textOutput("warning_mask_days")
                     ),
                     column(width = 4.0, h5(strong("% Physical Distancing")),
                            tags$h5(paste0("Current: ", 100*current$reduc[which(current$date==lubridate::today())]),"%"),
                            shinyFeedback::useShinyFeedback(),
                            numericInput('distancing_perc',
                                         label = "New",
                                         value = 8,
                                         min = 8,
                                         max = 100),
                            textOutput("warning_distancing"),
                            shinyFeedback::useShinyFeedback(),
                            uiOutput("distancing_intervention"),
                            textOutput("warning_dist_days")
                     ),
                     column(width = 4,
                            #--Time Horizon Projection
                            shinyFeedback::useShinyFeedback(),
                            numericInput('projection', 'End of Model (days after today)',
                                         value = 90,
                                         min = 1, 
                                         max = 90,
                            ),
                            textOutput("stop_function"),
                            
                            #tags$p("The user may select up to 90 days."),
                            
                            
                            
                     ),
                     
                     column(width = 6, offset = 3,
                            actionButton("runreportButton", strong("Run Report"),
                                         icon = icon("redo"),
                                         style = "color: white;
                                                        font-size:120%;
                                                 background-color: #009933; 
                                                 position: relative;
                                                 left: 3%;
                                                 height: 45px;
                                                 width: 277px;
                                                 text-align:center;
                                                 text-indent: -2px;
                                                 border-radius: 6px;
                                                 border-width: 2px")
                     ),
                     ##--Display Options
                     fluidRow(
                       column(width = 12,
                              h2(strong("Display Options")),
                              br(),
                              column(width = 4,
                                     ##--Set x axis for plot
                                     sliderInput('begin_plot', 'Start of Model (days before today)',
                                                 min = 15, 
                                                 max = as.numeric(difftime(today(), as.Date("2020-04-01"), units = "days")), 
                                                 value = 15, 
                                                 step = 1)    
                              ),
                              column(width = 4, 
                                     ##--Level of interest 
                                     selectInput('level', 'Please select the level of interest',
                                                 choices = c("National", "District", "TA")),
                                     
                              ),
                              column(width = 4,
                                     ##
                                     uiOutput("district_ui"),
                                     uiOutput("district_ui2"),
                                     uiOutput("ta_ui")
                                     ##
                              )
                              
                       )
                     ),
                     ##--Fixed parameters
                     fluidRow(column(width = 12,
                                     h2(strong("Fixed Model Parameters")),
                                     tags$hr(),
                                     column(width = 3,
                                            tags$p(h5(strong("R0:"), "1.9")),
                                            tags$p(h5(strong("Infectious Time (Days):"), "7")),
                                            tags$p(h5(strong("Hospitalized Time (Days):"), "4")),
                                            tags$p(h5(strong("ICU Time (Days):"), "8"))
                                     ),
                                     column(width = 3,
                                            tags$p(h5(strong("Hospitalized Rate of Infected:"))),
                                            tags$p(h5(em("Pediatrics (<20):", "0.0090%"))),
                                            tags$p(h5(em("Adults (20-49):", "1.2%"))),
                                            tags$p(h5(em("Elderly (50+):", "5.5%")))
                                     ),
                                     column(width = 3,
                                            tags$p(h5(strong("ICU Risk Among Hospitalized:"))),
                                            tags$p(h5(em("Pediatrics (<20):"), "5.0%")),
                                            tags$p(h5(em("Adults (20-49):"), "14%")),
                                            tags$p(h5(em("Elderly (50+):"), "28%"))
                                     ),
                                     
                                     column(width = 3,
                                            tags$p(h5(strong("Fatality Rate of ICU:"))),
                                            tags$p(h5(em("Pediatrics (<20):", "9.0%"))),
                                            tags$p(h5(em("Adults (20-49):", "20%"))),
                                            tags$p(h5(em("Elderly (50+):", "59%")))
                                     ))
                     ),
              )
            ),#--end of fluid row
            
            fluidRow(
              column(width = 11, offset = 0.75,
                     ##
                     
                     column(width = 5,
                            column(width = 12,
                                   offset = 4,
                                   
                            )),#--end column 5
              )#--end of column 11
            ),#--end of fluidRow
            ## Reduction tables
            fluidRow(
              column(width = 11, offset = 0.75,
                     br(),
                     paste("Generated results:", today()),
                     br(),
                     br(),
                     uiOutput("print_national_table_title"),
                     uiOutput("print_district_table_title"),
                     uiOutput("print_ta_table_title"),
                     tags$p("Make sure to show",
                            strong("all"),
                            "entries (Note that selection will determine the number of entries in the export)"),
                     uiOutput("national_ui5"),
                     uiOutput("district_ui_table"),
                     uiOutput("ta_table")
              )#--end of column for result table
            )##end fluidRow table reductions
    ),##--end of Home
    ##--User guide
    tabItem(tabName = "user_guide",
            fluidRow(column(11, offset = 0.75,
                            h1("Overview"),
                            tags$p(h4("This dynamic, epidemiological dashboard for COVID-19 
                                                  in Malawi generates estimated projections on number of cases, 
                                                  hospitalizations, ICU stays and deaths based on user-defined, policy parameters.")),
                            h1("Policy levers"),
                            tags$ul(
                              img(src='policy_levers.png', align = "center", height = "50%",
                                  width = "50%")),
                            tags$div(
                              tags$ul(
                                tags$li(h4(strong("% Masking – "), "Percentage of population wearing a mask (Note that the model assumes mask efficacy at 50%); User may select values from 0 to 100, inclusively")),
                                tags$li(h4(strong("% Physical Distancing –"), "Percentage of population physical distancing (i.e., reducing number of close contacts); User may select values from -100 to 100, inclusively (Note that negative values indicate that gatherings are increasing)")),
                                tags$li(h4(strong("Length of Intervention (in days)  –"), "Number of days that each respective intervention will be carried out in the simulation; User may select values from 0 to the user-defined", em("End of Model"), "(see below)")),
                                #tags$li(h4(strong("Start of model (days before today) – "), "The number of days before today (i.e., Day 1 of simulation with user-defined policy levers) displayed on the x-axes of generated line plots")),
                                tags$li(h4(strong("End of model (days after today) –"), "The number of days after today (i.e., Day 1 of simulation) for which estimates in the generated outputs will reflect; User may select values from 0 to 90, inclusively")),
                                #tags$li(h4(strong("Level of Interest –"), "Option to select whether line plots will reflect values at the national-, district- or TA-level (Note that if either district or TA is selected, user must subsequently select a specific district or TA to generate results for)")),
                              )
                            ),
                            tags$p(h4("Once the user sets all parameter values for manipulative policy levers, they may click",
                                      strong("Run Report"), "to update the model outputs based on their selections.")),
                            ##-- Display Options
                            h1("Display Options:"),
                            tags$p(h4("The user may also set different parameters to influence the display settings for the generated outputs.")),
                            tags$ul(
                              tags$li(h4(strong("Level of Interest - "), "Option to select whether line plots will reflect values at the national-, 
                                         district- or TA-level", "(", 
                                         em("Note that if either district or TA is selected, 
                                         an additional dropdown will appear requiring the user to subsequently select a specific district or TA 
                                         to generate results for"),")"
                              )),
                              tags$li(h4(strong("Start of Model - "), "Slider to select the number of days prior to today (i.e., Day 1 of simulation) 
                                         visualized on the x-axis. The number of days is capped so that the user can look back as far as the onset 
                                         of the pandemic in Malawi (i.e., April 1, 2020). See below:"))
                            ),
                            tags$ul(
                              img(src='display_options.png', align = "center", height = "50%",
                                  width = "50%")),
                            ##--Fixed Model parameters
                            h1("Fixed model parameters"),
                            
                            tags$p(h4("Certain parameters in the dashboard that are derived from scientific literature describing the virus, 
                                         how it spreads and whom it is most likely to burden are fixed and therefore unavailable for user manipulation. 
                                         These fixed model parameters are listed and sourced below:")),
                            ##add figure for fixed parameters
                            tags$ul(
                              img(src="fixed.png", height = "50%", width = "50%")
                            ),
                            h1("Output:"),
                            
                            tags$p(h4("After making their selections and re-running the model, the following outputs will be generated:")),
                            tags$ul(tags$p(h4(strong("1. 4 Widgets  (green, orange, red and black boxes) - "), 
                                              "to display the numeric and percentage decreases in cases, 
                                              hospitalizations, ICU stays and deaths from the baseline to
                                              the user-defined simulation. See below:"))),
                            tags$ul(
                              img(src="widgets.png", align = "center", height = "80%", width = "80%")
                            ),
                            tags$ul(tags$p(h4(strong("2. 4 Line Plots - "), "to depict cases, hospitalizations, 
                            ICU stays and deaths (across the user defined time period) for the baseline simulation and for 
                                              the simulation based on the user-defined inputs. 
                                              The black, vertical, dashed line indicates today or Day 1 of the simulation.
                                              The length of the masking intervention, the physical distancing intervention and 
                                              the overlap of these two interventions is visualized using blue, red and purple shading, 
                                              respectively. 
                                              See below: "))),
                            tags$ul(img(src="plot2.png", align = "center", height = "60%", width = "60%"))),
                     column(11, offset = 0.75,
                            tags$ul(
                              tags$p(h4(strong("3. 1 Results Table - "), "which provides number of cases, hospitalizations, ICU stays and deaths for the following categories:"))
                            ),
                            tags$ul(
                              tags$p(h4(strong("a.	To Date –"), "Cumulative cases, hospitalizations, ICU stays and deaths")),
                              tags$p(h4(strong("b.	Status Quo –"), "Cumulative values up until the user-defined time horizon based on the baseline scenario policy levers (i.e., masking at 15% and physical distancing at 8%)")),
                              tags$p(h4(strong("c.	User-defined simulation –"), "Cumulative values up until the user-defined time horizon based on the user-defined policy lever selections"))
                            ), 
                            br(),
                            tags$p(h4("*Various functionalities are included that allow the user to select 
                                      the number of entries shown on the initial interface of the table and to export
                                      to CSV or Excel. Note that this table displays the sub-level of data immediately
                                      following the user-defined level of interest, in order to offer a more detailed 
                                      breakdown of the results and facilitate comparisons across districts and TAs. 
                                      Further, that when “National” is selected as the level of interest, 
                                      this table displays district-level data. When “District” is selected as 
                                      the level of interest and a particular district is specified, this table 
                                      displays TA-level data for that district. When “TA” is selected as the 
                                      level of interest, this table displays TA-level data for the district that 
                                      the chosen TA falls within. See below:")),
                            
                            br(),
                            br(),
                            tags$p(h4(strong("District-level data when “National” is the selected level of interest"))),
                            tags$ul(img(src="result_national.png", align = "center", height = "90%", width = "90%")),
                            br(),
                            br(),
                            
                            tags$p(h4(strong("TA-level data when “District” is the selected level of interest"))),
                            tags$ul(img(src="result_district.png", align = "center", height = "90%", width = "90%")),
                            br(),
                            br(),
                            
                            tags$p(h4("For more information regarding the dashboard’s software and underlying technical program, please refer to the", strong("“Technical infrastructure guide”"), "tab."))
                     )##--end column 
            )##--end of fluidRow
    ), #--end of user guide
    ##--Technical guide
    tabItem(tabName = "technical_guide",
            fluidRow(column(11, offset = 0.75,
                            h1("Technical infrastructure guide"),
                            tags$p(h4("Our model is coded using freely available open source 
                                      R programming language (version 3.6.3) with the RStudio IDE (version 1.4.869)."),
                                   
                                   h4("Our analytic code leverages the packages", 
                                      strong("tidyverse"), ",",
                                      strong("deSolve"), ",",
                                      "and",
                                      strong("ggplot2."), 
                                      "The graphical user interface for the dynamic web-based dashboard is programmed using freely available open source RShiny dashboards. Besides the", strong("Shiny"), "and", strong("shinydashboard"), "libraries, other additional libraries 
                                    were used to organize the model outputs and present the visualizations on the final app, 
                                    such as")),
                            
                            tags$ul(
                              tags$li(h4("shinyFeedback")),
                              tags$li(h4("shinythemes")),
                              tags$li(h4("shinyWidgets")),
                              tags$li(h4("readr")),
                              tags$li(h4("plotly")),
                              tags$li(h4("DT")),
                              tags$li(h4("lubridate")),
                              tags$li(h4("tidyverse")),
                              tags$li(h4("scales")),
                              tags$li(h4("htmlwidgets")),
                              tags$li(h4("jsonlite")),
                            ),
                            br(),
                            tags$p(h4("For user instructions and explanations of the dashboard’s interface, please refer to the", 
                                      strong("“User guide”"), "tab.")),
                            br(),
                            br(),
                            tags$p(h4(strong("The code for this epidemiological dashboard can be found in the following link:",
                                             tags$a(href = "https://github.com/edneide/EpiModel_Dashboard_Malawi", "GitHub.")))
                            )
                            
                            
            )##--end of column
            )##--end of fluid row
    )#--end of technical guide
  )#--end of tabItems
)#--end of dashboardBody


##4. UI------------------

ui <- dashboardPage(skin = "purple", header = header, sidebar = sidebar, body = body)

##-------------##
## 5. SERVER----
##-------------##


server <- function(input, output, session){
  
  ##---------------------------------------------------------##
  #---5.1 Creating the inputs .csv's based on user's selection----- 
  ##---------------------------------------------------------##
  ##--For model Status Quo
  restrictionsInput <- observe({
    if(input$runreportButton == 0) return()
    
    masking_sq <- masking[1:n_days, ]
    
    write.csv(data.frame(masking_compliance = masking_sq$masking_compliance),
              "out/masking_baseline.csv", row.names = FALSE)
    ##-----###
    current <- current[1:n_days, ] 
    write.csv(data.frame(reduc = current$reduc), 
              "out/distancing_baseline.csv", 
              row.names = FALSE)
  })
  
  ##--For model simulation
  restrictionsInput <- observe({
    if(input$runreportButton == 0) return()
    
    masking_new <- masking %>% 
      mutate(
        masking_compliance2 = ifelse(date <= today(), masking_compliance,
                                     ifelse(between(date, today()+1, today() + days(input$time_intervention_mask)),
                                            input$mask_perc/100, masking_compliance))
      )
    
    write.csv(data.frame(masking_compliance = masking_new$masking_compliance2),
              "out/masking_sim.csv", row.names = FALSE)
    ##-----###
    current <- current %>% 
      mutate(reduc_new = ifelse(date <= today(), reduc, 
                                ifelse(between(date, today(), today() +
                                                 days(input$time_intervention_dist)),
                                       input$distancing_perc/100, reduc)))
    write.csv(data.frame(reduc = current$reduc_new), 
              "out/distancing_sim.csv", 
              row.names = FALSE)
  })
  
  ##----------------------##
  ##--5.2 Reactive functions----
  ##----------------------##
  district_choices <- reactiveVal(districts_names$districts)
  
  ##--5.3 End of Model---- 
  end_of_model <- eventReactive(
    input$runreportButton,
    {input$projection}
  )
  
  ##--5. 4 UI for length of masking intervention----
  output$masking_intervention <- renderUI({
    #shinyFeedback::useShinyFeedback()
    numericInput('time_intervention_mask',
                 label = "Length of masking intervention\n(# Days)",
                 value = 14,
                 min = 0,
                 max = input$projection)
    #textOutput("warning_masking")
  })
  
  ##--5.5 UI for length of distancing intervention----
  output$distancing_intervention <- renderUI({
    numericInput('time_intervention_dist',
                 label = "Length of distancing intervention\n(# Days)",
                 value = 14,
                 min = 7,
                 max = input$projection)
  })
  
  
  
  
  
  ##--5.6 Simulation----------------------
  simulation_function <- reactive({
    if(input$runreportButton == 0) return()

    df_distancing <- read_csv('out/distancing_sim.csv', col_types=cols())
    df_masking <- read_csv('out/masking_sim.csv', col_types=cols())
    
    return(runModel(df_distancing, df_masking, df_locations, df_params, df_seed))
  }) # end of simulation function
  
  ##--Creating country table from simulation 
  country_projection_sim <- reactive({
    df_country_spread <- simulation_function()[[1]] %>%
      dplyr::mutate(Cases = `New Infections`)
    Cases_cum <- cumsum(df_country_spread$Cases) 
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized)/hosp_time 
    ICU_cum <- cumsum(df_country_spread$Critical)/crit_time 
    Death <- df_country_spread$Dead 
    Severe_cases <- ICU_cum + Hospitalized_cum + Death
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              time = 1:n_days,
                              Cases_sim = round(Cases_cum),
                              Hospitalizations_sim = round(Hospitalized_cum),
                              ICU_sim = round(ICU_cum), 
                              Death_sim = round(Death),
                              Severe_sim = round(Severe_cases))
    
    
    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today() + lubridate::days(end_of_model()) - 1)
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(date, Cases_sim, Hospitalizations_sim, ICU_sim, Death_sim)
    names(df_country_dash_projection2) <- c("date", "Cases (simulation projection)",
                                            "Hosp. (simulation projection)",
                                            "ICU (simulation projection)",
                                            "Death (simulation projection)")
    list_sim <- list(df_country_dash, df_country_dash_projection2)
    return(list_sim)
  })
  ##--Creating TA table from simulation
  ta_simulation <- reactive({
    ta_test2 <- simulation_function()[[3]] %>% 
      filter(Lvl4 == input$ta) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State) %>% 
      summarize(new_inf = sum(People)) %>% 
      mutate(TA = input$ta)
    
    ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
    Hosp_cum_ta = cumsum(ta1_spread$Hospitalized)/hosp_time 
    Critical_cum_ta = cumsum(ta1_spread$Critical)/crit_time 
    Dead_ta =  ta1_spread$Dead %>% round()
    Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
    
    simulation_ta <- tibble(date = ta1_spread$date,
                            Cases = Cases_cum_ta,
                            Hospitalizations = round(Hosp_cum_ta),
                            ICU = round(Critical_cum_ta),
                            Death = Dead_ta,
                            Severe = Severe_ta)
    ##--Total
    #simulation_ta
    ##-------------------------##
    ##--Projection--##
    projection_ta <- simulation_ta %>% 
      filter(date == lubridate::today() + lubridate::days(end_of_model()) - 1)
    names(projection_ta) <- c("date", "Cases (simulation projection)",
                              "Hosp. (simulation projection)",
                              "ICU (simulation projection)",
                              "Death (simulation projection)")
    #projection_ta 
    
    result <- list(simulation_ta, projection_ta)
    return(result)
  })
  
  
  ##--5.7 Simulation Baseline--------------------
  
  simulation_baseline <- reactive({
    if(input$runreportButton == 0) return()
    
    df_distancing <- read_csv('out/distancing_with_date.csv', col_types=cols())
    df_masking <- read_csv('out/masking_with_date.csv', col_types=cols())

    return(runModel(df_distancing, df_masking, df_locations, df_params, df_seed))
  }) # end of simulation baseline
  
  ##--Creating country table simulation for status quo
  country_projection_status_quo <- reactive({
    df_country_spread <- simulation_baseline()[[1]] %>% dplyr::mutate(Cases = `New Infections`)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized)/hosp_time 
    ICU_cum <- cumsum(df_country_spread$Critical)/crit_time 
    Death <- df_country_spread$Dead %>% round()
    Severe_cases <- round(Death + Hospitalized_cum + ICU_cum)
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              time = 1:n_days,
                              Cases_sq = Cases_cum,
                              Hospitalizations_sq = round(Hospitalized_cum),
                              ICU_sq = round(ICU_cum), 
                              Death_sq = Death,
                              Severe_sq = Severe_cases)
    
    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today() + lubridate::days(end_of_model()) - 1)
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(Cases_sq, Hospitalizations_sq, ICU_sq, Death_sq)
    names(df_country_dash_projection2) <- c("Cases (status quo)",
                                            "Hosp. (status quo)",
                                            "ICU (status quo)",
                                            "Death (status quo)")
    list_status_quo <- list(df_country_dash, df_country_dash_projection2)
    return(list_status_quo)
  })
  ##--Creating country table simulation for to date
  country_projection_to_date <- reactive({
    df_country_spread <- simulation_baseline()[[1]] %>%
      dplyr::mutate(Cases = `New Infections`)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized)/hosp_time 
    ICU_cum <- cumsum(df_country_spread$Critical)/crit_time 
    Death <- df_country_spread$Dead %>% round()
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              Cases_sim = Cases_cum,
                              Hospitalizations_sim = round(Hospitalized_cum),
                              ICU_sim = round(ICU_cum), 
                              Death_sim = Death)
    
    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today())
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(Cases_sim, Hospitalizations_sim, ICU_sim, Death_sim)
    names(df_country_dash_projection2) <- c("Cases (to date)",
                                            "Hosp. (to date)",
                                            "ICU (to date)",
                                            "Death (to date)")
    return(df_country_dash_projection2)
  })
  ##--Creating district table simulation for status quo 
  district_projection_status_quo <- reactive({
    district_df <- simulation_baseline()[[2]] %>% 
      filter(Lvl3 == input$district) %>% 
      mutate(Cases = `New Infections`)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized)/hosp_time 
    ICU_cum_dist <- cumsum(district_df$Critical)/crit_time 
    Death_dist <- district_df$Dead %>% round()
    Severe_dist <- round(Hospitalized_cum_dist + ICU_cum_dist + Death_dist)
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:n_days), 
                           date = district_df$date,
                           Cases_sq = Cases_cum_dist, 
                           Hospitalizations_sq = round(Hospitalized_cum_dist),
                           ICU_sq = round(ICU_cum_dist), 
                           Death_sq = Death_dist,
                           Severe_sq = Severe_dist
    ) 
    #district_df2 -- OK
    ##------------------------------------------------------------##
    ##--Projection--##
    district_projection_sim <- district_df2 %>% 
      dplyr::filter(date == lubridate::today() + lubridate::days(end_of_model()) - 1) %>% 
      dplyr::select(Cases_sq, Hospitalizations_sq, ICU_sq, Death_sq)
    names(district_projection_sim) <- c("Cases (status quo)",
                                        "Hosp. (status quo)",
                                        "ICU (status quo)",
                                        "Death (status quo)")
    ##--district_projection_sim -- OK
    result <- list(district_df2, district_projection_sim)
    return(result)
  })
  ##--Creating district table simulation for to date
  district_projection_to_date <- reactive({
    district_df <- simulation_baseline()[[2]] %>% 
      filter(Lvl3 == input$district) %>% 
      mutate(Cases = `New Infections`)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized)/hosp_time
    ICU_cum_dist <- cumsum(district_df$Critical)/crit_time
    Death_dist <- district_df$Dead %>% round()
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:n_days), 
                           date = district_df$date,
                           Cases_sq = Cases_cum_dist, 
                           Hospitalizations_sq = round(Hospitalized_cum_dist),
                           ICU_sq = round(ICU_cum_dist), 
                           Death_sq = Death_dist) 
    #district_df2 -- OK
    ##------------------------------------------------------------##
    ##--Projection--##
    district_projection_sim <- district_df2 %>% 
      dplyr::filter(date == lubridate::today()) %>% 
      dplyr::select(Cases_sq, Hospitalizations_sq, ICU_sq, Death_sq)
    names(district_projection_sim) <- c("Cases (to date)",
                                        "Hosp. (to date)",
                                        "ICU (to date)",
                                        "Death (to date)")
    
    district_projection_sim
  })
  ##--Creating TA table simulation status quo
  ta_simulation_status_quo <- reactive({
    ta_test2 <- simulation_baseline()[[3]] %>% 
      filter(Lvl4 == input$ta) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State) %>% 
      summarize(new_inf = sum(People)) %>% 
      mutate(TA = input$ta)
    
    ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
    Hosp_cum_ta = cumsum(ta1_spread$Hospitalized)/hosp_time
    Critical_cum_ta = cumsum(ta1_spread$Critical)/crit_time
    Dead_ta =  ta1_spread$Dead %>% round()
    Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
    
    simulation_ta_sq <- tibble(date = ta1_spread$date,
                               time = seq(1:n_days),
                               Cases_sq = Cases_cum_ta,
                               Hospitalizations_sq = round(Hosp_cum_ta),
                               ICU_sq = round(Critical_cum_ta),
                               Death_sq = Dead_ta,
                               Severe_sq = Severe_ta)
    ##-------------------------##
    projection_ta_sq <- simulation_ta_sq[,-2] %>% 
      filter(date == lubridate::today() + lubridate::days(end_of_model()) - 1) %>% 
      select(date, Cases_sq:Death_sq)
    names(projection_ta_sq) <- c("date", "Cases (status quo projection)",
                                 "Hosp. (status quo projection)",
                                 "Critical (status quo projection)",
                                 "Death (status quo projection)")
    
    ##-------------------------##
    to_date_ta_sq <- simulation_ta_sq[,-2] %>% 
      filter(date == lubridate::today()) %>% 
      select(date, Cases_sq:Death_sq)
    names(to_date_ta_sq) <- c("date", "Cases (to date)",
                              "Hosp. (to date)",
                              "Critical (to date)",
                              "Death (to date)")
    
    result <- list(simulation_ta_sq, projection_ta_sq, to_date_ta_sq)
    return(result)
  })
  ##--Creating district table from simulation
  district_sim <- reactive({
    district_df <- simulation_function()[[2]] %>% 
      filter(Lvl3 == input$district) %>% 
      mutate(Cases = `New Infections`)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized)/hosp_time 
    ICU_cum_dist <- cumsum(district_df$Critical)/crit_time 
    Death_dist <- district_df$Dead %>% round()
    Severe_cases <- ICU_cum_dist + Hospitalized_cum_dist + Death_dist
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:n_days), 
                           date = district_df$date,
                           Cases = Cases_cum_dist, 
                           Hospitalizations = round(Hospitalized_cum_dist),
                           ICU = round(ICU_cum_dist), 
                           Death = Death_dist,
                           Severe = Severe_cases) 
    ##------------------------------------------------------------##
    ##--Projection--##
    district_projection_sim <- district_df2 %>% 
      dplyr::filter(date == lubridate::today() + lubridate::days(end_of_model()) - 1) %>% 
      dplyr::select(Cases, Hospitalizations, ICU, Death)
    names(district_projection_sim) <- c("Cases (simulation projection)",
                                        "Hosp. (simulation projection)",
                                        "ICU (simulation projection)",
                                        "Death (simulation projection)")
    ##--district_projection_sim -- OK
    result <- list(district_df2, district_projection_sim)
    return(result)
  })
  
  
  
  
  ##--5.8 Plots National--------------
  
  #--Title National--
  output$plot_national_title <- renderText({
    "Graphs at National Level"
  })
  #--Cases--
  output$fig <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash %>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(90))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Cases_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'rgb(0, 102, 0)')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% layout(
        title = "<b>Cases (in millions)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      #fig <- fig %>% layout(plot_bgcolor='transparent')
      fig <- fig %>% add_trace(y = ~ Cases_sim, name = 'Intervention', line = list(color = 'rgb(102, 255, 153)'))
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today') %>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      
      return(fig)
    }else{
      ##---Making update depending on "Run Report" Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      data_final_plot <- cbind(country_projection_status_quo()[[1]], 
                               country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Cases_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'rgb(0, 102, 0)')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% layout(
        title = "<b>Cases (in millions)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      #fig <- fig %>% layout(plot_bgcolor='transparent')
      fig <- fig %>% add_trace(y = ~ Cases_sim, name = 'Intervention', line = list(color = 'rgb(102, 255, 153)'))
      fig <- fig %>% add_trace(x = today(),
                               type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'),
                               name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases_sim)), 
                           y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases_sim)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases_sim)), 
                           y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases_sim)), 
                           yref = "y")
                      # list(type = "line",
                      #      x0 = today(), x1 = today(), xref = "x",
                      #      y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases_sim)), 
                      #      y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases_sim)),
                      #      yref = "y",
                      #      line = list(color = 'grey', dash = 'dash'),
                      #      text = 'Today'
                      #      )
                    )) %>% 
        config(displayModeBar = F)
      
      
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    } 
  })
  
  ##--Hospitalizations--
  output$fig_country2 <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(90))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(input$time_intervention_mask))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(input$time_intervention_dist))]
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Hospitalizations_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'orange')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sim, name = 'Intervention', 
                               line = list(color = 'rgb(255, 223, 153)'))
      fig <- fig %>% layout(
        title = "<b>Hospitalizations (in thousands)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      #fig <- fig %>% layout(plot_bgcolor='transparent')
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today')%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    }else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Hospitalizations_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'orange')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sim, 
                               name = 'Intervention', 
                               line = list(color = 'rgb(255, 223, 153)'))
      fig <- fig %>% layout(
        title = "<b>Hospitalizations (in thousands)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      #fig <- fig %>% layout(plot_bgcolor='transparent')
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations_sim)), 
                           y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations_sim)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations_sim)), 
                           y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations_sim)), 
                           yref = "y")
                      # list(type = "line",
                      #      x0 = today(), x1 = today(), xref = "x",
                      #      y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations_sim)), 
                      #      y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations_sim)),
                      #      yref = "y",
                      #      line = list(color = 'grey', dash = 'dash'),
                      #      name = 'Today'
                      # )
                    ))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    } 
  })
  
  ##--Critical Care----
  output$fig_country3 <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(90))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ ICU_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'red')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ ICU_sim, name = 'Intervention', line = list(color = 'pink'))
      fig <- fig %>% layout(
        title = "<b>ICU (in thousands)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today')%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    }else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ ICU_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'red')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ ICU_sim, name = 'Intervention', line = list(color = 'pink'))
      fig <- fig %>% layout(
        title = "<b>ICU (in thousands)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU_sim)), 
                           y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU_sim)),
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU_sim)), 
                           y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU_sim)),
                           yref = "y")
                      # list(type = "line",
                      #      x0 = today(), x1 = today(), xref = "x",
                      #      y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU_sim)), 
                      #      y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU_sim)),
                      #      yref = "y",
                      #      line = list(color = 'grey', dash = 'dash'),
                      #      name = 'Today'
                      # )
                    ))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    }
    
  })
  ##--Deaths
  output$fig_country4 <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(90))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'black')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death_sim, name = 'Intervention', line = list(color = 'grey'))
      fig <- fig %>% layout(
        title = "<b>Deaths</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today')%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    } else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'black')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death_sim, name = 'Intervention', line = list(color = 'grey'))
      fig <- fig %>% layout(
        title = "<b>Deaths</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                               line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death_sim)), 
                           y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death_sim)),
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death_sim)), 
                           y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death_sim)),
                           yref = "y")
                      # list(type = "line",
                      #      x0 = today(), x1 = today(), xref = "x",
                      #      y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death_sim)), 
                      #      y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death_sim)),
                      #      yref = "y",
                      #      line = list(color = 'grey', dash = 'dash'),
                      #      name = 'Today'
                      # )
                    ))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)
    }
  })
  
  
  ##--5.9 Plots District---------------------
  
  #--Title
  output$plot_district_title <- renderText({
    paste0("Graphs at District Level: ", input$district)
  })
  ##--Cases
  output$fig_district <- renderPlotly({
    
    if(input$runreportButton == 0){
      return()
    }else{ 
      
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      #data_final_plot <- district_projection_status_quo()[[1]]
      data_final_plot <- cbind(district_projection_status_quo()[[1]], 
                               district_sim()[[1]][,-c(1,2)]) %>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Cases_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'rgb(0, 102, 0)')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      
      fig <- fig %>% layout(
        title = "<b>Cases (in thousands)</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig %>% add_trace(y = ~ Cases, name = 'Intervention', line = list(color = 'rgb(102, 255, 153)'))
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases)), 
                           y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases)),
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases)), 
                           y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases)),
                           yref = "y")
                      # list(type = "line",
                      #      x0 = today(), x1 = today(), xref = "x",
                      #      y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases)), 
                      #      y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases)),
                      #      yref = "y",
                      #      line = list(color = 'grey', dash = 'dash'),
                      #      name = 'Today'
                      #)
                    ))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      fig <- fig  %>% add_trace(x = today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      return(fig)
    }
  })
  
  ##--Hospitalizations
  output$fig_district2 <- renderPlotly({
    
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Hospitalizations_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'orange')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Intervention', line = list(color = 'rgb(255, 223, 153)'))
      fig <- fig %>% layout(
        title = "<b>Hospitalizations</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig  %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations)), 
                           y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations)), 
                           y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations)), 
                           yref = "y")))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)}
  })
  ##--Critical care----
  output$fig_district3 <- renderPlotly({
    
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot)&
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ ICU_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'red')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU (Intervention)', line = list(color = 'pink'))
      fig <- fig %>% layout(
        title = "<b>ICU</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig  %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU)), 
                           y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU)), 
                           y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU)), 
                           yref = "y")))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)}
  })
  
  
  
  ##--Deaths----
  output$fig_district4 <- renderPlotly({
    
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      #data_final_plot <- district_projection_status_quo()[[1]]
      data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'black')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death, name = 'Intervention', line = list(color = 'grey'))
      fig <- fig %>% layout(
        title = "<b>Deaths</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <-  fig %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death)), 
                           y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death)), 
                           y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death)), 
                           yref = "y")))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)}
  })
  
  
  
  ##--5.10 Plots TA------------------
  
  ##--Title
  output$plot_ta_title <- renderText({
    paste0("Graphs at TA Level: ", input$ta)
  })
  ##--Cases
  output$fig_ta <- renderPlotly({
    req(input$district2, input$ta)
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      ##--Changing title
      title <- vector(mode = "character", length = 1)
      if(data_final_plot$Cases_sq/100 > 99){
        title[1] <- "Cases (in thousands)"
      }else{
        title[1] <- "Cases"
      }
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Cases_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'rgb(0, 102, 0')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Cases, name = 'Intervention', line = list(color = 'rgb(102, 255, 153)'))
      
      fig <- fig %>% layout(
        #title = "<b>Cases</b>"
        title = paste0("<b>",title[1],"<b>")
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig  %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases)), 
                           y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Cases_sq), min(data_final_plot$Cases)), 
                           y1 = max(max(data_final_plot$Cases_sq), max(data_final_plot$Cases)), 
                           yref = "y")))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)}
  })
  
  ##--Hospitalizations
  output$fig_ta2 <- renderPlotly({
    req(input$district2, input$ta)
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Hospitalizations_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'orange')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Intervention', line = list(color = 'rgb(255, 223, 153)'))
      
      fig <- fig %>% layout(
        title = "<b>Hospitalizations</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig  %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations)), 
                           y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Hospitalizations_sq), min(data_final_plot$Hospitalizations)), 
                           y1 = max(max(data_final_plot$Hospitalizations_sq), max(data_final_plot$Hospitalizations)), 
                           yref = "y")))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)} 
  })
  
  
  
  ##--ICU
  ##--ICU
  output$fig_ta3 <- renderPlotly({
    req(input$district2, input$ta)
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ ICU_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'red')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ ICU, name = 'Intervention', line = list(color = 'pink'))
      fig <- fig %>% layout(
        title = "<b>ICU</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig  %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU)), 
                           y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU)), 
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$ICU_sq), min(data_final_plot$ICU)), 
                           y1 = max(max(data_final_plot$ICU_sq), max(data_final_plot$ICU)), 
                           yref = "y")))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)}
  })
  
  
  ##--Death
  output$fig_ta4 <- renderPlotly({
    ##--Require certain inputs
    req(input$district2, input$ta)
    if(input$runreportButton == 0){return()}else{
      ##---Make update depending on Run Report Button 
      input$runreportButton
      masking_time <- isolate(input$time_intervention_mask)
      distancing_time <- isolate(input$time_intervention_dist)
      ##
      
      
      data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
        filter(date >= today() - days(input$begin_plot) &
                 date <= today() + days(end_of_model() - 1))
      x_start <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops <- data_final_plot$date[which(data_final_plot$date == today() + days(masking_time))]
      x_start_distancing <- data_final_plot$date[which(data_final_plot$date == today())]
      x_stops_distancing <- data_final_plot$date[which(data_final_plot$date == today() + days(distancing_time))]
      
      
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'black')
                      # width = 450,
                      # height = 200
      )
      fig <- fig %>% layout(xaxis = list(title = ""),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death, name = 'Intervention', line = list(color = 'grey'))
      fig <- fig %>% layout(
        title = "<b>Deaths</b>"
      )
      fig <- fig %>% layout(paper_bgcolor='transparent')
      fig <- fig  %>% add_trace(x =today(), type = 'scatter', mode = 'lines',
                                line = list(color = 'grey', dash = 'dash'), name = 'Today')
      
      fig <- layout(fig,
                    shapes = list(
                      list(type = "rect",
                           fillcolor = "blue", line = list(color = "blue"), opacity = 0.1,
                           x0 = x_start, x1 = x_stops, xref = "x",
                           y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death)), 
                           y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death)),
                           yref = "y"),
                      list(type = "rect",
                           fillcolor = "red", line = list(color = "red"), opacity = 0.1,
                           x0 = x_start_distancing, x1 = x_stops_distancing, xref = "x",
                           y0 = min(min(data_final_plot$Death_sq), min(data_final_plot$Death)), 
                           y1 = max(max(data_final_plot$Death_sq), max(data_final_plot$Death)),
                           yref = "y")
                    ))%>% 
        config(displayModeBar = F)
      fig <- fig %>% layout(legend = list(orientation = 'h'))
      return(fig)}
  })
  
  
  
  ##--Table National--------------------
  
  #--Title
  output$national_table_title <- renderText({
    "District-level data for Malawi"
  })
  #--Baseline
  table_all_districts_baseline <- reactive({
    district_df <- simulation_baseline()[[2]] %>% 
      mutate(Cases = `New Infections`)
    
    list_district_baseline <- list()
    districts <- districts_names$districts
    
    ##--Loop
    for(i in 1:length(districts)){
      df_district2 <- district_df %>% 
        filter(Lvl3 == districts_names$districts[i])
      
      Cases_cum_dist <- cumsum(df_district2$Cases) %>% round()
      Hospitalized_cum_dist <- cumsum(df_district2$Hospitalized)/hosp_time 
      ICU_cum_dist <- cumsum(df_district2$Critical)/crit_time
      Death_dist <- df_district2$Dead %>% round()
      Severe_dist <- round(Hospitalized_cum_dist + ICU_cum_dist + Death_dist)
      
      df_district <- tibble(time = seq(1:n_days), 
                            date = df_district2$date,
                            District = districts[i],
                            Cases = Cases_cum_dist, 
                            Hospitalizations = round(Hospitalized_cum_dist),
                            ICU = round(ICU_cum_dist), 
                            Death = Death_dist,
                            Severe = Severe_dist)
      
      list_district_baseline[[i]] <- df_district
    }#--end of loop
    ##--Projection baseline
    table_all <- do.call(rbind, list_district_baseline)
    df_simulation_baseline <- table_all %>% 
      filter(date == today() + days(end_of_model() - 1)) %>% 
      select(Cases:Death)
    names(df_simulation_baseline) <- c("Cases (status quo projection)",
                                       "Hosp. (status quo projection)", 
                                       "ICU (status quo projection)",
                                       "Death (status quo projection)")
    
    ##--To date
    df_baseline_to_date <- table_all %>% 
      filter(date == today()) %>% 
      select(District:Death)
    names(df_baseline_to_date) <- c("District",
                                    "Cases (to date)",
                                    "Hosp. (to date)", 
                                    "ICU (to date)",
                                    "Death (to date)")
    
    ##--Result 
    result <- list(df_baseline_to_date, df_simulation_baseline)
    return(result)
  }) #--end table_all_districts_baseline
  #--Simulation Projection
  table_all_districts_simulation <- reactive({
    district_df <- simulation_function()[[2]] %>% 
      mutate(Cases = `New Infections`)
    
    list_district_simulation <- list()
    districts <- districts_names$districts
    
    ##--Loop
    for(i in 1:length(districts)){
      df_district2 <- district_df %>% 
        filter(Lvl3 == districts_names$districts[i])
      
      Cases_cum_dist <- cumsum(df_district2$Cases) %>% round()
      Hospitalized_cum_dist <- cumsum(df_district2$Hospitalized)/hosp_time 
      ICU_cum_dist <- cumsum(df_district2$Critical)/crit_time
      Death_dist <- df_district2$Dead %>% round()
      Severe_dist <- round(Hospitalized_cum_dist + ICU_cum_dist + Death_dist)
      
      df_district <- tibble(time = seq(1:n_days), 
                            date = df_district2$date,
                            District = districts[i],
                            Cases = Cases_cum_dist, 
                            Hospitalizations = round(Hospitalized_cum_dist),
                            ICU = round(ICU_cum_dist), 
                            Death = Death_dist,
                            Severe = Severe_dist)
      
      list_district_simulation[[i]] <- df_district
    }#--end of loop
    table_all <- do.call(rbind, list_district_simulation)
    ##
    df_simulation_projection <- table_all %>% 
      filter(date == today() + days(end_of_model() - 1)) %>% 
      select(Cases:Death)
    names(df_simulation_projection) <- c("Cases (simulation projection)",
                                         "Hosp. (simulation projection)", 
                                         "ICU (simulation projection)",
                                         "Death (simulation projection)")
    df_simulation_projection
  }) #--end table_all_districts_simulation
  output$table_national <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        #--Simulation projection
        df_all_districts <- cbind(table_all_districts_baseline()[[1]],
                                  table_all_districts_baseline()[[2]],
                                  table_all_districts_simulation())
        
        
        #--Country table
        df_country <- cbind(country_projection_to_date(),
                            country_projection_status_quo()[[2]],
                            country_projection_sim()[[2]][,-1])
        df_country$District <- "Total"
        var1 <- names(df_country)[1]
        var2 <- names(df_country)[length(names(df_country))-1]
        df_country <- df_country %>%
          select(District, var1:var2)
        names(df_country) <- names(df_all_districts)
        
        df_final <- rbind(df_country, df_all_districts)
        df_final
        
        #--comma separators
        
        point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
        df2 <- tibble(df_final[,1], point(df_final[,2]), point(df_final[,3]), point(df_final[,4]), 
                      point(df_final[,5]), point(df_final[,6]), point(df_final[,7]), 
                      point(df_final[,8]), point(df_final[,9]), point(df_final[,10]), 
                      point(df_final[,11]), point(df_final[,12]), point(df_final[,13]))
        names(df2) <- names(df_final)
        df2
        
        
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      #filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = -1,
        autoWidth = TRUE,
        dom = 'Blfrtip',
        #dom = 'Bt',
        buttons = c('csv', 'excel'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(25, 50, -1), c(25, 50, "All")
        )
      )
    )
  )
  
  
  ##--Table District-----------
  
  ##--Title
  output$table_district_title <- renderText({
    paste0("TA-level data for ", input$district)
  })
  
  
  ##--Simulation
  table_tas_district_sim <- reactive({
    df_ta_all <- simulation_function()[[3]] %>% 
      filter(Lvl3 == input$district) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    
    ##--Creating Loop
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    
    ta_all_final <- ta_all %>% 
      filter(date == today() + days(end_of_model() - 1)) %>% 
      select(TA:Death)
    
    names(ta_all_final) <- c("TA", "Cases (simulation projection)", "Hosp. (simulation projection)",
                             "ICU (simulation projection)", "Death (simulation projection)")
    ta_all_final
  })
  ##--Status quo
  table_tas_district_sq <- reactive({
    df_ta_all <- simulation_baseline()[[3]] %>% 
      filter(Lvl3 == input$district) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    
    ##--Creating Loop
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    
    ta_all_final <- ta_all %>% 
      filter(date == today() + days(end_of_model() - 1)) %>% 
      select(TA:Death) 
    
    names(ta_all_final) <- c("TA", "Cases (status quo projection)", "Hosp. (status quo projection)",
                             "ICU (status quo projection)", "Death (status quo projection)")
    
    ##--To date
    ta_all_to_date <- ta_all %>% 
      filter(date == today()) %>% 
      select(TA:Death)
    
    names(ta_all_to_date) <- c("TA", "Cases (to date)", "Hosp. (to date)",
                               "ICU (to date)", "Death (to date)")
    result <- list(ta_all_to_date, ta_all_final)
    return(result)
  })
  ##--------------------------------###
  output$table_district <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        df <- cbind(district_projection_to_date(),
                    district_projection_status_quo()[[2]],
                    district_sim()[[2]]) %>%
          mutate(TA = "Total") %>%
          select(TA, `Cases (to date)` : `Death (simulation projection)`)
        
        names(df)[6:9] <- c("Cases (status quo projection)", "Hosp. (status quo projection)",
                            "ICU (status quo projection)", "Death (status quo projection)")
        # df
        
        df_all <- cbind(table_tas_district_sq()[[1]],
                        table_tas_district_sq()[[2]][,-1], 
                        table_tas_district_sim()[,-1])
        #df_all
        
        df_final <- rbind(df, df_all)
        
        
        point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
        df2 <- tibble(df_final[,1], point(df_final[,2]), point(df_final[,3]), point(df_final[,4]),
                      point(df_final[,5]), point(df_final[,6]), point(df_final[,7]), point(df_final[,8]),
                      point(df_final[,9]), point(df_final[,10]), point(df_final[,11]),
                      point(df_final[,12]), point(df_final[,13]))
        names(df2) <- names(df_final)
        df2
        
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      #filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = -1,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        #dom = 'Bt',
        buttons = c('csv', 'excel'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  )
  
  
  ##--Table TAs-------------------
  
  #--Title
  output$ta_table_title <- renderText({
    paste0("TA-level data for ", input$district2)
  })
  ##--Status quo
  new_ta_table_status_quo <-  reactive({
    df_ta2 <- simulation_baseline()[[3]]
    ##--Saving the district the selected TA belongs to 
    district_for_ta <- df_ta2$Lvl3[which(df_ta2$Lvl4 == input$ta)[1]]
    ##--Filtering for TAs in the same district  
    df_ta_all <- df_ta2 %>% 
      filter(Lvl3 == district_for_ta) %>%
      select(Lvl4:date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    ##--Spreading the data
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    ##--Organizing the information for table
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    #--to date
    status_quo_ta_all_to_date <- ta_all %>% 
      filter(date == today())
    names(status_quo_ta_all_to_date) <- c("date", "TA", "Cases (to date)", "Hosp. (to date)",
                                          "ICU (to date)", "Death (to date)", "Severe (to date)")
    ##--Making the selected TA goes to the top of the table 
    top_table <- status_quo_ta_all_to_date[status_quo_ta_all_to_date$TA==input$ta,]
    bottom_table <- status_quo_ta_all_to_date %>% 
      filter(TA != input$ta)
    ta_to_date_sq <- rbind(top_table, bottom_table)
    ##------------------------------------------------##
    #--projection status quo
    status_quo_ta_all_projection <- ta_all %>% 
      filter(date == today() + days(end_of_model() - 1))
    names(status_quo_ta_all_projection) <- c("date", "TA", "Cases (status quo projection)", 
                                             "Hosp. (status quo projection)",
                                             "ICU (status quo projection)", 
                                             "Death (status quo projection)", 
                                             "Severe (status quo projection)")
    top_table2 <- status_quo_ta_all_projection[status_quo_ta_all_projection$TA==input$ta,]
    bottom_table2 <- status_quo_ta_all_projection %>% 
      filter(TA != input$ta)
    ta_projection_sq <- rbind(top_table2, bottom_table2)
    ##------------------------------------------------##
    #--Binding to date with status quo projection 
    final_status_quo <- cbind(ta_to_date_sq[,-c(1,7)], ta_projection_sq[,-c(1,2,7)])
    return(final_status_quo)
  })
  ##--Simulation 
  new_ta_table_simulation <- reactive({
    df_ta2 <- simulation_function()[[3]]
    ##--Saving the district the selected TA belongs to 
    district_for_ta <- df_ta2$Lvl3[which(df_ta2$Lvl4 == input$ta)[1]]
    ##--Filtering for TAs in the same district  
    df_ta_all <- df_ta2 %>% 
      filter(Lvl3 == district_for_ta) %>%
      select(Lvl4:date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    ##--Spreading the data
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    ##--Organizing the information for table
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    #--projection status quo
    simulation_ta_all_projection <- ta_all %>% 
      filter(date == today() + days(end_of_model() - 1))
    names(simulation_ta_all_projection) <- c("date", "TA", "Cases (simulation projection)", 
                                             "Hosp. (simulation projection)",
                                             "ICU (simulation projection)", 
                                             "Death (simulation projection)", 
                                             "Severe (simulation projection)")
    top_table2 <- simulation_ta_all_projection[simulation_ta_all_projection$TA==input$ta,]
    bottom_table2 <- simulation_ta_all_projection %>% 
      filter(TA != input$ta)
    ta_projection_simulation <- rbind(top_table2, bottom_table2)
    return(ta_projection_simulation)
  })
  ##---------------------------------------##
  output$table_ta <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        
        df <- cbind(new_ta_table_status_quo(), new_ta_table_simulation()[, -c(1,2,7)])
        point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
        df2 <- tibble(df[,1], point(df[,2]), point(df[,3]), point(df[,4]),
                      point(df[,5]), point(df[,6]), point(df[,7]), point(df[,8]),
                      point(df[,9]), point(df[,10]), point(df[,11]), point(df[,12]),
                      point(df[,13]))
        names(df2) <- names(df)
        df2
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      #filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = -1,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        #dom = 'Bt',
        buttons = c('csv', 'excel'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))
      )
    )
  )
  
  
  ##--Table for Reductions National--------------
  
  output$table_reductions_country_abs <- renderTable({
    if(input$runreportButton == 0) return()
    
    reduc_cases <- country_projection_status_quo()[[2]][1,1] - country_projection_sim()[[2]][1,2]
    reduc_cases2 <- paste(format(reduc_cases, big.mark = ","), "", sep = " ")
    reduc_hosp <- country_projection_status_quo()[[2]][1,2]  - country_projection_sim()[[2]][1,3] 
    reduc_hosp2 <- paste(format(reduc_hosp, big.mark = ","), "", sep = " ")
    reduc_icu <- country_projection_status_quo()[[2]][1,3]  - country_projection_sim()[[2]][1,4] 
    reduc_icu2 <- paste(format(reduc_icu, big.mark = ","), "", sep = " ")
    reduc_death <-  country_projection_status_quo()[[2]][1,4]  - country_projection_sim()[[2]][1,5]
    reduc_death2 <- paste(format(reduc_death, big.mark = ","), "", sep = " ")
    table <- tibble("Reduction in Cases" = reduc_cases2,
                    "Reduction in Hosp." = reduc_hosp2,
                    "Reduction in ICU" = reduc_icu2,
                    "Reduction in Death" = reduc_death2
    )
    return(table)
  })
  output$table_reductions_country <- renderTable({
    print("EXECUTING THIS")
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(100*(country_projection_sim()[[2]][1,2]/country_projection_status_quo()[[2]][1,1] - 1))
    reduc_hosp <- abs(100*(country_projection_sim()[[2]][1,3]/country_projection_status_quo()[[2]][1,2] - 1))
    reduc_icu <- abs(100*(country_projection_sim()[[2]][1,4]/country_projection_status_quo()[[2]][1,3] - 1))
    reduc_death <- abs(100*(country_projection_sim()[[2]][1,5]/country_projection_status_quo()[[2]][1,4] - 1))
    table <- tibble("% Reduction in Cases" = reduc_cases,
                    "% Reduction in Hosp." = reduc_hosp,
                    "% Reduction in ICU" = reduc_icu,
                    "% Reduction in Death" = reduc_death)
    return(table)
  })
  
  
  
  ##--Table for Reductions District-------------------------------
  
  output$table_reductions_district_abs <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- district_projection_status_quo()[[2]][1,1] - district_sim()[[2]][1,1] 
    reduc_cases2 <- paste(format(reduc_cases, big.mark = ","), "", sep = " ")
    reduc_hosp <- district_projection_status_quo()[[2]][1,2]  - district_sim()[[2]][1,2] 
    reduc_hosp2 <- paste(format(reduc_hosp, big.mark = ","), "", sep = " ")
    reduc_icu <- district_projection_status_quo()[[2]][1,3]  - district_sim()[[2]][1,3] 
    reduc_icu2 <- paste(format(reduc_icu, big.mark = ","), "", sep = " ")
    reduc_death <-  district_projection_status_quo()[[2]][1,4]  - district_sim()[[2]][1,4]
    reduc_death2 <- paste(format(reduc_death, big.mark = ","), "", sep = " ")
    table <- tibble("Reduction in Cases" = reduc_cases2,
                    "Reduction in Hosp." = reduc_hosp2,
                    "Reduction in ICU" = reduc_icu2,
                    "Reduction in Death" = reduc_death2
    )
    return(table)
  })
  output$table_reductions_districts <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(100*(district_sim()[[2]][1,1]/district_projection_status_quo()[[2]][1,1] - 1))
    reduc_hosp <- abs(100*(district_sim()[[2]][1,2]/district_projection_status_quo()[[2]][1,2] - 1))
    reduc_icu <- abs(100*(district_sim()[[2]][1,3]/district_projection_status_quo()[[2]][1,3] - 1))
    reduc_death <- abs(100*(district_sim()[[2]][1,4]/district_projection_status_quo()[[2]][1,4] - 1))
    table <- tibble("% Reduction in Cases" = reduc_cases,
                    "% Reduction in Hosp." = reduc_hosp,
                    "% Reduction in ICU" = reduc_icu,
                    "% Reduction in Death" = reduc_death)
    return(table)
  })
  
  
  ##--Table for Reductions TAs-------------
  
  output$table_reductions_ta_abs <- renderTable({
    req(input$ta)
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(ta_simulation_status_quo()[[2]][1,2] - ta_simulation()[[2]][1,2]) 
    reduc_hosp <- abs(ta_simulation_status_quo()[[2]][1,3] - ta_simulation()[[2]][1,3]) 
    reduc_icu <- abs(ta_simulation_status_quo()[[2]][1,4] - ta_simulation()[[2]][1,4]) 
    reduc_death <-  abs(ta_simulation_status_quo()[[2]][1,5] - ta_simulation()[[2]][1,5]) 
    table <- tibble("Reduction in Cases" = reduc_cases,
                    "Reduction in Hosp." = reduc_hosp,
                    "Reduction in ICU" = reduc_icu,
                    "Reduction in Death" = reduc_death
    )
    return(table)
  })
  output$table_reductions_ta_perc <- renderTable({
    req(input$ta)
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(100*(ta_simulation()[[2]][1,2] /ta_simulation_status_quo()[[2]][1,2] - 1))
    reduc_hosp <- abs(100*(ta_simulation()[[2]][1,3] /ta_simulation_status_quo()[[2]][1,3] - 1))
    reduc_icu <- abs(100*(ta_simulation()[[2]][1,4] /ta_simulation_status_quo()[[2]][1,4] - 1))
    reduc_death <- abs(100*(ta_simulation()[[2]][1,5] /ta_simulation_status_quo()[[2]][1,5] - 1))
    table <- tibble("% Reduction in Cases" = reduc_cases,
                    "% Reduction in Hosp." = reduc_hosp,
                    "% Reduction in ICU" = reduc_icu,
                    "% Reduction in Death" = reduc_death)
    return(table)
  })
  
  
  
  
  ##--Widgets National----------------------------
  reduc_cases_national <- reactive(
    {country_projection_status_quo()[[2]][1,1] - country_projection_sim()[[2]][1,2]}
  )
  reduc_hosp_national <- reactive({country_projection_status_quo()[[2]][1,2]  - country_projection_sim()[[2]][1,3]}) 
  reduc_icu_national <- reactive({country_projection_status_quo()[[2]][1,3]  - country_projection_sim()[[2]][1,4]}) 
  reduc_death_national <-  reactive({country_projection_status_quo()[[2]][1,4]  - country_projection_sim()[[2]][1,5]})
  
  reduc_cases_national_perc <- reactive({round(abs(100*(country_projection_sim()[[2]][1,2]/country_projection_status_quo()[[2]][1,1] - 1)),2)})
  reduc_hosp_national_perc <- reactive({round(abs(100*(country_projection_sim()[[2]][1,3]/country_projection_status_quo()[[2]][1,2] - 1)),2)})
  reduc_icu_national_perc <- reactive({round(abs(100*(country_projection_sim()[[2]][1,4]/country_projection_status_quo()[[2]][1,3] - 1)),2)})
  reduc_death_national_perc <- reactive({round(abs(100*(country_projection_sim()[[2]][1,5]/country_projection_status_quo()[[2]][1,4] - 1)), 2)})
  
  
  ##--Cases
  output$cases <- renderValueBox({
    #req(input$mask_perc, input$time_intervention_mask, input$distancing_perc, input$time_interval_dist)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in Cases', '(National)')
        , icon = icon("virus")
        , color = "green"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_cases_national(), format = "d", big.mark = ','), " (", reduc_cases_national_perc(), "%)"), style = "font-size: 80%;")
        , paste('Reduction in Cases', '(National)')
        , icon = icon("virus")
        , color = "green"
      )
    } 
  })
  
  
  ##--Hospitalizations 
  output$hosp <- renderValueBox({
    #req(input$mask_perc, input$time_intervention_mask, input$distancing_perc, input$time_interval_dist)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, format = "d", big.mark = ',')
        , paste('Reduction in Hospitalizations', '(National)')
        , icon = icon("hospital-user")
        , color = "orange"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_hosp_national(), format = "d", big.mark = ','), " (", reduc_hosp_national_perc(), "%)"), style = "font-size: 80%;")
        , paste('Reduction in Hospitalizations', '(National)')
        , icon = icon("hospital-user")
        , color = "orange"
      )
    } 
  })
  
  ##--ICU 
  output$icu <- renderValueBox({
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in ICU', '(National)')
        , icon = icon("hospital")
        , color = "red"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_icu_national(), 
                              format = "d", big.mark = ','), " (", reduc_icu_national_perc(), "%)"), style = "font-size: 80%;")
        , paste('Reduction in ICU', '(National)')
        , icon = icon("hospital")
        , color = "red"
      )
    } 
  })
  ##--Death 
  output$death <- renderValueBox({
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in Deaths', '(National)')
        , icon = icon("stats", lib = 'glyphicon')
        , color = "black"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_death_national(), 
                              format = "d", big.mark = ','), " (", reduc_death_national_perc(), "%)"), style = "font-size: 80%;")
        , paste('Reduction in Deaths', '(National)')
        , icon = icon("stats", lib = 'glyphicon')
        , color = "black"
      )
    } 
  })
  
  
  
  
  
  ##--Widgets Districts----------------------------
  
  reduc_cases_districts <- reactive({district_projection_status_quo()[[2]][1,1] - district_sim()[[2]][1,1]}) 
  reduc_hosp_districts <- reactive({district_projection_status_quo()[[2]][1,2]  - district_sim()[[2]][1,2]})  
  reduc_icu_districts <- reactive({district_projection_status_quo()[[2]][1,3]  - district_sim()[[2]][1,3]})  
  reduc_death_districts <-  reactive({district_projection_status_quo()[[2]][1,4]  - district_sim()[[2]][1,4]}) 
  
  reduc_cases_districts_perc <- reactive({round(abs(100*(district_sim()[[2]][1,1]/district_projection_status_quo()[[2]][1,1] - 1)),2)}) 
  reduc_hosp_districts_perc <- reactive({round(abs(100*(district_sim()[[2]][1,2]/district_projection_status_quo()[[2]][1,2] - 1)),2)}) 
  reduc_icu_districts_perc <- reactive({round(abs(100*(district_sim()[[2]][1,3]/district_projection_status_quo()[[2]][1,3] - 1)),2)}) 
  reduc_death_districts_perc <- reactive({round(abs(100*(district_sim()[[2]][1,4]/district_projection_status_quo()[[2]][1,4] - 1)),2)}) 
  
  
  ##--Cases
  output$cases_districts <- renderValueBox({
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in Cases', "")
        , icon = icon("virus")
        , color = "green"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_cases_districts(), format = "d", big.mark = ','), " (", 
                      reduc_cases_districts_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in Cases', " (", input$district, ")")
        , icon = icon("virus")
        , color = "green"
      )
    } 
  })
  
  ##--Hospitalizations 
  output$hosp_districts <- renderValueBox({
    #req(input$mask_perc, input$time_intervention_mask, input$distancing_perc, input$time_interval_dist)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, format = "d", big.mark = ',')
        , paste('Reduction in Hospitalizations', "")
        , icon = icon("hospital-user")
        , color = "orange"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_hosp_districts(), format = "d", big.mark = ','), " (", 
                      reduc_hosp_districts_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in Hospitalizations', " (", input$district, ")")
        , icon = icon("hospital-user")
        , color = "orange"
      )
    } 
  })
  
  ##--ICU 
  output$icu_districts <- renderValueBox({
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in ICU', '')
        , icon = icon("hospital")
        , color = "red"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_icu_districts(), 
                              format = "d", big.mark = ','), " (", 
                      reduc_icu_districts_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in ICU', " (", input$district, ")")
        , icon = icon("hospital")
        , color = "red"
      )
    } 
  })
  ##--Death 
  output$death_districts <- renderValueBox({
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in Deaths', '')
        , icon = icon("stats", lib = 'glyphicon')
        , color = "black"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_death_districts(), 
                              format = "d", big.mark = ','), " (", 
                      reduc_death_districts_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in Deaths', " (", input$district, ")")
        , icon = icon("stats", lib = 'glyphicon')
        , color = "black"
      )
    } 
  })
  
  
  
  
  
  
  ##--Widgets TAs----------------------------
  reduc_cases_ta <- reactive({ta_simulation_status_quo()[[2]][1,2] - ta_simulation()[[2]][1,2]}) 
  reduc_hosp_ta <- reactive({ta_simulation_status_quo()[[2]][1,3] - ta_simulation()[[2]][1,3]}) 
  reduc_icu_ta <- reactive({ta_simulation_status_quo()[[2]][1,4] - ta_simulation()[[2]][1,4]})
  reduc_death_ta <-  reactive({ta_simulation_status_quo()[[2]][1,5] - ta_simulation()[[2]][1,5]}) 
  
  reduc_cases_ta_perc <- reactive({round(abs(100*(ta_simulation()[[2]][1,2] /ta_simulation_status_quo()[[2]][1,2] - 1)),2)})
  reduc_hosp_ta_perc <- reactive({round(abs(100*(ta_simulation()[[2]][1,3] /ta_simulation_status_quo()[[2]][1,3] - 1)),2)})
  reduc_icu_ta_perc <- reactive({round(abs(100*(ta_simulation()[[2]][1,4] /ta_simulation_status_quo()[[2]][1,4] - 1)),2)})
  reduc_death_ta_perc <- reactive({round(abs(100*(ta_simulation()[[2]][1,5] /ta_simulation_status_quo()[[2]][1,5] - 1)),2)})
  
  
  ##--Cases
  output$cases_tas <- renderValueBox({
    req(input$district2, input$ta)
    #req(input$mask_perc, input$time_intervention_mask, input$distancing_perc, input$time_interval_dist)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in Cases', "")
        , icon = icon("virus")
        , color = "green"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_cases_ta(), format = "d", big.mark = ','), " (", 
                      reduc_cases_ta_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in Cases', " (", input$ta, ")")
        , icon = icon("virus")
        , color = "green"
      )
    } 
  })
  
  ##--Hospitalizations 
  output$hosp_tas <- renderValueBox({
    req(input$district2, input$ta)
    #req(input$mask_perc, input$time_intervention_mask, input$distancing_perc, input$time_interval_dist)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, format = "d", big.mark = ',')
        , paste('Reduction in Hospitalizations', "")
        , icon = icon("hospital-user")
        , color = "orange"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_hosp_ta(), format = "d", big.mark = ','), " (", 
                      reduc_hosp_ta_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in Hospitalizations', " (", input$ta, ")")
        , icon = icon("hospital-user")
        , color = "orange"
      )
    } 
  })
  
  ##--ICU 
  output$icu_tas <- renderValueBox({
    req(input$district2, input$ta)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in ICU', '')
        , icon = icon("hospital")
        , color = "red"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_icu_ta(), 
                              format = "d", big.mark = ','), " (", 
                      reduc_icu_ta_perc(), "%)"),style = "font-size: 80%;")
        , paste0('Reduction in ICU', " (", input$ta, ")")
        , icon = icon("hospital")
        , color = "red"
      )
    } 
  })
  ##--Death 
  output$death_tas <- renderValueBox({
    req(input$district2, input$ta)
    if(input$runreportButton == 0){
      valueBox(
        formatC(0, 
                format = "d", big.mark = ',')
        , paste('Reduction in Deaths', '')
        , icon = icon("stats", lib = 'glyphicon')
        , color = "black"
      )
    }else{
      valueBox(
        tags$p(paste0(formatC(reduc_death_ta(), 
                              format = "d", big.mark = ','), " (", 
                      reduc_death_ta_perc(), "%)"), style = "font-size: 80%;")
        , paste0('Reduction in Deaths', " (", input$ta, ")")
        , icon = icon("stats", lib = 'glyphicon')
        , color = "black"
      )
    } 
  })
  
  
  
  
  
  
  
  
  
  ##--Observe Functions---------------------------------
  
  ##--Observe for inputs--------------------------------
  # observeEvent(input$projection,
  #              shinyFeedback::feedbackWarning(
  #                "projection",
  #               input$projection > min(as.numeric(difftime(as.Date("2021-03-31"), today(), units = "days")),90),
  #                "Please select a smaller number!"
  #              )
  #              )
  stop_function <- reactive({
    result <- input$projection <= 90
    shinyFeedback::feedbackWarning(
      "projection", !result, 
      paste0("Please select a number ", "≤", " ", 90)
    )
    req(result)
    input$projection
  })
  
  output$stop_function <- renderText(paste0("The user may select up to 90 days. Selected number: ", stop_function()))
  
  ##--For masking %
  warning_masking <- reactive({
    result <- input$mask_perc <= 100 & input$mask_perc >=0
    shinyFeedback::feedbackWarning(
      "mask_perc", !result, 
      "Please select a value  ≥ 0 and ≤ 100"
    )
    req(result)
    input$mask_perc
  })
  
  output$warning_masking <- renderText(paste0("Selected: ", warning_masking()))
  
  #-- Length of intervention
  #time_intervention_dist
  warning_dist_days <- reactive({
    result <- input$time_intervention_dist <= input$projection
    shinyFeedback::feedbackWarning(
      "time_intervention_dist", !result,
      paste0("Please select a value ≤ ", "End of Model ", "selection")
    )
    req(result)
    input$time_intervention_dist
  })
  
  output$warning_dist_days <- renderText(paste0("Selected: ", warning_dist_days()))
  
  ##--For distancing %
  
  warning_distancing <- reactive({
    result <- input$distancing_perc <= 100 & input$distancing_perc >=-100
    shinyFeedback::feedbackWarning(
      "distancing_perc", !result, 
      "Please select a number  ≥ -100 and ≤ 100"
    )
    req(result)
    input$distancing_perc
  })
  
  output$warning_distancing <- renderText(paste0("Selected: ", warning_distancing()))
  
  
  #-- Length of intervention
  #time_intervention_dist
  warning_mask_days <- reactive({
    result <- input$time_intervention_mask <= input$projection
    shinyFeedback::feedbackWarning(
      "time_intervention_mask", !result,
      paste0("Please select a value ≤ ", "End of Model ", "selection")
    )
    req(result)
    input$time_intervention_mask
  })
  
  output$warning_mask_days <- renderText(paste0("Selected: ", warning_mask_days()))
  
  ##--District Level Observe-------------
  districtObs <- observe({
    if(input$level == "District"){
      output$district_ui <- renderUI({
        selectInput('district',
                    'Select a District',
                    choices = district_choices())
      })
      #--UIs district
      output$district_ui_plot1 <- renderUI({plotlyOutput("fig_district")})
      output$district_ui_plot2 <- renderUI({plotlyOutput("fig_district2")})
      output$district_ui_plot3 <- renderUI({plotlyOutput("fig_district3")})
      output$district_ui_plot4 <- renderUI({plotlyOutput("fig_district4")})
      output$district_ui_table <- renderUI({DT::DTOutput("table_district")})
      output$district_ui_reduction <- renderUI({tableOutput("table_reductions_district_abs")})
      output$district_ui_reduction_perc <- renderUI({tableOutput("table_reductions_districts")})
      output$district_title <- renderUI({div(textOutput("plot_district_title"), style = "font-size:25px")})
      output$print_district_table_title <- renderUI({div(textOutput("table_district_title"), style = "font-size:25px")}) 
      output$widgets_district <- renderUI({
        fluidRow(
          valueBoxOutput("cases_districts", width = 3),
          valueBoxOutput("hosp_districts", width = 3),
          valueBoxOutput("icu_districts", width = 3),
          valueBoxOutput("death_districts", width = 3)
        )
      })
      #--UI TA
      output$ta_ui <- renderUI({})
      output$ta_plot1 <- renderUI({})
      output$ta_plot2 <- renderUI({})
      output$ta_plot3 <- renderUI({})
      output$ta_plot4 <- renderUI({})
      output$ta_table <- renderUI({})
      output$ta_table_reduction <- renderUI({})
      output$ta_table_reduction_perc <- renderUI({})
      output$ta_title <- renderUI({})
      output$district_ui2 <- renderUI({})
      output$print_ta_table_title <- renderUI({})
      output$widgets_tas <- renderUI({})
      #--UI National
      output$national_ui <- renderUI({})
      output$national_ui2 <- renderUI({})
      output$national_ui3 <- renderUI({})
      output$national_ui4 <- renderUI({})
      output$national_ui5 <- renderUI({})
      output$national_reduction <- renderUI({})
      output$national_reduction_perc <- renderUI({})
      output$national_title <- renderUI({})
      output$print_national_table_title <- renderUI({})
      output$widgets_national <- renderUI({})
    }
  })
  
  #--TA Level Observe---------------------
  taObs <- observe({
    if(input$level == "TA"){
      ##--UIs TA
      output$district_ui2 <- renderUI({
        selectInput('district2',
                    'Select a District',
                    choices = district_choices())
      })
      output$ta_ui <- renderUI({req(input$district2)
        selectInput('ta',
                    'Select a TA', 
                    choices = tas_names2 %>% 
                      filter(Lvl3 == input$district2) %>% 
                      select(Lvl4) %>% pull())})
      output$ta_plot1 <- renderUI({plotlyOutput("fig_ta")})
      output$ta_plot2 <- renderUI({plotlyOutput("fig_ta2")})
      output$ta_plot3 <- renderUI({plotlyOutput("fig_ta3")})
      output$ta_plot4 <- renderUI({plotlyOutput("fig_ta4")})
      output$ta_table <- renderUI({DT::DTOutput("table_ta")})
      output$ta_table_reduction <- renderUI({tableOutput("table_reductions_ta_abs")})
      output$ta_table_reduction_perc <- renderUI({tableOutput("table_reductions_ta_perc")})
      output$ta_title <- renderUI({div(textOutput("plot_ta_title"), style = "font-size:25px")})
      output$print_ta_table_title <- renderUI({div(textOutput("ta_table_title"), style = "font-size:25px")})
      output$widgets_tas <- renderUI({
        fluidRow(
          valueBoxOutput("cases_tas", width = 3),
          valueBoxOutput("hosp_tas", width = 3),
          valueBoxOutput("icu_tas", width = 3),
          valueBoxOutput("death_tas", width = 3)
        )
      })
      
      ##--UIs National
      output$national_ui <- renderUI({})
      output$national_ui2 <- renderUI({})
      output$national_ui3 <- renderUI({})
      output$national_ui4 <- renderUI({})
      output$national_ui5 <- renderUI({})
      output$national_reduction <- renderUI({})
      output$national_reduction_perc <- renderUI({})
      output$national_title <- renderUI({})
      output$print_national_table_title <- renderUI({})
      output$widgets_national <- renderUI({})
      #--UIs district
      output$district_ui <- renderUI({})
      output$district_ui_plot1 <- renderUI({})
      output$district_ui_plot2 <- renderUI({})
      output$district_ui_plot3 <- renderUI({})
      output$district_ui_plot4 <- renderUI({})
      output$district_ui_table <- renderUI({})
      output$district_ui_reduction <- renderUI({})
      output$district_ui_reduction_perc <- renderUI({})
      output$district_title <- renderUI({})
      output$print_district_table_title <- renderUI({})
      output$widgets_district <- renderUI({})
    }
  })
  
  ##--National Observe------------------
  nationalObs <- observe({
    if(input$level == "National"){
      ##--District
      output$district_ui <- renderUI({})
      output$district_ui_plot1 <- renderUI({})
      output$district_ui_plot2 <- renderUI({})
      output$district_ui_plot3 <- renderUI({})
      output$district_ui_plot4 <- renderUI({})
      output$district_ui_table <- renderUI({})
      output$district_ui_reduction <- renderUI({})
      output$district_ui_reduction_perc <- renderUI({})
      output$district_title <- renderUI({})
      output$print_district_table_title <- renderUI({})
      output$widgets_district <- renderUI({})
      ##--TA
      output$ta_ui <- renderUI({})
      output$ta_plot1 <- renderUI({})
      output$ta_plot2 <- renderUI({})
      output$ta_plot3 <- renderUI({})
      output$ta_plot4 <- renderUI({})
      output$ta_table <- renderUI({})
      output$ta_table_reduction <- renderUI({})
      output$ta_table_reduction_perc <- renderUI({})
      output$ta_title <- renderUI({})
      output$district_ui2 <- renderUI({})
      output$print_ta_table_title <- renderUI({})
      output$widgets_tas <- renderUI({})
      ##--National
      output$national_ui <- renderUI({plotlyOutput("fig")})
      output$national_ui2 <- renderUI({plotlyOutput("fig_country2")})
      output$national_ui3 <- renderUI({plotlyOutput("fig_country3")})
      output$national_ui4 <- renderUI({plotlyOutput("fig_country4")})
      output$national_ui5 <- renderUI({DT::DTOutput('table_national')})
      output$national_reduction <- renderUI({tableOutput('table_reductions_country_abs')})
      output$national_reduction_perc <- renderUI({tableOutput('table_reductions_country')})
      output$national_title <- renderUI({div(textOutput("plot_national_title"), style = "font-size:25px")})
      output$print_national_table_title <- renderUI({div(textOutput("national_table_title"), style = "font-size:25px")})
      output$widgets_national <- renderUI({
        fluidRow(
          valueBoxOutput("cases", width = 3),
          valueBoxOutput("hosp", width = 3),
          valueBoxOutput("icu", width = 3),
          valueBoxOutput("death", width = 3)
        )
      })
    }
  })
}

# This is a function that runs a simulation and then saves the country-level results to a file 
# for the dashboard to start up with. Only run manually when the model changes.
# The df_ variables at the top of the file must already be initialized
saveInitialDashboardState <- function() {
  model_results <- run_stepwise(df_params, df_locations, df_masking, df_distancing, df_seed, n_days)
  df_country <- model_results$country
  
  df_country_infected <- subset(df_country, State=='Infected')
  df_country_newinfected <- subset(df_country, State=='New Infections')
  df_country_hospitalized <- subset(df_country, State=='Hospitalized')
  df_country_critical <- subset(df_country, State=='Critical')
  df_country_deaths <- subset(df_country, State=='Dead')
  
  df_for_dash = data.frame('time'=df_country_infected$Day, 'Cases_sq'=df_country_infected$People)
  df_for_dash$date = as.Date("2020-04-01") + (df_for_dash$time - 1)
  df_for_dash$Cases_sq = round(cumsum(df_country_infected$People))
  df_for_dash$Hospitalizations_sq = round(cumsum(df_country_hospitalized$People))
  df_for_dash$ICU_sq = round(cumsum(df_country_critical$People))
  df_for_dash$Death_sq = round(cumsum(df_country_deaths$People))
  df_for_dash$Severe_sq = df_for_dash$Hospitalizations_sq + df_for_dash$ICU_sq + df_for_dash$Death_sq
  
  df_for_dash$Cases_sim = round(cumsum(df_country_infected$People))
  df_for_dash$Hospitalizations_sim = round(cumsum(df_country_hospitalized$People))
  df_for_dash$ICU_sim = round(cumsum(df_country_critical$People))
  df_for_dash$Death_sim = round(cumsum(df_country_deaths$People))
  df_for_dash$Severe_sim = df_for_dash$Hospitalizations_sim + df_for_dash$ICU_sim + df_for_dash$Death_sim
  
  write.csv(df_for_dash, '../dashboard/in/saved-initial-dashboard-state.csv')
}

# ONLY RUN MANUALLY
# saveInitialDashboardState()

##--App-------------
shiny::shinyApp(ui, server)