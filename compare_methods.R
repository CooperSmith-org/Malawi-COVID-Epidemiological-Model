### the following scripts creates plots, a list of plots meant for inspecting the
### difference between the stepwise and lsoda epi models
### indexing into the first layer of the list selects the comparison variable
### for example plots[['r0']] will select the plots run with a high or low r0
### indexing into the second layer of the list selects plots for the low or high
### form of the variable
### for example, plots[['r0']][['high]] will select plots for all variables with 
### r0 taking the high value
### index one layer further to select a specific plot
### for example, plots[['r0']][['high']][['New Infections']] to see the plot for
### new infections.  This can readily be compared with plots[['r0']][['low']][['New Infections']]



library(ggplot2)
library(tidyverse)
library(cowplot)

setwd("/Users/nselman/cs/git/Malawi-COVID-Epidemiological-Model/inputs")

source('../epi_model.R')
source('../epi_stepwise.R')
source('../plotting.R')

### setup
setup <- function() {
  distancing_path <- 'reductionScenarios/current.csv'
  masking_path <- 'masking/masking_compliance.csv'
  inputs_path <- 'MW COVID Inputs.csv'
  
  params_path <- 'params_standard.csv'
  params <- read_csv(params_path)
  return(list(distancing_path, masking_path, inputs_path, params))  
}

run_compare <- function(params, masking_path, distancing_path, inputs) {
  df <- data.frame(row.names = c('Day', 'State', 'People', 'Age', 'Model', 'Run Name', 'Compare', 'Low_High'))
  for (row in 1:nrow(params)) {
    param_row <- params[row,]
    fixed_params <- sapply(param_row %>% 
                             select(eld2ped:susceptibility_p), 
                           as.numeric)
    inits <- param_row %>% select(E_e:crits_p)
    
    ## LSODA
    lsoda_results <- run_model_for_params(fixed_params, inits, masking_path, inputs)
    
    
    ### Reformat Output Like Stepwise
    lsoda_results <- lsoda_results %>%
      select(!starts_with('new') & (ends_with('_p') | ends_with('_a') | ends_with('_e')), starts_with('new_I') & !ends_with('all'), Day = time, TA_Code) %>%
      pivot_longer(ends_with('_p') | ends_with('_a') | ends_with('_e'), names_to = "State") %>%
      mutate(Age = case_when(str_sub(State, -1) == 'p' ~ 'Pediatric',
                             str_sub(State, -1) == 'a' ~ 'Adult',
                             str_sub(State, -1) == 'e' ~ 'Elderly',
                             TRUE ~ 'OTHER'),
             State = str_sub(State, 1, 1),
             Model = 'Lsoda',
             'Run Name' = param_row[['output_name']],
             'Compare' = param_row[['comparison_var']],
             'Low_High' = param_row[['low_high']]) %>%
      rename(People=value)
    lsoda_results$State <-recode(lsoda_results$State, 'S'='Susceptible', 'E'='Exposed', 'I'='Infected',
                                 'H'='Hospitalized', 'C'='Critical', 'R'='Recovered', 'n'='New Infections',
                                 'D'='Dead')
    
    # return(lsoda_results)
    
    ## Stepwise
    stepwise_results <- execute_stepwise(inputs, masking_path, distancing_path, param_row)
    # return(stepwise_results)
    stepwise_results <- stepwise_results %>%
      select(TA_Code, Day, State, People, Age) %>%
      # filter(State != 'New Infections') %>%
      mutate(Model = 'Stepwise',
             'Run Name' = param_row[['output_name']],
             'Compare' = param_row[['comparison_var']],
             'Low_High' = param_row[['low_high']])
    # return(stepwise_results)

    
    df <- rbind(df, lsoda_results, stepwise_results)
    # df[['Run Name']] <- param_row[['output_name']]
  }
  return(df)
}

compare_model_counts <- function(df){
  out <- df %>%
    select(Day, State, People, Age, Model, TA_Code) %>%
    filter(State %in% c('Dead', "Infected")) %>%
    pivot_wider(id_cols=c(Day, TA_Code, Age, State), names_from=Model, values_from=People)  
  return(out)
}

execute_comparison <- function() {

  setup_vals <- setup()
  distancing_path <- setup_vals[[1]]
  masking_path <- setup_vals[[2]]
  inputs_path <- setup_vals[[3]]
  params <- setup_vals[[4]]
  
  all_inputs <- read_csv(inputs_path)
  row_names = c('Day', 'TA_Code', 'Age', 'State', 'Lsoda', 'Stepwise')
  df_full <- data.frame(row.names = row_names)
  
  for (code in unique(all_inputs$TA_Code)) {
    print(paste("Running for TA", code))
    inputs <- filter(all_inputs, TA_Code==code)
    out <- run_compare(params, masking_path, distancing_path, inputs)
    out <- compare_model_counts(out)
    df_full <- rbind(df_full, out)
  }
  return(df_full)
}




# plots <- plot_comparison(df)


