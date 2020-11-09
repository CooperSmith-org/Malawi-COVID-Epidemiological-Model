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
distancing_path <- 'reductionScenarios/current.csv'
masking_path <- 'masking/masking_compliance.csv'
inputs_path <- 'test_inputs.csv'


## lsoda
# param_row <- read_csv(params_path)
# masking_compliance = param_row[['compliance_path']]
# inputs_path <- 'test_inputs.csv'

params_path <- 'params_compare.csv'
params <- read_csv(params_path)
df <- data.frame(row.names = c('Day', 'State', 'People', 'Age', 'Model', 'Run Name', 'Compare', 'Low_High'))

for (row in 1:nrow(params)) {
  param_row <- params[row,]
  fixed_params <- sapply(param_row %>% 
                           select(eld2ped:susceptibility_p), 
                         as.numeric)
  inits <- param_row %>% select(E_e:crits_p)
  
  ## LSODA
  lsoda_results <- run_model_for_params(fixed_params, inits, masking_path, inputs_path)
  
  ### Reformat Output Like Stepwise
  lsoda_results <- lsoda_results %>%
    select(!starts_with('new') & (ends_with('_p') | ends_with('_a') | ends_with('_e')), starts_with('new_I') & !ends_with('all'), Day = time) %>%
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
  
  ## Stepwise
  stepwise_results <- execute_stepwise(inputs_path, masking_path, distancing_path, param_row)
  stepwise_results <- stepwise_results %>%
    select(Day, State, People, Age) %>%
    # filter(State != 'New Infections') %>%
    mutate(Model = 'Stepwise',
           'Run Name' = param_row[['output_name']],
           'Compare' = param_row[['comparison_var']],
           'Low_High' = param_row[['low_high']])
  
  df <- rbind(df, lsoda_results, stepwise_results)
  # df[['Run Name']] <- param_row[['output_name']]
}

plots <- plot_comparison(df)


