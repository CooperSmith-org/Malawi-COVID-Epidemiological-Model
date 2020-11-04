library(tidyverse)
library(deSolve)


## to run, type execute('inputs/params_inits_template.csv') Or other file path
set.seed(1234)

# setwd("C:\\Users\\Noah\\Documents\\wsl\\git\\git\\africa-covid-work\\africa-covid-work")
# setwd("C:/Users/Michael/Git/Malawi-COVID-Epidemiological-Model")


execute <- function(params_df_path){
  ### runs model for all sets of params in params file
  ### params_df_path is the path to the parameters csv
  
  ## load params
  params_df <- load_params(params_df_path)
  
  for (param_row in 1:nrow(params_df)){
    print(paste0("Executing for parameter row ", param_row, "..."))
    data <- params_df[param_row, ]
    
    ## set fixed and initial values
    fixed_params <- sapply(data %>% 
                             select(eld2ped:susceptibility_p), 
                           as.numeric)
    inits <- data %>% select(E_e:crits_p)
    
    output_name <- data['output_name'][[1]]
    print(paste("output name", output_name, '\n'))
    date_threshold = data['seed_date_threshold'][[1]]
    masking_compliance = data['compliance_path'][[1]]

    print(paste('data_threshold ', date_threshold, '\n'))
    outpath <- setup(output_name)
    write_csv(data, file.path(outpath, "params.csv"))
    sim = run_model_for_params(fixed_params, inits, date_threshold, masking_compliance, inputs_path, date_path)
  }
}

setup <- function(outpath=NULL){
  ## creates directory for output and returns path
  
  if (is.null(outpath)){
    outpath <- file.path("epi_csvs", "Malawi", Sys.Date())
    print(paste("Outpath set to", outpath))
  }
  else {
    outpath <- file.path('epi_csvs', 'Malawi', outpath)
    print(paste("outpath:", outpath, '\n', sep=' '))
  }
  
  unlink(outpath, recursive=TRUE)
  dir.create(file.path(outpath))
  return(outpath)
}


run_model_for_params <- function(fixed_params, inits, date_threshold=1, masking_compliance, inputs_path, date_path){
  ## runs model for fixed params and init
  time1 <- Sys.time()
  
  ## load reductions
  reductions_path <- "reductionScenarios"
  reductions <- load_reductions(reductions_path)
  
  ## load masking
  fixed_params['compliance'] <- read_csv(file.path('masking', masking_compliance))
  
  
  ## load inputs
  # inputs_path <- "inputs/MW COVID Inputs.csv"
  # exclude_list <- list(17, 189, 166, 28, 34, 167, 100, 421, 99, 180, 230) ## excluded for various reasons - most often no population in an age group
  inputs <- load_inputs(inputs_path, inits)
  
  ## load seed dates
  # date_path <- "inputs/simulation-seeddates-ta-20200910.csv"
  seed_dates <- load_seed_dates(date_path, date_threshold)
  inputs <- left_join(inputs, seed_dates, by=c("TA_Code"="adm_id"))
  
  init_names <- inputs %>% select(ends_with("_e") | 
                                    ends_with("_a") | 
                                    ends_with("_p")) %>% colnames()
  
  ## iterate through reduction scenarios
  for (r in seq(1:length(reductions))){
    

    ## actually run model
    sim = apply(inputs, 1, run_model, fixed_params,
                init_names, reduction_name)
    
    ## aggregate results for all TAs
  return(sim)
  }
  
  ## timing not working for unknown reason
  time2 = Sys.time()
  delta = time2-time1
  print(delta)
  print(paste("Elapsed time:", delta))
}

load_seed_dates <- function(date_path, n){
  ### n refers to the threshold for selecting days
  MW_start_dates <- read_csv(date_path)
  MW_start_dates <- MW_start_dates %>%
    select(adm_id, start_day = paste0('day_n', n),
           start_date = paste0('date_n', n))
  
  return(MW_start_dates)
}


load_reductions <- function(relative_path){
  ### relative path describes the folder
  files <- list.files(relative_path, full.names = TRUE)
  reductions <- lapply(files, read_csv)
  names(reductions) <-gsub(".csv", "",
                           list.files(relative_path,
                                      full.names = FALSE),
                           fixed = TRUE)
  return(reductions)
}

load_masking_compliance <- function(path){
  masking_compliance <- read_csv(path)
  return(masking_compliance)
}


load_inputs <-function(filename, inits){
  ### 
  inputs <- read_csv(filename)
  inputs <- inputs %>%
    gather(var, val, (Hospitalization:Population)) %>%
    unite(temp, Age, var) %>%
    spread(temp, val)
  print('inits')
  print(inits)
  print('params')
  print(params)
  
  inputs$S_e = inputs$Elderly_Population-inits['E_e']
  inputs$E_e = inits['E_e']
  inputs$I_e = inits['I_e']
  inputs$H_e = inits['H_e']
  inputs$C_e = inits['C_e']
  inputs$R_e = inits['R_e']
  inputs$D_e = inits['D_e']
  inputs$S_a = inputs$Adults_Population-inits['E_a']
  inputs$E_a = inits['E_a']
  inputs$I_a = inits['I_a']
  inputs$H_a = inits['H_a']
  inputs$C_a = inits['C_a']
  inputs$R_a = inits['R_a']
  inputs$D_a = inits['D_a']
  inputs$S_p = inputs$Pediatrics_Population-inits['E_p']
  inputs$E_p = inits['E_p']
  inputs$I_p = inits['I_p']
  inputs$H_p = inits['H_p']
  inputs$C_p = inits['C_p']
  inputs$R_p = inits['R_p']
  inputs$D_p = inits['D_p']
  
  
  return(inputs)
}


build_params <- function(input_row, params){
  new_params <- params
  new_params['population_e'] <- input_row['Elderly_Population']
  new_params['eta_e'] <- input_row['Elderly_Hospitalization']
  new_params['eta2_e'] <- input_row['Elderly_Crit_of_Hosp']
  new_params['epsilon_e'] <- input_row['Elderly_FR_of_Crit']
  new_params['population_a'] <- input_row['Adults_Population']
  new_params['eta_a'] <- input_row['Adults_Hospitalization']
  new_params['eta2_a'] <- input_row['Adults_Crit_of_Hosp']
  new_params['epsilon_a'] <- input_row['Adults_FR_of_Crit']
  new_params['population_p'] <- input_row['Pediatrics_Population']
  new_params['eta_p'] <- input_row['Pediatrics_Hospitalization']
  new_params['eta2_p'] <- input_row['Pediatrics_Crit_of_Hosp']
  new_params['epsilon_p'] <- input_row['Pediatrics_FR_of_Crit']
  
  return(new_params)
}


run_model <- function(
  inputs, params, init_names, reduction_name){
  ### runs model using lsoda for a TA
  print(paste("running model for UID", inputs['UID']))
  params <- build_params(inputs, params)
  params <- lapply(params, as.numeric)
  init <- inputs[names(inputs) %in% init_names]
  init <- sapply(init, as.numeric)
  times <- seq(from=inputs['start_day'], to=365)
  
  sim <- as.data.frame(
    lsoda(y=init, times=times, func=model, parms=params))
  sim['Elderly_Population'] <- init['S_e']
  sim['Adults_Population'] <- init['S_a']
  sim['Pediatrics_Population'] <- init['S_p']
  sim <- calc_deltas(sim, params)
  sim <- aggregate_age_groups(sim)
  
  for (n in names(inputs)){
    if (!(n %in% names(sim))){
      sim[n] <- inputs[n]
    }
  }
  
  # filename <- paste(reduction_name, inputs['TA_Code'], Sys.Date(), sep="_")
  # write_csv(sim, file.path(outpath, paste0(filename, ".csv")))
}  


model <- function(times, init, parms) {
  ### model is an argument for lsoda, see lsoda documentation for details
  with(as.list(c(init, parms)), {
    beta_e2p <- (1-reductions[times])*susceptibility_e*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*eld2ped/(population_p)
    beta_e2a <- (1-reductions[times])*susceptibility_e*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*eld2ad/(population_a)
    beta_e2e <- (1-reductions[times])*susceptibility_e*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*eld2eld/(population_e)
    beta_a2p <- (1-reductions[times])*susceptibility_a*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*ad2ped/(population_p)
    beta_a2a <- (1-reductions[times])*susceptibility_a*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*ad2ad/(population_a)
    beta_a2e <- (1-reductions[times])*susceptibility_a*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*ad2eld/(population_e)
    beta_p2p <- (1-reductions[times])*susceptibility_p*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*ped2ped/(population_p)
    beta_p2a <- (1-reductions[times])*susceptibility_p*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*ped2ad/(population_a)
    beta_p2e <- (1-reductions[times])*susceptibility_p*(1 - compliance[times]*efficacy)*R0*(1/(1/kappa+1/kappa2))*ped2eld/(population_e)
    
    dS_e <- -(beta_e2e * (E_e + I_e) + beta_e2a * (I_a + E_a) + beta_e2p * (E_p + I_p)) * S_e #susceptible
    dE_e <- (beta_e2e * (E_e + I_e) + beta_e2a * (I_a + E_a) + beta_e2p * (E_p + I_p)) * S_e -  E_e * kappa #exposed but asymptomatic
    dI_e <- E_e * kappa - I_e * kappa2 #infectious, but mild severity
    dH_e <- eta_e * I_e  * kappa2 - tau * H_e #hospitalized
    dC_e <- eta2_e* tau * H_e  - tau2 * C_e #critical care
    dR_e <- (1 - eta_e) * I_e * kappa2 + (1 - eta2_e) * tau * H_e + (1 - epsilon_e) * tau2 * C_e #recovered
    dD_e <- epsilon_e * tau2 * C_e #dead
    
    dS_a <- -(beta_a2e * (E_e + I_e) + beta_a2a * (I_a + E_a) + beta_a2p * (E_p + I_p)) * S_a #susceptible
    dE_a <- (beta_a2e * (E_e + I_e) + beta_a2a * (I_a + E_a) + beta_a2p * (E_p + I_p)) * S_a -  E_a * kappa #exposed but asymptomatic``
    dI_a <- E_a * kappa - I_a * kappa2 #infectious, but mild severity
    dH_a <- eta_a * I_a  * kappa2 - tau * H_a #hospitalized
    dC_a <- eta2_a * tau * H_a  - tau2 * C_a #critical care
    dR_a <- (1 - eta_a) * I_a * kappa2 + (1 - eta2_a) * tau * H_a + (1 - epsilon_a) * tau2 * C_a #recovered
    dD_a <- epsilon_a * tau2 * C_a #dead
    
    dS_p <- -(beta_p2e * (E_e + I_e) + beta_p2a * (I_a + E_a) + beta_p2p * (E_p + I_p)) * S_p #susceptible
    dE_p <- (beta_p2e * (E_e + I_e) + beta_p2a * (I_a + E_a) + beta_p2p * (E_p + I_p)) * S_p -  E_p * kappa #exposed but asymptomatic``
    dI_p <- E_p * kappa - I_p * kappa2 #infectious, but mild severity
    dH_p <- eta_p * I_p  * kappa2 - tau * H_p #hospitalized
    dC_p <- eta2_p * tau * H_p - tau2 * C_p #critical care
    dR_p <- (1 - eta_p) * I_p * kappa2 + (1 - eta2_p) * tau * H_p + (1 - epsilon_p) * tau2 * C_p #recovered
    dD_p <- epsilon_p * tau2 * C_p #dead
    
    return(list(c(
      dS_e, dE_e, dI_e, dH_e, dC_e, dR_e, dD_e,
      dS_a, dE_a, dI_a, dH_a, dC_a, dR_a, dD_a,
      dS_p, dE_p, dI_p, dH_p, dC_p, dR_p, dD_p)))
  })
}


stack_results <- function(path){
  ## returns df that is the stack of all csvs in path (directory)
  
  print(paste('the path in stacked files is', path))
  file_list <- lapply(list.files(path), function(x) file.path(path, x))
  df_list <- lapply(file_list, read_csv)
  combined_df <- do.call(rbind, df_list)
  return(combined_df)
}

rename_cols <- function(df){
  df <- df %>%
    rename(
      Elderly_Susceptible=S_e,
      Elderly_Exposed=E_e,
      Elderly_Infected=I_e,
      Elderly_Hospitalizations=H_e,
      Elderly_Critical=C_e,
      Elderly_Recovered=R_e,
      Elderly_Dead=D_e,
      
      Adult_Susceptible=S_a,
      Adult_Exposed=E_a,
      Adult_Infected=I_a,
      Adult_Hospitalizations=H_a,
      Adult_Critical=C_a,
      Adult_Recovered=R_a,
      Adult_Dead=D_a,
      
      Pediatric_Susceptible=S_p,
      Pediatric_Exposed=E_p,
      Pediatric_Infected=I_p,
      Pediatric_Hospitalizations=H_p,
      Pediatric_Critical=C_p,
      Pediatric_Recovered=R_p,
      Pediatric_Dead=D_p,
      
      Total_Susceptible=S_all,
      Total_Exposed=E_all,
      Total_Infected=I_all,
      Total_Hospitalizations=H_all,
      Total_Critical=C_all,
      Total_Recovered=R_all,
      Total_Dead=D_all
    )
  return(df)
}

summarize_output <- function(df){
  ## aggregates measures of interest for all simulation runs in reduction scenario
  ## df should be output of stack_results
  df <- df %>%
    mutate(
      Population = Pediatrics_Population
      + Adults_Population
      + Elderly_Population
    ) %>%
    group_by(time) %>%
    summarise_at(
      vars(
        ends_with('_a'),
        ends_with('_e'),
        ends_with('_p'),
        ends_with('_all'),
        Population
      ),
      sum
    ) %>%
    rename_cols()
  return(df)
}  

load_params <- function(path){
  
  df <- read_csv(path)
  return(df)
  
}

# iterate_through_params <- function(params_df){
#   for (row in 1:nrow(params_df)){
#     data <- params_df[row, ]
#     p <- sapply(data %>% 
#                   select(eld2ped:susceptibility_p), 
#                 as.numeric)
#     init <- data %>% select(E_e:crits_p)
#     # print(p)
#     # print(init)
#     return(list(p, init))
#   }
# }

aggregate_age_groups <- function(df){
  ### creates variables with suffix _all that are sum of _e, _a, and _p
  for (c in c('S_', 'E_', 'I_', 'H_', 'C_', 'R_', 'D_',
              'new_E_', 'new_I_', 'new_H_', 'new_C_', 'new_D_')){
    df[paste0(c, 'all')] <- df %>% 
      select(starts_with(c)) %>% 
      rowSums(na.rm=TRUE)
  }
  return(df)
}

calc_deltas <- function(df, params){
  ### calculates movement in into each compartment (as opposed to change in each compartment)
  ### on each day
  new_df <- df %>%
    
    mutate_at(vars(ends_with('_a'), ends_with('_e'), ends_with('_p')),
              funs(lag))
  colnames(new_df) <- sapply(colnames(new_df), function(x) paste0('lag_', x))
  new_df <- cbind(df, new_df)
  
for (age in c('e', 'a', 'p')){
  new_df[paste0('new_E_', age)] = new_df[paste0('lag_S_', age)] - new_df[paste0('S_', age)]
  new_df[paste0('new_I_', age)] = new_df[paste0('lag_E_', age)] - new_df[paste0('E_', age)] + new_df[paste0('new_E_', age)]   
  new_df[paste0('new_H_', age)] = params[paste0('eta_', age)] * (new_df[paste0('lag_I_', age)] - new_df[paste0('I_', age)] + new_df[paste0('new_I_', age)])
  new_df[paste0('new_C_', age)] = params[paste0('eta2_', age)] * (new_df[paste0('lag_H_', age)] - new_df[paste0('H_', age)] + new_df[paste0('new_H_', age)])
  new_df[paste0('new_D_', age)] = new_df[paste0('D_', age)] - new_df[paste0('lag_D_', age)]
}
new_df[1,] [is.na(new_df[1,])] <- 0 ## NAs appear in first row during lag
new_df <- new_df %>% select(!(contains('lag')))
return(new_df)
}


###run model based on the parameter assumptions in the input template
# execute("inputs/params_inits_template.csv")
