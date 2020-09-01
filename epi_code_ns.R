set.seed(1234)
setwd("C:\\Users\\Noah\\Documents\\wsl\\git\\git\\africa-covid-work\\africa-covid-work")

library(tidyverse)

main <- function(){
  
  ## load reductions
  reductions_path <- "inputs/reductionScenarios"
  reductions <- load_reductions(reductions_path)
  
  ## load inputs
  inputs_path <- "inputs/MW COVID Inputs.csv"
  exclude_list <- list(17, 189, 166, 28, 34)
  inputs <- load_inputs(inputs_path, exclude_list)
  
  ## load seed dates
  date_path <- "inputs/MW_COVID_startDate_5days.csv"
  seed_dates <- load_seed_dates(date_path)
  
  ### Fixed contact rates
  params <- c(
    e2p = .43,
    e2a = .5,
    e2e = .07,
    a2p = .13,
    a2a = .5,
    a2e = .38,
    p2p = .09,
    p2a = .39,
    p2e = .52,
    kappa = 1 / 2.6, #time to infectiousness
    kappa2 = 1 / 2.6, #rest of infectious time and time to symptomatic
    tau = 1 / 8, #recovery rate for hospitalized cases
    tau2 = 1 / 16, #recovery rate for ICU cases
    R0 = 2.2, #basic reproductive number
    efficacy = .5, #assumed reduction of R0 via mask compliance
    compliance = .1 #assumed mask usage
  ) 
  
  init <- data.frame(UID = inputs$UID, S = inputs$Population - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, inci = 0, hosp = 0, crits = 0)
  
  
  
}



load_seed_dates <- function(date_path){
  MW_start_dates <- read_csv("inputs/MW_COVID_startDate_5days.csv") %>%
    filter(!(UID %in% list(17, 189)))
  ##Do some adjusting for start dates
  # t_from0df <- MW_start_dates %>%
  #   filter(UID == UIDlist[i])
  # t_from0 <- t_from0df$Date_from_0 + 1
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

load_inputs <-function(filename, exclude_list){
  ### 
  MW_COVID_Inputs <- read_csv(filename)
  MW_COVID_Inputs <- MW_COVID_Inputs %>% 
    group_by(UID) %>% 
    mutate(tot_pop = sum(Population)) %>%
    ungroup() %>%
    filter(!(UID %in% exclude_list))
  return(MW_COVID_Inputs)
}

### prep COVID inputs






### age bands
Pediatrics <- list(.09, .39, .52)
Adults <- list(.13, .50, .38)
Elderly <- list(.43, .50, .07)
ageBands <- list("Pediatrics" = Pediatrics, "Adults" = Adults, "Elderly" = Elderly)



run_model_for_reduction <- function(reductions, inputs){
    
  
  
}



run_model <- function(inputs, params, start_dates, init, times){
  cat("running model for UID", inputs$UID)
  sim <- as.data.table(lsoda(init, times, model, parms))
  sim$lvl2 <- inputs$lvl2
  sim$lvl3 <- inputs$lvl3
  sim$lvl4 <- inputs$lvl4
  sim$ID <- inputs$UID
  sim$POP <- inputs$pop_range
  sim$age <- inputs$Age
  sim$tot_pop <- inputs$tot_pop
  # sim$start <- t_from0
  
  return(sim)

}