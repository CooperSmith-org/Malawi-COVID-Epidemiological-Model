######DO NOT RUN WIP ############


set.seed(1234)

library(dplyr)
library(reshape2)
library(deSolve)
library(data.table)
library(ggplot2)
library(scales)
library(readr)
source("utils.R")

#Bring in inputs - add additional files in this format here!
MW_COVID_Inputs <- read_csv("inputs/MW COVID Inputs.csv")
BK_COVID_Inputs <- read_csv("inputs/BFA COVID Inputs.csv")
SSA_COVID_Inputs <- read_csv("inputs/SSA COVID Inputs.csv")

#Grab the reduction scenarios
files <- list.files("inputs/currentTest", full.names = TRUE) #For within countries
reductions <- lapply(files, read_csv)
names(reductions) <-gsub(".csv","",
                         list.files("inputs/currrentTest", full.names = FALSE), #For within countries
                         fixed = TRUE)

#Add in col to identify the data source
MW_COVID_Inputs$Run <- "Malawi"
BK_COVID_Inputs$Run <- "Burkina"
SSA_COVID_Inputs$Run <- "SSA"
combined_data <- rbind(MW_COVID_Inputs, BK_COVID_Inputs, SSA_COVID_Inputs)

#Modify based on scenario in question
countryList <- list("Malawi")

#probabilistic sensitivity analysis
for (runNum in seq(1,1000)){
  for (c in countryList){
    for (r in 1:length(reductions)){
      data_use <- filter(combined_data, combined_data$Run == c)
      pop_range <- data_use$Population #TA population total estimate
      eta_range <- data_use$Hospitalization #estimated age-standardized hospitalization rate
      eta2_range <- data_use$`Crit of Hosp` #estimated age-standardized ICU rate AMONG those hospitalized
      ep_range <-  data_use$`CFR of Crit` #estimated age-standardized fatality rate AMONG ICU patients
      lvl3 <-   data_use$`Lvl3` # name of TA
      UID <- data_use$UID
      
      names(reductions[[r]])[names(reductions[[r]])=="x"] <- "reduc"
      
      for(i in 1:length(pop_range)) {
        parms <- c(population = 0, #population size
                   eta = 0, #proportion of cases who are hospitalized
                   eta2 = 0, #ICU rate of hospitalized cases
                   epsilon = 0, #death rate of ICU cases
                   kappa = 1 / 2.6, #time to infectiousness
                   kappa2 = kappa, #rest of infectious time and time to symptomatic
                   tau = 1 /8, #recovery rate for hospitalized cases
                   tau2 = 1 / 16, #recovery rate for ICU cases
                   R0 = 2.2, #basic reproductive numbe
                   reductionList = list()) # day 1 assumed baseline reduction
        parms["kappa"] <- 1 / rnorm(1, 2.6, 0.5) #time to infectiousness
        parms["kappa2"] <- 1 / rnorm(1, 2.6, 0.5) #NOTE any rreason why i can't do this
        parms["tau"] <- 1 / rnorm(1, 8, 1.5) #recovery rate for hospitalized cases
        parms["tau2"] <- 1 / rnorm(1, 16, 2.0) #recovery rate for ICU cases
        parms["population"] <- pop_range[i]
        parms["eta"] <- eta_range[i] * rnorm(1, mean=mean(data_use$Hospitalization), sd=sd(data_use$Hospitalization))
        parms["eta2"] <- eta2_range[i] * rnorm(1, mean=mean(data_use$`Crit of Hosp`), sd=sd(data_use$`Crit of Hosp`))
        parms["epsilon"] <- ep_range[i] * rnorm(1, mean=mean(data_use$`CFR of Crit`), sd=sd(data_use$`CFR of Crit`))
        parms["reductionList"] <- list(reductions[[r]]$reduc)
        init <- c(S = pop_range[i] - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, hosp = 0, crits = 0)
        times <- seq(1,365)
        sim <- as.data.table(lsoda(init, times, model, parms))
        sim$TA <- lvl3[i]
        sim$ID <- UID[i]
        sim$POP <- pop_range[i]
        sim$runNum <- runNum

        #Use below for in-country
        if (UID[i] != "N/A"){
        write.csv(sim, paste0("epi_csvs/Sensitivity/",runNum,"/",lvl3[i],".csv"))}
      }
    }
  }
}