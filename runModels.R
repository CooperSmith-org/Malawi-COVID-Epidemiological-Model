time1 <- Sys.time()

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
MW_starts <- read_csv("inputs/first_cases/seeds_offset_param_35_urb21d_peri28d_rural35d.csv") %>%
  filter(!(ADM3_PCODE %in% list(10106, 20511, 21071, 30303)))
MW_starts$first_case_date <- as.Date(MW_starts$first_case_date, format="%m/%d/%y")
MW_starts$Date_from_0 <- as.numeric(difftime(MW_starts$first_case_date, as.Date("2020-04-02"), units="days"))

MW_COVID_Inputs <- left_join(MW_COVID_Inputs, MW_starts, by=c("TA_Code2" = "ADM3_PCODE"))
MW_starts <- left_join(MW_starts, MW_COVID_Inputs, by=c("ADM3_PCODE" = "TA_Code2"))

#Grab the reduction scenarios
files <- list.files("inputs/reductionScenarios", full.names = TRUE) #For within countries
reductions <- lapply(files, read_csv)
names(reductions) <-gsub(".csv","",
                      list.files("inputs/reductionScenarios", full.names = FALSE), #For within countries
                      fixed = TRUE)

#Add in col to identify the data source
MW_COVID_Inputs$Run <- "Malawi"

#add a total_pop col
MW_COVID_Inputs <- MW_COVID_Inputs %>% 
  group_by(UID) %>% 
  mutate(tot_pop = sum(Population)) %>%
  ungroup() %>%
  filter(!(TA_Code %in% list(10106, 20511, 21071, 30303 )))

combined_data <- MW_COVID_Inputs

#Modify based on scenario in question
countryList <- list("Malawi")

#create matrix lists for contact matrix
Pediatrics <- list(.09, .39, .52)
Adults <- list(.13, .50, .38)
Elderly <- list(.43, .50, .07)
ageBands <- list("Pediatrics" = Pediatrics, "Adults" = Adults, "Elderly" = Elderly)

#Create susceptibility 
suscep <- list("Pediatrics" = .5, "Adults" = .75, "Elderly" = 1.5)

#loop through each district, using the district-specific estimates of population size, hospitalization, ICU, and death
for (c in countryList){
  for (r in 1:length(reductions)){
      data_use <- filter(combined_data, combined_data$Run == "Malawi" & combined_data$Population > 0)
      pop_range <- data_use$Population #district population total estimate
      eta_range <- data_use$Hospitalization #estimated age-standardized hospitalization rate
      eta2_range <- data_use$`Crit of Hosp` #estimated age-standardized ICU rate AMONG those hospitalized
      ep_range <-  data_use$`FR of Crit` #estimated age-standardized fatality rate AMONG ICU patients
      lvl2 <-   data_use$`Lvl2` # name of country
      lvl3 <-   data_use$`Lvl3` # name of region
      lvl4 <-   data_use$`Lvl4` # name of district
      UIDlist <- data_use$UID
      Age <- data_use$`Age Band`
      tot_pop <- data_use$tot_pop
      
      names(reductions[[r]])[names(reductions[[r]])=="x"] <- "reduc"
      
      for(i in 1:length(pop_range)) {
        for (contactR in ageBands[[Age[i]]]){
          parms <- c(population = 0, #population size
                     eta = 0, #proportion of cases who are hospitalized
                     eta2 = 0, #ICU rate of hospitalized cases
                     epsilon = 0, #death rate of ICU cases
                     kappa = 1 / 3.5, #time to infectiousness
                     kappa2 = 1 / 3.5, #rest of infectious time and time to symptomatic
                     tau = 1 / 4, #recovery rate for hospitalized cases
                     tau2 = 1 / 8, #recovery rate for ICU cases
                     R0 = 2.2, #basic reproductive number
                     contact = 0, #assumed contact rate
                     susceptibility = 0, #assumed susceptibility
                     efficacy = .5, #assumed reduction of R0 via mask compliance
                     compliance = .15, #assumed mask usage
                     reductionList = list()) # day 1 assumed baseline reduction
          parms["population"] <- pop_range[i]
          parms["eta"] <- eta_range[i]
          parms["eta2"] <- eta2_range[i]
          parms["epsilon"] <- ep_range[i]
          parms["reductionList"] <- list(reductions[[r]]$reduc)
          parms["contact"] <- contactR
          parms["susceptibility"] <- suscep[[Age[i]]]
          
          ##Do some adjusting for start dates
          t_from0df <- MW_COVID_Inputs %>%
            filter(UID == UIDlist[i] & `Age Band`=="Pediatrics")
          t_from0 <- t_from0df$Date_from_0 + 1

          parms[["reductionList"]] <- parms[["reductionList"]][t_from0:365]
          init <- c(S = pop_range[i] - 1, E = 0, I = 0.333, H = 0, C = 0, R = 0, D = 0, inci = 0, hosp = 0, crits = 0)
          
          times <- seq(1,length(parms[["reductionList"]]))
          sim <- as.data.table(lsoda(init, times, model, parms))
          sim$lvl2 <- lvl2[i]
          sim$lvl3 <- lvl3[i]
          sim$lvl4 <- lvl4[i]
          sim$ID <- UIDlist[i]
          sim$POP <- pop_range[i]
          sim$age <- Age[i]
          sim$tot_pop <- tot_pop[i]
          sim$start <- t_from0

          if (UIDlist[i] != "N/A"){
            write.csv(sim, paste0("epi_csvs/",c,"/banded_",names(reductions[r]),"/",lvl4[i],"-", Age[i],".csv"))}
      }
    }
  }
}

time2 <- Sys.time()
time2 - time1