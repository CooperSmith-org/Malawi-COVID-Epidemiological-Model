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
library(icesTAF)
library(radiant.data)

#Bring in inputs - add additional files in this format here!
MW_COVID_Inputs <- read_csv("inputs/MW COVID Inputs.csv")

#Grab the reduction scenarios
files <- list.files("inputs/currentTest", full.names = TRUE) #For within countries
reductions <- lapply(files, read_csv)
names(reductions) <-gsub(".csv","",
                         list.files("inputs/currrentTest", full.names = FALSE), #For within countries
                         fixed = TRUE)

combined_data <- MW_COVID_Inputs %>% 
  summarise(Hospitalization = weighted.mean(Hospitalization, Population), `Critical Care` = weighted.mean(`Critical Care`, Population),
            `Crit of Hosp` = weighted.mean(`Crit of Hosp`, Population), CFR = weighted.mean(CFR, Population), `CFR of Crit` = weighted.mean(`CFR of Crit`, Population),
            `CFR of Hosp` = weighted.mean(`CFR of Hosp`, Population), Population = sum(Population))
combined_data$UID <- 1
combined_data$Lvl1 <- "Sub-Saharan Africa"
combined_data$Lvl2 <- "Malawi"
combined_data$Lvl3 <- "All"
combined_data$Lvl4 <- "All"
combined_data$Run <- "Malawi"

#Modify based on scenario in question
countryList <- list("Malawi")

#probabilistic sensitivity analysis
for (runNum in seq(1,1000)){
  for (c in countryList){
    for (r in 1:length(reductions)){
      data_use <- filter(combined_data, combined_data$Run == c)
      pop_range <- data_use$Population #TA population total estimate
      # eta_range <- list(data_use$Hospitalization) #estimated age-standardized hospitalization rate
      # eta2_range <- list(data_use$`Crit of Hosp`) #estimated age-standardized ICU rate AMONG those hospitalized
      # ep_range <-  list(data_use$`CFR of Crit`) #estimated age-standardized fatality rate AMONG ICU patients
      lvl2 <-   data_use$`Lvl2` # name of country
      lvl3 <-   data_use$`Lvl3` # name of region
      lvl4 <-   data_use$`Lvl4` # name of district
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
        parms["eta"] <- rnorm(1, mean=data_use$Hospitalization, sd=weighted.sd(MW_COVID_Inputs$Hospitalization, MW_COVID_Inputs$Population))
        parms["eta2"] <- rnorm(1, mean=data_use$`Crit of Hosp`, sd=weighted.sd(MW_COVID_Inputs$`Crit of Hosp`, MW_COVID_Inputs$Population))
        parms["epsilon"] <- rnorm(1, mean=data_use$`CFR of Crit`, sd=weighted.sd(MW_COVID_Inputs$`CFR of Crit`, MW_COVID_Inputs$Population))
        parms["reductionList"] <- list(reductions[[r]]$reduc)
        init <- c(S = pop_range[i] - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, inci = 0, hosp = 0, crits = 0)
        times <- seq(1,365)
        sim <- as.data.table(lsoda(init, times, model, parms))
        sim$lvl2 <- lvl2[i]
        sim$lvl3 <- lvl3[i]
        sim$lvl4 <- lvl4[i]
        sim$ID <- UID[i]
        sim$POP <- pop_range[i]
        sim$runNum <- runNum

        #Use below for in-country
        if (UID[i] != "N/A"){
        write.csv(sim, paste0("epi_csvs/Sensitivity/",runNum,".csv"))}
      }
    }
  }
}

sensitivity <- makeCombinedDF("epi_csvs/Sensitivity")

makeComparisonGraphSensitivity <- function(df, diseaseState, title, fileName) {
  df %>%
    filter(State %in% c(diseaseState)) %>%
    ggplot(aes(x=time, y=value, group=runNum, color=runNum)) +
    geom_line(size=.1) +
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal() #+
  ggsave(fileName, height=4 , width =8)
}

sensitivityNew <- sensitivity %>%
  group_by(time, runNum) %>%
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

longDataS <- melt(sensitivityNew, id = c("time", "runNum"))

names(longDataS)[names(longDataS)=="variable"] <- "State"
options(scipen=10000) #Override scientific notation default

makeComparisonGraphSensitivity(longDataS, "Infected", "Number of infected individuals across different model runs testing variable sensitivity", "images/Sensitivity/current_infection.png")
makeComparisonGraphSensitivity(longDataS, "Deaths", "Number of deaths across different model runs testing variable sensitivity", "images/Sensitivity/current_deaths.png")

makeSummaryCSVSensitivity <- function(df, filename) {
  dftocsv <- df %>%
    group_by(runNum) %>%
    summarise(Population = max(POP), Incidence = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
  write.csv(dftocsv, filename)
}

makeSummaryCSVSensitivity(sensitivity, "Sensitivity/enforcedrestrictions21day.csv")


### added new analysis for sensitivity 
makeComparisonGraphSensitivity <- function(df, diseaseState, title, fileName) {
  df %>%
    filter(State %in% c(diseaseState)) %>%
    ggplot(aes(x=time, y=value, group=runNum, color=runNum)) +
    geom_line(size=.1) +
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal() #+
  ggsave(fileName, height=4 , width =8)
}

sensitivityNew <- sensitivity %>%
  group_by(time, runNum) %>%
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

longDataS <- melt(sensitivityNew, id = c("time", "runNum"))

names(longDataS)[names(longDataS)=="variable"] <- "State"
options(scipen=10000) #Override scientific notation default

makeComparisonGraphSensitivity(longDataS, "Infected", "Number of infected individuals across different model runs testing variable sensitivity", "images/Sensitivity/current_infection.png")
makeComparisonGraphSensitivity(longDataS, "Deaths", "Number of deaths across different model runs testing variable sensitivity", "images/Sensitivity/current_deaths.png")

makeSummaryCSVSensitivity <- function(df, filename) {
  dftocsv <- df %>%
    group_by(runNum) %>%
    summarise(Population = max(POP), Incidence = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
  write.csv(dftocsv, filename)
}

makeSummaryCSVSensitivity(sensitivity, "Sensitivity/enforcedrestrictions21day.csv")