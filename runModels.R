set.seed(1234)

library(dplyr)
library(reshape2)
library(deSolve)
library(data.table)
library(ggplot2)
library(scales)
library(readr)

#SEIR Model of Differential Equations
model <- function(times, init, parms) {
  with(as.list(c(parms, init)), {
    ifelse(times < start0, beta <- R0*kappa/population, 
           ifelse(times < start, beta <- (1-reduction0)*R0*kappa/population, 
                  beta <- (1-reduction)*R0*kappa/population)) # time-dependent R0 if social distancing is implemented. 50% reduction but can be anything
    dS <- -beta * S * I #susceptible
    dE <- beta * S * I -  E * kappa #exposed but asymptomatic
    dI <- E * kappa - I * kappa2 #infectious, but mild severity
    dH <- eta * I  * kappa2 - tau * H #hospitalized
    dC <- eta2 * tau * H  - tau2 * C #critical care
    dR <- (1 - eta) * I * kappa2 + (1 - eta2) * tau * H + (1 - epsilon) * tau2 * C #recovered
    dD <- epsilon * tau2 * C #dead
    hosp <- eta * I  * kappa2 # incident hospitalizations
    crits <- eta2 * tau * H # incident ICUs
    list(c(dS, dE, dI, dH, dC, dR, dD, hosp, crits))
  })
}

#bring in Malawi-specific TA data and SSA data
MW_TA_COVID_Inputs <- read_csv("inputs/MW TA COVID Inputs.csv")
SSA_COVID_Inputs <- read_csv("inputs/SSA COVID Inputs.csv")
combined_data <- rbind(MW_TA_COVID_Inputs, SSA_COVID_Inputs)

pop_range <- combined_data$Population #TA population total estimate
eta_range <- combined_data$Hospitalization #estimated age-standardized hospitalization rate
eta2_range <- combined_data$`Crit of Hosp` #estimated age-standardized ICU rate AMONG those hospitalized
ep_range <-  combined_data$`CFR of Crit` #estimated age-standardized fatality rate AMONG ICU patients
lvl3 <-   combined_data$`Lvl3` # name of TA
lvl2 <-   combined_data$`Lvl2` # adding in a check on country
UID <- combined_data$UID
startList <- list(45) #start dates we are evaluating
reductionList <- list(.15, .35, .50, .75) #potential reductions we are evaluating - note: .15 is our current "baseline"

#loop through each TA, using the TA-specific estimates of population size, hospitalization, ICU, and death
for (s in 1:length(startList)){
  for (r in 1:length(reductionList)){
    for(i in 1:length(pop_range)) {
      parms <- c(population = 0, #population size
                 eta = 0, #proportion of cases who are hospitalized
                 eta2 = 0, #ICU rate of hospitalized cases
                 epsilon = 0, #death rate of ICU cases
                 kappa = 1 / 2.6, #time to infectiousness
                 kappa2 = 1 / 2.6, #rest of infectious time and time to symptomatic
                 tau = 1 / 8, #recovery rate for hospitalized cases
                 tau2 = 1 / 16, #recovery rate for ICU cases
                 R0 = 2.2, #basic reproductive number
                 start = 45, #placeholder
                 start0 = 15,
                 reduction0 = .15,
                 reduction = .15) #baseline reduction
      parms["population"] <- pop_range[i]
      parms["eta"] <- eta_range[i]
      parms["eta2"] <- eta2_range[i]
      parms["epsilon"] <- ep_range[i]
      parms["start"] <- startList[s]
      parms["reduction"] <- reductionList[r]
      init <- c(S = pop_range[i] - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, hosp = 0, crits = 0)
      times <- seq(0,365)
      sim <- as.data.table(lsoda(init, times, model, parms))
      sim$TA <- lvl3[i]
      sim$ID <- UID[i]
      sim$POP <- pop_range[i]
      
      if (UID[i] != "N/A"){
        write.csv(sim, paste0("epi_csvs/",startList[s],"-",reductionList[r],"/",UID[i],".csv"))}
    }
  }
}

df_45_.35 <- list.files(path = "epi_csvs/45-0.35",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_45_.75 <- list.files(path = "epi_csvs/45-0.75",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_45_.15 <- list.files(path = "epi_csvs/45-0.15",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_45_.50 <- list.files(path = "epi_csvs/45-0.5",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

baseline <- list.files(path = "epi_csvs/baseline",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

################**********NEW SCENARIOS*******##########################

new_df_45_.35 <- df_45_.35 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_45_.75 <- df_45_.75 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_45_.15 <- df_45_.15 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_45_.50 <- df_45_.50 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

summary_baseline <- baseline %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

#write summaries to csvs

#########################********NEW SCENARIOS*******########################################################

write.csv(new_df_45_.35, "summary_csv_day/new_summary_45-.35.csv")
write.csv(new_df_45_.15, "summary_csv_day/new_summary_45-.15.csv")
write.csv(new_df_45_.75, "summary_csv_day/new_summary_45-.75.csv")
write.csv(new_df_45_.50, "summary_csv_day/new_summary_45-.50.csv")
write.csv(summary_baseline, "summary_csv_day/baseline.csv")

##################***********NEW EXAMPLES***********##########################

summary_df_45_.35 <- df_45_.35 %>%
  group_by(TA, ID) %>%
  summarise(Population = max(POP), Incidents = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
write.csv(summary_df_45_.35, "summary_by_TA/45-.35.csv")

summary_df_45_.15 <- df_45_.15 %>%
  group_by(TA, ID) %>%
  summarise(Population = max(POP), Incidents = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
write.csv(summary_df_45_.15, "summary_by_TA/45-.15.csv")

summary_df_45_.75 <- df_45_.75 %>%
  group_by(TA, ID) %>%
  summarise(Population = max(POP), Incidents = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
write.csv(summary_df_45_.75, "summary_by_TA/45-.75.csv")

summary_df_45_.50 <- df_45_.50 %>%
  group_by(TA, ID) %>%
  summarise(Population = max(POP), Incidents = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
write.csv(summary_df_45_.50, "summary_by_TA/45-.50.csv")

summary_baseline <- baseline %>%
  group_by(TA, ID) %>%
  summarise(Population = max(POP), Incidents = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
write.csv(summary_baseline, "summary_by_TA/baseline.csv")
