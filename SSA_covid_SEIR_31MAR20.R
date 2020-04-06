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
    ifelse(times < start, beta <- R0*kappa/population, beta <- reduction*R0*kappa/population) # time-dependent R0 if social distancing is implemented. 50% reduction but can be anything
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
MW_TA_COVID_Inputs <- read_csv("MW TA COVID Inputs.csv")
SSA_COVID_Inputs <- read_csv("SSA COVID Inputs.csv")
combined_data <- rbind(MW_TA_COVID_Inputs, SSA_COVID_Inputs)

pop_range <- combined_data$Population #TA population total estimate
eta_range <- combined_data$Hospitalization #estimated age-standardized hospitalization rate
eta2_range <- combined_data$`Crit of Hosp` #estimated age-standardized ICU rate AMONG those hospitalized
ep_range <-  combined_data$`CFR of Crit` #estimated age-standardized fatality rate AMONG ICU patients
lvl3 <-   combined_data$`Lvl3` # name of TA
lvl2 <-   combined_data$`Lvl2` # adding in a check on country
UID <- combined_data$UID
startList <- list(30, 45, 60) #start dates we are evaluating
reductionList <- list(.25, .5, .75) #potential reductions we are evaluating - note: 1 is baseline

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
                     start = 0, #start date for the model
                     reduction = .5) #baseline reduction
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

#grab the baseline
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
             start = 0, #start date for the model
             reduction = .5) #baseline reduction
  parms["population"] <- pop_range[i]
  parms["eta"] <- eta_range[i]
  parms["eta2"] <- eta2_range[i]
  parms["epsilon"] <- ep_range[i]
  parms["start"] <- 0
  parms["reduction"] <- 1.0
  init <- c(S = pop_range[i] - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, hosp = 0, crits = 0)
  times <- seq(0,365)
  sim <- as.data.table(lsoda(init, times, model, parms))
  sim$TA <- lvl3[i]
  sim$ID <- UID[i]
  sim$POP <- pop_range[i]
  
  if (UID[i] != "N/A"){
    write.csv(sim, paste0("epi_csvs/baseline/",UID[i],".csv"))}
}

#Write csvs
#load + concatenate the csvs for each scenario
df_30_.25 <- list.files(path = "epi_csvs/30-0.25",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_30_.5 <- list.files(path = "epi_csvs/30-0.5",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_30_.75 <- list.files(path = "epi_csvs/30-0.75",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_45_.25 <- list.files(path = "epi_csvs/45-0.25",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_45_.5 <- list.files(path = "epi_csvs/45-0.5",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_45_.75 <- list.files(path = "epi_csvs/45-0.75",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_60_.25 <- list.files(path = "epi_csvs/60-0.25",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_60_.5 <- list.files(path = "epi_csvs/60-0.5",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_60_.75 <- list.files(path = "epi_csvs/60-0.75",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_baseline <- list.files(path = "epi_csvs/baseline",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

#create summary dataframe
new_df_30_.25 <- df_30_.25 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_30_.5 <- df_30_.5 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_30_.75 <- df_30_.75 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_45_.25 <- df_45_.25 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_45_.5 <- df_45_.5 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_45_.75 <- df_60_.75 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_60_.25 <- df_60_.25 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_60_.5 <- df_60_.5 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_60_.75 <- df_60_.75 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

new_df_baseline <- df_baseline %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

#write summaries to csvs
write.csv(new_df_30_.25, "summary_csv_day/new_summary_30-.25.csv")
write.csv(new_df_30_.5, "summary_csv_day/new_summary_30-.5.csv")
write.csv(new_df_30_.75, "summary_csv_day/new_summary_30-.75.csv")

write.csv(new_df_45_.25, "summary_csv_day/new_summary_45-.25.csv")
write.csv(new_df_45_.5, "summary_csv_day/new_summary_45-.5.csv")
write.csv(new_df_45_.75, "summary_csv_day/new_summary_45-.75.csv")

write.csv(new_df_60_.25, "summary_csv_day/new_summary_60-.25.csv")
write.csv(new_df_60_.5, "summary_csv_day/new_summary_60-.5.csv")
write.csv(new_df_60_.75, "summary_csv_day/new_summary_60-.75.csv")

write.csv(new_df_baseline, "summary_csv_day/new_summary_baseline.csv")