set.seed(1234)

#setwd("C:/Users/dylan/Dropbox (Cooper Smith)/CS_Team/Projects/COVID-19 Response/Malawi/Analytics/EPI Modeling")
library(deSolve)
library(data.table)
library(ggplot2)
library(scales)
library(readr)

#SEIR Model of Differential Equations
model <- function(times, init, parms) {
  with(as.list(c(parms, init)), {
    ifelse(times < 365, beta <- R0*kappa/population, beta <- .5*R0*kappa/population) # time-dependent R0 if social distancing is implemented. 50% reduction but can be anything
    dS <- -beta * S * I #susceptible
    dE <- beta * S * I -  E * kappa #exposed but asymptomatic
    dI <- E * kappa - I * kappa2 #infectious, but mild severity
    dH <- eta * I  * kappa2 - tau * H #hospitalized
    dC <- eta2 * tau * H  - tau2 * C #critical care
    dR <- (1 - eta) * I / 14 + (1 - eta2) * tau * H + (1 - epsilon) * tau2 * C #recovered NOTE: 14 is the time to recovery for mild cases
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

#creating lists that data will go into
P.List = list() #population
S.List = list() #susceptible
E.List = list() #exposed
I.List = list() #infectious, mild
H.List = list() #hospitalized
C.List = list() #ICU
R.List = list() #recovered
D.List = list() #dead
IN.List = list() #incidence
HOSP.List = list() #cumulative hospitalizations
CRITS.List = list() #cumulative ICUs

#loop through each TA, using the TA-specific estimates of population size, hospitalization, ICU, and death
for(i in 1:length(pop_range)) {
  parms <- c(population = 0, #population size
             eta = 0, #proportion of cases who are hospitalized
             eta2 = 0, #ICU rate of hospitalized cases
             epsilon = 0, #death rate of ICU cases
             kappa = 1 / 2.6, #time to infectiousness
             kappa2 = 1 / 2.6, #rest of infectious time and time to symptomatic
             tau = 1 / 8, #recovery rate for hospitalized cases
             tau2 = 1 / 16, #recovery rate for ICU cases
             R0 = 2.2) #basic reproductive number)
  parms["population"] <- pop_range[i]
  parms["eta"] <- eta_range[i]
  parms["eta2"] <- eta2_range[i]
  parms["epsilon"] <- ep_range[i]
  init <- c(S = pop_range[i] - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, hosp = 0, crits = 0)
  times <- seq(0,365)
  sim <- as.data.table(lsoda(init, times, model, parms))
  
  write.csv(sim, paste0("epi_csvs/",UID[i],"_epi.csv"))
  
  P.List[[length(P.List)+1]] = pop_range[i]
  S.List[[length(S.List)+1]] = min(sim$S)  #number susceptible at end of simulation
  E.List[[length(E.List)+1]] = max(sim$E)  #peak of exposed group
  I.List[[length(I.List)+1]] = max(sim$I)  #peak of infectious group
  H.List[[length(H.List)+1]] = max(sim$H)  #peak hospitalization
  C.List[[length(C.List)+1]] = max(sim$C)  #peak ICU
  R.List[[length(R.List)+1]] = max(sim$R)  #cumulative recovered
  D.List[[length(D.List)+1]] = max(sim$D)  #cumulative deaths
  IN.List[[length(IN.List)+1]] = sum(-diff(sim$S)) #cumulative incidence
  HOSP.List[[length(HOSP.List)+1]] = sum(diff(sim$hosp)) #cumulative hospitalization
  CRITS.List[[length(CRITS.List)+1]] = sum(diff(sim$crit)) #cumulative ICU
}

#combine all data lists an put into a data frame, then name variables
pop <- do.call(rbind, P.List)
susc <- do.call(rbind, S.List)
exp <- do.call(rbind, E.List)
inf <- do.call(rbind, I.List)
hosp <- do.call(rbind, H.List)
crit <- do.call(rbind, C.List)
recovered <- do.call(rbind, R.List)
deaths <- do.call(rbind, D.List)
incid <- do.call(rbind, IN.List)
hosp <- do.call(rbind, HOSP.List)
crits <- do.call(rbind, CRITS.List)
model_output <- as.data.frame(cbind(lvl3, pop, susc, exp, inf, hosp, crit, recovered, deaths, incid, hosp, crits))
names(model_output)[names(model_output)=="V2"] <- "Population"
names(model_output)[names(model_output)=="V3"] <- "Susceptible"
names(model_output)[names(model_output)=="V4"] <- "Exposed"
names(model_output)[names(model_output)=="V5"] <- "Infected"
names(model_output)[names(model_output)=="V6"] <- "Peak Hospitalizations"
names(model_output)[names(model_output)=="V7"] <- "Peak Critical Care"
names(model_output)[names(model_output)=="V8"] <- "Recovered"
names(model_output)[names(model_output)=="V9"] <- "Deaths"
names(model_output)[names(model_output)=="V10"] <- "Incidence"
names(model_output)[names(model_output)=="V11"] <- "Cumulative Hospitalizations"
names(model_output)[names(model_output)=="V12"] <- "Cumulative Critical Care"

#some summary statistics of interest
sum(as.numeric(as.character(model_output$Incidence)))
sum(as.numeric(as.character(model_output$`Cumulative Hospitalizations`)))
sum(as.numeric(as.character(model_output$`Cumulative Critical Care`)))
sum(as.numeric(as.character(model_output$Deaths)))
sum(as.numeric(as.character(model_output$`Peak Hospitalizations`)))
sum(as.numeric(as.character(model_output$`Peak Critical Care`)))
sum(as.numeric(as.character(model_output$`Cumulative Hospitalizations`)))/sum(as.numeric(as.character(model_output$Incidence)))
sum(as.numeric(as.character(model_output$Deaths)))/sum(as.numeric(as.character(model_output$`Cumulative Critical Care`)))

#print CSV output
write.csv(model_output,'model_output.csv')