set.seed(1234)

library(dplyr)
library(ggplot2)
library(scales)
library(readr)
library(reshape2)
library(deSolve)
library(data.table)

#This file contains functions to use in the other scripts to run analysis

#Function to run model - SEIR Model of Differential Equations
model <- function(times, init, parms) {
  with(as.list(c(init, parms)), {
      beta <- (1-reductionList[times])*susceptibility*(1 - compliance + compliance*efficacy)*R0*kappa/(population*contact)
    dS <- -beta * S * I #susceptible
    dE <- beta * S * I -  E * kappa #exposed but asymptomatic``
    dI <- E * kappa - I * kappa2 #infectious, but mild severity
    dH <- eta * I  * kappa2 - tau * H #hospitalized 
    dC <- eta2 * tau * H  - tau2 * C #critical care
    dR <- (1 - eta) * I * kappa2 + (1 - eta2) * tau * H + (1 - epsilon) * tau2 * C #recovered
    dD <- epsilon * tau2 * C #dead
    inci <- beta * S * I #incident infections
    hosp <- eta * I  * kappa2 # incident hospitalizations
    crits <- eta2 * tau * H # incident ICUs
    return(list(c(dS, dE, dI, dH, dC, dR, dD, inci, hosp, crits)))
  })
}

#Function to combine CSVs outputted by different model runs
makeCombinedDF <-function(location) {
  df <- list.files(path = location,full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  return(df)
}

#Function to combine different scenarios for lvl3
makeCombinedScenariolvl3 <-function(base_df, scenario_df, districtList) {
  scenario_df$scenario <- "reduction"
  base_df$scenario <- "baseline"
  
  use1 <- subset(scenario_df, scenario_df$lvl3 %in% districtList)
  use2 <- subset(base_df, base_df$lvl3 %in% setdiff(unique(base_df$lvl3), districtList))
  return(bind_rows(use1,use2)) 
}

#Function to make a summary graph for a district
makeSummaryGraph <- function(df, districtName, title, filename) {
  df$time <- df$time + df$start # add in staggered
}

#Function to make a summary graph for lvl3
makeSummaryGraphlvl3 <- function(df, districtName, title, filename) {
  names(df)[names(df)=="time"] <- "Day"
  df <- df %>%
    filter(df$lvl3==districtName) %>%
    group_by(Day) %>%
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
  
  longData <- melt(df, id = c("Day"))
  longData$variable <- factor(longData$variable,
                              levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical","Deaths"))
  
  names(longData)[names(longData)=="variable"] <- "State"
  options(scipen=10000)#Override scientific notation default
  
  ggplot(data=subset(longData, longData$State %in% c("Infected", "Hospitalized","Critical","Deaths")) %>% arrange(State),
         aes(x=Day, y=value, fill=State, color=State)) +
    geom_line(stat="identity", position = "identity") +
    scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
    scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal()
  ggsave(filename, height=4, width=8)
}

#Function to combine different scenarios for lvl4
makeCombinedScenariolvl4 <-function(base_df, scenario_df, districtList) {
  scenario_df$scenario <- "reduction"
  base_df$scenario <- "baseline"
  
  use1 <- subset(scenario_df, scenario_df$lvl4 %in% districtList)
  use2 <- subset(base_df, base_df$lvl4 %in% setdiff(unique(base_df$lvl4), districtList))
  return(bind_rows(use1,use2)) 
}

#Function to make a summary graph for lvl4
makeSummaryGraphlvl4 <- function(df, districtName, title, filename) {
  names(df)[names(df)=="time"] <- "Day"
  df <- df %>%
    filter(df$lvl4==districtName) %>%
    group_by(Day) %>%
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
  
  longData <- melt(df, id = c("Day"))
  longData$variable <- factor(longData$variable,
                              levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical","Deaths"))
  
  names(longData)[names(longData)=="variable"] <- "State"
  options(scipen=10000)#Override scientific notation default
  
  ggplot(data=subset(longData, longData$State %in% c("Infected", "Hospitalized","Critical","Deaths")) %>% arrange(State),
         aes(x=Day, y=value, fill=State, color=State)) +
    geom_line(stat="identity", position = "identity") +
    scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
    scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal()
  ggsave(filename, height=4, width=8)
}

#Function to make a summary DF for all districts

makeSummaryGraphAll <- function(df, title, filename) {
  df$X1 <- df$X1 + df$start # add in staggered
  names(df)[names(df)=="X1"] <- "Day"
  
  df <- df %>%
    group_by(Day) %>%
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
  
  longData <- melt(df, id = c("Day"))
  longData$variable <- factor(longData$variable,
                              levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical","Deaths"))
  
  names(longData)[names(longData)=="variable"] <- "State"
  options(scipen=10000) #Override scientific notation default
  
  ggplot(data=subset(longData, longData$State %in% c("Infected", "Hospitalized","Critical","Deaths")) %>% arrange(State),
         aes(x=Day, y=value, fill=State, color=State)) +
    geom_line(stat="identity", position = "identity") +
    scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
    scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal()
  ggsave(filename, height=4, width=8)
}

#Function to compare across different scenarios - specify variable of interest (disease state)

makeComparisonGraph <- function(df, diseaseState, title, fileName) {
  ggplot(data=subset(df, df$State %in% c(diseaseState)) %>% arrange(State),
         aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
    geom_line(stat="identity", position = "identity") +
    scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
    scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal() #+
  #theme(legend.position = "none") #turn on and off for legend
  ggsave(fileName, height=4 , width =8)
}

#Function to summarize data

makeSummaryCSV <- function(df, fileName) {
  df$time <- df$time + df$start # add in staggered
  df1 <- df %>% 
    filter(time <= 365) %>%
    group_by(time) %>% 
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D), Incidence = sum(inci), New_Hospitalized = sum(hosp), New_Crit = sum(crits), Population = sum(tot_pop))
  write.csv(df1, fileName)
}

makeSummaryCSVGeo <- function(df, filename) {
  df$X1 <- df$X1 + df$start # add in staggered
  dftocsv <- df %>%
    group_by(lvl3, ID) %>%
    summarise(Population = max(POP), Incidence = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
  write.csv(dftocsv, filename)
}

makeSummaryCSVAge <- function(df, filename) {
  df$X1 <- df$X1 + df$start # add in staggered
  dftocsv <- df %>%
    group_by(lvl3, ID, age) %>%
    summarise(Population = max(POP), Incidence = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
  write.csv(dftocsv, filename)
}