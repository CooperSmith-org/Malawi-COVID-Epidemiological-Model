set.seed(1234)

library(dplyr)
library(reshape)
library(ggplot2)
library(scales)
library(readr)
library(reshape2)
library(deSolve)
library(data.table)

#This file contains functions to use in the other scripts to run analysis

#Function to run model - SEIR Model of Differential Equations

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


#Function to combine CSVs outputted by different model runs

makeCombinedDF <-function(location) {
  df <- list.files(path = location,full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  return(df)
}

#Function to combine different scenarios by TA

makeCombinedScenario <-function(base_df, scenario_df, TAList) {
  scenario_df$scenario <- "reduction"
  base_df$scenario <- "baseline"
  
  use1 <- subset(scenario_df, scenario_df$TA %in% TAList)
  use2 <- subset(base_df, base_df$TA %in% setdiff(unique(base_df$TA), TAList))
  return(bind_rows(use1,use2)) 
}

#Function to make a summary graph for a TA

makeSummaryGraph <- function(df, TAName, title, filename) {
  names(df)[names(df)=="time"] <- "Day"
  df <- df %>%
    filter(df$TA==TAName) %>%
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
  df1 <- df %>% 
    group_by(X1) %>% 
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
  write,csv(df1, fileName)
}
