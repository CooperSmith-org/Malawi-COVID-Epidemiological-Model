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
files <- list.files("inputs/sensitivityScenarios", full.names = TRUE) #For within countries
reductions <- lapply(files, read_csv)
names(reductions) <-gsub(".csv","",
                         list.files("inputs/sensitivityScenarios", full.names = FALSE), #For within countries
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
        write.csv(sim, paste0("epi_csvs/Sensitivity/",names(reductions[r]),"/",runNum,".csv"))}
      }
    }
  }
}

#Get overall composite
for (c in countryList){
  for (r in 1:length(reductions)){
    data_use <- filter(combined_data, combined_data$Run == c)
    pop_range <- data_use$Population #TA population total estimate
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
                 kappa2 = 1 / 2.6, #rest of infectious time and time to symptomatic
                 tau = 1 / 8, #recovery rate for hospitalized cases
                 tau2 = 1 / 16, #recovery rate for ICU cases
                 R0 = 2.2, #basic reproductive numbe
                 reductionList = list()) # day 1 assumed baseline reduction
      parms["population"] <- pop_range[i]
      parms["eta"] <- eta_range[i]
      parms["eta2"] <- eta2_range[i]
      parms["epsilon"] <- ep_range[i]
      parms["reductionList"] <- list(reductions[[r]]$reduc)
      
      ##Do some adjusting for start dates
      #t_from0 <- MW_starts$Date_from_0[as.numeric(UID[i])]+1
      
      init <- c(S = pop_range[i] - 1, E = 0, I = 1, H = 0, C = 0, R = 0, D = 0, inci = 0, hosp = 0, crits = 0)
      
      times <- seq(1,length(parms[["reductionList"]]))
      sim <- as.data.table(lsoda(init, times, model, parms))
      sim$lvl2 <- lvl2[i]
      sim$lvl3 <- lvl3[i]
      sim$ID <- UID[i]
      sim$POP <- pop_range[i]
      #sim$start <- t_from0
      
      #Use below for in-country
      if (UID[i] != "N/A"){
        write.csv(sim, paste0("epi_csvs/Sensitivity/",names(reductions[r]),"_composite.csv"))}
    }
  }
}

#load the scenarios
sensitivity1 <- makeCombinedDF("epi_csvs/Sensitivity/additionalGuidelines")
sensitivity2 <- makeCombinedDF("epi_csvs/Sensitivity/additionalGuidelines21day")
sensitivity3 <- makeCombinedDF("epi_csvs/Sensitivity/current")
sensitivity4 <- makeCombinedDF("epi_csvs/Sensitivity/enforcedLockdown")
sensitivity5 <- makeCombinedDF("epi_csvs/Sensitivity/enforcedLockdown21day")
sensitivity6 <- makeCombinedDF("epi_csvs/Sensitivity/enforcedRestrictions")
sensitivity7 <- makeCombinedDF("epi_csvs/Sensitivity/enforcedRestrictions21day")
sensitivity8 <- makeCombinedDF("epi_csvs/Sensitivity/unmitigated")

#lists for loop
dfList <- list(sensitivity1, sensitivity2, sensitivity3, sensitivity4, sensitivity5, 
               sensitivity6, sensitivity7, sensitivity8)

summaryNameList <- list("Sensitivity/additionalGuidelines.csv","Sensitivity/additionalGuidelines21day.csv",
"Sensitivity/current.csv","Sensitivity/enforcedLockdown.csv","Sensitivity/enforcedLockdown21day.csv",
"Sensitivity/enforcedrestrictions21day.csv", "Sensitivity/enforcedrestrictions21day.csv", 
"Sensitivity/unmitigated.csv")

folderList <- list("epi_csvs/Sensitivity/additionalGuidelines",
                   "epi_csvs/Sensitivity/unmitigated",
                   "epi_csvs/Sensitivity/additionalGuidelines21day", 
                   "epi_csvs/Sensitivity/enforcedLockdown",
                   "epi_csvs/Sensitivity/enforcedLockdown21day",
                   "epi_csvs/Sensitivity/enforcedRestrictions",
                   "epi_csvs/Sensitivity/enforcedRestrictions21day")

#Function to make sensitivity summary csv
makeSummaryCSVSensitivity <- function(df, filename) {
  dftocsv <- df %>%
    group_by(runNum) %>%
    summarise(Population = max(POP), Incidence = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
  write.csv(dftocsv, filename)
}

#Loop to make all the summary CSVs
i=1
for (df in dfList) {
  makeSummaryCSVSensitivity(dfList[[i]], summaryNameList[[i]])
  i = i + 1
}

#delete the folders since we have csvs now - now GitHub should NOT try to push these!
num=1
for (folder in folderList) {
  unlink(folderList[num], recursive = TRUE)
  num = num +1
}

#Function to make the graphs
makeComparisonGraphSensitivity <- function(df, diseaseState, title, fileName) {
  ggplot(data=subset(df, df$State %in% c(diseaseState) & df$label == "predicted"),
         aes(x=time, y=value, fill=label, color=label)) +
    geom_line(stat="identity", position = "identity") +
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal()+
    geom_ribbon(aes(ymin = filter(df, df$State %in% c(diseaseState) & label %in% c("low"))$value, 
                    ymax= filter(df, df$State %in% c(diseaseState), label %in% c("high"))$value),
                alpha=0.1,       #transparency
                linetype=1,      #solid, dashed or other line types
                colour="grey70", #border line color
                size=.1,          #border line size
                fill="green")
  ggsave(fileName, height=4 , width =8)
}

#### GENERATE GRAPHS ######

####UNMITIGATED######
sensitivitylowUNMITIGATED <- sensitivity8 %>%
  group_by(time) %>%
  filter(runNum != 0)  %>%
  summarise(Susceptible = quantile(S, .15), Infected = quantile(I, .15), Deaths = quantile(D, .15))

sensitivityhighUNMITIGATED <- sensitivity8 %>%
  group_by(time) %>%
  filter(runNum != 0)  %>%
  summarise(Susceptible = quantile(S, .85), Infected = quantile(I, .85), Deaths = quantile(D, .85))

#get additional lines
unmitigatedGraph <- read_csv("summary_csv_day/Malawi/unmitigated.csv")
unmitigatedGraph <- select(unmitigatedGraph, -X1_1)
names(unmitigatedGraph)[names(unmitigatedGraph)=="X1"] <- "time"

#Add labels
sensitivitylowUNMITIGATED$label <- "low"
sensitivityhighUNMITIGATED$label <- "high"
unmitigatedGraph$label = "predicted"

#Reshape + combine
combinedSensitivityUNMITIGATED <- rbind(sensitivitylowUNMITIGATED, sensitivityhighUNMITIGATED)
longDataU <- melt(combinedSensitivityUNMITIGATED, id = c("time", "label"))
longDataPredU <- melt(unmitigatedGraph, id = c("time", "label"))

finalU <- combinedSensitivityU <- rbind(longDataPredU, longDataU)
names(finalU)[names(finalU)=="variable"] <- "State"

options(scipen=10000) #Override scientific notation default

makeComparisonGraphSensitivity(finalU, "Infected", "Number of infected individuals across model runs testing sensitivity for unmitigated scenario", "images/Sensitivity/unmitigated_infection.png")
makeComparisonGraphSensitivity(finalU, "Deaths", "Number of deaths across model runs testing sensitivity for unmitigated scenario", "images/Sensitivity/unmitigated_deaths.png")


####ADDITIONAL GUIDELINES######
sensitivitylowGLINES <- sensitivity1 %>%
  group_by(time) %>%
  filter(runNum != 0)  %>%
  summarise(Susceptible = quantile(S, .15), Infected = quantile(I, .15), Deaths = quantile(D, .15))

sensitivityhighGLINES <- sensitivity1 %>%
  group_by(time) %>%
  filter(runNum != 0)  %>%
  summarise(Susceptible = quantile(S, .85), Infected = quantile(I, .85), Deaths = quantile(D, .85))

#get additional lines
additionalGuidelinesGraph <- read_csv("summary_csv_day/Malawi/additionalGuideline.csv")
additionalGuidelinesGraph <- select(additionalGuidelinesGraph, -X1_1)
names(additionalGuidelinesGraph)[names(additionalGuidelinesGraph)=="X1"] <- "time"

#Add labels
sensitivitylowGLINES$label <- "low"
sensitivityhighGLINES$label <- "high"
additionalGuidelinesGraph$label = "predicted"

#Reshape + combine
combinedSensitivityGLINES <- rbind(sensitivitylowGLINES, sensitivityhighGLINES)
longDataG <- melt(combinedSensitivityGLINES, id = c("time", "label"))
longDataPredG <- melt(additionalGuidelinesGraph, id = c("time", "label"))

finalG <- combinedSensitivityG <- rbind(longDataPredG, longDataG)
names(finalG)[names(finalG)=="variable"] <- "State"

options(scipen=10000) #Override scientific notation default

makeComparisonGraphSensitivity(finalG, "Infected", "Number of infected individuals across model runs testing sensitivity for additional guidelines scenario", 
                               "images/Sensitivity/guidelines_infection.png")
makeComparisonGraphSensitivity(finalG, "Deaths", "Number of deaths across model runs testing sensitivity for additional guidelines scenario", 
                               "images/Sensitivity/guidelines_deaths.png")