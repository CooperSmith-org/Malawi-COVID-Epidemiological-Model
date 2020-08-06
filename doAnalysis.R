library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
source("utils.R")

####Create summary files - Malawi####

#Load individual csvs -NOTE, just comment out others when re-running a scenario

#day offset
current <- makeCombinedDF("epi_csvs/Malawi/banded_current")
#enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/banded_current_lockdown")

# #5 day offset
# current <- makeCombinedDF("epi_csvs/Malawi/banded_current_updated5")
# enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/banded_current_lockdown5")
# 
# #0 day offset
# current <- makeCombinedDF("epi_csvs/Malawi/banded_current_updated0")
# enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/banded_current_lockdown0")

#Create the daily summary
makeSummaryCSV(current, "summary_csv_day/Malawi/current.csv")
#makeSummaryCSV(enforcedLockdown, "summary_csv_day/Malawi/enforcedLockdown.csv")

#Create the district level summary
makeSummaryCSVGeo(current, "summary_by_district/Malawi/current_50day.csv")
makeSummaryCSVGeo(enforcedLockdown, "summary_by_district/Malawi/enforcedLockdown_50day.csv")

####Create Graphs####
currentGraphBAND <- read_csv("summary_csv_day/Malawi/current.csv")
enforcedLockdownGraphBAND <- read_csv("summary_csv_day/Malawi/enforcedLockdown.csv")

#Add label column
currentGraphBAND$Reduction <- "Current mobility reductions continue"
enforcedLockdownGraphBAND$Reduction <- "Lockdown implemented"

#Create comparison df
comparisonDF <- bind_rows(currentGraphBAND, enforcedLockdownGraphBAND)
comparisonDF <- select(comparisonDF, -c("X1_1"))
comparisonDF <- melt(comparisonDF, id = c("X1", "Reduction"))
names(comparisonDF)[names(comparisonDF)=="variable"] <- "State"

#Make the charts
options(scipen=10000)

makeComparisonGraph(comparisonDF, "Infected", "Infections", "images/Malawi/infectionScenariosAge_5.png")
makeComparisonGraph(comparisonDF, "Hospitalized", "Hospitalizations", "images/Malawi/hospitalizationScenariosAge_5.png")
makeComparisonGraph(comparisonDF, "Critical", "Critical Care", "images/Malawi/critcareScenariosAge_5.png")
makeComparisonGraph(comparisonDF, "Deaths", "Deaths", "images/Malawi/deathsScenariosAge_5.png")
