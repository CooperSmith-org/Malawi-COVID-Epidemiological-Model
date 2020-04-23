library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
source("utils.R")

#Load all data
unmitigated <- makeCombinedDF("epi_csvs/Malawi/unmitigated")
additionalGuidelines <- makeCombinedDF("epi_csvs/Malawi/additionalGuidelines")
enforcedRestrictions<- makeCombinedDF("epi_csvs/Malawi/enforcedRestrictions")
current <- makeCombinedDF("epi_csvs/Malawi/current")
enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/enforcedLockdown")

#Grab for specific TAs  
unmitigatedBlantyre <- filter(unmitigated, unmitigated$TA %in% c("Blantyre", "Blantyre City"))
additionalGuidelinesBlantyre <- filter(additionalGuidelines, additionalGuidelines$TA %in% c("Blantyre", "Blantyre City"))
enforcedRestrictionsBlantyre <- filter(enforcedRestrictions, enforcedRestrictions$TA %in% c("Blantyre", "Blantyre City"))
currentBlantyre <- filter(current, current$TA %in% c("Blantyre", "Blantyre City"))
enforcedLockdownBlantyre <- filter(enforcedLockdown, enforcedLockdown$TA %in% c("Blantyre", "Blantyre City"))

#Create Blantyre summary chart
makeSummaryGraphAll(unmitigatedBlantyre, "Infected, hospitalized, critical care, and deaths for unmitigated scenario in Blantyre TA", "images/Districts/Blantyre/summarycurve.png")

#Create the comparison charts
makeSummaryCSV(unmitigatedBlantyre, "summary_csv_day/Districts/Blantyre/unmitigatedBlantyre.csv")
makeSummaryCSV(additionalGuidelinesBlantyre, "summary_csv_day/Districts/Blantyre/addlGuidelinesBlantyre.csv")
makeSummaryCSV(currentBlantyre, "summary_csv_day/Districts/Blantyre/currentBlantyre.csv")
makeSummaryCSV(enforcedLockdownBlantyre, "summary_csv_day/Districts/Blantyre/enforcedLockdownBlantyre.csv")
makeSummaryCSV(enforcedRestrictionsBlantyre, "summary_csv_day/Districts/Blantyre/enforcedRestrictionsBlantyre.csv")

unmitigatedGraph <- read_csv("summary_csv_day/Districts/Blantyre/unmitigatedBlantyre.csv")
currentGraph <- read_csv("summary_csv_day/Districts/Blantyre/currentBlantyre.csv")
enforcedLockdownGraph <- read_csv("summary_csv_day/Districts/Blantyre/enforcedLockdownBlantyre.csv")
enforcedRestrictionsGraph <- read_csv("summary_csv_day/Districts/Blantyre/enforcedRestrictionsBlantyre.csv")
additionalGuidelinesGraph <- read_csv("summary_csv_day/Districts/Blantyre/addlGuidelinesBlantyre.csv")

#Add label column
unmitigatedGraph$Reduction <- "1. Unmitigated scenario"
currentGraph$Reduction <- "2. Current reduction of ~15% continue"
additionalGuidelinesGraph$Reduction <- "3. Additional distancing guidelines"
enforcedRestrictionsGraph$Reduction <- "4. Enforced restrictions in TA"
enforcedLockdownGraph$Reduction <- "4. Enforced lockdown in TA"

#Create comparison df
comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, additionalGuidelinesGraph, enforcedRestrictionsGraph, enforcedLockdownGraph)
comparisonDF <- select(comparisonDF, -c("X1_1"))
comparisonDF <- melt(comparisonDF, id = c("X1", "Reduction"))
names(comparisonDF)[names(comparisonDF)=="variable"] <- "State"

#Make the charts
options(scipen=10000)

makeComparisonGraph(comparisonDF, "Infected", "Number of individuals infected based on varying scenario assumptions over time", "images/Districts/Blantyre/infectionScenarios.png")
makeComparisonGraph(comparisonDF, "Hospitalized", "Number of individuals hospitalized based on varying scenario assumptions over time", "images/Districts/Blantyre/hospitalizationScenarios.png")
makeComparisonGraph(comparisonDF, "Critical", "Number of individuals in critical care based on varying scenario assumptions over time", "images/Districts/Blantyre/critcareScenarios.png")
makeComparisonGraph(comparisonDF, "Deaths", "Number of deaths based on varying scenario assumptions over time", "images/Districts/Blantyre/deathsScenarios.png")
