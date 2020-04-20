library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
source("utils.R")

####Create summary files####

#Load individual csvs
additionalGuidelines <- makeCombinedDF("epi_csvs/Malawi/additionalGuidelines")
enforcedRestrictions<- makeCombinedDF("epi_csvs/Malawi/enforcedRestrictions")
current <- makeCombinedDF("epi_csvs/Malawi/current")
unmitigated <- makeCombinedDF("epi_csvs/Malawi/unmitigated")
enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/enforcedLockdown")

#Create the current scenarios - 1: 4 major urban centers; 2: 4 + few otherrs

scenario1 <- makeCombinedScenario(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota"))
scenario2 <- makeCombinedScenario(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota", "Salima", "Mzuzu City", "Zomba City", "Zomba", "Mzimba", "Mangochi"))


#Create the daily summary
makeSummaryCSV(unmitigated, "summary_csv_day/unmitigated.csv")
makeSummaryCSV(current, "summary_csv_day/current.csv")
makeSummaryCSV(enforcedLockdown, "summary_csv_day/enforcedLockdown.csv")
makeSummaryCSV(enforcedRestrictions, "summary_csv_day/enforcedRestrictions.csv")
makeSummaryCSV(additionalGuidelines, "summary_csv_day/additionalGuideline.csv")
makeSummaryCSV(scenario1, "summary_csv_day/scenario1.csv")
makeSummaryCSV(scenario2, "summary_csv_day/scenario2.csv")

#Create the TA level summary
makeSummaryCSVGeo(unmitigated, "summary_by_TA/unmitigated.csv")
makeSummaryCSVGeo(current, "summary_by_TA/current.csv")
makeSummaryCSVGeo(enforcedLockdown, "summary_by_TA/enforcedLockdown.csv")
makeSummaryCSVGeo(enforcedRestrictions, "summary_by_TA/enforcedRestrictions.csv")
makeSummaryCSVGeo(additionalGuidelines, "summary_by_TA/additionalGuideline.csv")
makeSummaryCSVGeo(scenario1, "summary_by_TA/scenario1.csv")
makeSummaryCSVGeo(scenario2, "summary_by_TA/scenario2.csv")

####Create Graphs####

unmitigatedGraph <- read_csv("summary_csv_day/unmitigated.csv")
currentGraph <- read_csv("summary_csv_day/current.csv")
enforcedLockdownGraph <- read_csv("summary_csv_day/enforcedLockdown.csv")
enforcedRestrictionsGraph <- read_csv("summary_csv_day/enforcedRestrictions.csv")
additionalGuidelinesGraph <- read_csv("summary_csv_day/additionalGuidelines.csv")
scenario1Graph <- read_csv("summary_csv_day/scenario1.csv")
scenario2Graph <-read_csv("summary_csv_day/scenario2.csv")

#Add label column
unmitigatedGraph$Reduction <- "1. Unmitigated scenario"
currentGraph$Reduction <- "2. Current reduction of ~15% continue"
scenario1Graph$Reduction <- "3. Enforced restrictions in limited urban areas"
scenario2Graph$Reduction <- "4. Enforced restrictions in additional urban areas"

#Create comparison df
comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, scenario1Graph, scenario2Graph)
comparisonDF <- select(comparisonDF, -c("X1_1"))
comparisonDF <- melt(comparisonDF, id = c("X1", "Reduction"))
names(comparisonDF)[names(comparisonDF)=="variable"] <- "State"

#Make the charts
options(scipen=10000)

makeComparisonGraph(comparisonDF, "Infected", "Number of individuals infected based on varying scenario assumptions over time", "images/Malawi/infectionScenarios.png")
makeComparisonGraph(comparisonDF, "Hospitalized", "Number of individuals hospitalized based on varying scenario assumptions over time", "images/Malawi/hospitalizationScenarios.png")
makeComparisonGraph(comparisonDF, "Critical", "Number of individuals in critical care based on varying scenario assumptions over time", "images/Malawi/critcareScenarios.png")
makeComparisonGraph(comparisonDF, "Deaths", "Number of deaths based on varying scenario assumptions over time", "images/Malawi/deathsScenarios.png")

