library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
source("utils.R")

####Create summary files####

#Load individual csvs -NOTE, just change names of source folder too update
additionalGuidelines <- makeCombinedDF("epi_csvs/Malawi/new_additionalGuidelines")
enforcedRestrictions<- makeCombinedDF("epi_csvs/Malawi/new_enforcedRestrictions")
current <- makeCombinedDF("epi_csvs/Malawi/current")
unmitigated <- makeCombinedDF("epi_csvs/Malawi/unmitigated")
enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/new_enforcedLockdown")

#Create the current scenarios - 1: 4 major urban centers; 2: 4 + few otherrs

scenario1 <- makeCombinedScenario(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota"))
scenario2 <- makeCombinedScenario(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota", "Salima", "Mzuzu City", "Zomba City", "Zomba", "Mzimba", "Mangochi"))


#Create the daily summary
makeSummaryCSV(unmitigated, "summary_csv_day/unmitigated.csv")
makeSummaryCSV(current, "summary_csv_day/current.csv")
makeSummaryCSV(enforcedLockdown, "summary_csv_day/new_enforcedLockdown.csv")
makeSummaryCSV(enforcedRestrictions, "summary_csv_day/new_enforcedRestrictions.csv")
makeSummaryCSV(additionalGuidelines, "summary_csv_day/new_additionalGuideline.csv")
makeSummaryCSV(scenario1, "summary_csv_day/scenario1.csv")
makeSummaryCSV(scenario2, "summary_csv_day/scenario2.csv")

#Create the district level summary
makeSummaryCSVGeo(unmitigated, "summary_by_district/unmitigated.csv")
makeSummaryCSVGeo(current, "summary_by_district/current.csv")
makeSummaryCSVGeo(enforcedLockdown, "summary_by_district/new_enforcedLockdown.csv")
makeSummaryCSVGeo(enforcedRestrictions, "summary_by_district/new_enforcedRestrictions.csv")
makeSummaryCSVGeo(additionalGuidelines, "summary_by_district/new_additionalGuideline.csv")
makeSummaryCSVGeo(scenario1, "summary_by_district/scenario1.csv")
makeSummaryCSVGeo(scenario2, "summary_by_district/scenario2.csv")

####Create Graphs####

unmitigatedGraph <- read_csv("summary_csv_day/unmitigated.csv")
currentGraph <- read_csv("summary_csv_day/current.csv")
enforcedLockdownGraph <- read_csv("summary_csv_day/new_enforcedLockdown.csv")
enforcedRestrictionsGraph <- read_csv("summary_csv_day/new_enforcedRestrictions.csv")
additionalGuidelinesGraph <- read_csv("summary_csv_day/new_additionalGuideline.csv")
scenario1Graph <- read_csv("summary_csv_day/scenario1.csv")
scenario2Graph <-read_csv("summary_csv_day/scenario2.csv")

#Add label column
unmitigatedGraph$Reduction <- "1. Unmitigated scenario"
currentGraph$Reduction <- "2. Current reduction of ~15% continue"
additionalGuidelinesGraph$Reduction <- "3. Additional guidelines for 21 day period"
enforcedRestrictionsGraph$Reduction <- "4. Restrictive measures enforced for 21 day period"
enforcedLockdownGraph$Reduction <- "5. Lockdown enforced for 21 day period"

#scenario1Graph$Reduction <- "3. Enforced restrictions in limited urban areas"
#scenario2Graph$Reduction <- "4. Enforced restrictions in additional urban areas"

#Create comparison df
#comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, scenario1Graph, scenario2Graph)
comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, enforcedRestrictionsGraph, enforcedLockdownGraph, additionalGuidelinesGraph)
comparisonDF <- select(comparisonDF, -c("X1_1"))
comparisonDF <- melt(comparisonDF, id = c("X1", "Reduction"))
names(comparisonDF)[names(comparisonDF)=="variable"] <- "State"

#Make the charts
options(scipen=10000)

makeComparisonGraph(comparisonDF, "Infected", "Number of individuals infected based on varying scenario assumptions over time", "images/Malawi/new_infectionScenarios.png")
makeComparisonGraph(comparisonDF, "Hospitalized", "Number of individuals hospitalized based on varying scenario assumptions over time", "images/Malawi/new_hospitalizationScenarios.png")
makeComparisonGraph(comparisonDF, "Critical", "Number of individuals in critical care based on varying scenario assumptions over time", "images/Malawi/new_critcareScenarios.png")
makeComparisonGraph(comparisonDF, "Deaths", "Number of deaths based on varying scenario assumptions over time", "images/Malawi/new_deathsScenarios.png")

