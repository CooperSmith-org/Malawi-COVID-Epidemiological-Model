library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
source("utils.R")

####Create summary files####

#Load individual csvs -NOTE, just change names of source folder to update
unmitigated <- makeCombinedDF("epi_csvs/Malawi/unmitigated")
current <- makeCombinedDF("epi_csvs/Malawi/current")
additionalGuidelines <- makeCombinedDF("epi_csvs/Malawi/additionalGuidelines")
enforcedRestrictions <- makeCombinedDF("epi_csvs/Malawi/enforcedRestrictions")
enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/enforcedLockdown")
additionalGuidelines21day <- makeCombinedDF("epi_csvs/Malawi/additionalGuidelines21day")
enforcedRestrictions21day <- makeCombinedDF("epi_csvs/Malawi/enforcedRestrictions21day")
enforcedLockdown21day <- makeCombinedDF("epi_csvs/Malawi/enforcedLockdown21day")

#Create the current scenarios - 1: 4 major urban centers; 2: 4 + few otherrs

scenario1 <- makeCombinedScenario(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota"))
scenario2 <- makeCombinedScenario(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota", "Salima", "Mzuzu City", "Zomba City", "Zomba", "Mzimba", "Mangochi"))


#Create the daily summary
makeSummaryCSV(unmitigated, "summary_csv_day/unmitigated.csv")
makeSummaryCSV(current, "summary_csv_day/current.csv")
makeSummaryCSV(additionalGuidelines, "summary_csv_day/additionalGuideline.csv")
makeSummaryCSV(enforcedRestrictions, "summary_csv_day/enforcedRestrictions.csv")
makeSummaryCSV(enforcedLockdown, "summary_csv_day/enforcedLockdown.csv")
makeSummaryCSV(scenario1, "summary_csv_day/scenario1.csv")
makeSummaryCSV(scenario2, "summary_csv_day/scenario2.csv")
makeSummaryCSV(additionalGuidelines21day, "summary_csv_day/additionalGuideline21day.csv")
makeSummaryCSV(enforcedRestrictions21day, "summary_csv_day/enforcedRestrictions21day.csv")
makeSummaryCSV(enforcedLockdown21day, "summary_csv_day/enforcedLockdown21day.csv")

#Create the district level summary
makeSummaryCSVGeo(unmitigated, "summary_by_district/unmitigated.csv")
makeSummaryCSVGeo(current, "summary_by_district/current.csv")
makeSummaryCSVGeo(additionalGuidelines, "summary_by_district/additionalGuideline.csv")
makeSummaryCSVGeo(enforcedRestrictions, "summary_by_district/enforcedRestrictions.csv")
makeSummaryCSVGeo(enforcedLockdown, "summary_by_district/enforcedLockdown.csv")
makeSummaryCSVGeo(scenario1, "summary_by_district/scenario1.csv")
makeSummaryCSVGeo(scenario2, "summary_by_district/scenario2.csv")
makeSummaryCSVGeo(additionalGuidelines21day, "summary_by_district/additionalGuideline21day.csv")
makeSummaryCSVGeo(enforcedRestrictions21day, "summary_by_district/enforcedRestrictions21day.csv")
makeSummaryCSVGeo(enforcedLockdown21day, "summary_by_district/enforcedLockdown21day.csv")

####Create Graphs####

unmitigatedGraph <- read_csv("summary_csv_day/unmitigated.csv")
currentGraph <- read_csv("summary_csv_day/current.csv")
additionalGuidelinesGraph <- read_csv("summary_csv_day/additionalGuideline.csv")
enforcedRestrictionsGraph <- read_csv("summary_csv_day/enforcedRestrictions.csv")
enforcedLockdownGraph <- read_csv("summary_csv_day/enforcedLockdown.csv")
scenario1Graph <- read_csv("summary_csv_day/scenario1.csv")
scenario2Graph <-read_csv("summary_csv_day/scenario2.csv")
additionalGuidelines21dayGraph <- read_csv("summary_csv_day/additionalGuideline21day.csv")
enforcedRestrictions21dayGraph <- read_csv("summary_csv_day/enforcedRestrictions21day.csv")
enforcedLockdown21dayGraph <- read_csv("summary_csv_day/enforcedLockdown21day.csv")

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

