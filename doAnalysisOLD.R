library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
source("utils.R")

####Create summary files - Malawi####

#Load individual csvs -NOTE, just change names of source folder to update
# unmitigated <- makeCombinedDF("epi_csvs/Malawi/unmitigated")
# current <- makeCombinedDF("epi_csvs/Malawi/current")
# additionalGuidelines <- makeCombinedDF("epi_csvs/Malawi/additionalGuidelines")
# enforcedRestrictions <- makeCombinedDF("epi_csvs/Malawi/enforcedRestrictions")
# enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/enforcedLockdown")
# additionalGuidelines21day <- makeCombinedDF("epi_csvs/Malawi/additionalGuidelines21day")
# enforcedRestrictions21day <- makeCombinedDF("epi_csvs/Malawi/enforcedRestrictions21day")
# enforcedLockdown21day <- makeCombinedDF("epi_csvs/Malawi/enforcedLockdown21day")

# unmitigated <- makeCombinedDF("epi_csvs/Malawi/banded_unmitigated")
current <- makeCombinedDF("epi_csvs/Malawi/banded_current_updated50")
# additionalGuidelines <- makeCombinedDF("epi_csvs/Malawi/banded_additionalGuidelines")
# enforcedRestrictions <- makeCombinedDF("epi_csvs/Malawi/banded_enforcedRestrictions")
enforcedLockdown <- makeCombinedDF("epi_csvs/Malawi/banded_current_lockdown50")
# additionalGuidelines21day <- makeCombinedDF("epi_csvs/Malawi/banded_additionalGuidelines21day")
# enforcedRestrictions21day <- makeCombinedDF("epi_csvs/Malawi/banded_enforcedRestrictions21day")
# enforcedLockdown21day <- makeCombinedDF("epi_csvs/Malawi/banded_enforcedLockdown21day")
# 
#Create the current scenarios - 1: 4 major urban centers; 2: 4 + few otherrs
#scenario1 <- makeCombinedScenariolvl3(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota"))
#scenario2 <- makeCombinedScenariolvl3(current, enforcedRestrictions, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota", "Salima", "Mzuzu City", "Zomba City", "Zomba", "Mzimba", "Mangochi"))


#Create the daily summary
# makeSummaryCSV(unmitigated, "summary_csv_day/Malawi/unmitigated.csv")
makeSummaryCSV(current, "summary_csv_day/Malawi/current.csv")
# makeSummaryCSV(additionalGuidelines, "summary_csv_day/Malawi/additionalGuideline.csv")
# makeSummaryCSV(enforcedRestrictions, "summary_csv_day/Malawi/enforcedRestrictions.csv")
makeSummaryCSV(enforcedLockdown, "summary_csv_day/Malawi/enforcedLockdown.csv")
#makeSummaryCSV(scenario1, "summary_csv_day/Malawi/scenario1.csv")
#makeSummaryCSV(scenario2, "summary_csv_day/Malawi/scenario2.csv")
# makeSummaryCSV(additionalGuidelines21day, "summary_csv_day/Malawi/additionalGuideline21day.csv")
# makeSummaryCSV(enforcedRestrictions21day, "summary_csv_day/Malawi/enforcedRestrictions21day.csv")
# makeSummaryCSV(enforcedLockdown21day, "summary_csv_day/Malawi/enforcedLockdown21day.csv")
# # 
# #Create the district level summary
# makeSummaryCSVGeo(unmitigated, "summary_by_district/Malawi/unmitigated.csv")
# makeSummaryCSVGeo(current, "summary_by_district/Malawi/current.csv")
# makeSummaryCSVGeo(additionalGuidelines, "summary_by_district/Malawi/additionalGuideline.csv")
# makeSummaryCSVGeo(enforcedRestrictions, "summary_by_district/Malawi/enforcedRestrictions.csv")
# makeSummaryCSVGeo(enforcedLockdown, "summary_by_district/Malawi/enforcedLockdown.csv")
# #makeSummaryCSVGeo(scenario1, "summary_by_district/Malawi/scenario1.csv")
# #makeSummaryCSVGeo(scenario2, "summary_by_district/Malawi/scenario2.csv")
# makeSummaryCSVGeo(additionalGuidelines21day, "summary_by_district/Malawi/additionalGuideline21day.csv")
# makeSummaryCSVGeo(enforcedRestrictions21day, "summary_by_district/Malawi/enforcedRestrictions21day.csv")
# makeSummaryCSVGeo(enforcedLockdown21day, "summary_by_district/Malawi/enforcedLockdown21day.csv")

####Create Graphs####
# unmitigatedGraphBAND <- read_csv("summary_csv_day/Malawi/unmitigated.csv")
currentGraphBAND <- read_csv("summary_csv_day/Malawi/current.csv")
# additionalGuidelinesGraphBAND <- read_csv("summary_csv_day/Malawi/additionalGuideline.csv")
# enforcedRestrictionsGraphBAND <- read_csv("summary_csv_day/Malawi/enforcedRestrictions.csv")
enforcedLockdownGraphBAND <- read_csv("summary_csv_day/Malawi/enforcedLockdown.csv")
#scenario1GraphBAND <- read_csv("summary_csv_day/Malawi/scenario1.csv")
#scenario2GraphBAND <-read_csv("summary_csv_day/Malawi/scenario2.csv")
# additionalGuidelines21dayGraphBAND <- read_csv("summary_csv_day/Malawi/additionalGuideline21day.csv")
# enforcedRestrictions21dayGraphBAND <- read_csv("summary_csv_day/Malawi/enforcedRestrictions21day.csv")
# enforcedLockdown21dayGraphBAND <- read_csv("summary_csv_day/Malawi/enforcedLockdown21day.csv")



#Add label column
# unmitigatedGraphBAND$Reduction <- "1. Unmitigated scenario"
currentGraphBAND$Reduction <- "Current mobility reductions continue"
# additionalGuidelinesGraphBAND$Reduction <- "3. Additional guidelines for 21 day period"
# enforcedRestrictionsGraphBAND$Reduction <- "4. Restrictive measures enforced for 21 day period"
enforcedLockdownGraphBAND$Reduction <- "Lockdown implemented"

#scenario1Graph$Reduction <- "3. Enforced restrictions in limited urban areas"
#scenario2Graph$Reduction <- "4. Enforced restrictions in additional urban areas"

#Create comparison df
#comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, scenario1Graph, scenario2Graph)
comparisonDF <- bind_rows(currentGraphBAND, enforcedLockdownGraphBAND)
comparisonDF <- select(comparisonDF, -c("X1_1"))
comparisonDF <- melt(comparisonDF, id = c("X1", "Reduction"))
names(comparisonDF)[names(comparisonDF)=="variable"] <- "State"

#Make the charts
options(scipen=10000)

makeComparisonGraph(comparisonDF, "Infected", "Infections", "images/Malawi/infectionScenariosAge_50.png")
makeComparisonGraph(comparisonDF, "Hospitalized", "Hospitalizations", "images/Malawi/hospitalizationScenariosAge_50.png")
makeComparisonGraph(comparisonDF, "Critical", "Critical Care", "images/Malawi/critcareScenariosAge_50.png")
makeComparisonGraph(comparisonDF, "Deaths", "Deaths", "images/Malawi/deathsScenariosAge_50.png")

# ####Create summary files - Burkina####
# 
# #Load individual csvs -NOTE, just change names of source folder to update
# unmitigated <- makeCombinedDF("epi_csvs/Burkina/unmitigated")
# current <- makeCombinedDF("epi_csvs/Burkina/current")
# additionalGuidelines <- makeCombinedDF("epi_csvs/Burkina/additionalGuidelines")
# enforcedRestrictions <- makeCombinedDF("epi_csvs/Burkina/enforcedRestrictions")
# enforcedLockdown <- makeCombinedDF("epi_csvs/Burkina/enforcedLockdown")
# additionalGuidelines21day <- makeCombinedDF("epi_csvs/Burkina/additionalGuidelines21day")
# enforcedRestrictions21day <- makeCombinedDF("epi_csvs/Burkina/enforcedRestrictions21day")
# enforcedLockdown21day <- makeCombinedDF("epi_csvs/Burkina/enforcedLockdown21day")
# 
# #Create the current scenarios - 1: 4 major urban centers; 2: 4 + few otherrs
# scenario1 <- makeCombinedScenario(current, enforcedRestrictions, c("Bogodogo", "Baskuy", "Boulmiougou", "Nongr-Massoum", "Do", "Sig-Nonghin", "Dafra", "Gorom-Gorom", "Boromo", "Ziniare", "Dedougou", "Kongoussi", "Dori", "Zorgho", "Hounde", "Banfora", "Sindou", "Sapone", "Manga", "Dano", "Nouna"))
# scenario2 <- makeCombinedScenario(current, enforcedRestrictions, c("Bogodogo", "Baskuy", "Boulmiougou", "Nongr-Massoum", "Do", "Sig-Nonghin", "Dafra", "Gorom-Gorom", "Boromo", "Ziniare", "Dedougou", "Kongoussi", "Dori", "Zorgho", "Hounde", "Banfora", "Sindou", "Sapone", "Manga", "Dano", "Nouna"))
# 
# 
# #Create the daily summary
# makeSummaryCSV(unmitigated, "summary_csv_day/unmitigated.csv")
# makeSummaryCSV(current, "summary_csv_day/current.csv")
# makeSummaryCSV(additionalGuidelines, "summary_csv_day/additionalGuideline.csv")
# makeSummaryCSV(enforcedRestrictions, "summary_csv_day/enforcedRestrictions.csv")
# makeSummaryCSV(enforcedLockdown, "summary_csv_day/enforcedLockdown.csv")
# makeSummaryCSV(scenario1, "summary_csv_day/scenario1.csv")
# makeSummaryCSV(scenario2, "summary_csv_day/scenario2.csv")
# makeSummaryCSV(additionalGuidelines21day, "summary_csv_day/additionalGuideline21day.csv")
# makeSummaryCSV(enforcedRestrictions21day, "summary_csv_day/enforcedRestrictions21day.csv")
# makeSummaryCSV(enforcedLockdown21day, "summary_csv_day/enforcedLockdown21day.csv")

#Create the district level summary
# makeSummaryCSVGeo(unmitigated, "summary_by_district/unmitigated.csv")
# makeSummaryCSVGeo(current, "summary_by_district/current.csv")
# makeSummaryCSVGeo(additionalGuidelines, "summary_by_district/additionalGuideline.csv")
# makeSummaryCSVGeo(enforcedRestrictions, "summary_by_district/enforcedRestrictions.csv")
# makeSummaryCSVGeo(enforcedLockdown, "summary_by_district/enforcedLockdown.csv")
# makeSummaryCSVGeo(scenario1, "summary_by_district/scenario1.csv")
# makeSummaryCSVGeo(scenario2, "summary_by_district/scenario2.csv")
# makeSummaryCSVGeo(additionalGuidelines21day, "summary_by_district/additionalGuideline21day.csv")
# makeSummaryCSVGeo(enforcedRestrictions21day, "summary_by_district/enforcedRestrictions21day.csv")
# makeSummaryCSVGeo(enforcedLockdown21day, "summary_by_district/enforcedLockdown21day.csv")

# ####Create Graphs####
# unmitigatedGraph <- read_csv("summary_csv_day/unmitigated.csv")
# currentGraph <- read_csv("summary_csv_day/current.csv")
# additionalGuidelinesGraph <- read_csv("summary_csv_day/additionalGuideline.csv")
# enforcedRestrictionsGraph <- read_csv("summary_csv_day/enforcedRestrictions.csv")
# enforcedLockdownGraph <- read_csv("summary_csv_day/enforcedLockdown.csv")
# scenario1Graph <- read_csv("summary_csv_day/scenario1.csv")
# scenario2Graph <-read_csv("summary_csv_day/scenario2.csv")
# additionalGuidelines21dayGraph <- read_csv("summary_csv_day/additionalGuideline21day.csv")
# enforcedRestrictions21dayGraph <- read_csv("summary_csv_day/enforcedRestrictions21day.csv")
# enforcedLockdown21dayGraph <- read_csv("summary_csv_day/enforcedLockdown21day.csv")
# 
# #Add label column
# unmitigatedGraph$Reduction <- "1. Unmitigated scenario"
# currentGraph$Reduction <- "2. Current reduction of ~15% continue"
# additionalGuidelinesGraph$Reduction <- "3. Additional guidelines for 21 day period"
# enforcedRestrictionsGraph$Reduction <- "4. Restrictive measures enforced for 21 day period"
# enforcedLockdownGraph$Reduction <- "5. Lockdown enforced for 21 day period"
# 
# #scenario1Graph$Reduction <- "3. Enforced restrictions in limited urban areas"
# #scenario2Graph$Reduction <- "4. Enforced restrictions in additional urban areas"
# 
# #Create comparison df
# #comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, scenario1Graph, scenario2Graph)
# comparisonDF <- bind_rows(unmitigatedGraph, currentGraph, enforcedRestrictionsGraph, enforcedLockdownGraph, additionalGuidelinesGraph)
# comparisonDF <- select(comparisonDF, -c("X1_1"))
# comparisonDF <- melt(comparisonDF, id = c("X1", "Reduction"))
# names(comparisonDF)[names(comparisonDF)=="variable"] <- "State"
# 
# #Make the charts
# options(scipen=10000)
# 
# makeComparisonGraph(comparisonDF, "Infected", "Infections", "images/Burkina/infectionScenarios.png")
# makeComparisonGraph(comparisonDF, "Hospitalized", "Hospitalizations", "images/Burkina/hospitalizationScenarios.png")
# makeComparisonGraph(comparisonDF, "Critical", "Critical Care", "images/Burkina/critcareScenarios.png")
# makeComparisonGraph(comparisonDF, "Deaths", "Deaths", "images/Burkina/deathsScenarios.png")
# 
# 
