library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(readr)
source("utils.R")

#Create DFs
df_45_.35G <- makeCombinedDF("epi_csvs/45-0.35")
df_45_.75G <- makeCombinedDF("epi_csvs/45-0.75") 
df_45_.15G <- makeCombinedDF("epi_csvs/45-0.15") 
df_45_.50G <- makeCombinedDF("epi_csvs/45-0.5")
baselineG <- makeCombinedDF("epi_csvs/baseline")

#Current scenarios for Malawi
scenario1 <- makeCombinedScenario(df_45_.15G, df_45_.50G, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota"))
scenario2 <- makeCombinedScenario(df_45_.15G, df_45_.50G, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota", "Salima", "Mzuzu City", "Zomba City", "Zomba", "Mzimba", "Mangochi"))

makeSummaryCSV(scenario1, "summary_csv_day/currentScenario1.csv")
makeSummaryCSV(scenario2, "summary_csv_day/currentScenario2.csv")

makeSummaryGraphAll(scenario1, "Infections, Hospitalizations, Critical Care, and Deaths for Scenario 1", "images/new_scenarios/currentScenario1.png")
makeSummaryGraphAll(scenario2, "Infections, Hospitalizations, Critical Care, and Deaths for Scenario 2", "images/new_scenarios/currentScenario2.png")

#################################******GENERATE COMPARISONS FOR DIFF SCENARIOS****###########################################

df_1$Reduction <- "3. Enforced population restrictions under scenario 1"
df_2$Reduction <- "4. Enforced population restrictions under scenario 2"

baseline <- read.csv('summary_csv_day/baseline.csv')
baseline2 <- read.csv('summary_csv_day/new_summary_45-.15.csv')
baseline2 <- select(baseline2, -c("X"))
baseline <- select(baseline, -c("X"))

#add in baseline
baseline2$Reduction <- "2. No further reduction from current"
baseline$Reduction <- "1. No mitigation"
df_use <- bind_rows(baseline, baseline2, df_1, df_2)

meltedDF <- melt(df_use, id = c("X1", "Reduction"))
names(meltedDF)[names(meltedDF)=="variable"] <- "State"

#Override scientific notation default
options(scipen=10000)

# First chart - infections
makeComparisonGraph(meltedDF, "Infected", "Number of individuals....", "images/new_scenarios/geovariation_infected.png")

# Second chart - Hospitalizations
ggplot(data=subset(meltedDF, meltedDF$State %in% c("Hospitalized")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals hospitalized based on a policy of social distancing implemented ...") +
  scale_y_continuous(label=comma) +
  theme_minimal()+
  theme(legend.position = "none") #turn on and off for leegend
ggsave("images/new_scenarios/geovariation_hospitalizations_NOLegend.png", height=4 , width =8)

# Third chart - Critical care
ggplot(data=subset(meltedDF, meltedDF$State %in% c("Critical")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals in critical care based on a policy of social distancing implemented [....ADD VERBIAGE AS DESIRED]") +
  scale_y_continuous(label=comma) +
  theme_minimal()+
  theme(legend.position = "none") #turn on and off for leegend
ggsave("images/new_scenarios/geovariation_critical_NOLegend.png", height=4 , width =8)

# Fourth chart
ggplot(data=subset(meltedDF, meltedDF$State %in% c("Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of deaths based on a policy of social distancing implemented[....ADD VERBIAGE AS DESIRED]") +
  scale_y_continuous(label=comma) +
  theme_minimal()+
  theme(legend.position = "none") #turn on and off for leegend
ggsave("images/new_scenarios/geovariation_deaths_NOLegend.png", height=4 , width =8)
