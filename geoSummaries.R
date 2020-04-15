library(dplyr)
library(reshape)
library(ggplot2)
library(scales)
library(readr)

df_45_.35G <- list.files(path = "epi_csvs/45-0.35",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

df_45_.75G <- list.files(path = "epi_csvs/45-0.75",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_45_.15G <- list.files(path = "epi_csvs/45-0.15",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_45_.50G <- list.files(path = "epi_csvs/45-0.5",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

baselineG <- list.files(path = "epi_csvs/baseline",full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

#################################FUNCTION TO COMBINE DIFF SCENARRIOS BY TA ###########################################

makeCombinedScenario <-function(base_df, scenario_df, TAList) {
  scenario_df$scenario <- "reduction"
  base_df$scenario <- "baseline"
  
  use1 <- subset(scenario_df, scenario_df$TA %in% TAList)
  use2 <- subset(base_df, base_df$TA %in% setdiff(unique(base_df$TA), TAList))
  return(bind_rows(use1,use2)) 
  }

#################################FUNCTION TO MAKE A GRAPH FOR A TA ###########################################

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
  #Override scientific notation default
  options(scipen=10000)
  
  ####Create summary immage for each TA
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

##################*******FUNCTIONS FOR ALL DISTRICTS*******##############################
makeSummaryGraphAll <- function(df, title, filename) {
  names(df)[names(df)=="X1"] <- "Day"
  
  df <- df %>%
    group_by(Day) %>%
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
  
  longData <- melt(df, id = c("Day"))
  longData$variable <- factor(longData$variable,
                              levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical","Deaths"))
  
  names(longData)[names(longData)=="variable"] <- "State"
  #Override scientific notation default
  options(scipen=10000)
  
  ####Create summary immage for each TA
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
#################################FUNCTION TO COMBINE DIFF SCENARIOS BY TA ###########################################

##Run test scenario
scenario1 <- makeCombinedScenario(df_45_.15G, df_45_.50G, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota"))
scenario2 <- makeCombinedScenario(df_45_.15G, df_45_.50G, c("Lilongwe City", "Lilongwe", "Blantyre", "Blantyre City", "Chikwawa", "Nkhotakota", "Salima", "Mzuzu City", "Zomba City", "Zomba", "Mzimba", "Mangochi"))

df_1 <- scenario1 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

df_2 <- scenario2 %>% 
  group_by(X1) %>% 
  summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))

write.csv(df_1, "summary_csv_day/currentScenario1.csv")
write.csv(df_2, "summary_csv_day/currentScenario2.csv")

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
ggplot(data=subset(meltedDF, meltedDF$State %in% c("Infected")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected based on a policy of social distancing implemented ...") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/geovariation_infected.png", height=4 , width =8)

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
  theme_minimal()
ggsave("images/new_scenarios/geovariation_hospitalizations.png", height=4 , width =8)

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
  theme_minimal()
ggsave("images/new_scenarios/geovariation_critical.png", height=4 , width =8)

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
  theme_minimal()
ggsave("images/new_scenarios/geovariation_deaths.png", height=4 , width =8)
