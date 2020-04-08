library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

###################################################*******BASELINE********#############################################################

#First, generate baseline charts
baseline <- read.csv('summary_csv_day/baseline.csv')
baseline <- select(baseline, -c("X"))
longData <- melt(baseline, id = c("X1"))

#Order for purposes of chart
longData$variable <- factor(longData$variable,
                            levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical","Deaths"))

names(longData)[names(longData)=="variable"] <- "State"
#Override scientific notation default
options(scipen=10000)

# Summary chart - All states visualized together
ggplot(data=subset(longData, longData$State %in% c("Infected", "Hospitalized","Critical","Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) +
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected, hospitalized, in critical care, and deaths over time in baseline scenario of no mitigation") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/no-mitigation-baseline.png", height=4, width=8)


########################################**************NEW IMAGES*********************###############################################

#First, generate baseline charts
baseline2 <- read.csv('summary_csv_day/new_summary_45-.15.csv')
baseline2 <- select(baseline2, -c("X"))
longData4 <- melt(baseline2, id = c("X1"))

#Order for purposes of chart
longData4$variable <- factor(longData4$variable,
                             levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical","Deaths"))

names(longData4)[names(longData4)=="variable"] <- "State"
#Override scientific notation default
options(scipen=10000)

# Summary chart - All states visualized together
ggplot(data=subset(longData4, longData4$State %in% c("Infected", "Hospitalized","Critical","Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected, hospitalized, in critical care, and deaths over time in revised baseline scenario assuming 20% reduction in mobility") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/new_baseline.png", height=4 , width =8)

# First chart - just infected
ggplot(data=subset(longData4, longData4$State %in% c("Infected")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected in revised baseline scenario assuming 20% reduction in mobility") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/new_baseline_infected.png", height=4 , width =8)

#Second chart - just hospitalized
ggplot(data=subset(longData4, longData4$State %in% c("Hospitalized")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals hospitalized in revised baseline scenario assuming 20% reduction in mobility") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/new_baseline_hospitalized.png",height=4 , width =8)

# third chart - just critical care
ggplot(data=subset(longData4, longData4$State %in% c("Critical")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals in critical care in revised baseline scenario assuming 20% reduction in mobility") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/new_baseline_critical.png", height=4 , width =8)

# fourth chart - just deaths
ggplot(data=subset(longData4, longData4$State %in% c("Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of deaths in revised baseline scenario assuming 20% reduction in mobility") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/new_baseline_deaths.png", height=4 , width =8)

##############################################*****VARY THE INCREMENTAL REDUCTION*****#################################################

#Load the files for new run
df_45_35 <- read.csv('summary_csv_day/new_summary_45-.35.csv')
df_45_35$Reduction <- "Additional guidelines (30-40% reduction in mobility)"
df_45_50 <- read.csv('summary_csv_day/new_summary_45-.50.csv')
df_45_50$Reduction <- "Enforced population restrictions (40-60% reduction in mobility)"
df_45_75 <- read.csv('summary_csv_day/new_summary_45-.75.csv')
df_45_75$Reduction <- "Strict enforcement of lockdown (75% mobility reduction)"

df_new_reduction <- bind_rows(df_45_35, df_45_50, df_45_75)
df_new_reduction <- select(df_new_reduction, -c("X"))

#add in baseline
baseline2$Reduction <- "No further reduction from current"
baseline$Reduction <- "No mitigation"
df_new_reduction <- bind_rows(df_new_reduction, baseline, baseline2)

longData5 <- melt(df_new_reduction, id = c("X1", "Reduction"))
names(longData5)[names(longData5)=="variable"] <- "State"

#Override scientific notation default
options(scipen=10000)

# First chart - infections
ggplot(data=subset(longData5, longData5$State %in% c("Infected")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected based on a policy of social distancing implemented [....ADD VERBIAGE AS DESIRED]") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/variation_infected.png", height=4 , width =8)

# Second chart - Hospitalizations
ggplot(data=subset(longData5, longData5$State %in% c("Hospitalized")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals hospitalized based on a policy of social distancing implemented [....ADD VERBIAGE AS DESIRED]") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/variation_hospitalizations.png", height=4 , width =8)

# Third chart - Critical care
ggplot(data=subset(longData5, longData5$State %in% c("Critical")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals in critical care based on a policy of social distancing implemented [....ADD VERBIAGE AS DESIRED]") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/variation_critical.png", height=4 , width =8)

# Fourth chart
ggplot(data=subset(longData5, longData5$State %in% c("Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of deaths based on a policy of social distancing implemented[....ADD VERBIAGE AS DESIRED]") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/new_scenarios/variation_deaths.png", height=4 , width =8)