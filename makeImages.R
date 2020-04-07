library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

###################################################*******BASELINE********#############################################################

#First, generate baseline charts
baseline <- read.csv('summary_csv_day/new_summary_baseline.csv')
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
ggsave("images/baseline.png")

# First chart - just infected
ggplot(data=subset(longData, longData$State %in% c("Infected")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected in baseline scenario of no mitigation") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/baseline_infected.png")

#Second chart - just hospitalized
ggplot(data=subset(longData, longData$State %in% c("Hospitalized")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals hospitalized in baseline scenario of no mitigation") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/baseline_hospitalized.png")

# third chart - just critical care
ggplot(data=subset(longData, longData$State %in% c("Critical")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals in critical care in baseline scenario of no mitigation") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/baseline_critical.png")

# fourth chart - just deaths
ggplot(data=subset(longData, longData$State %in% c("Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=State, color=State)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of deaths in baseline scenario of no mitigation") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/baseline_deaths.png")


##############################################*****VARY RATE*****#################################################

#Load the files for holding start date constant
df_45_25 <- read.csv('summary_csv_day/new_summary_45-.25.csv')
df_45_25$pct <- "75 percent"
df_45_50 <- read.csv('summary_csv_day/new_summary_45-.5.csv')
df_45_50$pct <- "50 percent"
df_45_75 <- read.csv('summary_csv_day/new_summary_45-.75.csv')
df_45_75$pct <- "25 percent"

df_45 <- bind_rows(df_45_25, df_45_50, df_45_75)
df_45 <- select(df_45, -c("X"))

#add in baseline
baseline$pct <- "No change"
df_45 <- bind_rows(df_45, baseline)

longData2 <- melt(df_45, id = c("X1", "pct"))

#Order for purposes of chart
longData2$variable <- factor(longData2$variable,
                             levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical", "Deaths"))

names(longData2)[names(longData2)=="variable"] <- "State"

#Override scientific notation default
options(scipen=10000)

# First chart - infections
ggplot(data=subset(longData2, longData2$State %in% c("Infected")) %>% arrange(State),
       aes(x=X1, y=value, fill=pct, color=pct)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected based on a policy of social distancing implemented at day 45, with varying adherence") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/reduc_variation_infections.png")

#Override scientific notation default
options(scipen=10000)

# Second chart - hospitalizations
ggplot(data=subset(longData2, longData2$State %in% c("Hospitalized")) %>% arrange(State),
       aes(x=X1, y=value, fill=pct, color=pct)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals hospitalized based on a policy of social distancing implemented at day 45, with varying adherence") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/reduc_variation_hospitalization.png")

# Third chart - critical care
ggplot(data=subset(longData2, longData2$State %in% c("Critical")) %>% arrange(State),
       aes(x=X1, y=value, fill=pct, color=pct)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals in critical care based on a policy of social distancing implemented at day 45, with varying adherence") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/reduc_variation_critical.png")

# Fourth chart - deaths
ggplot(data=subset(longData2, longData2$State %in% c("Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=pct, color=pct)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of deaths based on a policy of social distancing implemented at day 45, with varying adherence") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/reduc_variation_death.png")

##############################################*****VARY START DATE*****#################################################

#Load the files for holding rate constant
df_30_50 <- read.csv('summary_csv_day/new_summary_30-.5.csv')
df_30_50$start <- "Day 30"
df_45_50 <- read.csv('summary_csv_day/new_summary_45-.5.csv')
df_45_50$start <- "Day 45"
df_60_50 <- read.csv('summary_csv_day/new_summary_60-.75.csv')
df_60_50$start <- "Day 60"

df_start <- bind_rows(df_30_50, df_45_50, df_60_50)

#add in baseline
baseline$start <- "No policy"
baselineStart <- select(baseline, -c("pct"))
df_start <- bind_rows(df_start, baselineStart)

df_start <- select(df_start, -c("X"))
longData3 <- melt(df_start, id = c("X1", "start"))

#Order for purposes of chart
longData3$variable <- factor(longData3$variable,
                             levels = c("Susceptible", "Recovered","Exposed", "Infected", "Hospitalized", "Critical", "Deaths"))

names(longData3)[names(longData3)=="variable"] <- "State"

#Override scientific notation default
options(scipen=10000)

# First chart - infections
ggplot(data=subset(longData3, longData3$State %in% c("Infected")) %>% arrange(State),
       aes(x=X1, y=value, fill=start, color=start)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals infected based on a policy of social distancing implemented at varying start dates, assumed to result in 50% reduction in R0") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/start_variation_infected.png")

# Second chart - Hospitalizations
ggplot(data=subset(longData3, longData3$State %in% c("Hospitalized")) %>% arrange(State),
       aes(x=X1, y=value, fill=start, color=start)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals hospitalized based on a policy of social distancing implemented at varying start dates, assumed to result in 50% reduction in R0") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/start_variation_hospitalized.png")

# Third chart - Critical care
ggplot(data=subset(longData3, longData3$State %in% c("Critical")) %>% arrange(State),
       aes(x=X1, y=value, fill=start, color=start)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of individuals in critical care based on a policy of social distancing implemented at varying start dates, assumed to result in 50% reduction in R0") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/start_variation_critical.png")

# Fourth chart
ggplot(data=subset(longData3, longData3$State %in% c("Deaths")) %>% arrange(State),
       aes(x=X1, y=value, fill=start, color=start)) +
  geom_line(stat="identity", position = "identity") +
  scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4")) + 
  xlab("Day (from t=0)") +
  ylab("Number of people") +
  ggtitle("Number of deaths based on a policy of social distancing implemented at varying start dates, assumed to result in 50% reduction in R0") +
  scale_y_continuous(label=comma) +
  theme_minimal()
ggsave("images/start_variation_deaths.png")