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

makeSummary <- function(df, TAName, title, filename) {
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
  
## Make 2 to check
makeSummary(baselineG, "Lilongwe City", "Number of individuals infected, hospitalized, in critical care, and deaths over time in revised baseline scenario assuming 20% reduction in mobility, for Lilongwe City", 
            "images/new_scenarios/TAs/LLWCity.png")

makeSummary(baselineG, "Kasungu", "Number of individuals infected, hospitalized, in critical care, and deaths over time in revised baseline scenario assuming 20% reduction in mobility, for Kasungu", 
            "images/new_scenarios/TAs/Kasungu.png")

