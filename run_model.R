library(ggplot2)

start_time <- Sys.time()

setwd("~/code/cs/covid19/inputs")

source('../epi_stepwise.R')

# setwd("C:/Users/Michael/Git/Malawi-COVID-Epidemiological-Model/inputs")
#setwd("C:/Users/dylan/Documents/GitHub/Malawi-COVID-Epidemiological-Model/inputs")

df_distancing <- read_csv('reductionScenarios/current.csv')
df_masking <- read_csv('masking/masking_compliance.csv')
df_locations <- read_csv('MW COVID Inputs.csv')
df_params <- read_csv('params_inits_template.csv')
df_seed <- read_csv('simulation-seeddates-ta-20200910.csv')

model_results <- run_stepwise(df_params, df_locations, df_masking, df_distancing, df_seed)

df_country <- model_results$country

df_for_plot <- model_results$country # %>% filter(Lvl3 == 'Mzuzu City')

ggplot(data=df_for_plot %>% filter(!(State %in% c('Susceptible','Recovered'))), aes(x=Day, y=People, group=State, color=State)) + geom_line()

df_country_infected <- subset(df_country, State=='Infected')
df_country_newinfected <- subset(df_country, State=='New Infections')
df_country_hospitalized <- subset(df_country, State=='Hospitalized')
df_country_critical <- subset(df_country, State=='Critical')
df_country_deaths <- subset(df_country, State=='Dead')

write.csv(df_country_infected, '../out/infected.csv')
write.csv(df_country_newinfected, '../out/new-infections.csv')
write.csv(df_country_hospitalized, '../out/hospitalized.csv')
write.csv(df_country_critical, '../out/critical.csv')
write.csv(df_country_deaths, '../out/deaths.csv')
