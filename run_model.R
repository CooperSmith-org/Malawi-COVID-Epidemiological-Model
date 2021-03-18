library(ggplot2)

start_time <- Sys.time()

setwd("~/code/cs/covid19/inputs")

source('../epi_stepwise.R')

# setwd("C:/Users/Michael/Git/Malawi-COVID-Epidemiological-Model/inputs")
#setwd("C:/Users/dylan/Documents/GitHub/Malawi-COVID-Epidemiological-Model/inputs")

df_params <- read_csv('../inputs/params_inits_template.csv', col_types=cols())
df_distancing <- read_csv('../inputs/reductionScenarios/current.csv', col_types=cols())
df_masking <- read_csv('../inputs/masking/masking_compliance.csv', col_types=cols())
df_locations <- read_csv('../inputs/MW COVID Inputs.csv', col_types=cols())
df_seed <- read_csv('../inputs/simulation-seeddates-ta-20200910.csv', col_types=cols())

start_date = as.Date("2020-01-01")
n_days <- dim(df_distancing)[1] + 30

model_results <- run_stepwise(df_params, df_locations, df_masking, df_distancing, df_seed, n_days)

df_country <- model_results$country

df_for_plot <- model_results$country # %>% filter(Lvl3 == 'Mzuzu City')

ggplot(data=df_for_plot %>% filter(!(State %in% c('Susceptible','Recovered'))), aes(x=Day, y=People, group=State, color=State)) + geom_line()

df_country_infected <- subset(df_country, State=='Infected')
df_country_newinfected <- subset(df_country, State=='New Infections')
df_country_hospitalized <- subset(df_country, State=='Hospitalized')
df_country_critical <- subset(df_country, State=='Critical')
df_country_deaths <- subset(df_country, State=='Dead')

df_summary <- round(df_country %>% pivot_wider(names_from='State', values_from='People'))
add_column(df_summary, Date=start_date + (df_summary$Day - 1), .after='Day')
write.csv(df_summary, '../out/pandemic-seir.csv')
