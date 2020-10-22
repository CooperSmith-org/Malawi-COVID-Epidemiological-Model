library(tidyverse)

start_time <- Sys.time()

setwd("C:/Users/Michael/Git/Malawi-COVID-Epidemiological-Model/inputs")

AGE_CHILD <- 1
AGE_ADULT <- 2
AGE_ELDER <- 3

ages = c(AGE_CHILD, AGE_ADULT, AGE_ELDER)

df_distancing <- read_csv('reductionScenarios/current.csv')
df_masking <- read_csv('masking/masking_compliance.csv')
df_locations <- read_csv('MW COVID Inputs.csv')
df_params <- read_csv('params_inits_template.csv')
df_seed <- read_csv('simulation-seeddates-ta-20200910.csv')

# Setup model parameters

age_infection_rates <- rbind(
  c(df_params$ped2ped, df_params$ped2ad, df_params$ped2eld),
  c(df_params$ad2ped,  df_params$ad2ad,  df_params$ad2eld),
  c(df_params$eld2ped, df_params$eld2ad, df_params$eld2eld)
)

susceptibility <- c(df_params$susceptibility_p, df_params$susceptibility_a, df_params$susceptibility_e)

r0 <- df_params$R0
exposed_time <- 1 / df_params$kappa
infected_time <- 1 / df_params$kappa2
hosp_time <- 1 / df_params$tau
crit_time <- 1 / df_params$tau2
mask_effectiveness <- df_params$efficacy
seed_threshold <- df_params$seed_date_threshold

excluded_locations <- c(20407,10106,20251,20118,20102,20511,21071,30303,21070,10110,10314)
df_locations <- df_locations %>% 
  filter(!(TA_Code %in% excluded_locations))

#df_locations <- head(df_locations, 5)
df_locations

df_locations$age_code = 0

df_locations$age_code[df_locations$Age == 'Pediatrics'] <- AGE_CHILD
df_locations$age_code[df_locations$Age == 'Adults'] <- AGE_ADULT
df_locations$age_code[df_locations$Age == 'Elderly'] <- AGE_ELDER

behaviour_mod <- (1 - df_distancing$reduc) * (1 - df_masking$masking_compliance * mask_effectiveness)

base_infection_rate <- r0 / (exposed_time + infected_time)

df_child = 
df_adult = 
df_elder = 

dfs_ages = list(
  df_locations[df_locations$age_code == AGE_CHILD,],
  df_locations[df_locations$age_code == AGE_ADULT,],
  df_locations[df_locations$age_code == AGE_ELDER,]
)

s <- list()
e <- list()
i <- list()
h <- list()
c <- list()
r <- list()
d <- list()

for (age in ages) {
  dfs_ages[[age]]$pop_infection_rate <- base_infection_rate * susceptibility[[age]] / dfs_ages[[age]]$Population
  dfs_ages[[age]]$empty_state = 0
  
  s[[age]] <- matrix(dfs_ages[[age]]$Population)
  e[[age]] <- matrix(dfs_ages[[age]]$empty_state)
  i[[age]] <- matrix(dfs_ages[[age]]$empty_state)
  h[[age]] <- matrix(dfs_ages[[age]]$empty_state)
  c[[age]] <- matrix(dfs_ages[[age]]$empty_state)
  r[[age]] <- matrix(dfs_ages[[age]]$empty_state)
  d[[age]] <- matrix(dfs_ages[[age]]$empty_state)
}

# Initial infections - 1 adult on day 1 in every location
e[[AGE_ADULT]][,1] <- 1 
n_days = 365

for (day in 2:n_days) {
  yday <- day - 1
  
  for (age in ages) {
    s_ <- s[[age]]
    e_ <- e[[age]]
    i_ <- i[[age]]
    h_ <- h[[age]]
    c_ <- c[[age]]
    r_ <- r[[age]]
    d_ <- d[[age]]
    
    # Calculate new infections from each source age
    new_by_age <- sapply(ages, function(src_age) {
      dfs_ages[[src_age]]$pop_infection_rate * age_infection_rates[[src_age, age]] * (e[[src_age]][,yday] + i[[src_age]][,yday])
    })
    new_infections <- rowSums(new_by_age) * behaviour_mod[[day]] * s_[,yday]
    
    s[[age]] <- cbind(s_, s_[,yday] - new_infections)
    e[[age]] <- cbind(e_, e_[,yday] + new_infections - e_[,yday] / exposed_time)
    i[[age]] <- cbind(i_, i_[,yday] + e_[,yday] / exposed_time - i_[,yday] / infected_time)
    h[[age]] <- cbind(h_, h_[,yday] + i_[,yday] * dfs_ages[[age]]$Hospitalization / infected_time - h_[,yday] / hosp_time)
    c[[age]] <- cbind(c_, c_[,yday] + h_[,yday] * dfs_ages[[age]]$Crit_of_Hosp / hosp_time - c_[,yday] / crit_time)
    r[[age]] <- cbind(r_, r_[,yday] + 
                          i_[,yday] * (1 - dfs_ages[[age]]$Hospitalization) / infected_time +
                          h_[,yday] * (1 - dfs_ages[[age]]$Crit_of_Hosp) / hosp_time +
                          c_[,yday] * (1 - dfs_ages[[age]]$FR_of_Crit) / crit_time)
    d[[age]] <- cbind(d_, d_[,yday] + c_[,yday] * dfs_ages[[age]]$FR_of_Crit / crit_time)
  }
}

end_time <- Sys.time()

print(end_time - start_time)

l = 400

plot(s[[AGE_ADULT]][l,], type='l', col='blue', ylim=c(0,max(s[[AGE_ADULT]][l,])))
lines(e[[AGE_ADULT]][l,], type='l', col='purple')
lines(i[[AGE_ADULT]][l,], type='l', col='yellow')
lines(h[[AGE_ADULT]][l,], type='l', col='orange')
lines(c[[AGE_ADULT]][l,], type='l', col='red')
lines(r[[AGE_ADULT]][l,], type='l', col='green')
lines(d[[AGE_ADULT]][l,], type='l', col='black')

