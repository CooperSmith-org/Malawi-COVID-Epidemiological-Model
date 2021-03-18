library(ggplot2)
library(tidyverse)

# Repeats last row to pad vector to desired length
pad_dataframe <- function(df_in, length) {
  padding_length <- length - dim(df_in)[1]
  if (padding_length > 0) {
    to_append <- df_in[rep(nrow(df_in), padding_length),]
    return(rbind(df_in, to_append))
  }
  else {
    return(df_in)
  }
}

# Stepwise implementation of SEIR model
run_stepwise <- function(df_params, df_locations, df_masking, df_distancing, df_seed, n_days) {
  start_time <- Sys.time()
  
  AGE_CHILD <- 1
  AGE_ADULT <- 2
  AGE_ELDER <- 3
  
  ages = c(AGE_CHILD, AGE_ADULT, AGE_ELDER)
  age_names = c('Pediatric', 'Adult', 'Elderly')
  
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
  
  df_locations$age_code = 0
  
  df_locations$age_code[df_locations$Age == 'Pediatrics'] <- AGE_CHILD
  df_locations$age_code[df_locations$Age == 'Adults'] <- AGE_ADULT
  df_locations$age_code[df_locations$Age == 'Elderly'] <- AGE_ELDER
  
  seed_column <- paste('day_n', seed_threshold, sep='')
  df_seed_dates <- df_seed[c('adm_id', seed_column)] %>% rename(start_day=seed_column)
  df_locations <- left_join(df_locations, df_seed_dates, by=c('TA_Code'='adm_id'))
  
  df_distancing <- pad_dataframe(df_distancing, n_days)
  df_masking <- pad_dataframe(df_masking, n_days)
  
  behaviour_mod <- (1 - df_distancing$reduc) * (1 - df_masking$masking_compliance * mask_effectiveness)
  
  base_infection_rate <- r0 / (exposed_time + infected_time)
  
  dfs_ages <- list(
    df_locations[df_locations$age_code == AGE_CHILD,],
    df_locations[df_locations$age_code == AGE_ADULT,],
    df_locations[df_locations$age_code == AGE_ELDER,]
  )
  
  s <- list()
  n <- list()
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
    n[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    e[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    i[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    h[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    c[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    r[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    d[[age]] <- matrix(dfs_ages[[age]]$empty_state)
  }
  
  for (day in 2:n_days) {
    yday <- day - 1
    
    for (age in ages) {
      s_ <- s[[age]]
      n_ <- n[[age]]
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
      n[[age]] <- cbind(n_, 1 * new_infections)
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
    
    # Add one infected adult to all locations with this start day
    e[[AGE_ADULT]][,day] <- e[[AGE_ADULT]][,day] + (dfs_ages[[AGE_ADULT]]$start_day == day)
  }
  
  df_loc_info <- dfs_ages[[age]][c('TA_Code','Lvl3','Lvl4')]
  df_loc_info
  
  df_pandemic <- bind_rows(lapply(ages, function(age) {
    rbind(
      cbind(df_loc_info, Age=age_names[[age]], State='Susceptible', s[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='New Infections', n[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='Exposed', e[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='Infected', i[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='Hospitalized', h[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='Critical', c[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='Recovered', r[[age]]),
      cbind(df_loc_info, Age=age_names[[age]], State='Dead', d[[age]])
    )
  }))
  
  # write.csv(df_pandemic, '../out/pandemic-stepwise.csv')
  
  df_ta <- df_pandemic %>% pivot_longer(
    !matches("[A-Za-z]"), names_to='Day', values_to='People', 
    names_transform=list(Day=as.integer))
  
  df_district <- df_ta %>% 
    group_by(Lvl3,Day,State) %>%
    summarise(People=sum(People))
  
  df_country <- df_district %>% 
    group_by(Day,State) %>%
    summarise(People=sum(People))
  
  
#write.csv(df_country, '../out/pandemic-stepwise.csv')

  end_time <- Sys.time()

  print(end_time - start_time)

  return(list(country=df_country, district=df_district, ta=df_ta, pandemic=df_pandemic))
}
