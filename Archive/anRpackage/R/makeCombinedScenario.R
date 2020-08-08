makeCombinedScenario <-
function(base_df, scenario_df, TAList) {
  scenario_df$scenario <- "reduction"
  base_df$scenario <- "baseline"
  
  use1 <- subset(scenario_df, scenario_df$TA %in% TAList)
  use2 <- subset(base_df, base_df$TA %in% setdiff(unique(base_df$TA), TAList))
  return(bind_rows(use1,use2)) 
}
