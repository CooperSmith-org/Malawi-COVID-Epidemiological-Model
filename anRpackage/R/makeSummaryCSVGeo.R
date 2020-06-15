makeSummaryCSVGeo <-
function(df, filename) {
  dftocsv <- df %>%
    group_by(TA, ID) %>%
    summarise(Population = max(POP), Incidents = max(R) + max(D), Recovered = max(R), Deaths = max(D), Peak_Hospital = max(H), Peak_Crit = max(C), Cumulative_Hospital = max(hosp), Cumulative_Critical = max(crits))
  write.csv(dftocsv, filename)
}
