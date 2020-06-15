makeSummaryCSV <-
function(df, fileName) {
  df1 <- df %>% 
    group_by(X1) %>% 
    summarise(Susceptible = sum(S), Exposed = sum(E), Infected = sum(I), Recovered = sum(R), Hospitalized = sum(H), Critical = sum(C), Deaths = sum(D))
  write.csv(df1, fileName)
}
