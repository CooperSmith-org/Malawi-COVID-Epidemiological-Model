makeCombinedDF <-
function(location) {
  df <- list.files(path = location,full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  return(df)
}
