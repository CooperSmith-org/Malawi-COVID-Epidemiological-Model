plot_comparison <- function(df) {
  all_plots = list()
  for (comparison in unique(df$Compare)) {
    plots_comparison <- list()
    df_low <- df %>%
      filter(Compare == comparison, Low_High == 'low')
    plots_comparison[['low']] <- plot_states(df_low)
    
    df_high <- df %>%
      filter(Compare == comparison, Low_High == 'low')
    plots_comparison[['high']] <- plot_states(df_high)
    all_plots[[comparison]] <- plots_comparison
  }
  return(all_plots)
}



plot_states <- function(df) {
  
  df_sub <- df %>%
    group_by(Model, State, Day) %>%
    summarize(People = sum(People))
  plots <- list()
  
  for (state in unique(df_sub$State)) {
    df_plot <- df_sub %>%
      filter(State == state)
    
    p <- ggplot(df_plot, aes(x=Day, y=People)) +
      geom_line(aes(color = Model)) +
      labs(y=state)
    plots[[state]] <- p
    
  } 
  return(plots)
}
