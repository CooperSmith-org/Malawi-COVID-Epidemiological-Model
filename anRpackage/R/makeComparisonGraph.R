makeComparisonGraph <-
function(df, diseaseState, title, fileName) {
  ggplot(data=subset(df, df$State %in% c(diseaseState)) %>% arrange(State),
         aes(x=X1, y=value, fill=Reduction, color=Reduction)) +
    geom_line(stat="identity", position = "identity") +
    scale_colour_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
    scale_fill_manual(values=c("lightblue4", "red", "blue", "indianred4", "gray")) + 
    xlab("Day (from t=0)") +
    ylab("Number of people") +
    ggtitle(title) +
    scale_y_continuous(label=comma) +
    theme_minimal() #+
  #theme(legend.position = "none") #turn on and off for legend
  ggsave(fileName, height=4 , width =8)
}
