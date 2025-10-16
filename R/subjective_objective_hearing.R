## Objective and subjective hearing function

plot_objective_vs_subjective_hearing <- function(df){
  
  df <- undo_standardise(df)
  
  df$HearingProbs <- as.factor(df$HearingProbs)
  
  levels(df$HearingProbs) <- c("No", "Yes")

  ## Plot 
  
  plot <- df |> 
    filter(!is.na(HearingProbs)) |> 
    ggplot(aes(x = BLToneAvg_Better, 
               colour = as.factor(HearingProbs), 
               fill = as.factor(HearingProbs))) + 
    geom_density(alpha= 0.3) +
    labs(x = "Better-ear 4-frequency PTA",
         y = "Density",
         colour = "Subjective hearing impairment",
         fill  = "Subjective hearing impairment") +
    geom_vline(aes(xintercept = 30), 
               alpha = 0.5, linetype = "dashed") +
    geom_vline(aes(xintercept = 70), 
               alpha = 0.5, linetype = "dashed") +
    bayesplot::theme_default() +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80))
  
  ggsave("figures/subjective_vs_objective_hearing_density.png", 
         plot,
         device = "png", 
         height = 6, width = 8, 
         bg = "white")
  
  return(plot)
}

  
