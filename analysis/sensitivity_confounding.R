## Sensitivity for confounding by indication/healthcare utilisation

sensitivity_confounding <- function(df_list){
  
  # to long
  long_list <- map(df_list, 
                   ~ map(.x, 
                         to_long, 
                         outcome_var = "g", 
                         final_visit = 10, 
                         effect_modifier = NULL))
  
  # add indicator for prescribed & non-adherent vs non-prescribed
  
  long_list <- map(long_list,
                   ~ map(.x, function(x)
                     mutate(
                       x,
                       A = case_when(
                         Y3M_HearingAid == 1 & Y3M_HearingAidUse == 1 ~ 1,
                         Y3M_HearingAid == 0 & Y3M_HearingAidUse == 1 ~ 0,
                         TRUE ~ NA
                       )
                     )))
  
  long_list <- map(long_list, ~ map(.x, function(x) filter(x, !is.na(A))))
  
  # estimate effect
  
  get_itt <- function(df){
    out <- 
      intention_to_treat(df, 
                         Y = "g", 
                         A = "A")
    out <- 
      get_estimates_itt(out, 
                        df, 
                        A = "A", 
                        comparator = "control", 
                        effect_modifier = NULL)
    return(out)
  }
  
  itt <- map(long_list, 
             ~ map(.x, get_itt),
             .progress = TRUE)
  
  itt <- map(itt, ~ list(estimates_itt = .x))
  
  estimates <- add_intervals(itt, 
                             satterthwaite = TRUE, 
                             as_treated = FALSE, 
                             effect_modifier = NULL)
  
  contrasts <- get_contrasts(itt, 
                             satterthwaite = TRUE, 
                             as_treated = FALSE,
                             effect_modifier = NULL)
  
  itt_plot <- plot_estimates(estimates, 
                             contrasts, 
                             comparator = "Control",
                             as_treated = FALSE)$itt_plot
  
  itt_plot <- itt_plot +
    scale_colour_manual(values = c("black", "#bc3c24"),
                        labels = c("No HA prescription",
                                   "New HA prescription (never user)"))
  
  itt_md_plot <- plot_estimates(estimates, 
                                contrasts, 
                                comparator = "Control",
                                as_treated = FALSE)$itt_md_plot
  
  plots <- list(itt_plot = itt_plot, itt_md_plot = itt_md_plot)
  
  save_cog_plot(plots = plots,
                file_name = "g_sensitivity_confounding_indication.png", 
                ylab = "Mean cognitive factor score", 
                breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5), 
                limits = c(-1,0.5),
                md_breaks = c(-0.4,-0.2,0, 0.2, 0.4),
                md_limits = c(-0.4,0.4),
                as_treated = FALSE
  )
  
  return(list(estimates = estimates, contrasts = contrasts, plots = plots))
}

