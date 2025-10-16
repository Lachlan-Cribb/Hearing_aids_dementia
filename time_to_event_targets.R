time_to_event_targets <- list(
  tar_map(
    values = list(outcome_var = c("Dem", "Imp", "Death", "Cancer")),
    tar_target(
      surv_estimates,
      survival_estimates(prepared_data, outcome_var = outcome_var),
      pattern = map(prepared_data),
      iteration = "list"
    ), 
    
    # Bootstrap confidence intervals 
    tar_target(
      surv_intervals,
      add_intervals_survival(surv_estimates, satterthwaite = TRUE)), 
    
    # plot
    tar_target(surv_plot, plot_cumulative_inc(surv_intervals))
  ),
  tar_target(
    save_surv_plots, 
    save_survival_plots(surv_plot_Dem, surv_plot_Imp, surv_plot_Death, surv_plot_Cancer)),
  tar_target(surv_table, get_surv_table(surv_intervals_Dem, surv_intervals_Imp))
)