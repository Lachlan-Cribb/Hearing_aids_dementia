#### SENSITIVITY ANALYSIS TARGETS
sensitivity_targets <- list(
  
  ## consent for ASPREE XT (negative outcome control)
  tar_target(
    xt_consent_estimates, 
    xt_consent_analysis(prepared_data, visits_data), 
    pattern = map(prepared_data)),
  tar_target(xt_consent_summary, summarise_xt_consent(xt_consent_estimates)),
  
  
  # IPW for time to event outcomes (sensitivity for model spec)
  tar_map(
    values = list(outcome_var = c("Dem", "Imp")),
    tar_target(
      ipw_estimates,
      survival_results_ipw(prepared_data, outcome_var),
      pattern = map(prepared_data),
      iteration = "list"),
    tar_target(
      ipw_intervals,
      add_intervals_survival(ipw_estimates, satterthwaite = TRUE))
  ),
  tar_target(ipw_plot_Dem, gform_vs_ipw(surv_intervals_Dem, ipw_intervals_Dem)),
  tar_target(ipw_plot_Imp, gform_vs_ipw(surv_intervals_Imp, ipw_intervals_Imp)),
  tar_target(saved_ipw_plots, save_ipw_plots(ipw_plot_Dem, ipw_plot_Imp)),
  
  ## Sensitivity for confounding by healthcare utilisation
  tar_target(confounding, sensitivity_confounding(prepared_data)),
  tar_target(confounding_summary, summarise_confounding(confounding)),
  
  
  ## Sensitivity with objective hearing eligibility criterion
  # Overall cognition score
  tar_target(
    estimates_g_hearing,
    get_estimates_cognition(
      data_hearing,
      outcome_var = "g",
      comparator = "control",
      effect_modifier = NULL
    ),
    pattern = map(data_hearing),
    iteration = "list"
  ),
  tar_target(
    intervals_g_hearing,
    cognition_intervals(
      estimates_g_hearing,
      effect_modifier = NULL,
      satterthwaite = TRUE
    )
  ),
  tar_target(plot_g_hearing, plot_estimates(intervals_g_hearing)),
  # survival endpoints
  tar_map(
    values = list(outcome_var = c("Dem", "Imp")),
    tar_target(
      surv_estimates_hear,
      survival_estimates(data_hearing, outcome_var = outcome_var),
      pattern = map(data_hearing),
      iteration = "list"
    ),
    
    # Bootstrap confidence intervals 
    tar_target(
      surv_intervals_hear,
      add_intervals_survival(surv_estimates_hear, satterthwaite = TRUE)), 
    
    # plot
    tar_target(surv_plot_hear, plot_cumulative_inc(surv_intervals_hear))
  ),
  tar_target(
    saved_hear_plots, 
    save_plots_hearing(plot_g_hearing, surv_plot_hear_Dem, surv_plot_hear_Imp)),
  
  
  ## plot objective vs subjective hearing
  tar_target(hearing_plot, plot_objective_vs_subjective_hearing(unimputed_data))
)
