## Sensitivity analysis: participants with objective hearing impairment

hearing_sensitivity <- function(dataset_paths, cind, xt04){
  
  df_list <- read_and_process_data(dataset_paths, selfrated_hearing = FALSE)
  df_list <- prepare_outcomes(df_list, cind, xt04)
  
  # Overall cognition score
  out <- get_results(df_list, "g")
  
  save_cog_plot(
    out$plots,
    file_name = "g_sensitivity_hearing_eligibility.png",
    ylab = "Mean overall cognition score",
    breaks = c(-1.25, -1, -0.75, -0.5, -0.25, 0, 0.25),
    limits = c(-1.25, 0.25),
    md_breaks = c(-0.3,-0.2,-0.1, 0, 0.1, 0.2, 0.3, 0.4),
    md_limits = c(-0.3,0.4)
  )
  
  # Dementia
  dem_estimates <- survival_estimates(df_list, "Dem")
  dem_intervals <- survival_intervals(dem_estimates, satterthwaite = TRUE)
  
  save_cuminc_plot(
    dem_intervals$plots,
    file_name = "Dementia_sensitivity_hearing_eligibility.png",
    y_label = "Dementia risk",
    breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15),
    limits = c(0, 0.15),
    rr_breaks = c(0.5,0.75,1,1.25, 1.5),
    rr_limits = c(0.5,1.5)
  )
  
  # Impairment
  # Dementia
  imp_estimates <- survival_estimates(df_list, "Imp")
  imp_intervals <- survival_intervals(imp_estimates, satterthwaite = TRUE)
  
  save_cuminc_plot(
    imp_intervals$plots,
    file_name = "impairment_hearing_eligibility.png",
    y_label = "Cognitive impairment risk",
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    limits = c(0, 0.6),
    rr_breaks = c(0.5, 0.75, 1, 1.25, 1.5),
    rr_limits = c(0.5,1.55)
  )
  
  return(list(Dem = dem_intervals, Imp = imp_intervals))
}
