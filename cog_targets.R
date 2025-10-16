## COGNITION TARGETS
cog_targets <- list(
  tar_map(
    values = list(
      cog_outcome =
        c(
          "g",
          "MS_OverallScore_C",
          "COWAT",
          "HVLT_TotalRecall",
          "HVLT_DelayedRecall",
          "HVLT_Retention",
          "HVLT_RDI",
          "SDMT"
        )
    ),
    tar_target(
      cog_estimates,
      get_estimates_cognition(
        prepared_data,
        cog_outcome,
        comparator = "control",
        effect_modifier = NULL
      ),
      pattern = map(prepared_data),
      iteration = "list"
    ),
    tar_target(
      cog_intervals,
      cognition_intervals(
        cog_estimates,
        effect_modifier = NULL,
        satterthwaite = TRUE
      )
    ),
    tar_target(cog_plot, plot_estimates(cog_intervals))
  ),
  
  # Save plots
  tar_target(
    output_plots,
    save_cognition_plots(
      cog_plot_g,
      cog_plot_MS_OverallScore_C,
      cog_plot_COWAT,
      cog_plot_HVLT_TotalRecall,
      cog_plot_HVLT_DelayedRecall,
      cog_plot_HVLT_Retention,
      cog_plot_HVLT_RDI,
      cog_plot_SDMT
    )
  ),
  
  ## Effect modification
  # create binary splits for effect modification assessment
  tar_target(
    effect_mod_data,
    lapply(prepared_data, function(.x) {
      .x$hearing <- ifelse(.x$BLToneAvg_Better < 30, "first", "second")
      .x$age <- ifelse(.x$AgeAtRand < 75, "first", "second")
      .x$ms <- ifelse(.x$BL_MS_OverallScore_C >= 95, "first", "second")
      .x$pcs <- ifelse(.x$BL_PCS >= 50, "first", "second")
      .x$frailty <- ifelse(.x$BL_Frailty_DAI50 < 0.09, "first", "second")
      .x$gender <- ifelse(.x$Gender == 1, "first", "second")
      .x$e4 <- ifelse(round(.x$apoe_e4) == 0, "first", "second")
      .x
    }),
    pattern = map(prepared_data)
  ),
  tar_map(
    values = list(
      effect_modifier =
        c("hearing", "age", "ms", "pcs", "frailty", "gender", "e4")
    ),
    tar_target(
      cog_estimates,
      get_estimates_cognition(
        effect_mod_data,
        outcome_var = "g",
        comparator = "control",
        effect_modifier = effect_modifier
      ),
      pattern = map(effect_mod_data),
      iteration = "list"
    ),
    tar_target(
      cog_intervals,
      cognition_intervals(
        cog_estimates,
        effect_modifier = effect_modifier,
        satterthwaite = TRUE
      )
    ),
    tar_target(cog_plot, plot_estimates(cog_intervals))
  ),
  # Save plots
  tar_target(
    effect_mod_plots,
    save_effect_mod_plots(
      cog_intervals_hearing,
      cog_intervals_age,
      cog_intervals_ms,
      cog_intervals_pcs,
      cog_intervals_frailty,
      cog_intervals_gender,
      cog_intervals_e4
    )
  )
)