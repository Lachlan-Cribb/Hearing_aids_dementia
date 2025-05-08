## Effect modification for overall cognition score 
## Plot effect modification

get_effect_mod_plots <- function(df_list){
  ### Hearing function ###
  
  hear_split <- 30
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, hearing = ifelse(
                   .x$BLToneAvg_Better < hear_split, "first", "second"))))
  
  out_g_hf <- get_results(df_list, 
                          outcome_var = "g", 
                          effect_modifier = "hearing")
  
  save_effectmod_plot(
    out_g_hf,
    effect_modifier = "hearing",
    labels = c(
      "Better-ear 4-frequency PTA < 30",
      "Better-ear 4-frequency PTA \u2265 30"
    ),
    ylab = "Mean overall cognition score",
    file_name = "g_by_hearing.png",
    breaks = c(-1.25,-1,-0.75,-0.5,-0.25,0,0.25, 0.5),
    limits = c(-1.25, 0.5)
  )
  
  ### Age ###
  
  age_split <- 75
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, age = ifelse(
                   .x$AgeAtRand < age_split, "first", "second"))))
  
  
  out_g_age <- get_results(df_list, 
                           outcome_var = "g", 
                           effect_modifier = "age")
  
  save_effectmod_plot(
    out_g_age,
    effect_modifier = "age",
    labels = c("Ages [70 - 75)", "Ages [75 - 84]"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_age.png",
    breaks = c(-1.75, -1.5,-1.25, -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5),
    limits = c(-1.75, 0.5)
  )
  
  ### 3MS ###
  
  ms_split <- 95
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, ms = ifelse(
                   .x$BL_MS_OverallScore_C >= ms_split, "first", "second"))))
  
  out_g_ms <- get_results(df_list, 
                          outcome_var = "g", 
                          effect_modifier = "ms")
  
  save_effectmod_plot(
    out_g_ms,
    effect_modifier = "ms",
    labels = c("3MS total score \u2265 95",
               "3MS total score < 95"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_3ms.png",
    breaks = c(-1.75,-1.25,-0.75,-0.25, 0.25, 0.75),
    limits = c(-1.75, 0.8)
  )
  
  ### PCS ###
  
  pcs_split <- 50
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, pcs = ifelse(
                   .x$BL_PCS >= pcs_split, "first", "second"))))
  
  out_g_pcs <- get_results(df_list, 
                           outcome_var = "g", 
                           effect_modifier = "pcs")
  
  save_effectmod_plot(
    out_g_pcs,
    effect_modifier = "pcs",
    labels = c(
      "SF-12 physical component score \u2265 50",
      "SF-12 physical component score < 50"
    ),
    ylab = "Mean overall cognition score",
    file_name = "g_by_pcs.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  ### Frailty
  
  frailty_split <- 0.09
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, frailty = ifelse(
                   .x$BL_Frailty_DAI50 < frailty_split, "first", "second"))))
  
  out_g_frailty <- get_results(df_list, 
                               outcome_var = "g", 
                               effect_modifier = "frailty")
  
  save_effectmod_plot(
    out_g_frailty,
    effect_modifier = "frailty",
    labels = c("Frailty DAI50 < 0.09",
               "Frailty DAI50 \u2265 0.09"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_frailty.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  
  ### Sex
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, gender = ifelse(
                   .x$Gender == 1, "first", "second"))))
  
  out_g_gender <- get_results(df_list, 
                              outcome_var = "g", 
                              effect_modifier = "gender")
  
  save_effectmod_plot(
    out_g_gender,
    effect_modifier = "gender",
    labels = c("Women",
               "Men"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_gender.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  ### APOE e4 positivty
  
  df_list <- map(df_list, 
                 ~ map(.x, ~ mutate(.x, e4 = ifelse(
                   .x$apoe_e4 == 0, "first", "second"))))
  
  out_g_e4 <- get_results(df_list, 
                          outcome_var = "g", 
                          effect_modifier = "e4")
  
  save_effectmod_plot(
    out_g_e4,
    effect_modifier = "e4",
    labels = c("ApoE4 non-carrier",
               "ApoE4 carrier"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_apoe.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  return(list(
    hf = out_g_hf, 
    age = out_g_age,
    ms = out_g_ms, 
    pcs = out_g_pcs,
    frailty = out_g_frailty,
    gender = out_g_gender,
    e4 = out_g_e4
  ))
}

