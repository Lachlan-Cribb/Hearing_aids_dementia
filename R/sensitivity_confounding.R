## Sensitivity for confounding by indication/healthcare utilisation

sensitivity_confounding <- function(df_list){
  
  # to long
  long_list <- map(df_list, to_long, outcome_var = "g", final_visit = 10, effect_modifier = NULL)
  
  # add indicator for prescribed & non-adherent vs non-prescribed
  
  long_list <- map(
    long_list, 
    ~ mutate(.x, Y3M_HearingAidUse = round(as.numeric(Y3M_HearingAidUse))))
  
  long_list <- map(long_list, ~ mutate(
    .x,
    A = case_when(
      Y3M_HearingAid == 1 & Y3M_HearingAidUse == 1 ~ 1,
      Y3M_HearingAid == 0 &
        Y3M_HearingAidUse == 1 ~ 0,
      TRUE ~ NA
    )
  ))
  
  long_list <- map(long_list, ~ filter(.x, !is.na(A)))
  
  # estimate effect
  
  get_itt <- function(df){
    out <- intention_to_treat(df, Y = "g", A = "A")
    out <- get_estimates_itt(out, df, A = "A", comparator = "control", effect_modifier = NULL)
    out
  }
  
  itt <- map(long_list, get_itt) |> bind_rows(.id = "m")
  itt$m <- as.numeric(as.factor(itt$m))
  itt
}

summarise_confounding <- function(result){
  out <- bind_rows(result)
  out |> 
    group_by(estimand, time, A) |>
    summarise(mean = mean(Y))
}

