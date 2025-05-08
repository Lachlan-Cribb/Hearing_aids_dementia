## Sensitivity analysis: effect of hearing aids on mortality

options(future.globals.maxSize = Inf)

## function for death 

get_results_death <-
  function(data, outcome_var, satterthwaite) {
    
    plan(multisession, workers = 3)
    
    result <- future_lapply(
      data, survival_results_death, outcome_var, future.seed = 1234)
    
    out <- get_risk(result)
    
    out <- add_intervals_survival(out, satterthwaite = satterthwaite)
    
    # plot
    
    plots <- plot_cumulative_inc(out)
    
    save_cuminc_plot(plots,
                     file_name = "death.png",
                     y_label = "Risk of death",
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                     limits = c(0,0.25),
                     rr_breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2),
                     rr_limits = c(0,2)
    )
    
    return(list(out = out, plot = plots))
  }


survival_results_death <- function(data_list, outcome_var) {
  
  
  long_list <- map(data_list, to_long_survival_death)
  
  # Remove all observations after first occurrence of death or outcome
  
  long_list <- map(long_list, remove_postevent_death)
  
  # intention to treat
  
  itt_fit <- map(long_list, my_gform_itt, outcome_var = outcome_var)
  
  # as treated
  
  at_fit <- map(long_list, my_gform_at, outcome_var = outcome_var)

  return(list(itt_fit = itt_fit, at_fit = at_fit))
}

to_long_survival_death <- function(data) {
  # remove AV11 & AV12 variables (few/no events)
  data <- data |> dplyr::select(-contains("AV12"),-contains("AV11"))
  
  # set baseline CVD, and cancer to zero
  data$Cancer_BL <- 0
  data$CVD_BL <- 0
  
  # create indicators for censoring
  
  for (i in 4:10) {
    data[[paste0("Cens_AV", i)]] <-
      if_else(is.na(data[[paste0("Death_AV", i)]]), 1, 0)
  }
  
  ## for outcome, death, and censoring, label with previous time
  ## to match gfoRmula package input structure. This way, e.g.,
  ## events at time 0 refer to those occurring in the next interval
  
  old_names <- data |>
    dplyr::select(contains("Death_"),
                  contains("Cens")) |>
    dplyr::select(contains("AV")) |>
    names()
  
  back_one <- function(string) {
    num <- as.numeric(gsub(".*AV(\\d+)", "\\1", string))
    new_string <- gsub("(AV)\\d+", paste0("\\1", num - 1), string)
    return(new_string)
  }
  
  new_names <- map_vec(old_names, back_one)
  
  setnames(data, old_names, new_names)
  
  ## rename event variables (swap event name and AV*, for pivoting)
  
  to_rename <- data |>
    dplyr::select(
      contains("Cancer_"),
      contains("CVD_"),
      contains("Death_"),
      contains("Cens_")
    ) |>
    dplyr::select(-contains("DSR"),-contains("protocol")) |>
    names()
  
  names(data)[names(data) %in% to_rename] <-
    gsub("(.*)_(.*)", "\\2_\\1", names(data)[names(data) %in% to_rename])
  
  ## To long format
  
  long <- data |>
    dplyr::select(
      Safehaven,
      ID,
      Y3M_HearingAid,
      Y3M_HearingAidUse,
      all_of(base_covs),
      contains("_PCS"),
      contains("_MCS"),
      contains("Frailty"),
      contains("Polypharmacy"),
      contains("Cesd"),
      contains("_Cancer"),
      contains("_CVD"),
      contains("_Death"),
      contains("_Cens"),-any_of("CIND_Any_censor")
    ) |>
    dplyr::select(-contains("DSR"),-contains("Protocol")) |>
    pivot_longer(
      -c(
        Safehaven,
        ID,
        all_of(base_covs),
        Y3M_HearingAid,
        Y3M_HearingAidUse
      ),
      names_to = c("time", ".value"),
      names_sep = "_"
    ) |>
    mutate(time = ifelse(time == "BL", "0", time)) |>
    mutate(time = as.numeric(gsub("AV", "", time)))
  
  ## Last observation carried forward for missing covariates
  
  time_varying_covars <-
    c(
      "PCS",
      "MCS",
      "Frailty",
      "Polypharmacy",
      "CesdOverall",
      "Cancer",
      "CVD"
    )
  
  long <- long |>
    arrange(ID, time) |>
    group_by(ID) |>
    fill(time_varying_covars, .direction = "down") |>
    ungroup()
  
  # round depression score to integer
  
  long$CesdOverall <- round(long$CesdOverall)
  
  # remove time = 10 (representing events in 11th year of FU) and
  # and first three years, when death could not occur
  
  long <- filter(long, time > 2 & time < 10)
  
  long$time <- long$time - 3
  
  return(long)
}

remove_postevent_death <- function(data) {
  setDT(data)
  
  data <- data[order(ID, time)]
  
  data[, sumDeath := cumsum(Death), by = "ID"]
  data <- data[sumDeath < 2 | is.na(sumDeath), ]
  
  data[, sumCens := cumsum(Cens), by = "ID"]
  data <- data[sumCens < 2 | is.na(sumCens), ]
  
  return(data)
}
