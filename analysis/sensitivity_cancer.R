## Cancer negative outcome control

cancer_outcome_control <- function(df_list){
  source("analysis/cancer_formulas.R")
  options(future.globals.maxSize = Inf)

  ## analysis
  
  out <- get_results_cancer(df_list, "Cancer")
  
  save_cuminc_plot(plot_cumulative_inc(out$out),
                   file_name = "cancer.png",
                   y_label = "Cancer risk",
                   breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                   limits = c(0,0.25),
                   rr_breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3),
                   rr_limits = c(0.7,1.3)
  )
  
  return(out)
}

## Functions for cancer negative outcome control sensitivity analysis
get_results_cancer <-
  function(data,
           outcome_var,
           comparator = "control",
           satterthwaite = TRUE) {
    
    plan(multisession, workers = 3)
    
    result <- future_lapply(
      data,
      FUN = function(.x) {
        survival_results_cancer(.x, outcome_var = outcome_var)
      },
      future.seed = 1234,
      future.packages = "rms"
    )
    
    out <- get_risk(result)
    
    out <- add_intervals_survival(out, satterthwaite = satterthwaite)
    
    # plot
    
    plots <-
      plot_cumulative_inc(out, comparator = comparator)
    
    return(list(out = out, plot = plots))
    
  }


survival_results_cancer <- function(data_list, 
                                    outcome_var){
  
  long_list <- map(data_list, 
                   to_long_survival_cancer, 
                   outcome_var = outcome_var)
  
  # Remove all observations after first occurrence of death or outcome
  
  long_list <- map(long_list, 
                   remove_postevent, 
                   outcome_var = outcome_var)
  
  # intention to treat
  
  itt_fit <- map(long_list,
                 my_gform_itt,
                 outcome_var = outcome_var)
  
  # as treated
  
  at_fit <- map(long_list,
                my_gform_at,
                outcome_var = outcome_var)
  
  
  return(list(itt_fit = itt_fit,
              at_fit = at_fit))
}

### to long format 

to_long_survival_cancer <- function(data, outcome_var) {
  # remove AV11 & AV12 variables (few/no events)
  data <- data |> dplyr::select(-contains("AV12"), -contains("AV11"))
  
  # set early death to zero
  data$Death_AV1 <- 0
  data$Death_AV2 <- 0
  data$Death_AV3 <- 0
  
  # set baseline CVD to zero 
  data$CVD_BL <- 0
  
  # create indicators for censoring
  
  for (i in 1:10) {
    data[[paste0("Cens_AV", i)]] <-
      if_else(is.na(data[[paste0(outcome_var, "_AV", i)]]) &
                (is.na(data[[paste0("Death_AV", i)]]) |
                   data[[paste0("Death_AV", i)]] == 0), 1, 0)
    if (i > 1) {
      data[[paste0("Cens_AV", i)]] <-
        if_else(data[[paste0("Cens_AV", i-1)]] == 1, 1,
                data[[paste0("Cens_AV", i)]])
    }
  }
  
  # recode death to NA if event has already occurred
  for (i in 1:10) {
    data[[paste0("Death_AV", i)]] <-
      if_else(data[[paste0(outcome_var, "_AV", i)]] == 1 &
                !is.na(data[[paste0(outcome_var, "_AV", i)]]), NA, 
              data[[paste0("Death_AV", i)]])
  }
  
  # remove participants with event prior to time zero
  data <- data[data[[paste0(outcome_var, "_AV3")]] == 0, ]
  
  # remove participants censored before time zero
  data <- filter(data, Cens_AV3 == 0)
  
  ## for outcome, death, and censoring, label with previous time
  ## to match gfoRmula package input structure. This way, e.g.,
  ## events at time 0 refer to those occurring in the next interval
  
  old_names <- data |> 
    dplyr::select(contains(paste0(outcome_var,"_")), 
                  contains("Death_"), 
                  contains("Cens")) |> 
    dplyr::select(contains("AV")) |> 
    names()
  
  back_one <- function(string){
    num <- as.numeric(gsub(".*AV(\\d+)", "\\1", string))
    new_string <- gsub("(AV)\\d+", paste0("\\1",num-1), string)
    return(new_string)
  }
  
  new_names <- map_vec(old_names, back_one)
  
  # Rename "AV0" to "BL" to be consistent with other time-varying covars
  
  new_names <- gsub("AV0", "BL", new_names)
  
  setnames(data, old_names, new_names)
  
  ## rename event variables (swap event name and AV*, for pivoting)
  
  vars <- paste(outcome_var, paste("AV", 1:9, sep = ""), sep = "_")
  vars <- c(vars, paste(outcome_var, "BL", sep = "_"))
  
  to_rename <- data |>
    dplyr::select(
      all_of(vars),
      contains("CVD_"),
      contains("Death_"),
      contains("Cens_")
    ) |>
    dplyr::select(-contains("DSR"), -contains("protocol")) |>
    names()
  
  names(data)[names(data) %in% to_rename] <-
    gsub("(.*)_(.*)", "\\2_\\1", names(data)[names(data) %in% to_rename])
  
  vars <- gsub("(.*)_(.*)", "\\2_\\1", vars)
  
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
      all_of(vars),
      contains("_CVD"),
      contains("_Death"),
      contains("_Cens"),
      -any_of("CIND_Any_censor")
    ) |>
    dplyr::select(-contains("DSR"), -contains("Protocol")) |>
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
  
  ## Last observation carried forward for occasional missing covariate
  
  time_varying_covars <- 
    c(
      "PCS",
      "MCS",
      "Frailty",
      "Polypharmacy",
      "CesdOverall",
      "CVD"    
    )
  
  long <- long |>
    arrange(ID, time) |>
    group_by(ID) |>
    fill(all_of(time_varying_covars), .direction = "down") |>
    ungroup()
  
  # round depression score to integer
  
  long$CesdOverall <- round(long$CesdOverall)
  
  # remove time = 10 (representing events in 11th year of FU)
  
  long <- filter(long, time < 10)
  
  # start follow-up at ASPREE year 3 
  
  long <- filter(long, time > 2)
  long$time <- long$time - 3
  
  ## If censoring present, set death to NA
  
  long <- long |> mutate(Death = if_else(Cens == 1, NA, Death))
  
  return(long)
}
