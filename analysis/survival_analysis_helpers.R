### Functions for g-computation: time to event outcomes

base_covs <- c(
  "BL_PCS",
  "BL_MCS",
  "BL_Frailty_DAI50",
  "BL_Polypharmacy",
  "BL_CesdOverall",
  "AgeAtRand",
  "BL_COWAT",
  "BL_HVLT_TotalRecall",
  "BL_HVLT_DelayedRecall",
  "BL_HVLT_Retention",
  "BL_SDMT",
  "BL_HVLT_RDI",
  "BL_MS_OverallScore_C",
  "BLToneAvg_Better",
  "BL_BMI",
  "CommunityEngagement",
  "LeisureActivities",
  "BL_AlcWk",
  "Pt_scoreIRSAD",
  "HearingProbs_2",
  "Edu",
  "EyesRate",
  "Buzz",
  "QuietRm",
  "CrowdedRm",
  "HrsSleep",
  "PresPhysAct",
  "Alone",
  "Income",
  "Gender",
  "Racial",
  "DAB",
  "Pt_Cr",
  "BL_SmHis2",
  "BL_SmHis3",
  "hypstatus_BLNo_untreated",
  "hypstatus_BLYes_treated",
  "hypstatus_BLYes_untreated",
  "apoe_e4"
)

survival_results <- function(data_list, 
                             outcome_var){
  
  long_list <- map(data_list,
                   to_long_survival,
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
  
  return(list(itt_fit = itt_fit, at_fit = at_fit))
}

### Data to long format ###

to_long_survival <- function(data, outcome_var) {

  # set early death to zero
  data$Death_AV1 <- 0
  data$Death_AV2 <- 0
  data$Death_AV3 <- 0
  
  # set baseline CVD, and cancer to zero 
  data$Cancer_BL <- 0
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
  ## events at time t refer to those occurring in the next interval
  
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
      contains("Cancer_"),
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
  
  suppressWarnings({
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
      contains("_Cancer"),
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
    mutate(time = as.numeric(gsub("AV", "", time)))}
  )
  
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
    fill(all_of(time_varying_covars), .direction = "downup") |>
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

### Remove rows after event/death/censoring occurs

remove_postevent <- function(data, outcome_var) {
  
  setDT(data)
  
  data <- data[order(ID,time)]
  
  data[, sumDeath := cumsum(Death), by = "ID"]
  data <- data[sumDeath < 2 | is.na(sumDeath),]
  
  data[, sumCens := cumsum(Cens), by = "ID"]
  data <- data[sumCens < 2 | is.na(sumCens),]
  
  data[, sumOutcome := cumsum(.SD[[outcome_var]]), by = "ID"]
  data <- data[sumOutcome < 2 | is.na(sumOutcome),]
  
  return(data)
}


## Estimates for survival outcomes 

survival_estimates <-
  function(data, survival_outcome) {
    # parallel
    plan(multisession, workers = 3)
    
    # see survival_analysis_helpers for survival_results()
    results <- future_lapply(data,
                             survival_results,
                             survival_outcome,
                             future.packages = "rms",
                             future.seed = 1234)
    
    plan(sequential)
    
    return(results)
  }

## Confidence intervals for survival outcomes

survival_intervals <- function(results, satterthwaite) {
  
  risks <- get_risk(results)
  
  risks <- add_intervals_survival(risks, satterthwaite = satterthwaite)
  
  plots <- plot_cumulative_inc(risks)
  
  return(list(risks = risks, plots = plots))
}

### Estimate intention-to-treat effect

my_gform_itt <- function(data, outcome_var, comparator = "control"){
  gform_risk <- function(data, outcome_var, interv){
    setDT(data)
    data[, A := Y3M_HearingAid]
    data$Y <- data[[outcome_var]]
    
    # create lags
    
    data[,lag1_PCS:= shift(PCS, fill=0), by = ID]
    data[,lag1_MCS:= shift(MCS, fill=0), by = ID]
    data[,lag1_Frailty:= shift(Frailty, fill=0), by = ID]
    data[,lag1_Polypharmacy:= shift(Polypharmacy, fill=0), by = ID]
    data[,lag1_CesdOverall:= shift(CesdOverall, fill=0), by = ID]
    data[,lag1_CVD:= shift(CVD, fill=0), by = ID]
    if(!outcome_var == "Cancer"){
      data[,lag1_Cancer:= shift(Cancer, fill=0), by = ID]
    }
    
    # fit models 
    
    models <- list()
    
    models$cvd_mod <- 
      glm(as.formula(CVD_form), data = data, family = binomial())
    
    if(!outcome_var == "Cancer"){
      models$cancer_mod <-
        glm(as.formula(cancer_form), data=data, family = binomial())
    }
    
    models$outcome_mod <-
      glm(as.formula(ymodel), data = data, family = binomial())
    
    if(!outcome_var == "Death"){
      if(outcome_var=="Imp"){
        models$death_mod <-
          glm(as.formula(compevent_model_imp), data=data, family = binomial())
      } else {
        models$death_mod <-
          glm(as.formula(compevent_model),
                   data=data, family = binomial())
      }
    }
    
    models$pcs_mod <- speedglm(as.formula(PCS_form), 
                        data = data, fitted = TRUE)
    
    models$mcs_mod <- speedglm(as.formula(MCS_form), 
                        data = data, fitted = TRUE)
    
    models$poly_mod <- speedglm(as.formula(polypharmacy_form), 
                         data = data, fitted = TRUE)
    
    models$frailty_mod <- speedglm(as.formula(frailty_form), 
                            data = data, fitted = TRUE)
    
    models$cesd_mod <- speedglm(as.formula(cesd_form), 
                         data = data, fitted = TRUE)
    

    # get ranges
    
    ranges <- data.table(
      PCS_low = min(data$PCS, na.rm = T),
      PCS_high = max(data$PCS, na.rm = T),
      MCS_low = min(data$MCS, na.rm = T),
      MCS_high = max(data$MCS, na.rm = T),
      Frailty_low = min(data$Frailty, na.rm = T),
      Frailty_high = max(data$Frailty, na.rm = T),
      Polypharmacy_low = min(data$Polypharmacy, na.rm = T),
      Polypharmacy_high = max(data$Polypharmacy, na.rm =T),
      CesdOverall_low = min(data$CesdOverall, na.rm = T),
      CesdOverall_high = max(data$CesdOverall, na.rm = T)
    )
    
    rmse <- data.table(
      PCS = add_rmse(data$PCS, models$pcs_mod),
      MCS = add_rmse(data$MCS, models$mcs_mod),
      Frailty = add_rmse(data$Frailty, models$frailty_mod),
      Polypharmacy = add_rmse(data$Polypharmacy, models$poly_mod),
      CesdOverall = add_rmse(data$CesdOverall, models$cesd_mod)
    )
    
    ## time = 0 
    
    data <- data[time==0,]
    # resample to 10,000 unique IDs
    data <- data[sample(1:nrow(data), 10000, replace = TRUE),]
    data[, newid := seq_len(.N)]
    data <- data[rep(1:nrow(data), each = 7),]
    data[,time := rep(0:6, 10000)]
    
    ## time > 0
    # set intervention value
    if(interv == 0){
      data.table(data[, A := 0])
    } else if(interv == 1){
      data.table(data[, A := 1])
    }
    
    # simulate covariates
    
    data <- simulate(data, 1, models, ranges, rmse, outcome_var = outcome_var)
    
    for(k in 2:6){
      data <- add_lags(data, k, outcome_var = outcome_var)
      data <- simulate(data, k, models, ranges, rmse, outcome_var = outcome_var)
    }
    
    # Outcome risk 
    
    if(!outcome_var == "Death"){
      data[, `:=`(
        h_Y = predict(models$outcome_mod, type = "response", newdata = .SD),
        h_D = predict(models$death_mod, type = "response", newdata = .SD))]
      data[, risk := 
             cumsum(h_Y * cumprod((1 - shift(h_Y, fill = 0)) * (1 - h_D))), 
           by = newid]
    } else {
      data[, h_Y := 
             predict(models$outcome_mod, type = "response", newdata = .SD)]
      data[, risk := 1 - cumprod(1 - shift(h_Y, fill = 0)), by = newid]
    }
    
    data <- data[, .(Y = mean(risk)), by = time]
    data.table(data[, interv := interv])
  }
  
  if(comparator == "control"){
    interventions <- c(0,1)
  } else {
    interventions <- c(1,2)
  }
  
  out <- map(
    interventions, 
    gform_risk, 
    data = data, 
    outcome_var = outcome_var
    )
  
  out <- 
    bind_rows(out) |> 
    pivot_wider(names_from = interv, values_from = Y, names_prefix = "Y")
  
  if(comparator == "control"){
    out$RR <- out$Y1 / out$Y0
    out$RD <- out$Y1 - out$Y0
  } else {
    out$RR <- out$Y1 / out$Y2
    out$RD <- out$Y1 - out$Y2
  }
  
  out <- out |> 
    pivot_longer(starts_with("Y"), 
                 names_to = "interv", values_to = "Y", names_prefix = "Y")
  
  setnames(out,
           c("time", "RR", "RD", "interv", "Y"),
           c("k","Risk ratio", "Risk difference", "Interv.", "g-form risk"))
  
  return(out)
}

### Estimate as treated effect

my_gform_at <- function(data, outcome_var){
  gform_risk <- function(data, outcome_var, interv){
    setDT(data)
    data[, A2 := ifelse(Y3M_HearingAidUse %in% c(2,3), 1, 0)]
    data[, A3 := ifelse(Y3M_HearingAidUse %in% c(4,5), 1, 0)]
    data$Y <- data[[outcome_var]]
    
    # create lags
    data[,lag1_PCS:= shift(PCS, fill=0), by = ID]
    data[,lag1_MCS:= shift(MCS, fill=0), by = ID]
    data[,lag1_Frailty:= shift(Frailty, fill=0), by = ID]
    data[,lag1_Polypharmacy:= shift(Polypharmacy, fill=0), by = ID]
    data[,lag1_CesdOverall:= shift(CesdOverall, fill=0), by = ID]
    data[,lag1_CVD:= shift(CVD, fill=0), by = ID]
    if(!outcome_var == "Cancer"){
      data[,lag1_Cancer:= shift(Cancer, fill=0), by = ID]
    }
    
    # fit models 
    models <- list()
    
    models$cvd_mod <-
      glm(as.formula(CVD_form_at), data = data, family = binomial())
    
    if(!outcome_var == "Cancer"){
      models$cancer_mod <-
        glm(as.formula(cancer_form_at), data=data, family = binomial())
    }
    
    models$outcome_mod <-
      glm(as.formula(ymodel_at), data = data, family = binomial())
    
    if(!outcome_var == "Death"){
      if(outcome_var=="Imp"){
        models$death_mod <-
          glm(as.formula(compevent_model_at_imp), 
              data=data, 
              family = binomial())
      } else {
        models$death_mod <-
          glm(as.formula(compevent_model_at), 
              data=data, 
              family = binomial())
      }
    }
    
    models$pcs_mod <- speedglm(as.formula(PCS_form_at), 
                        data = data, fitted = TRUE)
    
    models$mcs_mod <- speedglm(as.formula(MCS_form_at), 
                        data = data, fitted = TRUE)
    
    models$poly_mod <- speedglm(as.formula(polypharmacy_form_at), 
                         data = data, fitted = TRUE)
    
    models$frailty_mod <- speedglm(as.formula(frailty_form_at), 
                            data = data, fitted = TRUE)
    
    models$cesd_mod <- speedglm(as.formula(cesd_form_at), 
                         data = data, fitted = TRUE)
    
    # get ranges
    
    ranges <- data.table(
      PCS_low = min(data$PCS, na.rm = T),
      PCS_high = max(data$PCS, na.rm = T),
      MCS_low = min(data$MCS, na.rm = T),
      MCS_high = max(data$MCS, na.rm = T),
      Frailty_low = min(data$Frailty, na.rm = T),
      Frailty_high = max(data$Frailty, na.rm = T),
      Polypharmacy_low = min(data$Polypharmacy, na.rm = T),
      Polypharmacy_high = max(data$Polypharmacy, na.rm =T),
      CesdOverall_low = min(data$CesdOverall, na.rm = T),
      CesdOverall_high = max(data$CesdOverall, na.rm = T)
    )
    
    rmse <- data.table(
      PCS = add_rmse(data$PCS, models$pcs_mod),
      MCS = add_rmse(data$MCS, models$mcs_mod),
      Frailty = add_rmse(data$Frailty, models$frailty_mod),
      Polypharmacy = add_rmse(data$Polypharmacy, models$poly_mod),
      CesdOverall = add_rmse(data$CesdOverall, models$cesd_mod)
    )
    
    ## time = 0 
    
    data <- data[time==0,]
    # resample to 10,000 unique IDs
    data <- data[sample(1:nrow(data), 10000, replace = TRUE),]
    data[, newid := seq_len(.N)]
    data <- data[rep(1:nrow(data), each = 7),]
    data[,time := rep(0:6, 10000)]
    
    ## time > 0
    # set intervention value
    if(interv == 0){
      data[, `:=`(A2 = 0, A3 = 0)]
    } else if(interv == 1){
      data[, `:=`(A2 = 1, A3 = 0)]
    } else{
      data[, `:=`(A2 = 0, A3 = 1)]
    }
    
    # simulate covariates
    
    data <- simulate(data, 1, models, ranges, rmse, outcome_var = outcome_var)
    
    for(k in 2:6){
      data <- add_lags(data, k, outcome_var = outcome_var)
      data <- simulate(data, k, models, ranges, rmse, outcome_var = outcome_var)
    }
    
    # Outcome risk 
    
    if(!outcome_var == "Death"){
      data[, `:=`(
        h_Y = predict(models$outcome_mod, type = "response", newdata = .SD),
        h_D = predict(models$death_mod, type = "response", newdata = .SD))]
      
      data[, risk := 
             cumsum(h_Y * cumprod((1 - shift(h_Y, fill = 0)) * (1 - h_D))), 
           by = newid]
    } else {
      data[, h_Y := 
             predict(models$outcome_mod, type = "response", newdata = .SD)]
      data[, risk := 1 - cumprod(1 - shift(h_Y, fill = 0)), by = newid]
    }
    
    data <- data[, .(Y = mean(risk)), by = time]
    data.table(data[, interv := interv])
  }
  
  out <- map(c(0,1,2), gform_risk, 
             data = data, outcome_var = outcome_var) |> 
    bind_rows() |> 
    pivot_wider(names_from = interv, values_from = Y, names_prefix = "Y") |>
    mutate(RR = Y2 / Y0, RD = Y2 - Y0) |> 
    pivot_longer(c(Y0, Y1, Y2), 
                 names_to = "interv", values_to = "Y", names_prefix = "Y")
  
  setnames(out,
           c("time", "RR", "RD", "interv", "Y"),
           c("k","Risk ratio", "Risk difference", "Interv.", "g-form risk"))
  
  return(out)
}

## Simulating covariates over follow-up

simulate <- 
  function(data, time, models, ranges, rmse, outcome_var){
  if(time == 1){
    
    # CVD 
    data[time == 1,
         CVD := rbinom(nrow(.SD[time == 1, ]),
                       1,
                       predict(
                         models$cvd_mod,
                         newdata = .SD[time == 1, ],
                         type = "response"
                       ))]
    
    if(!outcome_var == "Cancer"){
      # Cancer
      data[time == 1,
           Cancer := rbinom(nrow(.SD[time == 1, ]),
                            1,
                            predict(models$cancer_mod,
                                    newdata = .SD[time == 1, ],
                                    type = "response"))]
    }
    
    # Polypharmacy
    
    data[time == 1, Polypharmacy := rnorm(
      nrow(.SD[time == 1,]), 
      predict(models$poly_mod, newdata = .SD[time == 1,]),
      rmse$Polypharmacy)]
    
    data[, Polypharmacy := 
           ifelse(Polypharmacy < ranges$Polypharmacy_low, 
                  ranges$Polypharmacy_low, Polypharmacy)]
    data[, Polypharmacy := 
           ifelse(Polypharmacy > ranges$Polypharmacy_high, 
                  ranges$Polypharmacy_high, Polypharmacy)]
    
    
    # Frailty
    
    data[time == 1, Frailty := rnorm(
      nrow(.SD[time == 1,]),
      predict(models$frailty_mod, newdata = .SD[time == 1,]), rmse$Frailty)]
    
    data[, Frailty := 
           ifelse(Frailty < ranges$Frailty_low, ranges$Frailty_low, Frailty)]
    data[, Frailty := 
           ifelse(Frailty > ranges$Frailty_high, ranges$Frailty_high, Frailty)]
    
    # PCS
    
    data[time == 1, PCS := rnorm(
      nrow(.SD[time == 1,]), 
      predict(models$pcs_mod, newdata = .SD[time == 1,]), rmse$PCS)]
    
    data[, PCS := ifelse(PCS < ranges$PCS_low, ranges$PCS_low, PCS)]
    data[, PCS := ifelse(PCS > ranges$PCS_high, ranges$PCS_high, PCS)]
    
    # MCS
    
    data[time == 1, MCS := rnorm(
      nrow(.SD[time == 1,]), 
      predict(models$mcs_mod, newdata = .SD[time == 1,]), rmse$MCS)]
    
    data[, MCS := ifelse(MCS < ranges$MCS_low, ranges$MCS_low, MCS)]
    data[, MCS := ifelse(MCS > ranges$MCS_high, ranges$MCS_high, MCS)]

    
    # Cesd
    
    data[time == 1, CesdOverall := rnorm(
      nrow(.SD[time == 1,]), 
      predict(models$cesd_mod, newdata = .SD[time == 1,]), rmse$CesdOverall)]
    
    data[, CesdOverall := 
           ifelse(CesdOverall < ranges$CesdOverall_low, 
                  ranges$CesdOverall_low, CesdOverall)]
    data[, CesdOverall := 
           ifelse(CesdOverall > ranges$CesdOverall_high, 
                  ranges$CesdOverall_high, CesdOverall)]
    
    data.table(data)
    
  } else {
    # CVD
    
    data[time == time,
         CVD := rbinom(
           nrow(.SD[time==time,]),
           1,
           predict(models$cvd_mod,
                   newdata = .SD[time == time, ],
                   type = "response")
         )]
    
    data[time == time, CVD := ifelse(lag1_CVD == 1, 1, CVD)]
    
    # Cancer
    
    if(!outcome_var == "Cancer"){
      data[time == time,
           Cancer := rbinom(
             nrow(.SD[time==time,]),
             1,
             predict(models$cancer_mod,
                     newdata = .SD[time == time, ],
                     type = "response")
           )]
      data[time == time, Cancer := ifelse(lag1_Cancer == 1, 1, Cancer)]
    }

    # Polypharmacy
    
    data[time==time, Polypharmacy := rnorm(
      nrow(.SD[time==time,]), 
      predict(models$poly_mod, newdata = .SD[time==time,]),
      rmse$Polypharmacy)]
    
    data[, Polypharmacy := 
           ifelse(Polypharmacy < ranges$Polypharmacy_low, 
                  ranges$Polypharmacy_low, Polypharmacy)]
    data[, Polypharmacy := 
           ifelse(Polypharmacy > ranges$Polypharmacy_high, 
                  ranges$Polypharmacy_high, Polypharmacy)]
    
    # Frailty
    
    data[time==time, Frailty := rnorm(
      nrow(.SD[time==time,]),
      predict(models$frailty_mod, newdata = .SD[time==time,]), rmse$Frailty)]
    
    data[, Frailty := 
           ifelse(Frailty < ranges$Frailty_low, ranges$Frailty_low, Frailty)]
    data[, Frailty := 
           ifelse(Frailty > ranges$Frailty_high, ranges$Frailty_high, Frailty)]
    
    # PCS
    
    data[time==time, PCS := rnorm(
      nrow(.SD[time==time,]), 
      predict(models$pcs_mod, newdata = .SD[time==time,]), rmse$PCS)]
    
    data[, PCS := ifelse(PCS < ranges$PCS_low, ranges$PCS_low, PCS)]
    data[, PCS := ifelse(PCS > ranges$PCS_high, ranges$PCS_high, PCS)]
    
    # MCS
    
    data[time==time, MCS := rnorm(
      nrow(.SD[time==time,]), 
      predict(models$mcs_mod, newdata = .SD[time==time,]), rmse$MCS)]
    
    data[, MCS := ifelse(MCS < ranges$MCS_low, ranges$MCS_low, MCS)]
    data[, MCS := ifelse(MCS > ranges$MCS_high, ranges$MCS_high, MCS)]
    
    # Cesd
    
    data[time==time, CesdOverall := rnorm(
      nrow(.SD[time==time,]), 
      predict(models$cesd_mod, newdata = .SD[time==time,]), rmse$CesdOverall)]
    
    data[, CesdOverall := 
           ifelse(CesdOverall < ranges$CesdOverall_low, 
                  ranges$CesdOverall_low, CesdOverall)]
    data[, CesdOverall := 
           ifelse(CesdOverall > ranges$CesdOverall_high, 
                  ranges$CesdOverall_high, CesdOverall)]
    
    data.table(data)
  }
}

add_lags <- function(data, time, outcome_var){
  data[time <= time, lag1_PCS:=shift(PCS, fill=0),by=newid]
  data[time <= time, lag1_MCS:=shift(MCS, fill=0),by=newid]
  data[time <= time, lag1_Frailty:=shift(Frailty, fill=0),by=newid]
  data[time <= time, lag1_Polypharmacy:=shift(Polypharmacy, fill=0),by=newid]
  data[time <= time, lag1_CesdOverall:=shift(CesdOverall, fill=0),by=newid]
  data[time <= time, lag1_CVD:=shift(CVD, fill=0),by=newid]
  if(!outcome_var == "Cancer"){
    data[time <= time, lag1_Cancer:=shift(Cancer, fill=0),by=newid]
  }
  data.table(data)
}

add_rmse <- function(Y, model) {
  sqrt(mean((Y - fitted(model)) ^ 2))
}

## Extract risk

get_risk <- function(result_list, as_treated = TRUE){

    itt <- bind_rows(result_list, .id = "B") |> 
    dplyr::select(B, itt_fit) |> 
    mutate(M = names(itt_fit)) |> 
    unnest(itt_fit)
  
  itt <- dplyr::select(itt, B, M, k, `Interv.`, `g-form risk`, 
                `Risk ratio`, `Risk difference`) |> 
    set_names(c("B","M","time","A","Y","RR","RD"))
  
  if(isTRUE(as_treated)){
    at <- bind_rows(result_list, .id = "B") |> 
      dplyr::select(B, at_fit) |> 
      mutate(M = names(at_fit)) |> 
      unnest(at_fit)
    
    at <- dplyr::select(at, B, M, k, `Interv.`, `g-form risk`, 
                 `Risk ratio`, `Risk difference`) |> 
      set_names(c("B","M","time","A","Y","RR","RD"))
    
    return(list(itt = itt, at = at))
  } else {
    return(list(itt = itt))
  }
}

## Bootstrap intervals ##

add_intervals_survival <- 
  function(result, satterthwaite = satterthwaite, as_treated = TRUE){

  # ITT
  itt <- result$itt
  
  itt$RR <- ifelse(is.na(itt$RR), 1, itt$RR)
  
  params <- 
    expand_grid(unique(itt$A), unique(itt$time), c("Y","RR","RD")) |> 
    set_names(c("A","time", "estimand"))
  
  params <- list(A = params$A, time = params$time, estimand = params$estimand)
  
  itt_out <- pmap(params,
                  get_variance_survival, 
                  data = itt, 
                  satterthwaite = satterthwaite)
  
  itt_out <- bind_rows(itt_out) |> 
    pivot_wider(id_cols = c(A, time), 
                values_from = c(Y,se,lower,upper), 
                names_from = estimand) |> 
    rename(Y = "Y_Y", RD = "Y_RD", RR = "Y_RR")
  
  # As treated
  
  if(isTRUE(as_treated)){
    at <- result$at 
    
    at$RR <- ifelse(is.na(at$RR), 1, at$RR)
    
    params <- 
      expand_grid(unique(at$A), unique(at$time), c("Y", "RR", "RD")) |> 
      set_names(c("A","time", "estimand"))
    
    params <- list(A = params$A, time = params$time, estimand = params$estimand)
    
    at_out <- pmap(params,
                   get_variance_survival, 
                   data = at, 
                   satterthwaite = satterthwaite)
    
    at_out <- bind_rows(at_out) |> 
      pivot_wider(id_cols = c(A, time), 
                  values_from = c(Y,se,lower,upper), 
                  names_from = estimand) |> 
      rename(Y = "Y_Y", RD = "Y_RD", RR = "Y_RR")
    
    return(list(itt = itt_out, at = at_out))
  } else {
    return(list(itt = itt_out))
  }
}

get_variance_survival <- function(data,
                                  time,
                                  A,
                                  estimand,
                                  satterthwaite) {
    
    data$Y <- data[[estimand]]
    
    B <- as.numeric(max(data$B))
    # by time and treatment
    data <- data |> filter(time == {{ time }}, A == {{ A }})
    # estimate between and within mean sum of squares
    model <- aov(Y ~ B, data = data)
    MSB <- summary(model)[[1]]$`Mean Sq`[1]
    MSW <- summary(model)[[1]]$`Mean Sq`[2]
    sigma1 <- (MSB - MSW) / 2
    sigma1 <- ifelse(sigma1 < 0, 0, sigma1)
    sigma2 <- ifelse(sigma1 < 0, var(data$Y), MSW)
    Var <- ((1 + (1 / B)) * sigma1) + ((1 / (B * 2)) * sigma2)
    part1 <- ((((B + 1) / (B * 2)) ^ 2) * (MSB ^ 2)) / (B - 1)
    part2 <- MSW ^ 2 / (4 * B)
    df <- Var ^ 2 / (part1 + part2)
    suppressMessages(data <- data |> group_by(time, A) |>
                       summarise(Y = mean(Y, na.rm=T)) |> 
                       ungroup())
    data$se <- sqrt(Var)
    data$lower <-
      ifelse(satterthwaite,
             data$Y - qt(0.975, df) * data$se,
             data$Y - 1.96 * data$se)
    data$upper <-
      ifelse(satterthwaite,
             data$Y + qt(0.975, df) * data$se,
             data$Y + 1.96 * data$se)
    
    data$estimand <- estimand
    
    data <- data |> dplyr::select(A, time, Y, se, lower, upper, estimand)
    
    if (satterthwaite) {
      data$df <- df
    }
    
    return(data)
}


## Add cognitive impairment to dataset

add_imp <- function(data, cind){
  
  data <- map(data, ~ left_join(.x, cind, by = "Safehaven"))
  
  data <- map(data, add_cind)
  
  data <- map(data, add_impairment)
  
  return(data)
}

### Create cognitive decline variables

add_cind <- function(data) {
  
  cind_function <- function(data, visit) {
    ifelse(
      data$CIND_Any == 1 & data$CIND_DSR < (365 * visit), 1,
      ifelse(data$CIND_Any == 0 &
               data$CIND_DSR < (365 * visit), NA, 0)
    )
  }
  
  for(i in 1:10){
    data[[paste0("CIND_AV",i)]] <- cind_function(data, i)
  }
  
  return(data)
}

### Create cognitive impairment composite 

add_impairment <- function(data){
  
  for(i in 1:10){
    data[[paste0("Imp_AV", i)]] <- 
      if_else(data[[paste0("CIND_AV", i)]] == 1 | 
                data[[paste0("Dem_AV", i)]] == 1, 
              1, data[[paste0("Dem_AV", i)]])
  }
  return(data)
}

## Replace XT06 dementia events with XT04

revert_dementia <- function(data, events_xt04) {
  
  events_xt04 <- events_xt04 |>
    dplyr::select(Safehaven, contains("Dem_")) |>
    dplyr::select(-Dem_AV11, -Dem_AV12)
  
  # remove XT06 events and replace with XT04
  data <- map(data, 
              function(.x) {
                dplyr::select(.x,-contains("Dem")) |>
                  left_join(events_xt04, by = "Safehaven")
              }
  )
  return(data)
}

## Revert cancer to xt04 (complete outcome adjudication)
revert_cancer <- function(df_list, events_xt04) {
  
  events_xt04 <- events_xt04 |>
    dplyr::select(Safehaven, contains("Cancer_")) |>
    dplyr::select(-Cancer_AV11, -Cancer_AV12)
  
  # remove XT06 events and replace with XT04
  df_list <- map(
    df_list, 
    function(.x) {
      dplyr::select(.x,-contains("Cancer")) |>
        left_join(events_xt04, by = "Safehaven")
    }
  )
  
  return(df_list)
}
