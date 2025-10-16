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

survival_estimates <- function(data_list, outcome_var){
  
  # long format and remove all observations after first occurrence of death or outcome
  if(outcome_var == "Death"){
    long_list <- map(data_list, to_long_survival_death)
    long_list <- map(long_list, remove_postevent_death)
  } else if(outcome_var == "Cancer"){
    long_list <- map(data_list, to_long_survival_cancer, outcome_var = outcome_var)
    long_list <- map(long_list, remove_postevent, outcome_var = outcome_var)
  } else {
    long_list <- map(data_list, to_long_survival, outcome_var = outcome_var)
    long_list <- map(long_list, remove_postevent, outcome_var = outcome_var)
  }
  
  # intention to treat and as-treated effects
  
  estimates <- map(long_list, function(.x) {
    itt <- my_gform_itt(.x, outcome_var = outcome_var)
    at <- my_gform_at(.x, outcome_var = outcome_var)
    bind_rows(itt, at)
  })
  
  # combine imputations 
  bind_rows(estimates, .id = "m")
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
    select(contains(paste0(outcome_var,"_")), 
           contains("Death_"), 
           contains("Cens")) |> 
    select(contains("AV")) |> 
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
    select(
      all_of(vars),
      contains("Cancer_"),
      contains("CVD_"),
      contains("Death_"),
      contains("Cens_")
    ) |>
    select(-contains("DSR"), -contains("protocol")) |>
    names()
  
  names(data)[names(data) %in% to_rename] <-
    gsub("(.*)_(.*)", "\\2_\\1", names(data)[names(data) %in% to_rename])
  
  vars <- gsub("(.*)_(.*)", "\\2_\\1", vars)
  
  ## To long format
  
  suppressWarnings({
    long <- data |>
      select(
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
      select(-contains("DSR"), -contains("Protocol")) |>
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


### Estimate intention-to-treat effect

my_gform_itt <- function(data, outcome_var, comparator = "control"){
  
  if(outcome_var == "Cancer"){
    source("R/cancer_formulas.R")
  }
  
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
  
  # fit outcome and covariate models 
  
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
  
  models$pcs_mod <- lm(as.formula(PCS_form), data = data)
  
  models$mcs_mod <- lm(as.formula(MCS_form), data = data)
  
  models$poly_mod <- lm(as.formula(polypharmacy_form), data = data)
  
  models$frailty_mod <- lm(as.formula(frailty_form), data = data)
  
  models$cesd_mod <- lm(as.formula(cesd_form), data = data)
  
  # get max and min for each covariate
  
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
  
  # RMSE (for simulating covariates)
  
  rmse <- data.table(
    PCS = add_rmse(data$PCS, models$pcs_mod),
    MCS = add_rmse(data$MCS, models$mcs_mod),
    Frailty = add_rmse(data$Frailty, models$frailty_mod),
    Polypharmacy = add_rmse(data$Polypharmacy, models$poly_mod),
    CesdOverall = add_rmse(data$CesdOverall, models$cesd_mod)
  )
  
  ## take baseline data and then simulate full follow-up for all participants
  
  data <- data[time==0,]
  # resample to 10,000 unique IDs
  data <- data[sample(1:nrow(data), 10000, replace = TRUE),]
  # new unique ID variable
  data[, newid := seq_len(.N)]
  data <- data[rep(1:nrow(data), each = 7),]
  data[,time := rep(0:6, 10000)]
  
  
  ## function for g-computation
  
  gform_risk <- function(data, outcome_var, interv){
    
    # set intervention value (else leave treatment at observed value)
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
  
  ## for each intervention, estimate risk
  
  if(comparator == "control"){
    interventions <- c(0,1)
  } else {
    interventions <- c(1,2)
  }
  
  out <- map(interventions, gform_risk, data = data, outcome_var = outcome_var)
  
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
  
  out |> 
    pivot_longer(
      starts_with("Y"), 
      names_to = "interv", 
      values_to = "Y", 
      names_prefix = "Y") |> 
    mutate(estimand = "ITT", comparator = comparator)
  
}

### Estimate as treated effect

my_gform_at <- function(data, outcome_var){
  
  if(outcome_var == "Cancer"){
    source("R/cancer_formulas.R")
  }
  
  setDT(data)
  data[, Y3M_HearingAidUse := round(as.numeric(Y3M_HearingAidUse))]
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
  
  models$pcs_mod <- lm(as.formula(PCS_form_at), data = data)
  
  models$mcs_mod <- lm(as.formula(MCS_form_at), data = data)
  
  models$poly_mod <- lm(as.formula(polypharmacy_form_at), data = data)
  
  models$frailty_mod <- lm(as.formula(frailty_form_at), data = data)
  
  models$cesd_mod <- lm(as.formula(cesd_form_at), data = data)
  
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
  
  ## FUnction for g-computation
  
  gform_risk <- function(data, outcome_var, interv){
    
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
  
  ## Estimate risk for each intervention
  
  map(c(0, 1, 2), gform_risk, data = data, outcome_var = outcome_var) |>
    bind_rows() |>
    pivot_wider(names_from = interv,
                values_from = Y,
                names_prefix = "Y") |>
    mutate(RR = Y2 / Y0, RD = Y2 - Y0) |>
    pivot_longer(
      c(Y0, Y1, Y2),
      names_to = "interv",
      values_to = "Y",
      names_prefix = "Y"
    ) |> 
    mutate(estimand = "AT", comparator = "Y0")
}

## Simulating covariates over follow-up

simulate <- 
  function(data, time, models, ranges, rmse, outcome_var){
    if(time == 1){
      
      # CVD 
      data[time == 1,
           CVD := rbinom(
             nrow(.SD[time == 1, ]),
             1,
             predict(models$cvd_mod, newdata = .SD[time == 1, ], type = "response"))]
      
      if(!outcome_var == "Cancer"){
        # Cancer
        data[time == 1,
             Cancer := rbinom(
               nrow(.SD[time == 1, ]),
               1,
               predict(models$cancer_mod, newdata = .SD[time == 1, ], type = "response"))]
      }
      
      # Polypharmacy
      
      data[time == 1, Polypharmacy := Rnorm(nrow(.SD[time == 1,])) * rmse$Polypharmacy + 
             predict(models$poly_mod, newdata = .SD[time == 1,])]
      
      data[, Polypharmacy := 
             ifelse(Polypharmacy < ranges$Polypharmacy_low, 
                    ranges$Polypharmacy_low, Polypharmacy)]
      data[, Polypharmacy := 
             ifelse(Polypharmacy > ranges$Polypharmacy_high, 
                    ranges$Polypharmacy_high, Polypharmacy)]
      
      
      # Frailty
      
      data[time == 1, Frailty := Rnorm(nrow(.SD[time == 1,])) * rmse$Frailty + 
             predict(models$frailty_mod, newdata = .SD[time == 1,])]
      
      data[, Frailty := 
             ifelse(Frailty < ranges$Frailty_low, ranges$Frailty_low, Frailty)]
      data[, Frailty := 
             ifelse(Frailty > ranges$Frailty_high, ranges$Frailty_high, Frailty)]
      
      # PCS
      
      data[time == 1, PCS := Rnorm(nrow(.SD[time == 1,])) * rmse$PCS + 
             predict(models$pcs_mod, newdata = .SD[time == 1,])]
      
      data[, PCS := ifelse(PCS < ranges$PCS_low, ranges$PCS_low, PCS)]
      data[, PCS := ifelse(PCS > ranges$PCS_high, ranges$PCS_high, PCS)]
      
      # MCS
      
      data[time == 1, MCS := Rnorm(nrow(.SD[time == 1,])) * rmse$MCS + 
             predict(models$mcs_mod, newdata = .SD[time == 1,])]
      
      data[, MCS := ifelse(MCS < ranges$MCS_low, ranges$MCS_low, MCS)]
      data[, MCS := ifelse(MCS > ranges$MCS_high, ranges$MCS_high, MCS)]
      
      
      # Cesd
      
      data[time == 1, CesdOverall := Rnorm(nrow(.SD[time == 1,])) * rmse$CesdOverall + 
             predict(models$cesd_mod, newdata = .SD[time == 1,])]
      
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
             predict(models$cvd_mod, newdata = .SD[time == time, ], type = "response"))]
      
      data[time == time, CVD := ifelse(lag1_CVD == 1, 1, CVD)]
      
      # Cancer
      
      if(!outcome_var == "Cancer"){
        data[time == time,
             Cancer := rbinom(
               nrow(.SD[time==time,]),
               1,
               predict(models$cancer_mod, newdata = .SD[time == time, ], type = "response"))]
        data[time == time, Cancer := ifelse(lag1_Cancer == 1, 1, Cancer)]
      }
      
      # Polypharmacy
      
      data[time==time, Polypharmacy := Rnorm(nrow(.SD[time==time,])) * rmse$Polypharmacy +
             predict(models$poly_mod, newdata = .SD[time==time,])]
      
      data[, Polypharmacy := 
             ifelse(Polypharmacy < ranges$Polypharmacy_low, 
                    ranges$Polypharmacy_low, Polypharmacy)]
      data[, Polypharmacy := 
             ifelse(Polypharmacy > ranges$Polypharmacy_high, 
                    ranges$Polypharmacy_high, Polypharmacy)]
      
      # Frailty
      
      data[time==time, Frailty := Rnorm(nrow(.SD[time==time,])) * rmse$Frailty + 
             predict(models$frailty_mod, newdata = .SD[time==time,])]
      
      data[, Frailty := 
             ifelse(Frailty < ranges$Frailty_low, ranges$Frailty_low, Frailty)]
      data[, Frailty := 
             ifelse(Frailty > ranges$Frailty_high, ranges$Frailty_high, Frailty)]
      
      # PCS
      
      data[time==time, PCS := Rnorm(nrow(.SD[time==time,])) * rmse$PCS + 
             predict(models$pcs_mod, newdata = .SD[time==time,])]
      
      data[, PCS := ifelse(PCS < ranges$PCS_low, ranges$PCS_low, PCS)]
      data[, PCS := ifelse(PCS > ranges$PCS_high, ranges$PCS_high, PCS)]
      
      # MCS
      
      data[time==time, MCS := Rnorm(nrow(.SD[time==time,])) * rmse$MCS + 
             predict(models$mcs_mod, newdata = .SD[time==time,])]
      
      data[, MCS := ifelse(MCS < ranges$MCS_low, ranges$MCS_low, MCS)]
      data[, MCS := ifelse(MCS > ranges$MCS_high, ranges$MCS_high, MCS)]
      
      # Cesd
      
      data[time==time, CesdOverall := Rnorm(nrow(.SD[time==time,])) * rmse$CesdOverall + 
             predict(models$cesd_mod, newdata = .SD[time==time,])]
      
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