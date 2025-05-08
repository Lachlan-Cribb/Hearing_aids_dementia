## IP weighting for time to event outcomes
## sensitivity analysis for model specification

options(future.globals.maxSize = Inf)

ipw_all_outcomes <- function(data_list, gform_estimates, satterthwaite = TRUE){
  
  out_dem <- ipw_results(data_list, "Dem", TRUE)
  out_imp <- ipw_results(data_list, "Imp", TRUE)
  
  ## Plot versus g-formula estimates
  
  # Dementia
  plot_dem <- gform_vs_ipw(gform_estimates$Dem, out_dem)
  
  save_cuminc_plot(plot_dem, 
                   "Dem_gform_ipw.png",
                   "Dementia risk",
                   ratios = FALSE,
                   breaks = c(0,0.02,0.04,0.06,0.08,0.1),
                   limits = c(0,0.1),
                   legend.position = "right")
  
  # Impairment
  plot_imp <- gform_vs_ipw(gform_estimates$Imp, out_imp)
  
  save_cuminc_plot(plot_imp, 
                   "Imp_gform_ipw.png",
                   "Cognitive impairment risk",
                   ratios = FALSE,
                   breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                   limits = c(0, 0.5),
                   legend.position = "right")
  
  return(list(out_dem = out_dem, out_imp = out_imp, 
              plot_dem = plot_dem, plot_imp = plot_imp))
}

## extract bootstrapped results 

ipw_results <- function(data_list, outcome_var, satterthwaite = TRUE) {
  
  out <- lapply(
    data_list,
    FUN = function(.x) {
      survival_results_ipw(
        .x, 
        outcome_var = outcome_var, 
        satterthwaite = satterthwaite
        )
    }
  )
  
  out <- get_risk(out)
  
  out <- add_intervals_survival(out, satterthwaite = satterthwaite)
  
  return(out)
}

survival_results_ipw <- function(data_list, 
                                 outcome_var,
                                 satterthwaite = satterthwaite){
  
  long_list <- map(data_list,
                   to_long_survival,
                   outcome_var = outcome_var)
  
  # Remove all observations after first occurrence of death or outcome
  
  long_list <- map(long_list,
                   remove_postevent,
                   outcome_var = outcome_var)
  
  # Estimate weighted risk
  
  risks <- ipw_fit(long_list, outcome_var = outcome_var)
  
  return(risks)
}


### Estimate IP weights and calculated weighted risk 

ipw_fit <- function(data_list, outcome_var){
  
  ## ITT 
  
  # estimate treatment and censoring weights
  ipw_itt <- function(data, outcome_var){
    data$Y <- data[[outcome_var]]
    data$A <- data$Y3M_HearingAid
    
    # treatment weights
    trt_model <- weightit(
      as.formula(A_form), 
      data = data, method = "glm",
      keep.mparts = FALSE)
    
    data$iptw <- trt_model$weights
    
    # censoring weights
    denom_model <- glm(as.formula(cens_form), data = data, family = binomial())
    data$denom <- predict(denom_model, newdata = data, type = "response")
    denom_model <- NULL
    
    setDT(data)
    
    data <- data[, `:=`(
      numerator = cumprod(1-Cens), 
      denominator = cumprod(1 - denom)),
      by = ID]
    
    data <- data[, ipcw := numerator / denominator]
    
    # combine weights
    data[, weights := trim(data$iptw*data$ipcw, 0.98)]
    
    # estimate subdistribution hazard and risk non-parametrically
    itt <- data[, lag_Y := shift(Y, fill = 0, type = "lag")]
    itt <- itt[, `:=`(
      hsub_noint = sum(Y * (1 - lag_Y) * weights * (1-A), na.rm = T) /
        sum((1 - lag_Y) * weights * (1-A), na.rm = T),
      hsub_int =
        sum(Y * (1 - lag_Y) * weights * A, na.rm = T) /
        sum((1 - lag_Y) * weights * A, na.rm = T)
    ), by = time]
    
    itt <- itt[, `:=`(
      risk_noint = cumsum(hsub_noint *
                            cumprod(1 - shift(hsub_noint, fill=0, type="lag"))),
      risk_int = cumsum(hsub_int *
                          cumprod(1 - shift(hsub_int, fill=0, type="lag")))
    ), by = ID]
    
    itt <- itt[, .(Y_1 = mean(risk_int), Y_0 = mean(risk_noint)), by = time]
    itt <- itt[, `:=`(RR = Y_1 / Y_0, RD = Y_1 - Y_0)]
    
    itt <- melt(
      itt,
      id.vars = c("time", "RR", "RD"),
      measure.vars = c("Y_0", "Y_1"),
      variable.name = "A",
      value.name = "Y"
    )[, A := sub("Y_", "", A)]
    
    itt$RR <- ifelse(itt$RR == Inf, NA, itt$RR)
    
    setnames(itt,
             c("k", "Risk ratio", "Risk difference", "Interv.", "g-form risk"))
    
    return(itt)
  }
  
  ## AT
  ipw_at <- function(data, outcome_var){
    data$Y <- data[[outcome_var]]
    data$A <- data$Y3M_HearingAidUse
    data$A <- ifelse(data$A %in% c("2","3"), "2", data$A)
    data$A <- ifelse(data$A %in% c("4","5"), "3", data$A)
    
    # treatment weights
    weight_mod <- weightit(as.formula(A_form), data = data, method = "glm")
    data$iptw <- weight_mod$weights
    
    # censoring weights
    denom_model <- glm(as.formula(cens_form), data = data, family = binomial())
    data$denom <- predict(denom_model, newdata = data, type = "response")
    denom_model <- NULL
    
    setDT(data)
    
    data <- data[, `:=`(
      numerator = cumprod(1-Cens), 
      denominator = cumprod(1 - denom)),
      by = ID]
    
    data <- data[, ipcw := numerator / denominator]
    
    # combine weights
    
    data[, weights := trim(data$iptw*data$ipcw, 0.98)]
    
    # estimate subdistribution hazard
    at <- data[, lag_Y := shift(Y, fill = 0, type = "lag")]
    at <- at[, `:=`(
      hsub_1 = 
        sum(Y * (1-lag_Y)*weights*ifelse(A=="1",1,0), na.rm=T) /
        sum((1-lag_Y)*weights*ifelse(A=="1",1,0), na.rm=T),
      hsub_2 = 
        sum(Y * (1-lag_Y)*weights*ifelse(A=="2",1,0), na.rm=T) /
        sum((1-lag_Y)*weights*ifelse(A=="2",1,0), na.rm=T),
      hsub_3 = 
        sum(Y * (1-lag_Y)*weights*ifelse(A=="3",1,0), na.rm=T) /
        sum((1-lag_Y)*weights*ifelse(A=="3",1,0), na.rm=T)
    ), by = time]
    
    at <- at[, `:=`(
      risk_1 = 
        cumsum(hsub_1 * cumprod(1-shift(hsub_1, fill = 0, type = "lag"))),
      risk_2 = 
        cumsum(hsub_2 * cumprod(1-shift(hsub_2, fill = 0, type = "lag"))),
      risk_3 = 
        cumsum(hsub_3 * cumprod(1-shift(hsub_3, fill = 0, type = "lag")))
    ), by = ID]
    
    at <- at[, .(
      Y_1 = mean(risk_1), 
      Y_2 = mean(risk_2), 
      Y_3 = mean(risk_3)
      ), by = time]
    
    at <- at[, `:=`(RR = Y_3 / Y_1, RD = Y_3 - Y_1)]
    
    at <- melt(
      at,
      id.vars = c("time", "RR", "RD"),
      measure.vars = c("Y_1", "Y_2", "Y_3"),
      variable.name = "A",
      value.name = "Y"
    )[, A := sub("Y_", "", A)]
    
    at$RR <- ifelse(at$RR == Inf, NA, at$RR)
    
    setnames(at, 
             c("k","Risk ratio", "Risk difference", "Interv.", "g-form risk"))
    
    return(at)
  }
  
  itt_fit <- map(data_list, ipw_itt, outcome_var = outcome_var)
  at_fit <- map(data_list, ipw_at, outcome_var = outcome_var)
  
  return(list(itt_fit = itt_fit,
              at_fit = at_fit))
}

