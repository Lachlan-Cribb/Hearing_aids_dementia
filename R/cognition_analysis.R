### Helper functions for analysis

covars <- Hmisc::Cs(
    time,
    AgeAtRand,
    Gender,
    Racial,
    Edu,
    BL_MS_OverallScore_C,
    BL_COWAT,
    BL_HVLT_TotalRecall,
    BL_HVLT_DelayedRecall,
    BL_HVLT_Retention,
    BL_HVLT_RDI,
    BL_SDMT,
    EyesRate,
    HearingProbs,
    HearingProbs_2,
    BLToneAvg_Better,
    Buzz,
    QuietRm,
    CrowdedRm,
    HrsSleep,
    SupplementUse,
    CommunityEngagement,
    LeisureActivities,
    PresPhysAct,
    Alone,
    Income,
    BL_CesdOverall,
    DAB,
    Pt_Cr,
    BL_BMI,
    BL_SmHis2,
    BL_SmHis3,
    BL_AlcWk,
    BL_MCS,
    BL_PCS,
    Pt_scoreIRSAD,
    BL_Frailty_DAI50,
    hypstatus_BLNo_untreated,
    hypstatus_BLYes_treated,
    hypstatus_BLYes_untreated,
    BL_Polypharmacy,
    apoe_e4
  )

## Fit cognitive models and return results

get_estimates_cognition <- function(data_list,
                                    outcome_var,
                                    comparator,
                                    final_visit = 10,
                                    effect_modifier) {
  long_list <- map(
    data_list,
    to_long,
    outcome_var = outcome_var,
    final_visit = final_visit,
    effect_modifier = effect_modifier
  )
  
  ## intention to treat
  
  itt <- map(long_list, intention_to_treat, Y = outcome_var)
  
  estimates_itt <- map2(
    itt,
    long_list,
    get_estimates_itt,
    comparator = comparator,
    effect_modifier = effect_modifier
  )
  
  ## dose response
  
  at <- map(long_list, as_treated, Y = outcome_var)
  
  estimates_at <- map2(
    at, 
    long_list, 
    get_estimates_at, 
    effect_modifier = effect_modifier)
  
  ## Merge 
  
  estimates_itt <- bind_rows(estimates_itt, .id = "m")
  estimates_itt$m <- as.numeric(as.factor(estimates_itt$m))
  estimates_at <- bind_rows(estimates_at, .id = "m")
  estimates_at$m <- as.numeric(as.factor(estimates_at$m))
  
  bind_rows(estimates_itt, estimates_at)
}

## Data to long format 

to_long <- function(data,
                    outcome_var,
                    final_visit = final_visit,
                    effect_modifier = effect_modifier) {
  
  vars <- paste(paste("AV", 3:final_visit, sep = ""), outcome_var, sep = "_")
  
  ## Data to long format
  
  long <-
    data |>
    pivot_longer(
      all_of(vars),
      names_to = "time",
      values_to = outcome_var,
      names_prefix = "AV"
    ) |>
    mutate(time = as.numeric(sub(
      paste("_", outcome_var, sep = ""), "", time
    )))
  
  # remove observations with missing data
  
  if (is.null(effect_modifier)) {
    long <- long |>
      dplyr::select(
        ID,
        Safehaven,
        time,
        all_of(outcome_var),
        Y3M_HearingAid,
        Y3M_HearingAidUse,
        all_of(covars)
      ) |>
      drop_na()
  } else {
    long <- long |>
      dplyr::select(
        ID,
        Safehaven,
        time,
        all_of(outcome_var),
        Y3M_HearingAid,
        Y3M_HearingAidUse,
        all_of(covars),
        all_of(effect_modifier)
      ) |>
      drop_na()
  }
  return(long)
}
  
 
## Intention to treat effect 

intention_to_treat <- function(data, Y, A = "Y3M_HearingAid"){
  
  data[["Y"]] <- data[[Y]]
  data[["A"]] <- data[[A]]
  
  # model formula
  lm_form <- as.formula(paste("Y ~", cog_itt_form))

  # fit model
  itt_out <- lm(lm_form, data = data)
  itt_out <- strip_lm(itt_out)
  
  return(itt_out)
}


## dose response effect

as_treated <- function(data, Y, A = "Y3M_HearingAidUse"){
  
  data[["Y"]] <- data[[Y]]
  data[["A"]] <- data[[A]]
  data$A <- as.factor(round(as.numeric(data$A)))
  data[["A"]] <- 
    fct_recode(data[["A"]], "2" = "3", "3" = "4", "3" = "5")
  
  # model formula
  lm_form <- as.formula(paste("Y ~", cog_at_form))

  # fit model 
  at_out <- lm(lm_form, data = data)
  at_out <- strip_lm(at_out)
  
  return(at_out)
}

### Function for extracting estimates from model objects

get_estimates_itt <- function(out,
                              data,
                              A = "Y3M_HearingAid",
                              comparator = c("control", "no intervention"),
                              effect_modifier = effect_modifier) {
  
  gcomp <- function(effect_modifier_level=NULL){
    
    if(!is.null(effect_modifier_level)){
      data <- data[data[[effect_modifier]]==effect_modifier_level,]
    }
    
    data[["A"]] <- data[[A]]
    
    if(comparator == "control"){
      data[["A"]] <- 0
    }
    
    model <- out
    
    # comparator
    comp <- mutate(data, A = data$A)
    comp$Y <- predict(model, newdata = comp)
    comp$A <- as.factor(comp$A)
    comp <- comp |> group_by(time) |> summarise(Y = mean(Y))
    comp$A <- "0"
    
    # hearing aid assignment
    ha <- mutate(data, A = 1)
    ha$Y <- predict(model, newdata = ha)
    ha$A <- as.factor(ha$A)
    ha <- ha |> group_by(time) |> summarise(Y = mean(Y))
    ha$A <- "1"
    
    gcomp_out <- bind_rows(comp, ha)
    
    if(!is.null(effect_modifier_level)){
      gcomp_out[[effect_modifier]] <- effect_modifier_level
    }
    
    return(gcomp_out)
  }
  
  if(!is.null(effect_modifier)){
    effect_modifier_levels <- unique(data[[effect_modifier]])
    out <- map_df(effect_modifier_levels, gcomp)
    out$estimand <- "ITT"
    return(out)
  } else {
    out <- gcomp()
    out$estimand <- "ITT"
    return(out)
  }
}

## Get estimates for as-treated effect

get_estimates_at <- function(out,
                             data,
                             A = "Y3M_HearingAidUse",
                             effect_modifier = effect_modifier) {
  
  dose_response <- function(effect_modifier_level=NULL){
    
    if(!is.null(effect_modifier_level)){
      data <- data[data[[effect_modifier]]==effect_modifier_level,]
    }
    
    data[["A"]] <- data[[A]]
    model <- out
    
    dose_response_gcomp <- function(out, i) {
      df <- mutate(data, A = i)
      df$Y <- as.numeric(predict(model, df))
      df <- df |> group_by(time) |> summarise(Y = mean(Y))
      df$A <- as.character(i)
      return(df)
    }
    
    gcomp_out <- 
      map(as.factor(1:3), 
          dose_response_gcomp, 
          out = out) |> 
      bind_rows()
    
    if(!is.null(effect_modifier_level)){
      gcomp_out[[effect_modifier]] <- effect_modifier_level
    }
    
    return(gcomp_out)
  }
  
  if(!is.null(effect_modifier)){
    effect_modifier_levels <- unique(data[[effect_modifier]])
    out <- map_df(effect_modifier_levels, dose_response)
    out$estimand <- "AT"
    return(out)
  } else {
    out <- dose_response()
    out$estimand <- "AT"
    return(out)
  }
}