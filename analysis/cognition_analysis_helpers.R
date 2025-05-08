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

# Prepare dataset for fitting models 

prepare_data <- function(imputed_data,
                         selfrated_hearing,
                         truncate_threshold,
                         remove_post_death){
  
  # Return continuous variables to their original scale 
  
  df <- undo_standardise(imputed_data)
  
  # New ID variable (Safehaven no longer unique)
  
  df$ID <- 1:nrow(df)

  # apply eligibility criteria
  
  df <- apply_criteria(df, selfrated_hearing)
  
  # remove post-death observations
  
  if(isTRUE(remove_post_death)){
    df <- remove_postdeath(df)
  }
  
  # calculate cognitive factor scores
  
  factors <- get_factorscores(df, truncate_threshold)
  
  df <- left_join(df, factors, by = "ID")
  
  # remove 3 from 3MS variable names 
  
  names(df)[grepl("MS", names(df))] <- 
    gsub("_3", "_", names(df)[grepl("MS", names(df))])
  
  # set hearing aid use to factor
  
  df$Y3M_HearingAidUse <- as.factor(df$Y3M_HearingAidUse)
  
  return(df)
}

## get cognition results 

get_results <- function(df_list,
                        outcome_var = "g",
                        comparator = "control",
                        final_visit = 10,
                        effect_modifier = NULL,
                        satterthwaite = TRUE) {
  estimates <- 
    cognition_estimates(df_list, 
                        cognition_outcome = outcome_var,
                        comparator= comparator,
                        final_visit = final_visit,
                        effect_modifier = effect_modifier)
  
  intervals <- 
    cognition_intervals(estimates,
                        effect_modifier = effect_modifier,
                        satterthwaite = satterthwaite)
  
  return(intervals)
}

## Point estimates for cognitive outcomes

cognition_estimates <-
  function(data,
           cognition_outcome,
           comparator,
           final_visit,
           effect_modifier) {
    
    # parallel
    plan(multisession)
    
    # see cognition_analysis_helpers for get_estimates_cognition()
    results <-future_lapply(
      data, 
      get_estimates_cognition,
      cognition_outcome,
      comparator = comparator,
      final_visit = final_visit,
      effect_modifier = effect_modifier,
      future.packages = "rms"
    )
    
    plan(sequential)
    
    return(results)
  }

## Confidence intervals for cognitive outcomes

cognition_intervals <- function(point_estimates,
                                effect_modifier,
                                satterthwaite){
  
  estimates <- add_intervals(
    point_estimates,
    satterthwaite = satterthwaite,
    effect_modifier = effect_modifier
  )
  
  contrasts <- get_contrasts(
    point_estimates,
    satterthwaite = satterthwaite,
    effect_modifier = effect_modifier
  )
  
  # plots
  plots <- plot_estimates(estimates, contrasts)
  
  return(list(
    estimates = estimates,
    contrasts = contrasts,
    plots = plots
  ))
}

## Fit cognitive models and return results

get_estimates_cognition <- function(data_list,
                                    outcome_var,
                                    comparator,
                                    final_visit = 10,
                                    effect_modifier = effect_modifier) {
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
  
  return(list(estimates_itt = estimates_itt, estimates_at = estimates_at))
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
    return(out)
  } else {
    out <- gcomp()
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
    return(out)
  } else {
    out <- dose_response()
    return(out)
  }
}

## mean between group differences with 95% CI 

get_contrasts <- function(results, 
                          as_treated = TRUE, 
                          satterthwaite,
                          effect_modifier = effect_modifier){
  
  # ITT
  itt <- bind_rows(results, .id = "B") |> 
    dplyr::select(B, estimates_itt) |> 
    unnest(estimates_itt) |> 
    mutate(B = as.factor(as.integer(factor(B))))
  
  # wide format for contrasts 
  
  contrasts_wide <- function(effect_modifier_level = NULL){
    if(is.null(effect_modifier)){
      itt <- itt |> 
        group_by(B, time, A) |> 
        mutate(M = 1:2) |> 
        ungroup() |> 
        pivot_wider(names_from = A, 
                    values_from = Y, 
                    names_prefix = "estimate_") |> 
        mutate(Y = estimate_1 - estimate_0,
               A = "1_0") |> 
        dplyr::select(B, A, time, Y)
      return(itt)
    } else {
      itt <- itt[itt[[effect_modifier]] == effect_modifier_level,]
      itt <- itt |> 
        group_by(B, time, A) |> 
        mutate(M = 1:2) |> 
        ungroup() |> 
        pivot_wider(names_from = A, 
                    values_from = Y, 
                    names_prefix = "estimate_") |> 
        mutate(Y = estimate_1 - estimate_0,
               A = "1_0") |> 
        dplyr::select(B, A, time, Y)
      itt[[effect_modifier]] <- effect_modifier_level
      return(itt)
    }
  }
  
  if(is.null(effect_modifier)){
    
    itt <- contrasts_wide()
    
    params <- 
      expand_grid(unique(itt$A), unique(itt$time)) |> 
      set_names(c("A","time"))
    
    itt_out <- map2(params$time, 
                    params$A, 
                    get_variance, 
                    data = itt, 
                    satterthwaite = satterthwaite,
                    effect_modifier = effect_modifier)
    
    itt_out <- bind_rows(itt_out)
  } else {
    
    itt <- map_df(unique(itt[[effect_modifier]]), contrasts_wide)
    
    params <- 
      expand_grid(unique(itt$A), 
                  unique(itt$time), 
                  unique(itt[[effect_modifier]])) |> 
      set_names(c("A","time", "effect_modifier_level"))
    
    itt_out <- pmap(list(time = params$time,
                         A = params$A,
                         effect_modifier_level = params$effect_modifier_level), 
                    get_variance, 
                    data = itt, 
                    effect_modifier = effect_modifier,
                    satterthwaite = satterthwaite)
    itt_out <- bind_rows(itt_out)
  }
  
  # dose response
  
  if(isTRUE(as_treated)){
    at <- bind_rows(results, .id = "B") |> 
      dplyr::select(B, estimates_at) |> 
      unnest(estimates_at) |> 
      mutate(B = as.factor(as.integer(factor(B))))
    
    contrasts_wide_at <- function(effect_modifier_level = NULL){
      
      if(is.null(effect_modifier)){
        at <- at |> 
          group_by(B, time, A) |> 
          mutate(M = 1:2) |> 
          ungroup() |> 
          pivot_wider(names_from = A, 
                      values_from = Y, 
                      names_prefix = "estimate_") |> 
          mutate(Y = estimate_3 - estimate_1,
                 A = "3_1") |> 
          dplyr::select(B, A, time, Y)
        return(at)
      } else {
        at <- at[at[[effect_modifier]] == effect_modifier_level,]
        at <- at |> 
          group_by(B, time, A) |> 
          mutate(M = 1:2) |> 
          ungroup() |> 
          pivot_wider(names_from = A, 
                      values_from = Y, 
                      names_prefix = "estimate_") |> 
          mutate(Y = estimate_3 - estimate_1,
                 A = "3_1") |> 
          dplyr::select(B, A, time, Y)
        at[[effect_modifier]] <- effect_modifier_level
        return(at)
      }
    }
    
    if(is.null(effect_modifier)){
      at <- contrasts_wide_at()
      
      params <- 
        expand_grid(unique(at$A), unique(at$time)) |> 
        set_names(c("A","time"))
      
      at_out <- map2(params$time, 
                     params$A, 
                     get_variance, 
                     data = at, 
                     satterthwaite = satterthwaite,
                     effect_modifier = effect_modifier)
      
      at_out <- bind_rows(at_out)
    } else {
      at <- map_df(unique(at[[effect_modifier]]), contrasts_wide_at)
      
      params <- 
        expand_grid(unique(at$A), 
                    unique(at$time), 
                    unique(at[[effect_modifier]])) |> 
        set_names(c("A","time", "effect_modifier_level"))
      
      at_out <- pmap(list(time = params$time,
                          A = params$A,
                          effect_modifier_level = params$effect_modifier_level), 
                     get_variance, 
                     data = at, 
                     effect_modifier = effect_modifier,
                     satterthwaite = satterthwaite)
      at_out <- bind_rows(at_out)
    }
    
    return(list(itt = itt_out, at = at_out))
  } else{
    return(list(itt = itt_out))
  }
}

## Bootstrap confidence intervals

add_intervals <- function(results, 
                          satterthwaite, 
                          as_treated = TRUE,
                          effect_modifier = effect_modifier){
  
  # ITT
  itt <- bind_rows(results, .id = "B") |> 
    dplyr::select(B, estimates_itt) |> 
    unnest(estimates_itt) |> 
    mutate(B = as.factor(as.integer(factor(B))))
  
  if(is.null(effect_modifier)){
    params <- 
      expand_grid(unique(itt$A), unique(itt$time)) |> 
      set_names(c("A","time"))
    
    itt_out <- map2(params$time, 
                    params$A, 
                    get_variance, 
                    data = itt, 
                    satterthwaite = satterthwaite,
                    effect_modifier = effect_modifier)
  } else {
    params <- 
      expand_grid(unique(itt$A), 
                  unique(itt$time), 
                  unique(itt[[effect_modifier]])) |> 
      set_names(c("A","time", "effect_modifier_level"))
    
    itt_out <- pmap(list(time = params$time,
                         A = params$A,
                         effect_modifier_level = params$effect_modifier_level), 
                    get_variance, 
                    data = itt, 
                    effect_modifier = effect_modifier,
                    satterthwaite = satterthwaite)
  }
  
  itt_out <- bind_rows(itt_out)
  
  # dose response 
  
  if(isTRUE(as_treated)){
    
    at <- bind_rows(results, .id = "B") |> 
      dplyr::select(B, estimates_at) |> 
      unnest(estimates_at) |> 
      mutate(B = as.factor(as.integer(factor(B))))
    
    if(is.null(effect_modifier)){
      
      params <- 
        expand_grid(unique(at$A), unique(at$time)) |> 
        set_names(c("A","time"))
      
      at_out <- map2(params$time, 
                     params$A, 
                     get_variance, 
                     data = at, 
                     satterthwaite = satterthwaite,
                     effect_modifier = effect_modifier)
      
    } else {
      
      params <- 
        expand_grid(unique(at$A), 
                    unique(at$time), 
                    unique(at[[effect_modifier]])) |> 
        set_names(c("A","time", "effect_modifier_level"))
      
      at_out <- pmap(list(time = params$time,
                          A = params$A,
                          effect_modifier_level = params$effect_modifier_level), 
                      get_variance, 
                      data = at, 
                      effect_modifier = effect_modifier,
                      satterthwaite = satterthwaite)
    }
    
    at_out <- bind_rows(at_out)
    
    return(list(itt = itt_out, at = at_out))
  } else {
    return(list(itt = itt_out))
  }
}

## get variance for 95% CIs

get_variance <- function(data, 
                         time, 
                         A, 
                         satterthwaite,
                         effect_modifier = effect_modifier,
                         effect_modifier_level = NULL){

  B <- max(as.numeric(data$B))
  # by time, treatment, and effect modifier
  
  if(is.null(effect_modifier)){
    data <- data |> filter(time == {{ time }}, A == {{ A }})
  } else {
    data <- data |> filter(time == {{ time }}, A == {{ A }})
    data <- data[data[[effect_modifier]] == effect_modifier_level,]
  }
  
  # estimate between and within mean sum of squares
  model <- aov(Y ~ B, data = data)
  MSB <- summary(model)[[1]]$`Mean Sq`[1]
  MSW <- summary(model)[[1]]$`Mean Sq`[2]
  sigma1 <- (MSB - MSW) / 2
  sigma1 <- ifelse(sigma1 < 0, 0, sigma1)
  sigma2 <- ifelse(sigma1 < 0, var(data$Y), MSW)
  Var <- ((1 + (1/B))*sigma1) + ((1/(B*2))*sigma2)
  part1 <- ((((B+1)/(B*2))^2) * (MSB^2))/(B-1)
  part2 <- MSW^2 / (4*B)
  df <- Var^2 / (part1 + part2)
  
  suppressMessages(data <- data |> group_by(time, A) |> 
                     summarise(estimate = mean(Y)))
  
  if(!is.null(effect_modifier)){
    data[[effect_modifier]] <- effect_modifier_level
  }
  
  data$se <- sqrt(Var)
  data$lower <- ifelse(satterthwaite, 
                       data$estimate - qt(0.975, df)*data$se, 
                       data$estimate - 1.96*data$se)
  data$upper <- ifelse(satterthwaite, 
                       data$estimate + qt(0.975, df)*data$se, 
                       data$estimate + 1.96*data$se)
  if(satterthwaite){
    data$df <- df
  }
  return(data)
}

