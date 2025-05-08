## Consent for participation in ASPREE-XT (negative outcome control)

xt_consent <- function(df_list){
  
  # add visit data
  visits <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionH1_Visits_XT06_v1.csv"
    )
  )
  
  visits <- 
    visits |> 
    dplyr::select(Safehaven, ends_with("Possible"), ends_with("Conduct")) |>
    dplyr::select(Safehaven, starts_with("AV"), -contains("Reassess"))
  
  df_list <- map(df_list,
                 ~ map(.x, 
                       function(.x) left_join(.x, visits, by = "Safehaven")))
  
  ## add XT consent variable
  
  add_xt_consent <- function(data){
    data$xt_consent <- ifelse(
      rowSums(data |> dplyr::select(contains("_Possible")) == 7)>0, 0, 1)
    return(data)
  }
  
  df_list <- map(df_list, ~ map(.x, add_xt_consent))
  
  ## Regression formula
  
  consent_analysis <- function(data){
    
    data$Y <- data$xt_consent
    
    ## intention-to-treat
    data$A <- data$Y3M_HearingAid
    itt_mod <- glm(as.formula(consent_formula), data = data, family = binomial)
    
    # g-comp
    ITT_1 <- predict(itt_mod, newdata = mutate(data, A = 1), type = "response")
    ITT_0 <- predict(itt_mod, newdata = mutate(data, A = 0), type = "response")
    
    ## Dose-response 
    data$A <- as.factor(data$Y3M_HearingAidUse)
    data$A <- fct_collapse(data$A, "2" = c("2","3"), "3" = c("4","5"))
    
    at_mod <- glm(as.formula(consent_formula), data = data, family = binomial)
    
    AT_1 <- predict(at_mod, newdata = mutate(data, A = "1"), type = "response")
    AT_2 <- predict(at_mod, newdata = mutate(data, A = "2"), type = "response")
    AT_3 <- predict(at_mod, newdata = mutate(data, A = "3"), type = "response")
    
    out <- tibble(ITT_0 = mean(ITT_0), ITT_1 = mean(ITT_1), 
                  AT_1 = mean(AT_1), AT_2 = mean(AT_2), AT_3 = mean(AT_3))
    
    return(out)
  }
  
  out <- map(df_list, ~ map_df(.x, consent_analysis), .progress = TRUE)
  
  out <- map(out, function(.x) mutate(.x, m = c(1,2)))
  
  out <- bind_rows(out, .id = "B")
  
  return(colMeans(out |> dplyr::select(-m, -B)))
}




