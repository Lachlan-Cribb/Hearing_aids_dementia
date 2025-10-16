## Consent for participation in ASPREE-XT (negative outcome control)
get_visit_data <- function(){
  
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
}

xt_consent_analysis <- function(df_list, visits_data){
  df_list <- map(df_list, ~ left_join(.x, visits_data, by = "Safehaven"))
  
  ## add XT consent variable
  df_list <- map(
    df_list, 
    function(.x){
      .x$xt_consent <- ifelse(rowSums(select(.x, contains("_Possible")) == 7)>0, 0, 1)
      .x
    })

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
    data$A <- as.factor(round(as.numeric(data$Y3M_HearingAidUse)))
    data$A <- fct_collapse(data$A, "2" = c("2","3"), "3" = c("4","5"))
    
    at_mod <- glm(as.formula(consent_formula), data = data, family = binomial)
    
    AT_1 <- predict(at_mod, newdata = mutate(data, A = "1"), type = "response")
    AT_2 <- predict(at_mod, newdata = mutate(data, A = "2"), type = "response")
    AT_3 <- predict(at_mod, newdata = mutate(data, A = "3"), type = "response")
    
    tibble(
      ITT_0 = mean(ITT_0), 
      ITT_1 = mean(ITT_1), 
      AT_1 = mean(AT_1), 
      AT_2 = mean(AT_2), 
      AT_3 = mean(AT_3))
  }
  
  out <- map(df_list, consent_analysis)
  
  out <- bind_rows(out, .id = "m")
  out$m <- as.numeric(as.factor(out$m))
  out
}

summarise_xt_consent <- function(result){
  out <- bind_rows(result, .id = "b")
  colMeans(out |> select(ITT_0,  ITT_1, AT_1, AT_2, AT_3))
}
  
  





