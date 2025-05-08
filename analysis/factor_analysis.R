
cog_vars <- c("3MS_OverallScore_C",
              "HVLT_RDI",
              "HVLT_Retention",
              "HVLT_TotalRecall",
              "HVLT_DelayedRecall",
              "COWAT",
              "SDMT")

get_factorscores <- function(data, truncate_threshold) {
  
  
  # truncate function for extreme cog test values
  truncate <- function(x) {
    x <- ifelse(
      x > quantile(x, truncate_threshold, na.rm = T),
      quantile(x, truncate_threshold, na.rm = T),
      x
    )
    x <- ifelse(
      x < quantile(x, 1 - truncate_threshold, na.rm = T),
      quantile(x, 1 - truncate_threshold, na.rm = T),
      x
    )
    return(x)
  }
  
  ### Baseline CFA 
  
  bl_vars <- paste0("BL_", cog_vars)
  
  # truncate extreme values at 99th percentile
  cog_data <- data |> mutate(across(all_of(bl_vars), truncate))
  
  # prepare data for CFA
  cog_data <- dplyr::select(cog_data, ID, all_of(bl_vars))
  names(cog_data) <- c("ID", cog_vars)
  names(cog_data)[names(cog_data)=="3MS_OverallScore_C"] <- 
    "MS_OverallScore_C"
  
  # all tests available 
  cog_data <- cog_data |> 
    filter(!is.na(MS_OverallScore_C) & 
             !is.na(HVLT_TotalRecall) & 
             !is.na(HVLT_DelayedRecall) &
             !is.na(HVLT_Retention) & 
             !is.na(HVLT_RDI) & 
             !is.na(COWAT) & 
             !is.na(SDMT))
  
  ## CFA model
  
  bl_model <- "
# loadings
g =~ MS_OverallScore_C + HVLT_TotalRecall + HVLT_Retention + COWAT + 
HVLT_RDI + SDMT + HVLT_DelayedRecall

# intercepts
MS_OverallScore_C ~ 1
HVLT_TotalRecall ~ 1
HVLT_Retention ~ 1
COWAT ~ 1
HVLT_RDI ~ 1
SDMT ~ 1
HVLT_DelayedRecall ~ 1
g ~ 0*1

# variances
g ~~ 1*g

# covariances
HVLT_TotalRecall ~~ HVLT_Retention
HVLT_TotalRecall ~~ HVLT_RDI
HVLT_TotalRecall ~~ HVLT_DelayedRecall
HVLT_RDI ~~ HVLT_Retention
HVLT_RDI ~~ HVLT_DelayedRecall
HVLT_Retention ~~ HVLT_DelayedRecall
"
  ## Fit CFA model 
  
  cfa_out <- cfa(bl_model, 
                 data = cog_data, 
                 auto.fix.first = FALSE)
  
  bl_coefs <- coef(cfa_out)
  
  # Extract factors scores
  
  cog_data$BL_g <- 
    lavPredict(cfa_out, method = "Regression", transform = TRUE) |> 
    as.numeric()
  
  bl_out <- dplyr::select(cog_data, ID, BL_g)
  
  ### Follow-up CFA
  
  fu_cfa <- function(visit){
    vars <- paste(visit, cog_vars, sep = "_")
    
    # truncate extreme values at 99th percentile
    cog_data <- data |> mutate(across(all_of(vars), truncate))
    
    # prepare data for CFA
    cog_data <- dplyr::select(cog_data, ID, all_of(vars))
    names(cog_data) <- c("ID", cog_vars)
    names(cog_data)[names(cog_data)=="3MS_OverallScore_C"] <- 
      "MS_OverallScore_C"
    
    # at least one cog test available 
    cog_data <- cog_data |> 
      filter(!is.na(MS_OverallScore_C) & 
               !is.na(HVLT_TotalRecall) & 
               !is.na(HVLT_Retention) & 
               !is.na(HVLT_DelayedRecall) & 
               !is.na(COWAT) &
               !is.na(HVLT_RDI) & 
               !is.na(SDMT))
    
    ## CFA model
    
    fu_model <- 
    paste("g =~ ", bl_coefs[1], "* MS_OverallScore_C +",
          bl_coefs[2], "* HVLT_TotalRecall +", 
          bl_coefs[3], "* HVLT_Retention +",
          bl_coefs[4], "* COWAT +",
          bl_coefs[5], "* HVLT_RDI +",
          bl_coefs[6], "* SDMT +",
          bl_coefs[7], "* HVLT_DelayedRecall \n MS_OverallScore_C ~", 
          bl_coefs[8], "* 1 \n", 
          "HVLT_TotalRecall ~", bl_coefs[9], "* 1 \n",
          "HVLT_Retention ~", bl_coefs[10], "* 1 \n",
          "COWAT ~", bl_coefs[11], "* 1 \n",
          "HVLT_RDI ~", bl_coefs[12], "* 1 \n",
          "SDMT ~", bl_coefs[13], "* 1 \n",
          "HVLT_DelayedRecall ~", bl_coefs[14], "* 1 \n",
          "g ~ NA * 1 \n",
          "g ~~ NA*g \n",
          "HVLT_TotalRecall ~~", bl_coefs[15], "* HVLT_Retention \n",
          "HVLT_TotalRecall ~~", bl_coefs[16], "* HVLT_RDI \n",
          "HVLT_TotalRecall ~~", bl_coefs[17], "* HVLT_DelayedRecall \n",
          "HVLT_RDI ~~", bl_coefs[18], "* HVLT_Retention \n",
          "HVLT_RDI ~~", bl_coefs[19], "* HVLT_DelayedRecall \n",
          "HVLT_Retention ~~", bl_coefs[20], "* HVLT_DelayedRecall \n",
          "MS_OverallScore_C ~~", bl_coefs[21], "* MS_OverallScore_C \n",
          "HVLT_TotalRecall ~~", bl_coefs[22], "* HVLT_TotalRecall \n",
          "HVLT_Retention ~~", bl_coefs[23], "* HVLT_Retention \n",
          "COWAT ~~", bl_coefs[24], "* COWAT \n",
          "HVLT_RDI ~~", bl_coefs[25], "* HVLT_RDI \n",
          "SDMT ~~", bl_coefs[26], "* SDMT \n",
          "HVLT_DelayedRecall ~~", bl_coefs[27], "* HVLT_DelayedRecall")
    
    cfa_out <- cfa(fu_model, 
                   data = cog_data, 
                   auto.fix.first = FALSE)
    
    ## Extract factors scores
    
    cog_data[[paste(visit, "g", sep = "_")]] <- 
      lavPredict(cfa_out, method = "Regression", transform = TRUE) |> 
      as.numeric()
    
    return(cog_data |> dplyr::select(ID, contains("_g")))
  }
  
  out <- 
    map(c("AV3","AV4", "AV5", "AV6","AV7", "AV8", "AV9", "AV10"), 
        fu_cfa) |> 
    reduce(full_join, by = "ID")
  
  out <- full_join(out, bl_out, by = "ID")
  
  return(out)
  
}
