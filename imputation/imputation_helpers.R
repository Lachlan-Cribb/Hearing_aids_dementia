## Single imputation of variables with few (<10) missing

single_imputation <- function(data){
  
  # randomly fill missing in variables with <10 missing obs
  
  data[is.na(data$Edu), "Edu"] <- 
    sample(data$Edu, 1, replace = TRUE)
  
  data[is.na(data$BL_AlcWk), "BL_AlcWk"] <- 
    sample(data$BL_AlcWk, 1, replace = TRUE)
  
  data[is.na(data$BL_CesdOverall), "BL_CesdOverall"] <- 
    sample(data$BL_CesdOverall, 1, replace = TRUE)
  
  data[is.na(data$BL_MCS), "BL_MCS"] <- 
    sample(data$BL_MCS, 1, replace = TRUE)
  
  data[is.na(data$BL_PCS), "BL_PCS"] <- 
    sample(data$BL_PCS, 1, replace = TRUE)
  
  data[is.na(data$Racial), "Racial"] <- 
    sample(data$Racial, 1, replace = TRUE)

  return(data)
}

## Multiple imputation

imputation <- function(data, m, imp_name, complete_name){
  
  ## Remove variables after AV10
  
  data <- data |> 
    select(-contains("AV11"), -contains("AV12"), -contains("AV13"))
  
  ## First add derived variables to data (interactions and squared terms)
  
  # quadratic terms
  
  quad_terms <- 
    select(data, 
           where(is.numeric))[,apply(select(data, where(is.numeric)), 2, 
                                     function(x) 
                                       length(unique(x[!is.na(x)])) > 3)] |> 
    select(-contains("AV"), -contains("DSR")) |> 
    names()
  
  quad_terms <- c(quad_terms, c("BLToneAvg_Better", "Y3MToneAvg_Better"))
  
  # interaction terms
  
  interaction_vars <- c(
    "Y3M_HearingAidUse",
    "Y3M_HearingAid",
    "HearingProbs",
    "Edu",
    "Gender",
    "AgeAtRand",
    "BLToneAvg_Better",
    "QuietRm",
    "BL_CesdOverall",
    "AV5_CesdOverall",
    "Income",
    "BL_3MS_OverallScore_C",
    "AV3_3MS_OverallScore_C",
    "AV7_3MS_OverallScore_C",
    "BL_HVLT_DelayedRecall",
    "AV3_HVLT_DelayedRecall",
    "AV7_HVLT_DelayedRecall",
    "BL_Frailty_DAI50",
    "AV5_Frailty_DAI50",
    "apoe_e4"
  )
  
  interaction_terms <- 
    gtools::combinations(n = length(interaction_vars),
                         r = 2,
                         v = interaction_vars,
                         repeats.allowed = FALSE) |> 
    as.data.frame()
  
  interaction_terms <- interaction_terms |>
    filter(!(V1 == "Y3M_HearingAid" &
               V2 == "Y3M_HearingAidUse"))

  interaction_terms <- paste(interaction_terms$V1, interaction_terms$V2,
                             sep = ":")
  
  # Add derived terms to data 
  
  derived <- 
    add_derived(quadratic_terms = quad_terms,
                interaction_terms = interaction_terms,
                data = data)
  
  data <- bind_cols(data, derived$model_df)
  
  interaction_terms <- derived$int_terms
  
  # incomplete variables 
  
  incomplete <- data[,which(colSums(is.na(data)) > 0)]
  
  ### Construct predictor matrix 
  
  baseline_vars <-
    incomplete |>
    select(
      -contains("AV1"),
      -contains("AV2"),-contains("AV3"),
      -contains("AV4"),-contains("AV5"),
      -contains("AV6"),-contains("AV7"),
      -contains("AV8"),-contains("AV9"),
      -contains("AV10"),-contains("Death"),
      -contains("Dem"),-contains("Cancer"),
      -contains("Disability"), -contains("CVD"),
      -contains("DSR"),
      -any_of(names(derived$model_df))
    ) |>
    names()
  
  # event follow-up variables 
  
  event_vars <- data |> 
    select(contains("CVD"), contains("Dem"), contains("Cancer"), 
           contains("Disability")) |> 
    select(contains("AV"), -any_of(names(derived))) |> 
    names()
  
  miss_event_vars <- incomplete |> 
    select(contains("CVD"), contains("Dem"), contains("Cancer"), 
           contains("Disability")) |> 
    select(contains("AV"), -any_of(names(derived))) |> 
    names()
  
  # death vars
  
  death_vars <- data |> 
    select(contains("Death")) |> select(contains("AV")) |> 
    names()
  
  # non-event follow-up variables
  
  av_vars <- incomplete |> 
    select(-all_of(baseline_vars), -any_of(event_vars),
           -any_of(death_vars)) |> 
    select(contains("AV"), -any_of(names(derived))) |> 
    names()
  
  av1_vars <- av_vars[str_detect(av_vars, "AV1_")]
  av2_vars <- av_vars[str_detect(av_vars, "AV2")]
  av3_vars <- av_vars[str_detect(av_vars, "AV3")]
  av4_vars <- av_vars[str_detect(av_vars, "AV4")]
  av5_vars <- av_vars[str_detect(av_vars, "AV5")]
  av6_vars <- av_vars[str_detect(av_vars, "AV6")]
  av7_vars <- av_vars[str_detect(av_vars, "AV7")]
  av8_vars <- av_vars[str_detect(av_vars, "AV8")]
  av9_vars <- av_vars[str_detect(av_vars, "AV9")]
  av10_vars <- av_vars[str_detect(av_vars, "AV10")]
  
  # event and DSR variables 
  
  dsr_vars <- select(data, Cancer, Dementia, Death, CVD_protocol,
                     Disability, contains("DSR")) |> names()
  
  # complete baseline variables
  
  complete_vars <- data[,which(colSums(is.na(data)) == 0)] |> 
    select(-any_of(dsr_vars), -any_of(event_vars), 
           -Safehaven, -any_of(death_vars)) |> 
    names()
  
  ### Create predictor matrix 
  
  predmat <- make.predictorMatrix(data)
  predmat[,] <- 0 # set all to zero
  
  ## baseline vars 
  predmat[baseline_vars,] <- 1 # use everything
  predmat[baseline_vars, dsr_vars] <- 0
  predmat[baseline_vars, "Safehaven"] <- 0
  predmat["HearingAid", "HearingAidUse"] <- 0
  predmat["HearingAidUse", "HearingAid"] <- 0
  predmat["Y3M_HearingAid", "Y3M_HearingAidUse"] <- 0
  predmat["Y3M_HearingAidUse", "Y3M_HearingAid"] <- 0
  predmat["BLToneAvg_Better",
          str_detect(colnames(predmat), "AV7|AV8|AV9|AV10")] <- 0
  
  ## longitudinal follow-up vars (e.g., cognition)
  predmat[av_vars, baseline_vars] <- 1
  predmat[av_vars, av_vars] <- 1
  predmat[av_vars, names(derived$model_df)] <- 1
  predmat[av_vars, complete_vars] <- 1
  predmat[av_vars, event_vars] <- 1
  predmat[av_vars, death_vars] <- 1
  predmat[av4_vars, "Death_AV4"] <- 0
  predmat[av5_vars, c("Death_AV4", "Death_AV5")] <- 0
  predmat[av6_vars, c("Death_AV4", "Death_AV5", "Death_AV6")] <- 0
  predmat[av7_vars, paste("Death_AV", 4:7, sep = "")] <- 0
  predmat[av8_vars, paste("Death_AV", 4:8, sep = "")] <- 0
  predmat[av9_vars, paste("Death_AV", 4:9, sep = "")] <- 0
  predmat[av10_vars, paste("Death_AV", 4:10, sep = "")] <- 0
  
  # use event variables from only 2 previous visits
  predmat[av4_vars,
          event_vars[str_ends(event_vars, "AV1")]] <- 0
  predmat[av5_vars,
          event_vars[str_ends(event_vars, "AV1|AV2")]] <- 0
  predmat[av6_vars,
          event_vars[str_ends(event_vars, "AV1|AV2|AV3")]] <- 0
  predmat[av7_vars,
          event_vars[str_ends(event_vars, "AV1|AV2|AV3|AV4")]] <- 0
  predmat[av8_vars,
          event_vars[str_ends(event_vars, "AV1|AV2|AV3|AV4|AV5")]] <- 0
  predmat[av9_vars,
          event_vars[str_ends(event_vars, "AV1|AV2|AV3|AV4|AV5|AV6")]] <- 0
  predmat[av10_vars,
          event_vars[str_ends(event_vars, "AV1|AV2|AV3|AV4|AV5|AV6|AV7")]] <- 0

  # don't use AV9,10 variables to predict AV4 cognition (not available)
  predmat[c("AV4_COWAT", "AV4_HVLT_TotalRecall",
            "AV4_HVLT_Retention", "AV4_HVLT_RDI",
            "AV4_SDMT", "AV4_3MS_OverallScore_C"),
          av_vars[str_detect(av_vars, "AV9|AV10")]] <- 0
  
  ## longitudinal event variables
  predmat[miss_event_vars, baseline_vars] <- 1
  predmat[miss_event_vars, complete_vars] <- 1
  
  # CVD
  
  for (i in 2:10) {
    predmat[paste("CVD_AV", i, sep = ""), 
            c(paste("Dem_AV", i-1, sep = ""),paste("Cancer_AV", i-1, sep = ""),
              paste("Disability_AV", i-1, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-1, sep = ""))]
    predmat[paste("CVD_AV", i, sep = ""), av_varsi] <- 1
    
  }
  
  for (i in 3:10) {
    predmat[paste("CVD_AV", i, sep = ""), 
            c(paste("Dem_AV", i-2, sep = ""),paste("Cancer_AV", i-2, sep = ""),
              paste("Disability_AV", i-2, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-2, sep = ""))]
    predmat[paste("CVD_AV", i, sep = ""), av_varsi] <- 1
  }
  
  # Dem 
  
  for (i in 2:10) {
    predmat[paste("Dem_AV", i, sep = ""), 
            c(paste("CVD_AV", i-1, sep = ""),paste("Cancer_AV", i-1, sep = ""),
              paste("Disability_AV", i-1, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-1, sep = ""))]
    predmat[paste("Dem_AV", i, sep = ""), av_varsi] <- 1
  }
  
  for (i in 3:10) {
    predmat[paste("Dem_AV", i, sep = ""), 
            c(paste("CVD_AV", i-2, sep = ""),paste("Cancer_AV", i-2, sep = ""),
              paste("Disability_AV", i-2, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-2, sep = ""))]
    predmat[paste("Dem_AV", i, sep = ""), av_varsi] <- 1
  }
  
  # Cancer 

  for (i in 2:10) {
    predmat[paste("Cancer_AV", i, sep = ""), 
            c(paste("CVD_AV", i-1, sep = ""),paste("Dem_AV", i-1, sep = ""),
              paste("Disability_AV", i-1, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-1, sep = ""))]
    predmat[paste("Cancer_AV", i, sep = ""), av_varsi] <- 1
  }
  
  for (i in 3:10) {
    predmat[paste("Cancer_AV", i, sep = ""), 
            c(paste("CVD_AV", i-2, sep = ""),paste("Dem_AV", i-2, sep = ""),
              paste("Disability_AV", i-2, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-2, sep = ""))]
    predmat[paste("Cancer_AV", i, sep = ""), av_varsi] <- 1
  }
  
  # Disability
  
  for (i in 2:10) {
    predmat[paste("Disability_AV", i, sep = ""), 
            c(paste("CVD_AV", i-1, sep = ""),paste("Dem_AV", i-1, sep = ""),
              paste("Cancer_AV", i-1, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-1, sep = ""))]
    predmat[paste("Disability_AV", i, sep = ""), av_varsi] <- 1
  }
  
  for (i in 3:10) {
    predmat[paste("Disability_AV", i, sep = ""), 
            c(paste("CVD_AV", i-2, sep = ""),paste("Dem_AV", i-2, sep = ""),
              paste("Cancer_AV", i-2, sep = ""))] <- 1
    
    av_varsi <- av_vars[str_detect(av_vars, paste("AV", i-2, sep = ""))]
    predmat[paste("Disability_AV", i, sep = ""), av_varsi] <- 1
  }
  
  ## use baseline complete for everything else 
  
  predmat[,complete_vars] <- 1
  
  ## Don't use derived(x) to predict x 
  
  for(i in rownames(predmat)){
    predmat[i, str_detect(colnames(predmat), i)] <- 0
  }
  
  # don't use AV4 cognitive variables to predict (too many missing)
  predmat[,c("AV4_COWAT", "AV4_HVLT_TotalRecall",
            "AV4_HVLT_Retention", "AV4_HVLT_RDI",
            "AV4_SDMT", "AV4_3MS_OverallScore_C")] <- 0
  
  # don't use Visual limit
  
  predmat[,"BL_VisualLimit"] <- 0
  
  ### Vector of imputation methods 
  
  methods <- make.method(data)
  
  missing_methods <- methods == "pmm"
  
  miss_vec <- methods[missing_methods]
  
  # quadratic terms 
  
  quadratic_terms <- quad_terms[quad_terms %in% names(miss_vec)]
  
  quad_methods <- 
    paste("~I(", quadratic_terms, " * ", quadratic_terms, ")", sep = "")
  
  methods[paste(quadratic_terms, "2", sep= "")] <- quad_methods
  
  # interaction terms 
  
  int_terms <- interaction_terms[interaction_terms %in% names(miss_vec)]
  
  interaction_methods <- str_split_fixed(int_terms, "_by_", 2)
  interaction_methods <- paste("~I(",
                               interaction_methods[,1],
                               " * ",
                               interaction_methods[,2],
                               ")", 
                               sep = "")
  
  methods[int_terms] <- interaction_methods
  
  methods[methods=="pmm"] <- "elasticnet.pmm"

  ## imputation 
  
  imp_out <- 
    mice(data,
         m = m,
         method = methods,
         predictorMatrix = predmat,
         maxit = 10)
  
  # write mice output to disk
  
  write_rds(imp_out, 
            here("imputed_data",
                 "XT06",
                 "mice_output",
                 imp_name))
  
  ### Extract complete variables 
  
  imputed_data <- 
    map(complete(imp_out, "all"), extract_complete, raw_data = data)
  
  ## Remove interactions and non-linear terms from dataset
  
  quad_terms <- paste(quad_terms, "2", sep= "")
  
  remove_derived <- function(data){
    data <- data |> 
      select(-all_of(quad_terms), -contains("_by_"))
    return(data)
  }
  
  imputed_data <- map(imputed_data, remove_derived)
  
  write_rds(imputed_data, 
            here("imputed_data",
                 "XT06",
                 "imputed_datasets",
                 complete_name))
  
}


## extract complete variables and add to original data after imputation 

extract_complete <- function(raw_data, imp_data){
  
  # complete confounders 
  
  complete_vars <-
    c("apoe_e4",
      "HearingAid",
      "HearingAidUse",
      "Y3M_HearingAidUse",
      "Y3M_HearingAid",
      "EyesRate",
      "HearingProbs",
      "HearingProbs_2",
      "CochlearImplnt",
      "Buzz",
      "QuietRm",
      "CrowdedRm",
      "SupplementUse",
      "HrsSleep",
      "CommunityEngagement",
      "LeisureActivities",
      "PresPhysAct",
      "Alone",
      "SocialEngagement",
      "Income",
      "Racial",
      "BL_CesdOverall",
      "BL_Polypharmacy",
      "hypstatus_BLNo_untreated",
      "hypstatus_BLYes_treated",
      "hypstatus_BLYes_untreated",
      "BL_Frailty_DAI50",
      "BLToneAvg_Better",
      "Y3MToneAvg_Better",
      "Pt_Cr",
      "DAB",
      "BL_VisualLimit",
      "BL_BMI",
      "BL_SmHis2",
      "BL_SmHis3",
      "BL_AlcWk",
      "BL_MCS",
      "BL_PCS",
      "Pt_scoreIRSAD"
    )
  
  complete_data <- 
    imp_data |> 
    select(Safehaven, 
           all_of(complete_vars), 
           contains("Edu"),
           contains("COWAT"),
           contains("HVLT"),
           contains("SDMT"),
           contains("3MS"), 
           contains("PCS"),
           contains("MCS"),
           contains("Frailty"),
           contains("Polypharmacy"),
           contains("CesdOverall"))
  
  complete_vars <- names(complete_data)
  
  raw_data[,complete_vars] <- complete_data[,complete_vars]
  
  return(raw_data)
  
}

## derived variables for imputation
add_derived <- 
  function(quadratic_terms, interaction_terms, data){
    
    int_formula <- 
      paste(interaction_terms, collapse = " + ")
    
    quad_formula <- 
      paste(paste0("I(", quadratic_terms, "^2)"), collapse = " + ")
    
    model_formula <- 
      paste("~", paste(int_formula, quad_formula, sep = " + ")) |> 
      as.formula()
    
    model_df <- 
      model.matrix(model_formula,
                   model.frame(
                     model_formula, data = data, na.action = NULL))[,-1] |> 
      as_tibble()
    
    names(model_df) <- str_replace_all(names(model_df), "I\\(", "")
    
    names(model_df) <- str_replace_all(names(model_df), "\\^2", "2")
    
    names(model_df) <- str_replace_all(names(model_df), "\\)", "")
    
    names(model_df) <- str_replace_all(names(model_df), "\\:", "_by_")
    
    # remove derived variables that already exist in dataset 
    
    model_df <- model_df[,!names(model_df) %in% names(df)]
    
    quadratic_terms <- paste(quadratic_terms, "2", sep = "")
    
    interaction_terms <- model_df |> 
      dplyr::select(-all_of(quadratic_terms)) |> 
      names()
    
    return(list(model_df = model_df, 
                quad_terms = quadratic_terms,
                int_terms = interaction_terms
    ))
  }

