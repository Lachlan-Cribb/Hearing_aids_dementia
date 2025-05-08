# load packages
library(tidyverse)
library(here)
library(readxl)
library(psych)
library(haven)

## Load utility functions and imputation functions (for single imputation)

source(here("..", "..", "utils", "SF12_scoring.R"))

## Function to prepare data

prepare_data <- function(output_name) {
  # read ALSOP medical baseline data
  
  alsop_bl <-
    read_csv(
      here(
        "..",
        "..",
        "Data",
        "ALSOP Baseline Data Set",
        "ALSOP Baseline Medical",
        "ALSOP_MedicalBaseline_DataSet_SafehavenID_V3.csv"
      )
    )
  
  alsop_bl <- rename(alsop_bl, Safehaven = SAFEID)
  
  # read ALSOP medical 3 year data
  
  alsop_3y <-
    read_excel(
      here(
        "..",
        "..",
        "Data",
        "ALSOP Year 3",
        "ticket_5272_CC_ALSOP Medical Year 3 Dataset_22.04.2020_SafeHavenID.xlsx"
      )
    )
  
  # Read baseline ALSOP social
  
  alsop_social <-
    read_csv(
      here(
        "..",
        "..",
        "Data",
        "ALSOP Baseline Data Set",
        "ALSOP Baseline Social",
        "CSV",
        "ALSOP_SocialBaseline_CompleteDataSet_SafehavenID.csv"
      )
    )
  
  ## ALSOP Days since randomisation
  
  dsr <- read_csv(here("..", "..", "Data", "ALSOP_DSR.csv"))
  
  # merge datasets
  
  alsop <- left_join(alsop_bl, alsop_3y, by = "Safehaven")
  
  alsop <- left_join(alsop, alsop_social, by = "Safehaven")
  
  alsop <- left_join(alsop, dsr, by = "Safehaven")
  
  ### Create new variables
  
  # Sum score for community activities
  
  alsop$CommunityEngagement <-
    rowSums(alsop |>
              select(SmallGrp:SocEnt) |>
              select(-starts_with("C_")))
  
  # Sum score for social engagement
  
  alsop$SocialEngagement <-
    rowSums(alsop |>
              select(RelHearFrm:FrndHelp) |>
              select(-starts_with("C_")))
  
  # First PC of leisure activities
  
  alsop$LeisureActivities <-
    principal(alsop |>
                select(WatchTV:Cook) |>
                select(-starts_with("C_")),
              nfactors = 1)$scores |>
    as.numeric()
  
  # Supplement use
  
  alsop$SupplementUse <- rowSums(alsop |> select(FishOil:OtherHerb))
  
  ## Hearing aid use - make 1 (never) if never prescribed
  
  alsop <- alsop |>
    mutate(HearingAidUse = ifelse(HearingAid == 0 &
                                    (HearingAidUse < 2 | is.na(HearingAidUse)), 
                                  1, HearingAidUse))
  
  alsop <- alsop |>
    mutate(Y3M_HearingAidUse = ifelse(
      Y3M_HearingAid == 0 &
        (Y3M_HearingAidUse < 2 | is.na(Y3M_HearingAidUse)),
      1,
      Y3M_HearingAidUse
    ))
  
  ## Hearing aids - make 1 (prescribed) if missing & hearingaiduse > never
  
  alsop$HearingAid <-
    ifelse(alsop$HearingAidUse > 1, 1, alsop$HearingAid)
  
  alsop$Y3M_HearingAid <-
    ifelse(alsop$Y3M_HearingAidUse > 1, 1, alsop$Y3M_HearingAid)
  
  ## Cochlear implant - collapse one ear and both ears
  
  alsop$CochlearImplnt <-
    if_else(alsop$CochlearImplnt == 2, 1, alsop$CochlearImplnt)
  
  ## Select required ALSOP variables
  
  alsop_vars <-
    select(
      alsop,
      Safehaven,
      BM_ALSOP_DSR,
      BS_ALSOP_DSR,
      Y3M_ALSOP_DSR,
      C_EyesRate,
      EyesRate,
      C_HearingProbs,
      HearingProbs,
      C_HearingProbs_2,
      HearingProbs_2,
      C_CochlearImplnt,
      CochlearImplnt,
      C_HearingAid,
      HearingAid,
      C_HearingAidUse,
      HearingAidUse,
      C_Buzz,
      Buzz,
      C_QuietRm,
      QuietRm,
      C_CrowdedRm,
      CrowdedRm,
      C_Y3M_HearingProbs,
      Y3M_HearingProbs,
      C_Y3M_HearingProbs_1,
      Y3M_HearingProbs_1,
      C_Y3M_HearingAid,
      Y3M_HearingAid,
      C_Y3M_HearingAidUse,
      Y3M_HearingAidUse,
      C_Y3M_Buzz,
      Y3M_Buzz,
      C_Y3M_QuietRm_1,
      Y3M_QuietRm_1,
      C_Y3M_QuietRm_2,
      Y3M_QuietRm_2,
      C_Y3M_CrowdedRm,
      Y3M_CrowdedRm,
      SupplementUse,
      C_HrsSleep,
      HrsSleep,
      C_HrsSleep,
      HrsSleep,
      CommunityEngagement,
      LeisureActivities,
      C_PresPhysAct,
      PresPhysAct,
      C_MidPhysAct,
      MidPhysAct,
      Alone,
      SocialEngagement,
      C_Income,
      Income
    )
  
  #### Derived variables ####
  
  derived <- read_csv(
    here(
      "..",
      "..",
      "shared",
      "Resource-XT04",
      "CSV Version",
      "SectionI1_DerivedBaselineVariables_XT04_v1_SafeHavenID.csv"
    )
  )
  
  derived_vars <-
    derived |>
    select(Safehaven, Pt_scoreIRSAD)
  
  #### Hearing substudy data ####
  
  # Read hearing data
  
  hear <- read_excel(
    here(
      "..",
      "..",
      "Data",
      "Baseline_Hearingv2",
      "Safehaven",
      "Baseline_Hearing_SectionA_v2.xlsx"
    )
  )
  
  hear2 <- read_csv(
    here(
      "..",
      "..",
      "Data",
      "Longitudinal_Hearing",
      "Safehaven",
      "SectionAA Dataset_Safehaven.csv"
    )
  )
  
  hear <- full_join(hear, hear2, by = "Safehaven")
  
  ## select hearing vars ##
  
  hear$BLToneAvg_Better <- ifelse(
    hear$BLToneAvg_Left < hear$BLToneAvg_Right,
    hear$BLToneAvg_Left,
    hear$BLToneAvg_Right
  )
  
  hear$Y3MToneAvg_Better <- ifelse(
    hear$HV2ToneAvg_Left < hear$HV2ToneAvg_Right,
    hear$HV2ToneAvg_Left,
    hear$HV2ToneAvg_Right
  )
  
  hear_vars <-
    select(hear, Safehaven, BLToneAvg_Better, Y3MToneAvg_Better)
  
  #### ASPREE data ####
  
  ### Gen demo
  
  demo <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionA1_GenDemo_XT06_v1_SafeHavenID.csv"
    )
  )
  
  # Collapse racial categories
  
  demo$Racial <- ifelse(demo$Racial == 1, 0, 1)
  
  demo_vars <-
    select(
      demo,
      Safehaven,
      C_AgeAtRand,
      AgeAtRand,
      C_Gender,
      Gender,
      C_Racial,
      Racial,
      C_Edu,
      Edu
    )
  
  ### Alcohol and smoking
  
  alc <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionA2_AlcSmHis_XT06_v1_SafeHavenID.csv"
    )
  )
  
  # create typical weekly alcohol variable
  
  alc$BL_AlcWk <-
    with(alc, ifelse(BL_Alc %in% c(1, 2), BL_AlcDays * BL_AlcNo, 0))
  
  alc_vars <-
    select(alc, Safehaven, C_BL_SmHis, BL_SmHis, BL_AlcWk)
  
  ### Baseline medical history
  
  history <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionB1_MedicalHistory_XT06_v1_SafeHavenID.csv"
    )
  )
  
  # cancer, diabetes
  
  history$DAB <- ifelse(history$DAB == 1, 1, 0)
  
  history$Pt_Cr <- ifelse(history$Pt_Cr == 1, 1, 0)
  
  history_vars <- history |>
    select(Safehaven, C_Pt_Cr, Pt_Cr, C_DAB, DAB)
  
  ### Longitudinal physical assessments
  
  # BMI and BP
  
  phys <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionB2_PhysicalExam_XT06_v1_SafeHavenID.csv"
    )
  )
  
  # create hypertension variable
  
  hypertensive <- function(visit) {
    hbp <- ifelse(phys[[paste(visit, "SBP_Mean", sep = "_")]] > 130 |
                    phys[[paste(visit, "DBP_Mean", sep = "_")]] > 80, 1, 0)
    return(hbp)
  }
  
  hbp <- map(c("BL", paste("AV", 1:11, sep = "")), hypertensive) |>
    bind_cols()
  
  names(hbp) <- c("BL_HBP", paste("AV", 1:11, "_HBP", sep = ""))
  
  phys <- bind_cols(phys, hbp)
  
  # phys vars
  
  phys_vars <- select(phys, Safehaven, BL_BMI, contains("_HBP"))
  
  
  ### antihypertensive use
  
  conmeds <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT04",
      "CSV Version",
      "SectionB5_ConMeds_XT04_v1_SafeHavenID.csv"
    )
  )
  
  antihypertensive <- function(vars, visit, data) {
    ids <- data |>
      filter(str_detect(ATC, "^C02")) |>
      filter(if_any(vars, ~ . == 1)) |>
      pull(Safehaven)
    
    antihypertensive_use[[paste(visit, "Antihyp", sep = "_")]] <-
      ifelse(demo$Safehaven %in% ids, 1, 0)
    
    return(antihypertensive_use)
  }
  
  antihypertensive_use <- demo |> select(Safehaven)
  
  antihypertensive_use <- antihypertensive("Baseline_MedPres", "BL", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("BL_MedPres", "AV1_MedPres", "AV1_MedOn"), "AV1", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("AV2_MedPres", "AV2_MedOn"), "AV2", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("AV3_MedPres", "AV3_MedOn"), "AV3", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("AV4_MedPres", "AV4_MedOn"), "AV4", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("AV5_MedPres", "AV5_MedOn"), "AV5", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("AV6_MedPres", "AV6_MedOn"), "AV6", conmeds)
  
  antihypertensive_use <-
    antihypertensive(c("AV7_MedPres", "AV7_MedOn"), "AV7", conmeds)
  
  antihypertensive_use <- antihypertensive(c("AV8_MedOn"), "AV8", conmeds)
  
  antihypertensive_use <- antihypertensive(c("AV9_MedOn"), "AV9", conmeds)
  
  antihypertensive_use <- antihypertensive(c("AV10_MedOn"), "AV10", conmeds)
  
  antihypertensive_use <- antihypertensive(c("AV11_MedOn"), "AV11", conmeds)
  
  
  ### Polypharmacy
  
  get_polypharm <- function(vars, visit, data) {
    out <- data |>
      filter(if_any(vars, ~ . == 1)) |>
      group_by(Safehaven) |>
      tally(name = paste(visit, "Polypharmacy", sep = "_"))
    
    out <- left_join(polypharm, out, by = "Safehaven")
    
    return(out)
  }
  
  polypharm <- demo |> select(Safehaven)
  
  polypharm <- get_polypharm(c("Baseline_MedPres"), "BL", conmeds)
  
  polypharm$BL_Polypharmacy <-
    ifelse(is.na(polypharm$BL_Polypharmacy),
           0,
           polypharm$BL_Polypharmacy)
  
  polypharm <-
    get_polypharm(c("BL_MedPres", "AV1_MedPres", "AV1_MedOn"), "AV1", conmeds)
  
  polypharm <-
    get_polypharm(c("AV2_MedPres", "AV2_MedOn"), "AV2", conmeds)
  
  polypharm <-
    get_polypharm(c("AV3_MedPres", "AV3_MedOn"), "AV3", conmeds)
  
  polypharm <-
    get_polypharm(c("AV4_MedPres", "AV4_MedOn"), "AV4", conmeds)
  
  polypharm <-
    get_polypharm(c("AV5_MedPres", "AV5_MedOn"), "AV5", conmeds)
  
  polypharm <-
    get_polypharm(c("AV6_MedPres", "AV6_MedOn"), "AV6", conmeds)
  
  polypharm <-
    get_polypharm(c("AV7_MedPres", "AV7_MedOn"), "AV7", conmeds)
  
  polypharm <-
    get_polypharm(c("AV8_MedOn"), "AV8", conmeds)
  
  polypharm <-
    get_polypharm(c("AV9_MedOn"), "AV9", conmeds)
  
  polypharm <-
    get_polypharm(c("AV10_MedOn"), "AV10", conmeds)
  
  polypharm <-
    get_polypharm(c("AV11_MedOn"), "AV11", conmeds)
  
  # med vars
  
  conmed_vars <- full_join(antihypertensive_use, polypharm, by = "Safehaven")
  
  
  ### Hypertensive status
  # Hypertensive treated, hypertensive untreated, etc.
  
  hyp <- conmed_vars %>% left_join(phys_vars, by = "Safehaven")
  
  hyp_status <- function(data, visit) {
    case_when(
      data[[paste0(visit, "_Antihyp")]] == 0 &
        data[[paste0(visit, "_HBP")]] == 0 ~ "No_untreated",
      data[[paste0(visit, "_Antihyp")]] == 1 &
        data[[paste0(visit, "_HBP")]] == 0 ~ "No_treated",
      data[[paste0(visit, "_Antihyp")]] == 1 &
        data[[paste0(visit, "_HBP")]] == 1 ~ "Yes_treated",
      data[[paste0(visit, "_Antihyp")]] == 0 &
        data[[paste0(visit, "_HBP")]] == 1 ~ "Yes_untreated"
    )
  }
  
  hyp$hypstatus_BL <- hyp_status(hyp, "BL")
  
  hyp_vars <- hyp %>% select(Safehaven, hypstatus_BL)
  
  #### Cognitive outcomes ####
  
  ### 3MS
  
  ms <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionC1_3MS_XT06_v1_SafeHavenID.csv"
    )
  )
  
  # visutal limit
  
  ms$BL_VisualLimit <- ifelse(!ms$BL_3MS_Limit_Type %in% c(1, 4, 5, 7) |
                                is.na(ms$BL_3MS_Limit_Type),
                              0,
                              1)
  
  ## transform phone 3MS to be on same scale with
  ## regular 3MS, and then amalgamate
  
  transform_ms <- function(visit, data) {
    data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]] <-
      data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]] / 0.74
    
    out <-
      ifelse(is.na(data[[paste(visit, "3MS_OverallScore", sep = "_")]]) &
               !is.na(data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]]), 
             data[[paste(visit, "3MS_OverallScore_Ph", sep = "_")]], 
             data[[paste(visit, "3MS_OverallScore", sep = "_")]])
    return(out)
  }
  
  ms_transformed <-
    map(c("BL", paste("AV", 1:11, sep = "")), transform_ms, data = ms) |>
    bind_cols()
  
  names(ms_transformed) <-
    paste(c("BL", paste("AV", 1:11, sep = "")), "3MS_OverallScore_C", sep = "_")
  
  ms <- bind_cols(ms, ms_transformed)
  
  ms_vars <- select(ms, Safehaven, BL_VisualLimit, contains("OverallScore_C"))
  
  ### Other cognitive tests
  
  cog <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionC2_OtherCogs_XT06_v1_SafeHavenID.csv"
    )
  )
  
  ## HVLT
  
  get_hvlt <- function(visit, data) {
    data <- as_tibble(data)
    
    HVLT_TotalRecall <-
      rowSums(data[, paste(visit, c("HVLT1", "HVLT2", "HVLT3"), sep = "_")])
    
    HVLT_DelayedRecall <- data[[paste0(visit, "_HVLT4")]]
    
    Ret1 <- data[[paste(visit, "HVLT4", sep = "_")]] /
      data[[paste(visit, "HVLT2", sep = "_")]] * 100
    
    Ret2 <- data[[paste(visit, "HVLT4", sep = "_")]] /
      data[[paste(visit, "HVLT3", sep = "_")]] * 100
    
    Ret <- cbind(Ret1, Ret2)
    
    HVLT_Retention <- apply(Ret, 1, max)
    
    HVLT_Retention <- ifelse(HVLT_Retention == Inf, NA, HVLT_Retention)
    
    HVLT_RDI <-
      data[[paste(visit, "HVLTRec1", sep = "_")]] -
      data[[paste(visit, "HVLTRec2", sep = "_")]]
    
    out <- cbind(HVLT_TotalRecall,
                 HVLT_DelayedRecall,
                 HVLT_Retention,
                 HVLT_RDI)
    
    colnames(out) <- paste(visit, colnames(out), sep = "_")
    
    return(out)
  }
  
  hvlt_data <-
    map(c("BL", paste("AV", 1:11, sep = "")), get_hvlt, data = cog) |>
    bind_cols()
  
  cog <- bind_cols(cog, hvlt_data)
  
  ## Cog vars
  
  cog_vars <- cog |>
    select(
      Safehaven,
      contains("COWAT"),
      contains("TotalRecall"),
      contains("DelayedRecall"),
      contains("Retention"),
      contains("RDI"),
      contains("SDMT")
    )
  
  #### Other outcomes ####
  
  ### Death
  
  death <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionF3_SecondaryEndpoints_XT06_v1.csv"
    )
  )
  
  # Add Live variables to check if visit was completed
  
  death <- left_join(death, demo %>% select(Safehaven, contains("Live")), by = "Safehaven")
  
  # function for death by visit K
  
  death_function <- function(data, visit) {
    death_out <- ifelse(
      data$Death == 1 & data$Death_DSR < (365 * visit) &
        data[[paste0("C_AV", visit, "_Live")]] %in% c(25, 26),
      1,
      ifelse(
        data$Death == 0 & data$Death_DSR < (365 * visit) &
          data[[paste0("C_AV", visit, "_Live")]] %in% c(25, 26),
        NA,
        0
      )
    )
    
    if (visit > 1) {
      death_out <-
        ifelse(data[[paste("Death_AV", visit - 1, sep = "")]] == 1, 
               1, death_out)
    }
    
    return(death_out)
  }
  
  death$Death_AV1 <- death_function(death, 1)
  death$Death_AV2 <- death_function(death, 2)
  death$Death_AV3 <- death_function(death, 3)
  death$Death_AV4 <- death_function(death, 4)
  death$Death_AV5 <- death_function(death, 5)
  death$Death_AV6 <- death_function(death, 6)
  death$Death_AV7 <- death_function(death, 7)
  death$Death_AV8 <- death_function(death, 8)
  death$Death_AV9 <- death_function(death, 9)
  death$Death_AV10 <- death_function(death, 10)
  death$Death_AV11 <- death_function(death, 11)
  death$Death_AV12 <- ifelse(death$Death == 1, 1, death$Death_AV11)
  
  death_vars <- select(death, Safehaven, contains("Death"))
  
  ### Incident dementia
  
  dem_function <- function(data, visit) {
    ifelse(
      data$Dementia == 1 & data$Dementia_DSR < (365 * visit),
      1,
      ifelse(data$Dementia == 0 & data$Dementia_DSR < (365 * visit), NA, 0)
    )
  }
  
  death$Dem_AV1 <- dem_function(death, 1)
  death$Dem_AV2 <- dem_function(death, 2)
  death$Dem_AV3 <- dem_function(death, 3)
  death$Dem_AV4 <- dem_function(death, 4)
  death$Dem_AV5 <- dem_function(death, 5)
  death$Dem_AV6 <- dem_function(death, 6)
  death$Dem_AV7 <- dem_function(death, 7)
  death$Dem_AV8 <- dem_function(death, 8)
  death$Dem_AV9 <- dem_function(death, 9)
  death$Dem_AV10 <- dem_function(death, 10)
  death$Dem_AV11 <- dem_function(death, 11)
  death$Dem_AV12 <- ifelse(death$Dementia == 1, 1, death$Dem_AV11)
  
  dem_vars <- select(death, Safehaven, contains("Dem_"), Dementia, Dementia_DSR)
  
  ### Cancer
  
  death$Cancer_DSR <-
    ifelse(death$Cancer == 1, death$Cancer_DSR, death$Death_DSR)
  
  cancer_function <- function(data, visit) {
    ifelse(
      data$Cancer == 1 & data$Cancer_DSR < (365 * visit),
      1,
      ifelse(data$Cancer == 0 & data$Cancer_DSR < (365 * visit), NA, 0)
    )
  }
  
  death$Cancer_AV1 <- cancer_function(death, 1)
  death$Cancer_AV2 <- cancer_function(death, 2)
  death$Cancer_AV3 <- cancer_function(death, 3)
  death$Cancer_AV4 <- cancer_function(death, 4)
  death$Cancer_AV5 <- cancer_function(death, 5)
  death$Cancer_AV6 <- cancer_function(death, 6)
  death$Cancer_AV7 <- cancer_function(death, 7)
  death$Cancer_AV8 <- cancer_function(death, 8)
  death$Cancer_AV9 <- cancer_function(death, 9)
  death$Cancer_AV10 <- cancer_function(death, 10)
  death$Cancer_AV11 <- cancer_function(death, 11)
  death$Cancer_AV12 <- ifelse(death$Cancer == 1, 1, death$Cancer_AV11)
  
  cancer_vars <- select(death, Safehaven, contains("Cancer"), Cancer, Cancer_DSR)
  
  ### Katz ADLs
  
  katz_function <- function(data, visit) {
    ifelse(
      data$Disability == 1 & data$Disability_DSR < (365 * visit),
      1,
      ifelse(
        data$Disability == 0 & data$Disability_DSR < (365 * visit),
        NA,
        0
      )
    )
  }
  
  death$Disability_AV1 <- katz_function(death, 1)
  death$Disability_AV2 <- katz_function(death, 2)
  death$Disability_AV3 <- katz_function(death, 3)
  death$Disability_AV4 <- katz_function(death, 4)
  death$Disability_AV5 <- katz_function(death, 5)
  death$Disability_AV6 <- katz_function(death, 6)
  death$Disability_AV7 <- katz_function(death, 7)
  death$Disability_AV8 <- katz_function(death, 8)
  death$Disability_AV9 <- katz_function(death, 9)
  death$Disability_AV10 <- katz_function(death, 10)
  death$Disability_AV11 <- katz_function(death, 11)
  death$Disability_AV12 <- ifelse(death$Disability == 1, 1, death$Disability_AV11)
  
  katz_vars <- select(death, Safehaven, Disability, contains("Disability_"))
  
  #### CVD ####
  
  cvd <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionF5_DerivedEndpoints_XT06_v1.csv"
    )
  )
  
  ## Create CVD variable for each visit
  
  cvd_function <- function(data, visit) {
    if (visit == 1) {
      data[["CVD_AV1"]] <-
        ifelse(data$CVD_protocol == 1 &
                 data$CVD_protocol_DSR < 365, 1, 0)
    } else {
      data[[paste0("CVD_AV", visit)]] <-
        ifelse(data$CVD_protocol == 1 &
                 data$CVD_protocol_DSR < (365 * visit),
               1,
               0)
      data[[paste0("CVD_AV", visit)]] <-
        ifelse(data$CVD_protocol == 0 &
                 data$CVD_protocol_DSR < (365 * visit),
               NA,
               data[[paste0("CVD_AV", visit)]])
      
    }
    return(data[[paste0("CVD_AV", visit)]])
  }
  
  cvd$CVD_AV1 <- cvd_function(cvd, 1)
  cvd$CVD_AV2 <- cvd_function(cvd, 2)
  cvd$CVD_AV3 <- cvd_function(cvd, 3)
  cvd$CVD_AV4 <- cvd_function(cvd, 4)
  cvd$CVD_AV5 <- cvd_function(cvd, 5)
  cvd$CVD_AV6 <- cvd_function(cvd, 6)
  cvd$CVD_AV7 <- cvd_function(cvd, 7)
  cvd$CVD_AV8 <- cvd_function(cvd, 8)
  cvd$CVD_AV9 <- cvd_function(cvd, 9)
  cvd$CVD_AV10 <- cvd_function(cvd, 10)
  cvd$CVD_AV11 <- cvd_function(cvd, 11)
  cvd$CVD_AV12 <- ifelse(cvd$CVD_protocol == 1, 1, cvd$CVD_AV11)
  
  cvd_vars <- select(cvd, Safehaven, contains("CVD_"), CVD_protocol_DSR)
  
  ### CESD
  
  cesd <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionE2_CESD_XT06_v1_SafeHavenID.csv"
    )
  )
  
  cesd_vars <-
    select(cesd, Safehaven, contains("CesdOverall")) |>
    select(-contains("Reassess"))
  
  ### SF-12
  
  sf <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionE3_SF12_XT06_v1_SafeHavenID.csv"
    )
  )
  
  ## Calculate mental and physical component
  
  sf <- sf12("BL", sf)
  sf <- sf12("AV1", sf)
  sf <- sf12("AV2", sf)
  sf <- sf12("AV3", sf)
  sf <- sf12("AV4", sf)
  sf <- sf12("AV5", sf)
  sf <- sf12("AV6", sf)
  sf <- sf12("AV7", sf)
  sf <- sf12("AV8", sf)
  sf <- sf12("AV9", sf)
  sf <- sf12("AV10", sf)
  sf <- sf12("AV11", sf)
  
  sf_vars <- sf |> select(Safehaven, contains("MCS"), contains("PCS"))
  
  ### Frailty
  
  frailty <- read_dta(
    here(
      "..",
      "..",
      "Data",
      "frailty",
      "3. Final_Aung_Edited_FrailtyVariables_Only_Longitudinal_XT04_June_20240611.dta"
    )
  )
  
  frailty_vars <- select(frailty, Safehaven, ends_with("DAI50"))
  
  ### APOE
  
  apoe <- read_excel(
    here(
      "..",
      "..",
      "shared",
      "P552",
      "ticket_12797_SNP_TPS_APOE_no_Popul_final_dataset_withEthincity_n19114_msid_SafehavenID.xlsx"
    )
  )
  
  apoe$apoe_e4 <-
    case_when(
      apoe$APOE_geno == "e1/e3:e2/e4" ~ 1,
      apoe$APOE_geno == "e1/e4" ~ 1,
      apoe$APOE_geno == "e2/e2" ~ 0,
      apoe$APOE_geno == "e2/e3" ~ 0,
      apoe$APOE_geno == "e3/e3" ~ 0,
      apoe$APOE_geno == "e3/e4" ~ 1,
      apoe$APOE_geno == "e4/e4" ~ 2,
      TRUE ~ NA
    )
  
  apoe <- apoe |> select(Safehaven, apoe_e4)
  
  #### Create final dataframe ####
  
  ## Join created df's
  
  df_vec <- ls()[grep("_vars", ls())]
  
  df_out <-
    left_join(alsop_vars, demo_vars, by = "Safehaven") |>
    left_join(cesd_vars, by = "Safehaven") |>
    left_join(cog_vars, by = "Safehaven") |>
    left_join(dem_vars, by = "Safehaven") |>
    left_join(conmed_vars, by = "Safehaven") |>
    left_join(cvd_vars, by = "Safehaven") |>
    left_join(cancer_vars, by = "Safehaven") |>
    left_join(death_vars, by = "Safehaven") |>
    left_join(hyp_vars, by = "Safehaven") |>
    left_join(frailty_vars, by = "Safehaven") |>
    left_join(hear_vars, by = "Safehaven") |>
    left_join(history_vars, by = "Safehaven") |>
    left_join(katz_vars, by = "Safehaven") |>
    left_join(ms_vars, by = "Safehaven") |>
    left_join(phys_vars, by = "Safehaven") |>
    left_join(alc_vars, by = "Safehaven") |>
    left_join(sf_vars, by = "Safehaven") |>
    left_join(derived_vars, by = "Safehaven") |>
    left_join(apoe, by = "Safehaven")
  
  ## Remove some unneeded variables
  
  df_out2 <- df_out |>
    select(
      -contains("_HBP"),-contains("DeathSub"),-contains("DeathSubType"),-contains("_Detail"),-contains("EventID"),-contains("Subclass"),-contains("MetType"),-contains("Stage"),-contains("Antihyp"),-contains("MidPhys"),-starts_with("C_"),-DeathMode,
      -DeathType,-CancerType,-BM_ALSOP_DSR,
      -BS_ALSOP_DSR
    )
  
  # remove AV11 variables
  
  df_out2 <- df_out2 |> select(-starts_with("AV11"))
  
  ## Remove AV2 cognitive outcomes (almost no available data)
  
  df_out2 <- df_out2 |>
    select(
      -AV2_COWAT,
      -AV2_HVLT_RDI,
      -AV2_HVLT_TotalRecall,-AV2_HVLT_Retention,
      -AV2_3MS_OverallScore_C,
      -AV2_SDMT
    )
  
  ## Remove auxilliary variables with high levels of missingness
  
  df_out2 <- df_out2 |>
    select(
      -AV2_CesdOverall,
      -AV10_CesdOverall,
      -AV9_Frailty_DAI50,-AV10_Frailty_DAI50,
      -AV10_PCS,
      -AV10_MCS,
      -AV10_Polypharmacy
    )
  
  ## Make 'don't know' item responses missing data
  
  df_out2$CochlearImplnt <- 
    if_else(df_out2$CochlearImplnt == 9, NA, df_out2$CochlearImplnt)
  
  df_out2$HearingProbs <- 
    if_else(df_out2$HearingProbs == 9, NA, df_out2$HearingProbs)
  
  df_out2$HearingProbs_2 <- 
    if_else(df_out2$HearingProbs_2 == 9, NA, df_out2$HearingProbs_2)
  
  df_out2$Y3M_HearingProbs <- 
    if_else(df_out2$Y3M_HearingProbs == 9, NA, df_out2$Y3M_HearingProbs)
  
  df_out2$Y3M_HearingProbs_1 <- 
    if_else(df_out2$Y3M_HearingProbs_1 == 9, NA, df_out2$Y3M_HearingProbs_1)
  
  ## set variable types
  
  df_out2 <- df_out2 |>
    mutate(across(c("hypstatus_BL", "BL_SmHis"), as.factor))
  
  df_out2$Gender <- df_out2$Gender - 1
  
  ## Remove participants who died before ALSOP Y3
  
  df_out2 <- df_out2 |>
    filter(!(Death == 1 &
               Death_DSR < (365 * 3 + 200) & is.na(Y3M_ALSOP_DSR)))
  
  df_out2 <- df_out2 |> filter(Death_AV3 == 0)
  
  df_out2 <- df_out2 |> select(-Death_AV1, -Death_AV2, -Death_AV3)
  
  ## Write created dataset to rds file
  
  write_rds(df_out2, here("data", output_name))
}
