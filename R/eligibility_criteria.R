# apply eligiblity criteria

apply_criteria <- function(data, selfrated_hearing = TRUE){
  
  ## apply eligibility criteria 
   
  if(isTRUE(selfrated_hearing)){
    
    data <- data |> filter(HearingAid == 0)
    
    data <- data |> filter(CochlearImplnt == 0)
    
    data <- data |> filter(HearingProbs == 1)
    
    data <- data |> filter(BL_VisualLimit == 0)
    
    return(data)
    
  } else {
    
    data <- data |> filter(HearingAid == 0)

    data <- data |> filter(CochlearImplnt == 0)
    
    data <- data |> 
      filter(BLToneAvg_Better >= 30 & BLToneAvg_Better < 70)
    
    data <- data |> filter(BL_VisualLimit == 0)
    
    return(data)
  }
}


#### Remove cognitive data after death ####

remove_postdeath <- function(data){
  data$AV4_3MS_OverallScore_C <- 
    if_else(data$Death_AV4 == 1 | is.na(data$Death_AV4), NA, 
            data$AV4_3MS_OverallScore_C)
  
  data$AV5_3MS_OverallScore_C <- 
    if_else(data$Death_AV5 == 1 | is.na(data$Death_AV5), NA, 
            data$AV5_3MS_OverallScore_C)
  
  data$AV6_3MS_OverallScore_C <- 
    if_else(data$Death_AV6 == 1 | is.na(data$Death_AV6), NA, 
            data$AV6_3MS_OverallScore_C)
  
  data$AV7_3MS_OverallScore_C <- 
    if_else(data$Death_AV7 == 1 | is.na(data$Death_AV7), NA, 
            data$AV7_3MS_OverallScore_C)
  
  data$AV8_3MS_OverallScore_C <- 
    if_else(data$Death_AV8 == 1 | is.na(data$Death_AV8), NA, 
            data$AV8_3MS_OverallScore_C)
  
  data$AV9_3MS_OverallScore_C <- 
    if_else(data$Death_AV9 == 1 | is.na(data$Death_AV9), NA, 
            data$AV9_3MS_OverallScore_C)
  
  data$AV10_3MS_OverallScore_C <- 
    if_else(data$Death_AV10 == 1 | is.na(data$Death_AV10), NA, 
            data$AV10_3MS_OverallScore_C)
  
  return(data)
}


