#### Formulas for linear regression ####

## ITT

cog_itt_form <-
  "rcs(time, c(3,7,10)) * A +
    rcs(BL_COWAT, c(7, 12, 19)) +
    rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
    rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
    rcs(BL_HVLT_Retention, c(71, 100, 133)) +
    rcs(BL_HVLT_RDI, c(5, 9, 11)) +
    rcs(BL_SDMT, c(25, 38, 50)) +
    rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
    rcs(BLToneAvg_Better, c(16, 29, 40)) +
    rcs(BL_MCS, c(43, 57, 63)) +
    rcs(BL_PCS, c(30,48,57)) +
    rcs(BL_BMI, c(23, 27, 33)) +
    rcs(AgeAtRand, c(71, 73, 80)) +
    rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
    rcs(CommunityEngagement, c(12, 16, 21)) +
    rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
    rcs(BL_AlcWk, c(0, 3, 8)) +
    rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
    rcs(BL_CesdOverall, c(0, 4, 10)) +
    rcs(BL_Polypharmacy, c(1, 4, 9)) +
    HearingProbs_2 +
    Edu + I(Edu^2) +
    EyesRate + I(EyesRate^2) +
    Buzz + I(Buzz^2) +
    QuietRm + I(QuietRm^2) +
    CrowdedRm + I(CrowdedRm^2) +
    HrsSleep + I(HrsSleep^2) +
    PresPhysAct +
    Alone +
    Income + I(Income^2) +
    Gender +
    Racial +
    DAB +
    Pt_Cr +
    BL_SmHis2 +
    BL_SmHis3 +
    hypstatus_BLNo_untreated +
    hypstatus_BLYes_treated +
    hypstatus_BLYes_untreated + +
    apoe_e4 + 
    rcs(time, c(3,7,10)):BL_MS_OverallScore_C +
    rcs(time, c(3,7,10)):BL_PCS +
    rcs(time, c(3,7,10)):AgeAtRand +
    rcs(time, c(3,7,10)):BL_Frailty_DAI50 +
    rcs(time, c(3,7,10)):BLToneAvg_Better +
    rcs(time, c(3,7,10)):BL_BMI +
    rcs(time, c(3,7,10)):Edu + 
    rcs(time, c(3,7,10)):apoe_e4 +
    rcs(time, c(3,7,10)):BL_CesdOverall +
    rcs(time, c(3,7,10)):BL_HVLT_TotalRecall +
    rcs(time, c(3,7,10)):BL_HVLT_DelayedRecall +
    rcs(time, c(3,7,10)):BL_HVLT_Retention +
    rcs(time, c(3,7,10)):BL_HVLT_RDI +
    rcs(time, c(3,7,10)):BL_SDMT +
    A:BL_MS_OverallScore_C +
    A:BL_HVLT_DelayedRecall +
    A:BL_PCS +
    A:AgeAtRand  +
    A:BL_Frailty_DAI50 +
    A:BLToneAvg_Better +
    A:rcs(time, c(3,7,10)):BL_MS_OverallScore_C +
    A:rcs(time, c(3,7,10)):BL_HVLT_DelayedRecall +
    A:rcs(time, c(3,7,10)):BL_PCS +
    A:rcs(time, c(3,7,10)):AgeAtRand +
    A:rcs(time, c(3,7,10)):BL_Frailty_DAI50 +
    A:rcs(time, c(3,7,10)):BLToneAvg_Better + 
    rcs(AgeAtRand, c(71, 73, 80)):Gender +
    rcs(AgeAtRand, c(71, 73, 80)):BL_MS_OverallScore_C +
    rcs(AgeAtRand, c(71, 73, 80)):BLToneAvg_Better +
    rcs(AgeAtRand, c(71, 73, 80)):BL_Frailty_DAI50 +
    rcs(AgeAtRand, c(71, 73, 80)):Edu +
    rcs(AgeAtRand, c(71, 73, 80)):BL_HVLT_DelayedRecall
"

## As treated

cog_at_form <-
"rcs(time, c(3,7,10)) +
  A +
  time:A +
  rcs(BL_COWAT, c(7, 12, 19)) +
  rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rcs(BL_SDMT, c(25, 38, 50)) +
  rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rcs(BL_MCS, c(43, 57, 63)) +
  rcs(BL_PCS, c(30,48,57)) +
  rcs(BL_BMI, c(23, 27, 33)) +
  rcs(AgeAtRand, c(71, 73, 80)) +
  rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rcs(CommunityEngagement, c(12, 16, 21)) +
  rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rcs(BL_AlcWk, c(0, 3, 8)) +
  rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rcs(BL_CesdOverall, c(0, 4, 10)) +
  rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  Edu + I(Edu^2) +
  EyesRate + I(EyesRate^2) +
  Buzz + I(Buzz^2) +
  QuietRm + I(QuietRm^2) +
  CrowdedRm + I(CrowdedRm^2) +
  HrsSleep + I(HrsSleep^2) +
  PresPhysAct +
  Alone +
  Income + I(Income^2) +
  Gender +
  Racial +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  hypstatus_BLNo_untreated +
  hypstatus_BLYes_treated +
  hypstatus_BLYes_untreated + +
  apoe_e4 +
  rcs(time, c(3,7,10)):BL_MS_OverallScore_C +
  rcs(time, c(3,7,10)):BL_PCS +
  rcs(time, c(3,7,10)):AgeAtRand +
  rcs(time, c(3,7,10)):BL_Frailty_DAI50 +
  rcs(time, c(3,7,10)):BLToneAvg_Better +
  rcs(time, c(3,7,10)):BL_BMI +
  rcs(time, c(3,7,10)):Edu +
  rcs(time, c(3,7,10)):apoe_e4 +
  rcs(time, c(3,7,10)):BL_CesdOverall +
  rcs(time, c(3,7,10)):BL_HVLT_TotalRecall +
  rcs(time, c(3,7,10)):BL_HVLT_DelayedRecall +
  rcs(time, c(3,7,10)):BL_HVLT_Retention +
  rcs(time, c(3,7,10)):BL_HVLT_RDI +
  rcs(time, c(3,7,10)):BL_SDMT +
  A:BL_MS_OverallScore_C +
  A:BL_HVLT_DelayedRecall +
  A:BL_PCS +
  A:AgeAtRand  +
  A:BL_Frailty_DAI50 +
  A:BLToneAvg_Better +
  A:time:BL_MS_OverallScore_C +
  A:rcs(time, c(3,7,10)):BL_HVLT_DelayedRecall +
  A:time:BL_PCS +
  A:time:AgeAtRand +
  A:time:BL_Frailty_DAI50 +
  A:time:BLToneAvg_Better +
  rcs(AgeAtRand, c(71, 73, 80)):Gender +
  rcs(AgeAtRand, c(71, 73, 80)):BL_MS_OverallScore_C +
  rcs(AgeAtRand, c(71, 73, 80)):BLToneAvg_Better +
  rcs(AgeAtRand, c(71, 73, 80)):BL_Frailty_DAI50 +
  rcs(AgeAtRand, c(71, 73, 80)):Edu +
  rcs(AgeAtRand, c(71, 73, 80)):BL_HVLT_DelayedRecall"