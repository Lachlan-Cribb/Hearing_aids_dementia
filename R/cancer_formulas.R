### Regression formulas for time to event g-computation 
#### Y ####

ymodel <- 
  Y ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender


#### Death ####

compevent_model <- 
  Death ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender


#### Censoring ####

cens_model <- 
  Cens ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### CVD ####

CVD_form <- 
  CVD ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(lag1_Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### Polypharmacy

polypharmacy_form <- 
  Polypharmacy ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(lag1_Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### Frailty

frailty_form <- 
  Frailty ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(lag1_Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### PCS ####

PCS_form <- 
  PCS ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### MCS ####

MCS_form <- 
  MCS ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender


#### CESD ####

cesd_form <- 
  CesdOverall ~ 
  rms::rcs(time, c(1,3,6)) * A +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender


#### A ####

A_form <- 
  A ~ 
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  AgeAtRand:Gender

#### As treated ####

#### Y ####

ymodel_at <- 
  Y ~ 
  A2 + A3 +
  rms::rcs(time, c(1,3,6)) + 
  (A2 + A3):time +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### Death ####

compevent_model_at <- 
  Death ~ 
  A2 + A3 +
  rms::rcs(time, c(1,3,6)) + 
  (A2 + A3):time +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### Censoring ####

cens_model_at <- 
  Cens ~ 
  A2 + A3 +
  rms::rcs(time, c(1,3,6)) + 
  (A2 + A3):time +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### CVD ####

CVD_form_at <- 
  CVD ~ 
  A2 + A3 +
  rms::rcs(time, c(1,3,6)) + 
  (A2 + A3):time +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(lag1_Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### Polypharmacy

polypharmacy_form_at <- 
  Polypharmacy ~ 
  A2 + A3 +   
  rms::rcs(time, c(1,3,6)) +    
  (A2 + A3):time +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(lag1_Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### Frailty

frailty_form_at <- 
  Frailty ~ 
  A2 + A3 +   
  rms::rcs(time, c(1,3,6)) +    
  (A2 + A3):time +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### PCS ####

PCS_form_at <- 
  PCS ~ 
  A2 + A3 +   
  rms::rcs(time, c(1,3,6)) +    
  (A2 + A3):time +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_PCS, c(30,48,57)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### MCS ####

MCS_form_at <- 
  MCS ~ 
  A2 + A3 +   
  rms::rcs(time, c(1,3,6)) +    
  (A2 + A3):time +
  rms::rcs(PCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_MCS, c(43, 57, 63)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender


#### CESD ####

cesd_form_at <- 
  CesdOverall ~ 
  A2 + A3 +   
  rms::rcs(time, c(1,3,6)) +    
  (A2 + A3):time +
  rms::rcs(PCS, c(30,48,57)) +
  rms::rcs(MCS, c(43, 57, 63)) +
  rms::rcs(Frailty, c(0.05, 0.11, 0.27)) +
  rms::rcs(Polypharmacy, c(1, 4, 9)) +
  rms::rcs(lag1_CesdOverall, c(0, 4, 10)) +
  CVD +
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  time:BL_MS_OverallScore_C +
  time:AgeAtRand +
  time:Edu + 
  time:apoe_e4 +
  time:Gender +
  AgeAtRand:Gender

#### A2 ####

A2_form <- 
  A2 ~ 
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  AgeAtRand:Gender

#### A3 ####

A3_form <- 
  A3 ~ 
  rms::rcs(BL_COWAT, c(7, 12, 19)) +
  rms::rcs(BL_HVLT_TotalRecall, c(16, 23, 30)) +
  rms::rcs(BL_HVLT_DelayedRecall, c(4, 8, 11)) +
  rms::rcs(BL_HVLT_Retention, c(71, 100, 133)) +
  rms::rcs(BL_HVLT_RDI, c(5, 9, 11)) +
  rms::rcs(BL_SDMT, c(25, 38, 50)) +
  rms::rcs(BL_MS_OverallScore_C, c(88, 95, 99)) +
  rms::rcs(BLToneAvg_Better, c(16, 29, 40)) +
  rms::rcs(BL_MCS, c(43, 57, 63)) +
  rms::rcs(BL_PCS, c(30,48,57)) +
  rms::rcs(BL_BMI, c(23, 27, 33)) +
  rms::rcs(AgeAtRand, c(71, 73, 80)) +
  rms::rcs(BL_Frailty_DAI50, c(0.05, 0.11, 0.27)) +
  rms::rcs(CommunityEngagement, c(12, 16, 21)) +
  rms::rcs(LeisureActivities, c(-1.3, 0.1, 1.23)) +
  rms::rcs(BL_AlcWk, c(0, 3, 8)) +
  rms::rcs(Pt_scoreIRSAD, c(911, 1010, 1100)) +
  rms::rcs(BL_CesdOverall, c(0, 4, 10)) +
  rms::rcs(BL_Polypharmacy, c(1, 4, 9)) +
  HearingProbs_2 +
  poly(Edu,2) +
  poly(EyesRate,2) + 
  poly(Buzz,2) + 
  poly(QuietRm,2) +
  poly(CrowdedRm,2) + 
  poly(HrsSleep,2) + 
  poly(Income,2) + 
  PresPhysAct +
  Alone +
  Gender +
  DAB +
  Pt_Cr +
  BL_SmHis2 +
  BL_SmHis3 +
  apoe_e4 + 
  AgeAtRand:Gender