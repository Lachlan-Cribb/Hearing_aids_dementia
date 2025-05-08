### Functions for primary analysis

## read extra data to append to dataframes 

read_cind <- function(cind_data){ 
  read_dta(here("..", "..", "shared", "P552", cind_data))
}

read_xt04 <- function(xt04_data){
  read_rds(here("data", xt04_data))
}

## Get paths to imputed datasets to read

get_dataset_paths <- function(max_samples = NULL){
  n_samples <- 
    ifelse(!is.null(max_samples), 
            max_samples, 
            length(list.files(here("imputed_data","XT06","imputed_datasets"))))
    
  paste0(
    here("imputed_data","XT06","imputed_datasets"), 
    "/complete", 
    1:n_samples, 
    ".rds")
}

## read imputed datasets (list of two imputed dataframes per file) and 
## prepare for analysis (apply eligibility criteria, 
## remove post-death observations, calculate factor scores)


read_and_process_data <- function(dataset_path,
                                  selfrated_hearing = TRUE,
                                  truncate_threshold = 0.99,
                                  remove_post_death = TRUE) {
  # parallel
  plan(multisession)
  
  # read datasets and prepare for analysis
  # see cognition_analysis_helpers.R for prepare_data() definition
  data <- future_lapply(
    dataset_path,
    FUN = function(.x) {
      out <- read_rds(.x)
      map(
        out,
        prepare_data,
        selfrated_hearing = selfrated_hearing,
        truncate_threshold = truncate_threshold,
        remove_post_death = remove_post_death
      )
    }
  )
  
  plan(sequential)
  
  return(data)
}

## Add cognitive impairment outcome variable and revert dementia to previous
## dataset version (outcome adjudication incomplete in latest ASPREE XT 
## release)

prepare_outcomes <- function(data, cind, events_xt04){
  data <- map(data, add_imp, cind)
  data <- map(data, revert_dementia, events_xt04)
  data <- map(data, revert_cancer, events_xt04)
  return(data)
}

## run analysis for cognitive endpoints

all_cognition_estimates <- function(data, 
                                    comparator = "control",
                                    final_visit = 10,
                                    effect_modifier = NULL){
  
  # map over all cognition outcomes 
  cog_outcomes <- c("g", "MS_OverallScore_C", "COWAT", "HVLT_TotalRecall",
                    "HVLT_DelayedRecall", "HVLT_Retention", "HVLT_RDI", "SDMT")
  
  cognition_results_out <- map(cog_outcomes, 
                               cognition_estimates, 
                               data = data,
                               comparator = comparator,
                               final_visit = final_visit,
                               effect_modifier = effect_modifier)
  
  names(cognition_results_out) <- cog_outcomes
  
  return(cognition_results_out)
}

## combine imputations, add confidence intervals, and plot

cognition_intervals_all <- function(point_estimates,
                                    effect_modifier = NULL,
                                    satterthwaite = TRUE){
  
  cognition_intervals_out <- map(point_estimates, 
                                 cognition_intervals,
                                 effect_modifier,
                                 satterthwaite)
  
  return(cognition_intervals_out)
  
}


## function for saving all cognition plots 

save_cognition_plots <- function(results){
  #g
  save_cog_plot(
    results$g$plots,
    file_name = "g_with_md.png",
    ylab = "Mean overall cognition score",
    breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25),
    md_breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
    limits = c(-1, 0.3),
    md_limits = c(-0.3,0.3))
  
  #3ms
  save_cog_plot(
    results$MS_OverallScore_C$plots,
    file_name = "3ms_with_md.png",
    ylab = "Mean 3MS overall score",
    breaks = c(91, 92, 93, 94, 95),
    md_breaks = c(-1,-0.5,0,0.5,1,1.5),
    limits = c(90.75, 95.25),
    md_limits = c(-1,1.5))
  
  #cowat
  save_cog_plot(
    results$COWAT$plots,
    file_name = "cowat_with_md.png",
    ylab = "Mean COWAT score",
    breaks = c(12, 12.5, 13, 13.5, 14, 14.5),
    limits = c(12, 14.5),
    md_breaks = c(-0.4,-0.2,0,0.2,0.4,0.6, 0.8),
    md_limits = c(-0.4, 0.8))
  
  #total recall
  save_cog_plot(
    results$HVLT_TotalRecall$plots,
    file_name = "hvlt_recall_with_md.png",
    ylab = "Mean HVLT total recall",
    breaks = c(21, 22, 23, 24, 25),
    limits = c(20.9, 25),
    md_breaks = c(-1,-0.5, 0, 0.5, 1),
    md_limits = c(-1, 1))
  
  #delayed recall
  save_cog_plot(
    results$HVLT_DelayedRecall$plots,
    file_name = "hvlt_delayrecall_with_md.png",
    ylab = "Mean HVLT delayed recall",
    breaks = c(6.5, 7, 7.5, 8, 8.5, 9),
    limits = c(6.5, 9),
    md_breaks = c(-0.5,-0.25, 0, 0.25, 0.5),
    md_limits = c(-0.5, 0.5)
  )
  
  # retention
  save_cog_plot(
    results$HVLT_Retention$plots,
    file_name = "hvlt_retention_with_md.png",
    ylab = "Mean HVLT retention",
    breaks = c(85, 90, 95, 100, 105, 110),
    limits = c(85, 110),
    md_breaks = c(-4, -2, 0, 2, 4, 6, 8, 10),
    md_limits = c(-4, 10)
  )
  
  # rdi
  save_cog_plot(
    results$HVLT_RDI$plots,
    file_name = "hvlt_rdi_with_md.png",
    ylab = "Mean HVLT RDI",
    breaks = c(9.5, 10, 10.5, 11),
    limits = c(9.5, 11),
    md_breaks = c(-0.2,0,0.2, 0.4),
    md_limits = c(-0.2, 0.4)
  )
  
  # sdmt
  save_cog_plot(
    results$SDMT$plots,
    file_name = "SDMT_with_md.png",
    ylab = "Mean SDMT score",
    breaks = c(28, 30, 32, 34, 36, 38),
    limits = c(26.5, 38.5),
    md_breaks = c(-1,-0.5, 0, 0.5, 1, 1.5),
    md_limits = c(-1, 1.5)
  )
}

## Dementia and cognitive impairment estimtes 

all_survival_estimates <- function(data) {
  
  # map over all survival outcomes
  surv_outcomes <- c("Dem", "Imp")
  
  survival_results_out <- map(surv_outcomes, survival_estimates, data = data)
  
  names(survival_results_out) <- surv_outcomes
  
  return(survival_results_out)
}

## Add confidence intervals and plot

all_intervals_survival <- function(results, satterthwaite = TRUE) {
  
  survival_intervals_out <- map(results, survival_intervals, satterthwaite)
  
  return(survival_intervals_out)
}

## Save survival plots

save_survival_plots <- function(results){
  save_cuminc_plot(
    results$Dem$plots,
    file_name = "Dementia.png",
    y_label = "Dementia risk",
    breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125),
    limits = c(0, 0.13)
  )
  
  save_cuminc_plot(
    results$Imp$plots,
    file_name = "Impairment.png",
    y_label = "Cognitive impairment risk",
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
    limits = c(0, 0.51)
  )
}

