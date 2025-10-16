data_prep_targets <- list(
  # read CIND data
  tar_target(cind, read_dta(here("..", "..", "shared", "P552", "Aung_CIND_XT04.dta"))),
  
  # read XT04 endpoint data
  tar_target(events_xt04, read_rds(here("data", "analysis_data_28_10_24.rds"))),
  
  # visits data
  tar_target(visits_data, get_visit_data()),
  
  # unimputed data
  tar_target(unimputed_data, prepare_data()),
  
  # take bootstrap samples
  tar_rep(boot_samples, get_boot_samples(unimputed_data), reps = 1, batches = 200),
  
  # imputation
  #tar_target(imputed_data, run_imputation(boot_samples, m = 2), pattern = map(boot_samples)),
  
  # read imputed data (imputed separately from targets pipeline)
  tar_target(
    imp_paths, 
    list.files(here("imputed_data", "XT06", "imputed_datasets"), full.names = TRUE)),
  
  tar_target(imputed_data, read_rds(imp_paths), pattern = map(imp_paths)),
  
  # Preparing imputed data 
  tar_target(
    processed_data, 
    process_data(imputed_data,
                 selfrated_hearing = TRUE, 
                 truncate_threshold = 0.99, 
                 remove_post_death = TRUE),
    pattern = map(imputed_data)),
  
  # Prepare outcome data
  tar_target(
    prepared_data, 
    prepare_outcomes(processed_data, cind, events_xt04),
    pattern = map(processed_data)),
  
  # Objective hearing eligibility data
  tar_target(
    data_hearing_eligibility, 
    process_data(imputed_data,
                 selfrated_hearing = FALSE, 
                 truncate_threshold = 0.99, 
                 remove_post_death = TRUE),
    pattern = map(imputed_data)),
  
  # Prepare outcome data for hearing data
  tar_target(
    data_hearing, 
    prepare_outcomes(data_hearing_eligibility, cind, events_xt04),
    pattern = map(data_hearing_eligibility))
)