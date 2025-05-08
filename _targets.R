library(targets)
library(qs2)
library(here)

# Set target options:
tar_option_set(
  packages = c("dplyr", "haven", "tidyverse", "Hmisc", "lavaan",
               "rms", "speedglm", "data.table", "WeightIt", "nnet", 
               "haven", "patchwork", "bayesplot", "cowplot", "future",
               "future.apply", "knitr", "kableExtra", "progressr"),
  format = "qs"
)

data.table::setDTthreads(1)

# Source the R scripts in the analysis folder
tar_source(files = "analysis")

## Pipeline
list(
  # read CIND data
  tar_target(cind, read_cind("Aung_CIND_XT04.dta")),
  
  # read XT04 endpoint data
  tar_target(events_xt04, read_xt04("analysis_data_28_10_24.rds")),
  
  # unimputed data
  tar_target(unimputed_data, 
             read_rds(here("data", "analysis_data_10_01_25.rds"))),
  
  # paths to imputed datasets
  tar_target(dataset_paths, get_dataset_paths(max_samples = NULL)),
  
  # Reading and preparing imputed datafiles 
  tar_target(processed_data, read_and_process_data(dataset_paths)),
  
  # Prepare outcome data over processed datafiles
  tar_target(prepared_data, 
             prepare_outcomes(processed_data, cind, events_xt04)),

  # descriptive statistics 
  tar_target(desciptives, get_descriptives(prepared_data)),
  
  # table 2 
  tar_target(table2_out, make_table2(prepared_data)),
  
  # missing data summary (using data before imputation)
  tar_target(miss_summary, missing_data_table(unimputed_data)),
  
  # missing data summary ## NEED TO PROVIDE UNPROCESSED
  tar_target(flow_chat, make_flow_chart(dataset_paths)),
  
  ### Cognition outcomes
  # estimates
  tar_target(cog_estimates, all_cognition_estimates(prepared_data)),
  
  # Add confidence intervals and plot
  tar_target(cog_intervals, cognition_intervals_all(cog_estimates)),
  
  # Save plots
  tar_target(output_plots, save_cognition_plots(cog_intervals)),
  
  ## Effect modification
  tar_target(effect_mod_plots, get_effect_mod_plots(prepared_data)),
  
  ### Survival outcomes
  # estimates
  tar_target(surv_estimates, all_survival_estimates(prepared_data)),
  # 
  # # Add confidence intervals and plot
  tar_target(surv_intervals, all_intervals_survival(surv_estimates)),
  # 
  # # Save plots
  tar_target(output_surv_plots, save_survival_plots(surv_intervals)),
  # 
  # ### sensitivity analyses
  # # consent for ASPREE XT (negative outcome control)
  tar_target(aspree_xt_consent, xt_consent(prepared_data)),
  # 
  # # cancer negative outcome control
  tar_target(cancer, cancer_outcome_control(prepared_data)),
  # 
  # # IPW for time to event outcomes (sensitivity for model spec)
  tar_target(ipw_risks, ipw_all_outcomes(prepared_data, surv_intervals)),
  # 
  # # Sensitivity for confounding by indication/healthcare utilisation
  tar_target(confounding, sensitivity_confounding(prepared_data)),
  # 
  # # Sensitivity for mortality
  tar_target(death, get_results_death(prepared_data, "Death", TRUE)),
  # 
  # # Sensitivity with objective hearing eligibility criterion
  tar_target(hearing, hearing_sensitivity(dataset_paths, cind, events_xt04)),
  # 
  # # plot objective vs subjective hearing
  tar_target(hearing_plot, plot_objective_vs_subjective_hearing(unimputed_data))
)
