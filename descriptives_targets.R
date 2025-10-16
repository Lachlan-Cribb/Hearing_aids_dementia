## DESCRIPTIVES TARGETS 

descriptives_targets <- list(
  ## Sample size summary
  tar_target(sample_size, get_sample_size(prepared_data), pattern = map(prepared_data)),
  
  tar_target(sample_size_summary, sample_size_summarise(sample_size)),
  
  # table 2 - baseline characteristics
  tar_target(bl_table, get_baseline_table(prepared_data), pattern = map(prepared_data)),
  tar_target(bl_table_summary, baseline_table_summary(bl_table)),
  
  # missing data summary (using data before imputation)
  tar_target(miss_summary, missing_data_table(unimputed_data, visits_data)),
  
  # missing data summary ## NEED TO PROVIDE UNPROCESSED
  tar_target(flow_info, make_flow_chart(imputed_data), pattern = map(imputed_data)),
  tar_target(flow_chart_summary, flow_chart(flow_info))
)