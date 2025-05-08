### Run multiple imputation ###
library(foreach)
library(doParallel)
library(tidyverse)
library(mice)
library(here)
library(glmnet)
source(here("R", "utils.R"))
source(here("imputation", "custom_mice_functions.R"))
source(here("imputation", "imputation_helpers.R"))

run_imputation <- function(data = "analysis_data_10_01_25.rds", 
                           cores = 1,
                           mice_output_suffix = "imp",
                           complete_data_suffix = "complete") {
  ## Read data
  
  df <- read_rds(here("data", data))
  
  ## Single imputation for variables with rare (<10) missing values
  
  df <- single_imputation(df)
  
  ## standardise before imputation
  
  df <- standardise(df)
  
  ## impute for each bootstrap sample
  
  nrows <- length(df$Safehaven)
  start <- 1
  finish <- 200
  
  cluster <- parallel::makeCluster(cores, type = "PSOCK")
  registerDoParallel(cl = cluster)
  
  foreach(i = start:finish, 
          .packages = c("here", "tidyverse", "mice", "glmnet")) %dopar% {
    source(here("imputation", "imputation_helpers.R"))
    nrows <- length(df$Safehaven)
    set.seed(i)
    samples <- sample(1:nrows, nrows, replace = TRUE)
    df_sample <- df[samples, ]
    imputation(
      df_sample,
      m = 2,
      imp_name = paste0(mice_output_suffix, i, ".rds"),
      complete_name = paste0(output_suffix, i, ".rds")
    )
  }
}
