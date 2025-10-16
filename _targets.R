library(targets)
library(tarchetypes)
library(qs2)
library(here)
options(clustermq.scheduler = "multiprocess")
library(clustermq)

Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)

# Set target options:
tar_option_set(
  packages = c("readxl",
               "psych",
               "glmnet",
               "here",
               "mice",
               "haven", 
               "tidyverse", 
               "lavaan",
               "rms", 
               "speedglm", 
               "data.table", 
               "WeightIt", 
               "nnet", 
               "haven", 
               "patchwork", 
               "bayesplot", 
               "cowplot", 
               "knitr", 
               "kableExtra", 
               "progressr", 
               "prodlim", 
               "cmprsk",
               "caret",
               "Rfast"),
  format = "qs",
  storage = "main",
  seed = 1234,
  error = "continue"
)
select <- dplyr::select

# Source the R scripts in the analysis folder
tar_source()
source(here("..", "..", "utils", "SF12_scoring.R"))
source("data_preparation_targets.R")
source("descriptives_targets.R")
source("cog_targets.R")
source("time_to_event_targets.R")
source("sensitivity_targets.R")

## Pipeline
list(
  data_prep_targets,
  descriptives_targets,
  cog_targets,
  time_to_event_targets,
  sensitivity_targets
)
