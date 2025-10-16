## bootrsap
get_boot_samples <- function(data){
  rows <- sample(1:nrow(data), nrow(data), replace = TRUE)
  data[rows,]
}

# my max

my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

## standardise variables to mean 0 SD 1

standardise <- function(data){
  ID <- dplyr::select(data, Safehaven)
  ## Convert factor variables to dummies 
  num_data <- model.matrix(~ .,
                           model.frame(
                             ~ ., data = data[,-1], na.action = NULL))[,-1] |> 
    as_tibble()
  
  data <- bind_cols(ID, num_data)
  
  ## standardise non-binary variables 
  
  non_binary <- 
    data[,apply(data, 2, function(x) length(unique(x[!is.na(x)])) > 2)] |> 
    dplyr::select(-Safehaven) |> 
    names()
  
  mean_vec <- apply(data[,non_binary], 2, mean, na.rm=T)
  
  sd_vec <- apply(data[,non_binary], 2, sd, na.rm=T)
  
  mean_refs <- tibble(var_name = names(data[,non_binary]), 
                      mean = mean_vec, 
                      sd = sd_vec)
  
  # save for access later
  write_csv(mean_refs, here("data","mean_and_sd_reference.csv"))
  
  # centre and scale continuous variables 
  
  data <- data |> 
    mutate(across(all_of(non_binary), ~ (. - mean(.,na.rm=T)) / sd(.,na.rm=T)))
  
  return(data)
}

## return to original units 

undo_standardise <- function(data){
  
  mean_sd <- read_csv(here("data","mean_and_sd_reference.csv"), 
                      show_col_types = FALSE)
  
  mean_sd <- mean_sd |> filter(var_name %in% names(data))
  
  for (i in mean_sd$var_name){
    data[[i]] <- 
      (data[[i]] * as.numeric(mean_sd[mean_sd$var_name == i, "sd"])) +
      as.numeric(mean_sd[mean_sd$var_name == i, "mean"])
  }
  return(data)
}

## strip unneeded elements of lm() object, to save memory

strip_lm <- function(model){
  model$model <- c()
  model$residuals <- c()
  model$fitted.values <- c()
  model$effects <- c()
  model$linear.predictors <- c()
  model$weights <- c()
  model$prior.weights <- c()
  model$data <- c()
  return(model)
}

## Round function for table output

my_round <- function(x, digits){
  formatC(x, digits = digits, format = "fg", drop0trailing = FALSE, flag = "#")
}

my_summarise <- function(data, stat){
  data |> 
    group_by(m, Y3M_HearingAid) |> 
    summarise(across(everything(), ~ get_stat(., stat))) |> 
    group_by(Y3M_HearingAid) |> summarise(across(-m, mean)) |> 
    pivot_longer(-Y3M_HearingAid, names_to = "var") |> 
    pivot_wider(names_from = Y3M_HearingAid) |> 
    select(var, `1`, `0`) |> 
    set_names(c("Variable", paste0("Yes_", stat), paste0("No_", stat)))
}

get_stat <- function(x, stat){
  switch(stat,
         mean = mean(x, na.rm=T),
         lower = quantile(x, 0.25, na.rm=T),
         upper = quantile(x, 0.75, na.rm=T),
         count = sum(x, na.rm=T),
         perc = sum(x, na.rm=T) / length(!is.na(x)) * 100)
}