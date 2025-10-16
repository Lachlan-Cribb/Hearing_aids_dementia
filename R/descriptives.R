## Sample size across imputed datasets 
get_sample_size <- function(data){
  data <- lapply(data, as.data.table)
  total_n <- unlist(lapply(data, nrow))
  treated_n <- unlist(lapply(data, \(x) nrow(x[Y3M_HearingAid == 1,])))
  untreated_n <- unlist(lapply(data, \(x) nrow(x[Y3M_HearingAid == 0,])))
  data.table(total = total_n, treated = treated_n, untreated = untreated_n)
}

sample_size_summarise <- function(data){
  data[, .(
    mean_n = mean(total),
    median_n = median(total),
    lower_n = quantile(total, 0.25),
    upper_n = quantile(total, 0.75),
    mean_treated = mean(treated),
    median_treated = median(treated),
    lower_treated = quantile(treated, 0.25),
    upper_treated = quantile(treated, 0.75),
    mean_untreated = mean(untreated),
    median_untreated = median(untreated),
    lower_untreated = quantile(untreated, 0.25),
    upper_untreated = quantile(untreated, 0.75)
  )]
}

### Table 2 
## Average over imputations for baseline characteristics table

baseline_table_summary <- function(data){
  # extract and combine list elements
  numeric_data <- rbindlist(data[str_detect(names(data), "_num")], idcol = "b")
  cat_data <- rbindlist(data[str_detect(names(data), "_cat")], idcol = "b")
  
  # average over imputations and make presentable
  numeric_data <- 
    numeric_data |> 
    group_by(Variable) |> 
    summarise(across(-c(b), mean)) |> 
    mutate(across(-Variable, ~ round(., 1))) |> 
    mutate(across(-Variable, ~ gsub(" ", "", .))) |> 
    mutate(Yes = paste0(Yes_mean, " (", Yes_lower, ", ", Yes_upper, ")"),
           No = paste0(No_mean, " (", No_lower, ", ", No_upper, ")")) |> 
    select(Variable, Yes, No)
  
  cat_data <-
    cat_data |> 
    group_by(Variable) |> 
    summarise(across(-c(b), mean)) |> 
    mutate(across(-Variable, ~ round(., 0))) |> 
    mutate(across(-Variable, ~ gsub(" ", "", .))) |> 
    mutate(Yes = paste0(Yes_count, " (", Yes_perc,"%)"),
           No = paste0(No_count, " (", No_perc,"%)")) |> 
    select(Variable, Yes, No)
  
  rbindlist(list(cat_data, numeric_data))
  
}

## Baseline characteristic table

get_baseline_table <- function(data){
  data <- rbindlist(data, idcol = "m")
  data$m <- as.numeric(as.factor(data$m))
  
  data$Smoking <- ifelse(
    data$BL_SmHis2 == 1, 
    "Former", 
    ifelse(
      data$BL_SmHis3 == 1, 
      "Never", 
      "Current")
  )
  
  data$BL_HYP <- case_when(
    data$hypstatus_BLNo_untreated == 1 ~ "No_untreated",
    data$hypstatus_BLYes_untreated == 1 ~ "Yes_untreated",
    data$hypstatus_BLYes_treated == 1 ~ "Yes_treated",
    TRUE ~ "No_treated"
  )
  
  data$apoe_e4 <- ifelse(data$apoe_e4 > 0, 1, 0)
  
  data$Gender <- ifelse(data$Gender == 1, "Woman", "Man")
  
  data$Racial <- ifelse(data$Racial == 1, "Non-White", "White")
  
  data$Edu <- as.factor(data$Edu)
  levels(data$Edu) <- c("< 9", "9-11", "12", "13-15", "16", "17-21")
  
  bl_variables <- c(
    "m",
    "Y3M_HearingAid",
    "AgeAtRand",
    "Edu",
    "Gender",
    "Racial",
    "BL_BMI",
    "BL_HYP",
    "DAB",
    "Pt_Cr",
    "BL_Frailty_DAI50",
    "Smoking",
    "apoe_e4",
    "BL_COWAT",
    "BL_HVLT_TotalRecall",
    "BL_HVLT_Retention",
    "BL_HVLT_DelayedRecall",
    "BL_HVLT_RDI",
    "BL_SDMT",
    "BL_MS_OverallScore_C",
    "BL_g",
    "BLToneAvg_Better"
  )
  
  # all categorical variables to binary indicators
  data <- data[, ..bl_variables]
  data <- as.data.table(predict(dummyVars(~ ., data = data, fullRank = FALSE), newdata = data))
  
  # split numeric and categorical data
  num_data <- select(data, m, Y3M_HearingAid, where( ~ n_distinct(.) > 2))
  cat_data <- select(data, where( ~ n_distinct(.) == 2))
  
  # summarise continuous data
  num_out <- map(c("mean", "lower", "upper"), ~ my_summarise(num_data, .x))
  num_out <- Reduce(function(x,y) full_join(x, y, by = "Variable"), num_out)
  
  # summarise categorical data
  cat_out <- map(c("count", "perc"), ~ my_summarise(cat_data, .x))
  cat_out <- Reduce(function(x,y) full_join(x, y, by = "Variable"), cat_out)
  
  list(cat = cat_out, num = num_out)
}

## missing data summary
missing_data_table <- function(df, visits_data){
  
  # apply eligibility criteria
  
  df <- apply_criteria(df, selfrated_hearing = TRUE)
  
  # remove post-death observations
  df <- remove_postdeath(df)
  
  # remove 3 from 3MS variable names
  
  names(df)[grepl("MS", names(df))] <-
    gsub("_3", "_", names(df)[grepl("MS", names(df))])
  
  # set hearing aid use to factor
  
  df$Y3M_HearingAidUse <- as.factor(round(as.numeric(df$Y3M_HearingAidUse)))
  
  to_rename <- df |>
    dplyr::select(contains("Death_")) |>
    dplyr::select(-contains("DSR"), -contains("protocol")) |>
    names()
  
  names(df)[names(df) %in% to_rename] <-
    gsub("(.*)_(.*)", "\\2_\\1", names(df)[names(df) %in% to_rename])
  
  # long format
  
  df$AV3_Death <- 0
  final_visit <- 10
  outcome_var <- "MS_OverallScore_C"
  
  vars <- paste(paste("AV", 3:final_visit, sep = ""),
                outcome_var,
                sep = "_")
  
  vars_death <- paste(paste("AV", 3:final_visit, sep = ""),
                      "Death",
                      sep = "_")
  
  df_long_ms <-
    df |>
    dplyr::select(-Death) |>
    pivot_longer(
      c(all_of(vars), all_of(vars_death)),
      names_to = c("time", ".value"),
      names_sep = "_",
      names_prefix = "AV"
    ) |>
    mutate(time = as.numeric(sub(paste(
      "_", outcome_var, sep = ""), "", time)))
  
  # add visits
  df_long_ms <- visits_data |>
    dplyr::select(Safehaven,
                  contains("_Possible"),
                  contains("_Conduct"),
                  -contains("Reassess")) |>
    dplyr::select(Safehaven, starts_with("AV")) |>
    pivot_longer(-Safehaven,
                 names_to = c("time", ".value"),
                 names_sep = "_") |>
    mutate(time = ifelse(time == "BL", "0", time)) |>
    mutate(time = as.numeric(gsub("AV", "", time))) |>
    filter(time > 2 & time < 11) |>
    right_join(df_long_ms, by = c("Safehaven", "time"))
  
  df_long_ms <- df_long_ms |>
    arrange(Safehaven, time) |>
    group_by(Safehaven) |>
    mutate(missing = ifelse(
      is.na(MS) & is.na(lead(MS)) & is.na(lead(MS, 2)) &
        is.na(lead(MS, 3)) &
        is.na(lead(MS, 4)) &
        is.na(lead(MS, 5)),
      1,
      0
    )) |>
    ungroup() |>
    mutate(
      missreason = case_when(
        missing == 1 & (Death == 1 | Possible == 2) ~ "death",
        missing == 1 & (Death == 0 | is.na(Death)) &
          (Possible %in% c(3, 4, 7) | Conduct == 2) ~ "withdrawn",
        missing == 1 & Possible == 5 ~ "calendar_time",
        missing == 0 & !is.na(MS) ~ "available",
        TRUE ~ "other_missing"
      )
    ) |>
    group_by(Safehaven) |>
    mutate(lag_missreason = lag(missreason, default = "available")) |>
    mutate(missreason = ifelse(lag_missreason=="death", "death", missreason)) |>
    mutate(missreason = ifelse(lag_missreason=="withdrawn", "withdrawn", missreason)) |>
    mutate(lag_missreason = lag(missreason, default = "available")) |>
    ungroup()
  
  miss_list <- list()
  
  for(i in 3:10){
    miss_list[[i]] <- df_long_ms |>
      filter(time == i) |>
      group_by(missreason) |>
      tally() |>
      mutate(percentage = n/nrow(df)*100) |>
      mutate(time = i)
  }
  
  bind_rows(miss_list)
}

## study flow chart

make_flow_chart <- function(df_list){
  
  median_rows <- median(map_int(df_list, nrow))
  
  df_list <- map(df_list, ~ filter(.x, HearingAid == 0 & CochlearImplnt == 0))
  
  median_rows2 <- median(map_int(df_list, nrow))
  
  df_list <- map(df_list, ~ filter(.x, HearingProbs == 1))
  
  median_rows3 <- median(map_int(df_list, nrow))
  
  df_list <- map(df_list, ~ filter(.x, BL_VisualLimit == 0))
  
  median_rows4 <- median(map_int(df_list, nrow))
  
  tibble(
    median_rows = median_rows, 
    median_rows2 = median_rows2,
    median_rows3 = median_rows3,
    median_rows4 = median_rows4
  )
}

flow_chart <- function(data) {
  c(paste0(
    "Alive at ASPREE Y3: ",
    median(data$median_rows),
    paste0("No past HA treatment: ", median(data$median_rows2)),
    paste0("Hearing problems: ", median(data$median_rows3)),
    paste0("No visual limitations: ", median(data$median_rows4))
  ))
}