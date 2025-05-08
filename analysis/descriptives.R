## Sample descriptives

get_descriptives <- function(df_list, df_long){

  ## Overall n across imputed samples
  n_quantiles <- quantile(
    map_vec(df_list, ~ median(map_vec(.x, nrow))), 
    c(0.25,0.5,0.75)
    )
  
  ## treatment numbers across imputed samples 
  get_treatment_nums <- function(var) {
    unique_levels <- unique(df_list[[1]][[1]][[var]])
    unique_count <- function(data, var, level) {
      sum(data[[var]] == level)
    }
    num_tally <- function(data, var) {
      apply(rbindlist(lapply(data, function(.x) {
        rbindlist(lapply(.x, function(.y) {
          as.data.table(t(sapply(
            unique_levels,
            unique_count,
            data = .y,
            var = var
          ))) |> setnames(as.character(unique_levels))
        }))
      })), 2, median)
    }
    num_tally(df_list, var)
  }
  
  trt_vars <- c("Y3M_HearingAid", "Y3M_HearingAidUse")
  
  treatment_meds <- lapply(trt_vars, get_treatment_nums) |> set_names(trt_vars)
  
  ## follow-up duration by treatment
  df_long <- map(df_list, ~ map(.x, to_long_survival, outcome_var = "Dem"))
  df_long <- map(df_long, ~ map(.x, remove_postevent, outcome_var = "Dem"))
  
  get_follow_up <- function(level){
    median(sapply(unlist(df_long, recursive = FALSE),
                  function(.x) .x[Y3M_HearingAid==level, 
                                  list(m = max(time))]$m))
  }
  
  follow_up_meds <- map_vec(c(0,1), get_follow_up)
  
  ## Dementia cases by treatment
  get_dem_cases <- function(level){
    median(sapply(unlist(df_long, recursive = FALSE),
                  function(.x) .x[Dem==1 & Y3M_HearingAid==level, 
                                  list(n = .N)]$n))
  }
  
  dem_cases <- map_vec(c(0,1), get_dem_cases)
  
  ## cognitive impairment cases by treatment
  
  df_long_imp <- map(df_list, ~ map(.x, to_long_survival, outcome_var = "Imp"))
  df_long_imp <- map(
    df_long_imp, ~ map(.x, remove_postevent, outcome_var = "Imp"))
  
  
  get_imp_cases <- function(level){
    median(sapply(unlist(df_long_imp, recursive = FALSE),
                  function(.x) .x[Imp==1 & Y3M_HearingAid==level, 
                                  list(n = .N)]$n))
  }
  
  imp_cases <- map_vec(c(0,1), get_imp_cases)
  
  return(list(
    n_quantiles = n_quantiles,
    treatment_meds = treatment_meds,
    follow_up_meds = follow_up_meds,
    dem_cases = dem_cases,
    imp_cases = imp_cases))
}

## Table 2 

make_table2 <- function(df_list){
  ## Table 2
  
  table2_vars <- c("AgeAtRand", "Gender", "Edu", "Income", "Racial",
                   "DAB", "Pt_Cr", "apoe_e4", "BL_MS_OverallScore_C",
                   "hypstatus_BLNo_untreated","hypstatus_BLYes_treated",
                   "hypstatus_BLYes_untreated", "BLToneAvg_Better")
  
  table2_out <- table2(df_list, table2_vars)
}

## missing data summary
missing_data_table <- function(df){
  
  # add visit data
  visits <- read_csv(
    here(
      "..",
      "..",
      "Shared",
      "Resource-XT06",
      "CSV Version",
      "SectionH1_Visits_XT06_v1.csv"
    )
  )
  
  df <- prepare_data(
    df, 
    selfrated_hearing = TRUE, 
    remove_post_death = TRUE,
    truncate_threshold = 0.99)
  
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
      "_", outcome_var, sep = ""
    ), "", time)))
  
  # add visits
  df_long_ms <- visits |>
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

make_flow_chart <- function(dataset_paths){
  
  df_list <- map(dataset_paths, ~ read_rds(.x))
  
  median_rows <- median(sapply(unlist(df_list, recursive = FALSE), nrow))
  
  df_list <- map(df_list, ~ map(.x, ~ .x |> filter(HearingAid == 0 &
                                                     CochlearImplnt == 0)))
  
  median_rows2 <- median(sapply(unlist(df_list, recursive = FALSE), nrow))
  
  df_list <- map(df_list, ~ map(.x, ~ .x |> filter(HearingProbs == 1)))
  
  median_rows3 <- median(sapply(unlist(df_list, recursive = FALSE), nrow))
  
  df_list <- map(df_list, ~ map(.x, ~ .x |> filter(BL_VisualLimit == 0)))
  
  median_rows4 <- median(sapply(unlist(df_list, recursive = FALSE), nrow))
  
  return(c(paste0("Alive at ASPREE Y3: ", median_rows),
    paste0("No past HA treatment: ", median_rows2),
    paste0("Hearing problems: ", median_rows3),
    paste0("No visual limitations: ", median_rows4)))
  
}

## Table 2 function

table2 <- function(data_list, vars){
  
  get_summary <- function(data, var){
    
    if(length(unique(data[[1]][[var]])) > 8){
      
      mean_var <- mean(data[[1]][[var]], na.rm=T)
      mean_var2 <- mean(data[[2]][[var]], na.rm=T)
      mean_var <- mean(c(mean_var, mean_var2))
      sd_var <- sd(data[[1]][[var]], na.rm=T)
      sd_var2 <- sd(data[[2]][[var]], na.rm=T)
      sd_var <- mean(c(sd_var, sd_var2))
      
      out <- tibble(variable = var, summary = mean_var, spread = sd_var)
    } else {
      
      summary_cat <- function(data, var, i){
        total1 <- sum(data[[1]][[var]]==i)
        total2 <- sum(data[[2]][[var]]==i)
        total <- mean(c(total1, total2))
        perc1 <- (sum(data[[1]][[var]]==i) / nrow(data[[1]]))*100
        perc2 <- (sum(data[[2]][[var]]==i) / nrow(data[[2]]))*100
        perc <- mean(c(perc1, perc2))
        out <- tibble(variable = var, i = i, summary = total, spread = perc)
        return(out)
      }
      
      out <- 
        map(unique(data[[1]][[var]]), summary_cat, data = data, var = var) |> 
        bind_rows()
    }
    return(out)
  }
  
  get_summary_all <- function(data, vars){map(vars, get_summary, data = data)}
  
  out <- map(data_list, get_summary_all, vars = vars) |> bind_rows()
  
  out <- 
    out |> 
    group_by(variable, i) |> 
    summarise(across(c("summary","spread"), mean))
  
  # format for table
  
  out <- out |> 
    arrange(match(variable, vars)) |> 
    mutate(across(c("summary","spread"), ~ ifelse(is.na(i), 
                                                  my_round(.x, 3), 
                                                  my_round(.x, 1))))
  
  out$summary <- ifelse(is.na(out$i), 
                        paste0(out$summary, " (", out$spread, ")"),
                        paste0(out$summary, " (", out$spread, "%)"))
  
  out <- out |> dplyr::select(-spread)
  
  out$summary <- str_trim(out$summary)
  
  return(out)
}



