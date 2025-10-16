## Confidence intervals for cognitive outcomes

cognition_intervals <- function(point_estimates,
                                effect_modifier,
                                satterthwaite){
  
  point_estimates <- bind_rows(point_estimates, .id = "b")
  point_estimates$b <- as.numeric(as.factor(point_estimates$b))
  
  estimates <- add_intervals(
    point_estimates,
    satterthwaite = satterthwaite,
    effect_modifier = effect_modifier
  )
  
  contrasts <- get_contrasts(
    point_estimates,
    satterthwaite = satterthwaite,
    effect_modifier = effect_modifier
  )
  
  bind_rows(estimates, contrasts)
}

## Bootstrap confidence intervals

add_intervals <- function(results,
                          satterthwaite,
                          effect_modifier = effect_modifier) {

  if (is.null(effect_modifier)) {
    params <-
      expand_grid(unique(results$A), unique(results$time), unique(results$estimand)) |>
      set_names(c("A", "time", "estimand")) |> 
      filter(!(estimand == "ITT" & A %in% c("2", "3"))) |> 
      filter(!(estimand == "AT" & A == "0"))
    
    out <- pmap(
      list(time = params$time, A = params$A, estimand = params$estimand),
      get_variance,
      data = results,
      satterthwaite = satterthwaite,
      effect_modifier = effect_modifier
    )
    
  } else {
    
    params <- expand_grid(
      unique(results$A), 
      unique(results$time), 
      unique(results$estimand), 
      unique(results[[effect_modifier]])) |>
      set_names(c("A", "time", "estimand", "effect_modifier_level")) |> 
      filter(!(estimand == "ITT" & A %in% c("2", "3"))) |> 
      filter(!(estimand == "AT" & A == "0"))
    
    out <- pmap(
      list(
        time = params$time,
        A = params$A,
        estimand = params$estimand,
        effect_modifier_level = params$effect_modifier_level
      ),
      get_variance,
      data = results,
      effect_modifier = effect_modifier,
      satterthwaite = satterthwaite
    )
  }
  
  bind_rows(out)
}

## mean between group differences with 95% CI 

get_contrasts <- function(results, 
                          as_treated = TRUE, 
                          satterthwaite,
                          effect_modifier = effect_modifier){
  
  # wide format for contrasts 
  
  contrasts_wide <- function(effect_modifier_level = NULL){
    if(!is.null(effect_modifier)){
      results <- results[results[[effect_modifier]] == effect_modifier_level,]
    }
      itt <- results |> 
        filter(estimand == "ITT") |> 
        pivot_wider(names_from = A, 
                    values_from = Y, 
                    names_prefix = "estimate_") |> 
        mutate(Y = estimate_1 - estimate_0,
               A = "1_0") |> 
        select(b, estimand, A, time, Y)
      at <- results |> 
        filter(estimand == "AT") |> 
        pivot_wider(names_from = A, 
                    values_from = Y, 
                    names_prefix = "estimate_") |> 
        mutate(Y = estimate_3 - estimate_1,
               A = "3_1") |> 
        select(b, estimand, A, time, Y)
      out <- bind_rows(itt, at)
      if(!is.null(effect_modifier)){
        out[[effect_modifier]] <- effect_modifier_level
      }
      out
  }
  
  if(is.null(effect_modifier)){
    
    wide <- contrasts_wide(NULL)
    
    params <-
      expand_grid(unique(wide$A), unique(wide$time), unique(wide$estimand)) |>
      set_names(c("A", "time", "estimand")) |> 
      filter(!(estimand == "ITT" & A == "3_1")) |> 
      filter(!(estimand == "AT" & A == "1_0"))
    
    out <- pmap(
      list(time = params$time, A = params$A, estimand = params$estimand),
      get_variance,
      data = wide,
      satterthwaite = satterthwaite,
      effect_modifier = effect_modifier
    )
  } else {
    
    wide <- map_df(unique(results[[effect_modifier]]), contrasts_wide)
    
    params <- expand_grid(
      unique(wide$A), 
      unique(wide$time), 
      unique(wide$estimand), 
      unique(wide[[effect_modifier]])) |>
      set_names(c("A", "time", "estimand", "effect_modifier_level")) |> 
      filter(!(estimand == "ITT" & A == "3_1")) |> 
      filter(!(estimand == "AT" & A == "1_0"))
    
    out <- pmap(
      list(
        time = params$time,
        A = params$A,
        estimand = params$estimand,
        effect_modifier_level = params$effect_modifier_level
      ),
      get_variance,
      data = wide,
      effect_modifier = effect_modifier,
      satterthwaite = satterthwaite
    )
  }
  
  bind_rows(out)
}

## get variance for 95% CIs

get_variance <- function(data, 
                         time, 
                         A, 
                         estimand,
                         satterthwaite,
                         effect_modifier = effect_modifier,
                         effect_modifier_level = NULL){
  
  B <- max(as.numeric(data$b))
  # by time, treatment, and effect modifier
  
  if(is.null(effect_modifier)){
    data <- data |> filter(time == {{ time }}, A == {{ A }}, estimand == {{ estimand }})
  } else {
    data <- data |> filter(time == {{ time }}, A == {{ A }}, estimand == {{ estimand }})
    data <- data[data[[effect_modifier]] == effect_modifier_level,]
  }
  
  # estimate between and within mean sum of squares
  model <- aov(Y ~ as.factor(b), data = data)
  MSB <- summary(model)[[1]]$`Mean Sq`[1]
  MSW <- summary(model)[[1]]$`Mean Sq`[2]
  sigma1 <- (MSB - MSW) / 2
  sigma1 <- ifelse(sigma1 < 0, 0, sigma1)
  sigma2 <- ifelse(sigma1 < 0, var(data$Y), MSW)
  Var <- ((1 + (1/B))*sigma1) + ((1/(B*2))*sigma2)
  part1 <- ((((B+1)/(B*2))^2) * (MSB^2))/(B-1)
  part2 <- MSW^2 / (4*B)
  df <- Var^2 / (part1 + part2)
  
  suppressMessages(data <- data |> group_by(time, A) |> 
                     summarise(estimate = mean(Y)))
  
  if(!is.null(effect_modifier)){
    data[[effect_modifier]] <- effect_modifier_level
  }
  
  data$se <- sqrt(Var)
  data$lower <- ifelse(satterthwaite, 
                       data$estimate - qt(0.975, df)*data$se, 
                       data$estimate - 1.96*data$se)
  data$upper <- ifelse(satterthwaite, 
                       data$estimate + qt(0.975, df)*data$se, 
                       data$estimate + 1.96*data$se)
  if(satterthwaite){
    data$df <- df
  }
  data$estimand <- estimand
  return(data)
}