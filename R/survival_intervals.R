## Bootstrap confidence intervals ##
add_intervals_survival <- function(result, satterthwaite = satterthwaite) {
  result <- bind_rows(result, .id = "b")
  result$b <- as.numeric(as.factor(result$b))
  setnames(result, "interv", "A", skip_absent = TRUE)
  if (max(result$A) == "3") {
    result$A <- ifelse(result$estimand == "AT",
                       as.character(as.numeric(result$A) - 1),
                       result$A)
  }
  result <- na.omit(result)
  
  params <-
    expand_grid(
      unique(result$estimand),
      unique(result$A),
      unique(result$time),
      c("Y", "RR", "RD")
    ) |>
    set_names(c("estimand", "A", "time", "var"))
  
  params <- filter(params, !(A == "2" & estimand == "ITT"))
  
  params <- list(
    estimand = params$estimand,
    A = params$A,
    time = params$time,
    var = params$var
  )
  
  out <- pmap(params,
              get_variance_survival,
              data = result,
              satterthwaite = satterthwaite)
  
  bind_rows(out) |>
    pivot_wider(
      id_cols = c(A, time, estimand),
      values_from = c(Y, se, lower, upper),
      names_from = var
    ) |>
    rename(Y = "Y_Y", RD = "Y_RD", RR = "Y_RR")
}

get_variance_survival <- function(data, time, estimand, A, var, satterthwaite) {
  data$Y <- data[[var]]
  
  B <- max(as.numeric(data$b))
  # by time and treatment
  data <- data |> filter(time == {{ time }}, A == {{ A }}, estimand == {{ estimand }})
  # estimate between and within mean sum of squares
  model <- aov(Y ~ as.factor(b), data = data)
  MSB <- summary(model)[[1]]$`Mean Sq`[1]
  MSW <- summary(model)[[1]]$`Mean Sq`[2]
  sigma1 <- (MSB - MSW) / 2
  sigma1 <- ifelse(sigma1 < 0, 0, sigma1)
  sigma2 <- ifelse(sigma1 < 0, var(data$Y), MSW)
  variance <- ((1 + (1 / B)) * sigma1) + ((1 / (B * 2)) * sigma2)
  part1 <- ((((B + 1) / (B * 2)) ^ 2) * (MSB ^ 2)) / (B - 1)
  part2 <- MSW ^ 2 / (4 * B)
  df <- variance ^ 2 / (part1 + part2)
  suppressMessages(data <- data |> group_by(time, A) |>
                     summarise(Y = mean(Y, na.rm = T)) |>
                     ungroup())
  data$se <- sqrt(variance)
  data$lower <-
    ifelse(satterthwaite,
           data$Y - qt(0.975, df) * data$se,
           data$Y - 1.96 * data$se)
  data$upper <-
    ifelse(satterthwaite,
           data$Y + qt(0.975, df) * data$se,
           data$Y + 1.96 * data$se)
  
  data$var <- var
  data$estimand <- estimand
  
  data <- data |> select(estimand, A, time, Y, se, lower, upper, var)
  
  if (satterthwaite) {
    data$df <- df
  }
  
  return(data)
}
