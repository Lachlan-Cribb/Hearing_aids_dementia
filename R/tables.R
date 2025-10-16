## Summary table for survival results
get_surv_table <- function(dem_results, imp_results){
  dem_results <- 
    dem_results |> 
    filter(time == 6) |> 
    mutate(across(where(is.numeric), round, 3)) |> 
    mutate(Y = paste0(Y, " (", lower_Y, ", ", upper_Y, ")"),
           RR = paste0(RR, " (", lower_RR, ", ", upper_RR, ")"),
           RD = paste0(RD, " (", lower_RD, ", ", upper_RD, ")")) |> 
    mutate(Outcome = "Dem") |> 
    dplyr::select(Outcome, estimand, A, Y, RR, RD)

  imp_results <- 
    imp_results |> 
    filter(time == 6) |> 
    mutate(across(where(is.numeric), round, 3)) |> 
    mutate(Y = paste0(Y, " (", lower_Y, ", ", upper_Y, ")"),
           RR = paste0(RR, " (", lower_RR, ", ", upper_RR, ")"),
           RD = paste0(RD, " (", lower_RD, ", ", upper_RD, ")")) |> 
    mutate(Outcome = "Imp") |> 
    dplyr::select(Outcome, estimand, A, Y, RR, RD)
  
  bind_rows(dem_results, imp_results)
}