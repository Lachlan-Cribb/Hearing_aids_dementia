#### Survival outcome plots ####
## Save survival plots

save_survival_plots <- function(dem, imp, death, cancer){
  save_cuminc_plot(
    dem,
    file_name = "Dementia.png",
    y_label = "Dementia risk",
    breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125),
    limits = c(0, 0.13)
  )
  
  save_cuminc_plot(
    imp,
    file_name = "Impairment.png",
    y_label = "Cognitive impairment risk",
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
    limits = c(0, 0.51)
  )
  
  save_cuminc_plot(
    death,
    file_name = "death.png",
    y_label = "Risk of death",
    breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
    limits = c(0,0.25)
  )
  
  save_cuminc_plot(
    cancer,
    file_name = "cancer.png",
    y_label = "Cancer risk",
    breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
    limits = c(0,0.25)
  )
}

## Save plot function ##

save_cuminc_plot <- function(plots, 
                             file_name,
                             y_label,
                             ratios = FALSE,
                             breaks = seq(0,1,0.1),
                             limits = c(0,1),
                             rr_breaks = NULL,
                             rr_limits = NULL,
                             legend.position = "bottom"){
  
  ## ITT plot
  
  itt_plot <- 
    plots$itt_plot + scale_y_continuous(breaks = breaks, 
                                        limits = limits) +
    labs(y = y_label)
  
  if(isTRUE(ratios)){
    itt_rr_plot <- 
      plots$itt_rr_plot + scale_y_continuous(breaks = rr_breaks, 
                                             limits = rr_limits)
    
    itt_plot <- (itt_plot + itt_rr_plot) +
      plot_layout(guides = "collect") &
      theme(legend.position = legend.position)
  }
  
  ## AT plot
  
  at_plot <- 
    plots$at_plot + scale_y_continuous(breaks = breaks, 
                                       limits = limits) +
    theme(legend.position = "none") +
    labs(y = y_label)
  
  if(isTRUE(ratios)){
    at_rr_plot <- 
      plots$at_rr_plot + scale_y_continuous(breaks = rr_breaks, 
                                            limits = rr_limits) +
      theme(legend.position = "none")
    
    at_plot <- (at_plot + at_rr_plot) +
      plot_layout(guides = "collect") &
      theme(legend.position = legend.position)
  }
  
  ## combine plots
  
  
  plot <- cowplot::plot_grid(
    itt_plot,
    at_plot + theme(legend.position = legend.position),
    nrow = 2,
    labels = c("A", "B"),
    label_size = 16
  )
  
  ggsave(
    paste0("figures/", file_name),
    plot,
    device = "png",
    width = 10,
    height = 10,
    dpi = 400,
    bg = "white"
  )
  
  return(plot)
}

## Cumulative incidence plot

plot_cumulative_inc <- function(estimates, 
                                comparator = "Do not use hearing aids",
                                intervals = TRUE,
                                ratios = FALSE){
  
  #### ITT 
  
  itt <- estimates |> 
    filter(estimand == "ITT" & !A == "1_0") |> 
    select(A, time, Y, lower_Y, upper_Y, RR, lower_RR, upper_RR)
  
  itt$time <- itt$time + 1
  
  itt$A <- as.factor(itt$A)
  levels(itt$A) <- c(comparator, "Use hearing aids")
  
  itt$colour <- ifelse(itt$A == comparator, "black", "#bc3c24")
  
  itt$colour <- factor(itt$colour, c("black", "#bc3c24"))
  
  itt$lower_Y <- ifelse(itt$lower_Y < 0, 0, itt$lower_Y)
  
  itt_plot <- 
    ggplot(itt, aes(x = time, y = Y)) +
    geom_line(aes(colour = colour), linewidth = 0.5,
              position = position_dodge(0.3)) +
    geom_point(aes(colour = colour), size = 2.25,
               position = position_dodge(0.3)) +    
    # geom_ribbon(aes(ymin = lower_Y, ymax = upper_Y, fill = A),
    #             alpha = 0.3, show.legend = FALSE) +
    {if(isTRUE(intervals))
      geom_errorbar(aes(ymin = lower_Y, ymax = upper_Y, colour = colour),
                    width = 0.1, alpha = 0.5, linetype = "dashed",
                    position = position_dodge(0.3))
    } +
    scale_colour_identity(name = "",
                          labels = levels(itt$A),
                          guide = "legend") +
    labs(x= "Time (years)") +
    theme_default(base_size=15) +
    theme(legend.position = "bottom",
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(linewidth = rel(0.5))) +
    scale_x_continuous(breaks = 1:max(itt$time))
  
  
  ### Risk ratios
  
  if(isTRUE(ratios)){
    itt <- estimates |> 
      filter(A == "1_0") |> 
      select(A, time, Y, lower_Y, upper_Y, RR, lower_RR, upper_RR)
    
    itt_rr <- 
      ggplot(itt, aes(x = time, y = RR)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 2.25) +    
      # geom_ribbon(aes(ymin = lower_Y, ymax = upper_Y, fill = A),
      #             alpha = 0.3, show.legend = FALSE) +
      {if(isTRUE(intervals))
        geom_errorbar(aes(ymin = lower_RR, ymax = upper_RR),
                      width = 0.1, alpha = 0.5, linetype = "dashed")
      } +
      geom_hline(aes(yintercept = 1), linetype = "longdash", colour = "grey") +
      labs(x= "Time (years)", y = "Risk ratio", colour = "") +
      theme_default(base_size=15) +
      theme(panel.grid = element_line(colour = "grey92"),
            panel.grid.minor = element_line(linewidth = rel(0.5))) +
      scale_x_continuous(breaks = 1:max(itt$time))
  }
  
  ### Dose response
  
  at <- estimates |> 
    filter(estimand == "AT" & !A == "3_1") |> 
    select(A, time, Y, lower_Y, upper_Y, RR, lower_RR, upper_RR)
  
  at$time <- at$time + 1
  
  at$A <- as.factor(at$A)
  levels(at$A) <- c("Never use HAs", 
                    "Rarely/sometimes use HAs", 
                    "Often/always use HAs")
  
  at$colour <- ifelse(
    at$A == "Never use HAs",
    "black",
    ifelse(at$A == "Rarely/sometimes use HAs", "#0072b1",
           "#bc3c24")
  )
  
  at$colour <- factor(at$colour, c("black", "#0072b1", "#bc3c24"))
  
  at$lower_Y <- ifelse(at$lower_Y < 0, 0, at$lower_Y)
  
  at_plot <- 
    ggplot(at, aes(x = time, y = Y)) +
    geom_line(aes(colour = colour), linewidth = 0.5,
              position = position_dodge(0.3)) +
    geom_point(aes(colour = colour), size = 2.25,
               position = position_dodge(0.3)) +    
    # geom_ribbon(aes(ymin = lower_Y, ymax = upper_Y, fill = A),
    #             alpha = 0.3, show.legend = FALSE) +
    {if(isTRUE(intervals))
      geom_errorbar(aes(ymin = lower_Y, ymax = upper_Y, colour = colour),
                    width = 0.1, alpha = 0.5, linetype = "dashed",
                    position = position_dodge(0.3))
    } +
    scale_colour_identity(name = "",
                          labels = levels(at$A),
                          guide = "legend") +
    labs(x= "Time (years)") +
    theme_default(base_size=15) +
    theme(legend.position = "bottom",
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(linewidth = rel(0.5))) +
    scale_x_continuous(breaks = 1:max(itt$time))
  
  ### Risk ratio
  
  if(isTRUE(ratios)){
    at <- estimates |> 
      filter(A == "3_1") |> 
      select(A, time, Y, lower_Y, upper_Y, RR, lower_RR, upper_RR)
    
    at_rr <- 
      ggplot(at, aes(x = time, y = RR)) +
      geom_line(linewidth = 0.5,
                position = position_dodge(0.3)) +
      geom_point(size = 2.25,
                 position = position_dodge(0.3)) +    
      # geom_ribbon(aes(ymin = lower_Y, ymax = upper_Y, fill = A),
      #             alpha = 0.3, show.legend = FALSE) +
      {if(isTRUE(intervals))
        geom_errorbar(aes(ymin = lower_RR, ymax = upper_RR),
                      width = 0.1, alpha = 0.5, linetype = "dashed",
                      position = position_dodge(0.3))
      } +
      geom_hline(aes(yintercept = 1), linetype = "longdash", colour = "grey") +
      labs(x= "Time (years)", y = "Risk ratio (often or always / never)") +
      scale_colour_identity() +
      theme_default(base_size=15) +
      theme(panel.grid = element_line(colour = "grey92"),
            panel.grid.minor = element_line(linewidth = rel(0.5))) +
      scale_x_continuous(breaks = 1:max(itt$time))
    
    return(list(itt_plot = itt_plot, itt_rr_plot = itt_rr, 
                at_plot = at_plot, at_rr_plot = at_rr))
  }

  list(itt_plot = itt_plot, at_plot = at_plot)
}

#### G-formula vs IPW ####

save_ipw_plots <- function(dem, imp){
  save_cuminc_plot(dem, 
                   "Dem_gform_ipw.png",
                   "Dementia risk",
                   ratios = FALSE,
                   breaks = c(0,0.02,0.04,0.06,0.08,0.1),
                   limits = c(0,0.1),
                   legend.position = "right")
  
  save_cuminc_plot(imp, 
                   "Imp_gform_ipw.png",
                   "Cognitive impairment risk",
                   ratios = FALSE,
                   breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                   limits = c(0, 0.5),
                   legend.position = "right")
  
  
}

gform_vs_ipw <- function(gform,
                         ipw,
                         comparator = "Do not use hearing aids",
                         as_treated = TRUE) {
  
  #### ITT
  gform_itt <- gform |> filter(estimand == "ITT" & !A == "1_0")
  ipw_itt <- ipw |> filter(estimand == "ITT" & !A == "1_0")
  itt_plot <- bind_rows(gform_itt, ipw_itt, .id = "Estimator")
  itt_plot$Estimator <- as.factor(itt_plot$Estimator)
  levels(itt_plot$Estimator) <- c("g-formula", "IPW")
  itt_plot$A <- as.factor(itt_plot$A)
  levels(itt_plot$A) <- c(comparator, "Use hearing aids")
  
  itt_plot <- 
    ggplot(itt_plot,
           aes(x = time, y = Y, 
               colour = A, 
               shape = Estimator,
               linetype = Estimator)) +
    geom_point(size = 2.25,
               position = position_dodge(0.3)) +
    geom_line(linewidth = 0.5,
              position = position_dodge(0.3)) +
    labs(x = "Time (years)",
         colour = "",
         shape = "Method",
         linetype = "Method") +
    theme_default(base_size=15) +
    guides(shape = guide_legend(order = 2),
           linetype = guide_legend(order = 2),
           colour = guide_legend(order = 1)) +
    theme(
      legend.position = "right",
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5))
    ) +
    scale_colour_manual(values = c("black", "#bc3c24")) +
    scale_x_continuous(breaks = 0:7)
  
  #### Dose response 
  
  if (isTRUE(as_treated)) {
    
    gform_at <- gform |> filter(estimand == "AT" & !A == "3_1")
    ipw_at <- ipw |> filter(estimand == "AT" & !A == "3_1")
    gform_at$A <- as.character(as.numeric(gform_at$A) + 1)
    ipw_at$A <- as.character(as.numeric(ipw_at$A) + 1)
    at_plot <- bind_rows(gform_at, ipw_at, .id = "Estimator")
    at_plot$Estimator <- as.factor(at_plot$Estimator)
    levels(at_plot$Estimator) <- c("g-formula", "IPW")
    at_plot$A <- as.factor(at_plot$A)
    levels(at_plot$A) <- 
      c("Never use HAs", "Rarely/sometimes use HAs", "Often/always use HAs")
    
    at_plot <- 
      ggplot(at_plot,
             aes(x = time, y = Y, 
                 colour = A, 
                 shape = Estimator,
                 linetype = Estimator)) +
      geom_point(size = 2.25,
                 position = position_dodge(0.3)) +
      geom_line(linewidth = 0.5,
                position = position_dodge(0.3)) +
      labs(x = "Time (years)",
           colour = "",
           shape = "Method",
           linetype = "Method") +
      theme_default(base_size=15) +
      theme(
        legend.position = "right",
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5))
      ) +
      scale_colour_manual(values = c("black", "#0072b1", "#bc3c24")) +
      scale_x_continuous(breaks = 0:7)
    
    return(list(itt_plot = itt_plot, at_plot = at_plot))
  } else {
    return(list(itt_plot = itt_plot))
  }
}

#### HEARING ELIGIBILITY SENSITIBITY ####

save_plots_hearing <- function(g, dem, imp){
  save_cog_plot(
    g,
    file_name = "g_sensitivity_hearing_eligibility.png",
    ylab = "Mean overall cognition score",
    breaks = c(-1.25, -1, -0.75, -0.5, -0.25, 0, 0.25),
    limits = c(-1.25, 0.25),
    md_breaks = c(-0.3,-0.2,-0.1, 0, 0.1, 0.2, 0.3, 0.4),
    md_limits = c(-0.3,0.4)
  )
  
  save_cuminc_plot(
    dem,
    file_name = "Dementia_sensitivity_hearing_eligibility.png",
    y_label = "Dementia risk",
    breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15),
    limits = c(0, 0.15),
    rr_breaks = c(0.5,0.75,1,1.25, 1.5),
    rr_limits = c(0.5,1.5)
  )
  
  save_cuminc_plot(
    imp,
    file_name = "impairment_hearing_eligibility.png",
    y_label = "Cognitive impairment risk",
    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    limits = c(0, 0.6),
    rr_breaks = c(0.5, 0.75, 1, 1.25, 1.5),
    rr_limits = c(0.5,1.55)
  )
}

