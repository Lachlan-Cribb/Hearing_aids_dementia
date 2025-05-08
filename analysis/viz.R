#### Cognition ####

plot_estimates <- function(estimates,
                           contrasts,
                           comparator = "Do not use hearing aids",
                           as_treated = TRUE) {
  
  #### ITT
  
  estimates_itt <- estimates$itt
  estimates_itt$time <- estimates_itt$time - 3
  estimates_itt$A <- as.factor(estimates_itt$A)
  levels(estimates_itt$A) <-
    c(comparator, "Use hearing aids")
  
  itt_plot <- ggplot(estimates_itt,
                     aes(x = time, y = estimate, colour = A)) +
    geom_point(size = 2.25,
               position = position_dodge(0.3)) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(0.3),
      width = 0.1,
      alpha = 0.5,
      linetype = "dashed"
    ) +
    labs(x = "Time (years)",
         colour = "") +
    theme_default(base_size=15) +
    theme(
      legend.position = "bottom",
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5))
    ) +
    scale_colour_manual(values = c("black", "#bc3c24")) +
    scale_x_continuous(breaks = 0:7)
  
  ### Mean difference
  
  contrasts_itt <- contrasts$itt
  contrasts_itt$time <- contrasts_itt$time - 3
  
  itt_md_plot <- ggplot(contrasts_itt, aes(x = time, y = estimate)) +
    geom_point(size = 1.5,
               position = position_dodge(0.3)) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(0.3),
      width = 0.1,
      alpha = 0.5,
      linetype = "dashed"
    ) +
    geom_hline(aes(yintercept = 0), linetype = "longdash", colour = "grey") +
    labs(x = "Time (years)", 
         y = "Mean difference") +
    theme_default(base_size=15) +
    theme(
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5))
    ) +
    scale_x_continuous(breaks = 0:7)
  
  #### Dose response 
  
  if (isTRUE(as_treated)) {

    estimates_at <- estimates$at
    estimates_at$time <- estimates_at$time - 3
    estimates_at$A <- as.factor(estimates_at$A)
    levels(estimates_at$A) <-
      c("Never use HAs", "Rarely/sometimes use HAs", "Often/always use HAs")
    
    at_plot <-
      ggplot(estimates_at, aes(x = time, y = estimate, colour = A)) +
      geom_point(size = 2.25,
                 position = position_dodge(0.3)) +
      geom_errorbar(
        aes(ymin = lower, ymax = upper),
        position = position_dodge(0.3),
        width = 0.1,
        alpha = 0.5,
        linetype = "dashed"
      ) +
      labs(x = "Time (years)",
           colour = "") +
      theme_default(base_size=15) +
      theme(
        legend.position = "bottom",
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5))
      ) +
      scale_colour_manual(values = c("black", "#0072b1", "#bc3c24")) +
      scale_x_continuous(breaks = 0:7)
    
    ## mean difference 
    
    contrasts_at <- contrasts$at
    contrasts_at$time <- contrasts_at$time - 3

    at_md_plot <-
      ggplot(contrasts_at, aes(x = time, y = estimate)) +
      geom_point(size = 2.25,
                 position = position_dodge(0.3)) +
      geom_errorbar(
        aes(ymin = lower, ymax = upper),
        position = position_dodge(0.3),
        width = 0.1,
        alpha = 0.5,
        linetype = "dashed"
      ) +
      geom_hline(aes(yintercept = 0), linetype = "longdash", colour = "grey") +
      labs(x = "Time (years)",
           y = "Mean difference (Often/Always - Never)") +
      theme_default(base_size=15) +
      theme(
        legend.position = "bottom",
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5))
      ) +
      scale_x_continuous(breaks = 0:7)
    
    return(list(itt_plot = itt_plot, itt_md_plot = itt_md_plot, 
                at_plot = at_plot, at_md_plot = at_md_plot))
  } else {
    return(list(itt_plot = itt_plot, itt_md_plot = itt_md_plot))
  }
}


### save cognitive plots 


save_cog_plot <- function(plots, 
                          file_name, 
                          ylab, 
                          limits, 
                          breaks,
                          md_limits,
                          md_breaks,
                          as_treated = TRUE) {
  
  # ITT
  
  itt_plot <- 
    plots$itt_plot + scale_y_continuous(breaks = breaks, 
                                        limits = limits) +
    labs(y = ylab)
  
  itt_md_plot <- 
    plots$itt_md_plot + scale_y_continuous(breaks = md_breaks, 
                                           limits = md_limits)
  
  itt_plot <- (itt_plot + itt_md_plot) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  # dose response
  
  if(isTRUE(as_treated)){
    at_plot <- 
      plots$at_plot + scale_y_continuous(breaks = breaks, 
                                         limits = limits) +
      labs(y = ylab)
    
    at_md_plot <- 
      plots$at_md_plot + scale_y_continuous(breaks = md_breaks, 
                                            limits = md_limits)
    
    at_plot <- (at_plot + at_md_plot) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
    
    plot <- cowplot::plot_grid(
      itt_plot,
      at_plot,
      nrow = 2,
      labels = c("A","B"),
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
  } else {
    ggsave(
      paste0("figures/", file_name),
      itt_plot,
      device = "png",
      width = 8,
      height = 6,
      dpi = 400,
      bg = "white")
  }
}


#### Survival ####

plot_cumulative_inc <- function(estimates, 
                                comparator = "Do not use hearing aids",
                                intervals = TRUE,
                                ratios = FALSE){
  
  #### ITT 
  
  itt <- estimates$itt |> dplyr::select(A, time, Y, lower_Y, upper_Y,
                                 RR, lower_RR, upper_RR)
  
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
    itt <- itt |> filter(A == "Use hearing aids")
    
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
  
  at <- estimates$at |> dplyr::select(A, time, Y, lower_Y, upper_Y,
                                 RR, lower_RR, upper_RR)
  
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
    at <- at |> filter(A == "Often/always use HAs")
    
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
  
  return(list(itt_plot = itt_plot, at_plot = at_plot))
}


#### Save plot ####

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

#### Effect modification plot

save_effectmod_plot <- function(result,
                                effect_modifier,
                                comparator = "Do not use hearing aids",
                                labels, 
                                ylab, 
                                file_name, 
                                breaks,
                                limits){
  
  ## ITT 
  estimates_itt <- result$estimates$itt  
  estimates_itt$time <- estimates_itt$time - 3
  estimates_itt$A <- as.factor(estimates_itt$A)
  levels(estimates_itt$A) <-
    c(comparator, "Use hearing aids")
  estimates_itt$effect_modifier <- estimates_itt[[effect_modifier]]
  
  ## as treated
  estimates_at <- result$estimates$at
  estimates_at$time <- estimates_at$time - 3
  estimates_at$A <- as.factor(estimates_at$A)
  levels(estimates_at$A) <-
    c("Never use HAs", "Rarely/sometimes use HAs", "Often/always use HAs")
  estimates_at$effect_modifier <- estimates_at[[effect_modifier]]  
    
  new_labels <- c(first = labels[1], 
                  second = labels[2])
  
  plot <-
    ## ITT
    (
      ggplot(estimates_itt,
             aes(
               x = time, y = estimate, colour = A
             )) +
        geom_point(size = 2.25,
                   position = position_dodge(0.3)) +
        geom_errorbar(
          aes(ymin = lower, ymax = upper),
          position = position_dodge(0.3),
          width = 0.1,
          alpha = 0.5,
          linetype = "dashed"
        ) +
        labs(x = "Time (years)",
             colour = "") +
        theme_default(base_size = 15) +
        theme(
          legend.position = "bottom",
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(linewidth = rel(0.5))
        ) +
        scale_colour_manual(values = c("black", "#bc3c24")) +
        scale_x_continuous(breaks = 0:7) +
        ylab(ylab) +
        facet_wrap( ~ effect_modifier,
                    labeller = labeller(effect_modifier = new_labels)) +
        xlab("") +
        theme(legend.margin = margin(c(0, 0, 0, 0))) +
        scale_y_continuous(breaks = breaks, limits = limits) +
        
        ## AT
        ggplot(estimates_at, aes(x = time, y = estimate, colour = A)) +
        geom_point(size = 2.25,
                   position = position_dodge(0.3)) +
        geom_errorbar(
          aes(ymin = lower, ymax = upper),
          position = position_dodge(0.3),
          width = 0.1,
          alpha = 0.5,
          linetype = "dashed"
        ) +
        labs(x = "Time (years)",
             colour = "") +
        theme_default(base_size=15) +
        theme(
          legend.position = "bottom",
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(linewidth = rel(0.5))
        ) +
        scale_colour_manual(values = c("black", "#0072b1", "#bc3c24")) +
        scale_x_continuous(breaks = 0:7) +
        ylab(ylab) +
        facet_wrap( ~ effect_modifier,
                    labeller = labeller(effect_modifier = new_labels)) +
        theme(legend.margin = margin(c(0, 0, 0, 0))) +
        scale_y_continuous(breaks = breaks, limits = limits) +
        plot_layout(nrow = 2)
    )
  
  ggsave(paste0("figures/", file_name), 
         plot,
         device = "png",
         width = 10,
         height = 10,
         bg = "white")
}


#### G-formula vs IPW ####

gform_vs_ipw <- function(gform,
                         ipw,
                         comparator = "Do not use hearing aids",
                         as_treated = TRUE) {
  
  #### ITT
  gform_itt <- gform$risks$itt
  ipw_itt <- ipw$itt
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
    
    gform_at <- gform$risks$at
    gform_at$A <- as.character(as.numeric(gform_at$A) + 1)
    ipw_at <- ipw$at
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
