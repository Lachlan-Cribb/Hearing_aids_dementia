## function for saving all cognition plots 
save_cognition_plots <- function(g, ms, cowat, total, delayed, ret, rdi, sdmt){
  #g
  save_cog_plot(
    g,
    file_name = "g_with_md.png",
    ylab = "Mean overall cognition score",
    breaks = c(-1, -0.75, -0.5, -0.25, 0, 0.25),
    md_breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3),
    limits = c(-1, 0.3),
    md_limits = c(-0.3,0.3))
  
  #3ms
  save_cog_plot(
    ms,
    file_name = "3ms_with_md.png",
    ylab = "Mean 3MS overall score",
    breaks = c(91, 92, 93, 94, 95),
    md_breaks = c(-1,-0.5,0,0.5,1,1.5),
    limits = c(90.75, 95.25),
    md_limits = c(-1,1.5))
  
  #cowat
  save_cog_plot(
    cowat,
    file_name = "cowat_with_md.png",
    ylab = "Mean COWAT score",
    breaks = c(12, 12.5, 13, 13.5, 14, 14.5),
    limits = c(12, 14.5),
    md_breaks = c(-0.4,-0.2,0,0.2,0.4,0.6, 0.8),
    md_limits = c(-0.4, 0.8))
  
  #total recall
  save_cog_plot(
    total,
    file_name = "hvlt_recall_with_md.png",
    ylab = "Mean HVLT total recall",
    breaks = c(21, 22, 23, 24, 25),
    limits = c(20.9, 25),
    md_breaks = c(-1,-0.5, 0, 0.5, 1),
    md_limits = c(-1, 1))
  
  #delayed recall
  save_cog_plot(
    delayed,
    file_name = "hvlt_delayrecall_with_md.png",
    ylab = "Mean HVLT delayed recall",
    breaks = c(6.5, 7, 7.5, 8, 8.5, 9),
    limits = c(6.5, 9),
    md_breaks = c(-0.5,-0.25, 0, 0.25, 0.5),
    md_limits = c(-0.5, 0.5)
  )
  
  # retention
  save_cog_plot(
    ret,
    file_name = "hvlt_retention_with_md.png",
    ylab = "Mean HVLT retention",
    breaks = c(85, 90, 95, 100, 105, 110),
    limits = c(85, 110),
    md_breaks = c(-4, -2, 0, 2, 4, 6, 8, 10),
    md_limits = c(-4, 10)
  )
  
  # rdi
  save_cog_plot(
    rdi,
    file_name = "hvlt_rdi_with_md.png",
    ylab = "Mean HVLT RDI",
    breaks = c(9.5, 10, 10.5, 11),
    limits = c(9.5, 11),
    md_breaks = c(-0.2,0,0.2, 0.4),
    md_limits = c(-0.2, 0.4)
  )
  
  # sdmt
  save_cog_plot(
    sdmt,
    file_name = "SDMT_with_md.png",
    ylab = "Mean SDMT score",
    breaks = c(28, 30, 32, 34, 36, 38),
    limits = c(26.5, 38.5),
    md_breaks = c(-1,-0.5, 0, 0.5, 1, 1.5),
    md_limits = c(-1, 1.5)
  )
}

## save cognitive plots 

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

# plot cognition estimates

plot_estimates <- function(estimates, comparator = "Do not use hearing aids") {
  
  ### ITT
  estimates_itt <- estimates |> filter(estimand == "ITT" & !A == "1_0")
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
  
  ## Mean difference
  
  contrasts_itt <- estimates |> filter(A == "1_0")
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
  
  ### Dose response
  
  estimates_at <- estimates |> filter(estimand == "AT" & !A == "3_1")
  estimates_at$time <- estimates_at$time - 3
  estimates_at$A <- as.factor(estimates_at$A)
  levels(estimates_at$A) <-
    c("Never use HAs",
      "Rarely/sometimes use HAs",
      "Often/always use HAs")
  
  at_plot <-
    ggplot(estimates_at, aes(x = time, y = estimate, colour = A)) +
    geom_point(size = 2.25, position = position_dodge(0.3)) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(0.3),
      width = 0.1,
      alpha = 0.5,
      linetype = "dashed"
    ) +
    labs(x = "Time (years)", colour = "") +
    theme_default(base_size = 15) +
    theme(
      legend.position = "bottom",
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5))
    ) +
    scale_colour_manual(values = c("black", "#0072b1", "#bc3c24")) +
    scale_x_continuous(breaks = 0:7)
  
  ## mean difference
  
  contrasts_at <- estimates |> filter(A == "3_1")
  contrasts_at$time <- contrasts_at$time - 3
  
  at_md_plot <-
    ggplot(contrasts_at, aes(x = time, y = estimate)) +
    geom_point(size = 2.25, position = position_dodge(0.3)) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(0.3),
      width = 0.1,
      alpha = 0.5,
      linetype = "dashed"
    ) +
    geom_hline(aes(yintercept = 0),
               linetype = "longdash",
               colour = "grey") +
    labs(x = "Time (years)", y = "Mean difference (Often/Always - Never)") +
    theme_default(base_size = 15) +
    theme(
      legend.position = "bottom",
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5))
    ) +
    scale_x_continuous(breaks = 0:7)
  
  list(
    itt_plot = itt_plot,
    itt_md_plot = itt_md_plot,
    at_plot = at_plot,
    at_md_plot = at_md_plot
  )
}
