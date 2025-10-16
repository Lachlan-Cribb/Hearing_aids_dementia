### Effect modification plot

save_effect_mod_plots <- function(hearing, age, ms, pcs, frailty, gender, e4){
  
  effectmod_plot(
    hearing,
    effect_modifier = "hearing",
    labels = c(
      "Better-ear 4-frequency PTA < 30",
      "Better-ear 4-frequency PTA \u2265 30"
    ),
    ylab = "Mean overall cognition score",
    file_name = "g_by_hearing.png",
    breaks = c(-1.25,-1,-0.75,-0.5,-0.25,0,0.25, 0.5),
    limits = c(-1.25, 0.5)
  )
  
  effectmod_plot(
    age,
    effect_modifier = "age",
    labels = c("Ages [70 - 75)", "Ages [75 - 84]"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_age.png",
    breaks = c(-1.75, -1.5,-1.25, -1, -0.75, -0.5, -0.25, 0, 0.25, 0.5),
    limits = c(-1.75, 0.5)
  )
  
  effectmod_plot(
    ms,
    effect_modifier = "ms",
    labels = c("3MS total score \u2265 95",
               "3MS total score < 95"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_3ms.png",
    breaks = c(-1.75,-1.25,-0.75,-0.25, 0.25, 0.75),
    limits = c(-1.75, 0.8)
  )
  
  effectmod_plot(
    pcs,
    effect_modifier = "pcs",
    labels = c(
      "SF-12 physical component score \u2265 50",
      "SF-12 physical component score < 50"
    ),
    ylab = "Mean overall cognition score",
    file_name = "g_by_pcs.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  effectmod_plot(
    frailty,
    effect_modifier = "frailty",
    labels = c("Frailty DAI50 < 0.09",
               "Frailty DAI50 \u2265 0.09"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_frailty.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  effectmod_plot(
    gender,
    effect_modifier = "gender",
    labels = c("Women",
               "Men"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_gender.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
  
  effectmod_plot(
    e4,
    effect_modifier = "e4",
    labels = c("ApoE4 non-carrier",
               "ApoE4 carrier"),
    ylab = "Mean overall cognition score",
    file_name = "g_by_apoe.png",
    breaks = c(-1.25,-1,-0.75, -0.5,-0.25,0,0.25,0.5),
    limits = c(-1.25, 0.5)
  )
}

effectmod_plot <- function(result,
                           effect_modifier,
                           comparator = "Do not use hearing aids",
                           labels,
                           ylab,
                           file_name,
                           breaks,
                           limits) {
  ## ITT
  estimates_itt <- result |> filter(estimand == "ITT" & !A == "1_0")
  estimates_itt$time <- estimates_itt$time - 3
  estimates_itt$A <- as.factor(estimates_itt$A)
  levels(estimates_itt$A) <-
    c(comparator, "Use hearing aids")
  estimates_itt$effect_modifier <- estimates_itt[[effect_modifier]]
  
  ## as treated
  estimates_at <- result |> filter(estimand == "AT" & !A == "3_1")
  estimates_at$time <- estimates_at$time - 3
  estimates_at$A <- as.factor(estimates_at$A)
  levels(estimates_at$A) <-
    c("Never use HAs",
      "Rarely/sometimes use HAs",
      "Often/always use HAs")
  estimates_at$effect_modifier <- estimates_at[[effect_modifier]]
  
  new_labels <- c(first = labels[1], second = labels[2])
  
  plot <-
    ## ITT
    (
      ggplot(estimates_itt, aes(
        x = time, y = estimate, colour = A
      )) +
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
        scale_colour_manual(values = c("black", "#bc3c24")) +
        scale_x_continuous(breaks = 0:7) +
        ylab(ylab) +
        facet_wrap(~ effect_modifier, labeller = labeller(effect_modifier = new_labels)) +
        xlab("") +
        theme(legend.margin = margin(c(0, 0, 0, 0))) +
        scale_y_continuous(breaks = breaks, limits = limits) +
        
        ## AT
        ggplot(estimates_at, aes(
          x = time, y = estimate, colour = A
        )) +
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
        scale_x_continuous(breaks = 0:7) +
        ylab(ylab) +
        facet_wrap(~ effect_modifier, labeller = labeller(effect_modifier = new_labels)) +
        theme(legend.margin = margin(c(0, 0, 0, 0))) +
        scale_y_continuous(breaks = breaks, limits = limits) +
        plot_layout(nrow = 2)
    )
  
  ggsave(
    paste0("figures/", file_name),
    plot,
    device = "png",
    width = 10,
    height = 10,
    bg = "white"
  )
}