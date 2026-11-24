# MANOVA profile plots and DV correlation plot
# Run from the repository root: Rscript R/02_profile_plots.R

library(tidyverse)
library(dplyr)
dir.create("output", showWarnings = FALSE)

dat_raw <- read.csv("data/entrepreneurship.csv")

dat <- dat_raw %>%
  mutate(
    Gender_f = factor(
      Gender,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    Inclination_f = factor(
      Inclination,
      levels = c(1, 2),
      labels = c("Yes", "No")
    )
  )

dv_names <- c(
  "PBC",
  "Intention",
  "SSN",
  "Attitude",
  "Opportunity",
  "SocialCap",
  "Education",
  "Innova",
  "Locus",
  "Risk"
)

dv_labels <- c(
  PBC = "PBC",
  Intention = "Intention",
  SSN = "SSN",
  Attitude = "Attitude",
  Opportunity = "Opportunity",
  SocialCap = "Social capital",
  Education = "Education",
  Innova = "Innovativeness",
  Locus = "Locus of control",
  Risk = "Risk taking"
)

dv_plot_labels <- c(
  PBC = "PBC",
  Intention = "Intent",
  SSN = "SSN",
  Attitude = "Att",
  Opportunity = "Opp",
  SocialCap = "SocCap",
  Education = "Educ",
  Innova = "Innov",
  Locus = "Locus",
  Risk = "Risk"
)

tc_axis_highlight <- "#001F63"  # TCDarkBlue

dat_analysis <- dat %>%
  filter(if_all(all_of(c("Gender_f", "Inclination_f", dv_names)), ~ !is.na(.x)))

make_profile_data <- function(data, group_var, group_label) {
  group_ns <- data %>%
    count(.data[[group_var]], name = "group_n") %>%
    rename(Group = all_of(group_var))

  data %>%
    dplyr::select(all_of(group_var), all_of(dv_names)) %>%
    pivot_longer(
      cols = all_of(dv_names),
      names_to = "Outcome",
      values_to = "Score"
    ) %>%
    mutate(
      Outcome = factor(Outcome, levels = dv_names, labels = dv_labels[dv_names])
    ) %>%
    group_by(.data[[group_var]], Outcome) %>%
    summarise(
      n = n(),
      mean = mean(Score),
      sd = sd(Score),
      se = sd / sqrt(n),
      ci_low = mean - qt(.975, df = n - 1) * se,
      ci_high = mean + qt(.975, df = n - 1) * se,
      .groups = "drop"
    ) %>%
    rename(Group = all_of(group_var)) %>%
    left_join(group_ns, by = "Group") %>%
    mutate(Analysis = group_label)
}

profile_gender <- make_profile_data(
  data = dat_analysis,
  group_var = "Gender_f",
  group_label = "Gender"
)

profile_inclination <- make_profile_data(
  data = dat_analysis,
  group_var = "Inclination_f",
  group_label = "Entrepreneurial inclination"
) %>%
  mutate(
    Group = factor(
      Group,
      levels = c("Yes", "No"),
      labels = c("Inclined", "Not inclined")
    )
)

write.csv(profile_gender, "output/manova_profile_plot_data_gender.csv", row.names = FALSE)
write.csv(profile_inclination, "output/manova_profile_plot_data_inclination.csv", row.names = FALSE)

plot_profile <- function(plot_data, output_file, linetype_values, shape_values,
                         color_values, highlight_outcomes = character(0),
                         axis_highlight_color = tc_axis_highlight) {
  group_counts <- plot_data %>%
    distinct(Group, group_n)

  axis_labels <- dv_plot_labels[dv_names]
  axis_label_df <- tibble(
    Outcome = factor(dv_labels[dv_names], levels = dv_labels[dv_names]),
    Label = unname(axis_labels),
    Highlight = dv_names %in% highlight_outcomes
  )

  legend_labels <- paste0(
    names(linetype_values),
    " (n = ",
    group_counts$group_n[match(names(linetype_values), group_counts$Group)],
    ")"
  )

  plot_data <- plot_data %>%
    mutate(
      Group_label = paste0(Group, " (n = ", group_n, ")"),
      Group_label = factor(Group_label, levels = legend_labels),
      Outcome = factor(
        as.character(Outcome),
        levels = dv_labels[dv_names]
      )
    )

  names(linetype_values) <- legend_labels
  names(shape_values) <- names(linetype_values)
  names(color_values) <- names(linetype_values)

  dodge <- position_dodge(width = 0.16)

  p <- ggplot(
    plot_data,
    aes(
      x = Outcome,
      y = mean,
      group = Group_label,
      color = Group_label,
      linetype = Group_label,
      shape = Group_label
    )
  ) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      position = dodge,
      width = 0.13,
      linewidth = 0.38,
      alpha = 0.42
    ) +
    geom_line(position = dodge, linewidth = 0.72) +
    geom_point(position = dodge, fill = "white", size = 2.6, stroke = 0.85) +
    geom_text(
      data = filter(axis_label_df, !Highlight),
      aes(x = Outcome, y = 0.76, label = Label),
      inherit.aes = FALSE,
      color = "black",
      size = 4.0,
      vjust = 1,
      show.legend = FALSE
    ) +
    geom_text(
      data = filter(axis_label_df, Highlight),
      aes(x = Outcome, y = 0.76, label = Label),
      inherit.aes = FALSE,
      color = "black",
      fontface = "plain",
      size = 4.0,
      vjust = 1,
      show.legend = FALSE
    ) +
    scale_color_manual(values = color_values) +
    scale_linetype_manual(values = linetype_values) +
    scale_shape_manual(values = shape_values) +
    scale_y_continuous(
      breaks = 1:7,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_x_discrete(
      labels = function(x) rep("", length(x))
    ) +
    coord_cartesian(
      ylim = c(1, 7),
      clip = "off"
    ) +
    labs(
      x = NULL,
      y = "Mean score (1-7)",
      linetype = NULL,
      shape = NULL
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(
          linetype = unname(linetype_values),
          shape = unname(shape_values),
          linewidth = 0.72,
          size = 2.8
        )
      ),
      linetype = "none",
      shape = "none"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12, color = "black", margin = margin(r = 6)),
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(size = 12, color = "black"),
      plot.margin = margin(6, 8, 30, 8)
    )

  ggsave(
    filename = output_file,
    plot = p,
    width = 5.8,
    height = 3.7,
    dpi = 450,
    bg = "white"
  )
}

plot_profile(
  plot_data = profile_gender,
  output_file = "output/manova_profile_plot_gender.png",
  linetype_values = c("Male" = "solid", "Female" = "dashed"),
  shape_values = c("Male" = 16, "Female" = 17),
  color_values = c("Male" = "#1A1A1A", "Female" = "#757575"),  # greyscale
  highlight_outcomes = c("PBC", "SSN", "Innova", "Education"),
  axis_highlight_color = "black"
)

plot_profile(
  plot_data = profile_inclination,
  output_file = "output/manova_profile_plot_inclination.png",
  linetype_values = c("Inclined" = "solid", "Not inclined" = "dashed"),
  shape_values = c("Inclined" = 16, "Not inclined" = 17),
  color_values = c("Inclined" = "#005FBE", "Not inclined" = "#E03C39"),
  highlight_outcomes = c("Intention")
)

# Black-and-white copies for paper appendices. These use the same geometry,
# labels, Ns, confidence intervals, and highlighted outcomes as the slide plots,
# but rely only on line type, point shape, and bold axis labels.
plot_profile(
  plot_data = profile_gender,
  output_file = "output/manova_profile_plot_gender_bw.png",
  linetype_values = c("Male" = "solid", "Female" = "dashed"),
  shape_values = c("Male" = 16, "Female" = 17),
  color_values = c("Male" = "black", "Female" = "black"),
  highlight_outcomes = c("PBC", "SSN", "Innova", "Education"),
  axis_highlight_color = "black"
)

plot_profile(
  plot_data = profile_inclination,
  output_file = "output/manova_profile_plot_inclination_bw.png",
  linetype_values = c("Inclined" = "solid", "Not inclined" = "dashed"),
  shape_values = c("Inclined" = 16, "Not inclined" = 17),
  color_values = c("Inclined" = "black", "Not inclined" = "black"),
  highlight_outcomes = c("Intention"),
  axis_highlight_color = "black"
)

# Correlation heatmap across the dependent variables.
# This uses the same analytic sample and DV order as the MANOVA models.
make_dv_correlation_plot <- function(data, output_file) {
  corr_mat <- data %>%
    dplyr::select(all_of(dv_names)) %>%
    cor(use = "pairwise.complete.obs")

  corr_data <- as.data.frame(as.table(corr_mat)) %>%
    rename(
      Outcome_x = Var1,
      Outcome_y = Var2,
      r = Freq
    ) %>%
    mutate(
      x_index = match(Outcome_x, dv_names),
      y_index = match(Outcome_y, dv_names),
      Outcome_x = factor(
        Outcome_x,
        levels = dv_names,
        labels = dv_plot_labels[dv_names]
      ),
      Outcome_y = factor(
        Outcome_y,
        levels = rev(dv_names),
        labels = rev(dv_plot_labels[dv_names])
      ),
      r_label = sprintf("%.2f", r)
    ) %>%
    filter(x_index < y_index)

  p <- ggplot(corr_data, aes(x = Outcome_x, y = Outcome_y, fill = r)) +
    geom_tile(color = "white", linewidth = 0.55) +
    geom_text(aes(label = r_label), size = 3.2, color = "black") +
    scale_fill_gradient(
      low = "white",
      high = "black",
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      name = "r"
    ) +
    coord_fixed() +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      plot.margin = margin(8, 8, 8, 8)
    )

  ggsave(
    filename = output_file,
    plot = p,
    width = 6.2,
    height = 5.5,
    dpi = 450,
    bg = "white"
  )

  write.csv(corr_mat, "output/manova_dv_correlation_matrix.csv")
}

make_dv_correlation_plot(
  data = dat_analysis,
  output_file = "output/manova_dv_correlation_plot.png"
)

