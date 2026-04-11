rm(list=ls())
library(readr)
library(ggplot2)
library(svglite)
library(tidyverse)
library(dplyr)
library(reshape2)
library(viridis)
library(patchwork)
library(plotrix)
library(scales)
library(purrr)
library(latex2exp)
library(ggh4x)
library(broom)
library(glue)
library(ggrastr)
library(stringr)




make_grouped_palette_ucd <- function() {
  
  base_red <- "#FF4B00"
  base_blue <- "#005AFF"
  base_green <- "#03AF7A"
  
  g1 <- c(colorspace::darken(base_red, 0.3), base_red, colorspace::lighten(base_red, 0.3))
  g2 <- base_blue
  g3 <- c(colorspace::darken(base_green, 0.25), base_green)
  
  cols <- c(g1, g2, g3)
  names(cols) <- c("group1_1", "group1_2", "group1_3", "group2", "group3_1", "group3_2")
  cols
}

cols <- make_grouped_palette_ucd()

theme_set(theme_minimal(base_size = 10))


result_df <- read_csv("../result/dll_10cv_so2_all_data.csv", show_col_types = FALSE)

result_df <- result_df %>%
  group_by(predictor) %>%
  mutate(
    p_adjusted = p.adjust(p_value, method = "BH"),
    sig = if_else(p_adjusted < 0.05, "*", "")
  ) %>%
  ungroup()

dataset_levels <- c(
  "Dundee First pass",
  "Dundee Go-past",
  "Dundee Total fixation",
  "Natural Stories",
  "OneStop First pass",
  "OneStop Go-past",
  "OneStop Total fixation"
)
predictor_levels <- c(
  "PSG Top-down",
  "PSG Bottom-up",
  "PSG Left-corner",
  "Dependency",
  "CCG-Right",
  "CCG-Left"
)

plot_df <- result_df %>%
  mutate(
    dataset   = factor(dataset, levels = dataset_levels),
    predictor = factor(predictor, levels = predictor_levels),
    predictor_num = as.numeric(predictor)
  )

cols <- make_grouped_palette_ucd()
names(cols) <- predictor_levels


p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(
    data = plot_df,
    aes(x = predictor_num, y = mean_dll, color = predictor),
    size = 2.5
  ) +
  geom_errorbar(
    data = plot_df,
    aes(x = predictor_num, ymin = lower_ci, ymax = upper_ci, color = predictor),
    width = 0.04
  ) +
  geom_text(
    data = plot_df,
    aes(x = predictor_num, y = upper_ci + 0.00008, label = sig, color = predictor),
    size = 4, show.legend = FALSE
  ) +
  scale_color_manual(values = cols, name = "") +
  guides(color = guide_legend(title = "", nrow = 1)) +
  scale_x_continuous(breaks = 1:length(predictor_levels) - 0.1,
                     labels = predictor_levels,
                     minor_breaks = NULL) +
  facet_wrap(~ dataset, ncol = 7) +
  ylab("Delta Log Likelihood (average per word)") +
  xlab("") +
  theme_bw() +
  theme(
    text = element_text(size = 8,  hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(face = "bold")
  )
ggsave("../result/png/dll_all_data.png", p, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")


coeff_long_df <- plot_df %>%
  select(dataset, predictor,
         mean_coef_term_1, mean_coef_term_2, mean_coef_term_3) %>%
  pivot_longer(cols = starts_with("mean_coef_term_"),
               names_to = "type_raw",
               values_to = "coeff") %>%
  mutate(
    type = case_when(
      type_raw == "mean_coef_term_1" ~ "w[i]",
      type_raw == "mean_coef_term_2" ~ "w[i-1]",
      type_raw == "mean_coef_term_3" ~ "w[i-2]",
      TRUE ~ type_raw
    ),
    type = factor(type, levels = c("w[i]", "w[i-1]", "w[i-2]"))
  ) %>%
  group_by(dataset, predictor, type) %>%
  summarise(coeff = mean(coeff), .groups = "drop") %>%
  mutate(
    dataset   = factor(dataset, levels = dataset_levels),
    predictor = factor(predictor, levels = predictor_levels)
  )

lims_tbl <- coeff_long_df %>%
  group_by(dataset) %>%
  summarise(M = max(abs(coeff), na.rm = TRUE), .groups = "drop") %>%
  mutate(M = M * 1.05)

lims <- setNames(lims_tbl$M, as.character(lims_tbl$dataset))

mk_scale <- function(M) {
  scale_y_continuous(
    limits = c(-M, M),
    breaks = c(-M, 0, M),
    labels = label_number(accuracy = 1.0),
    expand = expansion(mult = 0.02)
  )
}

p2 <- ggplot(coeff_long_df, aes(x = type, y = coeff, fill = predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.8) +
  scale_fill_manual(values = cols, name = "") +
  guides(fill = guide_legend(title = "", nrow = 1)) +
  ylab("Coefficient Estimate") +
  xlab("") +
  scale_x_discrete(
    labels = c("w[i]"   = TeX("$w_i$"),
               "w[i-1]" = TeX("$w_{i+1}$"),
               "w[i-2]" = TeX("$w_{i+2}$"))
  ) +
  facet_wrap(~ dataset, ncol = 7, scales = "free_y") +
  theme_bw() +
  theme(
    text = element_text(size = 7.8, hjust = 0.5),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x  = element_text(hjust = 1),
    axis.text.y  = element_text(size = 5),
    axis.ticks.length = unit(0.05, "cm"),
    axis.ticks.x = element_line(),
    panel.spacing = unit(0.05, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  facetted_pos_scales(
    y = list(
      dataset == "Dundee First pass"      ~ mk_scale(lims["Dundee First pass"]),
      dataset == "Dundee Go-past"         ~ mk_scale(lims["Dundee Go-past"]),
      dataset == "Dundee Total fixation"  ~ mk_scale(lims["Dundee Total fixation"]),
      dataset == "Natural Stories"        ~ mk_scale(lims["Natural Stories"]),
      dataset == "OneStop First pass"     ~ mk_scale(lims["OneStop First pass"]),
      dataset == "OneStop Go-past"        ~ mk_scale(lims["OneStop Go-past"]),
      dataset == "OneStop Total fixation" ~ mk_scale(lims["OneStop Total fixation"])
    )
  )
p2
ggsave("../result/png/dllcoef_all_data.png", p2, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")
