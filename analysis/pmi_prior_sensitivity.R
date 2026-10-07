rm(list = ls())

library(tidyverse)
library(brms)
library(posterior)
library(tidybayes)

options(mc.cores = 4, brms.backend = "cmdstanr")
theme_set(theme_minimal(base_size = 10))

FIT_DIR    <- "fits"
RESULT_DIR <- "../result"

dir.create(FIT_DIR, showWarnings = FALSE, recursive = TRUE)
walk(c("png", "pdf"), ~ dir.create(file.path(RESULT_DIR, .x), showWarnings = FALSE, recursive = TRUE))

MCMC <- list(
  chains = 4, 
  threads = 16,
  iter = 2000, 
  warmup = 1000, 
  seed = 1223, 
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

make_grouped_palette_ucd <- function() {
  base_red   <- "#FF4B00"
  base_blue  <- "#005AFF"
  base_green <- "#03AF7A"
  cols <- c(
    colorspace::darken(base_red, 0.3), base_red, colorspace::lighten(base_red, 0.3),
    base_blue,
    colorspace::darken(base_green, 0.25), base_green
  )
  names(cols) <- c("group1_1", "group1_2", "group1_3", "group2", "group3_1", "group3_2")
  cols
}
cols <- make_grouped_palette_ucd()

nc_map <- c(
  "PSG Top-down"    = "topdown_nc",
  "PSG Bottom-up"   = "bottomup_nc",
  "PSG Left-corner" = "leftcorner_nc",
  "Dependency"      = "dep_nc",
  "CCG-Right"       = "ccgright_reduce",
  "CCG-Left"        = "ccgleft_reduce"
)
nc_names  <- names(nc_map)
nc_cols   <- unname(nc_map)
nc_colors <- setNames(unname(cols), nc_names)

as_nc_factor <- function(x) factor(x, levels = nc_names)


MIN_POS_N <- 50
read_upos <- function(path) {
  tibble(line = read_lines(path)) %>%
    mutate(sent = cumsum(line == "")) %>%
    filter(line != "", !str_starts(line, "#")) %>%
    separate_wider_delim(
      line, "\t",
      names = c("id", "form", "lemma", "upos", "xpos", "feats", "head", "deprel", "deps", "misc")
    ) %>%
    filter(!str_detect(id, "[-.]")) %>%
    mutate(start = as.integer(str_match(misc, "start_char=(\\d+)")[, 2]),
           end   = as.integer(str_match(misc, "end_char=(\\d+)")[, 2])) %>%
    group_by(sent) %>%
    mutate(word_id = cumsum(is.na(lag(end)) | start != lag(end))) %>%
    group_by(sent, word_id) %>%
    summarise(word = paste0(form, collapse = ""),
              upos = paste(upos, collapse = "+"),
              .groups = "drop")
}

pred_ns <- read_csv(
  "../data/preds_ns.csv",
  col_types = cols_only(zone = "d", position = "d", wlen = "d", unigram = "d", unisurp = "d", surp_gpt2 = "d", dep_nc = "d", topdown_nc = "d", bottomup_nc = "d", leftcorner_nc = "d", ccgright_reduce = "d", ccgleft_reduce = "d", story = "c", punc = "d", line_start = "d", line_end = "d", word = "c")
) %>%
  mutate(cpmi = unisurp - surp_gpt2) %>%
  rename(article = story)

upos_ns <- read_upos("../data/dep_ns.txt")
stopifnot(nrow(pred_ns) == nrow(upos_ns), all(pred_ns$word == upos_ns$word))

pred_ns <- pred_ns %>%
  mutate(pos = upos_ns$upos) %>%
  filter(punc == 0, line_start == 0, line_end == 0) %>%
  select(-punc, -line_start, -line_end)

d <- pred_ns %>%
  filter(!is.na(cpmi), if_all(all_of(nc_cols), ~ !is.na(.x)))

pos_keep <- d %>% count(pos) %>% filter(n >= MIN_POS_N) %>% pull(pos)

d <- d %>%
  filter(pos %in% pos_keep) %>%
  mutate(
    pos = factor(pos),
    unisurp_c = unisurp - mean(unisurp, na.rm = TRUE),
    across(all_of(nc_cols), ~ .x - mean(.x), .names = "{.col}_c")
  )

make_data <- function(nc_name) {
  d %>% mutate(nc_c = .data[[paste0(nc_map[[nc_name]], "_c")]])
}

formula <- bf(cpmi ~ nc_c + unisurp_c + (1 + nc_c + unisurp_c | pos))


prior_sds <- c(0.1, 0.2, 0.5, 1, 2, 5, 10)

fit_sensitivity_sd <- function(nc_name, prior_sd) {
  priors_custom <- c(
    prior_string(sprintf("normal(0, %s)", prior_sd), class = "b"),
    prior(normal(0, 10),     class = "Intercept"),
    prior(normal(0, 2),      class = "sd", lb = 0),
    prior(lkj(2),            class = "cor"),
    prior(exponential(0.25), class = "sigma")
  )
  
  # ファイル名用に小数点をアンダースコアに変換
  sd_str <- gsub("\\.", "_", as.character(prior_sd))
  file_name <- file.path(FIT_DIR, sprintf("%s_sens_sd_%s", nc_map[[nc_name]], sd_str))
  
  brm(
    formula = formula,
    data    = make_data(nc_name),
    prior   = priors_custom,
    chains  = MCMC$chains,
    threads = threading(MCMC$threads),
    iter    = MCMC$iter,
    warmup  = MCMC$warmup,
    seed    = MCMC$seed,
    control = MCMC$control,
    file    = file_name,
    file_refit = "on_change"
  )
}

sens_grid <- expand_grid(nc = nc_names, sd = prior_sds)

draws_sens_summary <- pmap_dfr(sens_grid, function(nc, sd) {
  cat(sprintf("Fitting: %s with Prior N(0, %s)...\n", nc, sd))
  fit <- fit_sensitivity_sd(nc, sd)
  
  b_draws <- as_draws_df(fit, variable = "b_nc_c")$b_nc_c
  
  tibble(
    nc = nc,
    sd = sd,
    median = median(b_draws),
    lo = quantile(b_draws, 0.025),
    hi = quantile(b_draws, 0.975)
  )
})


draws_sens_plot <- draws_sens_summary %>% 
  mutate(nc = as_nc_factor(nc))

p_sens <- ggplot(draws_sens_plot, aes(x = sd, y = median, color = nc, fill = nc)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ nc) +
  scale_x_continuous(breaks = 0:10) +
  scale_color_manual(values = nc_colors) +
  scale_fill_manual(values = nc_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    x = expression(paste("Prior Standard Deviation of ", beta)),
    y = expression(paste("Estimated ", beta))
  )

walk(c("png", "pdf"), function(ext) {
  dev <- if (ext == "pdf") cairo_pdf else NULL
  ggsave(file.path(RESULT_DIR, ext, paste0("nc_prior_sensitivity_curve.", ext)), p_sens, width = 6, height = 4, dpi = 300, bg = "white", device = dev)
})

print(p_sens)
