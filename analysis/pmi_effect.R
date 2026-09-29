rm(list = ls())

library(tidyverse)
library(brms)
library(posterior)
library(tidybayes)

options(mc.cores = 8, brms.backend = "cmdstanr")
theme_set(theme_minimal(base_size = 10))

FIT_DIR    <- "fits"
RESULT_DIR <- "../result"
MCMC <- list(chains = 4, iter = 2000, warmup = 1000, seed = 1223, control = list(adapt_delta = 0.99, max_treedepth = 15))

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
    filter(!str_detect(id, "[-.]")) %>%   # drop MWT range / empty nodes
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
  col_types = cols_only(zone = "d", position = "d", wlen = "d", unigram = "d", unisurp = "d", surp_gpt2 = "d", dep_nc = "d", topdown_nc = "d", bottomup_nc = "d", leftcorner_nc = "d", ccgright_reduce = "d", ccgleft_reduce = "d", story = "c", punc = "d", line_start = "d", line_end = "d", word = "c"
  )
) %>%
  mutate(cpmi = unisurp - surp_gpt2) %>%
  rename(article = story)

upos_ns <- read_upos("../data/dep_ns.txt")
stopifnot(nrow(pred_ns) == nrow(upos_ns),
          all(pred_ns$word == upos_ns$word))

pred_ns <- pred_ns %>%
  mutate(pos = upos_ns$upos) %>%
  filter(punc == 0, line_start == 0, line_end == 0) %>%
  select(-punc, -line_start, -line_end)

d <- pred_ns %>%
  filter(!is.na(cpmi), if_all(all_of(nc_cols), ~ !is.na(.x)))

pos_keep <- d %>% count(pos) %>% filter(n >= MIN_POS_N) %>% pull(pos)
cat("POS categories dropped (n <", MIN_POS_N, "):", nrow(d) - sum(d$pos %in% pos_keep), "tokens\n")

d <- d %>%
  filter(pos %in% pos_keep) %>%
  mutate(
    pos = factor(pos),
    unisurp_c = unisurp - mean(unisurp, na.rm = TRUE),
    across(all_of(nc_cols), ~ .x - mean(.x), .names = "{.col}_c")
  )

print(d %>% count(pos, name = "n_tokens") %>% arrange(desc(n_tokens)), n = Inf)

make_data <- function(nc_name) {
  d %>% mutate(nc_c = .data[[paste0(nc_map[[nc_name]], "_c")]])
}

formula <- bf(cpmi ~ nc_c + unisurp_c + (1 + nc_c + unisurp_c | pos))

# weakly informative priors:
#    cpmi has mean ~7 and SD ~5 bits,
#    node counts have SD ~0.5-1.2, so slopes of a few bits per node (or per bit of unisurp) are well covered
priors <- c(
  prior(normal(0, 2),      class = "b"),
  prior(normal(0, 10),     class = "Intercept"),
  prior(normal(0, 2),      class = "sd", lb = 0),
  prior(lkj(2),            class = "cor"),
  prior(exponential(0.25), class = "sigma")
)

fit_one <- function(nc_name) {
  brm(
    formula = formula,
    data    = make_data(nc_name),
    prior   = priors,
    chains  = MCMC$chains,
    iter    = MCMC$iter,
    warmup  = MCMC$warmup,
    seed    = MCMC$seed,
    control = MCMC$control,
    file    = file.path(FIT_DIR, sprintf("%s_main_centered", nc_map[[nc_name]])),
    file_refit = "on_change"
  )
}

fits <- map(set_names(nc_names), fit_one)

diag_tab <- imap_dfr(fits, function(f, nm) {
  s  <- summarise_draws(as_draws(f, variable = "^b_|^sd_|^cor_|^sigma", regex = TRUE))
  nd <- nuts_params(f) %>% filter(Parameter == "divergent__") %>% pull(Value) %>% sum()
  tibble(nc = nm, max_rhat = max(s$rhat), min_ess_bulk = min(s$ess_bulk), min_ess_tail = min(s$ess_tail), divergences = nd)
})
print(diag_tab)
write_lines(capture.output(print(as.data.frame(diag_tab), digits = 4, row.names = FALSE)), file.path(RESULT_DIR, "diagnostics.txt"))

get_b <- function(fit) as_draws_df(fit, variable = "b_nc_c")$b_nc_c

draws_b <- imap_dfr(fits, ~ tibble(nc = .y, b = get_b(.x))) %>% mutate(nc = as_nc_factor(nc))

b_tab <- draws_b %>%
  group_by(nc) %>%
  summarise(median = median(b),
            lo     = quantile(b, .025),
            hi     = quantile(b, .975),
            p_pos  = mean(b > 0),
            .groups = "drop")
print(b_tab)
write_csv(b_tab, file.path(RESULT_DIR, "summary.csv"))

p_forest <- ggplot(draws_b, aes(x = b, y = fct_rev(nc), fill = nc)) +
  stat_halfeye(
    aes(thickness = after_stat(ifelse(is.na(level), 0, pdf))),
    .width = c(.8, .95),
    point_interval = median_qi,
    alpha = .8, 
    colour = "black"
  ) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_fill_manual(values = nc_colors, guide = "none") +
  scale_x_continuous(limits = c(-1.7, 1.7)) +
  labs(x = "Coefficient Estimates", y = NULL)

walk(c("png", "pdf"), function(ext) {
  dev <- if (ext == "pdf") cairo_pdf else NULL
  ggsave(file.path(RESULT_DIR, ext, paste0("nc_coef_forest.", ext)),
         p_forest, width = 6, height = 4, dpi = 300, bg = "white", device = dev)
})







# Prior Sensitivity Analysis

priors_flat <- c(
  prior(normal(0, 10),     class = "b"),
  prior(normal(0, 10),     class = "Intercept"),
  prior(normal(0, 2),      class = "sd", lb = 0),
  prior(lkj(2),            class = "cor"),
  prior(exponential(0.25), class = "sigma")
)

priors_strong <- c(
  prior(normal(0, 0.5),    class = "b"),
  prior(normal(0, 10),     class = "Intercept"),
  prior(normal(0, 2),      class = "sd", lb = 0),
  prior(lkj(2),            class = "cor"),
  prior(exponential(0.25), class = "sigma")
)

fit_sensitivity <- function(nc_name, prior_set, suffix) {
  brm(
    formula = formula,
    data    = make_data(nc_name),
    prior   = prior_set,
    chains  = MCMC$chains,
    iter    = MCMC$iter,
    warmup  = MCMC$warmup,
    seed    = MCMC$seed,
    control = MCMC$control,
    file    = file.path(FIT_DIR, sprintf("%s_sensitivity_%s", nc_map[[nc_name]], suffix)),
    file_refit = "on_change"
  )
}

fits_flat   <- map(set_names(nc_names), ~ fit_sensitivity(.x, priors_flat, "flat"))
fits_strong <- map(set_names(nc_names), ~ fit_sensitivity(.x, priors_strong, "strong"))

draws_b_base <- draws_b %>% 
  mutate(prior = "Base: N(0, 2)")

draws_b_flat <- imap_dfr(fits_flat, ~ tibble(nc = .y, b = get_b(.x))) %>% 
  mutate(nc = as_nc_factor(nc), prior = "Flat: N(0, 10)")

draws_b_strong <- imap_dfr(fits_strong, ~ tibble(nc = .y, b = get_b(.x))) %>% 
  mutate(nc = as_nc_factor(nc), prior = "Strong: N(0, 0.5)")

draws_sens <- bind_rows(draws_b_flat, draws_b_base, draws_b_strong) %>%
  mutate(prior = factor(prior, levels = c("Flat: N(0, 10)", "Base: N(0, 2)", "Strong: N(0, 0.5)")))

p_sens <- ggplot(draws_sens, aes(x = b, y = fct_rev(nc), fill = prior)) +
  stat_halfeye(
    aes(thickness = after_stat(ifelse(is.na(level), 0, pdf))),
    .width = c(.8, .95),
    point_interval = median_qi,
    alpha = .7, 
    colour = "black",
    position = position_dodge(width = 0.7)
  ) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_fill_manual(
    values = c("Flat: N(0, 10)" = "#80B1D3", "Base: N(0, 2)" = "#FB8072", "Strong: N(0, 0.5)" = "#B3DE69"),
    name = "Prior on fixed effects (b):"
  ) +
  scale_x_continuous(limits = c(-1.7, 1.7)) +
  labs(
    x = "Coefficient Estimates", 
    y = NULL
  ) +
  theme(legend.position = "bottom")

walk(c("png", "pdf"), function(ext) {
  dev <- if (ext == "pdf") cairo_pdf else NULL
  ggsave(file.path(RESULT_DIR, ext, paste0("nc_coef_prior_sensitivity.", ext)),
         p_sens, width = 7, height = 5, dpi = 300, bg = "white", device = dev)
})
