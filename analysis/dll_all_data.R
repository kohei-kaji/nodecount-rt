rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(plotrix)
library(jmuOutlier)


pred_path <- "../data/preds_ns.csv"
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",surp_gpt2="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",story="c"))
positions <- read_csv(pred_path,col_types = cols_only(zone="d",punc="d",line_start="d",line_end="d",story="c")) %>%
  rename(zone_id = zone)
pred <- pred %>% mutate(zone_id = zone)
pred_so1 <- pred %>% 
  mutate(zone_id = zone_id + 1) %>% 
  select(-zone) %>%
  rename_with(~ paste(., "_so1", sep = "")) %>%
  rename(story = story_so1, zone_id = zone_id_so1)
pred_so2 <- pred %>% 
  mutate(zone_id = zone_id + 2) %>% 
  select(-zone) %>% 
  rename_with(~ paste(., "_so2", sep = "")) %>% 
  rename(story = story_so2, zone_id = zone_id_so2)
pred <- pred %>%
  merge(pred_so1, by = c("story", "zone_id"), sort = F) %>%
  merge(pred_so2, by = c("story", "zone_id"), sort = F) %>%
  merge(positions, by = c("story", "zone_id"), sort = F) %>%
  rename(article = story) %>%
  mutate_at(
    vars(
      zone,position,
      wlen,unigram,surp_gpt2,dep_nc,topdown_nc,bottomup_nc,leftcorner_nc,ccgright_reduce,ccgleft_reduce,
      wlen_so1,unigram_so1,surp_gpt2_so1,dep_nc_so1,topdown_nc_so1,bottomup_nc_so1,leftcorner_nc_so1,ccgright_reduce_so1,ccgleft_reduce_so1,
      wlen_so2,unigram_so2,surp_gpt2_so2,dep_nc_so2,topdown_nc_so2,bottomup_nc_so2,leftcorner_nc_so2,ccgright_reduce_so2,ccgleft_reduce_so2
      ),
            function(x) {scale(x, center=T, scale=T)})

rt_path <- "../data/spr/naturalstories/naturalstories/naturalstories_RTS/processed_RTs.tsv"
rts_summary <- read.table(rt_path, sep = "\t", quote = "", header = TRUE) %>% 
  select(WorkerId, item, zone, RT) %>%
  rename(article = item, zone_id = zone) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    mean_logRT = mean(log(RT), na.rm = TRUE),
    .groups = "drop"
  )
df_ns <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)


pred_path <- "../data/preds_dundee.csv"
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",surp_gpt2="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",article="c")) %>% mutate(zone_id = zone)
pred$dep_nc[is.na(pred$dep_nc)] <- 0
positions <- read_csv(pred_path,col_types = cols_only(zone="d",punc="d",line_start="d",line_end="d",article="c")) %>%
  rename(zone_id = zone)
pred_so1 <- pred %>% 
  mutate(zone_id = zone_id + 1) %>% 
  select(-zone) %>%
  rename_with(~ paste(., "_so1", sep = "")) %>%
  rename(article = article_so1, zone_id = zone_id_so1)
pred_so2 <- pred %>% 
  mutate(zone_id = zone_id + 2) %>% 
  select(-zone) %>% 
  rename_with(~ paste(., "_so2", sep = "")) %>% 
  rename(article = article_so2, zone_id = zone_id_so2)
pred <- pred %>%
  merge(pred_so1, by = c("article", "zone_id"), sort = F) %>%
  merge(pred_so2, by = c("article", "zone_id"), sort = F) %>%
  merge(positions, by = c("article", "zone_id"), sort = F) %>%
  mutate_at(
    vars(
      zone,position,
      wlen,unigram,surp_gpt2,dep_nc,topdown_nc,bottomup_nc,leftcorner_nc,ccgright_reduce,ccgleft_reduce,
      wlen_so1,unigram_so1,surp_gpt2_so1,dep_nc_so1,topdown_nc_so1,bottomup_nc_so1,leftcorner_nc_so1,ccgright_reduce_so1,ccgleft_reduce_so1,
      wlen_so2,unigram_so2,surp_gpt2_so2,dep_nc_so2,topdown_nc_so2,bottomup_nc_so2,leftcorner_nc_so2,ccgright_reduce_so2,ccgleft_reduce_so2
    ),
    function(x) {scale(x, center=T, scale=T)})

rt_path <- "../data/rts_dundee.csv"
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",FPRT = "d")) %>%
  select(subject, article, zone, FPRT) %>%
  rename(zone_id=zone) %>%
  filter(FPRT > 0, FPRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(FPRT, na.rm = TRUE),
    mean_logRT = mean(log(FPRT), na.rm = TRUE),
    .groups = "drop")
df_dfp <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",GPRT = "d")) %>%
  select(subject, article, zone, GPRT) %>%
  rename(zone_id=zone) %>%
  filter(GPRT > 0, GPRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(GPRT, na.rm = TRUE),
    mean_logRT = mean(log(GPRT), na.rm = TRUE),
    .groups = "drop")
df_dgp <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",TRT = "d")) %>%
  select(subject, article, zone, TRT) %>%
  rename(zone_id=zone) %>%
  filter(TRT > 0, TRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(TRT, na.rm = TRUE),
    mean_logRT = mean(log(TRT), na.rm = TRUE),
    .groups = "drop")
df_dt <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)


pred_path <- "../data/preds_os.csv"
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",surp_gpt2="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",article_batch="c",article_id="c",difficulty_level="c")) %>%
  mutate(zone_id = zone, story = paste(article_batch,article_id,difficulty_level,sep="_")) %>%
  select(-article_batch, -article_id, -difficulty_level)
positions <- read_csv(pred_path,col_types = cols_only(zone="d",punc="d",line_start="d",line_end="d",article_batch="c",article_id="c",difficulty_level="c")) %>%
  mutate(zone_id = zone, story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  select(-zone, -article_batch, -article_id, -difficulty_level)
pred_so1 <- pred %>% 
  mutate(zone_id = zone_id + 1) %>% 
  select(-zone) %>%
  rename_with(~ paste(., "_so1", sep = "")) %>%
  rename(story = story_so1, zone_id = zone_id_so1)
pred_so2 <- pred %>% 
  mutate(zone_id = zone_id + 2) %>% 
  select(-zone) %>% 
  rename_with(~ paste(., "_so2", sep = "")) %>% 
  rename(story = story_so2, zone_id = zone_id_so2)
pred <- pred %>%
  merge(pred_so1, by = c("story", "zone_id"), sort = F) %>%
  merge(pred_so2, by = c("story", "zone_id"), sort = F) %>%
  merge(positions, by = c("story", "zone_id"), sort = F) %>%
  mutate_at(
    vars(
      zone,position,
      wlen,unigram,surp_gpt2,dep_nc,topdown_nc,bottomup_nc,leftcorner_nc,ccgright_reduce,ccgleft_reduce,
      wlen_so1,unigram_so1,surp_gpt2_so1,dep_nc_so1,topdown_nc_so1,bottomup_nc_so1,leftcorner_nc_so1,ccgright_reduce_so1,ccgleft_reduce_so1,
      wlen_so2,unigram_so2,surp_gpt2_so2,dep_nc_so2,topdown_nc_so2,bottomup_nc_so2,leftcorner_nc_so2,ccgright_reduce_so2,ccgleft_reduce_so2
    ),
    function(x) {scale(x, center=T, scale=T)})

rt_path <- "../data/OneStop/rts.csv"
rts_summary <- read_csv(rt_path,col_types = cols_only(article_batch="c",article_id="c",difficulty_level="c",zone="d",IA_FIRST_RUN_DWELL_TIME="d"), na = c("", "NA", ".")) %>%
  mutate(story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  select(story, zone, IA_FIRST_RUN_DWELL_TIME) %>%
  drop_na(IA_FIRST_RUN_DWELL_TIME) %>%
  filter(IA_FIRST_RUN_DWELL_TIME > 0, IA_FIRST_RUN_DWELL_TIME <= 2000) %>%
  rename(zone_id=zone) %>%
  group_by(story, zone_id) %>%
  summarise(mean_RT = mean(IA_FIRST_RUN_DWELL_TIME, na.rm = TRUE), .groups = "drop")
df_osfp <- merge(rts_summary, pred, by = c("story","zone_id"), sort=F)

rts_summary <- read_csv(rt_path,col_types = cols_only(article_batch="c",article_id="c",difficulty_level="c",zone="d",IA_REGRESSION_PATH_DURATION="d"), na = c("", "NA", ".")) %>%
  mutate(story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  select(story, zone, IA_REGRESSION_PATH_DURATION) %>%
  drop_na(IA_REGRESSION_PATH_DURATION) %>%
  filter(IA_REGRESSION_PATH_DURATION > 0, IA_REGRESSION_PATH_DURATION <= 2000) %>%
  rename(zone_id=zone) %>%
  group_by(story, zone_id) %>%
  summarise(mean_RT = mean(IA_REGRESSION_PATH_DURATION, na.rm = TRUE), .groups = "drop")
df_osgp <- merge(rts_summary, pred, by = c("story","zone_id"), sort=F)

rts_summary <- read_csv(rt_path,col_types = cols_only(article_batch="c",article_id="c",difficulty_level="c",zone="d",IA_DWELL_TIME="d"), na = c("", "NA", ".")) %>%
  mutate(story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  select(story, zone, IA_DWELL_TIME) %>%
  drop_na(IA_DWELL_TIME) %>%
  filter(IA_DWELL_TIME > 0, IA_DWELL_TIME <= 2000) %>%
  rename(zone_id=zone) %>%
  group_by(story, zone_id) %>%
  summarise(mean_RT = mean(IA_DWELL_TIME, na.rm = TRUE), .groups = "drop")
df_ost <- merge(rts_summary, pred, by = c("story","zone_id"), sort=F)

# df_os <- read_csv("../data/os_merged.csv", show_col_types = FALSE) %>% rename(punc=is_punct,line_start=bos,line_end=eos) %>% drop_na()
# df_os <- df_os %>%
#   mutate_at(
#     vars(
#       zone,position,
#       wlen,unigram,surp_gpt2,dep_nc,topdown_nc,bottomup_nc,leftcorner_nc,ccgright_reduce,ccgleft_reduce,
#       wlen_so1,unigram_so1,surp_gpt2_so1,dep_nc_so1,topdown_nc_so1,bottomup_nc_so1,leftcorner_nc_so1,ccgright_reduce_so1,ccgleft_reduce_so1,
#       wlen_so2,unigram_so2,surp_gpt2_so2,dep_nc_so2,topdown_nc_so2,bottomup_nc_so2,leftcorner_nc_so2,ccgright_reduce_so2,ccgleft_reduce_so2
#     ),
#     function(x) {scale(x, center=T, scale=T)})

# df_osfp <- df_os %>%
#   rename(mean_RT=mean_FPD)
# df_osgp <- df_os %>%
#   rename(mean_RT=mean_GPD)
# df_ost <- df_os %>%
#   rename(mean_RT=mean_TRT)


dfs <- list(ns = df_ns, dfp = df_dfp, dgp = df_dgp, dt = df_dt, osfp = df_osfp, osgp = df_osgp, ost = df_ost)
dataset_names <- c("Natural Stories", "Dundee First pass", "Dundee Go-past", "Dundee Total fixation", "OneStop First pass", "OneStop Go-past", "OneStop Total fixation")
nc_cols <- c("dep_nc", "topdown_nc", "bottomup_nc", "leftcorner_nc", "ccgright_reduce", "ccgleft_reduce")
predictor_names <- c("Dependency", "PSG Top-down", "PSG Bottom-up", "PSG Left-corner", "CCG-Right", "CCG-Left")


set.seed(1)


messagef <- function(...) { message(sprintf(...)); flush.console() }


run_cv_targets <- function(
  dfs,
  baseline_str,
  output_path,
  n_folds = 10,
  n_cores = max(1L, parallel::detectCores() - 2L)
){
  all_ds_rows <- list()

  for (ds_idx in seq_along(dfs)) {
    df <- dfs[[ds_idx]]
    ds_name <- dataset_names[ds_idx]

    base_f <- stats::as.formula(baseline_str)
    targets <- list(
      list(name="Dependency",      add = c("dep_nc","dep_nc_so1","dep_nc_so2")),
      list(name="PSG Top-down",   add = c("topdown_nc","topdown_nc_so1","topdown_nc_so2")),
      list(name="PSG Bottom-up",  add = c("bottomup_nc","bottomup_nc_so1","bottomup_nc_so2")),
      list(name="PSG Left-corner",add = c("leftcorner_nc","leftcorner_nc_so1","leftcorner_nc_so2")),
      list(name="CCG-Right",      add = c("ccgright_reduce","ccgright_reduce_so1","ccgright_reduce_so2")),
      list(name="CCG-Left",       add = c("ccgleft_reduce","ccgleft_reduce_so1","ccgleft_reduce_so2"))
    )

    n <- nrow(df)
    y <- df$mean_RT

    messagef("[START] %s (n=%d)", ds_name, n)

    fold_ids <- sample(rep(seq_len(n_folds), length.out = n))

    mu_base <- numeric(n)
    sd_base <- numeric(n)

    for (f in seq_len(n_folds)) {
      te_idx <- which(fold_ids == f)
      tr_idx <- which(fold_ids != f)

      tr <- df[tr_idx, , drop = FALSE]
      te <- df[te_idx, , drop = FALSE]

      m_base <- stats::lm(base_f, data = tr)
      mu_base[te_idx] <- stats::predict(m_base, newdata = te)
      sd_base[te_idx] <- stats::sigma(m_base)
    }

    ll_base <- stats::dnorm(y, mean = mu_base, sd = sd_base, log = TRUE)

    out_rows <- lapply(targets, function(tg){
      add_terms <- tg$add

      mu_targ <- numeric(n)
      sd_targ <- numeric(n)

      coef1_f <- numeric(n_folds)
      coef2_f <- numeric(n_folds)
      coef3_f <- numeric(n_folds)

      messagef("  - Target: %s", tg$name)

      for (f in seq_len(n_folds)) {
        te_idx <- which(fold_ids == f)
        tr_idx <- which(fold_ids != f)

        tr <- df[tr_idx, , drop = FALSE]
        te <- df[te_idx, , drop = FALSE]

        rhs_add <- paste(add_terms, collapse = " + ")
        targ_f  <- stats::update(base_f, paste0(". ~ . + ", rhs_add))

        m_targ  <- stats::lm(targ_f, data = tr)
        mu_targ[te_idx] <- stats::predict(m_targ, newdata = te)
        sd_val <- stats::sigma(m_targ)
        sd_targ[te_idx] <- sd_val

        coefs <- stats::coef(m_targ)
        coef1_f[f] <- if (add_terms[1] %in% names(coefs)) unname(coefs[[add_terms[1]]]) else NA_real_
        coef2_f[f] <- if (add_terms[2] %in% names(coefs)) unname(coefs[[add_terms[2]]]) else NA_real_
        coef3_f[f] <- if (add_terms[3] %in% names(coefs)) unname(coefs[[add_terms[3]]]) else NA_real_
      }

      ll_targ <- stats::dnorm(y, mean = mu_targ, sd = sd_targ, log = TRUE)
      dll     <- ll_targ - ll_base

      mean_c1 <- mean(coef1_f, na.rm = TRUE)
      mean_c2 <- mean(coef2_f, na.rm = TRUE)
      mean_c3 <- mean(coef3_f, na.rm = TRUE)

      pt_res <- perm.test(dll, alternative = "greater", num.sim = 20000)
      pvalue <- pt_res$p.value
      signif_code <- if      (pvalue >= 0.05)                  ""
                     else if (pvalue < 0.05 & pvalue >= 0.01)  "*"
                     else if (pvalue < 0.01 & pvalue >= 0.001) "**"
                     else                                       "***"

      data.frame(
        dataset           = ds_name,
        predictor         = tg$name,
        mean_dll          = mean(dll, na.rm = TRUE),
        lower_ci          = mean(dll, na.rm = TRUE) - (1.96 * std.error(dll, na.rm = TRUE)),
        upper_ci          = mean(dll, na.rm = TRUE) + (1.96 * std.error(dll, na.rm = TRUE)),
        p_value           = pvalue,
        sig               = signif_code,
        mean_coef_term_1  = mean_c1,
        mean_coef_term_2  = mean_c2,
        mean_coef_term_3  = mean_c3,
        stringsAsFactors  = FALSE
      )
    })

    all_ds_rows[[length(all_ds_rows) + 1L]] <- do.call(rbind, out_rows)
    messagef("[DONE ] %s", ds_name)
  }

  res <- do.call(rbind, all_ds_rows)
  utils::write.csv(res, file = output_path, row.names = FALSE)
}


run_cv_targets(dfs, 
  baseline_str = "mean_RT ~ zone + position + wlen*unigram + wlen_so1*unigram_so1 + wlen_so2*unigram_so2 + punc + line_start + line_end",
  output_path = "../result/dll_10cv_so2_all_data.csv"
)

run_cv_targets(dfs, 
  baseline_str = "mean_RT ~ zone + position + wlen*unigram + wlen_so1*unigram_so1 + wlen_so2*unigram_so2 + surp_gpt2 + surp_gpt2_so1 + surp_gpt2_so2 + punc + line_start + line_end",
  output_path = "../result/dll_surp_10cv_so2_all_data.csv"
)

