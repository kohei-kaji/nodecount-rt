# calculate best models using AIC subset selection

rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)


pred_path <- "../data/preds_ns.csv"
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",dep_nc="d",bottomup_nc="d",ccgright_reduce="d",zone="d",position="d",story="c"))
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
  filter(punc==0, line_start==0, line_end==0) %>%
  select(-punc, -line_start, -line_end) %>%
  mutate_at(
    vars(
      zone,position,
      wlen,unigram,dep_nc,bottomup_nc,ccgright_reduce,
      wlen_so1,unigram_so1,dep_nc_so1,bottomup_nc_so1,ccgright_reduce_so1,
      wlen_so2,unigram_so2,dep_nc_so2,bottomup_nc_so2,ccgright_reduce_so2
      ),
            function(x) {scale(x, center=T, scale=T)})

rt_path <- "../data/spr/naturalstories/naturalstories/naturalstories_RTS/processed_RTs.tsv"
rts_summary <- read.table(rt_path, sep = "\t", quote = "", header = TRUE) %>% 
  select(WorkerId, item, zone, RT) %>%
  rename(article = item, zone_id = zone) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    .groups = "drop"
  )
df_ns <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)


pred_path <- "../data/preds_dundee.csv"
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",article="c")) %>% mutate(zone_id = zone)
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
  filter(punc==0, line_start==0, line_end==0) %>%
  select(-punc, -line_start, -line_end) %>%
  mutate_at(
    vars(
      zone,position,
      wlen,unigram,dep_nc,topdown_nc,bottomup_nc,ccgright_reduce,ccgleft_reduce,
      wlen_so1,unigram_so1,dep_nc_so1,topdown_nc_so1,bottomup_nc_so1,ccgright_reduce_so1,ccgleft_reduce_so1,
      wlen_so2,unigram_so2,dep_nc_so2,topdown_nc_so2,bottomup_nc_so2,ccgright_reduce_so2,ccgleft_reduce_so2
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
    .groups = "drop")
df_dfp <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",GPRT = "d")) %>%
  select(subject, article, zone, GPRT) %>%
  rename(zone_id=zone) %>%
  filter(GPRT > 0, GPRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(GPRT, na.rm = TRUE),
    .groups = "drop")
df_dgp <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",TRT = "d")) %>%
  select(subject, article, zone, TRT) %>%
  rename(zone_id=zone) %>%
  filter(TRT > 0, TRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(
    mean_RT = mean(TRT, na.rm = TRUE),
    .groups = "drop")
df_dt <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)

df_os <- read_tsv("../data/os_merged.csv", show_col_types = FALSE) %>% drop_na()
df_os <- df_os %>%
  filter(is_punct==0, bos==0, eos==0) %>%
  select(-is_punct, -bos, -eos) %>%
  mutate_at(
    vars(
      zone,position,
      wlen,unigram,dep_nc,topdown_nc,bottomup_nc,leftcorner_nc,ccgright_reduce,ccgleft_reduce,
      wlen_so1,unigram_so1,dep_nc_so1,topdown_nc_so1,bottomup_nc_so1,leftcorner_nc_so1,ccgright_reduce_so1,ccgleft_reduce_so1,
      wlen_so2,unigram_so2,dep_nc_so2,topdown_nc_so2,bottomup_nc_so2,leftcorner_nc_so2,ccgright_reduce_so2,ccgleft_reduce_so2
    ),
    function(x) {scale(x, center=T, scale=T)})

df_osfp <- df_os %>%
  rename(mean_logRT=mean_logFPD, mean_RT=mean_FPD)
df_osgp <- df_os %>%
  rename(mean_logRT=mean_logGPD, mean_RT=mean_GPD)
df_ost <- df_os %>%
  rename(mean_logRT=mean_logTRT, mean_RT=mean_TRT)


dfs <- list(ns = df_ns, dfp = df_dfp, dgp = df_dgp, dt = df_dt, osfp = df_osfp, osgp = df_osgp, ost = df_ost)
dataset_names <- c("Natural Stories", "Dundee First pass", "Dundee Go-past", "Dundee Total fixation", "OneStop First pass", "OneStop Go-past", "OneStop Total fixation")
nc_cols <- c("dep_nc", "topdown_nc", "bottomup_nc", "leftcorner_nc", "ccgright_reduce", "ccgleft_reduce")
predictor_names <- c("Dependency", "PSG Top-down", "PSG Bottom-up", "PSG Left-corner", "CCG-Right", "CCG-Left")


run_aic_subset <- function(
    df,
    predictors,
    response   = "mean_RT",
    base_rhs   = "zone + position + wlen*unigram + wlen_so1*unigram_so1 + wlen_so2*unigram_so2",
    so_suffix  = c("_so1", "_so2"),
    train_prop = 0.5,
    seed       = 1L
){
  stopifnot(response %in% names(df))

  set.seed(seed)

  n   <- nrow(df)
  idx <- sample.int(n)
  train_size <- floor(train_prop * n)
  train_idx  <- idx[1:train_size]
  test_idx   <- idx[(train_size + 1):n]
  
  train <- df[train_idx, , drop = FALSE]
  test  <- df[test_idx,  , drop = FALSE]

  subsets <- list()
  if (length(predictors) == 0L) {
    subsets <- list(character(0))
  } else {
    for (k in 0:length(predictors)) {
      if (k == 0) {
        subsets <- c(subsets, list(character(0)))
      } else {
        cmb <- combn(predictors, k, simplify = FALSE)
        subsets <- c(subsets, cmb)
      }
    }
  }

  results <- data.frame(
    model_id      = seq_along(subsets),
    n_predictors  = integer(length(subsets)),
    predictors    = character(length(subsets)),
    AIC           = numeric(length(subsets)),
    stringsAsFactors = FALSE
  )
  
  models <- vector("list", length(subsets))
  
  for (i in seq_along(subsets)) {
    S <- subsets[[i]]
    
    extra_terms <- character(0)
    if (length(S) > 0) {
      extra_terms <- unlist(lapply(S, function(p) c(p, paste0(p, so_suffix))))
    }

    rhs_terms <- c(base_rhs, extra_terms)
    rhs       <- paste(rhs_terms, collapse = " + ")
    form      <- as.formula(paste(response, "~", rhs))

    fit <- lm(form, data = train)

    models[[i]]              <- fit
    results$n_predictors[i]  <- length(S)
    results$predictors[i]    <- if (length(S) == 0) "(none)" else paste(S, collapse = ", ")
    results$AIC[i]           <- AIC(fit)
  }

  results <- results[order(results$AIC), ]

  best_id      <- results$model_id[1]
  best_subset  <- subsets[[best_id]]
  best_model   <- models[[best_id]]
  best_formula <- formula(best_model)

  print(best_formula)
  m_test <- lm(best_formula, data = test)
  print(summary(m_test))
}


sink("../result/bestmodel.txt")
cat("Natural Stories\n")
run_aic_subset(df_ns, c("dep_nc", "bottomup_nc", "ccgright_reduce"))
cat("Dundee First pass\n")
run_aic_subset(df_dfp, c("bottomup_nc", "dep_nc", "ccgright_reduce", "ccgleft_reduce"))
cat("Dundee Go-past\n")
run_aic_subset(df_dgp, c("topdown_nc", "bottomup_nc", "dep_nc", "ccgright_reduce", "ccgleft_reduce"))
cat("Dundee Total fixation\n")
run_aic_subset(df_dt, c("topdown_nc", "bottomup_nc", "dep_nc", "ccgright_reduce", "ccgleft_reduce"))
cat("OneStop First pass\n")
run_aic_subset(df_osfp, c("bottomup_nc", "dep_nc", "ccgright_reduce", "ccgleft_reduce"))
cat("OneStop Go-past\n")
run_aic_subset(df_osgp, c("topdown_nc", "bottomup_nc", "leftcorner_nc", "dep_nc", "ccgright_reduce"))
cat("OneStop Total fixation\n")
run_aic_subset(df_ost, c("topdown_nc", "bottomup_nc", "dep_nc", "ccgright_reduce", "ccgleft_reduce"))
sink()
