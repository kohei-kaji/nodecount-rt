# Residual analysis for NCs

rm(list=ls())

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(parallel)



pred_path <- "../data/preds_ns.csv"
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",story="c"))
positions <- read_csv(pred_path,col_types = cols_only(zone="d",punc="d",line_start="d",line_end="d",story="c")) %>%
  rename(zone_id = zone)
pred <- pred %>%
  mutate(unigram = log(unigram + 1), freq = log(unigram+1), zone_id = zone)
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
  mutate_at(vars(zone,position,wlen,unigram,wlen_so1,unigram_so1,wlen_so2,unigram_so2), function(x) {scale(x, center=T, scale=T)})

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
pred <- read_csv(pred_path,col_types = cols_only(wlen="d",unigram="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",article="c")) %>%
  mutate(zone_id = zone, freq = unigram)
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
  mutate_at(vars(zone,position,wlen,unigram,wlen_so1,unigram_so1,wlen_so2,unigram_so2), function(x) {scale(x, center=T, scale=T)})

rt_path <- "../data/rts_dundee.csv"
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",FPRT = "d")) %>%
  select(subject, article, zone, FPRT) %>%
  rename(zone_id=zone) %>%
  filter(FPRT > 0, FPRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(mean_RT = mean(FPRT, na.rm = TRUE), .groups = "drop")
df_dfp <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",GPRT = "d")) %>%
  select(subject, article, zone, GPRT) %>%
  rename(zone_id=zone) %>%
  filter(GPRT > 0, GPRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(mean_RT = mean(GPRT, na.rm = TRUE), .groups = "drop")
df_dgp <- merge(rts_summary, pred, by = c("article","zone_id"), sort=F)
rts_summary <- read_csv(rt_path,col_types = cols_only(subject = "c",article = "c",zone = "d",TRT = "d")) %>%
  select(subject, article, zone, TRT) %>%
  rename(zone_id=zone) %>%
  filter(TRT > 0, TRT <= 2000) %>%
  group_by(article, zone_id) %>%
  summarise(mean_RT = mean(TRT, na.rm = TRUE), .groups = "drop")
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
  filter(punc==0, line_start==0, line_end==0) %>%
  select(-punc, -line_start, -line_end) %>%
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
  rename(zone_id=zone) %>%
  filter(IA_REGRESSION_PATH_DURATION > 0, IA_REGRESSION_PATH_DURATION <= 2000) %>%
  group_by(story, zone_id) %>%
  summarise(mean_RT = mean(IA_REGRESSION_PATH_DURATION, na.rm = TRUE), .groups = "drop")
df_osgp <- merge(rts_summary, pred, by = c("story","zone_id"), sort=F)

rts_summary <- read_csv(rt_path,col_types = cols_only(article_batch="c",article_id="c",difficulty_level="c",zone="d",IA_DWELL_TIME="d"), na = c("", "NA", ".")) %>%
  mutate(story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  select(story, zone, IA_DWELL_TIME) %>%
  drop_na(IA_DWELL_TIME) %>%
  rename(zone_id=zone) %>%
  filter(IA_DWELL_TIME > 0, IA_DWELL_TIME <= 2000) %>%
  group_by(story, zone_id) %>%
  summarise(mean_RT = mean(IA_DWELL_TIME, na.rm = TRUE), .groups = "drop")
df_ost <- merge(rts_summary, pred, by = c("story","zone_id"), sort=F)
# df_os <- read_tsv("../data/os_merged.csv", show_col_types = FALSE) %>% drop_na()
# df_os <- df_os %>%
#   filter(is_punct==0, bos==0, eos==0) %>%
#   select(-is_punct, -bos, -eos) %>%
#   mutate(freq = unigram) %>%
#   mutate_at(vars(zone,position,wlen,unigram,wlen_so1,unigram_so1,wlen_so2,unigram_so2), function(x) {scale(x, center=T, scale=T)})
# df_osfp <- df_os %>%
#   rename(mean_RT=mean_FPD)
# df_osgp <- df_os %>%
#   rename(mean_RT=mean_GPD)
# df_ost <- df_os %>%
#   rename(mean_RT=mean_TRT)

dfs <- list(ns = df_ns, dfp = df_dfp, dgp = df_dgp, dt = df_dt, osfp = df_osfp, osgp = df_osgp, ost = df_ost)
dataset_names <- c("Natural Stories", "Dundee First pass", "Dundee Go-past", "Dundee Total fixation", "OneStop First pass", "OneStop Go-past", "OneStop Total fixation")


result_list <- list()

base_formula <- mean_RT ~ zone + position + wlen * unigram + wlen_so1 * unigram_so1 + wlen_so2 * unigram_so2
nc_cols <- c("topdown_nc", "bottomup_nc", "leftcorner_nc", "dep_nc", "ccgright_reduce", "ccgleft_reduce")
predictor_names <- c("PSG Top-down", "PSG Bottom-up", "PSG Left-corner", "Dependency", "CCG-Right", "CCG-Left")

for (ds_idx in seq_along(dfs)) {
  df <- dfs[[ds_idx]]
  ds_name <- dataset_names[ds_idx]

  n <- nrow(df)
  message(sprintf("[START] %s (n=%d)", ds_name, n))

  df$fold <- sample(rep(1:10, length.out = n))

  for (k in 1:10) {
    train <- df[df$fold != k, , drop = FALSE]
    test  <- df[df$fold == k, , drop = FALSE]

    m_base <- lm(base_formula, data = train)
    pred_base <- predict(m_base, newdata = test)

    for (pred_idx in seq_along(nc_cols)) {
      pred_col  <- nc_cols[pred_idx]
      pred_name <- predictor_names[pred_idx]

      result_list[[length(result_list) + 1]] <- data.frame(
        dataset   = ds_name,
        predictor = pred_name,
        ndmain    = test[[pred_col]],
        resid     = test$mean_RT - pred_base,
        stringsAsFactors = FALSE
      )
    }
  }
}

result_df <- bind_rows(result_list)


print(result_df)
write.csv(result_df, "../result/resids_so2.csv", row.names = FALSE)
