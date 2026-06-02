# Visualization of Node Count analyses

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

# example of distributions of Node Counts
words <- c("If", "you", "were", "to", "journey", "to", "the", "North", "of", "England,",
           "you", "would", "come", "to", "a", "valley", "that", "is", "surrounded", "by", 
           "moors", "as", "high", "as", "mountains.")
col_ids <- paste0("W", seq_along(words))
data <- data.frame(
  Feature = c("PSG Top-down", "PSG Bottom-up", "PSG Left-corner", "Dependency", "CCG-Right", "CCG-Left"),
  matrix(c(
    2, 2, 1, 2, 1, 1, 2, 0, 1, 1, 1, 1, 1, 1, 2, 0, 2, 2, 1, 1, 2, 1, 0, 1, 1,
    0, 1, 0, 0, 0, 0, 0, 1, 0, 10, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 14,
    1, 2, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 0, 1, 2,
    0, 0, 2, 0, 2, 0, 0, 3, 0, 2, 0, 0, 4, 0, 0, 3, 0, 0, 3, 0, 2, 0, 2, 0, 2,
    0, 0, 0, 0, 0, 0, 0, 1, 0, 9, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 14,
    0, 1, 1, 1, 1, 1, 0, 2, 1, 2, 1, 1, 1, 1, 0, 2, 1, 1, 1, 1, 1, 0, 1, 1, 3
  ), nrow = 6, byrow = TRUE)
)
colnames(data)[2:ncol(data)] <- col_ids
data_long <- melt(data, id.vars = "Feature", variable.name = "WordID", value.name = "Value")
data_long$DisplayWord <- words[as.integer(sub("W", "", data_long$WordID))]
data_long <- data_long %>%
  group_by(Feature) %>%
  mutate(Normalized = Value / sum(Value)) %>%
  ungroup()
p <- ggplot(data_long, aes(x = factor(WordID, levels = col_ids), y = factor(Feature, levels = c("CCG-Left", "CCG-Right", "Dependency", "PSG Left-corner", "PSG Bottom-up", "PSG Top-down")), fill = Normalized)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Value), family = "serif", size = 3) +
  scale_x_discrete(labels = words) +
  scale_fill_gradient2(low = "#0080ff",  high = "#ff8000", midpoint = median(data_long$Normalized),
                       limits = range(data_long$Normalized), name = "") +
  coord_fixed() +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    axis.text.x = element_text(angle=45, hjust=1, family="serif", size=8),
    axis.text.y = element_text(family="serif",size=8),
    strip.text = element_text(face="bold",family="serif",size=10),
    legend.text = element_text(family="serif",size=9),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  )
ggsave("../result/pdf/Figure_5.pdf", p, device = "pdf", width = 7.5, height = 2, units = "in", bg = "transparent")
ggsave("../result/png/Figure_5.png", p, device = "png", width = 7.5, height = 2, units = "in", bg = "transparent")


pred_path <- "../data/preds_ns.csv"
pred <- read_csv(pred_path,col_types=cols_only(zone = "d",position = "d",story = "c",punc = "d",line_start = "d",line_end = "d")) %>%rename(article=story)  %>% filter(punc==0, line_start==0, line_end==0) %>% select(-punc, -line_start, -line_end)
rt_path <- "../data/spr/naturalstories/naturalstories/naturalstories_RTS/processed_RTs.tsv"
rts_summary <- read.table(rt_path, sep="\t", quote="", header=T) %>%
  select(WorkerId, item, zone, RT) %>%
  rename(article=item) %>%
  group_by(article, zone) %>%
  summarise(
    mean_RT = mean(RT, na.rm = TRUE),
    .groups = "drop")
df_ns <- merge(rts_summary, pred, by = c("article","zone"), sort=F)

pred_path <- "../data/preds_dundee.csv"
pred <- read_csv(pred_path, col_types = cols_only(zone = "d",position = "d",article = "c",punc = "d",line_start = "d",line_end = "d"))  %>% filter(punc==0, line_start==0, line_end==0) %>% select(-punc, -line_start, -line_end)
pred[is.na(pred)] <- 0
rt_path <- "../data/rts_dundee.csv"
rts_summary <- read_csv(rt_path, col_types = cols_only(subject = "c",article = "c",zone = "d",FPRT = "d")) %>%
  select(subject, article, zone, FPRT) %>%
  filter(FPRT > 0, FPRT <= 2000) %>%
  group_by(article, zone) %>%
  summarise(
    mean_RT = mean(FPRT, na.rm = TRUE),
    .groups = "drop")
df_dfp <- merge(rts_summary, pred, by = c("article","zone"), sort=F)

rts_summary <- read_csv(rt_path, col_types = cols_only(subject = "c",article = "c",zone = "d",GPRT = "d")) %>%
  select(subject, article, zone, GPRT) %>%
  filter(GPRT > 0, GPRT <= 2000) %>%
  group_by(article, zone) %>%
  summarise(
    mean_RT = mean(GPRT, na.rm = TRUE),
    .groups = "drop")
df_dgp <- merge(rts_summary, pred, by = c("article","zone"), sort=F)

rts_summary <- read_csv(rt_path, col_types = cols_only(subject = "c",article = "c",zone = "d",TRT = "d")) %>%
  select(subject, article, zone, TRT) %>%
  filter(TRT > 0, TRT <= 2000) %>%
  group_by(article, zone) %>%
  summarise(
    mean_RT = mean(TRT, na.rm = TRUE),
    .groups = "drop")
df_dt <- merge(rts_summary, pred, by = c("article","zone"), sort=F)


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

# df_os <- read_csv("../data/os_merged.csv", show_col_types = FALSE) %>% drop_na()
# df_os <- df_os %>%
#   filter(is_punct==0, bos==0, eos==0) %>%
#   select(-is_punct, -bos, -eos) %>%
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


df_ns$source <- "Natural Stories"
df_dfp$source <- "Dundee First pass"
df_dgp$source <- "Dundee Go-past"
df_dt$source <- "Dundee Total fixation"
df_osfp$source <- "OneStop First pass"
df_osgp$source <- "OneStop Go-past"
df_ost$source <- "OneStop Total fixation"

df_all_RT <- bind_rows(
  df_ns %>% select(mean_RT, source),
  df_dfp %>% select(mean_RT, source),
  df_dgp %>% select(mean_RT, source),
  df_dt %>% select(mean_RT, source),
  df_osfp %>% select(mean_RT, source),
  df_osgp %>% select(mean_RT, source),
  df_ost %>% select(mean_RT, source)
)

source_levels <- c(
  "Dundee First pass", 
  "Dundee Go-past", 
  "Dundee Total fixation", 
  "Natural Stories", 
  "OneStop First pass", 
  "OneStop Go-past", 
  "OneStop Total fixation"
)

df_all_RT$source <- factor(df_all_RT$source, levels = source_levels)

p <- ggplot(df_all_RT, aes(x = mean_RT, fill = source)) +
  geom_histogram(
    binwidth = 10
  ) +
  facet_wrap(~ source, nrow = 1) +
  theme_minimal() +
  labs(
    x = "Mean Reading Time (ms)",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 8, hjust = 0.5),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    panel.spacing = unit(0.1, "lines"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
ggsave("../result/pdf/Figure_A1.pdf", p, device = "pdf", width = 7.5, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_A1.png", p, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")


# correlation matrix

labels <- c("zone", "position", "wlen", "unisurp", "GPT2 surprisal", "PMI", "PSG Top-down", "PSG Bottom-up", "PSG Left-corner", "Dependency", "CCG-Right", "CCG-Left")

make_corplot <- function(df, title = "", hide_y = FALSE) {
  cor_matrix <- cor(df %>% select(
    zone, position, wlen, unisurp, surp_gpt2, cpmi,
    topdown_nc, bottomup_nc, leftcorner_nc,
    dep_nc, ccgright_reduce, ccgleft_reduce
  ))
  colnames(cor_matrix) <- rownames(cor_matrix) <- labels
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
  cor_matrix_melt <- melt(cor_matrix, na.rm = TRUE)
  
  p <- ggplot(cor_matrix_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size=1.8) +
    scale_fill_viridis(limits = c(-1, 1), name = "", guide  = "none") +
    ggtitle(title) +
    coord_fixed() +
    theme_minimal(base_size   = 8,
                  base_family = "serif") +
    theme(
      axis.title.x    = element_blank(),
      axis.title.y    = element_blank(),
      text            = element_text(family = "serif"),
      axis.text.x     = element_text(
        angle  = 45,
        hjust  = 1,
        size   = 8
      ),
      axis.text.y     = if (hide_y) element_blank()
      else element_text(size = 8),
      plot.title      = element_text(
        face   = "bold",
        size   = 12
      ),
      legend.position = "none"
    )
  p
}


pred_ns <- read_csv("../data/preds_ns.csv",
                    col_types = cols_only(zone="d", position="d", wlen="d", unigram="d", unisurp="d",surp_gpt2="d",
                                          dep_nc="d", topdown_nc="d", bottomup_nc="d", leftcorner_nc="d",
                                          ccgright_reduce="d", ccgleft_reduce="d",
                                          story="c", punc="d", line_start="d", line_end="d",word="c",pos="c")) %>%
  mutate(cpmi = unisurp - surp_gpt2) %>%
  rename(article = story) %>%
  filter(punc == 0, line_start == 0, line_end == 0) %>%
  select(-punc, -line_start, -line_end)
p1 <- make_corplot(pred_ns, "Natural Stories", TRUE)
pred_dundee <- read_csv("../data/preds_dundee.csv",
                        col_types = cols_only(zone="d", position="d", wlen="d", unigram="d", unisurp="d", surp_gpt2="d",
                                              dep_nc="d", topdown_nc="d", bottomup_nc="d", leftcorner_nc="d",
                                              ccgright_reduce="d", ccgleft_reduce="d", article="c",
                                              punc="d", line_start="d", line_end="d", word="c", pos="c")) %>%
  mutate(cpmi = unisurp - surp_gpt2) %>%
  filter(punc == 0, line_start == 0, line_end == 0) %>%
  select(-punc, -line_start, -line_end) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
p2 <- make_corplot(pred_dundee, "Dundee")

pred_os <- read_csv("../data/preds_os.csv",
                    col_types = cols_only(zone="d", position="d", wlen="d", unigram="d",surp_gpt2="d",
                                          dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",
                                          ccgright_reduce="d", ccgleft_reduce="d",
                                          punc="d", line_start="d", line_end="d",
                                          article_batch="c",article_id="c",difficulty_level="c")) %>%
  mutate(story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  mutate(cpmi = unigram - surp_gpt2) %>%
  rename(unisurp = unigram) %>%
  filter(punc == 0, line_start == 0, line_end == 0) %>%
  select(-punc, -line_start, -line_end)
p3 <- make_corplot(pred_os, "OneStop", TRUE)
# df_os <- read_csv("../data/os_merged.csv", show_col_types = FALSE) %>%
#   mutate(cpmi = unisurp - surp_gpt2) %>%
#   drop_na() %>%
#   filter(is_punct == 0, bos == 0, eos == 0) %>%
#   select(-is_punct, -bos, -eos)
# p3 <- make_corplot(df_os, "OneStop", TRUE)

combined_plot <- (p2 | p1 | p3) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")
ggsave("../result/pdf/Figure_A2.pdf", combined_plot, device = "pdf", width = 7.2, height = 4, units = "in", bg = "transparent")
ggsave("../result/png/Figure_A2.png", combined_plot, device = "png", width = 7.2, height = 4, units = "in", bg = "transparent")


nc_colors <- setNames(
  unname(c(
    cols["group1_1"],
    cols["group1_2"],
    cols["group1_3"],
    cols["group2"],
    cols["group3_1"],
    cols["group3_2"]
  )),
  c("PSG Top-down", "PSG Bottom-up", "PSG Left-corner", "Dependency", "CCG-Right", "CCG-Left")
)

nc_map <- c(
  "PSG Top-down"    = "topdown_nc",
  "PSG Bottom-up"   = "bottomup_nc",
  "PSG Left-corner" = "leftcorner_nc",
  "Dependency"      = "dep_nc",
  "CCG-Right"       = "ccgright_reduce",
  "CCG-Left"        = "ccgleft_reduce"
)

nc_cols <- unname(nc_map)

df_long <- bind_rows(
  pred_dundee %>%
    transmute(dataset = "Dundee", cpmi, across(all_of(nc_cols), ~ .x)),
  pred_ns %>%
    transmute(dataset = "Natural Stories", cpmi, across(all_of(nc_cols), ~ .x)),
  pred_os %>%
    transmute(dataset = "OneStop", cpmi, across(all_of(nc_cols), ~ .x))
) %>%
  pivot_longer(
    cols = all_of(nc_cols),
    names_to = "nc_col",
    values_to = "nodecount"
  ) %>%
  mutate(
    dataset = factor(dataset, levels = c("Dundee", "Natural Stories", "OneStop")),
    nc_type = factor(names(nc_map)[match(nc_col, nc_map)], levels = names(nc_map))
  ) %>%
  drop_na(cpmi, nodecount)


cor_tbl <- df_long %>%
  group_by(dataset, nc_type) %>%
  summarise(
    r   = cor(nodecount, cpmi, method = "pearson",  use = "pairwise.complete.obs"),
    rho = cor(nodecount, cpmi, method = "spearman", use = "pairwise.complete.obs"),
    .groups = "drop"
  ) %>%
  mutate(
    lab1 = glue("r = {sprintf('%.2f', r)}"),
    lab2 = glue("\u03C1 = {sprintf('%.2f', rho)}")
  )

p_corr <- ggplot(df_long, aes(x = nodecount, y = cpmi)) +
  ggrastr::geom_point_rast(alpha = 0.10, size = 0.35, raster.dpi = 600) +
  geom_smooth(
    aes(color = nc_type),
    method = "lm", formula = y ~ x, se = TRUE, linewidth = 1.2
  ) +
  facet_grid(dataset ~ nc_type, scales = "free_y") +
  coord_cartesian(xlim = c(0, 15)) +
  geom_text(
    data = cor_tbl,
    aes(label = lab1),
    x = Inf, y = Inf,
    hjust = 1.05, vjust = 1.35,
    size = 3.0,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = cor_tbl,
    aes(label = lab2),
    x = Inf, y = Inf,
    hjust = 1.05, vjust = 2.55,
    size = 3.0,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = nc_colors, guide = "none") +
  labs(
    x = "Node Count",
    y = TeX("$PMI(w_i;\\,w_{<i})$ (bits)")
  ) +
  theme_bw() +
  theme(
    strip.text.y    = element_text(size = 8),
    strip.text.x    = element_text(size = 8),
    strip.placement = "outside",
    axis.title.y    = element_text(size = 12),
    axis.title.x    = element_text(size = 12),
    axis.text.x     = element_text(size = 6),
    axis.text.y     = element_text(size = 6),
    panel.spacing   = unit(0.1, "lines"),
    legend.position = "none",
    legend.title    = element_blank(),
    legend.text     = element_text(size=8),
    strip.text      = element_text(face = "bold")
  )

ggsave("../result/pdf/Figure_7.pdf", p_corr, device = cairo_pdf, width = 6.2, height = 4, units = "in", bg = "transparent")
ggsave("../result/png/Figure_7.png", p_corr, device = png, width = 6.2, height = 4, units = "in", bg = "transparent")




pred_path <- "../data/preds_ns.csv"
pred <- read_csv(pred_path,col_types=cols_only(zone="d",position="d",wlen="d",unigram="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",story="c",punc="d",line_start="d",line_end="d")) %>%rename(article=story)  %>% filter(punc==0, line_start==0, line_end==0) %>% select(-punc, -line_start, -line_end)
rt_path <- "../data/spr/naturalstories/naturalstories/naturalstories_RTS/processed_RTs.tsv"
rts_summary <- read.table(rt_path, sep="\t", quote="", header=T) %>%
  select(WorkerId, item, zone, RT) %>%
  rename(article=item) %>%
  group_by(article, zone) %>%
  summarise(mean_RT = mean(RT, na.rm = TRUE), .groups = "drop")
df_ns <- merge(rts_summary, pred, by = c("article","zone"), sort=F)

pred_path <- "../data/preds_dundee.csv"
pred <- read_csv(pred_path, col_types = cols_only(zone="d",position="d",wlen="d",unigram="d",dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",ccgright_reduce="d",ccgleft_reduce="d",zone="d",position="d",article="c",punc="d",line_start="d",line_end="d"))  %>% filter(punc==0, line_start==0, line_end==0) %>% select(-punc, -line_start, -line_end)
pred[is.na(pred)] <- 0
rt_path <- "../data/rts_dundee.csv"
rts_summary <- read_csv(rt_path, col_types = cols_only(subject = "c",article = "c",zone = "d",FPRT = "d")) %>%
  select(subject, article, zone, FPRT) %>%
  filter(FPRT > 0, FPRT <= 2000) %>%
  group_by(article, zone) %>%
  summarise(mean_RT = mean(FPRT, na.rm = TRUE), .groups = "drop")
df_dfp <- merge(rts_summary, pred, by = c("article","zone"), sort=F)

# df_os <- read_csv("../data/os_merged.csv", show_col_types = FALSE) %>% drop_na()
# df_os <- df_os %>%
#   filter(is_punct==0, bos==0, eos==0) %>%
#   select(-is_punct, -bos, -eos)
df_os <- read_csv("../data/preds_os.csv",
                    col_types = cols_only(zone="d", position="d", wlen="d", unigram="d",surp_gpt2="d",
                                          dep_nc="d",topdown_nc="d",bottomup_nc="d",leftcorner_nc="d",
                                          ccgright_reduce="d", ccgleft_reduce="d",
                                          punc="d", line_start="d", line_end="d",
                                          article_batch="c",article_id="c",difficulty_level="c")) %>%
  mutate(story = paste(article_batch, article_id, difficulty_level, sep="_")) %>%
  mutate(cpmi = unigram - surp_gpt2) %>%
  filter(punc == 0, line_start == 0, line_end == 0) %>%
  select(-punc, -line_start, -line_end)


# plot nodecount freqs
predictor_levels <- c(
  "PSG Top-down",
  "PSG Bottom-up",
  "PSG Left-corner",
  "Dependency",
  "CCG-Right",
  "CCG-Left"
)
cols <- make_grouped_palette_ucd()
names(cols) <- predictor_levels
vars <- c("topdown_nc","bottomup_nc","leftcorner_nc","dep_nc","ccgright_reduce","ccgleft_reduce")
df_list <- list(
  "Natural Stories"= df_ns,
  "Dundee"         = df_dfp,
  "OneStop"        = df_os
)

df_long <- imap_dfr(df_list, ~ {
  .x %>%
    select(all_of(vars)) %>%
    pivot_longer(
      cols      = all_of(vars),
      names_to  = "predictor",
      values_to = "nc"
    ) %>%
    mutate(dataset = .y) %>%
    select(dataset, predictor, nc)
}) %>%
  mutate(
    predictor = recode(predictor,
                       topdown_nc     = "PSG Top-down",
                       bottomup_nc    = "PSG Bottom-up",
                       leftcorner_nc  = "PSG Left-corner",
                       dep_nc         = "Dependency",
                       ccgright_reduce = "CCG-Right",
                       ccgleft_reduce = "CCG-Left"
    ),
    predictor = factor(predictor, levels = c(
      "PSG Top-down",
      "PSG Bottom-up",
      "PSG Left-corner",
      "Dependency",
      "CCG-Right",
      "CCG-Left"
    )),
    nc_bin = ifelse(nc >= 5, "5+", as.character(nc))
  )

df_summary <- df_long %>%
  group_by(dataset, predictor, nc_bin) %>%
  summarise(count = n(), .groups = "drop")

p <- ggplot(df_summary, aes(x = nc_bin)) +
  geom_col(aes(y = count, fill = predictor), width = 1) +
  facet_grid(dataset ~ predictor, scales = "free_y") +
  scale_fill_manual(values = cols) +
  scale_y_continuous(
    name   = "# Words",
    labels = label_number()
  ) +
  scale_x_discrete(
    name    = "Node Count",
    limits  = c(as.character(0:4), "5+"),
    position = "bottom"
  ) +
  theme_bw() +
  theme(
    strip.text.y    = element_text(size = 8),
    strip.text.x    = element_text(size = 8),
    strip.placement = "outside",
    axis.title.y    = element_text(size = 12),
    axis.title.x    = element_text(size = 12),
    axis.text.x     = element_text(size = 6),
    axis.text.y     = element_text(size = 6),
    panel.spacing   = unit(0.1, "lines"),
    legend.position = "none",
    legend.title    = element_blank(),
    legend.text     = element_text(size=8),
    strip.text      = element_text(face = "bold")
  )
ggsave("../result/pdf/Figure_6.pdf", p, device = "pdf", width = 6.2, height = 4, units = "in", bg = "transparent")
ggsave("../result/png/Figure_6.png", p, device = "png", width = 6.2, height = 4, units = "in", bg = "transparent")






# plot dll and coefficients
# baseline does not include surprisal


result_df <- read_csv("../result/dll_10cv_so2.csv", show_col_types = FALSE)

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
    width = 0.04,
    show.legend = FALSE
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
  theme_bw() +
  theme(
    text = element_text(size = 8,  hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave("../result/pdf/Figure_8.pdf", p, device = "pdf", width = 7.5, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_8.png", p, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")


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
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(hjust = 0.5, size = 8),
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

ggsave("../result/pdf/Figure_9.pdf", p2, device = "pdf", width = 7.5, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_9.png", p2, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")


result_df <- read_csv("../result/dll_10cv_so2.csv", show_col_types = FALSE)
surp_df <- read_csv("../result/dll_surp_10cv_so2.csv", show_col_types = FALSE)


for (predictor in unique(result_df$predictor)) {
  ds_subset <- result_df[result_df$predictor == predictor, ]
  p_values <- ds_subset$p_value

  p_adjusted <- p.adjust(p_values, method = "BH")
  signif_codes <- ifelse(
    p_adjusted >= 0.05, "", "*"
  )
  
  result_df$p_adjusted[result_df$predictor == predictor] <- p_adjusted
  result_df$sig[result_df$predictor == predictor] <- signif_codes
}

for (predictor in unique(surp_df$predictor)) {
  ds_subset <- surp_df[surp_df$predictor == predictor, ]
  p_values <- ds_subset$p_value

  p_adjusted <- p.adjust(p_values, method = "BH")
  signif_codes <- ifelse(
    p_adjusted >= 0.05, "", "*"
  )
  
  surp_df$p_adjusted[surp_df$predictor == predictor] <- p_adjusted
  surp_df$sig[surp_df$predictor == predictor] <- signif_codes
}


plot_df <- result_df %>%
  filter(
    (dataset == "Natural Stories" & predictor %in% c("PSG Bottom-up", "Dependency", "CCG-Right")) |
      (dataset == "Dundee First pass" & predictor %in% c("PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "Dundee Go-past" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "Dundee Total fixation" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "OneStop First pass" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "OneStop Go-past" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "PSG Left-corner","Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "OneStop Total fixation" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left"))
  ) %>%
  mutate(dataset = factor(dataset,
                         levels = dataset_levels,
                         labels = c("Dundee First pass",
                                    "Dundee Go-past",
                                    "Dundee Total fixation",
                                    "Natural Stories",
                                    "OneStop First pass",
                                    "OneStop Go-past",
                                    "OneStop Total fixation"))) %>%
  mutate(predictor = factor(predictor,
                           levels = predictor_levels))
surp_df_subset <- surp_df %>%
  filter(
    (dataset == "Natural Stories" & predictor %in% c("PSG Bottom-up", "Dependency", "CCG-Right")) |
      (dataset == "Dundee First pass" & predictor %in% c("PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "Dundee Go-past" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "Dundee Total fixation" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "OneStop First pass" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "OneStop Go-past" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "PSG Left-corner","Dependency", "CCG-Right", "CCG-Left")) |
      (dataset == "OneStop Total fixation" & predictor %in% c("PSG Top-down", "PSG Bottom-up", "Dependency", "CCG-Right", "CCG-Left"))
  ) %>%
  mutate(
    dataset = factor(dataset, levels = dataset_levels),
    predictor = factor(predictor, levels = predictor_levels)
  )

plot_df <- plot_df %>%
  mutate(predictor = factor(predictor, levels = predictor_levels),
         predictor_num = as.numeric(predictor),
         x_plot = predictor_num - 0.2)

surp_df_subset <- surp_df_subset %>%
  mutate(predictor = factor(predictor, levels = predictor_levels),
         predictor_num = as.numeric(predictor),
         x_plot = predictor_num + 0.2)


p <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_errorbar(data = surp_df_subset, aes(x = x_plot, ymin = lower_ci, ymax = upper_ci, color = predictor), width = 0.04, show.legend = FALSE) +
  geom_point(data = surp_df_subset, aes(x = x_plot, y = mean_dll, color = predictor, shape = "w/ surprisal"), fill = "white", size = 2.5) +
  geom_text(data = surp_df_subset, aes(x = x_plot, y = upper_ci + 0.00008, label = sig, color = predictor), size = 4, show.legend = FALSE) +
  geom_point(data = plot_df, aes(x = x_plot, y = mean_dll, color = predictor, shape = "w/o surprisal"), size = 2.5) +
  geom_errorbar(data = plot_df, aes(x = x_plot, ymin = lower_ci, ymax = upper_ci, color = predictor), width = 0.04, show.legend = FALSE) +
  geom_text(data = plot_df, aes(x = x_plot, y = upper_ci + 0.00008, label = sig, color = predictor), size = 4, show.legend = FALSE) +
  scale_color_manual(values = cols, name = "") +
  scale_shape_manual(
    values = c("w/o surprisal" = 19, "w/ surprisal" = 21),
    breaks = c("w/o surprisal", "w/ surprisal"),
    name = ""
  ) +
  guides(
    color = guide_legend(title = "", nrow = 1, order = 1, override.aes = list(shape = 19)),
    shape = guide_legend(title = "", nrow = 1, order = 2, override.aes = list(color = "black", fill = "white"))
  ) +
  scale_x_continuous(breaks = 1:6 - 0.1, labels = predictor_levels, minor_breaks = NULL) +
  facet_wrap(~ dataset, ncol = 7) +
  ylab("Delta Log Likelihood (average per word)") +
  theme_bw() +
  theme(
    text = element_text(size = 8,  hjust = 0.5),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = -10),
    legend.box.margin = margin(t = 0),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave("../result/pdf/Figure_13.pdf", p, device = "pdf", width = 7.5, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_13.png", p, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")




predictor_levels <- c(
  "PSG Top-down",
  "PSG Bottom-up",
  "PSG Left-corner",
  "Dependency",
  "CCG-Right",
  "CCG-Left"
)

type_levels <- c("w[i]", "w[i-1]", "w[i-2]")

full_grid <- expand.grid(
  dataset   = dataset_levels,
  predictor = predictor_levels,
  type      = type_levels,
  stringsAsFactors = FALSE
) %>%
  tibble() %>%
  mutate(
    dataset   = factor(dataset,   levels = dataset_levels),
    predictor = factor(predictor, levels = predictor_levels),
    type      = factor(type,      levels = type_levels)
  )
plot_df <- read_csv("../result/dll_10cv_so2_each.csv",
                    show_col_types = FALSE) %>%
  rename(predictor_code = predictor) %>%
  group_by(dataset) %>%
  mutate(
    p_bh = p.adjust(p_value, method = "BH"),
    sig_bh = case_when(
      p_bh < 0.001 ~ "***",
      p_bh < 0.01  ~ "**",
      p_bh < 0.05  ~ "*",
      TRUE         ~ ""
    )
  ) %>%
  ungroup() %>%
  mutate(
    type = case_when(
      str_detect(predictor_code, "_so1$") ~ "w[i-1]",
      str_detect(predictor_code, "_so2$") ~ "w[i-2]",
      TRUE                               ~ "w[i]"
    ),
    type = factor(type, levels = type_levels),
    
    predictor_base = str_remove(predictor_code, "_so[12]$"),
    
    predictor = case_when(
      predictor_base == "topdown_nc"      ~ "PSG Top-down",
      predictor_base == "bottomup_nc"     ~ "PSG Bottom-up",
      predictor_base == "leftcorner_nc"   ~ "PSG Left-corner",
      predictor_base == "dep_nc"          ~ "Dependency",
      predictor_base == "ccgright_reduce" ~ "CCG-Right",
      predictor_base == "ccgleft_reduce"  ~ "CCG-Left",
      TRUE                                ~ NA_character_
    ),
    
    dataset   = factor(dataset,   levels = dataset_levels),
    predictor = factor(predictor, levels = predictor_levels)
  ) %>%
  mutate(
    mean_coef_plot = if_else(p_bh < 0.05, mean_coef, NA_real_)
  ) %>%
  select(dataset, predictor, type, mean_coef_plot)

plot_df_full <- full_grid %>%
  left_join(plot_df, by = c("dataset", "predictor", "type"))

lims_tbl <- plot_df_full %>%
  group_by(dataset) %>%
  summarise(M = max(abs(mean_coef_plot), na.rm = TRUE), .groups = "drop") %>%
  mutate(
    M = if_else(is.finite(M), M * 1.05, 1)
  )

lims <- setNames(lims_tbl$M, as.character(lims_tbl$dataset))

mk_scale <- function(M) {
  scale_y_continuous(
    limits = c(-M, M),
    breaks = c(-M, 0, M),
    labels = label_number(accuracy = 1.0),
    expand = expansion(mult = 0.02)
  )
}

p2 <- ggplot(plot_df_full,
             aes(x = type, y = mean_coef_plot, fill = predictor)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.8),
           width = 0.8, na.rm = TRUE) +
  scale_fill_manual(values = cols, name = "") +
  guides(fill = guide_legend(title = "", nrow = 1)) +
  ylab("Coefficient Estimate") +
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
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(hjust = 0.5, size = 8),
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

ggsave("../result/pdf/Figure_10.pdf", p2, device = "pdf", width = 7.5, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_10.png", p2, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")







result_df <- read_csv("../result/dll_10cv_so2_surp.csv", show_col_types = FALSE)

dataset_levels <- c(
  "Dundee First pass",
  "Dundee Go-past",
  "Dundee Total fixation",
  "Natural Stories",
  "OneStop First pass",
  "OneStop Go-past",
  "OneStop Total fixation")
plot_df <- result_df %>% mutate(dataset = factor(dataset, levels = dataset_levels))


coeff_long_df <- plot_df %>%
  select(dataset, predictor,
         mean_coef_term_1, mean_coef_term_2, mean_coef_term_3) %>%
  pivot_longer(
    cols      = starts_with("mean_coef_term_"),
    names_to  = "type_raw",
    values_to = "coeff"
  ) %>%
  mutate(
    type = case_when(
      type_raw == "mean_coef_term_1" ~ "w[i]",
      type_raw == "mean_coef_term_2" ~ "w[i-1]",
      type_raw == "mean_coef_term_3" ~ "w[i-2]",
      TRUE ~ type_raw
    ),
    type    = factor(type, levels = c("w[i]", "w[i-1]", "w[i-2]")),
    dataset = factor(dataset,   levels = dataset_levels)
  ) %>%
  group_by(dataset, predictor, type) %>%
  summarise(coeff = mean(coeff), .groups = "drop")

lims_tbl <- coeff_long_df %>%
  group_by(dataset) %>%
  summarise(M = max(coeff, na.rm = TRUE), .groups = "drop") %>%
  mutate(M = M * 1.05)

lims <- setNames(lims_tbl$M, as.character(lims_tbl$dataset))

mk_scale <- function(M) {
  scale_y_continuous(
    limits = c(0, M),
    breaks = c(0, M/2, M),
    labels = label_number(accuracy = 1.0),
    expand = expansion(mult = 0.02)
  )
}

p2 <- ggplot(coeff_long_df, aes(x = type, y = coeff)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_bar(
    stat     = "identity",
    position = position_dodge(width = 0.8),
    width    = 0.8,
    fill     = "#00BFC4",
    color    = "#00BFC4"
  ) +
  ylab("Coefficient Estimate") +
  scale_x_discrete(
    labels = c(
      "w[i]"   = TeX("$w_i$"),
      "w[i-1]" = TeX("$w_{i+1}$"),
      "w[i-2]" = TeX("$w_{i+2}$")
    )
  ) +
  facet_wrap(~ dataset, ncol = 7, scales = "free_y") +
  theme_bw() +
  theme(
    text = element_text(size = 7.8, hjust = 0.5),
    legend.position = "bottom",
    legend.margin = margin(t = -10),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(hjust = 0.5, size = 8),
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

ggsave("../result/pdf/Figure_11.pdf", p2, device = "pdf", width = 7.5, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_11.png", p2, device = "png", width = 7.5, height = 3, units = "in", bg = "transparent")



# residuals
result_df <- read_csv("../result/resid_so2.csv")
result_df <- result_df %>%
  mutate(
    predictor = factor(predictor, levels = c(
      "PSG Top-down",
      "PSG Bottom-up",
      "PSG Left-corner",
      "Dependency",
      "CCG-Right",
      "CCG-Left"
    )))

df_summary <- result_df %>%
  group_by(dataset, predictor, ndmain) %>%
  summarise(
    count      = n(),
    mean_resids = mean(resid),
    se_resids   = std.error(resid),
    .groups    = "drop"
  )

gray_out_data <- data.frame(
  predictor = c("PSG Top-down", "PSG Left-corner", "PSG Left-corner", "PSG Left-corner", "PSG Left-corner", "CCG-Left", "PSG Left-corner", "PSG Left-corner"),
  dataset = c("Dundee First pass", "Dundee First pass", "Dundee Go-past", "Dundee Total fixation", "Natural Stories", "Natural Stories", "OneStop First pass", "OneStop Total fixation")
) %>%
  mutate(
    predictor = factor(predictor, levels = c(
      "PSG Top-down",
      "PSG Bottom-up",
      "PSG Left-corner",
      "Dependency",
      "CCG-Right",
      "CCG-Left"
    )))

p <- ggplot(df_summary, aes(x = ndmain)) +
  geom_errorbar(aes(
    y    = mean_resids,
    ymin = mean_resids - 1.96*se_resids,
    ymax = mean_resids + 1.96*se_resids,
    color=predictor
  ), width = 0.1) +
  geom_point(aes(y = mean_resids, fill=predictor, color=predictor), shape = 21, size = 0.75) +
  facet_grid(predictor ~ dataset, scales = "fixed") +
  geom_rect(
    data = gray_out_data,
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
    fill = "grey90",
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    name    = "Mean Residual",
    breaks = c(-100, 0, 100)
  ) +
  scale_x_continuous(
    name     = "Node Count",
    labels   = label_number(accuracy = 1),
    position = "bottom"
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_bw() +
  theme(
    text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text = element_text(face = "bold")
  )

ggsave("../result/pdf/Figure_A3.pdf",p,device="pdf",width=7.5,height=5,units="in",bg="transparent")
ggsave("../result/png/Figure_A3.png",p,device="png",width=7.5,height=5,units="in",bg="transparent")




wt <- tribble(
  ~dataset,                 ~`PSG Top-down`, ~`PSG Bottom-up`, ~`PSG Left-corner`, ~Dependency, ~`CCG-Right`, ~`CCG-Left`,
  "Dundee First pass",      "--",            "--",             "--",               "--",         "--",      "--",
  "Dundee Go-past",         "--",            "--",             "--",               "-***",       "--",      "--",
  "Dundee Total fixation",  "--",            "--",             "--",               "-***",       "--",      "--",
  "Natural Stories",        "--",            "-*",             "--",               "-***",       "***",     "--",
  "OneStop First pass",     "--",            "--",             "--",               "-*",         "-***",    "--",
  "OneStop Go-past",        "-***",          "-*",             "--",               "-**",        "--",      "--",
  "OneStop Total fixation", "--",            "-**",            "--",               "--",         "-***",    "--"
)

wt1 <- tribble(
  ~dataset,                 ~`PSG Top-down`, ~`PSG Bottom-up`, ~`PSG Left-corner`, ~Dependency, ~`CCG-Right`, ~`CCG-Left`,
  "Dundee First pass",      "--",            "---",            "--",               "--",         "--",      "-*",
  "Dundee Go-past",         "--",            "-**",            "--",               "--",         "--",      "--",
  "Dundee Total fixation",  "--",            "--",             "--",               "-**",        "--",      "--",
  "Natural Stories",        "--",            "--",             "--",               "-*",         "***",     "--",
  "OneStop First pass",     "--",            "--",             "--",               "-***",       "--",      "--",
  "OneStop Go-past",        "--",            "--",             "--",               "--",         "--",      "--",
  "OneStop Total fixation", "--",            "--",             "--",               "-***",       "--",      "--"
)

wt2 <- tribble(
  ~dataset,                 ~`PSG Top-down`, ~`PSG Bottom-up`, ~`PSG Left-corner`, ~Dependency, ~`CCG-Right`, ~`CCG-Left`,
  "Dundee First pass",      "--",            "--",           "--",               "--",         "--",      "***",
  "Dundee Go-past",         "--",            "***",          "--",               "--",         "--",      "--",
  "Dundee Total fixation",  "--",            "--",           "--",               "--",         "***",     "--",
  "Natural Stories",        "--",            "--",           "--",               "--",         "***",     "--",
  "OneStop First pass",     "--",            "--",           "--",               "-*",         "***",     "--",
  "OneStop Go-past",        "--",            "-*",           "--",               "--",         "--",      "--",
  "OneStop Total fixation", "--",            "--",           "--",               "--",         "***",     "--"
)

ds_levels   <- c(
  "Dundee First pass",
  "Dundee Go-past",
  "Dundee Total fixation",
  "Natural Stories",
  "OneStop First pass",
  "OneStop Go-past",
  "OneStop Total fixation"
)

pred_levels <- c(
  "PSG Top-down",
  "PSG Bottom-up",
  "PSG Left-corner",
  "Dependency",
  "CCG-Right",
  "CCG-Left"
)



region_levels <- c("wi", "wi_1", "wi_2")
region_labels <- c(
  wi   = TeX("$w_i$"),
  wi_1 = TeX("$w_{i+1}$"),
  wi_2 = TeX("$w_{i+2}$")
)

prep_wt <- function(wt_df, region_key){
  wt_df %>%
    pivot_longer(-dataset, names_to = "predictor", values_to = "sig") %>%
    mutate(
      dataset   = factor(dataset, levels = ds_levels),
      predictor = factor(predictor, levels = pred_levels),
      region    = factor(region_key, levels = region_levels),
      has_star  = str_detect(sig, "\\*"),
      sign = case_when(
        !has_star              ~ NA_character_,
        str_starts(sig, "-")   ~ "neg",
        TRUE                   ~ "pos"
      )
    )
}


df_long <- bind_rows(
  prep_wt(wt,  "wi"),
  prep_wt(wt1, "wi_1"),
  prep_wt(wt2, "wi_2")
)

tile_h <- 0.2

p <- ggplot(df_long, aes(x = region, y = 1)) +
  geom_tile(
    width  = 1,
    height = tile_h,
    color  = "white",
    fill   = "white",
    linewidth = 0.1
  ) +
  geom_point(
    data = df_long %>% filter(has_star),
    aes(fill = predictor, shape = sign),
    size  = 5,
    color = "black"
  ) +
  facet_grid(dataset ~ predictor, switch = "y") +
  scale_fill_manual(
    values = cols,
    name   = "Predictor"
  ) +
  scale_shape_manual(
    values = c(pos = 24, neg = 25),
    breaks = c("pos", "neg"),
    labels = c(pos = "Positive", neg = "Negative"),
    name   = ""
  ) +
  scale_x_discrete(
    drop = FALSE,
    labels = c(
      wi   = TeX("$w_i$"),
      wi_1 = TeX("$w_{i+1}$"),
      wi_2 = TeX("$w_{i+2}$")
    ),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(1 - tile_h/2, 1 + tile_h/2),
    expand = c(0, 0)
  ) +
  guides(
    fill = "none",
    shape = guide_legend(override.aes = list(fill = "white")) 
  ) +
  theme_bw(base_size = 8) +
  theme(
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.35),
    
    strip.text.x = element_text(size = 7, face = "bold"),
    strip.text.y.left = element_text(
      size = 7, face = "bold",
      angle = 0, hjust = 0, vjust = 0.5
    ),
    strip.text.y = element_text(size = 7, face = "bold", angle = 0),
    strip.switch.pad.grid = unit(0.15, "lines"),
    
    axis.text.x  = element_text(size = 7, color = "black"),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title   = element_blank(),
    
    panel.grid    = element_blank(),
    panel.spacing = unit(0.02, "lines"),
    legend.position = "bottom",
    legend.margin = margin(t = -8),
    legend.text = element_text(size = 8),
  )

ggsave("../result/pdf/Figure_12.pdf", p, device = "pdf", width = 6, height = 3, units = "in", bg = "transparent")
ggsave("../result/png/Figure_12.png", p, device = "png", width = 6, height = 3, units = "in", bg = "transparent")
