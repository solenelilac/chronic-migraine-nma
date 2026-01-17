##########################################################
# Frequentist NMA – Chronic Migraine
##########################################################
# Author: Lilac Zihui Zhao
# Date: 16/01/2026
#
# Outputs (base-case):
#  - fig_network.png
#  - fig_forest_vs_placebo.png
#  - fig_ranking_pscore.png
#  - table_summary_stats.tex
#  - table_effects_vs_placebo.tex
#  - table_league.tex
#  - table_ranking_pscore.tex
#
# Outputs (sensitivity):
#  - table_base_vs_sensitivity.tex
##########################################################

# Packages
library(readxl)
library(dplyr)
library(netmeta)
library(knitr)

####################################
# Load data
####################################
df_raw <- read_excel("C:/Users/big_d/Dropbox/DS/Chronic_Migraine_dataset.xlsx")

####################################
# Data cleaning
####################################
df <- df_raw %>%
  mutate(
    trt   = as.character(trt),
    study = as.character(study),
    na    = as.numeric(na),
    TE    = as.numeric(y),
    seTE  = suppressWarnings(as.numeric(se))
  ) %>%
  filter(trt != "Placebo") %>%
  transmute(
    studlab     = study,
    treat1      = trt,
    treat2      = "Placebo",
    TE          = TE,
    seTE        = seTE,
    num_of_arms = na
  ) %>%
  filter(!is.na(seTE), seTE > 0) %>%
  mutate(
    # Multi-arm trials: no covariance available, so treat each contrast as independent
    studlab = ifelse(num_of_arms > 2, paste0(studlab, "_", make.names(treat1)), studlab)
  )


####################################
# Summary Statistics
####################################
n_studies   <- n_distinct(df$studlab)
n_contrasts <- nrow(df)
treatments  <- paste(sort(unique(df$treat1)), collapse = ", ")

summary_table <- data.frame(
  Characteristic = c("Number of studies",
                     "Number of treatment--placebo contrasts",
                     "Treatments included"),
  Value = c(n_studies,
            n_contrasts,
            treatments)
)


####################################
# Base-case NMA (common-effect)
####################################
nma <- netmeta(
  TE, seTE,
  treat1, treat2,
  studlab,
  data = df,
  sm = "MD",
  reference.group = "Placebo",
  common = TRUE,
  random = FALSE
)

summary(nma)


################################
# Ranking (P-scores)
################################
rnk <- netrank(nma, small.values = "good")

ps <- rnk$ranking.common

table_rank <- data.frame(
  Treatment = names(ps),
  Pscore = as.numeric(ps)
) %>%
  filter(!is.na(Pscore), Treatment != "", Treatment != "Placebo") %>%
  arrange(desc(Pscore)) %>%
  mutate(Pscore = round(Pscore, 3))


##########################################################################
# Sensitivity NMA (exclude outlier study label starting "Cleverley")
##########################################################################
df_sens <- df %>% filter(!grepl("^Cleverley", studlab))

nma_sens <- netmeta(
  TE, seTE,
  treat1, treat2,
  studlab,
  data = df_sens,
  sm = "MD",
  reference.group = "Placebo",
  common = TRUE,
  random = FALSE
)

summary(nma_sens)




####################################
# Figures (base-case)
####################################

################
# Network plot
################
png("fig_network.png", 1800, 1200, res = 200)
netgraph(nma, points = TRUE, cex.points = 4, thickness = "number.of.studies", 
         col.points = "skyblue3", col = "grey40",  cex = 1.2, multiarm = FALSE,number=FALSE)
dev.off()


################################################################
# Forest plot vs placebo (sorted by pooled effect vs placebo)
################################################################
sv <- nma$TE.common[, "Placebo"]

png("fig_forest_vs_placebo.png", width = 7.2, height = 6.0, units = "in", res = 300)

par(mar = c(3, 2.0, 2.0, 0.6)) 

forest(
  nma,
  ref = "Placebo",
  sortvar = sv,
  digits = 2,
  xlab = "Mean difference vs placebo (negative = fewer migraine days)",
  main = "Pooled treatment effects versus placebo"
)

dev.off()


################################
# Ranking plot (P-scores)
################################
png("fig_ranking_pscore.png", width = 2000, height = 1400, res = 220)

op <- par(no.readonly = TRUE)
par(mar = c(3.5, 6, 1.8, 0.6) + 0.1, xpd = NA)

bp <- barplot(
  table_rank$Pscore,
  names.arg = table_rank$Treatment,
  las = 1,                 
  cex.names = 1.1,        
  cex.axis  = 1.1,
  cex.lab   = 1.2,
  ylim = c(0, 1),
  ylab = "P-score (higher = better)",
  main = "Treatment ranking (P-scores)"
)

text(
  x = bp,
  y = table_rank$Pscore,
  labels = sprintf("%.3f", table_rank$Pscore),
  pos = 3,
  cex = 1.0
)

par(op)
dev.off()

####################################
# Tables (base-case)
####################################

################################
# Effects vs placebo table
################################
TE_mat <- nma$TE.common
SE_mat <- nma$seTE.common  # important: SE matrix

ref  <- "Placebo"
trts <- setdiff(rownames(TE_mat), ref)

table_eff <- data.frame(
  Treatment = trts,
  MD = as.numeric(TE_mat[trts, ref]),
  SE = as.numeric(SE_mat[trts, ref])
)
table_eff$Lower <- table_eff$MD - 1.96 * table_eff$SE
table_eff$Upper <- table_eff$MD + 1.96 * table_eff$SE

table_eff <- table_eff %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

writeLines(
  kable(
    table_eff,
    format = "latex",
    booktabs = TRUE,
    caption = "Pooled mean differences versus placebo (common-effect NMA). Negative values indicate fewer monthly migraine days.",
    label = "tab:effects_vs_placebo"
  ),
  "table_effects_vs_placebo.tex"
)

################
# League table
################
lg <- netleague(nma, digits = 2)

writeLines(
  kable(
    as.data.frame(lg$common),
    format = "latex",
    booktabs = TRUE,
    caption = "League table of pooled mean differences (row vs column; common-effect NMA).",
    label = "tab:league"
  ),
  "table_league.tex"
)

################################
# Ranking (P-scores) table
################################
table_rank_tex <- table_rank %>%
  mutate(Pscore = round(Pscore, 3))

writeLines(
  kable(
    table_rank_tex,
    format = "latex",
    booktabs = TRUE,
    caption = "Treatment ranking based on P-scores (higher indicates better performance).",
    label = "tab:ranking_pscore"
  ),
  "table_ranking_pscore.tex"
)

####################################
# Sensitivity comparison table
####################################
TE2 <- nma_sens$TE.common
SE2 <- nma_sens$seTE.common
trts2 <- setdiff(rownames(TE2), ref)

table_sens <- data.frame(
  Treatment = trts2,
  MD_sens = as.numeric(TE2[trts2, ref]),
  SE_sens = as.numeric(SE2[trts2, ref])
)
table_sens$Lower_sens <- table_sens$MD_sens - 1.96 * table_sens$SE_sens
table_sens$Upper_sens <- table_sens$MD_sens + 1.96 * table_sens$SE_sens

compare_tbl <- merge(table_eff, table_sens, by = "Treatment")
compare_tbl <- compare_tbl %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

writeLines(
  kable(
    compare_tbl,
    format = "latex",
    booktabs = TRUE,
    caption = "Sensitivity analysis: base-case vs excluding outlier study.",
    label = "tab:base_vs_sensitivity"
  ),
  "table_base_vs_sensitivity.tex"
)

cat("\nSaved figures:\n",
    " - fig_network.png\n",
    " - fig_forest_vs_placebo.png\n",
    " - fig_ranking_pscore.png\n\n",
    "Saved LaTeX tables:\n",
    " - table_summary_stats.tex\n",
    " - table_effects_vs_placebo.tex\n",
    " - table_league.tex\n",
    " - table_ranking_pscore.tex\n",
    " - table_base_vs_sensitivity.tex\n", sep = "")
