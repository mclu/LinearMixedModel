# LinearMixedModel
#
# The script utilizes the functions written for mousetrap::KH2017_raw and 
# explores how each curvature measure differs by condition by fitting linear 
# mixed models.
#
# Author: Ming-Chen Lu (mingchlu@umich.edu)
# Updated: Dec 29, 2019
#80: ---------------------------------------------------------------------------

# libraries: -------------------------------------------------------------------
library(tidyverse, warn.conflicts = FALSE)
library(mousetrap)
library(lme4)

# Functions: -------------------------------------------------------------------
# setwd("/Users/Amy/Desktop/LinearMixedModel")
source('./funcs.R')

# Function to split a list of factor: ------------------------------------------
split_x = function(x) {
  # This function converts a list of factor into a numeric vector.
  # 
  # Inputs: x - a list of factor
  # Outputs: a numeric vector
  
  x %>% 
    as.character() %>%
    substr( 2, str_length(x)-1 ) %>%
    str_split( ", " ) %>%
    unlist() %>%
    as.double()
}

# Represent the trajectories in a numeric format: ------------------------------
ms_tib = 
  KH2017_raw %>%
  select( subject_nr, count_trial, xpos_get_response, 
          ypos_get_response, timestamps_get_response ) %>%
  transmute( subject_nr = subject_nr,
             count_trial = count_trial,
             xpos = lapply(xpos_get_response, split_x),
             ypos = lapply(xpos_get_response, split_x),
             t = lapply(timestamps_get_response, split_x) ) %>%
  as_tibble()

# Compute curvature measures and present in one row per subject and trial: -----
ms_tidy = 
  ms_tib %>%
  group_by(subject_nr, count_trial) %>%
  do( results = 
        matrix(unlist( c(.$xpos, .$ypos, .$t) ), ncol = 3 ) %>%
        normalize_matrix() %>%
        get_curvature()
  ) %>%
  unnest_wider(results)

# Filter to trials in which the subject choose the right response: -------------
df = 
  KH2017_raw %>%
  filter( correct == 1 ) %>%
  select( subject_nr, count_trial, Condition, Exemplar ) %>%
  left_join( ms_tidy )

# Make "typical" the reference level: ------------------------------------------
df = mutate(df, Condition = factor(Condition, c('Typical', 'Atypical')))

# Fit linear mixed models: -----------------------------------------------------
lm_dist = lmer( log(dist) ~ Condition + (1|subject_nr) + (1|Exemplar), 
                data = df )
lm_maxd = lmer( log(max_dev) ~ Condition + (1|subject_nr) + (1|Exemplar), 
                data = df )
lm_avgd = lmer( log(mean_dev) ~ Condition + (1|subject_nr) + (1|Exemplar), 
                data = df )
lm_auc = lmer( log(area) ~ Condition + (1|subject_nr) + (1|Exemplar), 
               data = df )

summary(lm_dist)

cap = paste0("*Linear mixed models comparison for four log transformed*", 
             "*curvature measures*",
             "*Each model takes 'Condition' as fixed effect and 'subject_nr'*",
             "*,'Exemplar' as random effects.*",
             "*The table presents coefficients and standard error for*",
             "*fixed effect of condition.*")

df_lm = data.frame( 
  "Total_Distance" = c(exp(lm_dist@beta[2]), sqrt(diag(vcov(lm_dist)))[2]),
  "Max_Abs_Dev" = c(exp(lm_maxd@beta[2]), sqrt(diag(vcov(lm_maxd)))[2]),
  "Avg_Abs_Dev" = c(exp(lm_avgd@beta[2]), sqrt(diag(vcov(lm_avgd)))[2]),
  "AUC" = c(exp(lm_auc@beta[2]), sqrt(diag(vcov(lm_auc)))[2])
)
rownames(df_lm) = c('Coefficients', 'Std. Error')
knitr::kable(df_lm, caption = cap, align = 'c')
## The result shows that the atypical condition had the largest effect on the 
## total Euclidean distance traveled in the experiment.
