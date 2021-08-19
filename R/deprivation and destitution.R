library(tidyverse)
library(GGally)
library(IMD)

destitution <- read_csv("data/destitution.csv") |>
  rename(lad_code = lad17cd)

imd_dest <-
  imd_england_lad |>
  select(lad_code, Score, Proportion, Extent) |>
  left_join(destitution)

imd_dest |>
  select(Score:destitution_all) |>
  ggpairs(lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.01)), diag = NULL) +
  theme_classic()

# ---> destitution is strongly aligned with population-weighted average deprivation scores
# less so with extent and proportion
# it'd be interesting to look at lower deprivation places with higher destitution (but there aren't many high deprivation places with low destitution)
