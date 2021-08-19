library(tidyverse)
library(GGally)
library(IMD)

# 10 = highest destitution
destitution <-
  read_csv("data/destitution.csv") |>
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

lad_names <-
  geographr::boundaries_lad |>
  sf::st_drop_geometry()

# Highest deprivation in the 20% least destitute areas
imd_dest |>
  left_join(lad_names) |>
  filter(destitution_all <= 2) |>
  arrange(desc(Extent))

# Lowest deprivation in the 20% most destitute areas
imd_dest |>
  left_join(lad_names) |>
  filter(destitution_all >= 9) |>
  arrange(Extent)
