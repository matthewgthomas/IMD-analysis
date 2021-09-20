library(tidyverse)
library(geographr)
library(IMD)
library(GGally)

imd_england_all <-
  left_join(
    IMD::imd_england_lsoa,
    IMD::imd_england_lsoa_subdomains
  )

# Create LSOA to Region lookup
lsoa_region <-
  geographr::lookup_lsoa_msoa |>
  left_join(geographr::lookup_msoa_lad) |>
  left_join(geographr::lookup_lad_region) |>

  select(lsoa_code, region_name)

imd_england_all <-
  imd_england_all |>
  left_join(lsoa_region)

imd_pairs <-
  imd_england_all |>
  select(ends_with("rank"), region_name) |>

  ggpairs(
    columns = 1:14,
    mapping = aes(colour = region_name),
    diag = NULL,
    lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.01))
  ) +
  theme_classic()

ggsave(plot = imd_pairs, file = "output/domain correlation by region.png", width = 1000, height = 1000, units = "mm")
