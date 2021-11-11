##
## Places that are in the most-deprived decile across domains (or sub-domains, for 'barriers' in England)
##
library(tidyverse)
library(geographr)
library(IMD)
library(sf)

eimd <-
  IMD::imd_england_lsoa |>
  left_join(
    IMD::imd_england_lsoa_subdomains |>
      select(lsoa_code, starts_with("Geographical_barriers"), starts_with("Wider_barriers")),
    by = "lsoa_code"
  )

eimd_deprived <-
  eimd |>
  filter(
    Income_decile <= 2,
    Employment_decile <= 2,
    Education_decile <= 2,
    Health_decile <= 2,
    Crime_decile <= 2,
    Environment_decile <= 2,
    Geographical_barriers_decile <= 2,
    Wider_barriers_decile <= 2
  )

eimd_deprived |>
  select(lsoa_code) |>

  left_join(
    geographr::boundaries_lsoa |>
      st_drop_geometry()
  )

