library(tidyverse)
library(geographr)
library(IMD)
library(sf)

lookup_lsoa01_lsoa11 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA01_LSOA11_LAD11_EW_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  st_drop_geometry()

lookup_lsoa01_lsoa11 <-
  lookup_lsoa01_lsoa11 |>
  select(lsoa01_code = LSOA01CD, lsoa11_code = LSOA11CD)

imd2004 <- IMD::imd2004_lsoa01_england |> left_join(lookup_lsoa01_lsoa11) |> mutate(year = 2004) |> select(lsoa11_code, year, IMD_decile)
imd2007 <- IMD::imd2007_lsoa01_england |> left_join(lookup_lsoa01_lsoa11) |> mutate(year = 2007) |> select(lsoa11_code, year, IMD_decile)
imd2010 <- IMD::imd2010_lsoa01_england |> left_join(lookup_lsoa01_lsoa11) |> mutate(year = 2010) |> select(lsoa11_code, year, IMD_decile)
imd2015 <- IMD::imd2015_lsoa11_england |> mutate(year = 2015) |>  select(lsoa11_code = lsoa_code, year, IMD_decile)
imd2019 <- IMD::imd_england_lsoa |> mutate(year = 2019) |>  select(lsoa11_code = lsoa_code, year, IMD_decile)

imd_trends <- bind_rows(imd2004, imd2007, imd2010, imd2015, imd2019)

imd_trends <-
  imd_trends |>
  # There are some duplicates due to best-fit lookup between 2001 and 2011 codes, so play it safe and take the most-deprived of any duplicates
  group_by(lsoa11_code, year) |>
  filter(IMD_decile == min(IMD_decile)) |>
  ungroup() |>

  distinct() |>

  pivot_wider(names_from = year, values_from = IMD_decile)

imd_unchanged <-
  imd_trends |>
  filter(`2004` == `2007` & `2007` == `2010` & `2010` == `2015` & `2015` == `2019`)

nrow(imd_unchanged) / nrow(imd_trends)

# Are you more likely to get stuck in deprivation? Yes
imd_unchanged |>
  janitor::tabyl(`2019`)

imd_unchanged |>
  mutate(core20 = if_else(`2004` <= 2, "20% most deprived", "Other")) |>
  janitor::tabyl(core20)
