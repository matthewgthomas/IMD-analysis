library(tidyverse)
library(geographr)
library(IMD)
library(sf)

# LSOA 2001 to LSOA 2011 lookup
# Source: https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-area-2001-to-lower-layer-super-output-area-2011-to-local-authority-district-2011-lookup-in-england-and-wales-1/about
lookup_lsoa01_lsoa11 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA01_LSOA11_LAD11_EW_LU_ddfe1cd1c2784c9b991cded95bc915a9/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  st_drop_geometry()

lookup_lsoa01_lsoa11 <-
  lookup_lsoa01_lsoa11 |>
  select(lsoa01_code = LSOA01CD, lsoa11_code = LSOA11CD)

imd2004 <- IMD::imd2004_lsoa01_england |> left_join(lookup_lsoa01_lsoa11) |> mutate(year = 2004) |> select(lsoa11_code, year, IMD_decile)
imd2007 <- IMD::imd2007_lsoa01_england |> left_join(lookup_lsoa01_lsoa11) |> mutate(year = 2007) |> select(lsoa11_code, year, IMD_decile)
imd2010 <- IMD::imd2010_lsoa01_england |> left_join(lookup_lsoa01_lsoa11) |> mutate(year = 2010) |> select(lsoa11_code, year, IMD_decile)
imd2015 <- IMD::imd2015_lsoa11_england |> mutate(year = 2015) |>  select(lsoa11_code = lsoa11_code, year, IMD_decile)
imd2019 <- IMD::imd_england_lsoa |> mutate(year = 2019) |>  select(lsoa11_code = lsoa_code, year, IMD_decile)

imd_trends <- bind_rows(imd2004, imd2007, imd2010, imd2015, imd2019)

imd_trends <-
  imd_trends |>

  mutate(IMD_quintile = case_when(
    IMD_decile == 1 | IMD_decile == 2 ~ 1,
    IMD_decile == 3 | IMD_decile == 4 ~ 2,
    IMD_decile == 5 | IMD_decile == 6 ~ 3,
    IMD_decile == 7 | IMD_decile == 8 ~ 4,
    IMD_decile == 9 | IMD_decile == 10 ~ 5
  )) |>

  select(-IMD_decile) |>

  # There are some duplicates due to best-fit lookup between 2001 and 2011 codes, so play it safe and take the most-deprived of any duplicates
  group_by(lsoa11_code, year) |>
  filter(IMD_quintile == min(IMD_quintile)) |>
  ungroup() |>

  distinct() |>

  pivot_wider(names_from = year, values_from = IMD_quintile)

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

# Number of LSOAs in each region
lsoas_per_region <-
  lookup_lsoa11_ltla21 |>
  left_join(lookup_ltla21_region21) |>

  count(region21_name, sort = TRUE, name = "total")

# Number of 20% most deprivation LSOAs in each region in 2019
deprived_lsoas_per_region <-
  IMD::imd_england_lsoa |>
  select(lsoa11_code = lsoa_code, IMD_decile) |>
  filter(IMD_decile <= 2) |>
  left_join(lookup_lsoa11_ltla21) |>
  left_join(lookup_ltla21_region21) |>

  count(region21_name, sort = TRUE, name = "total_deprived")

# In which regions are most areas that get stuck in deprivation?
imd_unchanged |>
  filter(`2004` <= 2) |>
  left_join(lookup_lsoa11_ltla21) |>
  left_join(lookup_ltla21_region21) |>

  count(region21_name, sort = TRUE) |>

  left_join(lsoas_per_region) |>
  mutate(proportion = n / total) |>

  left_join(deprived_lsoas_per_region) |>
  mutate(proportion_most_deprived = n / total_deprived)

#--> North West, Yorkshire, West Mids
# More than half of the most deprived areas of the North West, North East, Yorkshire, and West Mids in 2019 have been among the country's most deprived areas since 2004
# Nearly one in five neighbourhoods in the North West have been among the nation's most deprived for at least 15 years

# ---- Is persistent deprivation more likely in what are now known as left-behind areas? ----
# LSOA to ward lookup
# Source: https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-area-2011-to-ward-2017-lookup-in-england-and-wales-1/about
lookup_lsoa11_ward17 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA11_WD17_EW_LU_c65dfe9ca2fa4b0fa578b65a4d5c9856/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

lookup_lsoa11_ward17 <-
  lookup_lsoa11_ward17 |>
  st_drop_geometry() |>
  select(lsoa11_code = LSOA11CD, ward17_code = WD17CD)

#
lba_trends <-
  imd_trends |>

  left_join(lookup_lsoa11_ward17) |>
  left_join(IMD::cni_england_ward17) |>

  mutate(
    core20 = if_else(`2004` <= 1, "20% most deprived", "Less deprived areas"),
    deprivation_unchanged = if_else(`2004` == `2007` & `2007` == `2010` & `2010` == `2015` & `2015` == `2019`, "Persistent", "Changing"),
    lba = if_else(`Left Behind Area?`, "Left-behind area", "Other")
  )

lba_trends |>
  janitor::tabyl(core20, deprivation_unchanged, lba)

lba_trends |>
  group_by(lba, core20, deprivation_unchanged) |>
  summarise(n = n()) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>

  drop_na() |>

  mutate(core20 = fct_rev(core20)) |>

  ggplot(aes(x = core20, y = prop)) +
  geom_col(aes(fill = deprivation_unchanged)) +
  facet_wrap(~lba) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#d7d8d7", "#ee2a24")) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "top",
    plot.background = element_rect(fill = "white"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.subtitle = element_text(colour = "#5c747a")
  ) +
  labs(
    title = "Deprivation is most persistent in England's left behind' neghbourhoods",
    subtitle = "'Deprivation status' is based on whether a small area's deprivation quintile has \nchanged since 2004 ('Changing') or not ('Persistent').",
    fill = "Deprivation status",
    x = NULL,
    y = NULL,
    caption = "@matthewgthomas analysis of DLUHC and OCSI data"
  )

ggsave("output/persistent deprivation in left behind areas.png", width = 150, height = 100, units = "mm")
