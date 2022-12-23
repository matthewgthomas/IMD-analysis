library(tidyverse)
library(compositr)
library(geographr)
library(readxl)
library(IMD)
library(sf)

# ---- Lookup tables ----
# LSOA 2011 to Ward 2017 lookup
# Source: https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-area-2011-to-ward-2017-lookup-in-england-and-wales-1/about
lookup_lsoa11_ward17 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA11_WD17_EW_LU_c65dfe9ca2fa4b0fa578b65a4d5c9856/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

lookup_lsoa11_ward17 <-
  lookup_lsoa11_ward17 |>
  st_drop_geometry() |>
  select(lsoa11_code = LSOA11CD, ward17_code = WD17CD)

# MSOA to Ward lookup
lookup_msoa11_ward17 <-
  lookup_lsoa11_msoa11 |>
  select(lsoa11_code, msoa11_code) |>
  left_join(lookup_lsoa11_ward17) |>
  distinct(msoa11_code, ward17_code)

# ---- Load/wrangle life expectancy data (MSOA-level) ----
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/lifeexpectancyleandhealthylifeexpectancyhleatbirthbysexformiddlelayersuperoutputareasmsoasinengland
tf <- download_file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/lifeexpectancyleandhealthylifeexpectancyhleatbirthbysexformiddlelayersuperoutputareasmsoasinengland/current/hlereferencetable2_tcm77-417533.xls", ".xls")

le_female <- read_excel(tf, sheet = "MSOA_females", skip = 7)
le_male <- read_excel(tf, sheet = "MSOA_males", skip = 7)

le_female <-
  le_female |>
  select(msoa11_code = `MSOA Codes`, Region, `Life expectancy` = `LE (Years)`) |>
  filter(str_detect(msoa11_code, "^E")) |>
  left_join(lookup_msoa11_ward17, by = "msoa11_code") |>
  left_join(IMD::cni_england_ward17)

le_male <-
  le_male |>
  select(msoa11_code = `MSOA Codes`, Region, `Life expectancy` = `LE (Years)`) |>
  filter(str_detect(msoa11_code, "^E")) |>
  left_join(lookup_msoa11_ward17, by = "msoa11_code") |>
  left_join(IMD::cni_england_ward17)

# Plot life expectancy by CNI domain
le_female |>
  select(msoa11_code, `Left Behind Area?`, `Life expectancy`, `Civic Assets rank`, `Connectedness rank`, `Engaged community rank`) |>
  pivot_longer(cols = ends_with("rank"), names_to = "domain", values_to = "rank") |>

  # arrange(desc(`Life expectancy`)) |>

  ggplot(aes(x = rank, y = `Life expectancy`, colour = `Left Behind Area?`, fill = `Left Behind Area?`)) +
  # geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~domain)
