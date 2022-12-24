library(tidyverse)
library(compositr)
library(geographr)
library(readxl)
library(broom)
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

# Ward 2017 to LAD 2017 lookup
lookup_ward17_ltla17 <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD17_LAD17_UK_LU_4aa38f5449104ea197413eac1a582a02/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

lookup_ltla17_ltla19 <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/LAD%202017%20to%20LAD%202019%20codes.csv")

lookup_ward17_ltla21 <-
  lookup_ward17_ltla17 |>
  st_drop_geometry() |>
  left_join(lookup_ltla17_ltla19) |>
  left_join(geographr::lookup_ltla_ltla, by = c("LAD19CD" = "ltla19_code")) |>
  select(ward17_code = WD17CD, ltla21_code)

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

  # Some MSOAs appear in more than one ward. Keep the one with the greatest community needs (i.e. lowest rank)
  # (or use `max` instead of `min` to be conservative and use wards with the least community needs - it doesn't qualitatively change the results)
  group_by(msoa11_code, `Left Behind Area?`, `Life expectancy`, domain) |>
  summarise(rank = min(rank)) |>
  ungroup() |>

  ggplot(aes(x = rank, y = `Life expectancy`, colour = `Left Behind Area?`, fill = `Left Behind Area?`)) +
  # geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~domain)

le_male |>
  select(msoa11_code, `Left Behind Area?`, `Life expectancy`, `Civic Assets rank`, `Connectedness rank`, `Engaged community rank`) |>
  pivot_longer(cols = ends_with("rank"), names_to = "domain", values_to = "rank") |>

  # Some MSOAs appear in more than one ward. Keep the one with the greatest community needs (i.e. lowest rank)
  # (or use `max` instead of `min` to be conservative and use wards with the least community needs - it doesn't qualitatively change the results)
  group_by(msoa11_code, `Left Behind Area?`, `Life expectancy`, domain) |>
  summarise(rank = min(rank)) |>
  ungroup() |>

  ggplot(aes(x = rank, y = `Life expectancy`, colour = `Left Behind Area?`, fill = `Left Behind Area?`)) +
  # geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  facet_wrap(~domain)

# ---- Explore relationship using LA-level data ----
# - Summarise Community Needs Index into Local Authorities -
cni_ltla21_lba <-
  IMD::cni_england_ward17 |>
  left_join(lookup_ward17_ltla21) |>
  group_by(ltla21_code) |>
  summarise(
    lba_n = sum(`Left Behind Area?`),
    lba_prop = lba_n / n()
  ) |>
  ungroup()

cni_england_ward17 <-
  IMD::cni_england_ward17 |>
  left_join(lookup_ward17_ltla21) |>
  mutate(
    `Civic Assets decile` = quantise(`Civic Assets rank`, num_quantiles = 10),
    `Connectedness decile` = quantise(`Connectedness rank`, num_quantiles = 10),
    `Engaged community decile` = compositr::quantise(`Engaged community rank`, num_quantiles = 10)
  )

cni_ltla21_assets <-
  cni_england_ward17 |>
  IMD:::calculate_proportion(`Civic Assets decile`, higher_level_geography = ltla21_code) |>
  rename(assets_prop = Proportion)

cni_ltla21_connectedness <-
  cni_england_ward17 |>
  IMD:::calculate_proportion(`Connectedness decile`, higher_level_geography = ltla21_code) |>
  rename(connectedness_prop = Proportion)

cni_ltla21_engaged <-
  cni_england_ward17 |>
  IMD:::calculate_proportion(`Engaged community decile`, higher_level_geography = ltla21_code) |>
  rename(engaged_prop = Proportion)

cni_ltla21 <-
  cni_ltla21_lba |>
  left_join(cni_ltla21_assets) |>
  left_join(cni_ltla21_connectedness) |>
  left_join(cni_ltla21_engaged)

# - Life expectancy by Local Authority -
# Source: https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk
tf <- download_file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/datasets/healthstatelifeexpectancyallagesuk/current/reftablehsle1820new.xlsx", ".xlsx")

le_lad <- read_excel(tf, sheet = "HE - Local area estimates", skip = 0)

# Most recent data for England
le_birth_ltla21 <-
  le_lad |>
  filter(
    Period == "2018-20",
    str_detect(Code, "^E"),
    Ageband == 1
  ) |>
  select(Period, ltla21_code = Code, Area_name, Sex, LE, HLE) |>

  left_join(cni_ltla21)

# Plot correlation between life expectancy and domains of community needs in Local Authorities
le_birth_ltla21 |>
  pivot_longer(cols = ends_with("_prop")) |>

  ggplot(aes(x = value, y = LE, colour = Sex, fill = Sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name)

# Plot correlation between healthy life expectancy and domains of community needs in Local Authorities
le_birth_ltla21 |>
  pivot_longer(cols = ends_with("_prop")) |>

  ggplot(aes(x = value, y = HLE, colour = Sex, fill = Sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name)

# How do the CNI domains predict life expectancy?
#---> Engagement has the strongest negative effect on life expectancy
summary(lm(LE ~ assets_prop + connectedness_prop + engaged_prop, data = le_birth_ltla21))
summary(lm(HLE ~ assets_prop + connectedness_prop + engaged_prop, data = le_birth_ltla21))

lm(LE ~ assets_prop + connectedness_prop + engaged_prop, data = le_birth_ltla21) |>
  tidy(conf.int = TRUE) |>
  filter(term != "(Intercept)") |>

  ggplot(aes(term, estimate)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip() +
  labs(title = "Predictors of life expectancy in Local Authorities")
