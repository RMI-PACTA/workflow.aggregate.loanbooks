# coverage of ABCD----
library(dotenv)
library(tidyverse)

## set up project----
dotenv::load_dot_env()

if (file.exists(here::here(".env"))) {
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))
  input_path_scenario_tms <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_TMS"))
  input_path_scenario_sda <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_SDA"))
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- Sys.getenv("DIR_OUTPUT")
} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# read abcd
abcd <- readr::read_csv(file.path(input_path_abcd))
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

## specify region and countries for analysis----
scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
region_select <- Sys.getenv("PARAM_REGION_SELECT")
input_path_scenario_intermediate <- Sys.getenv("DIR_SCENARIO_INTERMEDIATE")

regions_geco_2022 <- readr::read_csv(input_path_regions_geco_2022)
regions_weo_2022 <- readr::read_csv(input_path_regions_weo_2022)

region_isos_complete <- r2dii.data::region_isos %>%
  rbind(regions_geco_2022) %>%
  rbind(regions_weo_2022)

region_isos_select <- region_isos_complete %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input,
    .data$region %in% .env$region_select
  )

countries_select <- region_isos_select %>%
  dplyr::pull(.data$isos) %>%
  toupper()

load(file.path(input_path_scenario_intermediate, "weo_2022.rda"))

load(file.path(input_path_scenario_intermediate, "geco_2022.rda"))


## prepare scenario benchmarks for coverage analysis----
### prepare benchmark: IEA WEO 2022----
weo2022_region_select <- weo_2022 %>%
  dplyr::mutate(scenario_geography = tolower(.data$scenario_geography)) %>%
  dplyr::filter(
    .data$year == 2021,
    .data$scenario_geography == .env$region_select,
    .data$scenario == "APS"
  ) %>%
  dplyr::summarise(
    production = sum(.data$value, na.rm = TRUE),
    .by = c("source", "scenario", "scenario_geography", "sector", "technology", "indicator", "units", "year")
  ) %>%
  dplyr::distinct(
    .data$source, .data$scenario_geography, .data$sector, .data$technology, .data$indicator,
    .data$units, .data$year, .data$production
  ) %>%
  dplyr::mutate(
    technology = tolower(.data$technology)
  )

# get steel data for 2021 from https://www.iea.org/data-and-statistics/charts/global-steel-production-in-the-net-zero-scenario-2010-2030-4558
steel_raw <- readr::read_csv(
  file = file.path(input_path_scenario, "global-steel-production-in-the-net-zero-scenario-2010-2030.csv"),
  skip = 3
)

steel <- steel_raw %>%
  janitor::clean_names() %>%
  dplyr::rename(
    year = "x1",
    global = "world"
  ) %>%
  dplyr::mutate(
    global = dplyr::if_else(
      is.na(.data$global),
      .data$advanced_economies + .data$emerging_market_and_developing_economies,
      .data$global
    )
  ) %>%
  tidyr::pivot_longer(
    cols = -"year",
    names_to = "scenario_geography",
    values_to = "production"
  )

steel_raw_unit <- readr::read_csv(
  file = file.path(input_path_scenario, "global-steel-production-in-the-net-zero-scenario-2010-2030.csv"),
  col_names = FALSE,
  skip = 2,
  n_max = 1
)

steel_unit <- steel_raw_unit %>%
  dplyr::rename(units = "X1") %>%
  dplyr::mutate(units = gsub("Units: ", "", .data$units))

steel <- steel %>%
  dplyr::bind_cols(steel_unit) %>%
  dplyr::mutate(
    source = "IEA Steel and Iron 2021",
    sector = "Steel",
    indicator = "Production"
  ) %>%
  dplyr::filter(
    .data$year == 2021,
    .data$scenario_geography == .env$region_select
  )

# get cement data for 2021 from https://www.iea.org/data-and-statistics/charts/global-cement-production-in-the-net-zero-scenario-2010-2030-4537
cement_raw <- readr::read_csv(
  file = file.path(input_path_scenario, "global-cement-production-in-the-net-zero-scenario-2010-2030.csv"),
  col_types = "ddd",
  skip = 3
)

cement <- cement_raw %>%
  janitor::clean_names() %>%
  dplyr::rename(
    year = "x1",
    production = "historical_emissions"
  ) %>%
  dplyr::select(-"x4") %>%
  dplyr::select(c("year", "production"))

cement_raw_unit <- readr::read_csv(
  file = file.path(input_path_scenario, "global-cement-production-in-the-net-zero-scenario-2010-2030.csv"),
  col_names = FALSE,
  skip = 2,
  n_max = 1
)

cement_unit <- cement_raw_unit %>%
  dplyr::rename(units = "X1") %>%
  dplyr::mutate(units = gsub("Units: Cement production in ", "", .data$units))

cement <- cement %>%
  dplyr::bind_cols(cement_unit) %>%
  dplyr::mutate(
    source = "IEA Cement 2021",
    scenario_geography = "global",
    sector = "Cement",
    indicator = "Production"
  ) %>%
  dplyr::filter(
    .data$year == 2021,
    .data$scenario_geography == .env$region_select
  )

# combine IEA data
weo2022_reference <- weo2022_region_select %>%
  dplyr::bind_rows(steel) %>%
  dplyr::bind_rows(cement)

### prepare benchmark: JRC geco 2022----
geco2022_region_select <- geco_2022 %>%
  dplyr::filter(
    .data$year == 2022,
    .data$scenario_geography %in% c("Global", "European Union"),
    .data$scenario == "Reference"
  ) %>%
  dplyr::summarise(
    production = sum(.data$value, na.rm = TRUE),
    .by = c("source", "scenario", "scenario_geography", "sector", "technology", "indicator", "units", "year")
  )

geco2022_reference <- geco2022_region_select %>%
  dplyr::filter(.data$sector %in% c("Power", "Automotive", "Oil&Gas", "Coal"))

## combine benchmark scenarios----
scenario_reference_adjusted <- geco2022_reference %>%
  dplyr::bind_rows(weo2022_reference)

## unit conversion----
# styler: off
unit_conversion_scenario_abcd <- tibble::tribble(
    ~source,                 ~sector,   ~unit_scenario,   ~unit_abcd,                   ~value_in_unit_abcd,
  "WEO2022",                  "coal",           "Mtce",     "t coal",                               1000000,
  # conversion of bcm to GJ, based on IEA WEO 2022, Annex C, p.486
  "WEO2022",           "oil and gas",            "bcm",         "GJ",                              36000000,
  # conversion of barrel to tonne and Mtoe to GJ, based on: https://iea.blob.core.windows.net/assets/49f4606d-5238-4b15-923f-0b3c0d1689af/Oil_documentation.pdf
  # and mtoe to GJ: https://www.iea.org/data-and-statistics/data-tools/unit-converter
  "WEO2022",           "oil and gas",           "mb/d",         "GJ", 1000000 * (1 / 7.37) * 365.25 * 41.87,
  "WEO2022",                 "power",             "GW",         "MW",                                  1000,
  "IEA Cement 2021",        "cement",             "Mt",   "t cement",                               1000000,
  "IEA Steel and Iron 2021", "steel",             "Mt",    "t steel",                               1000000,
  "WEO2022",            "automotive", "# (in million)", "# vehicles",                               1000000,
  # https://www.iea.org/data-and-statistics/data-tools/unit-converter
  "GECO2022",                 "coal",           "mtoe",     "t coal",                       1.429 * 1000000,
  # https://www.iea.org/data-and-statistics/data-tools/unit-converter
  "GECO2022",          "oil and gas",           "mtoe",         "GJ",                       41.87 * 1000000,
  "GECO2022",                "power",             "GW",         "MW",                                  1000,
  "GECO2022",           "automotive",          "k*veh", "# vehicles",                                  1000
)
# styler: on

scenario_reference_adjusted <- scenario_reference_adjusted %>%
  dplyr::mutate(
    sector = tolower(.data$sector),
    sector = dplyr::if_else(.data$sector == "oil&gas", "oil and gas", .data$sector),
    technology = tolower(.data$technology)
  ) %>%
  dplyr::inner_join(
    unit_conversion_scenario_abcd,
    by = c("source", "sector", "units" = "unit_scenario")
  )

scenario_reference_final <- scenario_reference_adjusted %>%
  dplyr::mutate(
    production_scenario = .data$production * .data$value_in_unit_abcd,
    units = unit_abcd
  ) %>%
  dplyr::select(-c("unit_abcd", "value_in_unit_abcd", "production", "indicator"))

## prepare production data for coverage analysis----
abcd_global_coverage <- abcd %>%
  dplyr::filter(
    plant_location %in% .env$countries_select,
    is_ultimate_owner,
    .data$year %in% c(2021, 2022)
  ) %>%
  dplyr::mutate(
    technology = dplyr::if_else(
      .data$sector %in% c("cement", "steel"),
      NA_character_,
      .data$technology
    )
  ) %>%
  dplyr::summarise(
    production = sum(.data$production, na.rm = TRUE),
    .by = c("sector", "technology", "year", "production_unit")
  )

## coverage by sector, technology and reference benchmark
production_data_coverage <- scenario_reference_final %>%
  dplyr::inner_join(
    abcd_global_coverage,
    by = c("sector", "technology", "year", "units" = "production_unit")
  ) %>%
  dplyr::mutate(share_coverage = round(.data$production / .data$production_scenario, 3))
