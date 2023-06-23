# load packages----
library(dotenv)
library(dplyr)
library(r2dii.data)
library(readr)
library(rlang)
library(tidyr)
library(vroom)

dotenv::load_dot_env()

# set up project paths----
if (file.exists(here::here(".env"))) {
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- Sys.getenv("DIR_OUTPUT")
} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}


# description production data ----
# read abcd
abcd <- readr::read_csv(file.path(input_path_abcd))
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0


# coverage of production by companies in loan books compared to total production----
# matched companies of all production, by sector
# TODO: this needs to be broken down by region/country

matched_prioritized <- readr::read_csv(
  file.path(input_path_matched, "matched_prio_all_groups.csv")
)


# create summary of loan book coverage----

matched_companies <- matched_prioritized %>%
  distinct(name_abcd, sector_abcd, loan_size_outstanding, loan_size_outstanding_currency, score)

# TODO: use current iso bridge
country_mapping <- r2dii.data::region_isos %>%
  dplyr::filter(.data$source == "weo_2021")

# available_regions
unique(country_mapping$region)

# region_select <- "european union"
region_select <- "global"
countries_select <- country_mapping %>%
  dplyr::filter(.data$region == .env$region_select) %>%
  dplyr::pull(.data$isos) %>%
  toupper()


# production_coverage_companies <- abcd %>%
production_coverage_companies <- abcd %>%
  dplyr::filter(
    .data$year == 2022,
    .data$plant_location %in% .env$countries_select
  ) %>%
  dplyr::summarise(
    emission_factor = stats::weighted.mean(.data$emission_factor, w = .data$production, na.rm = TRUE),
    production = sum(.data$production, na.rm = TRUE),
    .by = c(
      "company_id", "name_company", "lei", "is_ultimate_owner", "sector",
      "technology", "year", "production_unit", "emission_factor_unit", "ald_timestamp"
    )
  ) %>%
  dplyr::left_join(
    matched_companies,
    by = c("name_company" = "name_abcd", "sector" = "sector_abcd")
  )

production_coverage_summary <- production_coverage_companies %>%
  dplyr::mutate(
    financed_production = dplyr::if_else(.data$score == 1, .data$production, 0),
    matched_company = dplyr::if_else(.data$score == 1, .data$name_company, NA_character_)
  ) %>%
  dplyr::mutate(
    matched_rows_company_sector = sum(.data$score, na.rm = TRUE),
    .by =c("name_company", "sector")
  ) %>%
  dplyr::summarise(
    total_exposure = sum(loan_size_outstanding / matched_rows_company_sector, na.rm = TRUE),
    matched_number_companies = dplyr::n_distinct(.data$matched_company, na.rm = TRUE),
    matched_number_companies = dplyr::n_distinct(.data$matched_company, na.rm = TRUE),
    total_number_companies = dplyr::n_distinct(.data$name_company, na.rm = TRUE),
    total_production = sum(.data$production, na.rm = TRUE),
    financed_production = sum(.data$financed_production, na.rm = TRUE),
    .by = c("sector")
  ) %>%
  dplyr::mutate(
    share_matched_companies = .data$matched_number_companies / .data$total_number_companies,
    share_financed_production = .data$financed_production / .data$total_production
  )

