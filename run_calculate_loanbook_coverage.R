# load packages----
library(dotenv)
library(dplyr)
library(r2dii.data)
library(readr)
library(rlang)
library(tidyr)
library(vroom)

dotenv::load_dot_env()
source("expected_columns.R")

# set up project paths and params----
if (file.exists(here::here(".env"))) {
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))

  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
  start_year_select <- Sys.getenv("PARAM_START_YEAR")

  output_path <- Sys.getenv("DIR_OUTPUT")
} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}


# description production data ----
# read abcd
abcd <- readr::read_csv(
  file.path(input_path_abcd),
  col_types = col_types_abcd,
  col_select = dplyr::all_of(col_select_abcd)
)
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

abcd <- abcd %>%
  dplyr::filter(.data$year == .env$start_year_select)


# coverage of production by companies in loan books compared to total production----

matched_prioritized <- readr::read_csv(
  file.path(input_path_matched, "matched_prio_all_groups.csv"),
  col_types = col_types_matched_prio_all_groups,
  col_select = dplyr::all_of(col_select_matched_prio_all_groups)
)

# optional: apply sector split----
if (apply_sector_split) {
  companies_sector_split <- readr::read_csv(
    file.path(input_path_matched, "companies_sector_split.csv"),
    col_types = cols_only(
      company_id = "i",
      name_company = "c",
      sector = "c",
      sector_split = "n"
    ),
    col_select = dplyr::all_of(c("name_company", "sector", "sector_split"))
  )

  matched_prioritized <- matched_prioritized %>%
    dplyr::left_join(
      companies_sector_split,
      by = c("name_abcd" = "name_company", "sector_abcd" = "sector")
    ) %>%
    dplyr::mutate(
      # renaming the loan_id is not conditional to avoid any chance of accidentally
      # renaming a split loan to a loan_id that already exists elsewhere
      id_loan = paste(.data$id_loan, .data$sector_abcd, sep = "_"),
      loan_size_outstanding = dplyr::if_else(
        is.na(.data$sector_split),
        .data$loan_size_outstanding,
        .data$loan_size_outstanding * .data$sector_split
      ),
      loan_size_credit_limit = dplyr::if_else(
        is.na(.data$sector_split),
        .data$loan_size_credit_limit,
        .data$loan_size_credit_limit * .data$sector_split
      )
    ) %>%
    dplyr::select(-"sector_split")
}

# create summary of loan book coverage----

matched_companies <- matched_prioritized %>%
  distinct(name_abcd, sector_abcd, loan_size_outstanding, loan_size_outstanding_currency, score)

regions_geco_2022 <- readr::read_csv(
  input_path_regions_geco_2022,
  col_types = col_types_region_isos,
  col_select = dplyr::all_of(col_select_region_isos)
)
regions_weo_2022 <- readr::read_csv(
  input_path_regions_weo_2022,
  col_types = col_types_region_isos,
  col_select = dplyr::all_of(col_select_region_isos)
)
# get required countries for region_select----
region_isos_complete <- r2dii.data::region_isos %>%
  rbind(regions_geco_2022) %>%
  rbind(regions_weo_2022)

region_isos_select <- region_isos_complete %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input
  )

# available_regions
available_regions <- unique(region_isos_select$region)

# calculate summary stats for each available region----
production_coverage_summary <- NULL

for (region_i in available_regions) {
  countries_select_i <- region_isos_select %>%
    dplyr::filter(.data$region == .env$region_i) %>%
    dplyr::pull(.data$isos) %>%
    toupper()

  # summarise abcd by relevant region
  production_coverage_summary_i <- abcd %>%
    dplyr::filter(.data$plant_location %in% .env$countries_select_i) %>%
    dplyr::summarise(
      emission_factor = stats::weighted.mean(.data$emission_factor, w = .data$production, na.rm = TRUE),
      production = sum(.data$production, na.rm = TRUE),
      .by = c(
        "company_id", "name_company", "lei", "is_ultimate_owner", "sector",
        "technology", "year", "production_unit", "emission_factor_unit", "ald_timestamp"
      )
    )

  # add information on matched companies across analysed loan books
  production_coverage_summary_i <- production_coverage_summary_i %>%
    dplyr::left_join(
      matched_companies,
      by = c("name_company" = "name_abcd", "sector" = "sector_abcd")
    )

  # calculate summary statistics
  production_coverage_summary_i <- production_coverage_summary_i %>%
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
      n_companies_matched = dplyr::n_distinct(.data$matched_company, na.rm = TRUE),
      n_companies_total = dplyr::n_distinct(.data$name_company, na.rm = TRUE),
      production_financed = sum(.data$financed_production, na.rm = TRUE),
      production_total = sum(.data$production, na.rm = TRUE),
      .by = c("sector")
    ) %>%
    dplyr::mutate(
      share_companies_matched = .data$n_companies_matched / .data$n_companies_total,
      share_production_financed = .data$production_financed / .data$production_total,
      region = .env$region_i
    )

  production_coverage_summary <- production_coverage_summary %>%
    bind_rows(production_coverage_summary_i)

}

# format
production_coverage_summary <- production_coverage_summary %>%
  select(
    c(
      "region",
      "sector",
      "total_exposure",
      "n_companies_matched",
      "n_companies_total",
      "share_companies_matched",
      "production_financed",
      "production_total",
      "share_production_financed"
    )
  )

# save to matched directory
production_coverage_summary %>%
  readr::write_csv(file.path(input_path_matched, "summary_statistics_loanbook_coverage.csv"))

