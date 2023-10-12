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
source("R/functions_prep_project.R")

# set up project----
if (file.exists(here::here(".env"))) {
  # paths
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))

  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- input_path_matched

  # project parameters
  scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
  start_year_select <- Sys.getenv("PARAM_START_YEAR")
  apply_sector_split <- as.logical(Sys.getenv("APPLY_SECTOR_SPLIT"))
  if (is.na(apply_sector_split)) {apply_sector_split <- FALSE}
  if (apply_sector_split) {sector_split_type_select <- Sys.getenv("SECTOR_SPLIT_TYPE")}

  # if a sector split is applied, write results into a directory that states the type
  if (apply_sector_split) {
    output_path <- file.path(output_path, sector_split_type_select)
  }

  dir.create(output_path, recursive = TRUE)

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

# optional: remove company-sector combinations where production in t5 = 0 when
# it was greater than 0 in t0.
if (remove_inactive_companies) {
  abcd_keep <- abcd %>%
    rm_inactive_companies(
      start_year = start_year,
      time_frame_select = time_frame_select
    )

  abcd_removed <- abcd %>%
    dplyr::anti_join(abcd_keep, by = c("company_id", "sector"))

  # write removed inactive companies to file for inspection
  abcd_removed %>%
    readr::write_csv(file.path(input_dir_abcd, "abcd_removed_inactive_companies.csv"))

  abcd <- abcd_keep
}

# filter to start year as we calculate coverage in start year
abcd <- abcd %>%
  dplyr::filter(.data$year == .env$start_year_select)


# coverage of production by companies in loan books compared to total production----

matched_prioritized <- readr::read_csv(
  file.path(input_path_matched, "matched_prio_all_groups.csv"),
  col_types = col_types_matched_prio_all_groups,
  col_select = dplyr::all_of(col_select_matched_prio_all_groups)
)

# optional: apply sector split----
if (apply_sector_split & sector_split_type_select %in% c("equal_weights", "worst_case")) {
  if (sector_split_type_select == "equal_weights") {
    companies_sector_split <- readr::read_csv(
      file.path(input_path_matched, "companies_sector_split.csv"),
      col_types = col_types_companies_sector_split,
      col_select = dplyr::all_of(col_select_companies_sector_split)
    )
  } else {
    companies_sector_split <- readr::read_csv(
      file.path(input_path_matched, "companies_sector_split_worst_case.csv"),
      col_types = col_types_companies_sector_split_worst_case,
      col_select = dplyr::all_of(col_select_companies_sector_split_worst_case)
    )
  }

  matched_prioritized <- matched_prioritized %>%
    apply_sector_split_to_loans(
      abcd = abcd,
      companies_sector_split = companies_sector_split,
      sector_split_type = sector_split_type_select,
      input_path_matched = input_path_matched
    )
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
  readr::write_csv(file.path(output_path, "summary_statistics_loanbook_coverage.csv"))

