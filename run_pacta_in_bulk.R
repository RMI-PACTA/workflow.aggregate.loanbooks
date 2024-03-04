# load packages----
library(dotenv)
library(dplyr)
library(pacta.aggregate.loanbook.plots)
library(r2dii.analysis)
library(r2dii.data)
library(r2dii.match)
library(r2dii.plot)
library(readxl)
library(rlang)
library(tidyr)
library(vroom)

dotenv::load_dot_env()
source("expected_columns.R")
source("R/functions_prep_project.R")
source("R/plots.R")

# set up project----
if (file.exists(here::here(".env"))) {
  # paths
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))
  input_path_scenario_tms <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_TMS"))
  input_path_scenario_sda <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_SDA"))
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- Sys.getenv("DIR_OUTPUT")
  output_path_standard <- file.path(output_path, "standard")

  # project parameters
  scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
  scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
  region_select <- Sys.getenv("PARAM_REGION_SELECT")
  start_year <- as.numeric(Sys.getenv("PARAM_START_YEAR"))
  time_frame_select <- as.integer(Sys.getenv("PARAM_TIME_FRAME"))
  apply_sector_split <- as.logical(Sys.getenv("APPLY_SECTOR_SPLIT"))
  if (is.na(apply_sector_split)) {apply_sector_split <- FALSE}
  if (apply_sector_split) {sector_split_type_select <- Sys.getenv("SECTOR_SPLIT_TYPE")}
  remove_inactive_companies <- as.logical(Sys.getenv("REMOVE_INACTIVE_COMPANIES"))
  if (is.na(remove_inactive_companies)) {remove_inactive_companies <- FALSE}

  # if a sector split is applied, write results into a directory that states the type
  if (apply_sector_split) {
    output_path_standard <- file.path(output_path, sector_split_type_select, "standard")
  }

  dir.create(output_path_standard, recursive = TRUE)

} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}


# TODO: add check if all files exist, resort to test files if not

# load input data----
region_isos_select <- r2dii.data::region_isos %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input,
    .data$region %in% .env$region_select
  )

scenario_input_tms <- readr::read_csv(
  input_path_scenario_tms,
  col_types = col_types_scenario_tms,
  col_select = dplyr::all_of(col_select_scenario_tms)
)
scenario_input_sda <- readr::read_csv(
  input_path_scenario_sda,
  col_types = col_types_scenario_sda,
  col_select = dplyr::all_of(col_select_scenario_sda)
)

# abcd <- abcd_test_data
abcd <- readr::read_csv(
  file.path(input_path_abcd),
  col_types = col_types_abcd,
  col_select = dplyr::all_of(col_select_abcd)
)

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

# read matched and prioritized loan book----
list_matched_prio <- list.files(input_path_matched)[grepl("matched_prio_", list.files(input_path_matched))]

matched_prioritized <- NULL

# combine all matched loan books into one object to loop over
for (i in list_matched_prio) {
  matched_prioritized_i <- readr::read_csv(
    file.path(input_path_matched, i),
    col_types = col_types_matched_prio_all_groups,
    col_select = dplyr::all_of(col_select_matched_prio_all_groups)
  )

  matched_prioritized <- matched_prioritized %>%
    dplyr::bind_rows(matched_prioritized_i)
}

# optional: apply sector split----
  # NOTE: to generate the worst case sector split, you need to run the script `run_aggregate_loanbooks.R`
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

# meta loan book----
# aggregate all individual loan books into one meta loan book and add that to
# the full list of loan books
matched_prioritized_meta <- matched_prioritized %>%
  dplyr::mutate(
    id_loan = paste0(.data$id_loan, "_", .data$group_id),
    group_id = "meta_loanbook"
  )

matched_prioritized <- matched_prioritized %>%
  dplyr::bind_rows(matched_prioritized_meta)

# generate all P4B outputs----
unique_loanbooks_matched <- unique(matched_prioritized$group_id)

## generate SDA outputs----
results_sda_total <- NULL

# generate SDA results for each individual loan book, including the meta loan book
for (i in unique_loanbooks_matched) {
  matched_i <- matched_prioritized %>%
    dplyr::filter(.data$group_id == i) %>%
    dplyr::select(-"group_id")

  results_sda_i <- matched_i %>%
    target_sda(
      abcd = abcd,
      co2_intensity_scenario = scenario_input_sda,
      region_isos = region_isos_select
    ) %>%
    dplyr::mutate(group_id = .env$i)

  results_sda_total <- results_sda_total %>%
    dplyr::bind_rows(results_sda_i)
}

# write SDA results to csv
results_sda_total %>%
  readr::write_csv(
    file.path(output_path_standard, "sda_results_all_groups.csv"),
    na = ""
  )


## generate TMS outputs----

results_tms_total <- NULL

# generate TMS results for each individual loan book, including the meta loan book
for (i in unique_loanbooks_matched) {
  matched_i <- matched_prioritized %>%
    dplyr::filter(.data$group_id == i) %>%
    dplyr::select(-"group_id")

  results_tms_i <- matched_i %>%
    target_market_share(
      abcd = abcd,
      scenario = scenario_input_tms,
      region_isos = region_isos_select
    ) %>%
    dplyr::mutate(group_id = .env$i)

  results_tms_total <- results_tms_total %>%
    dplyr::bind_rows(results_tms_i)
}

# write TMS results to csv
results_tms_total %>%
  readr::write_csv(
    file.path(output_path_standard, "tms_results_all_groups.csv"),
    na = ""
  )

# generate P4B plots----

## retrieve set of unique groups to loop over----
unique_groups_tms <- unique(results_tms_total$group_id)
unique_groups_sda <- unique(results_sda_total$group_id)

## run automatic result generation ----------

### automotive----
sector_select <- "automotive"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}
### coal----
sector_select <- "coal"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}
### oil and gas----
sector_select <- "oil and gas"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}
### power----
sector_select <- "power"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}

### aviation----
sector_select <- "aviation"
for (sda_i in unique_groups_sda) {
  available_rows <- results_sda_total %>%
    dplyr::filter(
      group_id == sda_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$emission_factor_metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_sda_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "sda",
      group_id = sda_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}
### cement----
sector_select <- "cement"
for (sda_i in unique_groups_sda) {
  available_rows <- results_sda_total %>%
    dplyr::filter(
      group_id == sda_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$emission_factor_metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_sda_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "sda",
      group_id = sda_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}
### steel----
sector_select <- "steel"
for (sda_i in unique_groups_sda) {
  available_rows <- results_sda_total %>%
    dplyr::filter(
      group_id == sda_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$emission_factor_metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_sda_total,
      matched_prioritized = matched_prioritized,
      output_directory = output_path_standard,
      target_type = "sda",
      group_id = sda_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select,
      start_year = start_year,
      time_horizon = time_frame_select
    )
  } else {
    next()
  }
}
