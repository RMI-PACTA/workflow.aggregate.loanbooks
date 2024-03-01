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

# set up project----
if (file.exists(here::here(".env"))) {
  # paths
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_scenario_tms <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_TMS"))
  input_path_scenario_sda <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_SDA"))
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- Sys.getenv("DIR_OUTPUT")
  output_path_aggregated <- file.path(output_path, "aggregated")

  # project parameters
  scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
  scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
  region_select <- Sys.getenv("PARAM_REGION_SELECT")
  start_year <- as.numeric(Sys.getenv("PARAM_START_YEAR"))
  time_frame_select <- as.integer(Sys.getenv("PARAM_TIME_FRAME"))
  benchmark_regions <- unlist(base::strsplit(Sys.getenv("PARAM_BENCHMARK_REGIONS"), ","))
  apply_sector_split <- as.logical(Sys.getenv("APPLY_SECTOR_SPLIT"))
  if (is.na(apply_sector_split)) {apply_sector_split <- FALSE}
  if (apply_sector_split) {sector_split_type_select <- Sys.getenv("SECTOR_SPLIT_TYPE")}
  remove_inactive_companies <- as.logical(Sys.getenv("REMOVE_INACTIVE_COMPANIES"))
  if (is.na(remove_inactive_companies)) {remove_inactive_companies <- FALSE}

  # if a sector split is applied, write results into a directory that states the type
  if (apply_sector_split) {
    output_path_aggregated <- file.path(output_path, sector_split_type_select, "aggregated")
  }

  dir.create(output_path_aggregated, recursive = TRUE)

} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# TODO: add check if all files exist, resort to test files if not


# load input data----
region_isos_complete <- r2dii.data::region_isos

region_isos_select <- region_isos_complete %>%
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

# write final version of abcd to file for use in plot_aggregate_loanbooks.R
abcd %>%
  readr::write_csv(file.path(input_dir_abcd, "abcd_final_for_plots.csv"))

# add loan book with corporate economy benchmark----
# benchmark_region can be selected based on r2dii.data::region_isos
matched_benchmark <- NULL

# matching the benchmark loan book separately, because it is not needed for the
# generation of standard PACTA output
for (i in benchmark_regions) {
  matched_benchmark_i <- abcd %>%
    create_benchmark_loanbook(
      scenario_source = scenario_source_input,
      start_year = start_year,
      region_isos = region_isos_complete,
      benchmark_region = i
    )

  matched_benchmark <- matched_benchmark %>%
    dplyr::bind_rows(matched_benchmark_i)
}

matched_benchmark %>%
  readr::write_csv(file.path(input_path_matched, "matched_benchmark.csv"))

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

# write final version of matched_prioritized to file for use in plot_aggregate_loanbooks.R
matched_prioritized %>%
  readr::write_csv(file.path(input_path_matched, "matched_prioritized_final_for_plots.csv"))

# aggregate P4B alignment----

## retrieve set of unique groups to loop over----
unique_loanbooks_matched <- unique(matched_prioritized$group_id)
unique_groups_tms <- unique_loanbooks_matched
unique_groups_sda <- unique_loanbooks_matched

## set specifications----

# for the calculation of the aggregate company alignment metric, we do not force companies
# to enter a new market to build out hydro power or nuclear power, as this may
# not be feasible for political and/or geographic reasons.
# in the power sector, only renewables continues to follow the SMSP logic
increasing_or_decreasing_aggregate_alignment <- r2dii.data::increasing_or_decreasing %>%
  dplyr::mutate(
    increasing_or_decreasing = dplyr::if_else(
      .data$technology %in% c("hydrocap", "nuclearcap"),
      "decreasing",
      .data$increasing_or_decreasing
    )
  )

# define if technologies should be treated as build out or phase down in the
# aggregation
technology_direction <- scenario_input_tms %>%
  dplyr::filter(.data$year %in% c(2022, 2027)) %>%
  dplyr::distinct(.data$scenario_source, .data$scenario, .data$sector, .data$technology, .data$region) %>%
  dplyr::inner_join(r2dii.data::increasing_or_decreasing, by = c("sector", "technology")) %>%
  dplyr::mutate(
    directional_dummy = dplyr::if_else(.data$increasing_or_decreasing == "increasing", 1, -1)
  ) %>%
  dplyr::select(-"increasing_or_decreasing")

# add benchmark loan book for aggregation
matched_total <- matched_prioritized %>%
  dplyr::bind_rows(matched_benchmark)

## prepare TMS company level P4B results for aggregation----
tms_result_for_aggregation <- NULL

for (i in unique_groups_tms) {
  tryCatch(
    {
      tms_result_for_aggregation_i <- target_market_share(
        data = matched_prioritized %>%
          dplyr::filter(.data$group_id == i) %>%
          dplyr::select(-"group_id"),
        abcd = abcd,
        scenario = scenario_input_tms,
        region_isos = region_isos_select,
        by_company = TRUE,
        weight_production = FALSE,
        increasing_or_decreasing = increasing_or_decreasing_aggregate_alignment
      )

      tms_result_for_aggregation_i <- tms_result_for_aggregation_i %>%
        dplyr::mutate(group_id = .env$i)

      tms_result_for_aggregation <- tms_result_for_aggregation %>%
        dplyr::bind_rows(tms_result_for_aggregation_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - group: {i} Problem in preparing data for aggregation. Skipping! \n")
      write(log_text, file = file.path(output_path_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

tms_result_for_aggregation_benchmark <- NULL

unique_benchmarks_tms <- unique(matched_benchmark$group_id)

for (i in unique_benchmarks_tms) {
  tryCatch(
    {
      benchmark_region_i <- gsub("benchmark_corporate_economy_", "", i)

      allowed_countries_i <- region_isos_complete %>%
        dplyr::filter(
          .data$source == .env$scenario_source_input,
          .data$region == .env$benchmark_region_i,
        ) %>%
        dplyr::pull(.data$isos) %>%
        toupper()

      abcd_benchmark_region_i <- abcd %>%
        dplyr::filter(.data$plant_location %in% .env$allowed_countries_i)

      tms_result_for_aggregation_benchmark_i <- target_market_share(
        data = matched_benchmark %>%
          dplyr::filter(.data$group_id == i) %>%
          dplyr::select(-"group_id"),
        abcd = abcd_benchmark_region_i,
        scenario = scenario_input_tms,
        region_isos = region_isos_select,
        by_company = TRUE,
        weight_production = FALSE,
        increasing_or_decreasing = increasing_or_decreasing_aggregate_alignment
      )

      tms_result_for_aggregation_benchmark_i <- tms_result_for_aggregation_benchmark_i %>%
        dplyr::mutate(group_id = .env$i)

      tms_result_for_aggregation_benchmark <- tms_result_for_aggregation_benchmark %>%
        dplyr::bind_rows(tms_result_for_aggregation_benchmark_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - group: {i} Problem in preparing data for benchmark aggregation. Skipping! \n")
      write(log_text, file = file.path(output_path_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

# bind the TMS results from the loan book and benchmark PACTA runs for further
# aggregation
tms_result_for_aggregation <- tms_result_for_aggregation %>%
  dplyr::bind_rows(tms_result_for_aggregation_benchmark)

## aggregate TMS P4B results to company level alignment metric----
# calculate aggregation for the loan book

company_technology_deviation_tms <- tms_result_for_aggregation %>%
  calculate_company_tech_deviation(
    technology_direction = technology_direction,
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    # bridge_tech = "gascap",
    time_frame = time_frame_select
  )

company_technology_deviation_tms %>%
  readr::write_csv(file.path(output_path_aggregated, "company_technology_deviation_tms.csv"))

company_aggregated_alignment_net_tms <- company_technology_deviation_tms %>%
  calculate_company_aggregate_alignment_tms(
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    level = "net"
  )

company_aggregated_alignment_net_tms %>%
  readr::write_csv(file.path(output_path_aggregated, "company_aggregated_alignment_net_tms.csv"))

company_aggregated_alignment_bo_po_tms <- company_technology_deviation_tms %>%
  calculate_company_aggregate_alignment_tms(
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    level = "bo_po"
  )

company_aggregated_alignment_bo_po_tms %>%
  readr::write_csv(file.path(output_path_aggregated, "company_aggregated_alignment_bo_po_tms.csv"))

## prepare SDA company level P4B results for aggregation----
sda_result_for_aggregation <- NULL

for (i in unique_groups_sda) {
  tryCatch(
    {
      sda_result_for_aggregation_i <- target_sda(
        data = matched_prioritized %>%
          dplyr::filter(.data$group_id == i) %>%
          dplyr::select(-"group_id"),
        abcd = abcd,
        co2_intensity_scenario = scenario_input_sda,
        by_company = TRUE,
        region_isos = region_isos_select
      )

      sda_result_for_aggregation_i <- sda_result_for_aggregation_i %>%
        dplyr::mutate(group_id = .env$i)

      sda_result_for_aggregation <- sda_result_for_aggregation %>%
        dplyr::bind_rows(sda_result_for_aggregation_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - group: {i} Problem in preparing data for aggregation. Skipping! \n")
      write(log_text, file = file.path(output_path_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

sda_result_for_aggregation_benchmark <- NULL

unique_benchmarks_sda <- unique(matched_benchmark$group_id)

for (i in unique_benchmarks_sda) {
  tryCatch(
    {
      benchmark_region_i <- gsub("benchmark_corporate_economy_", "", i)

      allowed_countries_i <- region_isos_complete %>%
        dplyr::filter(
          .data$source == .env$scenario_source_input,
          .data$region == .env$benchmark_region_i,
        ) %>%
        dplyr::pull(.data$isos) %>%
        toupper()

      abcd_benchmark_region_i <- abcd %>%
        dplyr::filter(.data$plant_location %in% .env$allowed_countries_i)

      sda_result_for_aggregation_benchmark_i <- target_sda(
        data = matched_benchmark %>%
          dplyr::filter(.data$group_id == i) %>%
          dplyr::select(-"group_id"),
        abcd = abcd_benchmark_region_i,
        co2_intensity_scenario = scenario_input_sda,
        by_company = TRUE,
        region_isos = region_isos_select
      )

      sda_result_for_aggregation_benchmark_i <- sda_result_for_aggregation_benchmark_i %>%
        dplyr::mutate(group_id = .env$i)

      sda_result_for_aggregation_benchmark <- sda_result_for_aggregation_benchmark %>%
        dplyr::bind_rows(sda_result_for_aggregation_benchmark_i)

    },
    error = function(e) {
      log_text <- glue::glue("{Sys.time()} - group: {i} Problem in preparing data for benchmark aggregation. Skipping! \n")
      write(log_text, file = file.path(output_path_aggregated, "error_messages.txt"), append = TRUE)
    }
  )
}

# bind the SDA results from the loan book and benchmark PACTA runs for further
# aggregation
sda_result_for_aggregation <- sda_result_for_aggregation %>%
  dplyr::bind_rows(sda_result_for_aggregation_benchmark) %>%
  dplyr::filter(.data$year >= .env$start_year)

## aggregate SDA P4B results to company level alignment metric----
# calculate aggregation for the loan book
# temporary fix for the scenario name issue in geco_2021, relates to https://github.com/RMI-PACTA/r2dii.analysis/issues/425
if (scenario_source_input == "geco_2021" & scenario_select == "1.5c") {scenario_select_sda <- "1.5c-unif"} else {scenario_select_sda <- scenario_select}

company_aggregated_alignment_net_sda <- sda_result_for_aggregation %>%
  calculate_company_aggregate_alignment_sda(
    scenario_source = scenario_source_input,
    scenario = scenario_select_sda,
    time_frame = time_frame_select
  )

company_aggregated_alignment_net_sda %>%
  readr::write_csv(file.path(output_path_aggregated, "company_aggregated_alignment_net_sda.csv"))


## calculate sector and loan book level aggregate alignment based on company exposures in loan book----

# the company level aggregate alignment metrics are then joined with the matched
# loan book to derive some high level summary statistics on the loan book level
company_aggregated_alignment_net <- company_aggregated_alignment_net_tms %>%
  dplyr::bind_rows(company_aggregated_alignment_net_sda)

# show exposures (n companies and loan size) by alignment with given scenario

# net
loanbook_exposure_aggregated_alignment_net <- company_aggregated_alignment_net %>%
  aggregate_alignment_loanbook_exposure(
    matched = matched_total,
    level = "net"
  )

loanbook_exposure_aggregated_alignment_net %>%
  readr::write_csv(file.path(output_path_aggregated, "loanbook_exposure_aggregated_alignment_net.csv"))

# buildout / phaseout
loanbook_exposure_aggregated_alignment_bo_po <- company_aggregated_alignment_bo_po_tms %>%
  aggregate_alignment_loanbook_exposure(
    matched = matched_total,
    level = "bo_po"
  )

loanbook_exposure_aggregated_alignment_bo_po %>%
  readr::write_csv(file.path(output_path_aggregated, "loanbook_exposure_aggregated_alignment_bo_po.csv"))
