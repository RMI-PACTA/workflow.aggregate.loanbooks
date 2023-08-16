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

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))
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

# TODO: remove the temp section once r2dii.data is updated
############# TEMP #############
# r2dii.data is not updated yet, so we manually update the region_isos data to
# cover the 2022 scenarios
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

region_isos_complete <- r2dii.data::region_isos %>%
  rbind(regions_geco_2022) %>%
  rbind(regions_weo_2022)
################################
# region_isos_complete <- r2dii.data::region_isos

region_isos_select <- region_isos_complete %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input,
    .data$region %in% .env$region_select
  )

# load input data----
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
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

# optional: remove company-sector combinations where production in t5 = 0 when
# it was greater than 0 in t0.
if (remove_inactive_companies) {
  abcd <- abcd %>%
    rm_inactive_companies(
      start_year = start_year,
      time_frame_select = time_frame_select
    )
}

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
  readr::write_csv(file.path(input_path_matched, "matched_prio_benchmark.csv"))

# read matched and prioritized loan book----
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
      sector_split_type = sector_split_type_select
    )
}

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

# generate plots for system-wide analysis----

# Plot sankey plot of financial flows scenario alignment - examples

if (!is.null(company_aggregated_alignment_net_tms)) {
  data_sankey_sector_tms <- prep_sankey(
    company_aggregated_alignment_net_tms,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "sector"
  )
} else {
  data_sankey_sector_tms <- NULL
}

if (!is.null(company_aggregated_alignment_net_sda)) {
  data_sankey_sector_sda <- prep_sankey(
    company_aggregated_alignment_net_sda,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "sector"
  )
} else {
  data_sankey_sector_sda <- NULL
}

data_sankey_sector <- rbind(data_sankey_sector_tms, data_sankey_sector_sda)

if (!is.null(data_sankey_sector)) {
  data_sankey_sector %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_sankey_sector.csv"
      )
    )

  plot_sankey(
    data_sankey_sector,
    save_png_to = output_path_aggregated,
    png_name = "plot_sankey_sector.png",
    nodes_order_from_data = TRUE
  )
}

if (!is.null(company_aggregated_alignment_net_tms)) {
  data_sankey_company_sector_tms <- prep_sankey(
    company_aggregated_alignment_net_tms,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "name_abcd",
    middle_node2 = "sector"
  )
} else {
  data_sankey_company_sector_tms <- NULL
}

if (!is.null(company_aggregated_alignment_net_sda)) {
  data_sankey_company_sector_sda <- prep_sankey(
    company_aggregated_alignment_net_sda,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "name_abcd",
    middle_node2 = "sector"
  )
} else {
  data_sankey_company_sector_sda <- NULL
}

data_sankey_company_sector <- rbind(
  data_sankey_company_sector_tms,
  data_sankey_company_sector_sda
)

if (!is.null(data_sankey_company_sector)) {
  data_sankey_company_sector %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_sankey_company_sector.csv"
      )
    )

  plot_sankey(
    data_sankey_company_sector,
    save_png_to = output_path_aggregated,
    png_name = "plot_sankey_company_sector.png"
  )
}

### scatter plot for group level comparison----
year_scatter <- 2027
region_scatter <- region_select
data_level_group <- "bank"
# automotive
sector_scatter <- "automotive"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_automotive_group <- prep_scatter(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_automotive_group %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_automotive_group.csv"
      )
    )

  plot_scatter(
    data_scatter_automotive_group,
    data_level = data_level_group,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select
  )
  ggplot2::ggsave(
    filename = "plot_scatter_automotive_group.png",
    path = output_path_aggregated,
    width = 8,
    height = 5
  )
}

# power
sector_scatter <- "power"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_power_group <- prep_scatter(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_power_group %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_power_group.csv"
      )
    )

  plot_scatter(
    data_scatter_power_group,
    data_level = data_level_group,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select
  )

  ggplot2::ggsave(
    filename = "plot_scatter_power_group.png",
    path = output_path_aggregated,
    width = 8,
    height = 5
  )
}

### animated scatter plot for group level comparison----
region_scatter <- region_select
data_level_group <- "bank"
# automotive
sector_scatter <- "automotive"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_automotive_group_a <- prep_scatter_animated(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_automotive_group_a %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_automotive_group_animated.csv"
      )
    )

  plot_scatter_automotive_group_a <- plot_scatter_animated(
    data_scatter_automotive_group_a,
    sector = sector_scatter,
    data_level = data_level_group,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    alignment_limit = 1
  )

  htmlwidgets::saveWidget(
    plot_scatter_automotive_group_a,
    file = file.path(output_path_aggregated, "plot_scatter_automotive_group_animated.html")
  )
}

# power
sector_scatter <- "power"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_power_group_a <- prep_scatter_animated(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_power_group_a %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_power_group_animated.csv"
      )
    )

  plot_scatter_power_group_a <- plot_scatter_animated(
    data_scatter_power_group_a,
    sector = sector_scatter,
    data_level = data_level_group,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    alignment_limit = 1
  )

  htmlwidgets::saveWidget(
    plot_scatter_power_group_a,
    file = file.path(output_path_aggregated, "plot_scatter_power_group_animated.html")
  )
}

# group level plots ----
### timeline plot: evolution of portfolio-weighted alignment over time----

region_timeline <- region_select
# build-out / phase-out for automotive
sector_timeline <- "automotive"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_bo_po %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  dir.create(file.path(output_path_aggregated, i), recursive = TRUE, showWarnings = FALSE)
}

for (i in unique_loanbook_group_id) {
  data_timeline_automotive <- prep_timeline(
    loanbook_exposure_aggregated_alignment_bo_po,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if(nrow(data_timeline_automotive) > 0) {
    data_timeline_automotive %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_bopo_automotive.csv"
        )
      )

    plot_timeline(
      data_timeline_automotive,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_bopo_automotive.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

# build-out / phase-out for power
sector_timeline <- "power"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_bo_po %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_power <- prep_timeline(
    loanbook_exposure_aggregated_alignment_bo_po,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_power) > 0) {
    data_timeline_power %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_bopo_power.csv"
        )
      )

    plot_timeline(
      data_timeline_power,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_bopo_power.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for automotive
sector_timeline <- "automotive"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_automotive <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_automotive) > 0) {
    data_timeline_automotive %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_automotive.csv"
        )
      )

    plot_timeline(
      data_timeline_automotive,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_automotive.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for coal
sector_timeline <- "coal"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_coal <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_coal > 0)) {
    data_timeline_coal %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_coal.csv"
        )
      )

    plot_timeline(
      data_timeline_coal,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_coal.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for oil & gas
sector_timeline <- "oil and gas"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_oil_and_gas <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_oil_and_gas) > 0) {
    data_timeline_oil_and_gas %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_oil_and_gas.csv"
        )
      )

    plot_timeline(
      data_timeline_oil_and_gas,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_oil_and_gas.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for power
sector_timeline <- "power"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_power <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_power) > 0) {
    data_timeline_power %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_power.csv"
        )
      )

    plot_timeline(
      data_timeline_power,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_power.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for aviation
sector_timeline <- "aviation"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_aviation <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_aviation) > 0) {
    data_timeline_aviation %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_aviation.csv"
        )
      )

    plot_timeline(
      data_timeline_aviation,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_aviation.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for cement
sector_timeline <- "cement"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_cement <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_cement) > 0) {
    data_timeline_cement %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_cement.csv"
        )
      )

    plot_timeline(
      data_timeline_cement,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_cement.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for steel
sector_timeline <- "steel"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_steel <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_steel) > 0) {
    data_timeline_steel %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_steel.csv"
        )
      )

    plot_timeline(
      data_timeline_steel,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_steel.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

### scatter plot for company level comparison----

# all excluding outliers
# for all companies per group, not all companies across groups

# company level, excluding outliers
year_scatter <- 2027
region_scatter <- region_select
data_level_company <- "company"

# automotive
sector_scatter <- "automotive"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_automotive_company_i <- prep_scatter(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    group_ids_to_plot = i,
    data_level = data_level_company
  )

  if (nrow(data_scatter_automotive_company_i) > 0) {
    data_scatter_automotive_company_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_automotive_company.csv"
        )
      )

    plot_scatter(
      data_scatter_automotive_company_i,
      data_level = data_level_company,
      year = year_scatter,
      sector = sector_scatter,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      cap_outliers = 2,
      floor_outliers = -2
    )

    ggplot2::ggsave(
      filename = "plot_scatter_automotive_company.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

# power
sector_scatter <- "power"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_power_company_i <- prep_scatter(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    group_ids_to_plot = i,
    data_level = data_level_company
  )

  if (nrow(data_scatter_power_company_i) > 0) {
    data_scatter_power_company_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_power_company.csv"
        )
      )

    plot_scatter(
      data_scatter_power_company_i,
      data_level = data_level_company,
      year = year_scatter,
      sector = sector_scatter,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      cap_outliers = 2,
      floor_outliers = -2
    )

    ggplot2::ggsave(
      filename = "plot_scatter_power_company.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

### animated scatter plot for company level comparison----

# for all companies per group, not all companies across groups

# automotive
sector_scatter <- "automotive"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_automotive_company_animated_i <- prep_scatter_animated(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_company,
    group_ids_to_plot = i
  )

  if (nrow(data_scatter_automotive_company_animated_i) > 0) {
    data_scatter_automotive_company_animated_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_automotive_company_animated.csv"
        )
      )

    plot_scatter_animated_i <- plot_scatter_animated(
      data_scatter_automotive_company_animated_i,
      sector = sector_scatter,
      data_level = data_level_company,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      floor_outliers = -1.5,
      cap_outliers = 1.5
    )

    htmlwidgets::saveWidget(
      plot_scatter_animated_i,
      file = file.path(output_path_aggregated, i, "plot_scatter_automotive_company_animated.html")
    )
  } else {
    next()
  }
}

# power
sector_scatter <- "power"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_power_company_animated_i <- prep_scatter_animated(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_company,
    group_ids_to_plot = i
  )

  if (nrow(data_scatter_power_company_animated_i) > 0) {
    data_scatter_power_company_animated_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_power_company_animated.csv"
        )
      )

    plot_scatter_animated_i <- plot_scatter_animated(
      data_scatter_power_company_animated_i,
      sector = sector_scatter,
      data_level = data_level_company,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      floor_outliers = -1.5,
      cap_outliers = 1.5
    )

    htmlwidgets::saveWidget(
      plot_scatter_animated_i,
      file = file.path(output_path_aggregated, i, "plot_scatter_power_company_animated.html")
    )
  } else {
    next()
  }
}
