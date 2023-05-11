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

# set up project paths----
if (file.exists(here::here(".env"))) {
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_raw <- Sys.getenv("DIR_RAW")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))
  input_path_scenario_tms <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_TMS"))
  input_path_scenario_sda <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_SDA"))
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- Sys.getenv("DIR_OUTPUT")
  output_path_standard <- file.path(output_path, "standard")
  output_path_aggregated <- file.path(output_path, "aggregated")
} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# set project parameters----
scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
region_select <- Sys.getenv("PARAM_REGION_SELECT")
start_year <- as.numeric(Sys.getenv("PARAM_START_YEAR"))
time_frame_select <- as.integer(Sys.getenv("PARAM_TIME_FRAME"))
benchmark_regions <- unlist(base::strsplit(Sys.getenv("PARAM_BENCHMARK_REGIONS"), ","))

# TODO: add check if all files exist, resort to test files if not

# TODO: remove the temp section once r2dii.data is updated
############# TEMP #############
# r2dii.data is not updated yet, so we manually update the region_isos data to
# cover the 2022 scenarios
regions_geco_2022 <- readr::read_csv(input_path_regions_geco_2022)
regions_weo_2022 <- readr::read_csv(input_path_regions_weo_2022)

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
scenario_input_tms <- read.csv(input_path_scenario_tms)
scenario_input_sda <- read.csv(input_path_scenario_sda)

# abcd <- abcd_test_data
abcd <- readr::read_csv(file.path(input_path_abcd))
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

# loanbook <- loanbook_test_data
loanbook <- purrr::map_dfr(list.files(input_path_raw, full.names = T), .f = vroom::vroom, id = "group_id")
# aggregation functions expect a group_id to be able to distinguish banks/loan books in later analysis
loanbook <- loanbook %>%
  dplyr::mutate(group_id = gsub(pattern = paste0(input_path_raw, "/"), replacement = "", x = .data$group_id)) %>%
  dplyr::mutate(group_id = gsub(pattern = ".csv", replacement = "", x = .data$group_id))

# match and prioritize loan book----
unique_loanbooks_raw <- unique(loanbook$group_id)

matched_loanbook <- NULL

for (i in unique_loanbooks_raw) {
  loanbook_i <- loanbook %>%
    dplyr::filter(.data$group_id == i)

  matched_i <- match_name(loanbook_i, abcd) %>%
    prioritize()

  matched_loanbook <- matched_loanbook %>%
    dplyr::bind_rows(matched_i)
}

# matched_loanbook %>%
#   readr::write_csv(file.path(input_path_matched, "matched_prio_all_groups.csv"))


# generate all P4B outputs----
unique_loanbooks_matched <- unique(matched_loanbook$group_id)

## generate SDA outputs----
results_sda_total <- NULL

for (i in unique_loanbooks_matched) {
  matched_i <- matched_loanbook %>%
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

# results_sda_total %>%
#   readr::write_csv(file.path(output_path_standard, "sda_results_all_groups.csv"))


## generate TMS outputs----

results_tms_total <- NULL

for (i in unique_loanbooks_matched) {
  matched_i <- matched_loanbook %>%
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

# results_tms_total %>%
#   readr::write_csv(file.path(output_path_standard, "tms_results_all_groups.csv"))

# generate P4B plots----

# results_tms_total <- readr::read_csv(file.path(output_path_standard, "tms_results_all_groups.csv"), col_types = readr::cols())
# results_sda_total <- readr::read_csv(file.path(output_path_standard, "sda_results_all_groups.csv"), col_types = readr::cols())
# matched_loanbook <- readr::read_csv("file.path(input_path_matched, "matched_prio_all_groups.csv"), col_types = readr::cols())

## retrieve set of unique groups to loop over----
unique_groups_tms <- unique(results_tms_total$group_id)
unique_groups_sda <- unique(results_sda_total$group_id)

## run automatic result generation ----------

# TODO: get all available sectors and produce outputs for them all)
for (tms_i in unique_groups_tms) {
  generate_individual_outputs(
    data = results_tms_total,
    matched_loanbook = matched_loanbook,
    output_directory = output_path_standard,
    target_type = "tms",
    group_id = tms_i,
    scenario_source = scenario_source_input,
    target_scenario = glue::glue("target_{scenario_select}"),
    region = "global",
    sector = "power"
  )
}

# TODO: get all available sectors and produce outputs for them all)
for (sda_i in unique_groups_sda) {
  generate_individual_outputs(
    data = results_sda_total,
    matched_loanbook = matched_loanbook,
    output_directory = output_path_standard,
    target_type = "sda",
    group_id = sda_i,
    scenario_source = scenario_source_input,
    target_scenario = glue::glue("target_{scenario_select}"),
    region = "global",
    sector = "steel"
  )
}
