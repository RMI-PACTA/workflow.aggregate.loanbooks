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
} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# set project parameters----
scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
region_select <- Sys.getenv("PARAM_REGION_SELECT")

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

# loanbook <- loanbook_test_data
loanbook <- purrr::map_dfr(
  list.files(input_path_raw, full.names = T, pattern = "*.csv"),
  .f = vroom::vroom,
  id = "group_id"
)
# aggregation functions expect a group_id to be able to distinguish banks/loan books in later analysis
loanbook <- loanbook %>%
  dplyr::mutate(group_id = gsub(pattern = paste0(input_path_raw, "/"), replacement = "", x = .data$group_id)) %>%
  dplyr::mutate(group_id = gsub(pattern = ".csv", replacement = "", x = .data$group_id))

# match loan book----
unique_loanbooks_raw <- unique(loanbook$group_id)

matched_loanbook <- NULL

for (i in unique_loanbooks_raw) {
  loanbook_i <- loanbook %>%
    dplyr::filter(.data$group_id == i)

  matched_i <- match_name(loanbook_i, abcd)

  matched_loanbook <- matched_loanbook %>%
    dplyr::bind_rows(matched_i)
}

matched_loanbook %>%
  readr::write_csv(file.path(input_path_matched, "matched_all_groups.csv"))

# manual step----

# prioritize checked matched loan book----
matched_loanbook_checked <- readr::read_csv(
  file.path(input_path_matched, "matched_all_groups.csv"),
  col_types = col_types_matched_all_groups,
  col_select = dplyr::all_of(col_select_matched_all_groups)
)


unique_matched_loanbook_checked <- unique(matched_loanbook_checked$group_id)

matched_prioritized <- NULL

for (i in unique_matched_loanbook_checked) {
  matched_loanbook_checked_i <- matched_loanbook_checked %>%
    dplyr::filter(.data$group_id == i)

  matched_prioritized_i <- matched_loanbook_checked_i %>%
    prioritize()

  matched_prioritized <- matched_prioritized %>%
    dplyr::bind_rows(matched_prioritized_i)
}

matched_prioritized %>%
  readr::write_csv(file.path(input_path_matched, "matched_prio_all_groups.csv"))
