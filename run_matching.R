# set up project and load packages----
library(dotenv)
library(r2dii.data)
library(r2dii.match)
library(tidyverse)
library(withr)

dotenv::load_dot_env()
source("expected_columns.R")
# source("R/functions_project.R")

# set up paths and parameters----
if (file.exists(here::here(".env"))) {
  # paths
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_dir_raw <- Sys.getenv("DIR_RAW")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_filename_raw <- Sys.getenv("FILENAME_RAW")
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  # optional: own sector classifications
  use_own_sector_classification <- Sys.getenv("USE_OWN_SECTOR_CLASSIFICATION")
  if (use_own_sector_classification == "TRUE") {
    input_dir_own_sector_classification <- Sys.getenv("DIR_OWN_SECTOR_CLASSIFICATION")
    input_path_own_sector_classification <- file.path(
      input_dir_own_sector_classification,
      Sys.getenv("FILENAME_OWN_SECTOR_CLASSIFICATION")
    )
  }

} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# load data----
raw_lbk <- readr::read_csv(
  file = file.path(input_dir_raw, input_filename_raw),
  col_types = col_types_raw
)
group_name = gsub(".csv", "", input_filename_raw)

abcd <- readr::read_csv(
  file.path(input_path_abcd),
  col_types = col_types_abcd,
  col_select = dplyr::all_of(col_select_abcd)
)

# match data----

## optional: use own sector classification system
if (use_own_sector_classification) {
  sector_classification_system <- readr::read_csv(
    file = input_path_own_sector_classification,
    col_types = readr::cols_only(
      code_system = "c",
      code = "c",
      sector = "c",
      borderline = "l"
    )
  ) %>%
    dplyr::select(names(r2dii.data::sector_classifications))

  withr::with_options(
    list(r2dii.match.sector_classifications = sector_classification_system),
    {getOption("r2dii.match.sector_classifications")
      matched_lbk <- r2dii.match::match_name(
        loanbook = raw_lbk,
        abcd = abcd,
        by_sector = TRUE,
        min_score = 0.9
      )}
  )
} else {
  matched_lbk <- r2dii.match::match_name(
    loanbook = raw_lbk,
    abcd = abcd,
    by_sector = TRUE,
    min_score = 0.9
  )
}

matched_lbk %>%
  readr::write_csv(
    file = file.path(input_path_matched, glue::glue("matched_lbk_{group_name}.csv")),
    na = ""
  )

## manual matching----

# if there are any scores other than 1 and 0, write the matched file to a local
# csv file, and edit the scores so that they are either 1 or 0. 1 means that the
# record is a correct match and should be kept whereas 0 means the potential
# match should not be kept. Then read the file back in and proceed with
# prioritization.
matched_lbk_manual <- readr::read_csv(
  file.path(input_path_matched, glue::glue("matched_lbk_{group_name}_manual.csv")),
  col_types = col_types_matched_manual
)

## prioritize----

matched_prio <- r2dii.match::prioritize(matched_lbk_manual) %>%
  dplyr::mutate(group_id = .env$group_name)

matched_prio %>%
  readr::write_csv(
    file = file.path(input_path_matched, glue::glue("matched_prio_{group_name}.csv")),
    na = ""
  )

