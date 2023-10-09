# load packages----
library(dotenv)
library(dplyr)
library(pacta.aggregate.loanbook.plots)
library(r2dii.data)
library(readxl)
library(rlang)
library(tidyr)

dotenv::load_dot_env()
source("expected_columns.R")
source("R/functions_prep_project.R")

# set up project----
if (file.exists(here::here(".env"))) {
  # paths
  input_path_raw <- Sys.getenv("DIR_RAW")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")

  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  # project parameters
  n_companies <- as.integer(Sys.getenv("SAMPLE_N_COMPANIES"))
  total_exposure_lbk <- as.numeric(Sys.getenv("SAMPLE_EXPOSURE"))
  total_credit_limit_lbk <- as.numeric(Sys.getenv("SAMPLE_CREDIT_LIMIT"))
  currency_lbk <- Sys.getenv("SAMPLE_CURRENCY")

} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# read abcd
abcd <- readr::read_csv(
  file.path(input_path_abcd),
  col_types = col_types_abcd,
  col_select = dplyr::all_of(col_select_abcd)
)
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

# Optional: sample only companies which have some production in a selected region_sample
region_companies_source <- "weo_2021"
region_companies <- "europe"
region_sample <- r2dii.data::region_isos %>%
  dplyr::filter(
    .data$source == .env$region_companies_source,
    .data$region == .env$region_companies
  ) %>%
  dplyr::distinct(.data$isos) %>%
  dplyr::pull() %>%
  toupper()

if (!is.null(region_sample) & length(region_sample) > 0) {
  valid_companies <- abcd %>%
    dplyr::filter(.data$plant_location %in% .env$region_sample) %>%
    dplyr::distinct(.data$company_id)

  abcd <- abcd %>%
    dplyr::semi_join(
      valid_companies,
      by = "company_id"
    )
}
# define parameters for sampling----

# styler: off
# TODO: activate "hdv" and "shipping" once they become available for P4B
sector_shares <- tibble::tribble(
  ~sector,    ~share,
  "aviation",   0.02,
  "automotive",  0.2,
  "cement",     0.02,
  "coal",       0.03,
  # "hdv",           0,
  "oil and gas", 0.1,
  "power",       0.6,
  # "shipping",      0,
  "steel",      0.03
)
# styler: on

# sample companies based on parameters----
# seed varies with number of companies and values of sector shares for some randomness
seed_i <- round(n_companies + sd(sector_shares$share) * 100)

if (exists("seed_i") & length(seed_i) == 1 & is.numeric(seed_i)) {
  set.seed(seed_i)
} else {
  set.seed(123)
}

loanbook_sample <- sample_raw_loanbook_from_abcd(
  abcd = abcd,
  sector_shares = sector_shares,
  n_companies = n_companies,
  currency = currency_lbk,
  total_exposure = total_exposure_lbk,
  total_credit_limit = total_credit_limit_lbk
)

# write to raw loan book path in project directory----
loanbook_sample %>%
  readr::write_csv(file.path(input_path_raw, glue::glue("sample_loanbook_{n_companies}_{seed_i}.csv")))
