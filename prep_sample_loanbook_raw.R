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

# TODO: regionalize?

# read abcd
abcd <- readr::read_csv(
  file.path(input_path_abcd),
  col_types = col_types_abcd,
  col_select = dplyr::all_of(col_select_abcd)
)
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

# define parameters for sampling----

# styler: off
sector_shares <- tibble::tribble(
  ~sector,    ~share,
  "aviation",   0.02,
  "automotive",  0.2,
  "cement",     0.02,
  "coal",       0.03,
  "hdv",           0,
  "oil and gas", 0.1,
  "power",       0.6,
  "shipping",      0,
  "steel",      0.03
)
# styler: on

# sample companies based on parameters----
# seed varies with number of companies and values of sector shares for some randomness
seed_i <- round(n_companies + sd(sector_shares$share) * 100)
set.seed(seed_i)

dist_abcd_sector <- abcd %>%
  dplyr::filter(.data$is_ultimate_owner) %>%
  dplyr::distinct(
    .data$company_id,
    .data$name_company,
    .data$sector
  )

# TODO: dist_abcd_multi_sector

n_copanies_sector <- sector_shares %>%
  dplyr::mutate(n = round(.env$n_companies * .data$share, 0)) %>%
  dplyr::select(-"share")

sectors_to_sample <- sector_shares %>%
  dplyr::filter(.data$share > 0) %>%
  dplyr::distinct(.data$sector) %>%
  dplyr::pull()

sample_sectors <- NULL

for (i in sectors_to_sample) {
  n_copanies_sector_i <- n_copanies_sector %>%
    dplyr::filter(.data$sector == i) %>%
    dplyr::distinct(.data$n) %>%
    dplyr::pull()

  sample_sector_i <- dist_abcd_sector %>%
    dplyr::filter(.data$sector == i) %>%
    dplyr::slice_sample(n = n_copanies_sector_i)

  sample_sectors <- sample_sectors %>%
    dplyr::bind_rows(sample_sector_i)
}

# set up basic loan book structure with sampled companies----
abcd_sample <- abcd %>%
  dplyr::inner_join(
    sample_sectors,
    by = c("company_id", "name_company", "sector")
  )

sample_sector_codes <- r2dii.data::nace_classification %>%
  dplyr::filter(.data$borderline == FALSE) %>%
  dplyr::group_by(.data$sector) %>%
  dplyr::slice_max(.data$code_level, n = 1) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::select("sector", "code") %>%
  dplyr::mutate(
    sector_classification_system = "NACE",
    sector_classification_input_type = "Code"
  )

abcd_sample_sector_codes <- abcd_sample %>%
  dplyr::inner_join(
    sample_sector_codes,
    by = "sector"
  )

loanbook_sample_prep <- abcd_sample_sector_codes %>%
  dplyr::distinct(
    .data$company_id,
    .data$name_company,
    .data$lei,
    .data$sector_classification_system,
    .data$sector_classification_input_type,
    .data$code
  )

# TODO: how to handle group_id?
loanbook_sample <- tibble::tibble(
  id_direct_loantaker = paste0("C", loanbook_sample_prep$company_id),
  name_direct_loantaker = loanbook_sample_prep$name_company,
  id_intermediate_parent_1 = NA_character_,
  name_intermediate_parent_1 = NA_character_,
  id_ultimate_parent = paste0("UP", loanbook_sample_prep$company_id),
  name_ultimate_parent = loanbook_sample_prep$name_company,
  loan_size_outstanding = NA_real_,
  loan_size_outstanding_currency = .env$currency_lbk,
  loan_size_credit_limit = NA_real_,
  loan_size_credit_limit_currency = .env$currency_lbk,
  sector_classification_system = loanbook_sample_prep$sector_classification_system,
  sector_classification_input_type = loanbook_sample_prep$sector_classification_input_type,
  sector_classification_direct_loantaker = as.numeric(loanbook_sample_prep$code),
  fi_type = "Loan",
  flag_project_finance_loan = "No",
  name_project = NA_character_,
  lei_direct_loantaker = loanbook_sample_prep$lei,
  isin_direct_loantaker = NA_character_
) %>%
  tibble::rowid_to_column() %>%
  dplyr::mutate(id_loan = paste0("L", .data$rowid)) %>%
  dplyr::select(-"rowid")

# add randomly sampled loan size----
loanbook_sample <- loanbook_sample %>%
  dplyr::mutate(
    random_beta = stats::rbeta(n = n_companies, shape1 = 1, shape2 = 3),
    share_exposure = .data$random_beta / sum(.data$random_beta, na.rm = TRUE)
  ) %>%
  dplyr::mutate(
    loan_size_outstanding = round(.data$share_exposure * .env$total_exposure_lbk, 0),
    loan_size_credit_limit = round(.data$share_exposure * .env$total_credit_limit_lbk, 0)
  ) %>%
  dplyr::select(-dplyr::all_of(c("random_beta", "share_exposure")))

# write to raw loan book path in project directory----
loanbook_sample %>%
  readr::write_csv(file.path(input_path_raw, glue::glue("sample_loanbook_{n_companies}_{seed_i}.csv")))
