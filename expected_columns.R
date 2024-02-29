# expected columns region isos file
col_types_region_isos <- readr::cols_only(
  region = "c",
  isos = "c",
  source = "c"
)
col_select_region_isos <- names(col_types_region_isos[["cols"]])

# expected columns tms scenario file
col_types_scenario_tms <- readr::cols_only(
  scenario_source = "c",
  region = "c",
  scenario = "c",
  sector = "c",
  technology = "c",
  year = "i",
  smsp = "n",
  tmsr = "n"
)
col_select_scenario_tms <- names(col_types_scenario_tms[["cols"]])

# expected columns sda scenario file
col_types_scenario_sda <- readr::cols_only(
  scenario_source = "c",
  region = "c",
  scenario = "c",
  sector = "c",
  year = "i",
  emission_factor = "n",
  emission_factor_unit = "c"
)
col_select_scenario_sda <- names(col_types_scenario_sda[["cols"]])

# expected columns abcd file
col_types_abcd <- readr::cols_only(
  company_id = "i",
  name_company = "c",
  lei = "c",
  is_ultimate_owner = "l",
  sector = "c",
  technology = "c",
  plant_location = "c",
  year = "i",
  production = "n",
  production_unit = "c",
  emission_factor = "n",
  emission_factor_unit = "c",
  ald_timestamp = "c"
)
col_select_abcd <- names(col_types_abcd[["cols"]])

# expected columns matched_prioritized_all_groups file
col_types_matched_prio_all_groups <- readr::cols_only(
  group_id = "c",
  id_loan = "c",
  id_direct_loantaker = "c",
  name_direct_loantaker = "c",
  id_intermediate_parent_1 = "c",
  name_intermediate_parent_1 = "c",
  id_ultimate_parent = "c",
  name_ultimate_parent = "c",
  loan_size_outstanding = "n",
  loan_size_outstanding_currency = "c",
  loan_size_credit_limit = "n",
  loan_size_credit_limit_currency = "c",
  sector_classification_system = "c",
  sector_classification_input_type = "c",
  sector_classification_direct_loantaker = "c",
  fi_type = "c",
  flag_project_finance_loan = "c",
  name_project = "c",
  lei_direct_loantaker = "c",
  isin_direct_loantaker = "c",
  id_2dii = "c",
  level = "c",
  sector = "c",
  sector_abcd = "c",
  name = "c",
  name_abcd = "c",
  score = "n",
  source = "c",
  borderline = "l"
)
col_select_matched_prio_all_groups <- names(col_types_matched_prio_all_groups[["cols"]])

# expected columns matched_all_groups file
col_types_matched_all_groups <- readr::cols_only(
  group_id = "c",
  id_loan = "c",
  id_direct_loantaker = "c",
  name_direct_loantaker = "c",
  id_intermediate_parent_1 = "c",
  name_intermediate_parent_1 = "c",
  id_ultimate_parent = "c",
  name_ultimate_parent = "c",
  loan_size_outstanding = "n",
  loan_size_outstanding_currency = "c",
  loan_size_credit_limit = "n",
  loan_size_credit_limit_currency = "c",
  sector_classification_system = "c",
  sector_classification_input_type = "c",
  sector_classification_direct_loantaker = "c",
  fi_type = "c",
  flag_project_finance_loan = "c",
  name_project = "c",
  lei_direct_loantaker = "c",
  isin_direct_loantaker = "c",
  id_2dii = "c",
  level = "c",
  sector = "c",
  sector_abcd = "c",
  name = "c",
  name_abcd = "c",
  score = "n",
  source = "c",
  borderline = "l"
)
col_select_matched_all_groups <- names(col_types_matched_all_groups[["cols"]])

# expected columns raw loan book file
col_types_raw <- readr::cols(
  id_loan = "c",
  id_direct_loantaker = "c",
  name_direct_loantaker = "c",
  id_intermediate_parent_1 = "c",
  name_intermediate_parent_1 = "c",
  id_ultimate_parent = "c",
  name_ultimate_parent = "c",
  loan_size_outstanding = "n",
  loan_size_outstanding_currency = "c",
  loan_size_credit_limit = "n",
  loan_size_credit_limit_currency = "c",
  sector_classification_system = "c",
  sector_classification_input_type = "c",
  sector_classification_direct_loantaker = "c",
  fi_type = "c",
  flag_project_finance_loan = "c",
  name_project = "c",
  lei_direct_loantaker = "c",
  isin_direct_loantaker = "c"
  # .default = "c"
)

# expected columns companies_sector_split file
col_types_companies_sector_split <- readr::cols_only(
  company_id = "i",
  sector = "c",
  sector_split = "n"
)
col_select_companies_sector_split <- names(col_types_companies_sector_split[["cols"]])

# expected columns companies_sector_split_worst_case file
col_types_companies_sector_split_worst_case <- readr::cols_only(
  name_company = "c",
  sector = "c",
  sector_split = "n"
)
col_select_companies_sector_split_worst_case <- names(col_types_companies_sector_split_worst_case[["cols"]])
