rm_inactive_companies <- function(data,
                                  start_year,
                                  time_frame_select) {
  data_no_prod_t5 <- data %>%
    dplyr::filter(
      year %in% c(.env$start_year, .env$start_year + .env$time_frame_select)
    ) %>%
    dplyr::summarise(
      sum_production = sum(.data$production, na.rm = TRUE),
      .by =c("name_company", "sector", "year")
    ) %>%
    tidyr::pivot_wider(
      names_from = "year",
      names_prefix = "prod_",
      values_from = "sum_production"
    ) %>%
    dplyr::filter(
      !!rlang::sym(paste0("prod_", start_year)) > 0,
      !!rlang::sym(paste0("prod_", start_year + time_frame_select)) == 0
    )

  comp_sec_no_prod_t5 <- data_no_prod_t5 %>%
    dplyr::distinct(.data$name_company, .data$sector)

  data_no_prod_t0_to_t5 <- data %>%
    dplyr::filter(
      year %in% c(.env$start_year, .env$start_year + .env$time_frame_select)
    ) %>%
    dplyr::summarise(
      sum_production = sum(.data$production, na.rm = TRUE),
      .by =c("name_company", "sector")
    ) %>%
    dplyr::filter(
      .data$sum_production == 0
    )

  comp_sec_no_prod_t0_to_t5 <- data_no_prod_t0_to_t5 %>%
    dplyr::distinct(.data$name_company, .data$sector)

  data <- data %>%
    dplyr::anti_join(
      comp_sec_no_prod_t5,
      by = c("name_company", "sector")
    ) %>%
    dplyr::anti_join(
      comp_sec_no_prod_t0_to_t5,
      by = c("name_company", "sector")
    )

  return(data)
}

apply_sector_split_to_loans <- function(data,
                                        abcd,
                                        companies_sector_split,
                                        sector_split_type,
                                        input_path_matched) {
  unique_companies_pre_split <- data %>%
    distinct(name_abcd)

  if (sector_split_type == "equal_weights") {
    abcd_id <- abcd %>%
      dplyr::distinct(.data$company_id, .data$name_company)

    # identify lost_companies_sector_split and write to csv for inspection
    lost_companies_sector_split <- companies_sector_split %>%
      dplyr::anti_join(
        abcd_id,
        by = c("company_id")
      )

    lost_companies_sector_split %>%
      readr::write_csv(
        path = file.path(input_path_matched, "lost_companies_sector_split.csv")
      )

    companies_sector_split <- companies_sector_split %>%
      dplyr::left_join(
        abcd_id,
        by = c("company_id")
      ) %>%
      dplyr::select(-"company_id")
  }

  data <- data %>%
    dplyr::inner_join(
      companies_sector_split,
      by = c("name_abcd" = "name_company", "sector_abcd" = "sector")
    ) %>%
    dplyr::mutate(
      # renaming the loan_id is not conditional to avoid any chance of accidentally
      # renaming a split loan to a loan_id that already exists elsewhere
      id_loan = paste(.data$id_loan, .data$sector_abcd, sep = "_"),
      loan_size_outstanding = dplyr::if_else(
        is.na(.data$sector_split),
        .data$loan_size_outstanding,
        .data$loan_size_outstanding * .data$sector_split
      ),
      loan_size_credit_limit = dplyr::if_else(
        is.na(.data$sector_split),
        .data$loan_size_credit_limit,
        .data$loan_size_credit_limit * .data$sector_split
      )
    ) %>%
    dplyr::select(-"sector_split")

  unique_companies_post_split <- data %>%
    distinct(name_abcd)

  if (nrow(unique_companies_pre_split) != nrow(unique_companies_post_split)) {
    warning(
      glue::glue(
        "Applying the sector split has lead to changes in the number of unique
        companies covered in the analysis. Prior to the split, there were
        {nrow(unique_companies_pre_split)} unique companies. After the split,
        there are {nrow(unique_companies_post_split)} unique companies."
      )
    )
  }

  return(data)
}

sample_raw_loanbook_from_abcd <- function(abcd = NULL,
                                          sector_shares = NULL,
                                          n_companies = NULL,
                                          currency = NULL,
                                          total_exposure = NULL,
                                          total_credit_limit = NULL) {
  # TODO: maybe add seed as argument?
  if (is.null(abcd)) {
    abcd <- r2dii.data::abcd_demo
    message("You did not provide any ABCD to sample from. Using r2dii.data::abcd_demo.")
  }
  if (is.null(sector_shares)) {
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
    message(
      "You did not provide any information on sector shares to be sampled.
      Using default sector shares."
    )
  }
  if (is.null(n_companies)) {
    n_companies <- 20
    message(
      "You did not provide a number of companies for the loan book you would
      like to sample. Using n_companies = 20 as a default."
    )
  }
  if (is.null(currency)) {
    currency <- "USD"
    message(
      "You did not provide a currency for the loans in the sample laon book.
      Using USD as a deafult."
    )
  }
  if (is.null(total_exposure)) {
    total_exposure <- 1000000
    message(
      "You did not provide a total_exposure value for the sample loan book.
      Using 1000000 as a default."
    )
  }
  if (is.null(total_credit_limit)) {
    total_credit_limit <- 2 * total_exposure
    message(
      "You did not provide a total_credit_limit value for the sample loan book.
      Using 2 * total_exposure as a default."
    )
  }

  validate_input_sample_raw_loanbook_from_abcd(
    abcd = abcd,
    sector_shares = sector_shares,
    n_companies = n_companies,
    currency = currency,
    total_exposure = total_exposure,
    total_credit_limit = total_credit_limit
  )

  dist_abcd_sector <- abcd %>%
    dplyr::filter(.data$is_ultimate_owner) %>%
    dplyr::distinct(
      .data$company_id,
      .data$name_company,
      .data$sector
    )

  # TODO: dist_abcd_multi_sector

  n_companies_sector <- sector_shares %>%
    dplyr::mutate(n = round(.env$n_companies * .data$share, 0)) %>%
    dplyr::select(-"share")

  # if the sum of n_companies_sector$n is not equal to the value n_companies,
  # calculate the remainder and adjust the sector with the alrgest sample by that remainder
  if (
    sum(n_companies_sector$n, na.rm = TRUE) != n_companies
  ) {
    remainder <- n_companies - sum(n_companies_sector$n, na.rm = TRUE)

    max_sector <- n_companies_sector %>%
      dplyr::slice_max(order_by = .data$n, n = 1) %>%
      dplyr::mutate(n = .data$n + remainder)

    n_companies_sector <- n_companies_sector %>%
      dplyr::filter(.data$sector != max_sector$sector) %>%
      dplyr::bind_rows(max_sector)
  }

  sectors_to_sample <- sector_shares %>%
    dplyr::filter(.data$share > 0) %>%
    dplyr::distinct(.data$sector) %>%
    dplyr::pull()

  sample_sectors <- NULL

  for (i in sectors_to_sample) {
    n_companies_sector_i <- n_companies_sector %>%
      dplyr::filter(.data$sector == i) %>%
      dplyr::distinct(.data$n) %>%
      dplyr::pull()

    sample_sector_i <- dist_abcd_sector %>%
      dplyr::filter(.data$sector == i) %>%
      dplyr::slice_sample(n = n_companies_sector_i)

    sample_sectors <- sample_sectors %>%
      dplyr::bind_rows(sample_sector_i)
  }

  # set up basic loan book structure with sampled companies----
  abcd_sample <- abcd %>%
    dplyr::inner_join(
      sample_sectors,
      by = c("company_id", "name_company", "sector")
    )

  # TODO: check if this is affected in case we update the nace_classification
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

  # TODO: how to handle group_id? not included at all for now, probably not needed
  loanbook_sample <- tibble::tibble(
    id_direct_loantaker = paste0("C", loanbook_sample_prep$company_id),
    name_direct_loantaker = loanbook_sample_prep$name_company,
    id_intermediate_parent_1 = NA_character_,
    name_intermediate_parent_1 = NA_character_,
    id_ultimate_parent = paste0("UP", loanbook_sample_prep$company_id),
    name_ultimate_parent = loanbook_sample_prep$name_company,
    loan_size_outstanding = NA_real_,
    loan_size_outstanding_currency = .env$currency,
    loan_size_credit_limit = NA_real_,
    loan_size_credit_limit_currency = .env$currency,
    sector_classification_system = loanbook_sample_prep$sector_classification_system,
    sector_classification_input_type = loanbook_sample_prep$sector_classification_input_type,
    sector_classification_direct_loantaker = loanbook_sample_prep$code,
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
      loan_size_outstanding = round(.data$share_exposure * .env$total_exposure, 0),
      loan_size_credit_limit = round(.data$share_exposure * .env$total_credit_limit, 0)
    ) %>%
    dplyr::select(-dplyr::all_of(c("random_beta", "share_exposure")))

  return(loanbook_sample)
}

validate_input_sample_raw_loanbook_from_abcd <- function(abcd,
                                                         sector_shares,
                                                         n_companies,
                                                         currency,
                                                         total_exposure,
                                                         total_credit_limit) {
  # validate input values
  validate_input_args_sample_raw_loanbook_from_abcd(
    n_companies = n_companies,
    currency = currency,
    total_exposure = total_exposure,
    total_credit_limit = total_credit_limit
  )

  # validate input data set
  validate_input_data_sample_raw_loanbook_from_abcd(
    abcd = abcd,
    sector_shares = sector_shares
  )

  invisible()
}

validate_input_args_sample_raw_loanbook_from_abcd <- function(n_companies,
                                                              currency,
                                                              total_exposure,
                                                              total_credit_limit) {
  if (!length(n_companies) == 1) {
    stop("Argument n_companies must be of length 1. Please check your input.")
  }
  if (!inherits(n_companies, "integer")) {
    stop("Argument n_companies must be of class integer Please check your input.")
  }
  if (!length(currency) == 1) {
    stop("Argument currency must be of length 1. Please check your input.")
  }
  if (!inherits(currency, "character")) {
    stop("Argument currency must be of class character. Please check your input.")
  }
  if (!length(total_exposure) == 1) {
    stop("Argument total_exposure must be of length 1. Please check your input.")
  }
  if (!inherits(total_exposure, "numeric")) {
    stop("Argument total_exposure must be of class numeric. Please check your input.")
  }
  if (!length(total_credit_limit) == 1) {
    stop("Argument total_credit_limit must be of length 1. Please check your input.")
  }
  if (!inherits(total_credit_limit, "numeric")) {
    stop("Argument total_credit_limit must be of class numeric. Please check your input.")
  }

  invisible()
}

validate_input_data_sample_raw_loanbook_from_abcd <- function(abcd,
                                                              sector_shares) {
  pacta.aggregate.loanbook.plots::validate_data_has_expected_cols(
    data = abcd,
    expected_columns = c(
      "company_id", "name_company", "lei", "is_ultimate_owner", "sector",
      "technology", "plant_location", "year", "production", "production_unit",
      "emission_factor", "emission_factor_unit"#, "ald_timestamp"
    )
  )

  pacta.aggregate.loanbook.plots::validate_data_has_expected_cols(
    data = sector_shares,
    expected_columns = c(
      "sector", "share"
    )
  )

  invisible()
}
