rm_inactive_companies <- function(data,
                                  start_year,
                                  time_frame_select) {
  data_no_prod_t5 <- data %>%
    dplyr::filter(
      year %in% c(.env$start_year, .env$start_year + .env$time_frame_select)
    ) %>%
    dplyr::summarise(
      sum_production = sum(production, na.rm = TRUE),
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

  data <- data %>%
    dplyr::anti_join(
      comp_sec_no_prod_t5,
      by = c("name_company", "sector")
    )

  return(data)
}

apply_sector_split_to_loans <- function(data,
                                        abcd,
                                        companies_sector_split,
                                        sector_split_type) {
  unique_companies_pre_split <- data %>%
    distinct(name_abcd)

  if (sector_split_type == "equal_weights") {
    abcd_id <- abcd %>%
      dplyr::distinct(.data$company_id, .data$name_company)

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

sample_raw_loanbook_from_abcd <- function(abcd,
                                          sector_shares = NULL,
                                          n_companies,
                                          currency,
                                          total_exposure,
                                          total_credit_limit) {
  # TODO: add input checks

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

  }

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
      loan_size_outstanding = round(.data$share_exposure * .env$total_exposure, 0),
      loan_size_credit_limit = round(.data$share_exposure * .env$total_credit_limit, 0)
    ) %>%
    dplyr::select(-dplyr::all_of(c("random_beta", "share_exposure")))

  return(loanbook_sample)
}
