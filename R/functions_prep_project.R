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
}
