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
