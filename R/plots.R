prep_scatter_alignment_exposure <- function(data,
                                            year,
                                            region,
                                            scenario,
                                            category,
                                            exclude_group_ids = "benchmark") {
  data <- data %>%
    dplyr::filter(
      !grepl(paste0(.env$exclude_group_ids, collapse = "|"), .data$group_id)
    ) %>%
    dplyr::filter(
      .data$year == .env$year,
      .data$region == .env$region,
      .data$scenario == .env$scenario
    ) %>%
    dplyr::select(
      dplyr::all_of(
        c(
          .env$category,
          "scenario",
          "region",
          "sector",
          "year",
          "exposure_weighted_net_alignment",
          "sum_loan_size_outstanding"
        )
      )
    )

  data
}

plot_scatter_alignment_exposure <- function(data,
                                            floor_outliers,
                                            cap_outliers,
                                            category,
                                            currency) {

  if (!is.null(floor_outliers)) {
    data <- data %>%
      mutate(
        exposure_weighted_net_alignment = if_else(
          .data$exposure_weighted_net_alignment <= .env$floor_outliers,
          .env$floor_outliers,
          .data$exposure_weighted_net_alignment
        )
      )
  }

  if (!is.null(cap_outliers)) {
    data <- data %>%
      mutate(
        exposure_weighted_net_alignment = if_else(
          .data$exposure_weighted_net_alignment >= .env$cap_outliers,
          .env$cap_outliers,
          .data$exposure_weighted_net_alignment
        )
      )
  }

  title <- glue::glue("Net Aggregate Alignment By Financial Exposure And Sector")
  subtitle <- ""
  if (any(!is.null(floor_outliers), !is.null(cap_outliers))) {
    subtitle <- glue::glue(
      "{subtitle}Outliers are displayed on the lower and upper boundaries: {floor_outliers} and {cap_outliers}.",
      .trim = FALSE
    )
  }

  plot <- data %>%
    dplyr::mutate(sector = tools::toTitleCase(.data$sector)) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = sum_loan_size_outstanding,
        y = exposure_weighted_net_alignment,
        color = !!rlang::sym(category)
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::ylim(-1, 1) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::facet_wrap(
      ~ sector
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      color = r2dii.plot::to_title(category)
    ) +
    ggplot2::xlab(glue::glue("Financial Exposure (in {currency})")) +
    ggplot2::ylab("Net Aggregate Alignment") +
    r2dii.plot::scale_colour_r2dii() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid = ggplot2::element_blank()
      # panel.grid.minor = ggplot2::element_blank()
    )

  plot
}
