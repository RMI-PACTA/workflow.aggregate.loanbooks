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
    )

  plot
}

# plot match success rate----
plot_match_success_rate <- function(data,
                                    aggregate,
                                    metric_type = c("absolute", "relative"),
                                    match_success_type = c("n", "outstanding", "credit_limit"),
                                    currency) {
  # prepare data
  if (aggregate) {
    data <- data %>%
      dplyr::filter(group_id == "meta_loanbook")
  } else {
    data <- data %>%
      dplyr::filter(group_id != "meta_loanbook")
  }

  data <- data %>%
    dplyr::filter(sector != "not in scope") %>%
    dplyr::filter(metric_type == .env$metric_type) %>%
    dplyr::filter(match_success_type == .env$match_success_type)

  # plot design
  fill_scale <- c(
    "Matched" = "darkolivegreen3",
    "Not Matched" = "brown3"
  )

  theme_match_success <- ggplot2::theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

  # plot description

  title <- r2dii.plot::to_title(glue::glue("{metric_type} Match Success Rate"))

  if (match_success_type == "n") {
    subtitle <- "number of loans by sector"
  } else if (match_success_type == "outstanding") {
    subtitle <- "loan size outstanding by sector"
  } else {
    subtitle <- "credit limit by sector"
  }

  if (aggregate) {
    subtitle <- r2dii.plot::to_title(glue::glue("aggregate {subtitle}"))
  } else {
    subtitle <- r2dii.plot::to_title(glue::glue("{subtitle} and loan book"))
  }

  if (match_success_type == "n") {
    y_label <- "Match success rate (n)"
  } else if (match_success_type == "outstanding") {
    y_label <- "Match success rate: loan size outstanding"
  } else {
    y_label <- "Match success rate: credit limit"
  }

  if (metric_type == "absolute") {
    y_label <- glue::glue("{y_label} (in {currency})")
  } else {
    y_label <- glue::glue("{y_label} (share of total)")
  }

  # plot
  plot <- data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = r2dii.plot::to_title(sector),
        y = match_success_rate,
        fill = matched
      )
    ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(values = fill_scale) +
    ggplot2::labs(
      x = "Sector",
      y = y_label,
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_bw() +
    theme_match_success

  if (!aggregate) {
    plot <- plot +
      ggplot2::facet_wrap(
        ~ group_id
      )
  }

  plot
}
