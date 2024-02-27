# set up project and load packages----
library(dotenv)
library(ggplot2)
library(r2dii.data)
library(r2dii.match)
library(tidyverse)
library(withr)

dotenv::load_dot_env()
source("expected_columns.R")
source("R/plots.R")

# set up paths and parameters----
if (file.exists(here::here(".env"))) {
  # paths
  input_dir_raw <- Sys.getenv("DIR_RAW")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

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
# read in raw loan books
list_raw <- list.files(input_dir_raw)[grepl(".csv", list.files(input_dir_raw))]

raw_lbk <- NULL

# combine all matched loan books into one object to loop over
for (i in list_raw) {
  group_name_i = gsub(".csv", "", i)

  raw_lbk_i <- readr::read_csv(
    file = file.path(input_dir_raw, i),
    col_types = col_types_raw
  ) %>%
    dplyr::mutate(group_id = .env$group_name_i)

  raw_lbk <- raw_lbk %>%
    dplyr::bind_rows(raw_lbk_i)
}

# read in matched prioritized loan books

list_matched_prio <- list.files(input_path_matched)[grepl("matched_prio_", list.files(input_path_matched))]

matched_prioritized <- NULL

# combine all matched loan books into one object to loop over
for (i in list_matched_prio) {
  matched_prioritized_i <- readr::read_csv(
    file.path(input_path_matched, i),
    col_types = col_types_matched_prio_all_groups,
    col_select = dplyr::all_of(col_select_matched_prio_all_groups)
  )

  matched_prioritized <- matched_prioritized %>%
    dplyr::bind_rows(matched_prioritized_i)
}

# read sector classification
# TODO: code_system should come directly from the input file
if (use_own_sector_classification) {
  sector_classification_system <- readr::read_csv(
    file = input_path_own_sector_classification,
    col_types = readr::cols_only(
      code = "c",
      sector = "c",
      borderline = "l"
    )
  ) %>%
    dplyr::mutate(
      # TODO: remove, must be in input
      code_system = "NAICS_CAN",
      # TODO: remove, we are using character strings here
      code = as.double(code)
    ) %>%
    dplyr::select(names(r2dii.data::sector_classifications))
} else {
  sector_classifications_used <- unique(raw_lbk$sector_classification_system)

  sector_classification_system <- r2dii.data::sector_classifications %>%
    dplyr::filter(
      .data$code_system == .env$sector_classifications_used
    )
}

# combine data----
# add sectors to raw loan books
raw_lbk_with_sectors <- raw_lbk %>%
  dplyr::left_join(
    sector_classification_system,
    by = c(
      "sector_classification_system" = "code_system",
      "sector_classification_direct_loantaker" = "code"
    )
  ) %>%
  dplyr::mutate(
    sector = dplyr::if_else(
      is.na(.data$sector),
      "not in scope",
      .data$sector
    ),
    borderline = dplyr::if_else(
      is.na(.data$sector),
      FALSE,
      .data$borderline
    )
  )


# join raw and matched loan books, matching on all common columns, but using the
# financial sector from the raw loan book to match the production sector.
# this simulates matching with the option by_sector = TRUE
matched_prioritized <- matched_prioritized %>%
  dplyr::select(-"sector")

lbk_match_success <- raw_lbk_with_sectors %>%
  dplyr::left_join(
    matched_prioritized,
    by = c(
      "id_direct_loantaker",
      "name_direct_loantaker",
      "id_intermediate_parent_1",
      "name_intermediate_parent_1",
      "id_ultimate_parent",
      "name_ultimate_parent",
      "loan_size_outstanding",
      "loan_size_outstanding_currency",
      "loan_size_credit_limit",
      "loan_size_credit_limit_currency",
      "sector_classification_system",
      "sector_classification_input_type",
      "sector_classification_direct_loantaker",
      "fi_type",
      "flag_project_finance_loan",
      "name_project",
      "lei_direct_loantaker",
      "isin_direct_loantaker",
      "id_loan",
      "group_id",
      "sector" = "sector_abcd",
      "borderline"
    )
  ) %>%
  dplyr::mutate(
    matched = dplyr::case_when(
      .data$score == 1 ~ "Matched",
      is.na(.data$score) ~ "Not Matched",
      TRUE ~ "Not Matched"
    ),
    # unmatched borderline loans are considered not in scope, as they would
    # otherwise increase the potential exposure wrongly and artificially without
    # there being a realistic way to match that exposure
    sector = dplyr::case_when(
      .data$borderline == TRUE & .data$matched == "Not Matched" ~ "not in scope",
      TRUE ~ .data$sector
    )
  )

# optional: manually exclude loans from the match success calculation
# this is intended to allow excluding loans that are misclassified as in scope,
# but research reveals that the company is not actually in scope
if (file.exists(file.path(input_path_matched, "loans_to_remove.csv"))) {
  loans_to_remove <- readr::read_csv(
    file = file.path(input_path_matched, "loans_to_remove.csv"),
    col_types = readr::cols_only(
      id_loan = "c",
      group_id = "c"
    )
  )

  lbk_match_success <- lbk_match_success %>%
    dplyr::anti_join(
      loans_to_remove,
      by = c("id_loan", "group_id")
    )
}

# add meta loan book
lbk_match_success_meta <- lbk_match_success %>%
  dplyr::mutate(
    id_loan = paste0(.data$id_loan, "_", .data$group_id),
    group_id = "meta_loanbook",
  )

lbk_match_success <- lbk_match_success %>%
  dplyr::bind_rows(lbk_match_success_meta)

# calculate match success rate----
lbk_match_success_rate <- lbk_match_success %>%
  dplyr::mutate(
    total_n = n(),
    total_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    total_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE),
    .by = c("group_id", "sector")
  ) %>%
  dplyr::summarise(
    match_n = n(),
    match_outstanding = sum(loan_size_outstanding, na.rm = TRUE),
    match_credit_limit = sum(loan_size_credit_limit, na.rm = TRUE),
    .by = c("group_id", "sector", "matched", "total_n", "total_outstanding", "total_credit_limit")
  ) %>%
  dplyr::mutate(
    match_success_rate_rel = .data$match_n / .data$total_n,
    match_success_outstanding_rel = .data$match_outstanding / .data$total_outstanding,
    match_success_credit_limit_rel = .data$match_credit_limit / .data$total_credit_limit
  ) %>%
  dplyr::select(
    dplyr::all_of(
      c(
        "group_id",
        "sector",
        "matched",
        "match_n",
        "total_n",
        "match_success_rate_rel",
        "match_outstanding",
        "total_outstanding",
        "match_success_outstanding_rel",
        "match_credit_limit",
        "total_credit_limit",
        "match_success_credit_limit_rel"
      )
    )
  ) %>%
  dplyr::arrange(
    .data$group_id,
    .data$sector,
    .data$matched
  )

# write to csv
lbk_match_success_rate %>%
  readr::write_csv(
    file = file.path(input_path_matched, "lbk_match_success_rate.csv")
  )

# prepare match success data for plotting----
data_lbk_match_success_rate <- lbk_match_success_rate %>%
  dplyr::select(
    -dplyr::starts_with("total")
  ) %>%
  tidyr::pivot_longer(
    cols = -c(
      "group_id",
      "sector",
      "matched"
    ),
    names_to = "match_success_type",
    values_to = "match_success_rate"
  ) %>%
  dplyr::mutate(
    metric_type = dplyr::if_else(
      grepl("_rel", .data$match_success_type),
      "relative",
      "absolute"
    ),
    match_success_type = dplyr::case_when(
      grepl("outstanding", .data$match_success_type) ~ "outstanding",
      grepl("credit_limit", .data$match_success_type) ~ "credit_limit",
      TRUE ~ "n"
    )
  )

# plot match success rate----
## set plot parameters----
plot_width <- 12
plot_height <- 8
plot_units <- "in"
plot_res <- 300

## plot relative match success rates for individual loan books----
plot_match_success_rate_rel_n_ind <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = FALSE,
    metric_type = "relative",
    match_success_type = "n",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_rel_n_individual.png"),
  plot = plot_match_success_rate_rel_n_ind,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_rel_outstanding_ind <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = FALSE,
    metric_type = "relative",
    match_success_type = "outstanding",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_rel_outstanding_individual.png"),
  plot = plot_match_success_rate_rel_outstanding_ind,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_rel_credit_limit_ind <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = FALSE,
    metric_type = "relative",
    match_success_type = "credit_limit",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_rel_credit_limit_individual.png"),
  plot = plot_match_success_rate_rel_credit_limit_ind,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

## plot relative match success rates for the aggregate loan book----
plot_match_success_rate_rel_n_agg <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = TRUE,
    metric_type = "relative",
    match_success_type = "n",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_rel_n_aggregate.png"),
  plot = plot_match_success_rate_rel_n_agg,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_rel_outstanding_agg <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = TRUE,
    metric_type = "relative",
    match_success_type = "outstanding",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_rel_outstanding_aggregate.png"),
  plot = plot_match_success_rate_rel_outstanding_agg,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_rel_credit_limit_agg <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = TRUE,
    metric_type = "relative",
    match_success_type = "credit_limit",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_rel_credit_limit_aggregate.png"),
  plot = plot_match_success_rate_rel_credit_limit_agg,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

## plot absolute match success rates for individual loan books----
plot_match_success_rate_abs_n_ind <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = FALSE,
    metric_type = "absolute",
    match_success_type = "n",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_abs_n_individual.png"),
  plot = plot_match_success_rate_abs_n_ind,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_abs_outstanding_ind <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = FALSE,
    metric_type = "absolute",
    match_success_type = "outstanding",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_abs_outstanding_individual.png"),
  plot = plot_match_success_rate_abs_outstanding_ind,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_abs_credit_limit_ind <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = FALSE,
    metric_type = "absolute",
    match_success_type = "credit_limit",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_abs_credit_limit_individual.png"),
  plot = plot_match_success_rate_abs_credit_limit_ind,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

## plot absolute match success rates for the aggregate loan book----
plot_match_success_rate_abs_n_agg <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = TRUE,
    metric_type = "absolute",
    match_success_type = "n",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_abs_n_aggregate.png"),
  plot = plot_match_success_rate_abs_n_agg,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_abs_outstanding_agg <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = TRUE,
    metric_type = "absolute",
    match_success_type = "outstanding",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_abs_outstanding_aggregate.png"),
  plot = plot_match_success_rate_abs_outstanding_agg,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)

plot_match_success_rate_abs_credit_limit_agg <- data_lbk_match_success_rate %>%
  plot_match_success_rate(
    aggregate = TRUE,
    metric_type = "absolute",
    match_success_type = "credit_limit",
    currency = "CAD"
  )

ggplot2::ggsave(
  filename = file.path(input_path_matched, "plot_match_success_rate_abs_credit_limit_aggregate.png"),
  plot = plot_match_success_rate_abs_credit_limit_agg,
  width = plot_width,
  height = plot_height,
  units = plot_units,
  dpi = plot_res
)
