# load packages----
library(dotenv)
library(dplyr)
library(pacta.aggregate.loanbook.plots)
library(r2dii.analysis)
library(r2dii.data)
library(r2dii.match)
library(r2dii.plot)
library(readxl)
library(rlang)
library(tidyr)
library(vroom)

dotenv::load_dot_env()
source("expected_columns.R")
source("R/functions_prep_project.R")
source("R/plots.R")

# set up project----
if (file.exists(here::here(".env"))) {
  # paths
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_matched <- Sys.getenv("DIR_MATCHED")
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))
  output_path <- Sys.getenv("DIR_OUTPUT")
  output_path_aggregated <- file.path(output_path, "aggregated")

  # project parameters
  scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
  scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
  region_select <- Sys.getenv("PARAM_REGION_SELECT")
  start_year <- as.numeric(Sys.getenv("PARAM_START_YEAR"))
  time_frame_select <- as.integer(Sys.getenv("PARAM_TIME_FRAME"))
  # benchmark_regions <- unlist(base::strsplit(Sys.getenv("PARAM_BENCHMARK_REGIONS"), ","))
  apply_sector_split <- as.logical(Sys.getenv("APPLY_SECTOR_SPLIT"))
  if (is.na(apply_sector_split)) {apply_sector_split <- FALSE}
  if (apply_sector_split) {sector_split_type_select <- Sys.getenv("SECTOR_SPLIT_TYPE")}
  remove_inactive_companies <- as.logical(Sys.getenv("REMOVE_INACTIVE_COMPANIES"))
  if (is.na(remove_inactive_companies)) {remove_inactive_companies <- FALSE}

  # if a sector split is applied, write results into a directory that states the type
  if (apply_sector_split) {
    output_path_aggregated <- file.path(output_path, sector_split_type_select, "aggregated")
  }

  dir.create(output_path_aggregated, recursive = TRUE)

} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# load required data----

## asset based company data----
abcd <- readr::read_csv(
  file.path(input_dir_abcd, "abcd_final_for_plots.csv"),
  col_types = col_types_abcd,
  col_select = dplyr::all_of(col_select_abcd)
)

## matched loanbook data----
matched_prioritized <- readr::read_csv(
  file.path(input_path_matched, "matched_prioritized_final_for_plots.csv"),
  col_types = col_types_matched_prio_all_groups,
  col_select = dplyr::all_of(col_select_matched_prio_all_groups)
)

## company level results----
company_aggregated_alignment_net_tms  <-
  readr::read_csv(file.path(output_path_aggregated, "company_aggregated_alignment_net_tms.csv"))

company_aggregated_alignment_net_sda <-
  readr::read_csv(file.path(output_path_aggregated, "company_aggregated_alignment_net_sda.csv"))

company_aggregated_alignment_bo_po_tms <-
  readr::read_csv(file.path(output_path_aggregated, "company_aggregated_alignment_bo_po_tms.csv"))

## loanbook level results----
loanbook_exposure_aggregated_alignment_bo_po <-
  readr::read_csv(file.path(output_path_aggregated, "loanbook_exposure_aggregated_alignment_bo_po.csv"))

loanbook_exposure_aggregated_alignment_net <-
  readr::read_csv(file.path(output_path_aggregated, "loanbook_exposure_aggregated_alignment_net.csv"))

# generate plots for system-wide analysis----

# Plot sankey plot of financial flows scenario alignment - examples

if (!is.null(company_aggregated_alignment_net_tms)) {
  data_sankey_sector_tms <- prep_sankey(
    company_aggregated_alignment_net_tms,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "sector"
  )
} else {
  data_sankey_sector_tms <- NULL
}

if (!is.null(company_aggregated_alignment_net_sda)) {
  data_sankey_sector_sda <- prep_sankey(
    company_aggregated_alignment_net_sda,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "sector"
  )
} else {
  data_sankey_sector_sda <- NULL
}

data_sankey_sector <- rbind(data_sankey_sector_tms, data_sankey_sector_sda)

if (!is.null(data_sankey_sector)) {
  data_sankey_sector %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_sankey_sector.csv"
      )
    )

  plot_sankey(
    data_sankey_sector,
    save_png_to = output_path_aggregated,
    png_name = "plot_sankey_sector.png",
    nodes_order_from_data = TRUE
  )
}

if (!is.null(company_aggregated_alignment_net_tms)) {
  data_sankey_company_sector_tms <- prep_sankey(
    company_aggregated_alignment_net_tms,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "name_abcd",
    middle_node2 = "sector"
  )
} else {
  data_sankey_company_sector_tms <- NULL
}

if (!is.null(company_aggregated_alignment_net_sda)) {
  data_sankey_company_sector_sda <- prep_sankey(
    company_aggregated_alignment_net_sda,
    matched_prioritized,
    region = "global",
    year = 2027,
    middle_node = "name_abcd",
    middle_node2 = "sector"
  )
} else {
  data_sankey_company_sector_sda <- NULL
}

data_sankey_company_sector <- rbind(
  data_sankey_company_sector_tms,
  data_sankey_company_sector_sda
)

if (!is.null(data_sankey_company_sector)) {
  data_sankey_company_sector %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_sankey_company_sector.csv"
      )
    )

  plot_sankey(
    data_sankey_company_sector,
    save_png_to = output_path_aggregated,
    png_name = "plot_sankey_company_sector.png"
  )
}

### scatter plot alignment by exposure and sector comparison----
year_scatter_alignment_exposure <- 2027
region_scatter_alignment_exposure <- region_select
# TODO: this should come from the loan book
currency <- unique(matched_prioritized$loan_size_outstanding_currency)
category <- "group_id"

if (
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_alignment_exposure <- loanbook_exposure_aggregated_alignment_net %>%
    prep_scatter_alignment_exposure(
      year = year_scatter_alignment_exposure,
      region = region_scatter_alignment_exposure,
      scenario = scenario_select,
      category = category,
      exclude_group_ids = "benchmark"
    )

  data_scatter_alignment_exposure %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_alignment_exposure.csv"
      )
    )

  plot_scatter_alignment_exposure <- data_scatter_alignment_exposure %>%
    plot_scatter_alignment_exposure(
      floor_outliers = -1,
      cap_outliers = 1,
      category = category,
      currency = currency
    )

  ggplot2::ggsave(
    filename = "plot_scatter_alignment_exposure.png",
    path = output_path_aggregated,
    width = 8,
    height = 5,
    dpi = 300,
    units = "in",
  )
}

### scatter plot for group level comparison----
year_scatter <- 2027
region_scatter <- region_select
data_level_group <- "bank"
# automotive
sector_scatter <- "automotive"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_automotive_group <- prep_scatter(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_automotive_group %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_automotive_group.csv"
      )
    )

  plot_scatter(
    data_scatter_automotive_group,
    data_level = data_level_group,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select
  )
  ggplot2::ggsave(
    filename = "plot_scatter_automotive_group.png",
    path = output_path_aggregated,
    width = 8,
    height = 5
  )
}

# power
sector_scatter <- "power"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_power_group <- prep_scatter(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_power_group %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_power_group.csv"
      )
    )

  plot_scatter(
    data_scatter_power_group,
    data_level = data_level_group,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select
  )

  ggplot2::ggsave(
    filename = "plot_scatter_power_group.png",
    path = output_path_aggregated,
    width = 8,
    height = 5
  )
}

### animated scatter plot for group level comparison----
region_scatter <- region_select
data_level_group <- "bank"
# automotive
sector_scatter <- "automotive"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_automotive_group_a <- prep_scatter_animated(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_automotive_group_a %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_automotive_group_animated.csv"
      )
    )

  plot_scatter_automotive_group_a <- plot_scatter_animated(
    data_scatter_automotive_group_a,
    sector = sector_scatter,
    data_level = data_level_group,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    alignment_limit = 1
  )

  htmlwidgets::saveWidget(
    plot_scatter_automotive_group_a,
    file = file.path(output_path_aggregated, "plot_scatter_automotive_group_animated.html")
  )
}

# power
sector_scatter <- "power"
if (
  nrow(loanbook_exposure_aggregated_alignment_bo_po) > 0 &
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_power_group_a <- prep_scatter_animated(
    loanbook_exposure_aggregated_alignment_bo_po,
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_group
  )

  data_scatter_power_group_a %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        "data_scatter_power_group_animated.csv"
      )
    )

  plot_scatter_power_group_a <- plot_scatter_animated(
    data_scatter_power_group_a,
    sector = sector_scatter,
    data_level = data_level_group,
    region = region_scatter,
    scenario_source = scenario_source_input,
    scenario = scenario_select,
    alignment_limit = 1
  )

  htmlwidgets::saveWidget(
    plot_scatter_power_group_a,
    file = file.path(output_path_aggregated, "plot_scatter_power_group_animated.html")
  )
}

# group level plots ----
### timeline plot: evolution of portfolio-weighted alignment over time----

region_timeline <- region_select
# build-out / phase-out for automotive
sector_timeline <- "automotive"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_bo_po %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  dir.create(file.path(output_path_aggregated, i), recursive = TRUE, showWarnings = FALSE)
}

for (i in unique_loanbook_group_id) {
  data_timeline_automotive <- prep_timeline(
    loanbook_exposure_aggregated_alignment_bo_po,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if(nrow(data_timeline_automotive) > 0) {
    data_timeline_automotive %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_bopo_automotive.csv"
        )
      )

    plot_timeline(
      data_timeline_automotive,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_bopo_automotive.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

# build-out / phase-out for power
sector_timeline <- "power"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_bo_po %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_power <- prep_timeline(
    loanbook_exposure_aggregated_alignment_bo_po,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_power) > 0) {
    data_timeline_power %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_bopo_power.csv"
        )
      )

    plot_timeline(
      data_timeline_power,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_bopo_power.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for automotive
sector_timeline <- "automotive"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_automotive <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_automotive) > 0) {
    data_timeline_automotive %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_automotive.csv"
        )
      )

    plot_timeline(
      data_timeline_automotive,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_automotive.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for coal
sector_timeline <- "coal"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_coal <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_coal > 0)) {
    data_timeline_coal %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_coal.csv"
        )
      )

    plot_timeline(
      data_timeline_coal,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_coal.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for oil & gas
sector_timeline <- "oil and gas"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_oil_and_gas <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_oil_and_gas) > 0) {
    data_timeline_oil_and_gas %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_oil_and_gas.csv"
        )
      )

    plot_timeline(
      data_timeline_oil_and_gas,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_oil_and_gas.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for power
sector_timeline <- "power"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_power <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_power) > 0) {
    data_timeline_power %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_power.csv"
        )
      )

    plot_timeline(
      data_timeline_power,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_power.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for aviation
sector_timeline <- "aviation"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_aviation <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_aviation) > 0) {
    data_timeline_aviation %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_aviation.csv"
        )
      )

    plot_timeline(
      data_timeline_aviation,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_aviation.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for cement
sector_timeline <- "cement"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_cement <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_cement) > 0) {
    data_timeline_cement %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_cement.csv"
        )
      )

    plot_timeline(
      data_timeline_cement,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_cement.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

# net aggregate alignment for steel
sector_timeline <- "steel"

unique_loanbook_group_id <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    .data$sector == .env$sector_timeline,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_timeline_steel <- prep_timeline(
    loanbook_exposure_aggregated_alignment_net,
    sector = sector_timeline,
    region = region_timeline,
    group_ids_to_plot = i)

  if (nrow(data_timeline_steel) > 0) {
    data_timeline_steel %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_timeline_net_steel.csv"
        )
      )

    plot_timeline(
      data_timeline_steel,
      sector = sector_timeline,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_timeline
    )

    ggplot2::ggsave(
      filename = "plot_timeline_net_steel.png",
      path = file.path(output_path_aggregated, i),
      width = 7,
      height = 5
    )
  } else {
    next()
  }
}

### scatter plot for company level comparison----

# all excluding outliers
# for all companies per group, not all companies across groups

# company level, excluding outliers
year_scatter <- 2027
region_scatter <- region_select
data_level_company <- "company"

# automotive
sector_scatter <- "automotive"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_automotive_company_i <- prep_scatter(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    group_ids_to_plot = i,
    data_level = data_level_company
  )

  if (nrow(data_scatter_automotive_company_i) > 0) {
    data_scatter_automotive_company_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_automotive_company.csv"
        )
      )

    plot_scatter(
      data_scatter_automotive_company_i,
      data_level = data_level_company,
      year = year_scatter,
      sector = sector_scatter,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      cap_outliers = 2,
      floor_outliers = -2
    )

    ggplot2::ggsave(
      filename = "plot_scatter_automotive_company.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

# power
sector_scatter <- "power"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_power_company_i <- prep_scatter(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    year = year_scatter,
    sector = sector_scatter,
    region = region_scatter,
    group_ids_to_plot = i,
    data_level = data_level_company
  )

  if (nrow(data_scatter_power_company_i) > 0) {
    data_scatter_power_company_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_power_company.csv"
        )
      )

    plot_scatter(
      data_scatter_power_company_i,
      data_level = data_level_company,
      year = year_scatter,
      sector = sector_scatter,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      cap_outliers = 2,
      floor_outliers = -2
    )

    ggplot2::ggsave(
      filename = "plot_scatter_power_company.png",
      path = file.path(output_path_aggregated, i),
      width = 8,
      height = 5
    )
  } else {
    next()
  }
}

### animated scatter plot for company level comparison----

# for all companies per group, not all companies across groups

# automotive
sector_scatter <- "automotive"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_automotive_company_animated_i <- prep_scatter_animated(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_company,
    group_ids_to_plot = i
  )

  if (nrow(data_scatter_automotive_company_animated_i) > 0) {
    data_scatter_automotive_company_animated_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_automotive_company_animated.csv"
        )
      )

    plot_scatter_animated_i <- plot_scatter_animated(
      data_scatter_automotive_company_animated_i,
      sector = sector_scatter,
      data_level = data_level_company,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      floor_outliers = -1.5,
      cap_outliers = 1.5
    )

    htmlwidgets::saveWidget(
      plot_scatter_animated_i,
      file = file.path(output_path_aggregated, i, "plot_scatter_automotive_company_animated.html")
    )
  } else {
    next()
  }
}

# power
sector_scatter <- "power"

unique_loanbook_group_id <- company_aggregated_alignment_bo_po_tms %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_loanbook_group_id) {
  data_scatter_power_company_animated_i <- prep_scatter_animated(
    company_aggregated_alignment_bo_po_tms,
    company_aggregated_alignment_net_tms,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_company,
    group_ids_to_plot = i
  )

  if (nrow(data_scatter_power_company_animated_i) > 0) {
    data_scatter_power_company_animated_i %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          i,
          "data_scatter_power_company_animated.csv"
        )
      )

    plot_scatter_animated_i <- plot_scatter_animated(
      data_scatter_power_company_animated_i,
      sector = sector_scatter,
      data_level = data_level_company,
      region = region_scatter,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      floor_outliers = -1.5,
      cap_outliers = 1.5
    )

    htmlwidgets::saveWidget(
      plot_scatter_animated_i,
      file = file.path(output_path_aggregated, i, "plot_scatter_power_company_animated.html")
    )
  } else {
    next()
  }
}
