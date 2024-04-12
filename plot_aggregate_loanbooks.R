# load packages----
library(dotenv)
library(dplyr)
library(pacta.multi.loanbook.plot)
library(r2dii.analysis)
library(r2dii.data)
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
  input_path_matched <- Sys.getenv("DIR_MATCHED")
  output_path <- Sys.getenv("DIR_OUTPUT")
  output_path_aggregated <- file.path(output_path, "aggregated")

  # project parameters
  scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
  scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
  region_select <- Sys.getenv("PARAM_REGION_SELECT")
  start_year <- as.numeric(Sys.getenv("PARAM_START_YEAR"))
  time_frame_select <- as.integer(Sys.getenv("PARAM_TIME_FRAME"))
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

# TODO: this script should take flexible grouping
# TODO: check this works appropriately for meta level / by_group = NULL
# TODO: flexible grouping on plot side still open for sankey and for animated scatter
# by_groups <- "group_id"
by_groups <- "foo"

## company level results----
company_aggregated_alignment_net  <-
  readr::read_csv(
    file = file.path(
      output_path_aggregated,
      glue::glue("company_exposure_net_aggregate_alignment_by_{paste(by_groups, collapse = \"_\")}.csv")
    )
  )

company_aggregated_alignment_bo_po <-
  readr::read_csv(
    file = file.path(
      output_path_aggregated,
      glue::glue("company_exposure_bo_po_aggregate_alignment_by_{paste(by_groups, collapse = \"_\")}.csv")
    )
  )

## loanbook level results----
loanbook_exposure_aggregated_alignment_bo_po <-
  readr::read_csv(
    file = file.path(
      output_path_aggregated,
      glue::glue("loanbook_exposure_bo_po_aggregate_alignment_by_{paste(by_groups, collapse = \"_\")}.csv")
    )
  )

loanbook_exposure_aggregated_alignment_net <-
  readr::read_csv(
    file = file.path(
      output_path_aggregated,
      glue::glue("loanbook_exposure_net_aggregate_alignment_by_{paste(by_groups, collapse = \"_\")}.csv")
    )
  )


# generate plots for system-wide analysis----
### sankey plot----
# TODO: when benchmarks get re-introduced, they need to be removed here
# Plot sankey plot of financial flows scenario alignment - examples
if (!is.null(company_aggregated_alignment_net)) {
  data_sankey_sector <- prep_sankey(
    company_aggregated_alignment_net,
    region = "global",
    year = 2027,
    middle_node = "sector"
  )
} else {
  data_sankey_sector <- NULL
}

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

if (!is.null(company_aggregated_alignment_net)) {
  data_sankey_company_sector <- prep_sankey(
    company_aggregated_alignment_net,
    region = "global",
    year = 2027,
    middle_node = "name_abcd",
    middle_node2 = "sector"
  )
} else {
  data_sankey_company_sector <- NULL
}

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
currency <- unique(company_aggregated_alignment_net$loan_size_outstanding_currency)
if (length(by_groups) > 1) {
  category <- by_groups[[1]]
  print(
    glue::glue("Scatter plot exposure by alignment cannot process more than one by_group at a time. Using the first group: {by_groups[[1]]}")
  )
} else {
  category <- by_groups
}

if (
  nrow(loanbook_exposure_aggregated_alignment_net) > 0
) {
  data_scatter_alignment_exposure <- loanbook_exposure_aggregated_alignment_net %>%
    prep_scatter_alignment_exposure(
      year = year_scatter_alignment_exposure,
      region = region_scatter_alignment_exposure,
      scenario = scenario_select,
      category = category,
      exclude_groups = "benchmark"
    )

  data_scatter_alignment_exposure %>%
    readr::write_csv(
      file = file.path(
        output_path_aggregated,
        glue::glue("data_scatter_alignment_exposure_{paste(by_groups, collapse = \"_\")}.csv")
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
    filename = glue::glue("plot_scatter_alignment_exposure_{paste(by_groups, collapse = \"_\")}.png"),
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
if (length(by_groups) <= 1) {
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
      by_group = by_groups,
      data_level = data_level_group
    )

    data_scatter_automotive_group %>%
      readr::write_csv(
        file = file.path(
          output_path_aggregated,
          glue::glue("data_scatter_automotive_by_{by_groups}.csv")
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
      filename = glue::glue("plot_scatter_automotive_by_{by_groups}.png"),
      path = output_path_aggregated,
      width = 8,
      height = 5
    )
  }
} else {
  print(
    glue::glue("Scatter plot BO/PO cannot process more than one by_group at a time. Skipping!")
  )
}

# power
sector_scatter <- "power"
if (length(by_groups) <= 1) {
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
      by_group = by_groups,
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
} else {
  print(
    glue::glue("Scatter plot BO/PO cannot process more than one by_group at a time. Skipping!")
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
# create sub directories for each relevant group.
# TODO: Note that this implies that no groups across different .by variables
# should have the same values, as this will confuse output directories

dirs_for_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
  dplyr::filter(
    !grepl("benchmark_corporate_economy_", !!rlang::sym(by_groups))
  ) %>%
  dplyr::pull(!!rlang::sym(by_groups)) %>%
  unique()

for (i in dirs_for_by_groups) {
  dir.create(file.path(output_path_aggregated, i), recursive = TRUE, showWarnings = FALSE)
}


### timeline plot: evolution of portfolio-weighted alignment over time----

region_timeline <- region_select
# build-out / phase-out for automotive
sector_timeline <- "automotive"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_bo_po %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", !!rlang::sym(by_groups))
    ) %>%
    dplyr::pull(!!rlang::sym(by_groups)) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_automotive <- prep_timeline(
      loanbook_exposure_aggregated_alignment_bo_po,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if(nrow(data_timeline_automotive) > 0) {
      data_timeline_automotive %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_bopo_automotive_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_automotive,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_bopo_automotive_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 8,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# build-out / phase-out for power
sector_timeline <- "power"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_bo_po %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_power <- prep_timeline(
      loanbook_exposure_aggregated_alignment_bo_po,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_power) > 0) {
      data_timeline_power %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_bopo_power_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_power,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_bopo_power_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 8,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for automotive
sector_timeline <- "automotive"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_automotive <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_automotive) > 0) {
      data_timeline_automotive %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_automotive_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_automotive,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_automotive_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for coal
sector_timeline <- "coal"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_coal <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_coal > 0)) {
      data_timeline_coal %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_coal_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_coal,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_coal_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for oil & gas
sector_timeline <- "oil and gas"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_oil_and_gas <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_oil_and_gas) > 0) {
      data_timeline_oil_and_gas %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_oil_and_gas_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_oil_and_gas,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_oil_and_gas_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for power
sector_timeline <- "power"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_power <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_power) > 0) {
      data_timeline_power %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_power_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_power,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_power_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for aviation
sector_timeline <- "aviation"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_aviation <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_aviation) > 0) {
      data_timeline_aviation %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_aviation_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_aviation,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_aviation_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for cement
sector_timeline <- "cement"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_cement <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_cement) > 0) {
      data_timeline_cement %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_cement_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_cement,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_cement_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
}

# net aggregate alignment for steel
sector_timeline <- "steel"

if (length(by_groups) <= 1) {
  unique_by_groups <- loanbook_exposure_aggregated_alignment_net %>%
    dplyr::filter(
      .data$sector == .env$sector_timeline,
      !grepl("benchmark_corporate_economy_", .data$group_id)
    ) %>%
    dplyr::pull(.data$group_id) %>%
    unique()

  for (i in unique_by_groups) {
    data_timeline_steel <- prep_timeline(
      loanbook_exposure_aggregated_alignment_net,
      sector = sector_timeline,
      region = region_timeline,
      by_group = by_groups,
      groups_to_plot = i)

    if (nrow(data_timeline_steel) > 0) {
      data_timeline_steel %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_timeline_net_steel_by_{by_groups}_{i}.csv")
          )
        )

      plot_timeline(
        data_timeline_steel,
        sector = sector_timeline,
        scenario_source = scenario_source_input,
        scenario = scenario_select,
        region = region_timeline,
        by_group = by_groups
      )

      ggplot2::ggsave(
        filename = glue::glue("plot_timeline_net_steel_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 7,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print("Timeline plot cannot process more than one by_group at a time. Skipping!")
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

if (length(by_groups) <= 1) {
  unique_by_groups <- company_aggregated_alignment_bo_po %>%
    dplyr::filter(
      .data$sector == .env$sector_scatter,
      !grepl("benchmark_corporate_economy_", !!rlang::sym(by_groups))
    ) %>%
    dplyr::pull(!!rlang::sym(by_groups)) %>%
    unique()

  for (i in unique_by_groups) {
    data_scatter_automotive_company_i <- prep_scatter(
      company_aggregated_alignment_bo_po,
      company_aggregated_alignment_net,
      year = year_scatter,
      sector = sector_scatter,
      region = region_scatter,
      by_group = by_groups,
      groups_to_plot = i,
      data_level = data_level_company
    )

    if (nrow(data_scatter_automotive_company_i) > 0) {
      data_scatter_automotive_company_i %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_scatter_automotive_company_by_{by_groups}_{i}.csv")
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
        filename = glue::glue("plot_scatter_automotive_company_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 8,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print(
    glue::glue("Scatter plot BO/PO cannot process more than one by_group at a time. Skipping!")
  )
}

# power
sector_scatter <- "power"

if (length(by_groups) <= 1) {
  unique_by_groups <- company_aggregated_alignment_bo_po %>%
    dplyr::filter(
      .data$sector == .env$sector_scatter,
      !grepl("benchmark_corporate_economy_", !!rlang::sym(by_groups))
    ) %>%
    dplyr::pull(!!rlang::sym(by_groups)) %>%
    unique()

  for (i in unique_by_groups) {
    data_scatter_power_company_i <- prep_scatter(
      company_aggregated_alignment_bo_po,
      company_aggregated_alignment_net,
      year = year_scatter,
      sector = sector_scatter,
      region = region_scatter,
      by_group = by_groups,
      groups_to_plot = i,
      data_level = data_level_company
    )

    if (nrow(data_scatter_power_company_i) > 0) {
      data_scatter_power_company_i %>%
        readr::write_csv(
          file = file.path(
            output_path_aggregated,
            i,
            glue::glue("data_scatter_power_company_by_{by_groups}_{i}.csv")
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
        filename = glue::glue("plot_scatter_power_company_by_{by_groups}_{i}.png"),
        path = file.path(output_path_aggregated, i),
        width = 8,
        height = 5
      )
    } else {
      next()
    }
  }
} else {
  print(
    glue::glue("Scatter plot BO/PO cannot process more than one by_group at a time. Skipping!")
  )
}

### animated scatter plot for company level comparison----

# for all companies per group, not all companies across groups

# automotive
sector_scatter <- "automotive"

unique_by_groups <- company_aggregated_alignment_bo_po %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_by_groups) {
  data_scatter_automotive_company_animated_i <- prep_scatter_animated(
    company_aggregated_alignment_bo_po,
    company_aggregated_alignment_net,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_company,
    groups_to_plot = i
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

unique_by_groups <- company_aggregated_alignment_bo_po %>%
  dplyr::filter(
    .data$sector == .env$sector_scatter,
    !grepl("benchmark_corporate_economy_", .data$group_id)
  ) %>%
  dplyr::pull(.data$group_id) %>%
  unique()

for (i in unique_by_groups) {
  data_scatter_power_company_animated_i <- prep_scatter_animated(
    company_aggregated_alignment_bo_po,
    company_aggregated_alignment_net,
    sector = sector_scatter,
    region = region_scatter,
    data_level = data_level_company,
    groups_to_plot = i
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
