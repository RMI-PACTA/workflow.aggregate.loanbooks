# load packages----
library(dotenv)
library(dplyr)
# library(pacta.aggregate.loanbook.plots)
library(r2dii.analysis)
library(r2dii.data)
library(r2dii.match)
library(r2dii.plot)
library(readxl)
library(rlang)
library(tidyr)
library(vroom)

dotenv::load_dot_env()

# set up project paths----
if (file.exists(here::here(".env"))) {
  input_path_scenario <- Sys.getenv("DIR_SCENARIO")
  input_dir_abcd <- Sys.getenv("DIR_ABCD")
  input_path_raw <- Sys.getenv("DIR_RAW")
  input_path_matched <- Sys.getenv("DIR_MATCHED")

  input_path_regions_geco_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_GECO_2022"))
  input_path_regions_weo_2022 <- file.path(input_path_scenario, Sys.getenv("FILENAME_REGIONS_WEO_2022"))
  input_path_scenario_tms <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_TMS"))
  input_path_scenario_sda <- file.path(input_path_scenario, Sys.getenv("FILENAME_SCENARIO_SDA"))
  input_path_abcd <- file.path(input_dir_abcd, Sys.getenv("FILENAME_ABCD"))

  output_path <- Sys.getenv("DIR_OUTPUT")
  output_path_standard <- file.path(output_path, "standard")
  output_path_aggregated <- file.path(output_path, "aggregated")
} else {
  stop("Please set up a configuration file at the root of the repository, as
       explained in the README.md")
}

# set project parameters----
scenario_source_input <- Sys.getenv("PARAM_SCENARIO_SOURCE")
scenario_select <- Sys.getenv("PARAM_SCENARIO_SELECT")
region_select <- Sys.getenv("PARAM_REGION_SELECT")

# TODO: add check if all files exist, resort to test files if not

# TODO: remove the temp section once r2dii.data is updated
############# TEMP #############
# r2dii.data is not updated yet, so we manually update the region_isos data to
# cover the 2022 scenarios
regions_geco_2022 <- readr::read_csv(input_path_regions_geco_2022)
regions_weo_2022 <- readr::read_csv(input_path_regions_weo_2022)

region_isos_complete <- r2dii.data::region_isos %>%
  rbind(regions_geco_2022) %>%
  rbind(regions_weo_2022)
################################
# region_isos_complete <- r2dii.data::region_isos

region_isos_select <- region_isos_complete %>%
  dplyr::filter(
    .data$source == .env$scenario_source_input,
    .data$region %in% .env$region_select
  )

# load input data----
scenario_input_tms <- read.csv(input_path_scenario_tms)
scenario_input_sda <- read.csv(input_path_scenario_sda)

# abcd <- abcd_test_data
abcd <- readr::read_csv(file.path(input_path_abcd))
# replace potential NA values with 0 in production
abcd["production"][is.na(abcd["production"])] <- 0

# loanbook <- loanbook_test_data
loanbook <- purrr::map_dfr(list.files(input_path_raw, full.names = T), .f = vroom::vroom, id = "group_id")
# aggregation functions expect a group_id to be able to distinguish banks/loan books in later analysis
loanbook <- loanbook %>%
  dplyr::mutate(group_id = gsub(pattern = paste0(input_path_raw, "/"), replacement = "", x = .data$group_id)) %>%
  dplyr::mutate(group_id = gsub(pattern = ".csv", replacement = "", x = .data$group_id))

# match and prioritize loan book----
unique_loanbooks_raw <- unique(loanbook$group_id)

matched_loanbook <- NULL

for (i in unique_loanbooks_raw) {
  loanbook_i <- loanbook %>%
    dplyr::filter(.data$group_id == i)

  matched_i <- match_name(loanbook_i, abcd) %>%
    prioritize()

  matched_loanbook <- matched_loanbook %>%
    dplyr::bind_rows(matched_i)
}

# matched_loanbook %>%
#   readr::write_csv(file.path(input_path_matched, "matched_prio_all_groups.csv"))


# generate all P4B outputs----
unique_loanbooks_matched <- unique(matched_loanbook$group_id)

## generate SDA outputs----
results_sda_total <- NULL

for (i in unique_loanbooks_matched) {
  matched_i <- matched_loanbook %>%
    dplyr::filter(.data$group_id == i) %>%
    dplyr::select(-"group_id")

  results_sda_i <- matched_i %>%
    target_sda(
      abcd = abcd,
      co2_intensity_scenario = scenario_input_sda,
      region_isos = region_isos_select
    ) %>%
    dplyr::mutate(group_id = .env$i)

  results_sda_total <- results_sda_total %>%
    dplyr::bind_rows(results_sda_i)
}

results_sda_total %>%
  readr::write_csv(file.path(output_path_standard, "sda_results_all_groups.csv"))


## generate TMS outputs----

results_tms_total <- NULL

for (i in unique_loanbooks_matched) {
  matched_i <- matched_loanbook %>%
    dplyr::filter(.data$group_id == i) %>%
    dplyr::select(-"group_id")

  results_tms_i <- matched_i %>%
    target_market_share(
      abcd = abcd,
      scenario = scenario_input_tms,
      region_isos = region_isos_select
    ) %>%
    dplyr::mutate(group_id = .env$i)

  results_tms_total <- results_tms_total %>%
    dplyr::bind_rows(results_tms_i)
}

results_tms_total %>%
  readr::write_csv(file.path(output_path_standard, "tms_results_all_groups.csv"))

# generate P4B plots----

# results_tms_total <- readr::read_csv(file.path(output_path_standard, "tms_results_all_groups.csv"), col_types = readr::cols())
# results_sda_total <- readr::read_csv(file.path(output_path_standard, "sda_results_all_groups.csv"), col_types = readr::cols())
# matched_loanbook <- readr::read_csv("file.path(input_path_matched, "matched_prio_all_groups.csv"), col_types = readr::cols())

## retrieve set of unique groups to loop over----
unique_groups_tms <- unique(results_tms_total$group_id)
unique_groups_sda <- unique(results_sda_total$group_id)

## function to generate individual ouputs----
generate_individual_outputs <- function(data,
                                        matched_loanbook,
                                        output_directory,
                                        target_type = c("tms", "sda"),
                                        group_id,
                                        scenario_source,
                                        scenario,
                                        region = "global",
                                        sector) {

  # match input values
  target_type <- match.arg(target_type)

  target_scenario <- paste0("target_", scenario)

  # validate input values
  validate_input_args_generate_individual_outputs(
    output_directory = output_directory,
    group_id = group_id,
    scenario_source = scenario_source,
    target_scenario = target_scenario,
    region = region,
    sector = sector
  )

  # TODO: add again once generate_individual_outputs() is removed from pacta.aggregate.loanbook.plots
  #validate input data
  # validate_input_data_generate_individual_outputs(
  #   data = data,
  #   matched_loanbook = matched_loanbook,
  #   target_type = target_type
  # )

      # create sub directory for the selected institute
      dir.create(file.path(output_directory, group_id), showWarnings = FALSE)

      # set and derive some parameters
      start_year <- min(data$year, na.rm = TRUE)
      time_horizon <- 5

      data <- data %>%
        dplyr::filter(
          group_id == .env$group_id,
          scenario_source == .env$scenario_source,
          region == .env$region,
          sector %in% .env$sector
        )

      matched_loanbook <- matched_loanbook %>%
        dplyr::filter(
          group_id == .env$group_id,
          sector %in% .env$sector
        )

      if (target_type == "tms") {
        # plot tech mix for given sector
        data_techmix <- data %>%
          dplyr::filter(
            .data$metric %in% c("projected", "corporate_economy", .env$target_scenario),
            dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)
          )

        plot_techmix <- data_techmix %>%
          r2dii.plot::plot_techmix(
            span_5yr = TRUE,
            convert_label = r2dii.plot::recode_metric_techmix,
            convert_tech_label = r2dii.plot::spell_out_technology
          ) +
          ggplot2::labs(
            title = glue::glue("Technology Mix: {tools::toTitleCase(sector)}"),
            subtitle = glue::glue("Institution ID: {group_id}")
          )

        # export tech mix
        data_techmix %>%
          readr::write_csv(
            file = file.path(
              output_directory,
              group_id,
              glue::glue("data_tech_mix_{sector}.csv")
            )
          )

        ggplot2::ggsave(
          filename = glue::glue("plot_tech_mix_{sector}.png"),
          plot = plot_techmix,
          device = "png",
          path = file.path(output_directory, group_id)
        )

        # plot trajectory charts for all available techs in given sector
        technologies_in_sector <- r2dii.data::green_or_brown %>%
          dplyr::filter(.data$sector == .env$sector) %>%
          dplyr::pull(.data$technology)

        technologies_to_plot <- data %>%
          dplyr::filter(
            .data$metric == .env$target_scenario,
            .data$technology %in% .env$technologies_in_sector
          ) %>%
          dplyr::distinct(.data$technology) %>%
          dplyr::arrange(.data$technology) %>%
          dplyr::pull()

        for (i in 1:length(technologies_to_plot)) {
              data_trajectory <- data %>%
                dplyr::filter(
                  .data$technology == .env$technologies_to_plot[i],
                  dplyr::between(.data$year, .env$start_year, .env$start_year + .env$time_horizon)
                )

              plot_trajectory <- data_trajectory %>%
                r2dii.plot::plot_trajectory(
                  span_5yr = TRUE,
                  convert_label = r2dii.plot::recode_metric_trajectory,
                  center_y = TRUE,
                  value_col = "percentage_of_initial_production_by_scope",
                  perc_y_scale = TRUE
                ) +
                ggplot2::labs(
                  title = glue::glue("Valume Trajectory: {tools::toTitleCase(technologies_to_plot[i])}"),
                  subtitle = glue::glue("Institution ID: {group_id}")
                ) +
                ggplot2::xlab("Year") +
                ggplot2::ylab("Value")

              # export trajectory chart
              data_trajectory %>%
                readr::write_csv(
                  file = file.path(
                    output_directory,
                    group_id,
                    glue::glue("data_trajectory_{sector}_{technologies_to_plot[i]}.csv")
                  )
                )

              ggplot2::ggsave(
                filename = glue::glue("plot_trajectory_{sector}_{technologies_to_plot[i]}.png"),
                plot = plot_trajectory,
                device = "png",
                path = file.path(output_directory, group_id)
              )
        }
      } else {
        # plot convergence chart for given sector
        adjusted_scenario <- paste0("adjusted_scenario_", scenario)

        plot_emission_intensity <- data %>%
          dplyr::filter(
            .data$emission_factor_metric %in% c(
              "projected",
              "corporate_economy",
              .env$target_scenario,
              .env$adjusted_scenario
            )
          ) %>%
          r2dii.plot::plot_emission_intensity(
            span_5yr = FALSE,
            convert_label = r2dii.plot::to_title
          ) +
          ggplot2::labs(
            title = glue::glue("Emission Intensity - Convergence Approach: {tools::toTitleCase(sector)}"),
            subtitle = glue::glue("Institution ID: {group_id}")
          ) +
          ggplot2::xlab("Year") +
          ggplot2::ylab("Emission Factor Value")

        # export convergence chart
        data %>%
          dplyr::filter(
            .data$emission_factor_metric %in% c(
              "projected",
              "corporate_economy",
              .env$target_scenario,
              .env$adjusted_scenario
            )
          ) %>%
          readr::write_csv(
            file = file.path(
              output_directory,
              group_id,
              glue::glue("data_emission_intensity_{sector}.csv")
            )
          )

        ggplot2::ggsave(
          filename = glue::glue("plot_emission_intensity_{sector}.png"),
          plot = plot_emission_intensity,
          device = "png",
          path = file.path(output_directory, group_id)
        )
      }
      companies_included <- matched_loanbook %>%
        dplyr::select(
          "group_id", "name_abcd", "sector_abcd", "loan_size_outstanding",
          "loan_size_outstanding_currency", "loan_size_credit_limit",
          "loan_size_credit_limit_currency"
        )

      companies_included %>%
        readr::write_csv(
          file = file.path(
            output_directory,
            group_id,
            glue::glue("companies_included_{sector}.csv")
          )
        )

}

validate_input_args_generate_individual_outputs <- function(output_directory,
                                                            group_id,
                                                            scenario_source,
                                                            target_scenario,
                                                            region,
                                                            sector) {
  if (!length(output_directory) == 1) {
    stop("Argument output_directory must be of length 1. Please check your input.")
  }
  if (!inherits(output_directory, "character")) {
    stop("Argument output_directory must be of class character. Please check your input.")
  }
  if (!length(group_id) == 1) {
    stop("Argument group_id must be of length 1. Please check your input.")
  }
  if (!length(scenario_source) == 1) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!inherits(scenario_source, "character")) {
    stop("Argument scenario_source must be of length 1. Please check your input.")
  }
  if (!length(target_scenario) == 1) {
    stop("Argument target_scenario must be of length 1. Please check your input.")
  }
  if (!inherits(target_scenario, "character")) {
    stop("Argument target_scenario must be of length 1. Please check your input.")
  }
  if (!length(region) == 1) {
    stop("Argument region must be of length 1. Please check your input.")
  }
  if (!inherits(region, "character")) {
    stop("Argument region must be of length 1. Please check your input.")
  }
  if (!length(sector) == 1) {
    stop("Argument sector must be of length 1. Please check your input.")
  }
  if (!inherits(sector, "character")) {
    stop("Argument sector must be of length 1. Please check your input.")
  }

  invisible()
}

validate_input_data_generate_individual_outputs <- function(data,
                                                            matched_loanbook,
                                                            target_type) {
  if (target_type == "sda") {
    validate_data_has_expected_cols(
      data = data,
      expected_columns = c(
        "sector", "year", "region", "scenario_source", "emission_factor_metric",
        "emission_factor_value", "group_id"
      )
    )
  } else if (target_type == "tms") {
    validate_data_has_expected_cols(
      data = data,
      expected_columns = c(
        "sector", "technology", "year", "region", "scenario_source", "metric",
        "production", "technology_share", "scope",
        "percentage_of_initial_production_by_scope", "group_id"
      )
    )
  }

  validate_data_has_expected_cols(
    data = matched_loanbook,
    expected_columns = c(
      "group_id", "name_abcd", "sector", "sector_abcd", "loan_size_outstanding",
      "loan_size_outstanding_currency", "loan_size_credit_limit",
      "loan_size_credit_limit_currency"
    )
  )

  invisible()
}


## run automatic result generation ----------

### automotive----
sector_select <- "automotive"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}
### coal----
sector_select <- "coal"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}
### oil and gas----
sector_select <- "oil and gas"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}
### power----
sector_select <- "power"
for (tms_i in unique_groups_tms) {
  available_rows <- results_tms_total %>%
    dplyr::filter(
      group_id == tms_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_tms_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "tms",
      group_id = tms_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}

### aviation----
sector_select <- "aviation"
for (sda_i in unique_groups_sda) {
  available_rows <- results_sda_total %>%
    dplyr::filter(
      group_id == sda_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$emission_factor_metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_sda_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "sda",
      group_id = sda_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}
### cement----
sector_select <- "cement"
for (sda_i in unique_groups_sda) {
  available_rows <- results_sda_total %>%
    dplyr::filter(
      group_id == sda_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$emission_factor_metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_sda_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "sda",
      group_id = sda_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}
### steel----
sector_select <- "steel"
for (sda_i in unique_groups_sda) {
  available_rows <- results_sda_total %>%
    dplyr::filter(
      group_id == sda_i,
      scenario_source == scenario_source_input,
      grepl(scenario_select, .data$emission_factor_metric),
      region == region_select,
      sector == sector_select
    ) %>%
    nrow()
  if (available_rows > 0) {
    generate_individual_outputs(
      data = results_sda_total,
      matched_loanbook = matched_loanbook,
      output_directory = output_path_standard,
      target_type = "sda",
      group_id = sda_i,
      scenario_source = scenario_source_input,
      scenario = scenario_select,
      region = region_select,
      sector = sector_select
    )
  } else {
    next()
  }
}
