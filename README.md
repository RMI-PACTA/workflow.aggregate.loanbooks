# workflow.aggregate.loanbooks
This repository produces analyses used to compare the loan books across many banks, including bulk PACTA runs and aggregation metrics including plots.

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This repository contains scripts that generate bulk PACTA for Banks analyses and run an aggregation of loan books for many groups or banks. It summarizes the aggregations in plots.
These types of analyses are developed for, but not restricted to, analyses of many financial institutions at once, e.g. in a supervisory context.

All third party data must be input by the user and is not part of this
repository.

File paths for input and output files are set up in the `.env` file as
explained below.

Scripts will be found at root level.

## Installation

The scripts depend on a number of PACTA related R packages, most of which can be found on CRAN. However, you will need to install the development version of `pacta.supervisor.analysis` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/pacta.supervisor.analysis")
```

## dotenv

A file called `.env` must be created in the root of this directory. The
file must have the following variables:

``` bash
# I/O directories
DIR_SCENARIO="PATH/TO/SCENARIO/FOLDER"
DIR_ABCD="PATH/TO/ABCD/FOLDER"
DIR_RAW="PATH/TO/RAW_LOANBOOK/FOLDER"
DIR_MATCHED="PATH/TO/MATCHED_LOANBOOK/FOLDER"
DIR_OUTPUT="PATH/TO/OUTPUT/FOLDER"

# input file names
FILENAME_SCENARIO_TMS="scenario_tms.csv"
FILENAME_SCENARIO_SDA="scenario_sda.csv"
FILENAME_REGIONS_GECO_2022="region_geco2022.csv"
FILENAME_REGIONS_WEO_2022="region_weo2022.csv"
FILENAME_ABCD="abcd_data.csv"

# project parameters
PARAM_SCENARIO_SOURCE="weo_2022"
PARAM_SCENARIO_SELECT="nze_2050"
PARAM_REGION_SELECT="global"
# normally the start year should correspond with year of the publication of the
# scenario in use
PARAM_START_YEAR=2022
# regions must be available for the selected scenario
PARAM_BENCHMARK_REGIONS="global,european union"
```

These configurations set up required directories (prefixed `DIR_`),
input files (prefixed `FILENAME_`) and project parameters (prefixed
`PARAM_`) which will all be used in the work flow. Please ensure to set
directories and file names that exist in your work environment and that
the paramters are consistent with the input files available to you.

This repository contains an `example.env` file, which you can use to
adjust the paths, file names and parameters to your environment and then
save as `.env` at the root level of the repository.

## Running the Analysis

Once you have set up the .env file correctly, you can simply run the
`run_aggregate_loanbooks.R` entirely to:

- set up project configurations
- load required input files
- prepare raw loan book data for batch processing in supervisor analysis
  (NOTE: that every separate raw loan book file in the raw data input
  file will be read in as a separate `group_id` This means that
  preparing the loan books as one csv per bank will split the analysis
  on a bank level. Adding one csv per loan book will split it on the
  loan book level and grouping the raw loan books in any other form in
  the csv files will propagate the corresponding grouping through the
  analysis. This enables slicing and dicing the analysis as required for
  each given context.)
- run a simplified version of the matching (NOTE: in a real project, you
  will have to follow the matching guidance as provided in the general
  PACTA for Banks user guide. There is currently no way you can around
  some manual matching or at least manual validation for obtaining
  reasonable PACTA for Banks results!)
- create a matched data set for calculations of benchmarks (no manual
  matching required)
- batch run PACTA for Banks TMS and SDA calculations for all groups
- generate all standard PACTA for Banks plots and ouput files for a
  given combination of `region` and `sector`
- prepare unweighted PACTA for Banks results at the company level as a
  preparatory step for calculating the alignment metrics.
- calculate alignment metrics both at the company level and the group
  level
- tweak the plot code to output supervisor-focused plots based on the
  alignment metrics, including:
  - Sankey plot of aligned/unaligned companies the groups are exposed to
  - Timeline plot that shows the forward-looking trend of the aligment
    metric over time (net, buildout and phaseout)
  - Scatter plot that allows for peer comparison of alignment metric
    across companies or groups (automotive and power sectors only)

