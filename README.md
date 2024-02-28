# workflow.aggregate.loanbooks <img src="man/figures/logo.png" align="right" width="120" />
This repository produces analyses used to compare the loan books across many banks, including bulk PACTA runs and aggregation metrics including plots.

It also contains a script that can be used to derive production-based sector split values for companies that are active across multiple energy related in-scope PACTA sectors.

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

The scripts depend on a number of PACTA related R packages, most of which can be found on CRAN. However, you will need to install the development version of `pacta.aggregate.loanbook.plots` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/pacta.aggregate.loanbook.plots")
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
FILENAME_ABCD="abcd_data.csv"

# project parameters
PARAM_SCENARIO_SOURCE="weo_2022"
PARAM_SCENARIO_SELECT="nze_2050"
PARAM_REGION_SELECT="global"
# normally the start year should correspond with year of the publication of the
# scenario in use
PARAM_START_YEAR=2022
PARAM_TIME_FRAME=5
# regions must be available for the selected scenario
PARAM_BENCHMARK_REGIONS="global,european union"
REMOVE_INACTIVE_COMPANIES=TRUE
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

### Optional: Use own sector classification system for matching loan books

You can optionally use your own sector classification system for matching loan
books. This can be useful if you primarily use a sector classification system
that is not featured in `r2dii.data` but that maps well to the PACTA sectors.

Instead of mapping the entire loan book to a sector classification system that
is inclued in `r2dii.data`, you can simply provide a bridge file. In order to
use this file, you will need to set the following variables in the `.env` file:

``` bash
# use own sector classification system
USE_OWN_SECTOR_CLASSIFICATION=FALSE
DIR_OWN_SECTOR_CLASSIFICATION="PATH/TO/SECTOR_CLASSIFICATION/FOLDER"
FILENAME_OWN_SECTOR_CLASSIFICATION="own_sector_classification.csv"
```

### Optional: Calculate sector splits for multi-sector companies

You can optionally generate a sector split file. This can be used to split the
loan exposure of a counterparty across the in-scope sectors the company operates
in. The way the split is calculated can be adjusted and should be informed by
the particular use case / research question to be answered.

Currently, two basic and one advanced variants of the sector split exist. The
basic variants are (1) an equal weights split, which allocates the loan equally
among the sectors a counterparty operates in and (2) a worst case split, which
allocates the entire loan to the sector that is most misaligned for the given
counterparty.

An advanced variant calculates an activity-based sector split for companies that
have at least two energy-related main business lines withing PACTA scope (at
least two of: coal mining, upstream oil & gas, power generation). This sector
split is meant to help with allocating portions of a loan to each of the
relevant business lines. As such, it allows for considering more than one main
sector. This improves coverage of transition activities and reflects better the
multi-sector focus of some companies.

To get this sector split, you will need some additional input files:

- a csv containing one column `company_id` that lists the companies you
  would like to get the activity-based sector split for.
- an `advanced_company_indicators` file that includes activity units for
  all three in-scope energy sectors, where the units must be tons of
  coal for coal mining, GJ for upstream oil & gas and MWh for power
  generation.

You will then need to add an additional section to the `.env` file used
for the configuration of the analysis:

``` bash
# parameters for company sector split
APPLY_SECTOR_SPLIT=FALSE
SECTOR_SPLIT_TYPE="equal_weights"
DIR_SPLIT_COMPANY_ID="PATH/TO/SPLIT/FOLDER"
FILENAME_SPLIT_COMPANY_ID="split_company_ids.csv"
DIR_ADVANCED_COMPANY_INDICATORS="PATH/TO/ADVANCED_COMP_INDICATORS/FOLDER"
FILENAME_ADVANCED_COMPANY_INDICATORS="advanced_company_indicators.xlsx"
```

You can now get the company sector split by running the root level
script `prep_sector_split_energy_companies.R`, which will output the
file `companies_sector_split.csv` into the matched directory as set up
in `.env`.

### Usage of the sector split
When companies operate in multiple sectors within the PACTA scope the following rules can be used to split the loan value between the different sectors:

**Rule 1:**

- Case description: Secondary business operation is in the PACTA scope but only supportive to the main business line.

- Outcome: The secondary business operation is not to be included in the analysis.

**Rule 2:**

- Case description: Multiple business operations are in the PACTA scope and are considered main business lines.

   **Rule 2a)**

  - Case description: Multiple business operations are in the PACTA scope and in non-energy related sectors.

  - Outcome: Loans are to be split evenly by the number of sectors.

   **Rule 2b)**

  - Case description: Multiple business operations are in the PACTA scope and are in energy-related sectors (Oil & Gas, Coal, power).

  - Outcome: Loans are to be split based on a common primary energy production unit (tons of oil equivalent).
  
   **Rule 2c)**

  - Case description: Multiple business operations are in the PACTA scope and are found both in in energy-related sectors (Oil & Gas, Coal, power) and non-energy related sectors.

  - Outcome: Loans are to be split evenly by the number of sectors. For the energy sectors, the remaining share after evenly splitting the loan is then allocated based on a common primary energy production unit (tons of oil equivalent).


## Methodological note: Sector split for energy companies

Where a company has activities in multiple energy-related sectors, a common output unit of primary energy is needed to compare quantities across sectors. The chosen common unit of primary energy is million tons of oil equivalent (Mtoe) and is converted for the respective sectors as follows:

- coal mining sector is converted from metric tonnes of coal (t coal)
- upstream oil & gas is converted from gigajoules (GJ)
- power generation is converted from megawatt hours (MWh)

A methodological distinction between fossil fuel-based high carbon power generation and fossil-free low carbon power generation is made:

- In order to compare the power generation sector to the upstream fossil fuel extractive sectors a further conversion is needed to account for the primary energy efficiency of fossil fuel-based power generation. This is because a large proportion of the thermal energy from burning fuel is not converted into electricity. This loss is taken into account by using primary energy efficiency factors for the respective technologies in the power sector.
- This step is not required for low carbon technologies because even though some have relatively low primary energy efficiencies (e.g. geothermal power at 10%) the input energy is not a fossil fuel and so from an accounting point of view does not contribute to the exposure of a company to fossil fuel production and use.

It follows that to calculate the primary energy use ($E$) for a company $c$ per technology $a$ in sector $b = Power$ after accounting for primary energy efficiency factor $P$ (where $g$ is initial electricity generation before conversion to primary energy use) the following formula shall be used:

$$E_{a,b=power,c} = \dfrac{g_{a,b=power,c}}{P_{a}}$$

The primary energy efficiency factors are taken from the IEA[^1].

[^1]: IEA (2008) Energy efficiency indicators for public electricity production from fossil fuels, IEA Information Paper, OECD/IEA, July 2008

Then in the next step the conversion to common units of primary energy across the three respective sectors mentioned is made. The conversion factors ($F$) are taken from the [IEA World Energy Balances 2022](http://wds.iea.org/wds/pdf/WORLDBAL_Documentation.pdf) publication and the [IEA Unit Converter](https://www.iea.org/data-and-statistics/data-tools/unit-converter).

The output in Mtoe for $a$ company $c$ in sector $b$ with conversion factor $F$ is:

$$E_{b,c}^{Mtoe} = \sum_{\forall a \in b} E_{a,b,c} \times F_{b}$$

The relative production weighting per sector $b$ for a company $c$, is then calculated as:

$$sector\ share_{a,b,c} = \dfrac{E_{b,c}^{Mtoe}}{\sum_{b} E_{b,c}^{Mtoe}}$$

This company level sector split can now be used as a proxy to attribute parts of a loan to different transition relevant sectors a company operates in, taking into account the relative importance of each sector in the companies production profile. Note that the split only refers to the energy related in-sope PACTA sectors. This means that if a company additionally operates in another non-energy PACTA sector, the split should only be applied to the share of a loan that is attributed to the energy sectors.
