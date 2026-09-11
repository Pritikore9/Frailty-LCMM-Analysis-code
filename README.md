# Frailty Trajectory Group (LCMM) Analysis

## Overview
This repository contains the R analysis script used to examine predictors of
latent-class frailty trajectory group membership (Very Low / Moderate / Higher
frailty) among older adults. Frailty groups were derived in a prior step using
latent class mixed modeling (LCMM); this script picks up after group
assignment and covers baseline characteristic comparisons, univariate and
multivariable multinomial logistic regression, and post-hoc predicted
probability estimation.

## What the script does
1. Imports the dataset (with pre-assigned LCMM frailty groups) from an Excel file.
2. Produces baseline characteristics tables comparing frailty groups (raw and recoded factor versions).
3. Recodes demographic, clinical, and comorbidity variables into ordered factors for modeling.
4. Runs univariate multinomial logistic regression models (crude odds ratios) for each candidate predictor.
5. Runs a multivariable multinomial logistic regression model (adjusted odds ratios), including a p < 0.20 variable-selection step with clinically forced covariates.
6. Estimates adjusted vs. unadjusted predicted probabilities of frailty group membership using `emmeans`.

## Requirements
- R (≥ 4.2 recommended)
- R packages:
  - `readxl`
  - `dplyr`
  - `tidyr`
  - `stringr`
  - `purrr`
  - `gtsummary`
  - `gt`
  - `nnet`
  - `broom`
  - `emmeans`
  - `openxlsx`

Install all at once:

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "gtsummary", "gt", "nnet", "broom", "emmeans", "openxlsx"
))
```

## Data
To run this script, place your data file in a folder
named `data/` in the project root, and update the `RAW_DATA_FILE` variable
in the CONFIG section at the top of the script to match your filename.

## How to run
1. Clone or download this repository.
2. Place your data file inside `data/`.
3. Open `frailty_lcmm_analysis.R` in RStudio (or your R environment of choice).
4. Update the CONFIG section (data folder path, filename) if needed.
5. Run the script top to bottom. All output tables are written to an
   auto-created `outputs/` folder.
## License
This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.
