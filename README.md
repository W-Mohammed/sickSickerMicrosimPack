
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sickSickerMicrosimPack

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8075586.svg)](https://doi.org/10.5281/zenodo.8075586)
[![R-CMD-check](https://github.com/W-Mohammed/sickSickerMicrosimPack/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/W-Mohammed/sickSickerMicrosimPack/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/W-Mohammed/sickSickerMicrosimPack/graph/badge.svg)](https://app.codecov.io/gh/W-Mohammed/sickSickerMicrosimPack)
<!-- badges: end -->

`sickSickerMicrosimPack` provides a set of functions to build and run a
microsimulation model with four health states: **Healthy (H)**, **Sick
(S1)**, **Sicker (S2)** and **Dead (D)**. The model is vectorised to
efficiently simulate large cohorts of individuals. Transition
probabilities can depend on the time spent in a state and on individual
characteristics, and both costs and utilities can be modified by
covariates.

## Structure of GitHub repository

    #> .
    #> ├── DESCRIPTION
    #> ├── LICENSE
    #> ├── LICENSE.md
    #> ├── Meta
    #> │   └── vignette.rds
    #> ├── NAMESPACE
    #> ├── R
    #> │   ├── calc_costsV.R
    #> │   ├── calc_discount_wts.R
    #> │   ├── calc_effsV.R
    #> │   ├── data.R
    #> │   ├── run_microSimV.R
    #> │   ├── sampleV.R
    #> │   └── update_probsV.R
    #> ├── README.Rmd
    #> ├── README.md
    #> ├── _pkgdown.yml
    #> ├── codecov.yml
    #> ├── data
    #> │   ├── cycle_length.rda
    #> │   ├── discount_rate_QALYs.rda
    #> │   ├── discount_rate_costs.rda
    #> │   ├── l_trans_probs.rda
    #> │   ├── m_indi_features.rda
    #> │   ├── num_cycles.rda
    #> │   ├── num_i.rda
    #> │   ├── seed.rda
    #> │   ├── v_cost_coeffs.rda
    #> │   ├── v_starting_states.rda
    #> │   ├── v_states_costs.rda
    #> │   ├── v_states_costs1.rda
    #> │   ├── v_states_costs2.rda
    #> │   ├── v_states_names.rda
    #> │   ├── v_states_utilities.rda
    #> │   ├── v_states_utilities1.rda
    #> │   ├── v_states_utilities2.rda
    #> │   ├── v_util_coeffs.rda
    #> │   ├── v_util_t_decs.rda
    #> │   └── wtp.rda
    #> ├── data-raw
    #> │   └── model_inputs.R
    #> ├── doc
    #> │   ├── sickSickerMicrosimPack.R
    #> │   ├── sickSickerMicrosimPack.Rmd
    #> │   └── sickSickerMicrosimPack.html
    #> ├── inst
    #> │   └── bibliography
    #> │       └── references.bib
    #> ├── man
    #> │   ├── calc_costsV.Rd
    #> │   ├── calc_discount_wts.Rd
    #> │   ├── calc_effsV.Rd
    #> │   ├── cycle_length.Rd
    #> │   ├── discount_rate_QALYs.Rd
    #> │   ├── discount_rate_costs.Rd
    #> │   ├── l_trans_probs.Rd
    #> │   ├── m_indi_features.Rd
    #> │   ├── num_cycles.Rd
    #> │   ├── num_i.Rd
    #> │   ├── run_microSimV.Rd
    #> │   ├── sampleV.Rd
    #> │   ├── seed.Rd
    #> │   ├── update_probsV.Rd
    #> │   ├── v_cost_coeffs.Rd
    #> │   ├── v_starting_states.Rd
    #> │   ├── v_states_costs.Rd
    #> │   ├── v_states_costs1.Rd
    #> │   ├── v_states_costs2.Rd
    #> │   ├── v_states_names.Rd
    #> │   ├── v_states_utilities.Rd
    #> │   ├── v_states_utilities1.Rd
    #> │   ├── v_states_utilities2.Rd
    #> │   ├── v_util_coeffs.Rd
    #> │   ├── v_util_t_decs.Rd
    #> │   └── wtp.Rd
    #> ├── sickSickerMicrosimPack.Rproj
    #> ├── tests
    #> │   ├── testthat
    #> │   │   ├── test_calc_costsV.R
    #> │   │   ├── test_calc_discount_wts.R
    #> │   │   ├── test_calc_effsV.R
    #> │   │   ├── test_run_microSimV.R
    #> │   │   ├── test_sampleV.R
    #> │   │   ├── test_update_probsV.R
    #> │   │   └── testdata
    #> │   │       ├── data_run_microSimV.rds
    #> │   │       └── data_update_probsV.rds
    #> │   └── testthat.R
    #> └── vignettes
    #>     └── sickSickerMicrosimPack.Rmd

## Installation

You can install the development version of sickSickerMicrosimPack from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("W-Mohammed/sickSickerMicrosimPack")
```

Alternatively, you can clone the repository and build/install it
manually.

## Quick example

The following example simulates a small cohort of 10 individuals for
five cycles. Individual ages and sexes are sampled, and transition
probabilities are provided along with cost and utility parameters. The
example returns average costs and QALYs per individual.

``` r
# load the package
library(sickSickerMicrosimPack)
# define model inputs
num_i <- 10
num_cycles <- 5
v_start <- rep("H", num_i)
m_feats <- cbind(
  age = rnorm(num_i, 50, 3), 
  sex = sample(c(0,1), num_i, replace = TRUE)
)
v_states_names <- c("H", "S1", "S2", "D")
v_states_costs <- c(H = 2000, S1 = 4000, S2 = 15000, D = 0)
v_cost_coeffs  <- c(age = 11.5, sex = 300)
v_states_utilities <- c(H = 1, S1 = 0.75, S2 = 0.5, D = 0)
v_util_coeffs <- c(age = -0.0018, sex = -0.015)
v_util_t_decs <- c(S1 = -0.0015, S2 = -0.0020)
l_trans_probs <- list(
  p_HD = 0.005, 
  p_HS1 = 0.15, 
  p_S1H = 0.5,
  p_S1S2 = 0.105,
  p_S1D = 0.01488751,
  p_S2D = 0.04877166,
  rp_S1 = 0.2,
  rp_S2 = 0.29
)

res <- run_microSimV(
  v_starting_states = v_start,
  num_i = num_i,
  num_cycles = num_cycles,
  m_indi_features = m_feats,
  v_states_names = v_states_names,
  v_states_costs = v_states_costs,
  v_cost_coeffs = v_cost_coeffs,
  v_states_utilities = v_states_utilities,
  v_util_coeffs = v_util_coeffs,
  v_util_t_decs = v_util_t_decs,
  l_trans_probs = l_trans_probs,
  discount_rate_costs = 0.03,
  discount_rate_QALYs = 0.015,
  cycle_length = 1,
  starting_seed = 1
)

res$mean_costs
#> [1] 14496.84
res$mean_qalys
#> [1] 5.139279
```

Refer to the documentation of individual functions for further details
on parameters and usage.
