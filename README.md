<!-- README.md for sickSickerMicrosimPack -->

# sickSickerMicrosimPack

`sickSickerMicrosimPack` provides a set of functions to build and run a
microsimulation model with four health states: **Healthy (H)**, **Sick (S1)**,
**Sicker (S2)** and **Dead (D)**. The model is vectorised to efficiently
simulate large cohorts of individuals. Transition probabilities can depend
on the time spent in a state and on individual characteristics, and both
costs and utilities can be modified by covariates.

## Installation

This package is not yet available on CRAN. To install from GitHub using
the [`remotes`](https://cran.r-project.org/package=remotes) package:

```r
remotes::install_github("W-Mohammed/sickSickerMicrosimPack")
```

Alternatively, you can clone the repository and build/install it manually.

## Quick example

The following example simulates a small cohort of 10 individuals for five
cycles. Individual ages and sexes are sampled, and transition probabilities
are provided along with cost and utility parameters. The example returns
average costs and QALYs per individual.

```r
library(sickSickerMicrosimPack)

num_i <- 10
num_cycles <- 5
v_start <- rep("H", num_i)
m_feats <- cbind(age = rnorm(num_i, 50, 3), sex = sample(c(0,1), num_i, replace = TRUE))
v_states_names <- c("H", "S1", "S2", "D")
v_states_costs <- c(H = 2000, S1 = 4000, S2 = 15000, D = 0)
v_cost_coeffs  <- c(age = 11.5, sex = 300)
v_states_utilities <- c(H = 1, S1 = 0.75, S2 = 0.5, D = 0)
v_util_coeffs <- c(age = -0.0018, sex = -0.015)
v_util_t_decs <- c(S1 = -0.0015, S2 = -0.0020)
l_trans_probs <- list(p_HD = 0.005, p_HS1 = 0.15, p_S1H = 0.5,
                      p_S1S2 = 0.105, p_S1D = 0.01488751,
                      p_S2D = 0.04877166, rp_S1 = 0.2, rp_S2 = 0.29)

res <- run_microSimV(v_starting_states = v_start,
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
                     starting_seed = 1)

res$mean_costs
res$mean_qalys
```

Refer to the documentation of individual functions for further details on
parameters and usage.