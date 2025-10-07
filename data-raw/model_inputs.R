## code to prepare `model_inputs` dataset goes here

# General parameters
num_i <- 1e5               # number of simulated individuals
num_cycles <- 30           # time horizon if each cycle is a year long
cycle_length <- 1          # length of cycle (in years)
seed <- 1234               # random number generator state
wtp <- 30000               # Willingness to pay for each QALY ($)
discount_rate_costs <- 0.03 # annual discount rate for costs
discount_rate_QALYs <- 0.015 # annual discount rate for health outcomes


# Population characteristics/features
mean_age <- 50             # mean age in the simulated population
sd_age <- 3                # standard deviation of the age in the simulated population
prop_females <- 0.6        # proportion of females in the simulated population
prop_males <- 1 - prop_females # proportion of males in the simulated population
set.seed(seed)
m_indi_features <- cbind(
  "age" = rnorm(
    n = num_i,
    mean = mean_age,
    sd = sd_age
  ),
  "sex" = sample(
    x = c(0, 1),
    size = num_i,
    replace = TRUE,
    prob = c(prop_females, prop_males)
  )
)


# Health states
v_states_names <- c("H","S1", "S2", "D")
v_starting_states <- rep("H", num_i)

# Transition probabilities (per cycle)
p_HD <- 0.005
p_HS1 <- 0.15
p_S1H <- 0.5
p_S1S2 <- 0.105
rr_S1 <- 3
rr_S2 <- 10
r_HD <- -log(1 - p_HD)
r_S1D <- rr_S1 * r_HD
r_S2D <- rr_S2 * r_HD
p_S1D <- 1 - exp(- r_S1D)
p_S2D <- 1 - exp(- r_S2D)
rp_S1 <- 0.2
rp_S2 <- 0.29

l_trans_probs <- list(
  "p_HD"   = p_HD,
  "p_HS1"  = p_HS1,
  "p_S1H"  = p_S1H,
  "p_S1S2" = p_S1S2,
  "p_S1D"  = p_S1D,
  "p_S2D"  = p_S2D,
  "rp_S1"  = rp_S1,
  "rp_S2"  = rp_S2
)

# Cost and utility inputs
c_H <- 2000
c_S1 <- 4000
c_S2 <- 15000
c_S1_Trt1 <- c_S1 + 12000
c_S2_Trt1 <- c_S2 + 12000
c_S1_Trt2 <- c_S1 + 11350
c_S2_Trt2 <- c_S2 + 11350
c_D <- 0
c_age_cof <- 11.5
c_sex_cof <- 300

v_cost_coeffs <- c(
  "age" = c_age_cof, "sex" = c_sex_cof
)

u_H <- 1
u_S1 <- 0.75
u_S2 <- 0.5
u_S1_Trt1 <- u_S1 + 0.2
u_S2_Trt1 <- u_S2
u_S1_Trt2 <- u_S1 + 0.15
u_S2_Trt2 <- u_S2 + 0.05
u_D <- 0
u_age_cof <- -0.0018
u_sex_cof <- -0.015
ru_S1 <- -0.0015
ru_S2 <- -0.0020

v_util_coeffs <- c(
  "age" = u_age_cof, "sex" = u_sex_cof
)
v_util_t_decs <- c(
  "S1" = ru_S1, "S2" = ru_S2
)

# Payoffs - no treatment
v_states_costs <- c("H" = c_H, "S1" = c_S1, "S2" = c_S2, "D" = c_D)
v_states_utilities <- c("H" = u_H, "S1" = u_S1, "S2" = u_S2, "D" = u_D)

# Payoffs - treatment 1
v_states_costs1 <- c("H" = c_H, "S1" = c_S1_Trt1, "S2" = c_S2_Trt1, "D" = c_D)
v_states_utilities1 <- c("H" = u_H, "S1" = u_S1_Trt1, "S2" = u_S2_Trt1, "D" = u_D)

# Payoffs - treatment 2
v_states_costs2 <- c("H" = c_H, "S1" = c_S1_Trt2, "S2" = c_S2_Trt2, "D" = c_D)
v_states_utilities2 <- c("H" = u_H, "S1" = u_S1_Trt2, "S2" = u_S2_Trt2, "D" = u_D)


usethis::use_data(
  num_i,
  num_cycles,
  cycle_length,
  seed,
  wtp,
  discount_rate_costs,
  discount_rate_QALYs,
  m_indi_features,
  v_states_names,
  v_starting_states,
  l_trans_probs,
  v_cost_coeffs,
  v_util_coeffs,
  v_util_t_decs,
  v_states_costs,
  v_states_utilities,
  v_states_costs1,
  v_states_utilities1,
  v_states_costs2,
  v_states_utilities2,
  overwrite = TRUE
)
