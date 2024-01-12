# Simulate data for beta-binomial models

# NOTE: OPEN BayesDLR.Rproj PRIOR TO RUNNING THIS CODE - otherwise output paths
# will not be set appropriately.

library(dplyr)
library(openxlsx)

set.seed(166774) # Set RNG seed to reproduce a result

# Path to output - Dataset 1: fixed r_{k}, one component
output_dir_1 <-
  file.path(getwd(), "data", "example_1", "raw_data")

# Path to output - Dataset 2: fixed r_{k}, two component mixture
output_dir_2 <-
  file.path(getwd(), "data", "example_2", "raw_data")


if(!file.exists(output_dir_1)){
  dir.create(output_dir_1, recursive = TRUE)
}

if(!file.exists(output_dir_2)){
  dir.create(output_dir_2, recursive = TRUE)
}


### Simulation Dataset 1 - One Component, Fixed r_{k} ##########################
# Number of variants to simulate
n_sim_data <- 200
# Control variant sample size
n_0 <- 250000
# Relative sample size in cases (n_1) relative to controls (n_0)
r_k <- 0.002
# Case sample size
n_1 <- r_k*n_0

# Distribution of variant prevalence in controls: Beta distribution
mean_pi_0 <- 0.05
var_pi_0 <- 0.000025
m_pi_0 <- mean_pi_0*(1 - mean_pi_0)/var_pi_0 - 1

# Mean/Variance of Prevalence Ratios: (gamma_{k}) cases/controls
pr_mean_1 <- 1
pr_var_1 <- 0.025

# Convert mean/variance of control variant prevalence to alpha/beta
alpha_pi_0 <- mean_pi_0*m_pi_0
beta_pi_0 <- (1 - mean_pi_0)*m_pi_0

# Convert prevalence ratio means/variances to alpha/beta
theta_k_mean_1 <- pr_mean_1*r_k
theta_k_var_1 <- pr_var_1*r_k^2

beta_1 <- 2 + (theta_k_mean_1*(1 + theta_k_mean_1))/theta_k_var_1
alpha_1 <- theta_k_mean_1*(beta_1 - 1)




# Simulate event data
sim_data <-
  data.frame(
    event = 1:n_sim_data,
    n_0 = n_0,
    n_1 = n_1,
    # Transformed Prevalence Ratio
    theta = rbeta(n = n_sim_data,
                  shape1 = alpha_1,
                  shape2 = beta_1)) %>%
  dplyr::mutate(
    # Prevalence Ratio
    gamma = theta/((1-theta)*r_k), 
    # Prevalence in Controls
    pi_0 = 
      rbeta(n = n_sim_data, shape1 = alpha_pi_0, shape2 = beta_pi_0),
    # Prevalence in Cases
    pi_1 = gamma*pi_0,
    # Counts in Controls, Cases
    y_0 = rbinom(n = n_sim_data, size = n_0, prob = pi_0),
    y_1 = rbinom(n = n_sim_data, size = n_1, prob = pi_1),
    # Total Events
    t_k = y_1 + y_0,
    # Relative sample sizes
    r_k = n_1/n_0,
    # Proportion of Events from Cases
    theta_k_hat = y_1/t_k,
    # Sample Prevalence in Controls, Cases
    pi_0_hat = y_0/n_0,
    pi_1_hat = y_1/n_1,
    # Sample Prevalence Ratio
    gamma_hat = pi_1_hat/pi_0_hat,
    flag = "include"
  )

if(sum(sim_data$pi_1 > 1) > 0)
  stop("Outcome probability in case population > 1:\n Check null distribution ",
       "and prevalence ratio distribution.")
if(sum(sim_data$t_k < 1)) {
  warning("At least one observation with no observed events. These will be ",
          "removed from the dataset.")
  sim_data <-
    sim_data %>%
    dplyr::filter(
      t_k > 0
    )
}




### Simulation Dataset 2 - Two Component, Fixed r_{k} ##########################
# Population 1 - Mean/Variance of Prevalence Ratios: (gamma_{k}) cases/controls
pr_mean_2 <- 3
pr_var_2 <- 1.5

# Proportion of variants from population 2
pr_component_2 <- 0.25

# Convert prevalence ratio means/variances to alpha/beta
theta_k_mean_2 <- pr_mean_2*r_k
theta_k_var_2 <- pr_var_2*r_k^2

beta_2 <- 2 + (theta_k_mean_2*(1 + theta_k_mean_2))/theta_k_var_2
alpha_2 <- theta_k_mean_2*(beta_2 - 1)

# Simulate event data
sim_data_2 <-
  data.frame(
    event = 1:n_sim_data,
    n_0 = n_0,
    n_1 = n_1,
    theta_1 = rbeta(n = n_sim_data, shape1 = alpha_1, shape2 = beta_1),
    theta_2 = rbeta(n = n_sim_data, shape1 = alpha_2, shape2 = beta_2),
    is_component_2 = 
      rbinom(n = n_sim_data, size = 1, prob =  pr_component_2)
  ) %>%
  dplyr::mutate(
    theta = (1 - is_component_2)*theta_1 + is_component_2*theta_2,
    gamma = theta/((1 - theta)*r_k),
    pi_0 = rbeta(n = n_sim_data,
                 shape1 = alpha_pi_0,
                 shape2 = beta_pi_0),
    pi_1 = gamma*pi_0,
    y_0 = rbinom(n = n_sim_data, size = n_0, prob = pi_0),
    y_1 = rbinom(n = n_sim_data, size = n_1, prob = pi_1),
    t_k = y_1 + y_0,
    r_k = n_1/n_0,
    theta_k_hat = y_1/t_k,
    pi_0_hat = y_0/n_0,
    pi_1_hat = y_1/n_1,
    gamma_hat = pi_1_hat/pi_0_hat,
    flag = "include"
  )

if(sum(sim_data_2$pi_1 > 1) > 0)
  stop("Outcome probability in case population > 1:\n Check null distribution ",
       "and prevalence ratio distribution.")
if(sum(sim_data_2$t_k < 1) > 0) {
  warning("At least one observation with no observed events. These will be ",
          "removed from the dataset.")
  sim_data_2 <-
    sim_data_2 %>%
    dplyr::filter(
      t_k > 0
    )
}




### Write Out Example Data in .xlsx and .csv Formats ###########################
example_1_workbook <-
  openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb = example_1_workbook,
  sheetName = "Sheet1",
)

openxlsx::writeData(
  wb = example_1_workbook,
  sheet = "Sheet1",
  x = sim_data %>% 
    dplyr::select(
      event, n_0, y_0, n_1, y_1, true_gamma = gamma, flag
    )
)

openxlsx::saveWorkbook(
  wb = example_1_workbook,
  file = file.path(output_dir_1, "example_1_component_v2_240110.xlsx"),
  overwrite = TRUE
)

write.csv(
  x = 
    sim_data %>% 
    dplyr::select(
      event, n_0, y_0, n_1, y_1, true_gamma = gamma, flag
    ),
  file = file.path(output_dir_1, "example_1_component_v2_240110.csv"),
  row.names = FALSE
)




example_2_workbook <-
  openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb = example_2_workbook,
  sheetName = "Sheet1",
)

openxlsx::writeData(
  wb = example_2_workbook,
  sheet = "Sheet1",
  x = sim_data_2 %>% 
    dplyr::select(
      event, n_0, y_0, n_1, y_1, true_gamma = gamma, flag
    )
)

openxlsx::saveWorkbook(
  wb = example_2_workbook,
  file = file.path(output_dir_2, "example_2_component_v1_240110.xlsx"),
  overwrite = TRUE
)

write.csv(
  x = 
    sim_data_2 %>% 
    dplyr::select(
      event, n_0, y_0, n_1, y_1, true_gamma = gamma, flag
    ),
  file = file.path(output_dir_2, "example_2_component_v1_240110.csv"),
  row.names = FALSE
)




### Introduce Errors: Test Data Checking #######################################
sim_data_v1 <-
  sim_data %>% 
  dplyr::select(
    event, n_0, y_0, n_1, y_1, true_gamma = gamma, flag
  )

# Non-integer
sim_data_v1$y_0[1] <- 0.5
sim_data_v1$y_1[2] <- 0.5
# Negative Value
sim_data_v1$y_0[3] <- -1
sim_data_v1$y_1[4] <- -1
# Numerator > Denominator
sim_data_v1$y_0[5] <- sim_data_v1$n_0[5] + 1
sim_data_v1$y_1[6] <- sim_data_v1$n_1[6] + 1
# No observed counts
sim_data_v1$y_0[7] <- 0
sim_data_v1$y_1[8] <- 0
# No observed counts in either group
sim_data_v1$y_0[9] <- 0
sim_data_v1$y_1[9] <- 0
# Missing Value
sim_data_v1$y_0[10] <- NA
sim_data_v1$y_1[11] <- NA
# Non-numeric
sim_data_v1$y_0[12] <- "A"
sim_data_v1$y_0[13] <- "A"
# Flag values for exclusion
sim_data_v1$flag[sample(x = 14:nrow(sim_data), size = 20)] <- "exclude"

write.csv(
  x = 
    sim_data_v1,
  file = file.path(output_dir_1, "example_1_component_v1_240110.csv"),
  row.names = FALSE
)