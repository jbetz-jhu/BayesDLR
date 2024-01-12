### Set Parameters #############################################################
project_root <- Sys.getenv(x = "bayes_dlr_project_root")
data_subfolder <- Sys.getenv(x = "bayes_dlr_data_subfolder")
data_version_name <- Sys.getenv(x = "bayes_dlr_data_version_name")
config_path <- Sys.getenv(x = "bayes_dlr_config_path")
bb_2_component_seed <- Sys.getenv(x = "bayes_dlr_bb_2_component_seed")
r_k_threshold <- Sys.getenv(x = "bayes_dlr_r_k_threshold")
analysis_version_name <- Sys.getenv(x = "bayes_dlr_analysis_version_name")

n_mc_samples <- 10000
posterior_quantiles <- 
  c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99)

### BEGIN CODE #################################################################
bb_2_run_time <- base::Sys.time()

code_version <- "0.0"
r_k_threshold <- as.numeric(r_k_threshold)

library(optimx)

bayes_dlr_path <-
  file.path(project_root, "code", "source", "bayes_dlr_functions.r")

bayes_dlr_sha256 <-
  openssl::sha256(file(bayes_dlr_path))

bayes_dlr_sha256 <-
  base::as.character(base::as.character(x = bayes_dlr_sha256))

source(bayes_dlr_path)

data_dir <- file.path(project_root, "data", data_subfolder)

processed_input_dir <-
  file.path(data_dir, "processed_data")

results_dir <-
  file.path(project_root, "results", data_subfolder)

if(!file.exists(results_dir)){
  dir.create(path = results_dir, recursive = TRUE)
}

file_prefix <-
  if(is.null(data_version_name)){
    data_version_name
  } else {
    paste0(c(data_subfolder, data_version_name), collapse = "-")
  }

processed_rdata_file <-
  paste0(file_prefix, "_processed_v", code_version, ".Rdata")

processed_rdata_path <-
  file.path(processed_input_dir, processed_rdata_file)

if(file.exists(processed_rdata_path)){
  load(processed_rdata_path)
  
  bb_2_input_sha256 <-
    openssl::sha256(file(processed_rdata_path))
  
  bb_2_input_sha256 <-
    base::as.character(base::as.character(x = bb_2_input_sha256))
  
  bb_2_input_info <-
    file.info(processed_rdata_path)
  bb_2_input_size <- bb_2_input_info$size/2^(10)
  bb_2_input_mod_time <- bb_2_input_info$mtime
  
  bb_2_source_data_file <- input_file_information$input_data_file
  bb_2_source_data_sha256 <- input_file_information$input_sha256
  
  bb_2_input_file <- processed_rdata_file
} else{
  stop("Unable to locate ", processed_rdata_path)
}

output_file <-
  paste0(data_subfolder, "_", analysis_version_name,
         "_2-Component_Beta_Binomial_v", code_version, ".Rdata")

output_path <-
  file.path(results_dir, output_file)



# Filter by r_k value
analytic_data$r_k <- with(analytic_data, n_1/n_0)
median_r_k <- median(analytic_data$r_k)
analytic_data$r_k_flag <-
  with(analytic_data,
       (r_k/median_r_k) < (1 - r_k_threshold) |
         (r_k/median_r_k) > (1 + r_k_threshold)
  )

set.seed(seed = as.numeric(bb_2_component_seed))

beta_binomial_2_component_mml <-
  bb.2c.mixture.grid.search(
    data = 
      subset(analytic_data,
             r_k_flag == FALSE),
    y.0 = "y_0",
    y.1 = "y_1",
    n.0 = "n_0",
    n.1 = "n_1",
    id = "row_id",
    mu.1 = c(0.1, 0.25, 0.5, 0.75, 0.9),
    mu.2 = c(0.1, 0.25, 0.5, 0.75, 0.9),
    m.1 = c(1),
    m.2 = c(1),
    epsilon = c(0.15, 0.35),
    
    n.mc = n_mc_samples,
    posterior.quantiles = posterior_quantiles,
    verbose = TRUE
  )

bb_2_session_info <- utils::sessionInfo()

save(
  file = output_path,
  list = 
    c("beta_binomial_2_component_mml",
      "ll.mbb",
      "ll.mbb.mix",
      "bb.2c.mixture.grid.search",
      "bb.2c.mixture.optimize",
      "analytic_data",
      "n_mc_samples",
      "posterior_quantiles",
      "bb_2_session_info",
      "bayes_dlr_sha256",
      "bb_2_component_seed",
      "bb_2_source_data_file",
      "bb_2_source_data_sha256",
      "bb_2_run_time",
      "bb_2_input_file",
      "bb_2_input_sha256",
      "bb_2_input_size",
      "bb_2_input_mod_time",
      "median_r_k",
      "r_k_threshold")
)