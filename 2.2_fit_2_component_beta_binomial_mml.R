# Get variables passed as environmental variables
gene <- Sys.getenv(x = "bayes_dlr_gene")
project_root <- Sys.getenv(x = "bayes_dlr_project_root")
data_version_name <- Sys.getenv(x = "bayes_dlr_data_version_name")
analysis_name <- Sys.getenv(x = "bayes_dlr_analysis_name")
data_dir <- Sys.getenv(x = "bayes_dlr_data_dir")
code_version <- Sys.getenv(x = "bayes_dlr_code_version")
n_mc_samples<- as.numeric(Sys.getenv(x = "bayes_dlr_n_mc_samples"))
posterior_quantiles <- Sys.getenv(x = "bayes_dlr_posterior_quantiles")
rng_seed_bb_2_component <- 
  as.numeric(Sys.getenv(x = "bayes_dlr_rng_seed_bb_2_component"))
data_dir <- Sys.getenv(x = "bayes_dlr_data_dir")
r_k_pct_change <- as.numeric(Sys.getenv(x = "bayes_dlr_r_k_pct_change"))

### BEGIN CODE #################################################################
bb_2_run_time <- base::Sys.time()

library(optimx)

posterior_quantiles <-
  sapply(
    X = strsplit(x = posterior_quantiles, split = ", ", fixed = TRUE)[[1]],
    FUN = as.numeric,
    USE.NAMES = FALSE
  )

bayes_dlr_sha256 <-
  openssl::sha256(file(file.path(getwd(), "bayes_dlr_functions.r")))

bayes_dlr_sha256 <-
  base::as.character(base::as.character(x = bayes_dlr_sha256))

source("bayes_dlr_functions.r")

processed_input_dir <-
  file.path(data_dir, "processed_input")

results_dir <-
  file.path(project_root, "results", gene)

if(!file.exists(results_dir)){
  dir.create(path = results_dir, recursive = TRUE)
}


data_file_prefix <- 
  paste0(c(gene, data_version_name), collapse = "-")
results_file_prefix <- 
  paste0(c(gene, data_version_name, analysis_name), collapse = "-")

processed_rdata_file <-
  paste0(data_file_prefix, "_processed_v", code_version, ".Rdata")

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
  paste0(results_file_prefix, "_2-Component_Beta_Binomial_v",
         code_version, ".Rdata")

output_path <-
  file.path(results_dir, output_file)

set.seed(seed = rng_seed_bb_2_component)

# Filter by r_k value
analytic_data$r_k <- with(analytic_data, n_1/n_0)
median_r_k <- median(analytic_data$r_k)
analytic_data$r_k_flag <-
  with(analytic_data, 
       (r_k/median_r_k) < (1 - r_k_pct_change) |
         (r_k/median_r_k) > (1 + r_k_pct_change)
  )

beta_binomial_2_component_mml <-
  bb.2c.mixture.grid.search(
    data = 
      subset(analytic_data,
             r_k_flag == FALSE),
    y.0 = "y_0",
    y.1 = "y_1",
    n.0 = "n_0",
    n.1 = "n_1",
    id = "variant_id",
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
      "rng_seed_bb_2_component",
      "bb_2_source_data_file",
      "bb_2_source_data_sha256",
      "bb_2_run_time",
      "bb_2_input_file",
      "bb_2_input_sha256",
      "bb_2_input_size",
      "bb_2_input_mod_time",
      "median_r_k",
      "r_k_pct_change")
)