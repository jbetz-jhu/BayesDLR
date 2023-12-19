rm(list = ls())

### USER PARAMETERS ############################################################
gene <- "GENE_NAME"
# Optional suffix to file names (e.g. v1.0)
data_version_name <- "V1-YYMMDD"

# If project cloned to the project root directory, use base::getwd()
# If reading/writing to a different root directory, specify below:
# project_root <- base::getwd() 
project_root <- 
  file.path("C:", "PROJECT_ROOT")

data_dir <-
  file.path(project_root, "data", gene)


code_version <- "0.0"

n_mc_samples <- 10000

posterior_quantiles <- 
  c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99)

# r_k_pct_change <- Inf # Do not filter by r_k # 
# Filter where (1 - x) <= n_0/n_1  <- (1 + x)
# r_k_pct_change <- 0.1 # +/- 10%
r_k_pct_change <- 0.2 # +/- 20%
# r_k_pct_change <- 0.25 # +/- 25%


rng_seed_bb_1_component <- 12345
rng_seed_bb_2_component <- 23456

bb_1_component_r_file <-
  "1.2_fit_1_component_beta_binomial_mml.R"
bb_2_component_r_file <-
  "1.3_fit_2_component_beta_binomial_mml.R"




### RUN ANALYSES ###############################################################
analysis_name <- 
  if(!is.finite(r_k_pct_change)){
    "no_r_k_threshold"
  } else {
    paste0("r_k_", 100*r_k_pct_change, "_pct")
  }
  


bb_1_component_result <-
  callr::r(
    func =
      function(...){
        source(file.path("2.1_fit_1_component_beta_binomial_mml.R"))
      },
    
    args =
      list(
        gene,
        analysis_name,
        data_dir,
        code_version,
        n_mc_samples,
        posterior_quantiles,
        rng_seed_bb_1_component,
        r_k_pct_change
      ),
    
    env =
      c(
        "bayes_dlr_gene" = gene,
        "bayes_dlr_project_root" = project_root,
        "bayes_dlr_data_version_name" = data_version_name,
        "bayes_dlr_analysis_name" = analysis_name,
        "bayes_dlr_data_dir" = data_dir,
        "bayes_dlr_code_version" = code_version,
        "bayes_dlr_n_mc_samples" = n_mc_samples,
        "bayes_dlr_posterior_quantiles" = 
          paste(posterior_quantiles, collapse = ", "),
        "bayes_dlr_rng_seed_bb_1_component" = rng_seed_bb_1_component,
        "bayes_dlr_r_k_pct_change" = r_k_pct_change
      )
  )


bb_2_component_result <-
  callr::r(
    func =
      function(...){
        source(file.path("2.2_fit_2_component_beta_binomial_mml.R"))
      },
    
    args =
      list(
        gene,
        analysis_name,
        data_dir,
        code_version,
        n_mc_samples,
        posterior_quantiles,
        rng_seed_bb_2_component,
        r_k_pct_change
      ),
    
    env =
      c(
        "bayes_dlr_gene" = gene,
        "bayes_dlr_project_root" = project_root,
        "bayes_dlr_data_version_name" = data_version_name,
        "bayes_dlr_analysis_name" = analysis_name,
        "bayes_dlr_data_dir" = data_dir,
        "bayes_dlr_code_version" = code_version,
        "bayes_dlr_n_mc_samples" = n_mc_samples,
        "bayes_dlr_posterior_quantiles" = 
          paste(posterior_quantiles, collapse = ", "),
        "bayes_dlr_rng_seed_bb_2_component" = rng_seed_bb_2_component,
        "bayes_dlr_r_k_pct_change" = r_k_pct_change
      )
  )