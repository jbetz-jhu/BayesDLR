rm(list = ls())

library(dplyr)
library(tidyr)
library(openxlsx)

### USER PARAMETERS ############################################################
gene <- "GENE_NAME"
# Optional suffix to file names (e.g. v1.0)
data_version_name <- "V1-YYMMDD"

# r_k_pct_change <- Inf # Do not filter by r_k # 
# Filter where (1 - x) <= n_0/n_1  <- (1 + x)
r_k_pct_change <- 0.1 # +/- 10%
# r_k_pct_change <- 0.2 # +/- 20%
# r_k_pct_change <- 0.25 # +/- 25%


# If project cloned to the project root directory, use base::getwd()
# If reading/writing to a different root directory, specify below:
# project_root <- base::getwd() 
project_root <- 
  file.path("C:", "PROJECT_ROOT")

code_version <- "0.0"

overwrite_existing_output <- TRUE




### COMPILE RESULTS ############################################################
analysis_name <- 
  if(!is.finite(r_k_pct_change)){
    "no_r_k_threshold"
  } else {
    paste0("r_k_", 100*r_k_pct_change, "_pct")
  }

results_file_prefix <- 
  paste0(c(gene, data_version_name, analysis_name), collapse = "-")

results_dir <-
  file.path(project_root, "results", gene)

bb_1_component_rdata_file <-
  paste0(results_file_prefix, "_1-Component_Beta_Binomial_v",
         code_version, ".Rdata")

bb_2_component_rdata_file <-
  paste0(results_file_prefix, "_2-Component_Beta_Binomial_v",
         code_version, ".Rdata")

bb_summary_xlsx_file <-
  paste0(results_file_prefix, "_Model_Summaries_v",
         code_version, ".xlsx")


bb_1_component_rdata_path <-
  file.path(results_dir, bb_1_component_rdata_file)

bb_2_component_rdata_path <-
  file.path(results_dir, bb_2_component_rdata_file)

bb_summary_xlsx_path <-
  file.path(results_dir, bb_summary_xlsx_file)


bb_1_exists <- file.exists(bb_1_component_rdata_path)
bb_2_exists <- file.exists(bb_2_component_rdata_path)


xlsx_workbook <-
  openxlsx::createWorkbook()


if(bb_1_exists){
  bb_1_sha256 <-
    bb_1_component_rdata_path %>%
    base::file() %>%
    openssl::sha256() %>%
    base::as.character() %>%
    base::as.character()
  
  bb_1_info <-
    file.info(bb_1_component_rdata_path)
  bb_1_size <- bb_1_info$size/2^(10)
  bb_1_mod_time <- bb_1_info$mtime
  
  load(bb_1_component_rdata_path)
  
  bb_1_metadata <-
    list(
      "Bayes DLR SHA256" = bayes_dlr_sha256,
      "Source File Name" = bb_1_source_data_file,
      "Source Data SHA256" = bb_1_source_data_sha256,
      "Input File Name" = bb_1_input_file,
      "Input Data SHA256" = bb_1_input_sha256,
      "Input File Size (MB)" = bb_1_input_size,
      "Input Modified" = bb_1_input_mod_time,
      "Results File Name" = bb_1_component_rdata_file,
      "Results File SHA256" = bb_1_sha256,
      "Results Size (MB)" = bb_1_size,
      "Results Modified" = bb_1_mod_time,
      
      "Version: R" = bb_1_session_info$R.version$version.string,
      "Version: optimx" = bb_1_session_info$otherPkgs$optimx$Version,
      "Run Datetime" = bb_1_run_time,
      "RNG Seed" = rng_seed_bb_1_component,
      "Total Variants" = nrow(analytic_data),
      "Highest r_k Allowed" =  median_r_k*(1 + r_k_pct_change),
      "Lowest r_k Allowed" =  median_r_k*(1 - r_k_pct_change),
      "Analyzed Variants" = sum(!analytic_data$r_k_flag)
    ) %>% 
    data.frame(check.names = FALSE) %>% 
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.character
      )
    ) %>% 
    tidyr::pivot_longer(
      cols = dplyr::everything()
    )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "1-Component - 0. Analytic Data"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "1-Component - 1. Results"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "1-Component - 2. Convergence"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "1-Component - 3. Parameters"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "1-Component - 4. Metadata"
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "1-Component - 0. Analytic Data",
    x = analytic_data
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "1-Component - 1. Results",
    x = beta_binomial_1_component_mml$results
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "1-Component - 2. Convergence",
    x = beta_binomial_1_component_mml$mml.solution
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "1-Component - 3. Parameters",
    x = 
      with(
        beta_binomial_1_component_mml,
        dplyr::tibble(
          parameter = c("mu", "m", "alpha", "beta",
                        "theta_prior_mean", "theta_prior_variance"),
          value = c(mu.mml, m.mml, alpha.mml, beta.mml,
                    theta.prior.mean.mml, theta.prior.variance.mml)
        )
      )
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "1-Component - 4. Metadata",
    x = bb_1_metadata
  )
  
} else {
  
}



if(bb_2_exists){
  bb_2_sha256 <-
    bb_2_component_rdata_path %>%
    base::file() %>%
    openssl::sha256() %>%
    base::as.character() %>%
    base::as.character()
  
  bb_2_info <-
    file.info(bb_2_component_rdata_path)
  bb_2_size <- bb_2_info$size/2^(10)
  bb_2_mod_time <- bb_2_info$mtime
  
  load(bb_2_component_rdata_path)
  
  bb_2_metadata <-
    list(
      "Bayes DLR SHA256" = bayes_dlr_sha256,
      "Source File Name" = bb_2_source_data_file,
      "Source Data SHA256" = bb_2_source_data_sha256,
      "Input File Name" = bb_2_input_file,
      "Input Data SHA256" = bb_2_input_sha256,
      "Input File Size (MB)" = bb_2_input_size,
      "Input Modified" = bb_2_input_mod_time,
      "Results File Name" = bb_2_component_rdata_file,
      "Results File SHA256" = bb_2_sha256,
      "Results Size (MB)" = bb_2_size,
      "Results Modified" = bb_2_mod_time,
      
      "Version: R" = bb_2_session_info$R.version$version.string,
      "Version: optimx" = bb_2_session_info$otherPkgs$optimx$Version,
      "Run Datetime" = bb_2_run_time,
      "RNG Seed" = rng_seed_bb_2_component,
      "Variants" = nrow(analytic_data)
    ) %>% 
    data.frame(check.names = FALSE) %>% 
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.character
      )
    ) %>% 
    tidyr::pivot_longer(
      cols = dplyr::everything()
    )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "2-Component - 0. Analytic Data"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "2-Component - 1. Results"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "2-Component - 2. Convergence"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "2-Component - 3. Parameters"
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "2-Component - 4. Metadata"
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "2-Component - 0. Analytic Data",
    x = analytic_data
  )

  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "2-Component - 1. Results",
    x = beta_binomial_2_component_mml$results
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "2-Component - 2. Convergence",
    x = beta_binomial_2_component_mml$all.results
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "2-Component - 3. Parameters",
    x = 
      with(
        beta_binomial_2_component_mml,
        dplyr::tibble(
          parameter = 
            stringr::str_replace_all(
              string = names(parameters),
              pattern ="\\.",
              replacement = "_"
            ),
          value = parameters
        )
      )
  )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "2-Component - 4. Metadata",
    x = bb_2_metadata
  )
  
} else {
  
}


if(file.exists(bb_summary_xlsx_file) & !overwrite_existing_output){
  warning(
    bb_summary_xlsx_file, " exitsts and `overwrite_existing_output` is ",
    "set to FALSE: Output will not be saved over this file."
  )
} else {
  openxlsx::saveWorkbook(
    wb = xlsx_workbook,
    file = bb_summary_xlsx_path,
    overwrite = overwrite_existing_output
  )
}
  