### USER MODIFIABLE PARAMETERS #################################################
data_subfolder <- "example_1"
data_version_name <- "V1-240110"

# `r_k_threshold` indicates which observations may be excluded if the ratio
# of the sample sizes is x% higher or lower than the median of all observations.
# For example, if the median value of r_k in the sample was 5, and the threshold
# was 0.20, only observations with r_k between 80% and 120% of 5: 4-6.
# `data_version_name` and `r_k_threshold` get appended to create
# `analysis_version_name`.

r_k_threshold <- 0.10
# r_k_threshold <- 0.20
# r_k_threshold <- 0.25
# r_k_threshold <- Inf # No threshold - NOT ADVISED

stop_on_data_error <- TRUE # Stop on any data entry error. RECOMMENDED.
stop_on_data_error <- FALSE # Exclude rows with errors. NOT RECOMMENDED.

overwrite_existing_output <- TRUE
# overwrite_existing_output <- FALSE




### BEGIN PROGRAM ##############################################################
# 1. Check if working directory is ../BayesDLR
project_root <- base::getwd()
working_subfolder <-
  tail(strsplit(x = project_root, split = "/")[[1]], 1)

if(working_subfolder != "BayesDLR"){
  stop("Load BayesDLR.Rproj before proceeding. See README.MD for instructions.")
} else {
  
  analysis_version_name <-
    paste0(data_version_name, "_r_k_", r_k_threshold*100, "_pct")
  
  code_dir <- file.path(project_root, "code")
  analysis_dir <- file.path(project_root, "analyses", data_subfolder)
  
  # Load configuration file
  config_file <- paste0(analysis_version_name, ".yml")
  config_path <- file.path(analysis_dir, config_file)
  
  if(!file.exists(config_path)) {
    stop("Configuration file not found:\n  Path: ",  config_path, "\n",
         "  Confirm `data_subfolder` (", data_subfolder, "), ",
         "`data_version_name` (", data_version_name, "), and ",
         "`r_k_threshold` (", r_k_threshold, ") are correctly specified.")
  } else {
    configuration <- config::get(file = config_path)
    
    for(i in 1:length(configuration)) {
      assign(x = names(configuration)[i], value = configuration[[i]])
    }
    
    data_dir <- file.path(project_root, "data", data_subfolder)
    processed_data_dir <- file.path(data_dir, "processed_data")
    
    source_path <- file.path(code_dir, "source", "bayes_dlr_functions.r")
    
    workflow_1_path <- 
      file.path(code_dir, "workflow", "1_process_raw_data.r")
    workflow_2_1_path <-
      file.path(code_dir, "workflow", "2.1_fit_1_component_beta_binomial_mml.r")
    workflow_2_2_path <-
      file.path(code_dir, "workflow", "2.2_fit_2_component_beta_binomial_mml.r")
    workflow_3_path <- 
      file.path(code_dir, "workflow", "3_extract_model_results.r")
    
    required_paths <-
      c(source_path,
        config_path,
        input_path,
        processed_data_dir,
        workflow_1_path,
        workflow_2_1_path,
        workflow_2_2_path,
        workflow_3_path
      )
    
    required_paths_exist <- file.exists(required_paths)
    
    if(!all(required_paths_exist)){
      stop("The following paths could not be found:\n",
           paste(required_paths[which(!required_paths_exist)], collapse = "\n"))
    } else {
      
      # Process Raw Data
      data_check_result <-
        callr::r(
          func =
            function(path, ...){
              source(file = path)
            },
          
          args =
            list(
              project_root = project_root,
              data_subfolder = data_subfolder,
              data_version_name = data_version_name,
              config_path = config_path,
              overwrite_existing_output = overwrite_existing_output,
              path = workflow_1_path
            ),
          
          env =
            c(
              "bayes_dlr_project_root" = project_root,
              "bayes_dlr_data_subfolder" = data_subfolder,
              "bayes_dlr_data_version_name" = data_version_name,
              "bayes_dlr_config_path" = config_path,
              "bayes_dlr_overwrite_existing_output" = overwrite_existing_output
            )
        )$value
      
      if(data_check_result$any_invalid_rows & stop_on_data_error){
        stop("Data entry errors detected in input file.\n",
             "  File: ", input_path, "\n  Rows: ", 
             paste0(data_check_result$invalid_rows, collapse = ", "))
      } else {
        
        if(data_check_result$any_invalid_rows){
          warning("Data entry errors detected in input file.\n",
                  "  File: ", input_path, "\n  Rows: ", 
                  paste0(data_check_result$invalid_rows, collapse = ", "), "\n",
                  "\n  REVIEW DATA CAREFULLY BEFORE PROCEEDING.")
        }
        
        callr::r(
          func =
            function(path, ...){
              source(file = path)
            },
          
          args =
            list(
              project_root = project_root,
              data_subfolder = data_subfolder,
              data_version_name = data_version_name,
              config_path = config_path,
              bb_1_component_seed = bb_1_component_seed,
              analysis_version_name = analysis_version_name,
              path = workflow_2_1_path
            ),
          
          env =
            c(
              "bayes_dlr_project_root" = project_root,
              "bayes_dlr_data_subfolder" = data_subfolder,
              "bayes_dlr_data_version_name" = data_version_name,
              "bayes_dlr_config_path" = config_path,
              
              "bayes_dlr_bb_1_component_seed" = bb_1_component_seed,
              "bayes_dlr_r_k_threshold" = r_k_threshold,
              "bayes_dlr_analysis_version_name" = analysis_version_name,
              "bayes_dlr_config_path" = config_path
              
            )
        )$value
        
        
        callr::r(
          func =
            function(path, ...){
              source(file = path)
            },
          
          args =
            list(
              project_root = project_root,
              data_subfolder = data_subfolder,
              data_version_name = data_version_name,
              config_path = config_path,
              bb_1_component_seed = bb_1_component_seed,
              analysis_version_name = analysis_version_name,
              path = workflow_2_2_path
            ),
          
          env =
            c(
              "bayes_dlr_project_root" = project_root,
              "bayes_dlr_data_subfolder" = data_subfolder,
              "bayes_dlr_data_version_name" = data_version_name,
              "bayes_dlr_config_path" = config_path,
              
              "bayes_dlr_bb_2_component_seed" = bb_2_component_seed,
              "bayes_dlr_r_k_threshold" = r_k_threshold,
              "bayes_dlr_analysis_version_name" = analysis_version_name,
              "bayes_dlr_config_path" = config_path
              
            )
        )$value
        
      }
    } 
  }
}