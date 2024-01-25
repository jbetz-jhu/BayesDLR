### Configure BayesDLR Analysis ################################################
# READ README.HTML BEFORE USING.

### USER MODIFIABLE PARAMETERS #################################################

# `data_subfolder` is a subfolder of ../BayesDLR/data where data are stored
data_subfolder <- "example_1"

# `input_data_file` is input for the analysis, stored in:
# ../BayesDLR/data/(data_subfolder)/raw_data
input_data_file <- "example_1_component_v1_240110.csv"
input_sheet_name <- "Sheet1" # Only used for .xlsx files

# `data_version_name` is a label for all files for analyzing `input_data_file`
# This includes: configuration files, processed input, results, and so on.
# Data version should be closely linked with `input_data_file` so it is easy
# to identify the corresponding analyses for a given raw input dataset.
# See `r_k_threshold` below.
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


# Column names in `input_data_file`
row_id <- "event"
case_denominator <- "n_1"
case_count <- "y_1"
control_denominator <- "n_0"
control_count <- "y_0"

# Column indicating whether to include or exclude each row
flag_column <- "flag"
# flag_column <- NULL # Set to NULL if all variants are to be included

# Values of flag variable indicating inclusion in analysis.
## NOTE: the flag column will be 'trimmed' (leading/trailing spaces removed) and
# 'squished' (all whitespace replaced with single space). This is to avoid
# accidental exclusion due to invisible whitespace.
flags_include <- c("include", "Include")


#### RNG Seeds ####

# RNG seed can be specified, or a new RNG seed can be created
bb_1_component_seed <- NULL # Generate new seed
# bb_1_component_seed <- 12345 # Specify seed - Reproduce analysis


bb_2_component_seed <- NULL # Generate new seed
# bb_2_component_seed <- 23456 # Specify seed - Reproduce analysis




### BEGIN PROGRAM ##############################################################
# 1. Check if working directory is ../BayesDLR
project_root <- base::getwd()
working_subfolder <-
  tail(strsplit(x = project_root, split = "/")[[1]], 1)

# 2. Check if all packages are installed
required_packages <-
  c("config", "dplyr", "openssl", "openxlsx", "optimx", "readr", "readxl",
    "stringr", "tidyr")

installed_packages <-
  as.data.frame(installed.packages()[, c("Package", "Version")])

all_dependencies_installed <-
  all(required_packages %in% installed_packages$Package)

if(!all_dependencies_installed){
  stop("Run 0_check_BayesDLR_configuration.r to install required packages.")
} else if(working_subfolder != "BayesDLR"){
  stop("Load BayesDLR.Rproj before proceeding. See README.MD for instructions.")
} else {
  
  analysis_version_name <-
    paste0(data_version_name, "_r_k_", r_k_threshold*100, "_pct")
  
  # Generate RNG Seeds if not supplied
  if(is.null(bb_1_component_seed)){
    bb_1_component_seed <- round(runif(n = 1, min = 1, max = 1e6))
  }
  
  # Generate RNG Seeds if not supplied
  if(is.null(bb_2_component_seed)){
    bb_2_component_seed <- round(runif(n = 1, min = 1, max = 1e6))
  }
  
  analyses_subfolder <- file.path(project_root, "analyses")
  
  if(!file.exists(analyses_subfolder)){
    dir.create(path = analyses_subfolder)
  }
  
  data_dir <- file.path(project_root, "data", data_subfolder)
  input_path <- file.path(data_dir, "raw_data", input_data_file)
  processed_data_dir <- file.path(data_dir, "processed_data")
  
  data_dir_exists <- file.exists(data_dir)
  input_path_exists <- file.exists(input_path)
    
  if(!(data_dir_exists & input_path_exists)){
    if(!file.exists(data_dir)){
      stop(data_dir, " does not exist.")
    }
    
    if(!file.exists(input_path)){
      stop(input_path, " does not exist.")
    }
  } else {
    
    if(!file.exists(processed_data_dir)){
      dir.create(path = processed_data_dir, recursive = TRUE)
    }
    
    analysis_dir <- file.path(analyses_subfolder, data_subfolder)
    
    if(!file.exists(analysis_dir)){
      dir.create(path = analysis_dir, recursive = TRUE)
    }
    
    config_file <- paste0(analysis_version_name, ".yml")
    config_path <-
      file.path(analysis_dir, config_file)
    
    config_yml <-
      c("default:",
        paste0("  ", "data_subfolder: ", data_subfolder),
        paste0("  ", "data_version_name: ", data_version_name),
        paste0("  ", "input_file: ", input_data_file),
        paste0("  ", "input_sheet_name: ", input_sheet_name),
        paste0("  ", "input_path: ", input_path),
        
        paste0("  ", "row_id: ", row_id),
        paste0("  ", "case_denominator: ", case_denominator),
        paste0("  ", "case_count: ", case_count),
        paste0("  ", "control_denominator: ", control_denominator),
        paste0("  ", "control_count: ", control_count),
        paste0("  ", "flag_column: ", flag_column),
        paste0("  ", "flags_include: ", paste0(flags_include, collapse = "|")),
        paste0("  ", "r_k_threshold: ", r_k_threshold),
        paste0("  ", "analysis_version_name: ", analysis_version_name),
        paste0("  ", "bb_1_component_seed: ", bb_1_component_seed),
        paste0("  ", "bb_2_component_seed: ", bb_2_component_seed)
      )
    
    writeLines(
      text = config_yml,
      con = config_path
    )
    
    cat("\n\nSuccessfully wrote ", config_file, " to ", analysis_dir,
        ".\n", "Proceed to 2_run_analysis.R", sep = "")
  }
}