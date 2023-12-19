library(stringr)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(openxlsx)
library(openssl)

### USER MODIFIABLE PARAMETERS #################################################

# stop_on_data_error <- TRUE # Stop on any data entry error. RECOMMENDED.
stop_on_data_error <- FALSE # Exclude rows with errors. NOT RECOMMENDED.


#### Input Path ####
gene <- "GENE_NAME"
data_version_name <- "V1-YYMMDD"
# data_version_name <- NULL # No Suffix

# If project cloned to the project root directory, use base::getwd()
# If reading/writing to a different root directory, specify below:
# project_root <- base::getwd() 
project_root <- 
  file.path("C:", "PROJECT_ROOT")

## NOTE: This file should be located in (project_root)\data\(gene)\raw_input
input_data_file <- "Input_file_v1.0_YYMMDD.xlsx"


# If existing output exists, should it be overwritten?
overwrite_existing_output <- TRUE
# overwrite_existing_output <- FALSE

#### Input Variable Names ####
variant_id <- "id"


case_denominator <- "n.1"


case_count <- "y.1"



control_denominator <- "n.0"


control_count <- "y.0"



# Column indicating whether to include or exclude each row
# flag_column <- NULL # Set to NULL if all variants are to be included
flag_column <- "flag"



# Values of flag variable indicating inclusion in analysis.
## NOTE: the flag column will be 'trimmed' (leading/trailing spaces removed) and
# 'squished' (all whitespace replaced with single space). This is to avoid
# accidental exclusion due to invisible whitespace.
flags_include <- "include"


#### Input Processing ####

# If input is Excel: Supply worksheet to load
input_sheet_name <- "Sheet1"

# How many rows to read to determine data type
rows_to_read_column_type <- 10000




### BEGIN PROGRAM ##############################################################
code_version <- "0.0"

is_whole_number <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

data_dir <-
  file.path(project_root, "data", gene)

run_time <- base::Sys.time()

input_processing_session_info <- utils::sessionInfo()

input_data_dir <-
  file.path("S:", "Bayes_Variant_DLR", "data", gene, "raw_input")

input_path <-
  file.path(
    input_data_dir,
    input_data_file
  )

if(!file.exists(input_path)){
  stop(
    "Unable to load ", input_path, " - ",
    "Check `project_root` and `input_data_file` against ",
    "the input file location."
  )
} else {
  
  input_sha256 <-
    input_path %>%
    base::file() %>%
    openssl::sha256() %>%
    base::as.character() %>%
    base::as.character()
  
  input_info <-
    file.info(input_path)
  input_size <- input_info$size/2^(10)
  input_mod_time <- input_info$mtime
  
  processed_input_dir <-
    file.path(data_dir, "processed_input")
  
  if(!file.exists(processed_input_dir)){
    dir.create(path = processed_input_dir, recursive = TRUE)
  }
  
  file_prefix <-
    if(is.null(data_version_name)){
      data_version_name
    } else {
      paste0(c(gene, data_version_name), collapse = "-")
    }
  
  
  processed_xlsx_file <-
    paste0(file_prefix, "_processed_v", code_version, ".xlsx")
  
  processed_xlsx_path <-
    file.path(processed_input_dir, processed_xlsx_file)
  
  processed_rdata_file <-
    paste0(file_prefix, "_processed_v", code_version, ".Rdata")
  
  processed_rdata_path <-
    file.path(processed_input_dir, processed_rdata_file)
  
  xlsx_workbook <-
    openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "1. Analytic Data",
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "2. Metadata",
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "3. Flag - Not Included",
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "4. Invalid Data",
  )
  
  openxlsx::addWorksheet(
    wb = xlsx_workbook,
    sheetName = "5. Original Data",
  )
  
  input_extension <-
    stringr::str_extract(
      string = input_data_file,
      pattern = "\\.(.*)$"
    ) %>% 
    stringr::str_remove(
      string = .,
      pattern = "^\\."
    )
  
  if(input_extension %in% c("xlsx", "xls")){
    raw_input <-
      readxl::read_excel(
        path = input_path,
        sheet = input_sheet_name,
        guess_max = rows_to_read_column_type
      )
  } else if(input_extension %in% c("csv")){
    raw_input <-
      readr::read_csv(
        file = input_path,
        guess_max = rows_to_read_column_type
      )
  } else{
    stop("Unsupported input extension `", input_extension, "`")
  }
  
  input_columns <- colnames(raw_input)
  
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "5. Original Data",
    x = raw_input
  )
  
  raw_input <-
    raw_input %>% 
    dplyr::mutate(
      .row_number = dplyr::row_number()
    )
  
  
  # Determine if a flag is in the data
  has_flag <- 
    ifelse(
      test = !is.null(flag_column),
      yes = flag_column %in% names(raw_input),
      no = FALSE
    )
  
  
  if(!is.null(flag_column) & !has_flag) {
    stop("Flag variable `", flag_column, "` not found.")
  }
  
  
  # Select relevant columns: Standardize names.
  # If a flag exists, trim/squish flag, keep rows flagged for inclusion.
  selected_input <-
    raw_input %>% 
    dplyr::select(
      dplyr::all_of(
        x = c(variant_id, case_denominator, case_count, 
              control_denominator, control_count, ".row_number")
      ),
      dplyr::any_of(x = c(flag_column))
    ) %>% 
    dplyr::rename(
      variant_id = !!dplyr::sym(variant_id),
      n_0 = !!dplyr::sym(case_denominator),
      y_0 = !!dplyr::sym(case_count),
      n_1 = !!dplyr::sym(control_denominator),
      y_1 = !!dplyr::sym(control_count)
    ) %>% 
    dplyr::mutate(
      row = dplyr::row_number()
    )
  
  
  if(has_flag){
    input_included <-
      selected_input %>% 
      dplyr::rename(
        .data = .,
        flag = !!dplyr::sym(flag_column)
      ) %>%
      dplyr::mutate(
        flag =
          stringr::str_trim(
            string = flag,
            side = "both"
          ) %>% 
          stringr::str_squish()
      ) %>% 
      dplyr::filter(
        flag %in% flags_include
      )
  } else {
    input_included <- selected_input
  }
  
  
  # Write out rows of original data not flagged for inclusion
  input_not_included <-
    dplyr::anti_join(
      x = raw_input,
      y = input_included,
      by = ".row_number"
    )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "3. Flag - Not Included",
    x = input_not_included
  )
  
  
  # If any columns are not numeric, retype as numeric. Check for non-numeric data.
  input_included <-
    input_included %>% 
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(
          x = c("n_0", "y_0", "n_1", "y_1")
        ),
        .fns = as.numeric
      ),
      
      data_check =
        case_when(
          is.na(n_0) + is.na(y_0) > 0 ~ "Missing data: controls",
          is.na(n_1) + is.na(y_1) > 0 ~ "Missing data: cases",
          is_whole_number(n_0) + is_whole_number(n_1) +
            is_whole_number(y_0) + is_whole_number(y_1) < 4 ~
            "Non-integer values in data",
          n_0 == 0 ~ "No observations in controls",
          n_1 == 0 ~ "No observations in cases",
          y_0 > n_0 ~ "More counts in controls than observations",
          y_1 > n_1 ~ "More counts in cases than observations",
          TRUE ~ "Pass"
        )
    )
  
  
  # Check for invalid input
  input_invalid <-
    input_included %>% 
    dplyr::filter(
      data_check != "Pass"
    )
  
  if(stop_on_data_error){
    stop("Data entry errors in rows ", 
         paste(input_invalid$row, collapse = ", "))
  } else{
    warning(
      "Data entry errors in rows ", 
      paste(input_invalid$row, collapse = ", "), " - ",
      "It is strongly recommended to check all data before proceeding."
    )
  }
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "4. Invalid Data",
    x = input_invalid
  )
  
  
  analytic_data <-
    dplyr::anti_join(
      x = input_included,
      y = input_invalid,
      by = ".row_number"
    )
  
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "1. Analytic Data",
    x = analytic_data
  )
  
  
  input_metadata <-
    dplyr::bind_rows(
      dplyr::tibble(Variable = "Run Time", Value = as.character(run_time)),
      dplyr::tibble(Variable = "R Version", Value = input_processing_session_info$R.version$version.string),
      dplyr::tibble(Variable = "Data Processing Code Version", Value = code_version),
      dplyr::tibble(Variable = "Gene", Value = gene),
      dplyr::tibble(Variable = "Project Directory", Value = data_dir),
      dplyr::tibble(Variable = "Rows Read to Determine Column Type", 
                    Value = as.character(rows_to_read_column_type)),
      dplyr::tibble(Variable = "File Name", Value = input_data_file),
      dplyr::tibble(Variable = "File SHA256", Value = input_sha256),
      dplyr::tibble(Variable = "File Last Modified",
                    Value = as.character(input_mod_time)),
      dplyr::tibble(Variable = "File Size (KB)", 
                    Value = as.character(input_size)),
      dplyr::tibble(Variable = "Input Rows", 
                    Value = as.character(nrow(raw_input))),
      dplyr::tibble(Variable = "Input Columns", 
                    Value = as.character(ncol(raw_input))),
      dplyr::tibble(Variable = "Input Column Names", 
                    Value = paste(colnames(input_columns), collapse = "|")),
      
      dplyr::tibble(Variable = "Variant ID Column", Value = variant_id),
      dplyr::tibble(Variable = "Case Denominator Column",
                    Value = case_denominator),
      dplyr::tibble(Variable = "Case Count Column", Value = case_count),
      dplyr::tibble(Variable = "Control Denominator Column",
                    Value = control_denominator),
      dplyr::tibble(Variable = "Control Count Column", Value = control_count),
      dplyr::tibble(
        Variable = "Flag Column",
        Value = 
          ifelse(
            test = is.null(flag_column),
            yes = "(none specified)",
            no = flag_column
          )),
      dplyr::tibble(
        Variable = "Flags to Include",
        Value = 
          ifelse(
            test = is.null(flag_column),
            yes = "",
            no = paste0(flags_include, collapse = "|")
          )
      ),
      dplyr::tibble(Variable = "Rows not flagged for inclusion", 
                    Value = as.character(nrow(input_not_included))),
      dplyr::tibble(Variable = "Rows with invalid data", 
                    Value = as.character(nrow(input_invalid))),
    )
  
  openxlsx::writeData(
    wb = xlsx_workbook,
    sheet = "2. Metadata",
    x = input_metadata
  )
  
  input_processing_parameters <-
    list(
      "code_version" = code_version,
      "variant_id" = variant_id,
      "input_data_file" = input_data_file,
      "input_path" = input_path,
      "case_denominator" = case_denominator,
      "case_count" = case_count,
      "control_denominator" = control_denominator,
      "control_count" = control_count,
      "flag_column" = flag_column,
      "flags_include" = flags_include,
      "input_sheet_name" = input_sheet_name,
      "rows_to_read_column_type" = rows_to_read_column_type,
      "flag_column" = flag_column,
      "processed_input_dir" = processed_input_dir,
      "processed_xlsx_file" = processed_xlsx_file,
      "processed_rdata_file" = processed_rdata_file
    )
  
  input_file_information <-
    list(
      "input_data_file" = input_data_file,
      "input_path" = input_path,
      "input_size" = input_size,
      "input_mod_time" = input_mod_time,
      "input_sha256" = input_sha256,
      "input_processing_session_info" = input_processing_session_info
    )
  
  
  if(file.exists(processed_xlsx_path) & !overwrite_existing_output){
    warning(
      processed_xlsx_path, " exitsts and `overwrite_existing_output` is ",
      "set to FALSE: Output will not be saved over this file."
    )
  } else {
    openxlsx::saveWorkbook(
      wb = xlsx_workbook,
      file = processed_xlsx_path,
      overwrite = overwrite_existing_output
    )
  }
  
  if(file.exists(processed_rdata_path) & !overwrite_existing_output){
    warning(
      processed_rdata_path, " exitsts and `overwrite_existing_output` is ",
      "set to FALSE: Output will not be saved over this file."
    )
  } else {
    base::save(
      file = processed_rdata_path,
      list = 
        c("analytic_data", "input_invalid", "input_not_included", "raw_input",
          "run_time", "input_processing_session_info", "gene", "data_version_name",
          "data_dir", "input_data_file", "input_file_information",
          "input_processing_parameters")
    )
  }
}