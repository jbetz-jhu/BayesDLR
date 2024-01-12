library(dplyr)
library(stringr)
library(openxlsx)

run_time <- base::Sys.time()

### Set Parameters #############################################################
# Load environmental parameters from callr::r call
data_subfolder <- Sys.getenv(x = "bayes_dlr_data_subfolder")
data_version_name <- Sys.getenv(x = "bayes_dlr_data_version_name")
config_path <- Sys.getenv(x = "bayes_dlr_config_path")
overwrite_existing_output <-
  Sys.getenv(x = "bayes_dlr_overwrite_existing_output")
project_root <-
  Sys.getenv(x = "bayes_dlr_project_root")


# For debugging only: Manually set parameters
data_subfolder <- "example_1"
data_version_name <- "V1-240110"
config_path <- file.path(getwd(), "analyses/example_1/V1-240110_r_k_10_pct.yml")
overwrite_existing_output <- TRUE
project_root <- getwd()



overwrite_existing_output <- as.logical(overwrite_existing_output)

rows_to_read_column_type <- 10000
code_version <- "0.0"


# Load configuration file
configuration <- config::get(file = config_path)

for(i in 1:length(configuration)) {
  assign(x = names(configuration)[i], value = configuration[[i]])
}

flags_include <-
  strsplit(x = flags_include, split = "|", fixed = TRUE)[[1]]




### Set up path structure ######################################################
data_dir <- file.path(project_root, "data", data_subfolder)
raw_data_dir <- file.path(data_dir, "raw_data")
processed_data_dir <- file.path(data_dir, "processed_data")




### Supplementary Code #########################################################
is_whole_number <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol




### Process Input File #########################################################
input_processing_session_info <- utils::sessionInfo()

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

input_type <-
  stringr::str_extract(
    string = input_file,
    pattern = "\\.(.*)$"
  ) %>%
  stringr::str_remove(
    string = .,
    pattern = "^\\."
  )

if(input_type %in% c("xlsx", "xls")){
  raw_input <-
    readxl::read_excel(
      path = input_path,
      sheet = input_sheet_name,
      guess_max = rows_to_read_column_type
    )
} else if(input_type %in% c("csv")){
  raw_input <-
    readr::read_csv(
      file = input_path,
      guess_max = rows_to_read_column_type
    )
} else{
  stop("Unsupported input extension `", input_extension, "`")
}




## Set Up Excel Workbook for Output ###########################################
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




### Save Original Data #########################################################
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




### Select Columns, Standardize Names, Keep Flagged Rows #######################
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
      x = c(row_id, case_denominator, case_count,
            control_denominator, control_count, ".row_number")
    ),
    dplyr::any_of(x = c(flag_column))
  ) %>%
  dplyr::rename(
    row_id = !!dplyr::sym(row_id),
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
      .fns = function(x) suppressWarnings(as.numeric(x))
    ),

    data_check =
      case_when(
        is.na(n_0) + is.na(y_0) > 0 ~ "Missing data: controls",
        is.na(n_1) + is.na(y_1) > 0 ~ "Missing data: cases",
        is_whole_number(n_0) + is_whole_number(n_1) +
          is_whole_number(y_0) + is_whole_number(y_1) < 4 ~
          "Non-integer values in data",
        y_0 < 0 ~ "Invalid numerator in controls",
        y_1 < 0 ~ "Invalid numerator in cases",
        y_0 == 0 & y_1 == 0 ~ "No observations in cases or controls",
        n_0 <= 0 ~ "Invalid denominator in controls",
        n_1 <= 0 ~ "Invalid denominator in cases",
        y_0 > n_0 ~ "More counts in controls than observations",
        y_1 > n_1 ~ "More counts in cases than observations",
        TRUE ~ "Pass"
      )
  )




### Check For Invalid Input - Save As Separate Sheet ###########################
input_invalid <-
  input_included %>%
  dplyr::filter(
    data_check != "Pass"
  )

any_invalid_rows <- nrow(input_invalid) > 0

if(any_invalid_rows){
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




### Create Output Paths ########################################################
file_prefix <-
  if(is.null(data_version_name)){
    data_version_name
  } else {
    paste0(c(data_subfolder, data_version_name), collapse = "-")
  }

processed_xlsx_file <-
  paste0(file_prefix, "_processed_v", code_version, ".xlsx")

processed_xlsx_path <-
  file.path(processed_data_dir, processed_xlsx_file)

processed_rdata_file <-
  paste0(file_prefix, "_processed_v", code_version, ".Rdata")

processed_rdata_path <-
  file.path(processed_data_dir, processed_rdata_file)




### Write Analytic Data & Metadata #############################################
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
  list(
    "Run Time"=  as.character(run_time),
    "R Version" = input_processing_session_info$R.version$version.string,
    "Data Processing Code Version" = code_version,
    "Data Subfolder" = data_subfolder,
    "Project Directory" = data_dir,
    "Rows Read to Determine Column Type" = rows_to_read_column_type,
    "File Name" = input_file,
    "File SHA256" = input_sha256,
    "File Last Modified" = input_mod_time,
    "File Size (KB)"= input_size,
    "Input Rows" = nrow(raw_input),
    "Input Columns" = ncol(raw_input),
    "Input Column Names" = paste(colnames(input_columns), collapse = "|"),
    "Row ID Column" = row_id,
    "Case Denominator Column" = case_denominator,
    "Case Count Column" = case_count,
    "Control Denominator Column" = control_denominator,
    "Control Count Column" = control_count,
    "Flag Column" =
      ifelse(
        test = is.null(flag_column),
        yes = "(none specified)",
        no = flag_column
      ),
    "Flags to Include" =
      ifelse(
        test = is.null(flag_column),
        yes = "",
        no = paste0(flags_include, collapse = "|")
      ),
    "Rows not flagged for inclusion" = nrow(input_not_included),
    "Rows with invalid data" = nrow(input_invalid)
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

openxlsx::writeData(
  wb = xlsx_workbook,
  sheet = "2. Metadata",
  x = input_metadata
)

input_processing_parameters <-
  list(
    "code_version" = code_version,
    "row_id" = row_id,
    "input_file" = input_file,
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
    "processed_data_dir" = processed_data_dir,
    "processed_xlsx_file" = processed_xlsx_file,
    "processed_rdata_file" = processed_rdata_file
  )

input_file_information <-
  list(
    "input_file" = input_file,
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
        "run_time", "input_processing_session_info", "data_subfolder",
        "data_version_name", "data_dir", "input_file", "input_file_information",
        "input_processing_parameters")
  )
}

list(
  any_invalid_rows = any_invalid_rows,
  invalid_rows = input_invalid$row
)