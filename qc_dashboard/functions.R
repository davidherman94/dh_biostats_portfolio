# Utility functions for QC Shiny App

### Connection to GCP --------------------------------------------------------

#' Import all RDS files from GCS bucket
import_rds_files_from_gcs <- function(bucket, prefix, file_pattern = "\\.rds") {
  files <- as.data.frame(gcs_list_objects(bucket = bucket, prefix = prefix)) %>%
    filter(str_detect(name, file_pattern) == TRUE) %>%
    mutate(file = sub(c("(^.+/)"), "", name)) %>%
    mutate(file = sub("\\.rds", "", file))

  for (i in 1:nrow(files)) {
    data <- gcs_get_object(
      files[i, "name"],
      bucket = bucket,
      parseFunction = gcs_parse_rds
    )
    assign(paste0(files[i, "file"]), data, envir = .GlobalEnv)
    rm(data)
  }

  return(paste0(nrow(files), " files successfully imported from ", bucket))
}

#' Import specific RDS file from GCS bucket
import_specific_rds_file <- function(bucket, prefix, specific_file_name) {
  files <- as.data.frame(gcs_list_objects(bucket = bucket, prefix = prefix)) %>%
    filter(str_detect(name, specific_file_name)) %>%
    mutate(file = sub(c("(^.+/)"), "", name)) %>%
    mutate(file = sub("\\.rds", "", file))

  if (nrow(files) == 1) {
    data <- gcs_get_object(
      files[1, "name"],
      bucket = bucket,
      parseFunction = gcs_parse_rds
    )
    assign(paste0(files[1, "file"]), data, envir = .GlobalEnv)
    rm(data)
    return(paste0(specific_file_name, " successfully imported"))
  } else {
    return(paste0("File ", specific_file_name, " not found in bucket ", bucket))
  }
}

### Load packages ------------------------------------------------------------

#' Load and install required packages
load_libraries <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

### Column identification ----------------------------------------------------

#' Identify date columns based on naming pattern
identify_date_columns <- function(data, pattern = "date") {
  names(data)[grepl(pattern, names(data), ignore.case = TRUE)]
}

#' Identify numeric columns
identify_numeric_columns <- function(data) {
  names(data)[sapply(data, is.numeric)]
}

#' Identify categorical columns (character or factor)
identify_categorical_columns <- function(data) {
  names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
}

### Data summary -------------------------------------------------------------

#' Generate dataset summary statistics
generate_summary <- function(data) {
  list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    n_numeric = length(identify_numeric_columns(data)),
    n_categorical = length(identify_categorical_columns(data)),
    n_date_cols = length(identify_date_columns(data)),
    memory_mb = round(object.size(data) / 1024^2, 2)
  )
}
