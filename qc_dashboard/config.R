# Configuration file for QC Shiny App
# Required packages
PACKAGES <- c(
  "shiny",
  "dplyr",
  "tidyr",
  "purrr",
  "ggplot2",
  "reactable",
  "stringr",
  "lubridate",
  "glue",
  "gargle",
  "googleCloudStorageR",
  "uuid",
  "plotly"
)

# GCS Configuration
GCS_BUCKET <- ""
GCS_PREFIX <- ""

# List of files to import from cloud storage  -------------------------------------
FILENAMES.GCS <- c(
)

# Date column identifier pattern
DATE_PATTERN <- "date"

# Plausibility thresholds for vitals (EMA RW-DQF plausibility checks)
VITAL_RANGES <- list(
  weight_kg = list(min = 0.5, max = 300),
  height_cm = list(min = 20, max = 250),
  heart_rate_bpm = list(min = 20, max = 250),
  systolic_bp_mmhg = list(min = 50, max = 250),
  diastolic_bp_mmhg = list(min = 20, max = 150),
  temperature_c = list(min = 30, max = 45),
  respiratory_rate = list(min = 5, max = 60),
  oxygen_saturation = list(min = 50, max = 100)
)

# Reference date for future date checks
REFERENCE_DATE <- Sys.Date()
