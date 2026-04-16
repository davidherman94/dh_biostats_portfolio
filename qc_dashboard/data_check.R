# Date-related quality checks (EMA RW-DQF: Plausibility, Consistency)

#' Safe date conversion
#'
#' @param x Date column (character, Date, or POSIXt)
#' @return Date object or NA
safe_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.character(x)) {
    # Try multiple formats
    parsed <- lubridate::parse_date_time(
      x,
      orders = c("Ymd", "dmY", "mdY", "Ymd HMS", "dmY HMS", "Y"),
      quiet = TRUE
    )
    return(as.Date(parsed))
  }
  return(as.Date(NA))
}

#' Tag dates occurring after death (Plausibility dimension)
#'
#' @param data Dataframe
#' @param death_date Column name for death date
#' @param other_dates Vector of date column names to check
#' @return Dataframe with tag columns
tag_date_after_death <- function(data, death_date, other_dates) {
  if (!death_date %in% names(data)) {
    return(data)
  }

  other_dates <- intersect(other_dates, names(data))
  other_dates <- setdiff(other_dates, death_date)

  # Convert death_date safely
  death_vals <- safe_as_date(data[[death_date]])

  for (date_i in other_dates) {
    tag_name <- glue::glue("tag_{date_i}_after_death")
    date_vals <- safe_as_date(data[[date_i]])

    data[[tag_name]] <- dplyr::case_when(
      is.na(date_vals) | is.na(death_vals) ~ NA,
      date_vals > death_vals ~ TRUE,
      TRUE ~ FALSE
    )
  }

  return(data)
}

#' Tag dates occurring before birth (Plausibility dimension)
#'
#' @param data Dataframe
#' @param birth_date Column name for birth date
#' @param other_dates Vector of date column names to check
#' @return Dataframe with tag columns
tag_date_before_birth <- function(data, birth_date, other_dates) {
  if (!birth_date %in% names(data)) {
    return(data)
  }

  other_dates <- intersect(other_dates, names(data))
  other_dates <- setdiff(other_dates, birth_date)

  # Convert birth_date safely
  birth_vals <- safe_as_date(data[[birth_date]])

  for (date_i in other_dates) {
    tag_name <- glue::glue("tag_{date_i}_before_birth")
    date_vals <- safe_as_date(data[[date_i]])

    data[[tag_name]] <- dplyr::case_when(
      is.na(date_vals) | is.na(birth_vals) ~ NA,
      date_vals < birth_vals ~ TRUE,
      TRUE ~ FALSE
    )
  }

  return(data)
}

#' Tag dates in the future (Plausibility dimension)
#'
#' @param data Dataframe
#' @param date_columns Vector of date column names to check
#' @param reference_date Reference date (default: today)
#' @return Dataframe with tag columns
tag_future_dates <- function(data, date_columns, reference_date = Sys.Date()) {
  date_columns <- intersect(date_columns, names(data))

  for (date_i in date_columns) {
    tag_name <- glue::glue("tag_{date_i}_future")
    date_vals <- safe_as_date(data[[date_i]])

    data[[tag_name]] <- dplyr::case_when(
      is.na(date_vals) ~ NA,
      date_vals > reference_date ~ TRUE,
      TRUE ~ FALSE
    )
  }

  return(data)
}

#' Tag start dates after end dates (Consistency dimension)
#'
#' @param data Dataframe
#' @param start_date Column name for start date
#' @param end_date Column name for end date
#' @return Dataframe with tag column
tag_date_start_after_end <- function(data, start_date, end_date) {
  if (!all(c(start_date, end_date) %in% names(data))) {
    return(data)
  }

  tag_name <- glue::glue("tag_{start_date}_after_{end_date}")

  start_vals <- safe_as_date(data[[start_date]])
  end_vals <- safe_as_date(data[[end_date]])

  data[[tag_name]] <- dplyr::case_when(
    is.na(start_vals) | is.na(end_vals) ~ NA,
    start_vals > end_vals ~ TRUE,
    TRUE ~ FALSE
  )

  return(data)
}

#' Check date format consistency
#'
#' @param date_string Date string to check
#' @return Detected format or status
check_date_format <- function(date_string) {
  formats <- c(
    "Ymd",
    "Ymd_h",
    "Ymd_hm",
    "Ymd_hms",
    "dmY",
    "dmY_h",
    "dmY_hm",
    "dmY_hms",
    "Ydm",
    "Ydm_h",
    "Ydm_hm",
    "Ydm_hms",
    "Y",
    "Ym",
    "mY"
  )

  if (is.na(date_string)) {
    return(NA_character_)
  }

  fmts <- purrr::map(formats, function(fmt) {
    parsed_date <- try(
      lubridate::parse_date_time(date_string, order = fmt),
      silent = TRUE
    )
    if (!inherits(parsed_date, "try-error") && !is.na(parsed_date)) {
      return(fmt)
    }
    return(NULL)
  }) %>%
    purrr::compact() %>%
    unlist()

  if (length(fmts) == 0) {
    return("Unknown format")
  }
  if (length(fmts) == 1) {
    return(fmts)
  }
  if (length(fmts) > 1) return("Ambiguous format")
}

#' Vectorized date format check
#'
#' @param date_field Vector of date strings
#' @return Character vector of detected formats
check_dates_format <- function(date_field) {
  purrr::map_chr(date_field, check_date_format)
}

#' Summarize date quality issues
#'
#' @param data Dataframe with date tag columns
#' @return Summary dataframe of date issues
summarize_date_issues <- function(data) {
  tag_cols <- names(data)[grepl(
    "^tag_.*_future$|^tag_.*_after_death$|^tag_.*_before_birth$|^tag_.*_after_",
    names(data)
  )]

  if (length(tag_cols) == 0) {
    return(tibble::tibble(
      check = character(),
      n_total = integer(),
      n_issues = integer(),
      perc_issues = numeric()
    ))
  }

  purrr::map_df(tag_cols, function(col) {
    tibble::tibble(
      check = col,
      n_total = sum(!is.na(data[[col]])),
      n_issues = sum(data[[col]] == TRUE, na.rm = TRUE),
      perc_issues = round(n_issues / max(n_total, 1) * 100, 2)
    )
  }) %>%
    dplyr::arrange(dplyr::desc(n_issues))
}
