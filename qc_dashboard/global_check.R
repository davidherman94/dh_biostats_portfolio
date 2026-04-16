# Global data quality checks (EMA RW-DQF: Consistency, Completeness)

#' Tag duplicate records (Consistency dimension)
#'
#' @param data Dataframe to check for duplicates
#' @param exclude_cols Columns to exclude from duplicate detection (e.g., IDs)
#' @return Dataframe with tag_duplicate column
tag_duplicates <- function(data, exclude_cols = NULL) {
  check_data <- data

  if (!is.null(exclude_cols)) {
    cols_to_remove <- intersect(exclude_cols, names(data))
    if (length(cols_to_remove) > 0) {
      check_data <- data[, setdiff(names(data), cols_to_remove), drop = FALSE]
    }
  }

  dup_groups <- check_data %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::group_by(dplyr::across(-".row_id")) %>%
    dplyr::mutate(
      tag_duplicate = dplyr::if_else(
        dplyr::n() > 1,
        dplyr::cur_group_id(),
        NA_integer_
      )
    ) %>%
    dplyr::ungroup()

  data$tag_duplicate <- dup_groups$tag_duplicate

  return(data)
}

#' Count missing values (Completeness dimension)
#'
#' @param data Dataframe to check
#' @param vars Columns to check (default: all)
#' @return Dataframe with missing value counts per variable
check_missing <- function(data, vars = NULL) {
  if (is.null(vars)) {
    vars <- names(data)
  }

  # Subset using base R to avoid select conflicts
  data_subset <- data[, vars, drop = FALSE]

  missing_values <- purrr::map_int(data_subset, ~ sum(is.na(.x)))

  tibble::tibble(
    variable = names(missing_values),
    n_total = nrow(data_subset),
    n_missing = missing_values
  ) %>%
    dplyr::mutate(
      perc_missing = round(n_missing / n_total * 100, 1),
      completeness = 100 - perc_missing
    )
}

#' Count modalities for categorical variables (Consistency dimension)
#'
#' @param data Dataframe
#' @param exclude_ids Columns to exclude (e.g., identifiers)
#' @param vars Specific columns to check (default: all categorical)
#' @return Dataframe with modality counts
check_modalities <- function(data, exclude_ids = NULL, vars = NULL) {
  # Identify categorical columns using base R
  if (is.null(vars)) {
    cat_cols <- names(data)[sapply(data, function(x) {
      is.character(x) | is.factor(x)
    })]
  } else {
    cat_cols <- intersect(vars, names(data))
  }

  # Exclude IDs
  if (!is.null(exclude_ids)) {
    cat_cols <- setdiff(cat_cols, exclude_ids)
  }

  if (length(cat_cols) == 0) {
    return(tibble::tibble(
      variable = character(),
      modalities = character(),
      n = integer()
    ))
  }

  # Subset using base R
  data_subset <- data[, cat_cols, drop = FALSE]

  purrr::map_df(names(data_subset), function(var_name) {
    data_subset %>%
      dplyr::rename(modalities = !!rlang::sym(var_name)) %>%
      dplyr::group_by(modalities) %>%
      dplyr::summarise(
        variable = var_name,
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(n)) %>%
      dplyr::filter(!is.na(modalities))
  }) %>%
    dplyr::select(variable, modalities, n)
}

#' Check units for clinical variables (Consistency dimension)
#'
#' @param data Dataframe with clinical measurements
#' @param measure_column Column with measure names
#' @param value_column Column with values
#' @param unit_column Column with units
#' @return Dataframe with unit counts and value ranges
check_units <- function(
  data,
  measure_column = NULL,
  value_column = NULL,
  unit_column = NULL
) {
  if (is.null(unit_column) || !unit_column %in% names(data)) {
    return(tibble::tibble(
      measure = character(),
      unit = character(),
      N = integer(),
      range = character()
    ))
  }

  if (!is.null(measure_column) && measure_column %in% names(data)) {
    units_df <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(
        measure_column,
        unit_column
      )))) %>%
      dplyr::summarise(
        N = dplyr::n(),
        range = paste(
          range(!!rlang::sym(value_column), na.rm = TRUE),
          collapse = " - "
        ),
        .groups = "drop"
      ) %>%
      dplyr::rename(
        measure = !!rlang::sym(measure_column),
        unit = !!rlang::sym(unit_column)
      ) %>%
      dplyr::arrange(dplyr::desc(N))
  } else if (!is.null(value_column)) {
    # Subset columns using base R
    cols_needed <- c(unit_column, value_column)
    data_subset <- data[, cols_needed, drop = FALSE]

    units_df <- data_subset %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(value_column),
        names_to = "measure"
      ) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::group_by(measure, !!rlang::sym(unit_column)) %>%
      dplyr::summarise(
        N = dplyr::n(),
        range = paste(range(value, na.rm = TRUE), collapse = " - "),
        .groups = "drop"
      ) %>%
      dplyr::rename(unit = !!rlang::sym(unit_column)) %>%
      dplyr::arrange(dplyr::desc(N))
  } else {
    units_df <- tibble::tibble(
      measure = character(),
      unit = character(),
      N = integer(),
      range = character()
    )
  }

  return(units_df)
}

#' Check uniqueness of identifier columns (Consistency dimension)
#'
#' @param data Dataframe
#' @param id_column Name of the identifier column
#' @return List with uniqueness statistics
check_uniqueness <- function(data, id_column) {
  if (!id_column %in% names(data)) {
    return(list(
      id_column = id_column,
      total_records = nrow(data),
      unique_ids = NA,
      duplicate_ids = NA,
      is_unique = NA,
      message = "Column not found"
    ))
  }

  total <- nrow(data)
  unique_count <- dplyr::n_distinct(data[[id_column]], na.rm = TRUE)
  na_count <- sum(is.na(data[[id_column]]))

  list(
    id_column = id_column,
    total_records = total,
    unique_ids = unique_count,
    duplicate_ids = total - unique_count - na_count,
    missing_ids = na_count,
    is_unique = (unique_count + na_count) == total
  )
}


#' Check data currency (Timeliness dimension - Currency subdimension)
#' Assesses how up-to-date the dataset is relative to a reference date
#'
#' @param data Dataframe
#' @param date_columns Vector of date column names
#' @param reference_date Reference date for currency assessment (default: Sys.Date())
#' @param data_cut_date Date when data was extracted/cut (if known)
#' @return Summary of data currency metrics
check_currency <- function(
  data,
  date_columns,
  reference_date = Sys.Date(),
  data_cut_date = NULL
) {
  date_columns <- intersect(date_columns, names(data))

  if (length(date_columns) == 0) {
    return(tibble::tibble(
      metric = "No date columns found",
      value = NA_character_
    ))
  }

  # Find most recent data point across all date columns
  max_dates <- purrr::map(date_columns, function(col) {
    max(safe_as_date(data[[col]]), na.rm = TRUE)
  })

  latest_data_point <- max(do.call(c, max_dates), na.rm = TRUE)

  # Data lag: time between last data point and reference/cut date

  if (!is.null(data_cut_date)) {
    data_lag <- as.numeric(as.Date(data_cut_date) - latest_data_point)
    availability_lag <- as.numeric(reference_date - as.Date(data_cut_date))
  } else {
    data_lag <- NA_real_
    availability_lag <- NA_real_
  }

  days_since_latest <- as.numeric(reference_date - latest_data_point)

  tibble::tibble(
    metric = c(
      "latest_data_point",
      "reference_date",
      "days_since_latest_record",
      "data_cut_date",
      "data_lag_days",
      "availability_lag_days"
    ),
    value = c(
      as.character(latest_data_point),
      as.character(reference_date),
      as.character(days_since_latest),
      ifelse(
        is.null(data_cut_date),
        "Not specified",
        as.character(data_cut_date)
      ),
      ifelse(is.na(data_lag), "Not calculable", as.character(data_lag)),
      ifelse(
        is.na(availability_lag),
        "Not calculable",
        as.character(availability_lag)
      )
    )
  )
}
