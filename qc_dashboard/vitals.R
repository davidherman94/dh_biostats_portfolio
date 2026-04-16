# Vitals quality checks (EMA RW-DQF: Plausibility, Accuracy)

#' Tag outliers based on plausible ranges
#'
#' @param data Dataframe
#' @param vital_name Name for the tag column
#' @param vital_column Column containing vital type (optional)
#' @param vital_modality Modality to filter (optional)
#' @param vital_value Column containing the numeric value
#' @param min_vital Minimum plausible value
#' @param max_vital Maximum plausible value
#' @return Dataframe with outlier tags
tag_outliers <- function(
  data,
  vital_name,
  vital_column = NULL,
  vital_modality = NULL,
  vital_value,
  min_vital,
  max_vital
) {
  if (!"tag_outlier_vitals" %in% names(data)) {
    data <- data %>% mutate(tag_outlier_vitals = NA_character_)
  }

  if (!vital_value %in% names(data)) {
    return(data)
  }

  if (is.null(vital_column) && is.null(vital_modality)) {
    data <- data %>%
      mutate(
        tag_outlier_vitals = if_else(
          !between(!!sym(vital_value), min_vital, max_vital),
          vital_name,
          tag_outlier_vitals
        )
      )
  }

  if (!is.null(vital_column) && !is.null(vital_modality)) {
    if (!vital_column %in% names(data)) {
      return(data)
    }

    data <- data %>%
      mutate(
        tag_outlier_vitals = if_else(
          !between(!!sym(vital_value), min_vital, max_vital) &
            !!sym(vital_column) %in% vital_modality,
          vital_name,
          tag_outlier_vitals
        )
      )
  }

  return(data)
}

#' Apply multiple vital range checks from config
#'
#' @param data Dataframe
#' @param vital_value Column with numeric values
#' @param vital_column Column with vital type names (optional)
#' @param ranges_list Named list of ranges (from config)
#' @return Dataframe with outlier tags
tag_vitals_from_config <- function(
  data,
  vital_value,
  vital_column = NULL,
  ranges_list
) {
  for (vital_name in names(ranges_list)) {
    range_i <- ranges_list[[vital_name]]

    if (!is.null(vital_column)) {
      data <- tag_outliers(
        data = data,
        vital_name = vital_name,
        vital_column = vital_column,
        vital_modality = vital_name,
        vital_value = vital_value,
        min_vital = range_i$min,
        max_vital = range_i$max
      )
    } else {
      if (vital_value == vital_name || vital_value %in% names(data)) {
        data <- tag_outliers(
          data = data,
          vital_name = vital_name,
          vital_value = vital_value,
          min_vital = range_i$min,
          max_vital = range_i$max
        )
      }
    }
  }

  return(data)
}

#' Summarize outlier counts
#'
#' @param data Dataframe with tag_outlier_vitals column
#' @return Summary dataframe
summarize_outliers <- function(data) {
  if (!"tag_outlier_vitals" %in% names(data)) {
    return(tibble(vital = character(), n_outliers = integer()))
  }

  data %>%
    filter(!is.na(tag_outlier_vitals)) %>%
    group_by(vital = tag_outlier_vitals) %>%
    summarise(n_outliers = n(), .groups = "drop") %>%
    arrange(desc(n_outliers))
}

#' Generate numeric variable distribution summary (Accuracy dimension)
#'
#' @param data Dataframe
#' @param numeric_cols Columns to summarize (default: all numeric)
#' @return Summary statistics dataframe
summarize_numeric_distribution <- function(data, numeric_cols = NULL) {
  if (is.null(numeric_cols)) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
  }

  numeric_cols <- intersect(numeric_cols, names(data))

  if (length(numeric_cols) == 0) {
    return(tibble(
      variable = character(),
      n = integer(),
      mean = numeric(),
      sd = numeric(),
      min = numeric(),
      max = numeric()
    ))
  }

  map_df(numeric_cols, function(col) {
    vals <- data[[col]]
    tibble(
      variable = col,
      n = sum(!is.na(vals)),
      n_missing = sum(is.na(vals)),
      mean = round(mean(vals, na.rm = TRUE), 2),
      sd = round(sd(vals, na.rm = TRUE), 2),
      min = min(vals, na.rm = TRUE),
      q25 = quantile(vals, 0.25, na.rm = TRUE),
      median = median(vals, na.rm = TRUE),
      q75 = quantile(vals, 0.75, na.rm = TRUE),
      max = max(vals, na.rm = TRUE)
    )
  })
}
