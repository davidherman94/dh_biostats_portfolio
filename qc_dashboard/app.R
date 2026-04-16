# QC Shiny App - Main Application
# Aligned with EMA RW-DQF (EMA/503781/2024)

# Source external scripts
source('../qc_shiny/global_check.R')
source('../qc_shiny/vitals.R')
source('../qc_shiny/date_check.R')
source('../qc_shiny/config.R')
source('../qc_shiny/functions.R')

# Load packages
load_libraries(PACKAGES)

# Google Cloud authentication

# Collect dataset names
data_to_qc <- grep("", ls(), value = TRUE) %>% set_names()

# ------------------------------------------------------------------------------
# EMA RW-DQF Data Quality Dimensions (EMA/503781/2024)
# Structure: Dimension > Subdimension
# ------------------------------------------------------------------------------
dq_framework <- list(
  extensiveness = list(
    title = "Extensiveness",
    color = "#3498db",
    subdimensions = list(
      completeness = "Presence of expected data values",
      coverage = "Population and temporal representativeness"
    )
  ),
  coherence = list(
    title = "Coherence",
    color = "#e74c3c",
    subdimensions = list(
      format = "Adherence to specified formats and standards",
      structural = "Relational integrity across tables",
      semantic = "Consistent meaning of coded values",
      uniqueness = "Absence of unintended duplicates"
    )
  ),
  reliability = list(
    title = "Reliability",
    color = "#2ecc71",
    subdimensions = list(
      accuracy = "Correspondence to true values",
      precision = "Level of detail in measurements",
      plausibility = "Believability given clinical/biological constraints"
    )
  ),
  timeliness = list(
    title = "Timeliness",
    color = "#9b59b6",
    subdimensions = list(
      currency = "How up-to-date are the data"
    )
  )
)

# Helper: Generate dimension card HTML
render_dimension_card <- function(dim_info) {
  subdim_items <- purrr::imap(dim_info$subdimensions, function(desc, name) {
    tags$li(tags$b(tools::toTitleCase(name)), ": ", desc)
  })

  tags$div(
    style = paste0(
      "border-left: 4px solid ",
      dim_info$color,
      "; padding-left: 10px; margin-bottom: 10px;"
    ),
    tags$h5(tags$b(dim_info$title)),
    tags$ul(
      style = "font-size: 11px; color: #555; margin: 0; padding-left: 15px;",
      subdim_items
    )
  )
}

# UI
ui <- fluidPage(
  titlePanel("Data Quality Control (EMA RW-DQF)"),

  # Framework description header
  fluidRow(
    column(
      width = 12,
      wellPanel(
        style = "padding: 10px;",
        fluidRow(
          column(
            12,
            tags$p(
              style = "font-size: 12px; color: #666; margin-bottom: 10px;",
              "Based on Data Quality Framework for EU medicines regulation: application to Real-World Data (EMA, 2026). ",
              "Dimensions and subdimensions below map to automated checks."
            )
          ),
          column(3, render_dimension_card(dq_framework$extensiveness)),
          column(3, render_dimension_card(dq_framework$coherence)),
          column(3, render_dimension_card(dq_framework$reliability)),
          column(3, render_dimension_card(dq_framework$timeliness))
        )
      )
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("dataset", "Select Dataset:", choices = data_to_qc),
      hr(),
      h4("Dataset Summary"),
      verbatimTextOutput("summary")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",

        # EXTENSIVENESS TAB
        tabPanel(
          "Extensiveness",
          h4("Completeness"),
          tags$p(
            class = "text-muted",
            "Missing data patterns and completeness rates."
          ),
          plotlyOutput("missing_plot"),
          hr(),
          reactableOutput("missing_table"),
          hr(),
          h4("Coverage"),
          tags$p(
            class = "text-muted",
            "Temporal and population coverage summary."
          ),
          reactableOutput("coverage_table")
        ),

        # COHERENCE TAB
        tabPanel(
          "Coherence",
          h4("Uniqueness"),
          tags$p(class = "text-muted", "Detection of duplicate records."),
          plotlyOutput("dup_plot"),
          hr(),
          h4("Format"),
          tags$p(
            class = "text-muted",
            "Categorical value validation and unit standardization."
          ),
          reactableOutput("modalities_table"),
          hr(),
          h4("Semantic"),
          tags$p(class = "text-muted", "Unit consistency across measurements."),
          reactableOutput("units_table")
        ),

        # RELIABILITY TAB
        tabPanel(
          "Reliability",
          h4("Plausibility"),
          tags$p(
            class = "text-muted",
            "Temporal sequence and value range validation."
          ),
          reactableOutput("date_checks_table"),
          hr(),
          h4("Accuracy"),
          tags$p(
            class = "text-muted",
            "Value distributions and outlier detection."
          ),
          plotOutput("outliers_plot"),
          reactableOutput("outliers_table"),
          hr(),
          h4("Precision"),
          tags$p(
            class = "text-muted",
            "Numeric variable distribution summary."
          ),
          reactableOutput("precision_table")
        ),

        # TIMELINESS TAB
        tabPanel(
          "Timeliness",
          h4("Currency"),
          tags$p(class = "text-muted", "Data recency assessment."),
          reactableOutput("currency_table"),
          hr()
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive dataset selection
  selected_data <- reactive({
    req(input$dataset)
    get(input$dataset, envir = .GlobalEnv)
  })

  # Dataset summary
  output$summary <- renderPrint({
    data <- selected_data()
    summary_list <- generate_summary(data)
    cat(
      "Rows:",
      summary_list$n_rows,
      "\n",
      "Columns:",
      summary_list$n_cols,
      "\n",
      "Numeric:",
      summary_list$n_numeric,
      "\n",
      "Categorical:",
      summary_list$n_categorical,
      "\n",
      "Date columns:",
      summary_list$n_date_cols,
      "\n",
      "Memory (MB):",
      summary_list$memory_mb
    )
  })

  # ---------------------------------------------------------------------------
  # EXTENSIVENESS: Completeness
  # ---------------------------------------------------------------------------
  output$missing_plot <- renderPlotly({
    data <- selected_data()
    missing_df <- check_missing(data)

    p <- missing_df %>%
      mutate(
        variable = factor(variable, levels = variable[order(perc_missing)])
      ) %>%
      ggplot(aes(
        x = variable,
        y = perc_missing,
        fill = perc_missing,
        text = paste0(
          "Variable: ",
          variable,
          "\n",
          "Missing: ",
          n_missing,
          " / ",
          n_total,
          "\n",
          "Percent: ",
          perc_missing,
          "%"
        )
      )) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(
        low = "#2ecc71",
        high = "#e74c3c",
        limits = c(0, 100),
        name = "% Missing"
      ) +
      coord_flip() +
      labs(x = "Variable", y = "% Missing") +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  output$missing_table <- renderReactable({
    data <- selected_data()
    missing_df <- check_missing(data)
    reactable(
      missing_df,
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 10
    )
  })

  # EXTENSIVENESS: Coverage
  output$coverage_table <- renderReactable({
    data <- selected_data()
    date_cols <- identify_date_columns(data, pattern = DATE_PATTERN)

    if (length(date_cols) == 0) {
      return(reactable(tibble(
        message = "No date columns for coverage analysis"
      )))
    }

    coverage_df <- purrr::map_df(date_cols, function(col) {
      dates <- safe_as_date(data[[col]])
      tibble(
        variable = col,
        min_date = as.character(min(dates, na.rm = TRUE)),
        max_date = as.character(max(dates, na.rm = TRUE)),
        span_days = as.numeric(
          max(dates, na.rm = TRUE) - min(dates, na.rm = TRUE)
        ),
        n_records = sum(!is.na(dates))
      )
    })

    reactable(coverage_df, searchable = TRUE, filterable = TRUE)
  })

  # ---------------------------------------------------------------------------
  # COHERENCE: Uniqueness
  # ---------------------------------------------------------------------------
  output$dup_plot <- renderPlotly({
    data <- selected_data()
    tagged_df <- tag_duplicates(data)

    total_records <- nrow(data)
    duplicated_records <- sum(!is.na(tagged_df$tag_duplicate))
    unique_count <- total_records - duplicated_records

    dup_data <- data.frame(
      Type = c("Unique Records", "Duplicate Records"),
      Count = c(unique_count, duplicated_records),
      Percentage = c(
        round(unique_count / total_records * 100, 2),
        round(duplicated_records / total_records * 100, 2)
      )
    )

    p <- ggplot(
      dup_data,
      aes(
        x = Type,
        y = Count,
        fill = Type,
        text = paste0(
          "Type: ",
          Type,
          "\n",
          "Count: ",
          Count,
          " / ",
          total_records,
          "\n",
          "Percent: ",
          Percentage,
          "%"
        )
      )
    ) +
      geom_bar(stat = "identity") +
      geom_text(
        aes(label = paste0(Count, " (", Percentage, "%)")),
        vjust = -0.5,
        size = 3.5
      ) +
      scale_fill_manual(
        values = c(
          "Duplicate Records" = "#e74c3c",
          "Unique Records" = "#3498db"
        )
      ) +
      labs(y = "Count", x = "") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p, tooltip = "text")
  })

  # COHERENCE: Format (Modalities)
  output$modalities_table <- renderReactable({
    data <- selected_data()
    modalities_df <- check_modalities(data)

    if (nrow(modalities_df) == 0) {
      return(reactable(tibble(message = "No categorical variables found")))
    }

    reactable(
      modalities_df,
      searchable = TRUE,
      filterable = TRUE,
      groupBy = "variable",
      defaultPageSize = 15
    )
  })

  # COHERENCE: Semantic (Units)
  output$units_table <- renderReactable({
    data <- selected_data()

    unit_col <- names(data)[grepl("unit", names(data), ignore.case = TRUE)][1]
    value_col <- names(data)[grepl(
      "value|result",
      names(data),
      ignore.case = TRUE
    )][1]
    measure_col <- names(data)[grepl(
      "measure|test|param",
      names(data),
      ignore.case = TRUE
    )][1]

    if (is.na(unit_col)) {
      return(reactable(tibble(message = "No unit column detected")))
    }

    units_df <- check_units(
      data,
      measure_column = measure_col,
      value_column = value_col,
      unit_column = unit_col
    )
    reactable(
      units_df,
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 15
    )
  })

  # ---------------------------------------------------------------------------
  # RELIABILITY: Plausibility (Date checks)
  # ---------------------------------------------------------------------------
  output$date_checks_table <- renderReactable({
    data <- selected_data()
    date_cols <- identify_date_columns(data, pattern = DATE_PATTERN)

    if (length(date_cols) == 0) {
      return(reactable(tibble(message = "No date columns detected")))
    }

    checked_data <- data
    checked_data <- tag_future_dates(checked_data, date_cols, REFERENCE_DATE)

    if ("birth_date" %in% names(checked_data)) {
      checked_data <- tag_date_before_birth(
        checked_data,
        "birth_date",
        date_cols
      )
    }
    if ("death_date" %in% names(checked_data)) {
      checked_data <- tag_date_after_death(
        checked_data,
        "death_date",
        date_cols
      )
    }

    date_summary <- summarize_date_issues(checked_data)

    if (nrow(date_summary) == 0) {
      return(reactable(tibble(message = "No plausibility issues detected")))
    }

    reactable(
      date_summary,
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 10
    )
  })

  # RELIABILITY: Accuracy (Outliers)
  output$outliers_plot <- renderPlot({
    data <- selected_data()
    value_col <- names(data)[grepl(
      "value|result",
      names(data),
      ignore.case = TRUE
    )][1]

    if (is.na(value_col) || !is.numeric(data[[value_col]])) {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No numeric value column detected"
          ) +
          theme_void()
      )
    }

    ggplot(data, aes(x = "", y = !!sym(value_col))) +
      geom_boxplot(
        fill = "#3498db",
        outlier.color = "#e74c3c",
        outlier.size = 2
      ) +
      labs(x = "", y = value_col) +
      theme_minimal()
  })

  output$outliers_table <- renderReactable({
    data <- selected_data()

    value_col <- names(data)[grepl(
      "value|result",
      names(data),
      ignore.case = TRUE
    )][1]
    vital_col <- names(data)[grepl(
      "measure|test|param|vital",
      names(data),
      ignore.case = TRUE
    )][1]

    if (is.na(value_col)) {
      return(reactable(tibble(message = "No value column detected")))
    }

    tagged_data <- data
    if (!is.na(vital_col)) {
      tagged_data <- tag_vitals_from_config(
        data = tagged_data,
        vital_value = value_col,
        vital_column = vital_col,
        ranges_list = VITAL_RANGES
      )
    }

    outlier_summary <- summarize_outliers(tagged_data)

    if (nrow(outlier_summary) == 0) {
      return(reactable(tibble(message = "No outliers detected")))
    }

    reactable(outlier_summary, searchable = TRUE, filterable = TRUE)
  })

  # RELIABILITY: Precision
  output$precision_table <- renderReactable({
    data <- selected_data()
    precision_df <- summarize_numeric_distribution(data)

    if (nrow(precision_df) == 0) {
      return(reactable(tibble(message = "No numeric variables found")))
    }

    reactable(
      precision_df,
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 10
    )
  })

  # TIMELINESS: Currency
  output$currency_table <- renderReactable({
    data <- selected_data()
    date_cols <- identify_date_columns(data, pattern = DATE_PATTERN)

    if (length(date_cols) == 0) {
      return(reactable(tibble(
        message = "No date columns for currency analysis"
      )))
    }

    # DATA_CUT_DATE should be defined in config.R if known
    cut_date <- if (exists("DATA_CUT_DATE")) DATA_CUT_DATE else NULL

    currency_df <- check_currency(
      data = data,
      date_columns = date_cols,
      reference_date = REFERENCE_DATE,
      data_cut_date = cut_date
    )

    reactable(currency_df, searchable = TRUE, filterable = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
