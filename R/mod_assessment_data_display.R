#' Assessment Data Display Module UI
#'
#' Shows raw evaluation data for selected assessment category
#' @param id Module namespace
#' @export
mod_assessment_data_display_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "card mb-4",
      div(
        class = "card-header bg-info text-white",
        h4(class = "mb-0",
           icon("table", class = "me-2"),
           "Selected Evaluation Data")
      ),
      div(
        class = "card-body",
        p(class = "text-muted mb-3",
          "View the complete assessment records for the selected category."),

        uiOutput(ns("data_display_content"))
      )
    )
  )
}

#' Assessment Data Display Module Server
#'
#' @param id Module namespace
#' @param selected_category Reactive containing the currently selected category key
#' @param category_data Reactive containing the filtered data for the selected category
#' @param data_dict Data dictionary
#' @export
mod_assessment_data_display_server <- function(id, selected_category, category_data, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$data_display_content <- renderUI({
      # Check if we have a selected category
      if (is.null(selected_category()) || is.null(category_data())) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle", class = "me-2"),
            "Select a category above to view detailed evaluation data."
          )
        )
      }

      data_info <- category_data()

      if (!data_info$has_data) {
        return(
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle", class = "me-2"),
            "No evaluation data available for the selected category."
          )
        )
      }

      # Display the data table
      div(
        h5(class = "mb-3",
           paste("Showing", nrow(data_info$data), "evaluation record(s) for:",
                 data_info$cat_info$name)),
        DT::DTOutput(ns("raw_data_table"))
      )
    })

    output$raw_data_table <- DT::renderDT({
      req(category_data())
      data_info <- category_data()
      req(data_info$has_data)

      # Get the raw data
      display_data <- data_info$data

      # Select relevant columns and format for display
      # Start with metadata columns
      meta_cols <- c("ass_date", "ass_faculty", "ass_level", "ass_specialty")
      meta_cols_present <- meta_cols[meta_cols %in% names(display_data)]

      # Add the category-specific fields
      value_cols <- data_info$cat_info$fields[data_info$cat_info$fields %in% names(display_data)]

      # Combine columns
      all_cols <- c(meta_cols_present, value_cols)

      # Filter to only columns that exist
      display_data <- display_data %>%
        dplyr::select(dplyr::any_of(all_cols))

      # Get readable column names from data dictionary
      col_labels <- sapply(names(display_data), function(field_name) {
        label <- data_dict %>%
          dplyr::filter(field_name == !!field_name) %>%
          dplyr::pull(field_label)

        if (length(label) > 0 && !is.na(label[1])) {
          return(label[1])
        }
        return(field_name)
      })

      # Decode choice values
      display_data_decoded <- display_data
      for (col_name in names(display_data)) {
        field_info <- data_dict %>%
          dplyr::filter(field_name == col_name)

        if (nrow(field_info) > 0 && !is.na(field_info$select_choices_or_calculations[1])) {
          choices_str <- field_info$select_choices_or_calculations[1]

          # Parse choices (format: "1, Label 1 | 2, Label 2")
          if (!is.na(choices_str) && choices_str != "") {
            choice_pairs <- strsplit(choices_str, "\\|")[[1]]
            choice_map <- list()

            for (pair in choice_pairs) {
              parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
              if (length(parts) >= 2) {
                code <- trimws(parts[1])
                label <- trimws(paste(parts[-1], collapse = ","))
                choice_map[[code]] <- label
              }
            }

            # Decode values
            display_data_decoded[[col_name]] <- sapply(display_data[[col_name]], function(val) {
              if (is.na(val) || val == "") return(val)
              val_str <- as.character(val)
              if (val_str %in% names(choice_map)) {
                return(choice_map[[val_str]])
              }
              return(val)
            })
          }
        }
      }

      # Create datatable
      DT::datatable(
        display_data_decoded,
        colnames = col_labels,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        filter = "top",
        extensions = 'Buttons'
      )
    })
  })
}
