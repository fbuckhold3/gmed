#' Custom Assessment Detail Visualization Module UI
#'
#' Dynamic assessment visualization with proper categorization and label decoding
#' @param id Module namespace
#' @export
mod_assessment_detail_custom_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"
      ),
      tags$style(HTML("
        .assessment-category-btn {
          margin: 5px;
          padding: 10px 15px;
          border-radius: 5px;
          transition: all 0.3s ease;
        }
        .assessment-category-btn:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        .assessment-category-btn.active {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-color: #667eea;
        }
        .assessment-obs-btn {
          margin: 3px;
          font-size: 0.9rem;
          transition: all 0.3s ease;
        }
        .assessment-obs-btn:hover {
          transform: scale(1.05);
        }
        .assessment-obs-btn.active {
          background: linear-gradient(135deg, #17a2b8 0%, #138496 100%);
          color: white;
          border-color: #17a2b8;
        }
        .assessment-viz-container {
          background: white;
          padding: 20px;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-top: 20px;
        }
        .obs-subtype-container {
          animation-duration: 0.5s;
        }
        .obs-viz-content {
          animation-duration: 0.4s;
        }
      "))
    ),

    div(
      class = "card mb-4",
      div(
        class = "card-header bg-primary text-white",
        h4(class = "mb-0",
           icon("chart-bar", class = "me-2"),
           "Detailed Assessment Analysis")
      ),
      div(
        class = "card-body",
        p(class = "text-muted mb-3",
          "View your assessment data organized by rotation type and evaluation category."),

        # Category buttons
        uiOutput(ns("category_buttons")),

        # Visualization output
        uiOutput(ns("viz_output"))
      )
    )
  )
}

#' Custom Assessment Detail Visualization Module Server
#'
#' @param id Module namespace
#' @param rdm_data Reactive containing assessment data
#' @param record_id Reactive containing resident record_id
#' @param data_dict Data dictionary
#' @export
mod_assessment_detail_custom_server <- function(id, rdm_data, record_id, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Extract assessment categories from data dictionary
    assessment_categories <- reactive({
      req(data_dict)

      # Get all assessment fields
      ass_fields <- data_dict %>%
        dplyr::filter(grepl("^ass_", field_name)) %>%
        dplyr::filter(!field_name %in% c("ass_date", "ass_level", "ass_plus",
                                         "ass_delta", "ass_faculty", "ass_specialty",
                                         "ass_rotator", "ass_cc_quart"))

      # Define category patterns
      categories <- list(
        list(
          key = "cc_inbasket",
          name = "CC: Inbasket Management",
          pattern = "^ass_cc_inb_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_documentation",
          name = "CC: Documentation",
          pattern = "^ass_cc_doc_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_intern_semi",
          name = "CC: Intern Semi-Annual",
          pattern = "^ass_cc_int_semi_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_pgy2_semi",
          name = "CC: PGY2 Semi-Annual",
          pattern = "^ass_cc_pgy2_semi_",
          type = "numeric_scale"
        ),
        list(
          key = "cc_pgy3_semi",
          name = "CC: PGY3 Semi-Annual",
          pattern = "^ass_cc_pgy3_semi_",
          type = "numeric_scale"
        ),
        list(
          key = "day_assessments",
          name = "Day Assessments",
          pattern = "^ass_day_",
          type = "numeric_scale"
        ),
        list(
          key = "consults",
          name = "Consults",
          pattern = "^ass_cons_",
          type = "numeric_scale"
        ),
        list(
          key = "inpatient_intern",
          name = "Inpatient: Intern",
          pattern = "^ass_int_ip_",
          type = "numeric_scale"
        ),
        list(
          key = "inpatient_resident",
          name = "Inpatient: Resident",
          pattern = "^ass_res_ip_",
          type = "numeric_scale"
        ),
        list(
          key = "bridge",
          name = "Bridge Clinic",
          pattern = "^ass_bridge_",
          type = "numeric_scale"
        ),
        list(
          key = "observations",
          name = "Direct Observations",
          pattern = "^ass_obs_",
          type = "observation"
        )
      )

      # Populate field lists for each category
      categories_with_fields <- lapply(categories, function(cat) {
        cat$fields <- ass_fields %>%
          dplyr::filter(grepl(cat$pattern, field_name)) %>%
          dplyr::pull(field_name)

        cat$field_info <- ass_fields %>%
          dplyr::filter(grepl(cat$pattern, field_name))

        cat
      })

      # Only return categories that have fields
      categories_with_fields[sapply(categories_with_fields, function(x) length(x$fields) > 0)]
    })

    # Filter data for current resident
    resident_assessments <- reactive({
      req(rdm_data(), record_id())

      rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          tolower(redcap_repeat_instrument) == "assessment"
        )
    })

    # Track selected category
    selected_category <- reactiveVal(NULL)

    # Render category buttons with counts
    output$category_buttons <- renderUI({
      req(assessment_categories(), resident_assessments())

      cats <- assessment_categories()
      res_data <- resident_assessments()

      buttons <- lapply(cats, function(cat) {
        # Count distinct assessments with any data in this category
        count <- res_data %>%
          dplyr::select(dplyr::any_of(cat$fields)) %>%
          dplyr::filter(dplyr::if_any(dplyr::everything(),
                                     ~!is.na(.) & as.character(.) != "" & as.character(.) != "0")) %>%
          nrow()

        # Create button
        button_class <- "btn assessment-category-btn"
        if (!is.null(selected_category()) && selected_category() == cat$key) {
          button_class <- paste(button_class, "active btn-primary")
        } else {
          button_class <- paste(button_class, "btn-outline-primary")
        }

        actionButton(
          ns(paste0("cat_", cat$key)),
          paste0(cat$name, " (", count, ")"),
          class = button_class
        )
      })

      div(
        class = "d-flex flex-wrap",
        buttons
      )
    })

    # Handle category button clicks
    observe({
      cats <- assessment_categories()
      if (is.null(cats)) return()

      lapply(cats, function(cat) {
        observeEvent(input[[paste0("cat_", cat$key)]], {
          selected_category(cat$key)
        })
      })
    })

    # Prepare data for visualization
    current_category_data <- reactive({
      req(selected_category(), assessment_categories(), resident_assessments())

      cat_key <- selected_category()
      cats <- assessment_categories()
      cat_info <- cats[[which(sapply(cats, function(x) x$key == cat_key))]]
      res_data <- resident_assessments()

      # Filter to assessments with data in this category
      category_data <- res_data %>%
        dplyr::select(record_id, ass_date, ass_faculty, dplyr::any_of(cat_info$fields)) %>%
        dplyr::filter(dplyr::if_any(dplyr::all_of(cat_info$fields),
                                   ~!is.na(.) & as.character(.) != "" & as.character(.) != "0"))

      list(
        data = category_data,
        cat_info = cat_info,
        has_data = nrow(category_data) > 0,
        total_assessments = nrow(res_data)
      )
    })

    # Render plotly chart for scale categories
    output$category_plot <- plotly::renderPlotly({
      req(current_category_data()$has_data)
      req(current_category_data()$cat_info$type == "numeric_scale")

      viz_result <- create_scale_visualization_data(
        current_category_data()$data,
        current_category_data()$cat_info
      )

      req(viz_result$has_numeric)
      viz_result$plot
    })

    # === OBSERVATION HANDLING ===

    # Track selected observation type
    selected_obs_type <- reactiveVal(NULL)

    # Detect observation types from ass_obs_type field
    observation_types <- reactive({
      req(current_category_data()$has_data)
      req(current_category_data()$cat_info$type == "observation")

      data <- current_category_data()$data
      cat_info <- current_category_data()$cat_info

      # Get unique observation types from ass_obs_type field
      obs_type_values <- data %>%
        dplyr::filter(!is.na(ass_obs_type), ass_obs_type != "") %>%
        dplyr::pull(ass_obs_type) %>%
        unique() %>%
        sort()

      # Map observation type codes to names
      obs_type_names <- c(
        "1" = "Clinical Decision Making",
        "2" = "Advance Care Planning",
        "3" = "Educational Session",
        "4" = "Physical Exam",
        "5" = "Presentation",
        "6" = "Written H&P",
        "7" = "Daily Notes",
        "8" = "Patient Discharge",
        "9" = "Patient / Family Counseling",
        "10" = "Supervision of Intern/Acting Intern",
        "11" = "Procedure",
        "12" = "Multi-D Rounds",
        "13" = "Emergency Condition (Rapid/Code)"
      )

      # Build list of available types
      type_list <- lapply(obs_type_values, function(type_code) {
        list(
          code = type_code,
          name = obs_type_names[[as.character(type_code)]],
          fields = cat_info$fields,
          field_info = cat_info$field_info
        )
      })

      names(type_list) <- obs_type_values
      type_list
    })

    # Render observation type buttons
    output$obs_subtype_buttons <- renderUI({
      req(observation_types())

      obs_types <- observation_types()
      data <- current_category_data()$data

      buttons <- lapply(names(obs_types), function(type_code) {
        type_info <- obs_types[[type_code]]

        # Count assessments of this type
        count <- data %>%
          dplyr::filter(ass_obs_type == type_code) %>%
          nrow()

        actionButton(
          ns(paste0("obs_", type_code)),
          paste0(type_info$name, " (", count, ")"),
          class = if (!is.null(selected_obs_type()) && selected_obs_type() == type_code)
            "btn btn-sm assessment-obs-btn active btn-info"
          else "btn btn-sm assessment-obs-btn btn-outline-info"
        )
      })

      div(
        class = "obs-subtype-container animate__animated animate__fadeInDown",
        style = "margin: 15px 0; padding: 10px; background: #f8f9fa; border-radius: 8px;",
        h6(class = "mb-2", icon("stethoscope"), "Select Observation Type:"),
        div(class = "d-flex flex-wrap gap-2", buttons)
      )
    })

    # Track observation type button clicks
    observe({
      obs_types <- observation_types()
      if (is.null(obs_types)) return()

      lapply(names(obs_types), function(type_code) {
        observeEvent(input[[paste0("obs_", type_code)]], {
          selected_obs_type(type_code)
        })
      })
    })

    # Render observation plotly chart (for numeric fields)
    output$obs_plot <- plotly::renderPlotly({
      req(selected_obs_type(), observation_types(), current_category_data()$has_data)

      type_code <- selected_obs_type()
      type_info <- observation_types()[[type_code]]
      data <- current_category_data()$data

      # Filter to assessments of this observation type
      type_data <- data %>%
        dplyr::filter(ass_obs_type == type_code)

      viz_result <- create_observation_viz_data(type_data, type_info)
      req(viz_result$has_numeric)
      viz_result$plot
    })

    # Render observation data table (for text fields only)
    output$obs_table <- DT::renderDT({
      req(selected_obs_type(), observation_types(), current_category_data()$has_data)

      type_code <- selected_obs_type()
      type_info <- observation_types()[[type_code]]
      data <- current_category_data()$data

      # Filter to assessments of this observation type
      type_data <- data %>%
        dplyr::filter(ass_obs_type == type_code)

      viz_result <- create_observation_viz_data(type_data, type_info)
      req(viz_result$has_text)

      DT::datatable(
        viz_result$text_display_data,
        options = list(pageLength = 10, scrollX = TRUE, scrollY = "300px"),
        rownames = FALSE,
        escape = FALSE
      )
    })

    # Render observation visualization area
    output$obs_viz_area <- renderUI({
      req(selected_obs_type(), observation_types(), current_category_data()$has_data)

      type_code <- selected_obs_type()
      type_info <- observation_types()[[type_code]]
      data <- current_category_data()$data

      # Filter to assessments of this observation type
      type_data <- data %>%
        dplyr::filter(ass_obs_type == type_code)

      if (nrow(type_data) == 0) {
        return(
          div(
            class = "alert alert-info animate__animated animate__fadeIn mt-3",
            icon("info-circle"), " No data for this observation type yet."
          )
        )
      }

      create_observation_subtype_viz(type_data, type_info, ns)
    })

    # Render UI container for visualization
    output$viz_output <- renderUI({
      req(current_category_data())

      data_info <- current_category_data()

      if (!data_info$has_data) {
        return(
          div(
            class = "alert alert-info assessment-viz-container",
            icon("info-circle", class = "me-2"),
            strong("No data available for this category yet."),
            br(),
            sprintf("You have %d total assessments, but none have data in the %s fields.",
                   data_info$total_assessments, data_info$cat_info$name)
          )
        )
      }

      # Create visualization based on category type
      if (data_info$cat_info$type == "numeric_scale") {
        create_scale_visualization_ui(data_info$data, data_info$cat_info, ns)
      } else if (data_info$cat_info$type == "observation") {
        create_observation_visualization(data_info$data, data_info$cat_info, ns)
      } else {
        div(
          class = "alert alert-warning assessment-viz-container",
          icon("wrench", class = "me-2"),
          sprintf("Visualization for %s is under development.", data_info$cat_info$name)
        )
      }
    })
  })
}

#' Process data for scale visualization
#' @keywords internal
create_scale_visualization_data <- function(data, cat_info) {

  # Pivot data to long format
  viz_data <- data %>%
    dplyr::select(record_id, ass_date, ass_faculty, dplyr::all_of(cat_info$fields)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cat_info$fields),
      names_to = "field",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value), value != "", value != "0")

  if (nrow(viz_data) == 0) {
    return(list(
      has_data = FALSE,
      has_numeric = FALSE,
      display_data = data.frame(),
      plot = NULL
    ))
  }

  # Get field labels from cat_info
  field_labels <- setNames(
    cat_info$field_info$field_label,
    cat_info$field_info$field_name
  )

  # Add readable field names
  viz_data <- viz_data %>%
    dplyr::mutate(
      field_label = field_labels[field],
      # Try to convert to numeric for summarization
      value_numeric = suppressWarnings(as.numeric(value))
    )

  # Create summary by field
  summary_data <- viz_data %>%
    dplyr::filter(!is.na(value_numeric)) %>%
    dplyr::group_by(field_label) %>%
    dplyr::summarise(
      mean_score = mean(value_numeric, na.rm = TRUE),
      median_score = median(value_numeric, na.rm = TRUE),
      n_assessments = dplyr::n_distinct(record_id, ass_date),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(mean_score))

  # Create plotly chart if we have numeric data
  plot_obj <- NULL
  has_numeric <- nrow(summary_data) > 0

  if (has_numeric) {
    plot_obj <- plotly::plot_ly(
      data = summary_data,
      x = ~mean_score,
      y = ~reorder(field_label, mean_score),
      type = "bar",
      orientation = "h",
      marker = list(color = "#667eea"),
      text = ~paste0("Mean: ", round(mean_score, 2),
                    "<br>Median: ", round(median_score, 2),
                    "<br>N: ", n_assessments),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = list(text = paste("Average Scores -", cat_info$name), x = 0),
        xaxis = list(title = "Average Score"),
        yaxis = list(title = ""),
        margin = list(l = 250)
      )
  }

  # Create display data
  display_data <- viz_data %>%
    dplyr::select(ass_date, ass_faculty, field_label, value) %>%
    dplyr::arrange(desc(ass_date))

  return(list(
    has_data = TRUE,
    has_numeric = has_numeric,
    display_data = display_data,
    plot = plot_obj,
    n_assessments = nrow(data)
  ))
}

#' Create scale visualization UI
#' @keywords internal
create_scale_visualization_ui <- function(data, cat_info, ns) {

  viz_result <- create_scale_visualization_data(data, cat_info)

  if (!viz_result$has_data) {
    return(
      div(
        class = "alert alert-warning assessment-viz-container",
        icon("info-circle", class = "me-2"),
        "No valid data found for visualization."
      )
    )
  }

  # Return chart only (no table)
  div(
    class = "assessment-viz-container",
    h5(paste("Total assessments in this category:", viz_result$n_assessments)),

    if (viz_result$has_numeric) {
      plotly::plotlyOutput(ns("category_plot"), height = "400px")
    } else {
      div(
        class = "alert alert-info mt-3",
        icon("info-circle"), " No numeric data available for charting."
      )
    }
  )
}

#' Create observation visualization with sub-type selection
#' @keywords internal
create_observation_visualization <- function(data, cat_info, ns) {
  div(
    class = "assessment-viz-container animate__animated animate__fadeIn",
    h5(paste("Total observation assessments:", nrow(data))),

    # Sub-type selection buttons (rendered by server)
    uiOutput(ns("obs_subtype_buttons")),

    # Visualization area (chart and/or table based on selection)
    uiOutput(ns("obs_viz_area"))
  )
}

#' Format observation sub-type name
#' @keywords internal
format_obs_subtype_name <- function(key) {
  name_map <- c(
    "pe" = "Physical Exam",
    "pres" = "Presentations",
    "writehp" = "Written H&P",
    "proc" = "Procedures",
    "counsel" = "Counseling",
    "handoff" = "Handoffs",
    "profess" = "Professionalism"
  )

  if (key %in% names(name_map)) {
    return(name_map[[key]])
  }

  # Default: capitalize and clean up
  gsub("_", " ", tools::toTitleCase(key))
}

#' Process observation data for visualization
#' @keywords internal
create_observation_viz_data <- function(data, type_info) {

  if (nrow(data) == 0) {
    return(list(
      has_data = FALSE,
      has_numeric = FALSE,
      has_text = FALSE,
      plot = NULL,
      text_display_data = data.frame(),
      n_assessments = 0
    ))
  }

  # Get observation fields that exist in the data
  obs_fields <- type_info$fields[type_info$fields %in% names(data)]

  # Filter to fields that are NOT metadata (date, faculty, type, etc.)
  obs_value_fields <- obs_fields[!obs_fields %in% c("ass_obs_type", "ass_date", "ass_faculty",
                                                      "ass_level", "ass_specialty", "record_id",
                                                      "redcap_repeat_instrument", "redcap_repeat_instance",
                                                      "source_form")]

  if (length(obs_value_fields) == 0) {
    return(list(
      has_data = FALSE,
      has_numeric = FALSE,
      has_text = FALSE,
      plot = NULL,
      text_display_data = data.frame(),
      n_assessments = nrow(data)
    ))
  }

  # Pivot data to long format
  viz_data <- data %>%
    dplyr::select(record_id, ass_date, ass_faculty, dplyr::any_of(obs_value_fields)) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(obs_value_fields),
      names_to = "field",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value), value != "", value != "0")

  if (nrow(viz_data) == 0) {
    return(list(
      has_data = FALSE,
      has_numeric = FALSE,
      has_text = FALSE,
      plot = NULL,
      text_display_data = data.frame(),
      n_assessments = nrow(data)
    ))
  }

  # Get field labels and types
  field_labels <- setNames(
    type_info$field_info$field_label,
    type_info$field_info$field_name
  )

  field_types <- setNames(
    type_info$field_info$field_type,
    type_info$field_info$field_name
  )

  # Add metadata
  viz_data <- viz_data %>%
    dplyr::mutate(
      field_label = field_labels[field],
      field_type = field_types[field],
      value_numeric = suppressWarnings(as.numeric(value))
    )

  # Separate numeric and text fields
  numeric_data <- viz_data %>%
    dplyr::filter(!is.na(value_numeric), !field_type %in% c("notes", "text"))

  text_data <- viz_data %>%
    dplyr::filter(is.na(value_numeric) | field_type %in% c("notes", "text"))

  # Create plot for numeric data
  plot_obj <- NULL
  has_numeric <- nrow(numeric_data) > 0

  if (has_numeric) {
    summary_data <- numeric_data %>%
      dplyr::group_by(field_label) %>%
      dplyr::summarise(
        mean_score = mean(value_numeric, na.rm = TRUE),
        median_score = median(value_numeric, na.rm = TRUE),
        n_assessments = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(desc(mean_score))

    plot_obj <- plotly::plot_ly(
      data = summary_data,
      x = ~mean_score,
      y = ~reorder(field_label, mean_score),
      type = "bar",
      orientation = "h",
      marker = list(color = "#17a2b8"),
      text = ~paste0("Mean: ", round(mean_score, 2),
                    "<br>Median: ", round(median_score, 2),
                    "<br>N: ", n_assessments),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = list(text = paste("Ratings -", type_info$name), x = 0),
        xaxis = list(title = "Average Score"),
        yaxis = list(title = ""),
        margin = list(l = 250)
      )
  }

  # Create table data for text fields
  text_display_data <- data.frame()
  has_text <- nrow(text_data) > 0

  if (has_text) {
    text_display_data <- text_data %>%
      dplyr::select(ass_date, ass_faculty, field_label, value) %>%
      dplyr::arrange(desc(ass_date))
  }

  return(list(
    has_data = TRUE,
    has_numeric = has_numeric,
    has_text = has_text,
    plot = plot_obj,
    text_display_data = text_display_data,
    n_assessments = nrow(data)
  ))
}

#' Render observation visualization area (called from server via uiOutput)
#' This is rendered in the server's renderUI for obs_viz_area
#' @keywords internal
create_observation_subtype_viz <- function(data, type_info, ns) {
  viz_result <- create_observation_viz_data(data, type_info)

  if (!viz_result$has_data) {
    return(
      div(
        class = "alert alert-info animate__animated animate__fadeIn mt-3",
        icon("info-circle"), " No data available for this observation type."
      )
    )
  }

  div(
    class = "obs-viz-content animate__animated animate__fadeIn",
    style = "margin-top: 20px;",

    # Show chart if numeric data exists
    if (viz_result$has_numeric) {
      div(
        class = "mb-4",
        plotly::plotlyOutput(ns("obs_plot"), height = "400px")
      )
    },

    # Show table if text data exists
    if (viz_result$has_text) {
      div(
        h6(icon("comment-alt"), " Narrative Comments"),
        DT::DTOutput(ns("obs_table"))
      )
    },

    # If neither, show message
    if (!viz_result$has_numeric && !viz_result$has_text) {
      div(
        class = "alert alert-warning",
        icon("exclamation-triangle"), " No displayable data for this type."
      )
    }
  )
}
