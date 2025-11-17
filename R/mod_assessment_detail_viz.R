#' Assessment Detail Visualization Module UI
#' 
#' Dynamic assessment viewer with category selector
#' @param id Module namespace
#' @export
mod_assessment_detail_viz_ui <- function(id) {
  ns <- NS(id)
  colors <- ssm_colors()
  
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        .assessment-detail-container {
          background: white;
          border-radius: 12px;
          padding: 1.5rem;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        .category-selector {
          background: linear-gradient(135deg, ", colors$primary, " 0%, ", colors$secondary, " 100%);
          padding: 1.5rem;
          border-radius: 8px;
          margin-bottom: 1.5rem;
        }
        .category-btn {
          background: white;
          border: 2px solid ", colors$primary, ";
          color: ", colors$primary, ";
          padding: 0.75rem 1.5rem;
          border-radius: 6px;
          margin: 0.25rem;
          cursor: pointer;
          transition: all 0.3s;
          font-weight: 600;
        }
        .category-btn:hover {
          background: ", colors$primary, ";
          color: white;
          transform: translateY(-2px);
        }
        .category-btn.active {
          background: ", colors$success, ";
          border-color: ", colors$success, ";
          color: white;
        }
        .viz-section {
          margin-top: 1.5rem;
        }
      ")))
    ),
    
    div(class = "assessment-detail-container",
        h3(icon("chart-line"), "Detailed Assessment Analysis"),
        
        # Category selector with cool buttons
        div(class = "category-selector",
            h4("Select Assessment Category", style = "color: white; margin-bottom: 1rem;"),
            uiOutput(ns("category_buttons"))
        ),
        
        # Dynamic visualization area
        div(class = "viz-section",
            uiOutput(ns("viz_output"))
        )
    )
  )
}

#' Assessment Detail Visualization Module Server
#' @param id Module namespace
#' @param rdm_data Reactive returning full RDM data
#' @param record_id Reactive returning resident record_id
#' @param data_dict Reactive or static data dictionary
#' @export
mod_assessment_detail_viz_server <- function(id, rdm_data, record_id, data_dict) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Extract assessment categories from data dictionary
    assessment_categories <- reactive({
      dict <- if(is.reactive(data_dict)) data_dict() else data_dict
      
      extract_assessment_categories(dict)
    })
    
    # Selected category
    selected_category <- reactiveVal("cc_inbasket")
    
    # Render category buttons
    output$category_buttons <- renderUI({
      req(rdm_data(), record_id())
      cats <- assessment_categories()

      # Get filtered data for this resident
      filtered_data <- rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          tolower(redcap_repeat_instrument) == "assessment"
        )

      buttons <- lapply(names(cats), function(cat_key) {
        cat_info <- cats[[cat_key]]

        # Count non-empty values for this category
        cat_count <- filtered_data %>%
          dplyr::select(dplyr::any_of(cat_info$fields)) %>%
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "field",
            values_to = "value"
          ) %>%
          dplyr::filter(!is.na(value) & value != "" & value != "0") %>%
          nrow()

        # Create button label with count
        button_label <- paste0(cat_info$display_name, " (", cat_count, ")")

        actionButton(
          ns(paste0("cat_", cat_key)),
          label = button_label,
          class = if(selected_category() == cat_key) "category-btn active" else "category-btn"
        )
      })

      do.call(tagList, buttons)
    })
    
    # Observe button clicks
    observe({
      cats <- assessment_categories()
      
      lapply(names(cats), function(cat_key) {
        observeEvent(input[[paste0("cat_", cat_key)]], {
          selected_category(cat_key)
        })
      })
    })
    
    # Render visualization based on selected category
    output$viz_output <- renderUI({
      req(selected_category(), rdm_data(), record_id())
      
      cat_key <- selected_category()
      cats <- assessment_categories()
      cat_info <- cats[[cat_key]]
      
      # Filter data for this resident and category
      filtered_data <- rdm_data() %>%
        dplyr::filter(
          record_id == !!record_id(),
          !is.na(redcap_repeat_instrument),
          tolower(redcap_repeat_instrument) == "assessment"
        )
      
      # Create visualization based on category type
      if (cat_info$type == "numeric_scale") {
        create_scale_viz(filtered_data, cat_info, ns)
      } else if (cat_info$type == "observation") {
        create_observation_viz(filtered_data, cat_info, ns)
      } else {
        create_general_viz(filtered_data, cat_info, ns)
      }
    })
  })
}

#' Extract Assessment Categories from Data Dictionary
#' @param data_dict Data dictionary
#' @return Named list of category configurations
#' @export
extract_assessment_categories <- function(data_dict) {
  
  # Get all assessment fields
  ass_fields <- data_dict %>%
    dplyr::filter(form_name == "assessment") %>%
    dplyr::filter(grepl("^ass_", field_name)) %>%
    dplyr::filter(!field_name %in% c("ass_date", "ass_level", "ass_plus", "ass_delta", 
                                      "ass_faculty", "ass_specialty"))
  
  # Define categories (you can expand this)
  categories <- list(
    cc_inbasket = list(
      display_name = "CC: Inbasket",
      pattern = "^ass_cc_inb_",
      type = "numeric_scale",
      description = "Continuity Clinic Inbasket Management"
    ),
    cc_documentation = list(
      display_name = "CC: Documentation",
      pattern = "^ass_cc_doc_",
      type = "numeric_scale",
      description = "Continuity Clinic Documentation"
    ),
    cc_intern_semi = list(
      display_name = "CC: Intern Semi-Annual",
      pattern = "^ass_cc_int_semi_",
      type = "numeric_scale",
      description = "Intern Semi-Annual Assessment"
    ),
    cc_pgy2_semi = list(
      display_name = "CC: PGY2 Semi-Annual",
      pattern = "^ass_cc_pgy2_semi_",
      type = "numeric_scale",
      description = "PGY2 Semi-Annual Assessment"
    ),
    cc_pgy3_semi = list(
      display_name = "CC: PGY3 Semi-Annual",
      pattern = "^ass_cc_pgy3_semi_",
      type = "numeric_scale",
      description = "PGY3 Semi-Annual Assessment"
    ),
    day_assessment = list(
      display_name = "Day Assessments",
      pattern = "^ass_day_",
      type = "numeric_scale",
      description = "Day Assessment Observations"
    ),
    consult = list(
      display_name = "Consult Assessments",
      pattern = "^ass_cons_",
      type = "mixed",
      description = "Consultation Assessments"
    ),
    intern_ip = list(
      display_name = "Intern Inpatient",
      pattern = "^ass_int_ip_",
      type = "numeric_scale",
      description = "Intern Inpatient Assessments"
    ),
    resident_ip = list(
      display_name = "Resident Inpatient",
      pattern = "^ass_res_ip_",
      type = "numeric_scale",
      description = "Resident Inpatient Assessments"
    ),
    bridge = list(
      display_name = "Bridge Clinic",
      pattern = "^ass_bridge_",
      type = "numeric_scale",
      description = "Bridge Clinic Assessments"
    ),
    observations = list(
      display_name = "Direct Observations",
      pattern = "^ass_obs_",
      type = "observation",
      description = "Direct Observation Assessments"
    )
  )
  
  # Add field lists to each category
  for (cat_key in names(categories)) {
    cat_fields <- ass_fields %>%
      dplyr::filter(grepl(categories[[cat_key]]$pattern, field_name))
    
    categories[[cat_key]]$fields <- cat_fields$field_name
    categories[[cat_key]]$field_labels <- setNames(
      cat_fields$field_label,
      cat_fields$field_name
    )
  }
  
  return(categories)
}

#' Create Scale Visualization (for numeric/radio fields)
#' @keywords internal
create_scale_viz <- function(data, cat_info, ns) {
  
  # Count total assessments
  total_assessments <- nrow(data)

  # Get non-NA values for these fields
  field_data <- data %>%
    dplyr::select(record_id, ass_date, dplyr::any_of(cat_info$fields)) %>%
    tidyr::pivot_longer(
      cols = dplyr::any_of(cat_info$fields),
      names_to = "field",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value) & value != "" & value != "0") %>%
    dplyr::mutate(
      value_num = suppressWarnings(as.numeric(value)),
      field_label = cat_info$field_labels[field]
    )

  if (nrow(field_data) == 0) {
    return(div(
      class = "alert alert-info",
      icon("info-circle"),
      paste0(" No data available for this category yet. ",
             "Total assessments: ", total_assessments,
             ", but none contain entries for these specific fields.")
    ))
  }
  
  # Create summary stats
  summary_stats <- field_data %>%
    dplyr::group_by(field_label) %>%
    dplyr::summarise(
      mean_score = round(mean(value_num, na.rm = TRUE), 2),
      median_score = median(value_num, na.rm = TRUE),
      n_assessments = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(mean_score))
  
  tagList(
    h4(cat_info$description),
    p(class = "text-muted", 
      "Total assessments in this category: ", nrow(field_data)),
    
    # Plotly bar chart
    plotlyOutput(ns("category_chart")),
    
    # Data table
    DT::dataTableOutput(ns("category_table"))
  )
}

#' Create Observation Visualization
#' @keywords internal
create_observation_viz <- function(data, cat_info, ns) {
  
  # Count observations by type
  obs_counts <- data %>%
    dplyr::filter(!is.na(ass_obs_type) & ass_obs_type != "") %>%
    dplyr::count(ass_obs_type, sort = TRUE)
  
  if (nrow(obs_counts) == 0) {
    return(div(
      class = "alert alert-info",
      icon("info-circle"),
      " No observation data available yet."
    ))
  }
  
  tagList(
    h4("Direct Observations Summary"),
    p(class = "text-muted", 
      "Total observations: ", sum(obs_counts$n)),
    
    plotlyOutput(ns("obs_chart")),
    
    DT::dataTableOutput(ns("obs_table"))
  )
}

#' Create General Visualization
#' @keywords internal
create_general_viz <- function(data, cat_info, ns) {
  div(
    class = "alert alert-warning",
    icon("wrench"),
    " Visualization for this category is under development."
  )
}