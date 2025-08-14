#' Create Consistently Styled DataTable for GMED Apps (Tested Version)
#'
#' Creates a standardized DT datatable with consistent styling across all gmed applications.
#' Includes SLUCare SSM theming, responsive design, and proper null value handling.
#' This is the tested version from the coach app.
#'
#' @param data A data frame to display
#' @param caption Optional character string for table caption/title
#' @param page_length Number of rows to show per page (default: 10)
#' @param searchable Whether to include search functionality (default: TRUE)
#' @param show_buttons Whether to show export buttons (default: FALSE)
#' @param highlight_columns Vector of column names to highlight (default: NULL)
#' @param max_char_length Maximum characters to show in cells before truncating (default: 100)
#'
#' @return A DT::datatable object with gmed styling applied
#' @export
create_gmed_datatable_tested <- function(data, 
                                         caption = NULL, 
                                         page_length = 10,
                                         searchable = TRUE,
                                         show_buttons = FALSE,
                                         highlight_columns = NULL,
                                         max_char_length = 100) {
  
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for datatable creation")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data processing")
  }
  
  # Handle empty data
  if (is.null(data) || nrow(data) == 0) {
    empty_message <- if (!is.null(caption)) {
      paste0("No ", tolower(caption), " data available")
    } else {
      "No data available"
    }
    
    return(DT::datatable(
      data.frame(Message = empty_message),
      options = list(
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE
      ),
      caption = caption,
      rownames = FALSE,
      class = 'gmed-empty-table'
    ))
  }
  
  # Truncate long text in cells for better display
  if (max_char_length > 0) {
    data <- data %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ ifelse(
        nchar(.x) > max_char_length,
        paste0(substr(.x, 1, max_char_length), "..."),
        .x
      )))
  }
  
  # Build DOM string based on options
  dom_string <- if (show_buttons) {
    if (searchable) "Bfrtip" else "Brtip"
  } else {
    if (searchable) "frtip" else "rtip"
  }
  
  # Create base datatable
  dt <- DT::datatable(
    data,
    options = list(
      pageLength = page_length,
      dom = dom_string,
      scrollX = TRUE,
      scrollY = "400px",
      scrollCollapse = TRUE,
      autoWidth = TRUE,
      buttons = if (show_buttons) {
        list(
          list(extend = 'copy', text = 'Copy'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel')
        )
      } else {
        NULL
      },
      columnDefs = list(
        # Handle null/empty values
        list(
          targets = "_all",
          render = DT::JS(
            "function(data, type, row) {
              if (data === null || data === '' || data === 'Not provided' || data === 'Not specified') {
                return '<span style=\"color: #999; font-style: italic;\">' + 
                       (data === '' || data === null ? 'Not provided' : data) + 
                       '</span>';
              }
              return data;
            }"
          )
        ),
        # Make columns responsive
        list(
          targets = "_all",
          className = "dt-left"
        )
      ),
      # Language customization
      language = list(
        search = "Search:",
        lengthMenu = "Show _MENU_ entries",
        info = "Showing _START_ to _END_ of _TOTAL_ entries",
        infoEmpty = "No entries available",
        infoFiltered = "(filtered from _MAX_ total entries)",
        paginate = list(
          first = "First",
          last = "Last", 
          `next` = "Next",
          previous = "Previous"
        )
      )
    ),
    caption = caption,
    rownames = FALSE,
    escape = FALSE,  # Allow HTML in cells
    class = 'gmed-datatable cell-border stripe hover'
  )
  
  # Apply base SLUCare SSM styling
  dt <- dt %>%
    DT::formatStyle(
      columns = names(data),
      backgroundColor = '#fafafa',
      borderColor = '#e1e5e9',
      fontSize = '14px',
      fontFamily = '"Segoe UI", Tahoma, Geneva, Verdana, sans-serif'
    )
  
  # Apply header styling
  dt <- dt %>%
    DT::formatStyle(
      columns = names(data),
      target = 'row',
      backgroundColor = DT::styleEqual('', '#f8f9fa')
    )
  
  # Highlight specific columns if requested
  if (!is.null(highlight_columns)) {
    highlight_cols <- intersect(highlight_columns, names(data))
    if (length(highlight_cols) > 0) {
      dt <- dt %>%
        DT::formatStyle(
          columns = highlight_cols,
          backgroundColor = '#e3f2fd',
          fontWeight = 'bold'
        )
    }
  }
  
  # Special styling for Plus/Delta tables - UPDATED TO BLUE SHADES
  if (all(c("Plus", "Delta") %in% names(data))) {
    dt <- dt %>%
      DT::formatStyle(
        'Plus',
        backgroundColor = '#e3f2fd',  # Light blue
        borderLeft = '3px solid #1976d2'  # Darker blue
      ) %>%
      DT::formatStyle(
        'Delta', 
        backgroundColor = '#e8f4fd',  # Slightly different light blue
        borderLeft = '3px solid #1565c0'  # Different darker blue
      )
  }
  
  # Special styling for Date columns
  date_columns <- grep("date|Date", names(data), value = TRUE, ignore.case = TRUE)
  if (length(date_columns) > 0) {
    dt <- dt %>%
      DT::formatStyle(
        columns = date_columns,
        fontWeight = 'bold',
        color = '#0056b3'
      )
  }
  
  return(dt)
}

# Override the main function with the tested version for now
# This ensures backward compatibility while we test the enhanced version
create_gmed_datatable <- create_gmed_datatable_tested#' @title Standardized Datatable Components for GMED
#' @description Consistent DT and reactable formatting functions with SSM SLUCare styling
#' @name datatable_components
NULL

#' Create Standardized GMED Datatable
#'
#' Creates a consistently styled DT::datatable with SSM SLUCare branding
#' and enhanced functionality for use across all GMED applications.
#'
#' @param data Data frame to display
#' @param caption Optional caption for the table
#' @param page_length Number of rows per page (default: 10)
#' @param highlight_columns Vector of column names to highlight
#' @param scrollX Enable horizontal scrolling (default: TRUE)
#' @param selection Selection mode: 'single', 'multiple', or 'none'
#' @param dom Table control elements layout
#' @param class Additional CSS classes
#'
#' @return A DT::datatable object with consistent GMED styling
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic table
#' create_gmed_datatable(mtcars, "Car Data")
#' 
#' # With highlighting
#' create_gmed_datatable(mtcars, "Car Data", 
#'                       highlight_columns = c("mpg", "hp"))
#' }
create_gmed_datatable <- function(data, 
                                  caption = NULL,
                                  page_length = 10,
                                  highlight_columns = NULL,
                                  scrollX = TRUE,
                                  selection = 'single',
                                  dom = 'ftp',
                                  class = 'cell-border stripe hover') {
  
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for datatable creation")
  }
  
  # Handle empty data
  if (nrow(data) == 0) {
    empty_message <- if (!is.null(caption)) {
      paste0("No ", tolower(caption), " data available")
    } else {
      "No data available"
    }
    
    return(DT::datatable(
      data.frame(Message = empty_message),
      options = list(dom = 't'),
      caption = caption,
      rownames = FALSE,
      class = class
    ))
  }
  
  # Define column definitions for consistent formatting
  columnDefs <- list(
    # Handle null/empty values consistently
    list(
      targets = "_all",
      render = DT::JS(
        "function(data, type, row) {
          if (data === null || data === '' || data === undefined) {
            return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
          }
          return data;
        }"
      )
    )
  )
  
  # Add highlighting for specified columns
  if (!is.null(highlight_columns)) {
    highlight_indices <- which(names(data) %in% highlight_columns) - 1  # 0-indexed
    if (length(highlight_indices) > 0) {
      columnDefs <- append(columnDefs, list(
        list(
          targets = highlight_indices,
          className = "gmed-highlight-column"
        )
      ))
    }
  }
  
  # Create the datatable
  dt <- DT::datatable(
    data,
    options = list(
      pageLength = page_length,
      dom = dom,
      scrollX = scrollX,
      columnDefs = columnDefs,
      autoWidth = FALSE,
      language = list(
        search = "Search:",
        lengthMenu = "Show _MENU_ entries",
        info = "Showing _START_ to _END_ of _TOTAL_ entries",
        paginate = list(
          first = "First",
          last = "Last",
          `next` = "Next",
          previous = "Previous"
        )
      )
    ),
    caption = caption,
    rownames = FALSE,
    class = class,
    selection = selection,
    escape = FALSE
  )
  
  # Apply SSM SLUCare styling
  dt <- dt %>%
    DT::formatStyle(
      columns = names(data),
      backgroundColor = '#f8f9fa',
      borderColor = '#dfe2e5',
      fontSize = '14px'
    )
  
  # Highlight specified columns if provided
  if (!is.null(highlight_columns)) {
    existing_cols <- intersect(highlight_columns, names(data))
    if (length(existing_cols) > 0) {
      dt <- dt %>%
        DT::formatStyle(
          columns = existing_cols,
          backgroundColor = '#e3f2fd',
          borderColor = '#1976d2',
          fontWeight = 'bold'
        )
    }
  }
  
  return(dt)
}

#' Create Styled DT for Assessment Data
#'
#' Wrapper around create_gmed_datatable specifically for assessment/evaluation data
#' with common formatting patterns used across evaluation forms.
#'
#' @param data Data frame of evaluation data
#' @param caption Optional caption for the table
#'
#' @return A DT::datatable object with assessment-specific styling
#' @export
#'
#' @examples
#' \dontrun{
#' evaluation_data <- data.frame(
#'   Date = Sys.Date(),
#'   Plus = "Good communication",
#'   Delta = "Needs work on documentation"
#' )
#' create_styled_dt(evaluation_data, "Plus/Delta Feedback")
#' }
create_styled_dt <- function(data, caption = NULL) {
  
  if (nrow(data) == 0) {
    empty_msg <- if (!is.null(caption)) {
      paste0("No ", tolower(caption), " data available")
    } else {
      "No data available"
    }
    
    return(DT::datatable(
      data.frame(Message = empty_msg),
      options = list(dom = 't'),
      caption = caption,
      rownames = FALSE
    ))
  }
  
  # Use the main gmed datatable function with assessment-specific settings
  create_gmed_datatable(
    data,
    caption = caption,
    page_length = 5,
    scrollX = TRUE,
    highlight_columns = c("Plus", "Delta", "Feedback")
  )
}

#' Create Datatable with Click Handling
#'
#' Enhanced datatable with JavaScript click handling for row selection,
#' commonly used in resident selection tables.
#'
#' @param data Data frame to display
#' @param caption Optional caption
#' @param shiny_input_id Input ID for click events (default: "selected_resident_in_table")
#' @param id_columns Vector of column indices that contain ID information (0-indexed)
#'
#' @return DT::datatable with JavaScript click callbacks
#' @export
#'
#' @examples
#' \dontrun{
#' residents <- data.frame(
#'   Name = c("John Doe", "Jane Smith"),
#'   Level = c("Intern", "PGY2"),
#'   Access_Code = c("ABC123", "DEF456")
#' )
#' datatable_with_click(residents, "Resident List")
#' }
datatable_with_click <- function(data, 
                                 caption = NULL,
                                 shiny_input_id = "selected_resident_in_table",
                                 id_columns = c(0, 1, 2)) {
  
  if (nrow(data) == 0) {
    return(create_gmed_datatable(data, caption))
  }
  
  # Build JavaScript callback for row clicking
  js_callback <- sprintf("
    table.on('click', 'tbody tr', function() {
      table.$('tr.selected').removeClass('selected');
      $(this).addClass('selected');
      
      var rowData = table.row(this).data();
      var residentName = rowData[0];
      var residentLevel = rowData[1];
      var accessCode = rowData[2];
      var reviewRole = rowData[3];
      var reviewPeriod = rowData[4];
      
      Shiny.setInputValue('%s', 
        {
          name: residentName, 
          level: residentLevel,
          access_code: accessCode, 
          review_role: reviewRole, 
          review_period: reviewPeriod
        }, 
        {priority: 'event'});
    });
  ", shiny_input_id)
  
  dt <- DT::datatable(
    data,
    escape = FALSE,
    options = list(
      pageLength = 10,
      dom = 'ftp',
      scrollX = TRUE,
      columnDefs = list(
        list(
          targets = "_all",
          render = DT::JS(
            "function(data, type, row) {
              if (data === null || data === '') {
                return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
              }
              return data;
            }"
          )
        )
      )
    ),
    caption = caption,
    rownames = FALSE,
    class = 'cell-border stripe hover',
    selection = 'single',
    callback = DT::JS(js_callback)
  )
  
  # Apply SSM styling
  dt <- dt %>%
    DT::formatStyle(
      columns = names(data)[1:min(5, ncol(data))],  # Style first 5 columns
      backgroundColor = '#f8f9fa',
      borderColor = '#dfe2e5'
    )
  
  # Style status columns if they exist
  status_cols <- grep("Status|status", names(data), value = TRUE)
  if (length(status_cols) > 0) {
    dt <- dt %>%
      DT::formatStyle(
        columns = status_cols,
        textAlign = 'center'
      )
  }
  
  return(dt)
}

#' Create Continuity Clinic Completion Table
#'
#' Creates a reactable table for CC evaluation status with enhanced styling.
#' Migrated from function_assessment.R with GMED enhancements.
#'
#' @param data Dataframe with columns: name, Level, cc_eval_type, Evaluator
#' @param name Character string for filtering
#'
#' @return A reactable table object
#' @export
create_cc_table <- function(data, name) {
  
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("Package 'reactable' is needed for this function. Please install it.")
  }
  
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is needed for this function. Please install it.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is needed for this function. Please install it.")
  }
  
  # Filter data for the specified name
  filtered_data <- data %>%
    dplyr::filter(name == !!name) %>%
    dplyr::arrange(Level)
  
  if (nrow(filtered_data) == 0) {
    return(reactable::reactable(
      data.frame(Message = "No continuity clinic data available"),
      columns = list(Message = reactable::colDef(name = "")),
      defaultPageSize = 5
    ))
  }
  
  # Standard evaluation types for all quarters
  eval_types <- c("Intern Presentation", "Documentation", "Use Evidence", "Respect")
  display_names <- eval_types
  
  # Get unique levels and create result structure
  levels <- unique(filtered_data$Level)
  result_df <- data.frame(Level = levels, stringsAsFactors = FALSE)
  
  # Create columns for each evaluation type
  for (i in seq_along(eval_types)) {
    eval_type <- eval_types[i]
    display_name <- display_names[i]
    
    # Status column
    status_col <- paste0(display_name, " Status")
    result_df[[status_col]] <- sapply(levels, function(level) {
      level_data <- filtered_data[filtered_data$Level == level, ]
      any(!is.na(level_data$cc_eval_type) & level_data$cc_eval_type == eval_type)
    })
    
    # Evaluator column
    evaluator_col <- paste0(display_name, " Evaluator")
    result_df[[evaluator_col]] <- sapply(levels, function(level) {
      level_data <- filtered_data[filtered_data$Level == level, ]
      matching_rows <- level_data[!is.na(level_data$cc_eval_type) & 
                                    level_data$cc_eval_type == eval_type, ]
      if (nrow(matching_rows) > 0) {
        return(matching_rows$Evaluator[1])  # Return first evaluator
      } else {
        return(NA_character_)
      }
    })
  }
  
  # Create custom theme for SSM SLUCare styling
  custom_theme <- reactable::reactableTheme(
    headerStyle = list(
      backgroundColor = "#0066a1",
      color = "#ffffff",
      fontWeight = 600,
      borderBottom = "2px solid #004d78"
    ),
    cellStyle = list(
      fontSize = "14px"
    ),
    rowHighlightStyle = list(
      backgroundColor = "#f5f5f5"
    )
  )
  
  # Create column definitions
  cols <- list(
    Level = reactable::colDef(
      name = "Level",
      minWidth = 120,
      cell = function(value) {
        htmltools::div(
          style = "font-weight: 600; color: #0066a1;",
          value
        )
      }
    )
  )
  
  # Create column groups for each evaluation type
  column_groups <- list()
  for (i in 1:length(display_names)) {
    display_name <- display_names[i]
    
    status_col <- paste0(display_name, " Status")
    eval_col <- paste0(display_name, " Evaluator")
    
    cols[[status_col]] <- reactable::colDef(
      name = display_name,
      cell = function(value) {
        if (isTRUE(value)) {
          htmltools::div(
            style = "display: flex; justify-content: center; align-items: center;",
            htmltools::div(
              style = "background-color: #e6f4ea; color: #137333; border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-size: 18px;",
              "✓"
            )
          )
        } else {
          htmltools::div(
            style = "display: flex; justify-content: center; align-items: center;",
            htmltools::div(
              style = "background-color: #fce8e6; color: #c5221f; border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-weight: bold; font-size: 18px;",
              "○"
            )
          )
        }
      },
      width = 120,
      align = "center"
    )
    
    cols[[eval_col]] <- reactable::colDef(
      name = "Evaluator",
      cell = function(value) {
        if (is.na(value)) {
          htmltools::div(style = "color: #999; font-style: italic;", "Not assigned")
        } else {
          htmltools::div(style = "font-weight: 500;", value)
        }
      },
      width = 130
    )
    
    column_groups[[length(column_groups) + 1]] <- reactable::colGroup(
      name = display_name,
      columns = c(status_col, eval_col)
    )
  }
  
  # Create the reactable
  reactable::reactable(
    result_df,
    columns = cols,
    bordered = TRUE,
    highlight = TRUE,
    striped = TRUE,
    theme = custom_theme,
    columnGroups = column_groups,
    defaultPageSize = 5
  )
}

#' Create Status Indicator Element
#'
#' Helper function to create consistent status indicators across datatables.
#'
#' @param status Logical or character indicating status
#' @param type Type of status indicator ("complete", "incomplete", "pending")
#'
#' @return HTML div element with status styling
#' @export
create_status_indicator <- function(status, type = "auto") {
  
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is required for HTML elements")
  }
  
  # Auto-determine type based on status
  if (type == "auto") {
    if (is.logical(status)) {
      type <- if (isTRUE(status)) "complete" else "incomplete"
    } else if (is.character(status)) {
      type <- switch(tolower(status),
                     "complete" = "complete",
                     "done" = "complete", 
                     "finished" = "complete",
                     "incomplete" = "incomplete",
                     "pending" = "pending",
                     "incomplete"  # default
      )
    } else {
      type <- "incomplete"
    }
  }
  
  # Define styling by type
  style_map <- list(
    "complete" = list(
      bg = "#e6f4ea",
      color = "#137333", 
      symbol = "✓"
    ),
    "incomplete" = list(
      bg = "#fce8e6",
      color = "#c5221f",
      symbol = "○"
    ),
    "pending" = list(
      bg = "#fff3cd",
      color = "#856404",
      symbol = "⏳"
    )
  )
  
  style_info <- style_map[[type]] %||% style_map[["incomplete"]]
  
  htmltools::div(
    style = "display: flex; justify-content: center; align-items: center;",
    htmltools::div(
      style = paste0(
        "background-color: ", style_info$bg, "; ",
        "color: ", style_info$color, "; ",
        "border-radius: 50%; width: 30px; height: 30px; ",
        "display: flex; justify-content: center; align-items: center; ",
        "font-size: 18px; font-weight: bold;"
      ),
      style_info$symbol
    )
  )
}