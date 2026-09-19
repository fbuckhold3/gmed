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
      fontSize = '20px',
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
      fontSize = '20px'
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
