#' @title Theme and Color Management for GMED
#' @description Color schemes, theme functions, and resource loading for consistent SSM SLUCare branding
#' @name theme_manager
NULL

#' Load gmed CSS and JavaScript resources
#' 
#' @param theme Character string specifying theme variant. Currently supports "default" and "slucare"
#' @export
load_gmed_styles <- function(theme = "default") {
  # Add resource path for CSS files
  shiny::addResourcePath("gmed", system.file("www", package = "gmed"))
  
  # Return CSS and JS tags
  shiny::tagList(
    shiny::tags$link(rel = "stylesheet", type = "text/css", 
                     href = "gmed/css/gmed-core.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", 
                     href = "gmed/css/gmed-components.css"),
    shiny::tags$link(rel = "stylesheet", type = "text/css", 
                     href = "gmed/css/gmed-themes.css"),
    shiny::tags$script(src = "gmed/js/gmed-components.js")
  )
}

#' Apply gmed styling classes to UI elements
#' 
#' @param element Shiny UI element to style
#' @param style Character string specifying gmed style class
#' @export
apply_gmed_style <- function(element, style = "gmed-card") {
  if ("class" %in% names(element$attribs)) {
    element$attribs$class <- paste(element$attribs$class, style)
  } else {
    element$attribs$class <- style
  }
  element
}

#' SSM SLUCare Color Palette
#'
#' Provides the official SSM Health/SLUCare color palette for consistent branding
#' across all GMED applications.
#'
#' @return Named list of SSM brand colors
#' @export
#'
#' @examples
#' \dontrun{
#' colors <- ssm_colors()
#' colors$primary  # "#003d5c"
#' }
ssm_colors <- function() {
  list(
    # Primary Brand Colors
    primary = "#003d5c",           # SSM Primary Blue (dark)
    secondary = "#0066a1",         # SSM Secondary Blue  
    light_blue = "#4a90a4",        # SSM Light Blue
    accent_blue = "#2196f3",       # SSM Accent Blue
    
    # Legacy Coaching Colors (mapped to SSM)
    coach_primary = "#0066a1",     # Was #0072B2
    coach_secondary = "#4a90a4",   # Was #56B4E9
    
    # Status Colors
    success = "#00a651",           # SSM Success Green
    warning = "#ff8c00",           # SSM Warning Orange
    danger = "#dc3545",            # SSM Error Red
    info = "#2196f3",              # SSM Info Blue
    
    # Neutral Colors
    neutral_gray = "#6c757d",      # SSM Neutral Gray
    light_gray = "#f8f9fa",        # SSM Light Gray
    white = "#ffffff",             # White
    
    # Text Colors
    text_primary = "#2c3e50",      # Primary text
    text_secondary = "#546e7a",    # Secondary text
    text_muted = "#6c757d",        # Muted text
    
    # Background Colors
    bg_primary = "#f8f9fa",        # Primary background
    bg_secondary = "#e9ecef",      # Secondary background
    bg_dark = "#343a40"            # Dark background
  )
}

#' Create GMED Bootstrap Theme
#'
#' Creates a bslib theme with SSM SLUCare colors and GMED styling.
#'
#' @param version Bootstrap version (default: 5)
#' @param base_font Base font family
#' @param heading_font Heading font family
#' @param custom_colors Optional named list of custom color overrides
#'
#' @return bslib bs_theme object
#' @export
#'
#' @examples
#' \dontrun{
#' theme <- create_gmed_theme()
#' 
#' # With custom font
#' theme <- create_gmed_theme(
#'   base_font = font_google("Inter"),
#'   heading_font = font_google("Inter", wght = 600)
#' )
#' }
create_gmed_theme <- function(version = 5, 
                              base_font = NULL, 
                              heading_font = NULL,
                              custom_colors = NULL) {
  
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required for theme creation")
  }
  
  # Get SSM colors
  colors <- ssm_colors()
  
  # Apply any custom color overrides
  if (!is.null(custom_colors)) {
    colors[names(custom_colors)] <- custom_colors
  }
  
  # Create the theme
  bslib::bs_theme(
    version = version,
    primary = colors$secondary,      # Use secondary blue as primary
    secondary = colors$light_blue,   # Light blue as secondary
    success = colors$success,
    warning = colors$warning,
    danger = colors$danger,
    info = colors$accent_blue,
    bg = colors$bg_primary,
    fg = colors$text_primary,
    base_font = base_font,
    heading_font = heading_font
  )
}

#' Setup GMED App UI Foundation
#'
#' Sets up the basic UI foundation for a GMED Shiny app with consistent
#' styling, JavaScript, and theme. This should be used as the wrapper
#' for all GMED applications.
#'
#' @param ... UI elements to include in the page
#' @param title Page title for the browser tab
#' @param theme_variant Theme variant ("slucare", "default")
#' @param custom_css Optional vector of additional CSS files to include
#' @param custom_js Optional vector of additional JS files to include
#' @param base_font Base font (default: Google Inter)
#' @param heading_font Heading font (default: Google Inter 600)
#' @param include_fontawesome Whether to include Font Awesome (default: TRUE)
#'
#' @return Shiny page_fluid with GMED foundation
#' @export
#'
#' @examples
#' \dontrun{
#' ui <- gmed_page(
#'   title = "Coach Dashboard",
#'   
#'   # Your app content here
#'   gmed_card(
#'     title = "Welcome",
#'     p("This is my GMED app")
#'   )
#' )
#' }
gmed_page <- function(..., 
                      title = "GMED Application",
                      theme_variant = "slucare",
                      custom_css = NULL,
                      custom_js = NULL,
                      base_font = NULL,
                      heading_font = NULL,
                      include_fontawesome = TRUE) {
  
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("Package 'bslib' is required for page creation")
  }
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for page creation")
  }
  
  # Set default fonts if not provided
  if (is.null(base_font)) {
    if (requireNamespace("bslib", quietly = TRUE)) {
      base_font <- tryCatch(bslib::font_google("Inter"), error = function(e) NULL)
    }
  }
  
  if (is.null(heading_font)) {
    if (requireNamespace("bslib", quietly = TRUE)) {
      heading_font <- tryCatch(bslib::font_google("Inter", wght = 600), error = function(e) NULL)
    }
  }
  
  # Create the page
  bslib::page_fluid(
    # Load GMED styles first
    load_gmed_styles(theme_variant),
    
    # Create and apply GMED theme
    theme = create_gmed_theme(
      base_font = base_font,
      heading_font = heading_font
    ),
    
    # Enable shinyjs
    shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/shinyjs/2.1.0/shinyjs.min.js"),
    shinyjs::useShinyjs(),
    
    # Include Font Awesome if requested
    if (include_fontawesome) {
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      )
    },
    
    # Page title
    shiny::tags$head(
      shiny::tags$title(title)
    ),
    
    # Include custom CSS files
    if (!is.null(custom_css)) {
      lapply(custom_css, function(css_file) {
        shiny::includeCSS(css_file)
      })
    },
    
    # Include custom JS files  
    if (!is.null(custom_js)) {
      lapply(custom_js, function(js_file) {
        shiny::includeScript(js_file)
      })
    },
    
    # App content
    ...
  )
}

#' Create Standard GMED App Header
#'
#' Creates a consistent header for GMED applications with SSM branding.
#'
#' @param title Main application title
#' @param subtitle Optional subtitle
#' @param logo_url Optional URL to logo image
#' @param class Additional CSS classes
#'
#' @return HTML div element with header styling
#' @export
#'
#' @examples
#' \dontrun{
#' gmed_app_header(
#'   title = "IMSLU Coaching",
#'   subtitle = "Resident Coaching and Assessment Platform"
#' )
#' }
gmed_app_header <- function(title, subtitle = NULL, logo_url = NULL, class = "") {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for UI components")
  }
  
  shiny::fluidRow(
    shiny::column(12,
                  shiny::div(
                    class = paste("gmed-app-header p-3 bg-primary text-white", class),
                    if (!is.null(logo_url)) {
                      shiny::div(
                        class = "float-start me-3",
                        shiny::img(src = logo_url, height = "50px", alt = "Logo")
                      )
                    },
                    shiny::div(
                      class = "text-center",
                      shiny::h1(title, class = "gmed-app-title"),
                      if (!is.null(subtitle)) {
                        shiny::p(subtitle, class = "gmed-app-subtitle mb-0")
                      }
                    )
                  )
    )
  )
}