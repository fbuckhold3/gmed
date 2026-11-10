#' @title Theme and Color Management for GMED
#' @description Color schemes, theme functions, and resource loading for a
#'   consistent, institution-neutral gmed brand (built to scale across
#'   programs, not tied to one health system's colors)
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

#' GMED Color Palette (deprecated; bridges to roundsui)
#'
#' gmed's own palette is being retired in favor of roundsui's "Ward Notes"
#' tokens. This keeps the old field names so existing callers keep working,
#' but every value now comes from \code{roundsui::roundsui_colors()}: the
#' old teal/cyan/coral brand colors all map to the single roundsui accent,
#' status and neutral fields map to their roundsui equivalents. New code
#' should call \code{roundsui::roundsui_colors()} directly.
#'
#' @return Named list of colors (old gmed field names, roundsui values)
#'
#' @examples
#' \dontrun{
#' colors <- gmed_colors()
#' colors$primary  # "#3355D8" (roundsui accent)
#' }
gmed_colors <- function() {
  # DEPRECATED palette: gmed is retiring its own colors in favor of
  # roundsui ("Ward Notes"). Field names are kept so existing callers don't
  # break, but every value now comes from roundsui::roundsui_colors().
  # New code should call roundsui::roundsui_colors() directly.
  rc <- roundsui::roundsui_colors()
  list(
    # Primary Brand Colors (old teal/cyan/coral all collapse onto the accent)
    primary = rc$accent,
    secondary = rc$accent,
    light_blue = rc$accent,
    accent = rc$accent,
    accent_blue = rc$accent,       # Deprecated alias for `accent`

    # Legacy Coaching Colors (kept for backward compatibility; unreferenced elsewhere)
    coach_primary = rc$accent,
    coach_secondary = rc$accent,

    # Status Colors
    success = rc$success,
    warning = rc$warning,
    danger = rc$danger,
    info = rc$accent,

    # Neutral Colors
    neutral_gray = rc$ink_muted,
    light_gray = rc$surface_2,
    white = "#ffffff",

    # Text Colors
    text_primary = rc$ink,
    text_secondary = rc$ink_muted,
    text_muted = rc$ink_faint,

    # Background Colors
    bg_primary = rc$ground,
    bg_secondary = rc$surface_2,
    bg_dark = rc$ink
  )
}

#' SSM SLUCare Color Palette (deprecated)
#'
#' Deprecated alias for \code{\link{gmed_colors}}. gmed moved to an
#' institution-neutral palette so it can be reused across programs, not
#' just SSM/SLUCare; this wrapper exists only so any caller still reading
#' \code{ssm_colors()} directly doesn't break.
#'
#' @return Named list of gmed brand colors (see \code{\link{gmed_colors}})
#' @export
ssm_colors <- function() {
  .Deprecated("gmed_colors")
  gmed_colors()
}

#' Create GMED Bootstrap Theme
#'
#' Creates a bslib theme with the gmed color palette and styling.
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

  # Get gmed colors
  colors <- gmed_colors()

  # Apply any custom color overrides
  if (!is.null(custom_colors)) {
    colors[names(custom_colors)] <- custom_colors
  }

  # Create the theme
  bslib::bs_theme(
    version = version,
    primary = colors$secondary,      # Main interactive cyan as bslib primary
    secondary = colors$light_blue,   # Lighter cyan as secondary
    success = colors$success,
    warning = colors$warning,
    danger = colors$danger,
    info = colors$accent,
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
#' Creates a consistent header for GMED applications.
#'
#' @param title Main application title
#' @param subtitle Optional subtitle
#' @param logo_url Optional URL to logo image
#' @param class Additional CSS classes
#'
#' @return HTML div element with header styling
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