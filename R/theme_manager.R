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