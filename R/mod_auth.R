#' @title Authentication Shiny Module
#' @description Provides mod_auth_ui / mod_auth_server — a self-contained login
#'   module that renders the Design A login screen and validates access codes
#'   via gmed::authenticate_resident().
#' @name mod_auth
NULL

#' Authentication Module UI
#'
#' Wraps \code{\link{gmed_login_page}} in a Shiny module namespace. Drop this
#' into any app's UI and pair it with \code{mod_auth_server}.
#'
#' @param id Module namespace ID
#' @param app_title Application title shown on the login screen
#' @param app_subtitle Program / subtitle line
#' @param org_eyebrow Small all-caps wordmark pill
#' @param disclaimer_text Full disclaimer text (NULL uses the IMSLU default)
#'
#' @return Shiny UI tagList
#' @export
mod_auth_ui <- function(id,
                        app_title       = "IMSLU Resident Portal",
                        app_subtitle    = "Internal Medicine Residency",
                        org_eyebrow     = "SSM HEALTH \u00b7 SLUCARE",
                        disclaimer_text = NULL) {

  ns <- shiny::NS(id)

  gmed_login_page(
    id              = ns("access_code"),
    app_title       = app_title,
    app_subtitle    = app_subtitle,
    org_eyebrow     = org_eyebrow,
    disclaimer_text = disclaimer_text
  )
}

#' Authentication Module Server
#'
#' Handles the sign-in button click, validates the access code via
#' \code{\link{authenticate_resident}}, and surfaces an inline error message
#' on failure. Returns a reactive that holds the full auth result list.
#'
#' @param id Module namespace ID (must match the corresponding \code{mod_auth_ui} call)
#' @param residents_r Reactive expression returning the residents data frame.
#'   The data frame must include an \code{access_code} column.
#' @param allow_record_id Whether to also accept record_id as a valid code
#'   (default: TRUE — useful for admin access)
#'
#' @return A \code{\link[shiny]{reactiveVal}} that is \code{NULL} before the
#'   first attempt, then a list with:
#'   \describe{
#'     \item{success}{Logical}
#'     \item{resident_info}{Named list of resident fields, or NULL}
#'     \item{message}{Character status message}
#'   }
#' @export
mod_auth_server <- function(id, residents_r, allow_record_id = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {

    auth_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$access_code_btn, {
      shiny::req(input$access_code)

      result <- authenticate_resident(
        access_code     = input$access_code,
        residents_df    = residents_r(),
        allow_record_id = allow_record_id
      )

      auth_result(result)

      # Show inline error on failure; clear it on success
      output$access_code_error <- shiny::renderUI({
        if (!result$success) {
          shiny::div(class = "gmed-login-error", result$message)
        }
      })
    })

    # Return the reactive so callers can observe auth state
    auth_result
  })
}
