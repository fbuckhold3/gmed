#' @title Milestone Visualization Functions
#' @description Functions for creating milestone assessment plots and visualizations
#' @name milestone_plotting
NULL

#' Create Milestone Radar Plot
#' 
#' Creates milestone spider/radar plot for resident assessments
#' @param self_scores Vector of self-assessment scores
#' @param program_scores Vector of program assessment scores  
#' @param milestone_names Vector of milestone names
#' @param resident_name Name of resident
#' @param period Review period
#' @return ggplot object
#' @export
miles_plot <- function(self_scores, program_scores, milestone_names, 
                       resident_name, period) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for milestone plotting")
  }
  
  # Ensure we have valid data
  if (length(self_scores) == 0 || length(program_scores) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::labs(title = "No milestone data available"))
  }
  
  # Prepare data for radar plot
  plot_data <- data.frame(
    milestone = rep(milestone_names, 2),
    score = c(self_scores, program_scores),
    assessment_type = rep(c("Self", "Program"), each = length(milestone_names)),
    stringsAsFactors = FALSE
  )
  
  # Remove any NA scores
  plot_data <- plot_data[!is.na(plot_data$score), ]
  
  if (nrow(plot_data) == 0) {
    return(ggplot2::ggplot() + 
             ggplot2::labs(title = "No valid milestone scores"))
  }
  
  # Create radar plot using polar coordinates
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = milestone, y = score, 
                                               color = assessment_type, 
                                               group = assessment_type)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::coord_polar() +
    ggplot2::scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    ggplot2::scale_color_manual(values = c("Self" = "#1f77b4", "Program" = "#ff7f0e")) +
    ggplot2::labs(
      title = paste("Milestone Assessment -", resident_name),
      subtitle = paste("Period:", period),
      color = "Assessment Type",
      x = "",
      y = "Score"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 8),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
  
  return(p)
}
