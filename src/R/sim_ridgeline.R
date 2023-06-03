#' @title sim_ridgeline
#'
#' @description
#' This function generates a ridgeline plot for the simulated data.
#' It returns a ggplot2 object showing about 10 plots from 10 of
#' the datasets from the sampling.
#'
#' @details
#' It returns only ten plots as the samples generated in this project
#' are a lot (1000), so that would ask for there to be 1000 plots in
#' one figure! Yikes! So instead, I create 10 plots from 10 of the
#' datasets.
#'
#' @param data_frame data to be graphed
#' @param str_var variable from the data_frame to be graphed
#' @export
sim_ridgeline <- function (
  data_frame
  , str_var
) {
  # get the data and clean it
  list_dataset_select <- seq(1, 1000, 100)
  df_shortened <- data_frame[dataset %in% list_dataset_select]
  # make a ridgeline plot
  ggplot2::ggplot(
    data = df_shortened # make a plot with the shortened data_frame
  ) +
  ggridges::geom_density_ridges( # make a rideline plot
    ggplot2::aes(
      x = .data[[str_var]] # use the pre_specified variable passed
      , y = factor(dataset) # and put the dataset # on the y axis
    )
    , alpha = 0.5 # don't make the fill solid
  ) +
  ggplot2::theme(
    legend.position = "none" # do not include a legend
  ) +
  ggplot2::theme_minimal() + # use the minimal theme
  ggplot2::labs(
    y = "Sample #" # add this to the y-axis for the label
    , x = "" # do not include anything for the x-axis lable
    )
}