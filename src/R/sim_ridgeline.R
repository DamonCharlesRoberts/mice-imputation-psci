'.__module__.'
#' generate ridgeline plots for simulated data
#' dependencies
#' @param data data to be graphed
#' @param datasets num of samples used
#' @param type type of sample. either original or amputed
#' @param var variable to display
#' @export
sim_ridgeline <- function (
    data
    ,datasets
    ,type
    ,var
) {
    # get the data and clean it
    datasetSelect <- seq(1, 1000, 100)
    shortenedDF <- data[dataset %in% datasetSelect]
    # make a ridgeline plot
    ggplot2::ggplot(data = shortenedDF) +
        ggridges::geom_density_ridges(
            ggplot2::aes(
                x=.data[[var]]
                ,y=factor(dataset)
            )
            ,alpha=0.5
        ) +
        ggplot2::theme(
            legend.position="none"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            y = "Sample #"
            ,x=""
        )
}
