% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_ridgeline.R
\name{sim_ridgeline}
\alias{sim_ridgeline}
\title{sim_ridgeline}
\usage{
sim_ridgeline(data_frame, str_var)
}
\arguments{
\item{data_frame}{data to be graphed}

\item{str_var}{variable from the data_frame to be graphed}
}
\description{
This function generates a ridgeline plot for the simulated data.
It returns a ggplot2 object showing about 10 plots from 10 of
the datasets from the sampling.
}
\details{
It returns only ten plots as the samples generated in this project
are a lot (1000), so that would ask for there to be 1000 plots in
one figure! Yikes! So instead, I create 10 plots from 10 of the
datasets.
}
