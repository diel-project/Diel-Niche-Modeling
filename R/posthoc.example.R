#' @title Post-hoc niche classification example data
#' @description The necessary components to use \code{\link{prob.overlap}} with the output from \code{overlap::densityPlot}.
#' @format A list object with three components
#' \describe{
#'   \item{tiger.kde}{the outputted matrix from \code{overlap::densityPlot()} when fit to the tiger data subset from their \code{kerinci} data object.}
#'    \describe{
#'      \item{x: }{a column vector of length 10000 that ranges from 0 to 24, which represents time in a day}
#'      \item{y: }{a column vector of length 10000 that is the kernel density estimate for each respective data point of \code{x}}
#'    }
#'   \item{dawn.range}{a numeric vector of length 2 that is the start and end of dawn in proportional hours for this example.}
#'   \item{dusk.range}{a numeric vector of length 2 that is the start and end of dusk in proportional hours for this example.}
#' }
#' @details
#' When we generated this kernel density estimate we set the \code{n.grid} argument within \code{overlap::densityPlot()} to 10000.
#' We did this because to get the probability of each diel period we must integrate under the curve, which needs a lot of samples to do
#' accurately. To get dawn and dusk range we used the \code{suncalc} package with the general area of the
#' Kerinci Seblat National Park (where the tiger data came from), used \code{Asia/Jakarta} as the local time zone,
#' and chose \code{2008-03-12} as the date to query \code{c("sunset", "night", "sunrise","nightEnd")}. As this area is
#' close to the equator there is little variation over the year in when dawn and dusk start, so the time of year matters
#' less in this specific case. In our case, dawn occurred between nightEnd and dawn whereas dusk occurred between dusk and night.
#' 
#' The help file for \code{\link{posthoc.niche}} includes the code used to generate all the
#' pieces of this specific file.
#' 
#' @source 
#' 
#' Ridout, M.S. and Linkie, M. (2009) Estimating overlap of daily activity patterns 
#' from camera trap data. Journal of Agricultural, Biological and Environmental Statistics, 14, 322-337.
"posthoc.example"