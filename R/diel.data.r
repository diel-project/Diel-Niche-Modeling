#' @title Data for Diel.Niche
#' @description A multi-species dataset containing detection frequencies during the twilight, daytime, and nighttime. Data come from camera-trapping along an urban gradient in Chicago, Illinois, USA. 
#' @format A data frame with 232 rows and 19 variables:
#' \describe{
#'   \item{scientificName}{Genus and species name}
#'   \item{twilight}{Frequency of detections during the twilight period; sunrise and sunset.}
#'   \item{day}{Frequency of detections during the daytime period.}
#'   \item{night}{Frequency of detections during the nighttime period.}
#'   \item{nsite}{The number of unique camera locations.}
#'   \item{min_date}{The first date of sampling in the sampling period.}
#'   \item{max_date}{The last date of sampling in the sampling period.}
#'   \item{mean_lat}{Avergage latitude of all camera locations during the sampling period.}
#'   \item{mean_lon}{Avergage longtitude of all camera locations during the sampling period.}
#'   \item{season}{Seasonal classification of the sampling period by summer, autumn, winter, and spring.}
#'   \item{country}{Country where thes sampling occured.}
#'   \item{phylum}{Phylum of the species.}
#'   \item{class}{Class of the species.}
#'   \item{order}{Order of the species.}
#'   \item{family}{Damily of the species.}
#'   \item{Project}{Project code name, and the city, state, and country location.}
#'   \item{unit_type}{The length of the sampling period.}
#'   \item{Common_name}{Common name for the species.}
#'   \item{Activity_Literature}{Activity classification for the species from the literature; see Cox, D. T. C., Gardner, A. S., & Gaston, K. J. (2021). Diel niche variation in mammals associated with expanded trait space. Nature communications, 12(1), 1753.}
#' }
#' @source \url{https://doi.org/10.1111/gcb.15800}
"diel.data"