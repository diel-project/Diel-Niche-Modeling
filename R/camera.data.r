#' @title Camera trap data for Diel.Niche
#' @description 2012 camera trap data of Virginia opossum (Didelphis virginiana) from urban greenspace throughout Chicago, Illinois, USA
#' @format A data frame with 2441 rows and 4 variables:
#' \describe{
#'   \item{species}{The common name of the species data.}
#'   \item{surveyID}{The site and season code for the respective opossum detection. The site is denoted by the first 8 characters and the season is the last four. WI12 = winter 2012, SP12 = spring 2012, SU12 = summer 2012, and FA12 = fall 2012.}
#'   \item{datetime}{The date & time (m-d-y h:m:s) of the respective opossum detection. Seconds are present in this column but were not actually included.}
#'   \item{timezone}{The timezone of the data as recognized by R (i.e., US/Central).}
#' }
#' @source \url{https://doi.org/10.1111/1365-2656.12967}
"camera.data"