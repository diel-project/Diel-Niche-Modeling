#' Bin Datetime Values into Diel Categories
#'
#' Assigns observations to diel bins (e.g., day, night, dawn, dusk, twilight) based on
#' sun angle transitions calculated with `suncalc::getSunlightTimes()`. The function uses
#' user-defined logical expressions stored in a `diel.bin.list` to classify times.
#'
#' @param data A data frame containing observation data, including datetime, latitude, and longitude columns, and an optional species column if your
#' dataset has multiple species.
#' @param datetime.column Character string giving the name of the POSIXct datetime column.
#' @param lat.column Character string giving the name of the latitude column (in decimal degrees).
#' @param lon.column Character string giving the name of the longitude column (in decimal degrees).
#' @param bin.type.list A list of logical expressions generated with [make.diel.bin.list()]. Must be of class `diel.bin.list`.
#' @param na_vals Character; how to handle NA values returned by `suncalc::getSunlightTimes()`. Must be either `"remove"` (default) or `"error"`. See
#' details for more information.
#'
#' @return A copy of the input data frame with an additional `dielBin` column specifying the assigned diel bin for each observation.
#'
#' @details
#' The function calculates solar transition times (e.g., sunrise, sunset, dawn, dusk) using the `suncalc` package. It then evaluates
#' user-specified logical expressions to determine which bin each observation falls into. The bin definitions must be specified
#' using a \code{diel.bin.list} object created with [make.diel.bin.list()]. This allows users to either define how each bin is
#' constructed with logical expressions based on solor transitions, or to use the default bins provided.
#'
#'
#'
#' # Example Expression Format
#'
#' @importFrom suncalc getSunlightTimes
#'
#' @examples
#' # Example with default bin.list
#' data(camera.data)
#' 
#' # remove images of the same species that occurred
#' #  within 15 minutes of a previous photo record
#' #  by site.
#' thinned_data <- trim.time(
#'   data = camera.data,
#'   datetime.column = "datetime",
#'   site.column = "surveyID",
#'   species.column = "species",
#'   progress.bar = FALSE
#' )
#' 
#' # construct bin list, using defaults
#' bin_list <- make.diel.bin.list(
#'   plot.bins = FALSE
#' )
#' 
#' # bin diel times
#' thinned_data <- bin.diel.times(
#'   data = thinned_data,
#'   datetime.column = "datetime",
#'   lat.column = "lat",
#'   lon.column = "lon",
#'   bin.type.list = bin_list
#' )
#' 
#' # calculate frequency of each bin,
#' #  this could then be
#' y <- table(
#'   thinned_data$species,
#'   thinned_data$dielBin
#' )
#' @seealso [make.diel.bin.list()]
#' @author Mason Fidino
#' @export
bin.diel.times <- function(
    data,
    datetime.column,
    lat.column,
    lon.column,
    bin.type.list = make.diel.bin.list(plot.bins = FALSE),
    na_vals = c("remove", "error")
){
  if(nrow(data) == 1){
    stop("Cannot apply this function to a single data point")
  }
  # data check
  if(!inherits(data, "data.frame")){
    stop("data must be a data.frame")
  }
  # datetime.column checks
  if (!datetime.column %in% colnames(data)) {
    stop("datetime.column must be a column name in 'data'")
  }
  # get timezone & date from datetime object
  if(!inherits(data[[datetime.column]], "POSIXct")){
    stop(
      "The datetime column in data must be of class POSIXct"
    )
  }
  # lat.column and lon.column must exist and be numeric
  if(!lat.column %in% colnames(data) || !is.numeric(data[[lat.column]])){
    stop("lat.column must be the name of a numeric column in 'data'")
  }
  if(!lon.column %in% colnames(data) || !is.numeric(data[[lon.column]])){
    stop("lon.column must be the name of a numeric column in 'data'")
  }
  # Check lat/lon ranges
  if(any(data[[lat.column]] < -90 | data[[lat.column]] > 90, na.rm = TRUE)){
    stop("Latitude values must be between -90 and 90 decimal degrees")
  }
  if(any(data[[lon.column]] < -180 | data[[lon.column]] > 180, na.rm = TRUE)){
    stop("Longitude values must be between -180 and 180 decimal degrees")
  }
  # some quick checks on na_vals
  if(is.null(na_vals)){
    stop("na_vals cannot be NULL")
  }
  if(length(na_vals)>1){
    na_vals <- "remove"
  }
  na_vals <- match.arg(na_vals, choices = c("remove", "error"))

  my_tz <- attr(data[[datetime.column]], "tzone")
  if(is.null(my_tz) || my_tz == ""){
    stop(
      "The timezone could not be pulled from the datetime.column in data. Please set it explicitly.")
  }
  data$date <- as.Date(
    data[[datetime.column]],
    tz = my_tz
  )
  if(!inherits(bin.type.list, "diel.bin.list")){
    stop(
      "bin.type.list must be of class diel.bin.list. Use make.diel.bin.list() to construct your bin.type.list."
    )
  }
  # get the datetime columns specified in bin.type.list
 datetime_cols <- get.diel.vars(
   bin.type.list
  )

  time_frame <- suncalc::getSunlightTimes(
    data = data[,c("date",lat.column, lon.column)],
    keep = datetime_cols,
    tz = my_tz
  )
  # Check to see if there are any NA values.
  any_na <- sapply(
    datetime_cols,
    function(column){
      is.na(time_frame[[column]])
    }
  )
  any_na <- rowSums(any_na)
  if(any(any_na>0)){
    if(na_vals == "remove"){
      remove_message <- paste(
        "NA values detected when calculating sunlight times.",
        "This often occurs with data from higher latitudes during certain times of the year.",
        sprintf("Of %d rows in data, %d have been removed.", nrow(data), sum(any_na > 0)),
        "To investigate this, set na_vals = 'error' instead.",
        sep = " "
      )
      
      data <- data[-which(any_na>0),]
      time_frame <- time_frame[-which(any_na>0),]
      if(nrow(time_frame) == 0){
        error_message <- paste(
          "All the data was removed when na_vals was set to 'remove'",
          "This can occur with data from higher latitudes during certain times of the year.",
          "try applying suncalc::getSunlightTimes() to see what sunlight transition times",
          "are missing."
        )
        
        stop("All the data was removed when na_vals was set to 'remove'")
      }
      warning(remove_message)
    }
    if(na_vals == "error"){
      error_message <- paste(
        "NA values detected when calculating sunlight times.",
        sprintf("Problematic rows: \n\n%s", 
                paste0("c(", paste(which(any_na>0), collapse = ", "),")")),
        "\n\nset na_vals = 'remove' to drop these rows.",
        sep = " "
      )
      stop(error_message)
    }
  }
  
  # check to make sure the dates in datetime_cols matches
  #  the date provided within data.
  matches <- sapply(
    datetime_cols,
    function(column){
      as.Date(time_frame[[column]], tz = my_tz) == time_frame$date
    }
  )
  if(any(rowSums(matches) != ncol(matches))){
    
    to_find <- which(
      !matches,
      arr.ind = TRUE
    )
    err_message <- paste(
      "Some of the dates coming from suncalc::getSunlightTimes()",
      "do not match the date you provided in your datetime.column.",
      "This can happen in certain parts of the world during times of",
      "the year. This is a known issue with suncalc::getSunlightTimes().",
      "To help us develop a workaround, please create an issue with",
      "a reproducible example here:\n\nhttps://github.com/diel-project/Diel-Niche-Modeling/issues",
      collapse = " "
    )
    stop(err_message)
  }
  
  eval_env <- list2env(
    list(
      x = data[[datetime.column]]
    ),
    parent = baseenv()
  )
  for(i in 1:length(datetime_cols)){
    eval_env[[datetime_cols[i]]] <- time_frame[[datetime_cols[i]]]
  }
  
  bin_results <- lapply(
    bin.type.list,
    function(expr) {
      eval(parse(text = expr)[[1]], envir = eval_env)
    })
  
  assigned_matrix <- do.call(
    cbind,
    bin_results
  )
  row_sums <- rowSums(assigned_matrix)
  # assign the last category
  
  assigned_matrix <- cbind(
    assigned_matrix,
    ifelse(
      row_sums == 0,
      TRUE,
      FALSE
    )
  )
  if(length(bin.type.list) == 2){
    missing_cat <- setdiff(
      c("night", "day", "twilight"),
      names(bin.type.list)
    )
    my_levels <- c("twilight", "day", "night")
  } else {
    missing_cat <- setdiff(
      c("night", "day", "dawn", "dusk"),
      names(bin.type.list)
    )
    my_levels <- c("night", "day", "dawn", "dusk")
  }
  colnames(assigned_matrix)[ncol(assigned_matrix)] <- missing_cat
  row_sums <- rowSums(assigned_matrix)
  # Check
  if(any(row_sums > 1)){
    stop(
      paste(
        "The defined diel bins you provided generated overlap.",
        "This occurs if you either:\n\n1) Specify overlapping sun times",
        "across night, day, dawn, or dusk.\n2) You have at least two equalities",
        "that that are both >= or <= on the same sun time (e.g., sunset)."
      )
    )
  }
  # check to make sure the bins are continuous (save for twilight)
  assigned <- rep(NA, nrow(assigned_matrix))
  for(i in 1:ncol(assigned_matrix)){
    assigned[
      assigned_matrix[,i]
    ] <- colnames(assigned_matrix)[i]
  }
  
  data$dielBin <- factor(
    assigned,
    levels = my_levels
  )

  
  return(data)

}
  
  
  
  

