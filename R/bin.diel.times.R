library(Diel.Niche)

data("camera.data")
data <- camera.data
datetime.column <- "datetime"
lat.column = "lat"
lon.column = "lon"
species.column = "species"
species.column = NULL
bin.type.list = make.diel.bin.list(plot.bins = FALSE)
na_vals = c("remove", "error")
bin.diel.times <- function(
    data,
    datetime.column,
    lat.column,
    lon.column,
    species.column = NULL,
    bin.type.list = make.diel.bin.list(plot.bins = FALSE),
    na_vals = c("remove", "error")
){
  # if bin.type.list not provided, create one based on 
  #  if twilight is TRUE or FALSE
  if(is.null(bin.type.list)){
    if(twilight){
      bin.type.list <- list(
        night = "x < nightEnd | x >= night",
        day = "x > sunriseEnd & x >= sunset"
      )
    } else {
      bin.type.list <- list(
        night = "x < nightEnd | x >= night",
        day = "x > sunriseEnd & x >= sunset",
        dawn = "x >= nightEnd & x < sunriseEnd "
      )
    }
  }
  # some quick checks on na_vals
  if(is.null(na_vals)){
    stop("na_vals cannot be null")
  }
  if(length(na_vals)>1){
    na_vals <- "remove"
  }
  # get timezone & date from datetime object
  my_tz <- attr(data[[datetime.column]], "tzone")
  data$date <- as.Date(
    data[[datetime.column]],
    tz = my_tz
  )
  
  # get the datetime columns specified in bin.type.list
 datetime_cols <- get.diel.vars(
   bin.type.list
  )

  # double-check if they are all in this vector

  
  
  
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
      warning(remove_message)
      data <- data[-which(any_na>0),]
      time_frame <- time_frame[-which(any_na>0),]
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
  
  # Check to make sure the bins cover the whole 24 hour period
  
  
  
  
  
}

