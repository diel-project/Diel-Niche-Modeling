
make.diel.bin.list <- function(
    day = NULL,
    night = NULL,
    dawn = NULL,
    dusk = NULL,
    twilight = TRUE,
    plot.bins = TRUE,
    date = "2020-03-12",
    lat = 41.8781,
    lon = -87.6298,
    tz = "US/Central"
) {
  # If all are NULL, define default expressions based on known transition times
  if (all(sapply(list(day, night, dawn, dusk), is.null))) {
    bin.type.list <- if(twilight){
      list(
        night = "x < nightEnd | x >= night",
        day   = "x >= sunriseEnd & x < sunset"
        # twilight is inferred as: x >= nightEnd & x < sunriseEnd OR x >= sunset & x < night
      )
    } else {
      list(
        night = "x < nightEnd | x >= night",
        dawn  = "x >= nightEnd & x < sunriseEnd",
        day   = "x >= sunriseEnd & x < sunset"
      )
    }
  } else {
    # Start with empty list
    bin.type.list <- list()
    
    # Add user-supplied expressions to the list
    if(!is.null(night)){
      bin.type.list$night <- night
    } 
    if(!is.null(day)){
      bin.type.list$day   <- day
    }
    if(!twilight) {
      if(!is.null(dawn)){
        bin.type.list$dawn <- dawn
      }
      if(!is.null(dusk)){
        bin.type.list$dusk <- dusk
      }
    }
    # check to make sure that we have 2 cats for twilight
    #  and 3 for !twilight
  }
  date <- as.Date(date, tz = tz)
  if(twilight & length(bin.type.list) != 2){
    stop(
      paste0(
        "If twilight = TRUE, then only two diel periods ",
        "need to be defined. You specified ",
        length(bin.type.list),". You defined:\n\n",
        paste0(names(bin.type.list), collapse = ", "),
        sep = " "
      )
    )
  }
  if(!twilight & length(bin.type.list) != 3){
    stop(
      paste0(
        "If twilight = FALSE, then only three diel periods ",
        "need to be defined. You specified ",
        length(bin.type.list),". You defined:\n\n",
        paste0(names(bin.type.list), collapse = ", "),
        sep = " "
      )
    )
  }
  # get the categories and make sure they can be pulled from 
  # suncalc::getSunlightTimes().
  datetime_cols <- get.diel.vars(
    bin.type.list
  )
  possible_datetime_cols <- c(
    "solarNoon", "nadir", "sunrise", "sunset", "sunriseEnd", "sunsetStart",
    "dawn", "dusk", "nauticalDawn", "nauticalDusk", "nightEnd", "night", "goldenHourEnd",
    "goldenHour"
  )
  if(!all(datetime_cols %in% possible_datetime_cols)){
    
    to_report <- datetime_cols[
      !datetime_cols %in% possible_datetime_cols
    ]
    stop(
      paste(
        "The named sunlight times in bin.type.list",
        "do not match what is available within",
        "suncalc::getSunlightTimes(). Fix the error",
        "within what you provided to bin.type.list.",
        "The error was caused by:\n\n",
        paste0(sort(to_report), collapse = ", "),
        "\n\nThe available sunlight times are:\n\n",
        paste0(sort(possible_datetime_cols), collapse = ", "),
        sep = " "
      )
    )
  }
  
  sun_times <- suncalc::getSunlightTimes(
    date = date,
    lat = lat,
    lon = lon,
    tz = tz,
    keep = datetime_cols
  )
  if(any(is.na(sun_times[,datetime_cols]))){
    stop(
      paste(strwrap(
        paste(
        "The location & time you specified has resulted in NA",
        "sun times for at least one of the transition points",
        "specified in either day, night, dawn, or dusk. This",
        "often occurs at high latitudes. Based on the definitions",
        "provided you cannot apply this analysis in this region.",
        "We are working on creating more holistic definitions that",
        "can be applied anywhere around the world."
      )), collapse = "\n")
    )
  }
  # Generate a day of seconds
  x <- seq.POSIXt(
    from = as.POSIXct(
      paste(as.character(date), "00:00:00"),
      tz = tz
    ),
    to = as.POSIXct(
      paste(as.character(date), "23:59:59"),
      tz = tz
    ),
    by = "1 sec"
  )
  
  # Evaluate bin logic
  eval_env <- list2env(
    list(
      x = x
    ),
    parent = baseenv()
  )
  for(i in 1:length(datetime_cols)){
    eval_env[[datetime_cols[i]]] <- sun_times[[datetime_cols[i]]]
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
  if(twilight){
    missing_cat <- setdiff(
      c("night", "day", "twilight"),
      names(bin.type.list)
    )
  } else {
    missing_cat <- setdiff(
      c("night", "day", "dawn", "dusk"),
      names(bin.type.list)
    )
  }
  colnames(assigned_matrix)[ncol(assigned_matrix)] <- missing_cat
  
  # check one last time
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
  arle <- rle(assigned)
  if(length(arle$values) != 5){
    to_report <- data.frame(
      lengths = arle$lengths,
      diel_category = arle$values
    )
    rle_table <- paste0(
      sprintf("%-10s | %-10s", "diel_category", "length"),
      "\n", strrep("-", 23), "\n",
      paste(
        sprintf("%-10s | %-10d", arle$values, arle$lengths),
        collapse = "\n"
      )
    )
    stop(
      paste0(
        "Expected 4 transitions across diel bins over a 24-hour period: ",
        "night → dawn, dawn → day, day → dusk, dusk → night.",
        "However, ", length(arle$values) - 1, " transitions were detected. ",
        "\n\nThis was determined by identifying runs of consecutive seconds assigned to the same diel bin. ",
        "Unexpected transitions suggest issues in the bin definitions or logic. ",
        "Run lengths of each diel bin:\n\n",
        rle_table
        )
      )
      

  }
  # visualize the diel bins.
  if(plot.bins){
    cat("plotting diel bins...\n")
    if(twilight){
      bin_cols <- data.frame(
        bin = c("night", "day", "twilight"),
        color = c("#0073C2FF", "#EFC000FF", "#A73030FF")
      )
    } else {
      bin_cols <- data.frame(
        bin = c("night", "day", "dawn", "dusk"),
        color = c("#0073C2FF", "#EFC000FF", "#A73030FF", "#79AF97FF")
      )
    }
    
    # Match colors to bins in the right order
    colors <- bin_cols$color[match(arle$values, bin_cols$bin)]
    
    starts <- cumsum(c(0, head(arle$lengths, -1)))
    ends <- cumsum(arle$lengths)
    
    # Make a horizontal stacked bar chart
 
    plot(
      NULL, xlim = c(0, sum(arle$lengths)), ylim = c(0, 1),
      xlab = "Seconds since midnight", ylab = "",
      yaxt = "n", bty = "n", main = "Diel Bins Across a 24-Hour Period"
    )
    
    # Draw the colored rectangles
    for (i in seq_along(starts)) {
      rect(
        xleft = starts[i], xright = ends[i],
        ybottom = 0.2, ytop = 0.6,
        col = colors[i], border = NA
      )
    }
    
    # Add legend
    legend("topright", legend = bin_cols$bin, fill = bin_cols$color, bty = "n")
  }

  # Check to ensure
  
  return(bin.type.list)
}

test <- make.diel.bin.list(twilight = TRUE)
