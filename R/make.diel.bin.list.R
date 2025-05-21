#' Create a List of Diel Bin Expressions Based on Solar Transitions
#'
#' This function generates a named list of logical expressions that define diel periods
#' (e.g., day, night, dawn, dusk) based on sunlight transition times computed from a given
#' location, date, and time zone. It can either use default definitions or allow the user to
#' define their own logical expressions. It validates those expressions against known solar
#' event names, checks for logical consistency, and optionally visualizes the resulting diel bins.
#'
#' @param day A character string representing the logical expression for the "day" period
#'   using sunlight transition times (e.g., \code{"x >= sunriseEnd & x < sunset"}). Default is \code{NULL}.
#'   See details for more information.
#' @param night A character string representing the logical expression for the "night" period.
#'   Default is \code{NULL}. See details for more information.
#' @param dawn A character string representing the logical expression for the "dawn" period.
#'   Only used if \code{twilight = FALSE}. See details for more information.
#' @param dusk A character string representing the logical expression for the "dusk" period.
#'   Only used if \code{twilight = FALSE}. See details for more information.
#' @param twilight Logical; if \code{TRUE}, defines a combined "twilight" bin using default
#'   rules inferred from the difference between day and night. If \code{FALSE}, expects
#'   separate "dawn" and "dusk" periods. Default is \code{TRUE}. If \code{TRUE} and the
#'   user is including their own equality statements, then only \code{day} and \code{night}
#'   should be defined (\code{twilight} is inferred as the remainder of time).
#' @param plot.bins Logical; if \code{TRUE}, produces a diagnostic plot showing how seconds
#'   over a 24-hour period are categorized into diel bins. Default is \code{TRUE}.
#' @param date Character; date string in \code{"YYYY-MM-DD"} or \code{"YYYY/MM/DD"}format for which to test
#'   solar transitions provided by the user. Default is \code{"2020-03-12"}.
#' @param lat Numeric; latitude of the location. Default is \code{41.8781}. Used to test
#'   solar transitions provided by the user.
#' @param lon Numeric; longitude of the location. Default is \code{-87.6298}. Used to test
#'   solar transitions provided by the user.
#' @param tz Character string specifying the time zone (e.g., \code{"US/Central"}). Default is \code{"US/Central"}.
#' to test solar transitions provided by the user.
#'
#' @return A named list of character strings representing logical expressions that define
#'   each diel period. The object is assigned the S3 class \code{"diel.bin.list"}.
#'
#' @details
#' If no expressions are provided for \code{day}, \code{night}, \code{dawn}, or \code{dusk},
#' the function uses default logic based on common sun transition events. The expressions
#' must refer to column names returned by \code{suncalc::getSunlightTimes()}, such as
#' \code{"sunriseEnd"}, \code{"sunset"}, \code{"night"}, etc. 
#' 
#' If \code{twilight = TRUE}, the default \code{diel.bin.list} is:
#' \preformatted{
#'     list(
#'       night = "x <= nightEnd | x > night",
#'       day   = "x >= sunriseEnd & x < sunset"
#'     )
#'    }
#' 
#' If \code{twilight = FALSE}, the default \code{diel.bin.list} is:
#' \preformatted{
#'      list(
#'       night = "x <= nightEnd | x > night",
#'       dawn  = "x > nightEnd & x < sunriseEnd",
#'       day   = "x >= sunriseEnd & x < sunset"
#'     )
#'  }
#'
#' The function verifies that all referenced transition variables exist and that the logic
#' results in valid, non-overlapping diel bins over a 24-hour period. If twilight is enabled,
#' the "twilight" bin is inferred from the gaps between night and day.
#' 
#' ## Tips for Constructing Expressions
#'
#' - Expressions must compare a vector of POSIXct datetimes (\code{x}) to solar event times.
#'   For example: \code{"x >= sunriseEnd & x < sunset"}. The expressions must use
#'   the variable \code{x} to denote the POSIXct datetimes.
#'
#' - All expressions must evaluate to logical vectors and refer only to solar transitions
#'   available from \code{suncalc::getSunlightTimes()} (e.g., \code{"night"}, \code{"sunset"}, etc.).
#'
#' - Each second of the day must fall into exactly one diel bin. Overlap between expressions
#'   is not allowed and will result in an error.
#'
#' - **Avoid using \code{>=} or \code{<=} on the same transition time across multiple bins.**
#'   For example, if you define:
#'   \preformatted{
#'     dawn = "x >= nightEnd & x <= sunriseEnd"
#'     day  = "x >= sunriseEnd & x < sunset"
#'   }
#'   Then \code{x == sunriseEnd} would match both expressions, leading to overlap.
#'   Instead, split the equality cleanly:
#'   \preformatted{
#'     dawn = "x >= nightEnd & x < sunriseEnd"
#'     day  = "x >= sunriseEnd & x < sunset"
#'   }
#'
#' - It's generally safest to use \code{>=} on the left-hand side and \code{<} on the right-hand side
#'   for all expressions. This ensures that adjacent bins are mutually exclusive and collectively exhaustive.
#'
#' - If \code{twilight = TRUE}, you must only define two bins (typically \code{day} and \code{night}).
#'   The \code{twilight} bin will be inferred automatically from the periods between \code{nightEnd}
#'   and \code{sunriseEnd}, and between \code{sunset} and \code{night}.
#'
#' - If \code{twilight = FALSE}, you must define three bins (e.g., \code{day}, \code{night}, and either
#'   \code{dawn} or \code{dusk}) using explicit expressions. Note that when \code{twilight = FALSE} you
#'   likely will not need \code{>=} or \code{<=} in one of the three defined bins. This is because
#'   you are defining diel bins that are adjacent to one another.
#'
#' The function will raise an error if:
#' - Any expressions reference unavailable solar transitions.
#' - Bins overlap (i.e., a time is assigned to more than one bin).
#' - Bins are discontinuous or missing (e.g., some seconds remain unassigned).
#'
#' @seealso [suncalc::getSunlightTimes()] to get a list of the available sun times.
#'
#' @examples
#' # Use default twilight bin definition
#' diel_bins <- make.diel.bin.list(
#'   twilight = TRUE,
#'   plot.bins = FALSE
#' )
#'
#' # Use custom definitions for dawn and dusk
#' diel_bins <- make.diel.bin.list(
#'   day = "x >= sunriseEnd & x < sunset",
#'   night = "x < nightEnd | x >= night",
#'   dawn = "x >= nightEnd & x < sunriseEnd",
#'   twilight = FALSE,
#'   plot.bins = FALSE
#' )
#' @author Mason Fidino
#' @import suncalc
#' @importFrom graphics legend rect
#' @importFrom utils head
#' @export
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

  # Check day, night, dawn, dusk: must be character if not NULL and contain "x"
  for (arg_name in c("day", "night", "dawn", "dusk")) {
    expr <- get(arg_name)
    if (!is.null(expr)) {
      if (!inherits(expr, "character")) {
        stop(sprintf("`%s` must be a character string if provided.", arg_name))
      }
      if (!grepl("\\bx\\b", expr)) {
        stop(sprintf("`%s` must include the variable name 'x' if provided.", arg_name))
      }
    }
  }
  
  # Check twilight and plot.bins: must be logical scalars
  if (!inherits(twilight, "logical") || length(twilight) != 1) {
    stop("`twilight` must be a logical (TRUE or FALSE).")
  }
  if(!twilight){
    warning(
      paste(
        "twilight set to FALSE. If you are planning on using Diel.Niche::diel.fit()",
        "to classify a species diel phenotype, then twilight must be TRUE."
      )
    )
  }
  if (!inherits(plot.bins, "logical") || length(plot.bins) != 1) {
    stop("`plot.bins` must be a logical (TRUE or FALSE).")
  }
  
  # Check date: must be character or Date
  if (!inherits(date, c("Date", "character"))) {
    stop("`date` must be a character string or a Date object.")
  }
  # If character, ensure format is yyyy-m-d or yyyy/m/d
  if (inherits(date, "character")) {
    if (!grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}$", date)) {
      stop("`date` must be in format 'yyyy-m-d' or 'yyyy/m/d'.")
    }
  }
  
  # Check lat/lon: must be numeric scalars within valid ranges
  if (!inherits(lat, "numeric") || length(lat) != 1 || lat < -90 || lat > 90) {
    stop("`lat` must be a numeric value between -90 and 90.")
  }
  if (!inherits(lon, "numeric") || length(lon) != 1 || lon < -180 || lon > 180) {
    stop("`lon` must be a numeric value between -180 and 180.")
  }
  
  # Check tz: must be character and valid Olson timezone
  if (!inherits(tz, "character") || length(tz) != 1) {
    stop("`tz` must be a character string representing a timezone.")
  }
  if (!tz %in% OlsonNames()) {
    stop(sprintf("`tz` must be a valid timezone listed in OlsonNames(). You provided: '%s'", tz))
  }
  # If all are NULL, define default expressions based on known transition times
  if (all(sapply(list(day, night, dawn, dusk), is.null))) {
    bin.type.list <- if(twilight){
      list(
        night = "x <= nightEnd | x > night",
        day   = "x >= sunriseEnd & x < sunset"
        # twilight is inferred as: x >= nightEnd & x < sunriseEnd OR x >= sunset & x < night
      )
    } else {
      list(
        night = "x <= nightEnd | x >= night",
        dawn  = "x > nightEnd & x < sunriseEnd",
        day   = "x >= sunriseEnd & x < sunset"
      )
    }
  } else {
    # Start with empty list
    bin.type.list <- list()
    n_not_null <- sum(
      c(!is.null(day),!is.null(night),
        !is.null(dawn),!is.null(dusk)
      )
    )
    if(twilight & n_not_null >2){
      stop("If twilight = TRUE then only 2 diel bins need to be defined with equalities.")
    }
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
        "night -> dawn, dawn -> day, day -> dusk, dusk -> night.",
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
  class(bin.type.list) <- "diel.bin.list"
  return(bin.type.list)
}


