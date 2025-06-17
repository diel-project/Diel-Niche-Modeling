#' @keywords internal

get.diel.vars <- function(x){
    all_expr <- paste(
      x,
      collapse = " "
    )
    all_vars <- unlist(
      regmatches(
        all_expr,
        gregexpr("\\b[a-zA-Z]\\w*\\b", all_expr)
      )
    )
  datetime_cols <- setdiff(
    all_vars,
    c("x", "TRUE", "FALSE")
  )
  return(datetime_cols)
}

#' @keywords internal
parse_conditions <- function(expr) {
  # Regular expression to extract conditions of the form: x OP var
  pattern <- "x\\s*(<=|>=|<|>)\\s*(\\w+)"
  
  # Extract matches
  matches <- gregexpr(pattern, expr, perl = TRUE)
  parts <- regmatches(expr, matches)[[1]]
  
  # Extract operators and variable names
  ops <- gsub(pattern, "\\1", parts, perl = TRUE)
  vars <- gsub(pattern, "\\2", parts, perl = TRUE)
  
  # Extract logical operators between conditions
  joiners <- unlist(strsplit(expr, pattern, perl = TRUE))
  joiners <- gsub("\\s+", "", joiners)
  joiners <- joiners[joiners %in% c("|", "&")]
  joiners <- c(joiners, NA)  # align lengths
  
  data.frame(variable = vars, operator = ops, join = joiners, stringsAsFactors = FALSE)
}

#' @keywords internal
compute_durations <- function(parsed, times) {
  n <- nrow(times)
  # we need both the start of the day
  #  and the start of the next day
  #  to evaulate nighttime.
  start_day <- as.POSIXct(
    paste0(
      times$date,
      " 00:00:00"
  ),
  tz = attr(times[[parsed$variable[1]]], "tzone")
  )
  end_day <- start_day + 86400  # next midnight
  
  intervals_list <- vector("list", nrow(parsed))
  
  for (i in seq_len(nrow(parsed))) {
    var <- parsed$variable[i]
    op  <- parsed$operator[i]
    
    if (op %in% c("<", "<=")) {
      intervals_list[[i]] <- cbind(
        start = start_day,
        end = times[[var]]
      )
    } else if (op %in% c(">", ">=")) {
      intervals_list[[i]] <- cbind(
        start = times[[var]],
        end = end_day
      )
    }
  }
  
  # Combine intervals based on join operators
  # Start with first interval
  intervals <- intervals_list[[1]]
  
  for (i in 2:nrow(parsed)) {
    join <- parsed$join[i - 1]
    next_interval <- intervals_list[[i]]
    
    if (join == "|") {
      # union of intervals -> sum individual durations (overlaps not corrected for!)
      intervals <- rbind(
        intervals,
        next_interval
      )
    } else if (join == "&") {
      # intersection of intervals -> use overlap only
      intervals <- cbind(
        start = pmax(
          intervals[, "start"],
          next_interval[, "start"]
        ),
        end = pmin(
          intervals[, "end"],
          next_interval[, "end"]
        )
      )
    }
  }
  
  # Duration of intervals (fix negative durations to 0)
  durations <- pmax(
    as.numeric(
      intervals[, "end"] - intervals[, "start"],
      units = "secs"
    ),
    0
  )
  
  # Sum total durations per row, this accomodates 
  #  night as it fills columnwise, then we sum
  #  across rows to get the total number of seconds
  #  in that diel period. For the other time periods
  #  it would just be 1 column, so rowSums just
  #  converts the matrix to a vector.
  rowSums(matrix(durations, nrow = n, byrow = FALSE))
}

#' @keywords internal
get_diel_proportions <- function(bin.type.list, times) {
  total_day <- 86400
  
  # Helper to parse and compute duration for one period
  compute_one <- function(expr) {
    parsed <- parse_conditions(expr)
    compute_durations(parsed, times) / total_day
  }
  
  provided_bins <- names(bin.type.list)
  
  # Enforce that user provides either 2 or 3 bins
  if (!(length(provided_bins) %in% c(2, 3))) {
    stop("You must provide either 2 bins (e.g., 'day' and 'night') or 3 bins (e.g., 'day', 'night', and 'dawn' or 'dusk').")
  }
  
  # Enforce that 'day' and 'night' must always be provided
  if (!all(c("day", "night") %in% provided_bins)) {
    stop("Both 'day' and 'night' bins must be provided.")
  }
  
  # Determine which bin to infer
  missing_bin <- NULL
  if (length(provided_bins) == 2) {
    missing_bin <- "twilight"
  } else if (length(provided_bins) == 3) {
    if (!("dawn" %in% provided_bins) && "dusk" %in% provided_bins) {
      missing_bin <- "dawn"
    } else if ("dawn" %in% provided_bins && !("dusk" %in% provided_bins)) {
      missing_bin <- "dusk"
    } else {
      stop("When providing 3 bins, you must provide either 'dawn' or 'dusk', but not both.")
    }
  }
  
  # Compute proportions for the provided bins
  result <- sapply(
    provided_bins,
    function(period) compute_one(bin.type.list[[period]])
  )
  
  # Infer the missing bin
  residual <- 1 - rowSums(as.data.frame(result))
  residual <- pmax(residual, 0)  # prevent tiny negative values due to floating point arithmetic
  result <- cbind(result, residual)
  colnames(result)[ncol(result)] <- missing_bin
  
  as.data.frame(result)
}
