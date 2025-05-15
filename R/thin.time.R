#' Thin detections based on a time threshold
#'
#' Filters detection records so that observations within a specified number of minutes
#' from the previous observation (at the same site) are removed. For multi-species
#' datasets, records should be thinned by site and species detected at that site.
#' This function can be used to thin, for example, camera trap or acoustic data where multiple
#' detections in rapid succession may not be independent.
#'
#' @param data A `data.frame` containing detection data. Must include a datetime column,
#'   a site identifier column, and optionally a species column.
#' @param datetime.column A character string giving the name of the column in `data` containing
#'   datetime information. The datetime column must be of class `POSIXct` or `POSIXt`.
#' @param site.column A character string giving the name of the column in `data` identifying the site
#'   where each detection occurred. 
#' @param species.column Optional. A string giving the name of the column in `data`
#'   identifying the species detected. If `NULL` (default), all records within a site are
#'   assumed to be of the same species. Thus, if the dataset contains multiple species you
#'   should use this argument.
#' @param minutes.between A positive number indicating the minimum number of minutes
#'   that must pass between detections at a site (and species, if specified) for both to be retained.
#'   Defaults to 15.
#' @param progress.bar Logical; if `TRUE`, displays a progress bar during processing.
#'   Defaults to `TRUE`.
#'
#' @return A `data.frame` containing the same columns as the input `data`, but with records
#'   removed so that no two retained records within the same site (and species, if specified)
#'   are within `minutes.between` minutes of each other.
#'
#' @details
#' This function first splits the data by site (and species, if provided), then iteratively removes
#' records that occur within `minutes.between` minutes of a previous detection. It handles
#' sequences of closely spaced detections intelligently to retain the earliest and ensure spacing.
#' 
#'
#' @examples
#' \dontrun{
#' # Example with datetime in POSIXct format
#' dat <- data.frame(
#'   site = rep("A", 5),
#'   species = rep("red fox", 5),
#'   datetime = as.POSIXct(c(
#'     "2023-01-01 00:00:00",
#'     "2023-01-01 00:05:00",
#'     "2023-01-01 00:20:00",
#'     "2023-01-01 00:21:00",
#'     "2023-01-01 00:40:00"
#'   ))
#' )
#' # Species column not needed as this dataset
#' #  has one species
#' response <- thin.time(
#'   dat,
#'   datetime.column = "datetime",
#'   site.column = "site",
#'   minutes.between = 15
#' )
#'           
#'   dat <- data.frame(
#'   site = rep("A", 10),
#'   species = rep(c("red fox", "coyote"), each = 5),
#'   datetime = as.POSIXct(c(
#'     "2023-01-01 00:00:00",
#'     "2023-01-01 00:05:00",
#'     "2023-01-01 00:20:00",
#'     "2023-01-01 00:21:00",
#'     "2023-01-01 00:40:00",
#'     "2023-01-01 00:00:00",
#'     "2023-01-01 00:05:00",
#'     "2023-01-01 00:20:00",
#'     "2023-01-01 00:21:00",
#'     "2023-01-01 00:40:00"
#'   ))
#' )
#' # Species solumn needed as this dataset
#' #  has more than one species.
#' response <- thin.time(
#'   dat,
#'   datetime.column = "datetime",
#'   species.column = "species",
#'   site.column = "site",
#'   minutes.between = 15
#' )
#' 
#' }
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export

thin.time <- function(data, datetime.column, site.column, species.column = NULL, minutes.between = 15,
                      progress.bar = TRUE){
  ## input checks
  if(!inherits(data, "data.frame")){
    stop("data must be a data.frame")
  }
  
  # add checks here
  if (!datetime.column %in% colnames(data)) {
    stop("datetime.column not found in data.")
  }
  if (!site.column %in% colnames(data)) {
    stop("site.column not found in data.")
  }
  if (!is.null(species.column) && !species.column %in% colnames(data)) {
    stop("species.column was provided but was not found in data.")
  }
  if (!inherits(data[[datetime.column]], "POSIXt")) {
    stop("datetime.column must be a POSIXct or POSIXt object.")
  }
  if (!is.numeric(minutes.between) || length(minutes.between) != 1 || minutes.between <= 0) {
    stop("`minutes.between` must be a single positive number.")
  }
  if(!inherits(progress.bar, "logical")){
    stop("progress.bar must be either TRUE or FALSE")
  }
  if(any(is.na(data[[datetime.column]]))){
    data <- data[-which(is.na(data[[datetime.column]])),]
  }
  # error if there is no data
  if(nrow(data) == 0){
    stop(
      "All date/times are NA."
    )
  }
  # make site column a factor for splitting
  if(!inherits(data[[site.column]], "factor")){
    data[[site.column]] <- factor(
      data[[site.column]]
    )
  }
  # do the split
  tmp <- split(
    data,
    data[[site.column]]
  )
  # split species within site
  if(!is.null(species.column)){
    for(j in 1:length(tmp)){
      tmp[[j]] <- split(
        tmp[[j]],
        factor(
          tmp[[j]][[species.column]],
          levels = unique(tmp[[j]][[species.column]])
        )
      )
    } 
  } else {
    for(j in 1:length(tmp)){
      tmp[[j]] <- list(tmp[[j]])
    }
  }
  if(progress.bar){
    pb <- txtProgressBar(max = length(tmp))
  }
  for(k in 1:length(tmp)){
    if(progress.bar){
      setTxtProgressBar(pb, k)
    }
    for(j in 1:length(tmp[[k]])){
      # one site and species
      oness <- tmp[[k]][[j]]
      # order by date
      tmp_data <- oness[order(oness[[datetime.column]]),]
      # ID for the rows
      tmp_data$id_rows <- seq_len(
        nrow(
          tmp_data
        )
      )
      # time between images
      tmp_data$delta <- c(
        minutes.between + 1, # keeping the first no matter what
        as.numeric(
          diff(
            tmp_data[[datetime.column]]
          ),
          units = "mins")
      )
      # first remove all zeroes as these are just duplicate images
      if(any(tmp_data$delta == 0)){
        tmp_data <- tmp_data[-which(tmp_data$delta == 0),]
        tmp_data$delta <- c(
          minutes.between + 1, # keeping the first no matter what
          as.numeric(
            diff(
              tmp_data[[datetime.column]]
            ),
            units = "mins"
          )
        )  
      }
      if(min(tmp_data$delta, na.rm = TRUE) < minutes.between){
        # calc cumulative sum and then the difference
        #  to see if there are any really close images
        #  in a sequence
        cs_delta <- cumsum(tmp_data$delta)
        diff_delta <- c(
          minutes.between + 1,
          diff(cs_delta)
        )
        # those that are less than mn_bt need to be evaluated
        #  to determine if they are in a sequence
        to_check <- which(diff_delta < minutes.between)
        # now we need to find sequences in here. If the difference
        #  is 1
        my_sequences <- c(minutes.between+1, diff(to_check)) == 1
        # bin the TRUE and FALSE statements into groups
        my_rle <- rle(my_sequences)
        # vector to determine which in to_check need to go
        to_go <- rep(FALSE, length(my_sequences))
        # for each batch of TRUE FALSE
        for(m in 1:length(my_rle$lengths)){
          # match batch start to where we need to begin
          #  in to_go
          if(m == 1){
            start_loc <- 1
          } else {
            start_loc <- sum(
              sum(my_rle$lengths[m-1])+1
            )
          }
          # if FALSE, then they just get removed because there
          #  is no sequence of values
          if(!my_rle$values[m]){
            to_go[start_loc:sum(my_rle$lengths[1:m])] <- TRUE
          }else{
            # Otherwise check if the sequence adds to over mn_bt
            #  get the sequence location and then grab the actual data
            #  and calculate the cumulative sum
            tmp_seq_loc <- to_check[
              start_loc:sum(my_rle$lengths[1:m])
            ]
            tmp_seq <- tmp_data$delta[tmp_seq_loc]
            tmp_cs <- cumsum(tmp_seq)
            # if there is anything greater than mn_bt
            #  then do not remove any of those.
            if(max(tmp_cs) > minutes.between){
              new_loc <- start_loc:sum(my_rle$lengths[1:m])
              new_loc <- new_loc[which(tmp_cs < minutes.between)]
              to_go[new_loc] <- TRUE
            }else{
              # if all less than mn_bt, then they all go
              to_go[start_loc:sum(my_rle$lengths[1:m])] <- TRUE
            }
          }
        }
        # remove the data
        tmp_data <- tmp_data[-to_check[to_go],]
        # recalculate the lagged time again
        tmp_data$delta <- c(
          minutes.between + 1,
          as.numeric(
            diff(
              tmp_data[[datetime.column]]
            ),
            units = "mins")
        )
      }
      # one last check to determine if there are any stragglers,
      #  if so remove them iteratively 
      while (min(tmp_data$delta, na.rm = T) < minutes.between) {
        
        rm_rows <- tmp_data[tmp_data$delta < minutes.between, ]
        min_dt  <- min(rm_rows[[datetime.column]], na.rm = TRUE)
        rm_rows <- rm_rows[rm_rows[[datetime.column]] == min_dt,]
        rm_rows <- rm_rows$id_rows
        tmp_data <- tmp_data[-which(tmp_data$id_rows %in% rm_rows),]
        tmp_data$delta <- c(
          minutes.between + 1,
          as.numeric(
            diff(
              tmp_data[[datetime.column]]
            ),
            units = "mins")
        )
      }
      tmp[[k]][[j]] <- tmp_data[,-which(colnames(tmp_data) %in% c("delta", "id_rows"))]
    }
    tmp[[k]] <- do.call("rbind", tmp[[k]])
  }
  to_return <- do.call("rbind", tmp)
  row.names(to_return) <- NULL
  return(to_return)
}



