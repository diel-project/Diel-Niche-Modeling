#' Kernel Overlap density integration
#'
#' @description Integrate kernel density to derive probability of twilight, daytime, nighttime
#' @param densityplot a densityPlot object from package overlap. See details for
#' additional information.
#' @param dawn beginning and end numeric (0-24) times for dawn. This is in
#' proportional hours such that 12.5 would be 12:30. See details for additional
#' information.
#' @param dusk beginning and end numeric (0-24) times for dusk. This is in
#' proportional hours such that 12.5 would be 12:30. See details for additional
#' information.
#' @return A matrix of three probabilities.
#' @details
#' When creating the density plot, it is important to increase the \code{n.grid}
#' argument, because to this function integrates the area under the curve to
#' compute the associated probabilities. We suggest to start setting \code{n.grid}
#'  in \code{overlap::densityPlot} to at least 10000, but you may be able to do
#'  less. Essentially, if the outputted sum is within about a thousandth from one
#'  (e.g., 0.999) then the output from this function can be used in 
#'  \code{\link{posthoc.niche}}.
#'  
#'  To compute proportional hours, you will need to start with a time object.
#'  Changing that time to a numeric should convert it to
#'  seconds from midnight. Given that there are 86,400 seconds in a day, divide
#'  by that number and multiply by 24 to create the proportional hours. See examples
#'  for some code on how to do this.
#'  
#'  @examples
#'  \dontrun{
#'  
#'  # NOTE: We have no clue really when the tiger data was collected
#'  #   so we chose a time around when the original overlap paper
#'  #   said the data was collected. With your own camera trap data
#'  #   this is something that you should know.
#'  
#'  # load packages
#'  library(suncalc)
#'  library(lubridate)
#'  library(overlap)
#'  
#'  ### Step 1. Get Kernel Density Estimate
#'  
#'  # load data
#'  data("kerinci")
#'  
#'  # subset to a single species
#'  tiger <- kerinci[kerinci$Sps == "tiger",]
#'  
#'  # convert to Radians
#'  tiger$Rad <- tiger$Time * 2 * pi
#'  
#'  # get kde, you need to increase the number
#'  #  of grid points in order to use
#'  #  Diel.Niche::posthoc.niche()
#'  tiger_kde <- overlap::densityPlot(
#'    tiger$Rad,
#'    extend = NULL,
#'    n.grid = 10000
#'  )
#'  
#'  # Step 2. Calculate start and end of dawn / dusk
#'  
#'  my_times <- suncalc::getSunlightTimes(
#'    date = as.Date("2008-03-12"),
#'    lat = -2.41,
#'    lon =  101.4836,
#'    keep = c("dusk", "night", "dawn","nightEnd"),
#'    tz = "Asia/Jakarta"
#'  )
#'  
#'  my_times$dusk <- lubridate::hms(
#'    format(
#'      my_times$dusk,
#'      "%H:%M:%S"
#'    )
#'  )
#'  
#'  my_times$night <- lubridate::hms(
#'    format(
#'      my_times$night,
#'      "%H:%M:%S"
#'    )
#'  )
#'  my_times$dawn <- lubridate::hms(
#'    format(
#'      my_times$dawn,
#'      "%H:%M:%S"
#'    )
#'  )
#'  my_times$nightEnd <- lubridate::hms(
#'    format(
#'      my_times$nightEnd,
#'      "%H:%M:%S"
#'    )
#'  )
#'  
#'  # calculate the time when dawn & dusk starts and stops
#'  my_dawn <- c(my_times$nightEnd, my_times$dawn)
#'  
#'  my_dusk <- c(my_times$dusk, my_times$night)
#'  
#'  # convert these to fractional hours, given that there
#'  #  are 86400 seconds in a day.
#'  my_dawn <- (as.numeric(my_dawn) / 86400) * 24
#'  my_dusk <- (as.numeric(my_dusk) / 86400) * 24
#'  
#'  ### Step 3. Combining the kernel density estimate with overlap.
#'  
#'  diel_probs <- Diel.Niche::prob.overlap(
#'    densityplot = tiger_kde,
#'    dawn = my_dawn,
#'    dusk = my_dusk
#'  )
#'  }
#'  
#'  
#'  
#' 
#' @export
#' @import sfsmisc

prob.overlap=function(densityplot,
                      dawn=c(6,7),
                      dusk=c(17,18)){

#########################
#Checks

  if(!is.data.frame(densityplot)  | ncol(densityplot)!=2 | any(colnames(densityplot)!=c("x","y"))| !is.numeric(densityplot$x) |  !is.numeric(densityplot$y)){
   stop("densityplot needs to be a dataframe with numeric values organized into two columns labled 'x' and 'y'") 
  }

  if(length(dawn)!=2 | length(dusk)!=2 | !is.numeric(dawn)| !is.numeric(dusk)){
   stop("dawn/dusk need to be vectors of length 2 with only numeric values")  
  }
    
#########################
  densityplot=densityplot[which(densityplot$x>=0 & densityplot$x<=24),]
  
  
  index.dawn=which(densityplot$x >= dawn[1] & densityplot$x<=dawn[2])
  index.dusk=which(densityplot$x>=dusk[1] & densityplot$x<=dusk[2])
  index.day=which(densityplot$x>dawn[2] & densityplot$x<dusk[1])
  index.night1=which(densityplot$x<dawn[1])
  index.night2=which(densityplot$x>dusk[2])
 
  if(length(index.dawn)==0){p.dawn=0}else{p.dawn=integrate.xy(densityplot$x[index.dawn],densityplot$y[index.dawn])}
  
  if(length(index.dusk)==0){p.dusk=0}else{p.dusk=integrate.xy(densityplot$x[index.dusk],densityplot$y[index.dusk])}
  
  if(length(index.day)==0){p.day=0}else{p.day=integrate.xy(densityplot$x[index.day],densityplot$y[index.day])}
  
  if(length(index.night1)==0){p.night1=0}else{p.night1=integrate.xy(densityplot$x[index.night1],densityplot$y[index.night1])}
  if(length(index.night2)==0){p.night2=0}else{p.night2=integrate.xy(densityplot$x[index.night2],densityplot$y[index.night2])}
  
  twi=p.dawn+p.dusk
  daytime=p.day
  nighttime=p.night1+p.night2
  
  y=t(matrix(c(twi,daytime,nighttime)))
  colnames(y)<-c("p.twi","p.day","p.night")
  
  y
  
}#End function
