#' Posthoc diel niche
#'
#' @description Diel model hypotheses based on posthoc probabilitiy values
#' For each input of twilight, daytime, and nightime probabilities, the function will return the defined diel hyppthesis
#' @param y a matrix of probabilities of twilight, daytime, and nightime (three columns). Each row is a replicate.
#' @param hyp Vector of diel hypotheses names representing hypotheses set or individual hypotheses.
#' @param diel.setup A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'. If not provided, it will use the defaults of the diel.ineq function.
#' @return data.frame of the probabilities of activity during twilight, daytime, nighttime, and matched hypothesis from the hypothesis set provided.
#' @details
#' 
#' This function only uses the diel inequalities to provide a post-hoc estimate
#' of which diel phenotype is associated to the data for a given species. As such,
#' the output from this function is only a point estimate, and there is no 
#' uncertainty associated to this estimate. If you are interested in the 
#' uncertainty estimate (which we suggest you should be), there are two things
#' you could do. First, if you have the data in hand use \code{\link{diel.fit}}
#' instead, and select the appropriate hypothesis set via \code{\link{hyp.sets}}.
#' Second, if you want to use the output from \code{overlap}, then you could also
#' bootstrap the kernel density estimate and use \code{\link{prob.overlap}} in
#' conjunction with \code{posthoc.niche} to get a distribution of classificaitons.
#' Following this, you can calculate the proportion of times different diel phenotypes
#' were selected.
#' 
#' Furthermore, in order for \code{posthoc.niche} to work, we integrate under the
#' kernel density estimate. Therefore, it is very important to increase the 
#' \code{n.grid} argument in \code{overlap::densityPlot} to a sufficient number (
#' e.g., over 10,000). 
#' 
#' 
#' @export
#' 
#' 
#' 
#' @examples
#' 
#' #'  data("posthoc.example")
#'  
#'  diel_probs <- prob.overlap(
#'    posthoc.example$tiger.kde,
#'    dawn = posthoc.example$dawn.range,
#'    dusk = posthoc.example$dusk.range
#'  )
#'  
#'  tiger_niche <- Diel.Niche::posthoc.niche(
#'   y = diel_probs,
#'   hyp = hyp.sets("Traditional")
#'  )
#' 
#' # look at output. 
#' tiger_niche
#' 
#' #       p.twi     p.day   p.night             Hypothesis
#' #1  0.1409727 0.5557304 0.3027171 Cathemeral Traditional
#' 
#' \dontrun{
#' 
#' # NOTE:  For this example we have to make some assumptions
#' #  because the data in overlap we used do not have 1) spatial coordinates,
#' #  2) the datetime information. Likewise, we also show here how to
#' #  calculate the start of dawn and dusk using suncalc and lubridate.
#' 
#' # Load libraries
#' library(overlap)
#' library(suncalc)
#' library(lubridate)
#' library(Diel.Niche)
#' 
#' ### Step 1. Get Kernel Density Estimate
#' 
#' # load data
#' data("kerinci")
#' 
#' # subset to a single species
#' tiger <- kerinci[kerinci$Sps == "tiger",]
#' 
#' # convert to Radians
#' tiger$Rad <- tiger$Time * 2 * pi
#' 
#' # get kde, you need to increase the number
#' #  of grid points in order to use
#' #  Diel.Niche::posthoc.niche()
#' tiger_kde <- overlap::densityPlot(
#'   tiger$Rad,
#'   extend = NULL,
#'   n.grid = 25000
#' )
#' 
#' ### Step 2. Get ranges for dawn and dusk
#' 
#' # Because we don't know the date the cameras were deployed
#' #  we are just going to choose a single date. Since this
#' #  location is close to the equator there should not
#' #  be much temporal variation anyways.
#' 
#' my_times <- suncalc::getSunlightTimes(
#'   date = as.Date("2008-03-12"),
#'   lat = -2.41,
#'   lon =  101.4836,
#'   keep = c("sunset", "night", "sunrise","nightEnd"),
#'   tz = "Asia/Jakarta"
#' )
#' 
#' # strip the month / year from these times
#' my_times$sunset <- lubridate::hms(
#'   format(
#'     my_times$sunset,
#'     "%H:%M:%S"
#'   )
#' )
#' 
#' my_times$night <- lubridate::hms(
#'   format(
#'     my_times$night,
#'     "%H:%M:%S"
#'   )
#' )
#' my_times$sunrise <- lubridate::hms(
#'   format(
#'     my_times$sunrise,
#'     "%H:%M:%S"
#'   )
#' )
#' my_times$nightEnd <- lubridate::hms(
#'   format(
#'     my_times$nightEnd,
#'     "%H:%M:%S"
#'   )
#' )
#' 
#' # calculate the time when dawn & dusk starts and stops
#' my_dawn <- c(my_times$nightEnd, my_times$sunrise)
#' 
#' my_dusk <- c(my_times$sunset, my_times$night)
#' 
#' # convert these to fractional hours, given that there
#' #  are 86400 seconds in a day.
#' my_dawn <- (as.numeric(my_dawn) / 86400) * 24
#' my_dusk <- (as.numeric(my_dusk) / 86400) * 24
#' 
#' ### Step 3. Combining the kernel density estimate with overlap.
#' 
#' diel_probs <- Diel.Niche::prob.overlap(
#'   densityplot = tiger_kde,
#'   dawn = my_dawn,
#'   dusk = my_dusk
#' )
#' 
#' ### Step 4. Classify diel niche posthoc
#' 
#' tiger_niche <- Diel.Niche::posthoc.niche(
#'   y = diel_probs,
#'   hyp = hyp.sets("Traditional")
#' )
#' 
#' # look at output. 
#' tiger_niche
#' 
#' #       p.twi     p.day   p.night             Hypothesis
#' #1  0.1409727 0.5557304 0.3027171 Cathemeral Traditional
#' }
#' 
posthoc.niche=function(y,
                       hyp,
                       diel.setup=NULL){
  
###################################
#Checks
  
#if y is a vector and not a matrix and has 3 elements, then turn into matrix
  if(length(y)==3 & is.matrix(y)==FALSE){
    warning("y was chanaged to a matrix")
    y=t(as.matrix(y))
  }
  
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
  
  check.inputs(y=y,hyp.set=hyp,bf.fit=FALSE,prior=NULL,diel.setup=diel.setup,post.fit=FALSE,n.chains=1,
               n.mcmc=1,burnin=1,prints=FALSE,alt.optim=FALSE,delta=0)  
  

# Allowable tolerance
  tol = 0.01 
  if(!all(apply(y,1,sum) >= 1-tol)){
    stop("all rows need to sum to 1 \n")
  }  


  index.models=match(hyp,names(diel.setup))
  

  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points=FALSE)))
  
  points=matrix(as.numeric(as.matrix(plot.points[,1:3])),ncol=3)
  
  index=apply(y,1,FUN=function(x){which.min(apply(abs(sweep(points,2,x)),1,sum))})
  
  #output from function
  dat.out<-data.frame(y,plot.points[index,4])
  colnames(dat.out)<-c("p.twi","p.day",'p.night',"Hypothesis")
  dat.out
}#End function  
