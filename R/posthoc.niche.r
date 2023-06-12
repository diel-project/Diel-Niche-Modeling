#' Posthoc diel niche
#'
#' Diel model hypotheses based on posthoc probabilitiy values
#' For each input of twilight, daytime, and nightime probabilities, the function will return the defined diel hyppthesis
#' @param y a matrix of probabilities of twilight, daytime, and nightime (three columns). Each row is a replicate.
#' @param hyp Vector of diel hypotheses names representing hypotheses set or individual hypotheses.
#' @param diel.setup A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'. If not provided, it will use the defaults of the diel.ineq function.
#' @return data.frame of the probabilities of activity during twilight, daytime, nighttime, and matched hypothesis from the hypothesis set provided.
#' @export
#' 
#' @examples
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
#'   keep = c("dusk", "night", "dawn","nightEnd"),
#'   tz = "Asia/Jakarta"
#' )
#' 
#' # strip the month / year from these times
#' my_times$dusk <- lubridate::hms(
#'   format(
#'     my_times$dusk,
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
#' my_times$dawn <- lubridate::hms(
#'   format(
#'     my_times$dawn,
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
#' my_dawn <- c(my_times$nightEnd, my_times$dawn)
#' 
#' my_dusk <- c(my_times$dusk, my_times$night)
#' 
#' # convert these to fractional hours, given that there
#' #  are 86400 seconds in a day.
#' my_dawn <- (as.numeric(my_dawn) / 86400) * 24
#' my_dusk <- (as.numeric(my_dusk) / 86400) * 24
#' 
#' ### Step 3. Combining the kernel density estimate with overlap.
#' 
#' diel_probs <- Diel.Niche::prob.Overlap(
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
#' #1 0.09626836 0.6007204 0.3027826 Cathemeral Traditional
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
  

tol = 0.01 
if(!all(apply(y,1,sum) >= 1-tol)){
  stop("all rows need to sum to 1 \n")
}  

###################################
#  y=matrix(y,ncol=3)


  index.models=match(hyp,names(diel.setup))
  

  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points=FALSE)))
  
  points=matrix(as.numeric(as.matrix(plot.points[,1:3])),ncol=3)
  
  index=apply(y,1,FUN=function(x){which.min(apply(abs(sweep(points,2,x)),1,sum))})
  
  #output from function
  dat.out<-data.frame(y,plot.points[index,4])
  colnames(dat.out)<-c("p.twi","p.day",'p.night',"Hypothesis")
  dat.out
}#End function  
