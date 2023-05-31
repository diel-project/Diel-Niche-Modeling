#' Kernel Overlap density integration
#'
#' Integrate kernel density to derieve probability of twilight, daytime, nighttime
#' @param densityplot a densityPlot object from package Overlap.
#' @param dawn begining and end numeric (0-24) times for dawn
#' @param dusk begining and end numeric (0-24) times for dusk
#' @export
#' @import sfsmisc

prob.Overlap=function(densityplot,
                      dawn=c(6,7),
                      dusk=c(17,18)){

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
