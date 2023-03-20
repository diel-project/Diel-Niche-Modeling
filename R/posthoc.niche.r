#' Posthoc diel niche
#'
#' Diel model hypotheses based on posthoc probabilitiy values
#' For each input of twilight, daytime, and nightime probabilities, the function will return the defined diel hyppthesis
#' @param y a matrix of probabilities of twilight, daytime, and nightime (three columns). Each row is a replicate.
#' @param hyp Vector of diel hypotheses names representing hypotheses set or individual hypotheses.
#' @param diel.setup A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'. If not provided, it will use the defaults of the diel.ineq function.
#' @return Internal list
#' @export
#' @keywords internal

posthoc.niche=function(y,
                       hyp,
                       diel.setup=NULL){
  

#Checks
if(!is.matrix(y)){
  stop("y needs to be a matrix  \n")
}
  
if(ncol(y)!=3){
  stop("y needs to have three columns \n")
}
tol = 1e-5  
if(!all(apply(y,1,sum) >= 1-tol)){
  stop("all rows need to sum to 1 \n")
}  


if(is.null(diel.setup)){diel.setup=diel.ineq()}

  
  y=matrix(y,ncol=3)


  index.models=match(hyp,names(diel.setup))
  plot.points=data.frame(do.call(rbind,setup.hyp.plot.params(diel.setup,index.models,more.points=FALSE)))
  
  points=matrix(as.numeric(as.matrix(plot.points[,1:3])),ncol=3)
  
  index=apply(y,1,FUN=function(x){which.min(apply(abs(sweep(points,2,x)),1,sum))})
  
  #output from function
  dat.out<-data.frame(y,plot.points[index,4])
  colnames(dat.out)<-c("p.twi","p.day",'p.night',"Hypothesis")
  dat.out
}#End function  