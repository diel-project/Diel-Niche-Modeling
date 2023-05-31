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
