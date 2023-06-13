#' Simulate 
#'
#' Simulate diel data
#' @param n.sim The number of simulated datasets (integer)
#' @param reps The number of sets (integer) of probabilities to use when simulating crepuscular, daytime, and nocturnal frequencies
#' @param n.sample The number of total samples (integer) for a given simulation
#' @param hyp The hypothesis code to simulate data from
#' @param diel.setup Multinomial inequalities for hypotheses setup using function 'diel.ineq'.
#' @param sd.error Normal distribution standard deviation to simulate error to add to the probabilities on the logit-scale. Default is 0
#' @param fast Default is TRUE, which uses a less precise probability interval sequence (0.005 vs 0.001). Does not apply to equality hyps.
#' @param return.probs Default is FALSE. If TRUE, returns probabilities from the hypothesis.
#' @return A list of outputs
#' \item{y}{Matrix of simulated datasets}
#' \item{p}{Probabilities used to simulate the data}  
#' @importFrom stats rmultinom
#' @examples 
#' sim.diel(n.sim=1,reps=1,n.sample=100,hyp="D.th")
#' @export

#n.sim=1;reps=1;n.sample=100;diel.setup=NULL;sd.error=0; fast=TRUE
sim.diel<- function(n.sim=1,reps=1,n.sample=100,hyp,diel.setup=NULL,sd.error=0,fast=TRUE,return.probs=FALSE){
 
################################################   
#if diel.setup not provided use defaults
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
################################################   

  
# Checks  
  if(sd.error<0| !is.numeric(sd.error)){
    stop("sd.error need to be numeric and greater than or equal to zero.")
  }

  if(!is.numeric(n.sim) | !is.numeric(reps) | !is.numeric(n.sample)){
    stop("n.sim, reps, and n.sample all need to be numeric.")
  }

  if(n.sim%%1!=0 | reps%%1!=0 | n.sample%%1!=0){
    stop("n.sim, reps, and n.sample all need to be integers.")
  }
  if(n.sim<1 | reps<1 | n.sample<1){
    stop("n.sim, reps, and n.sample must all be at least 1.")
  }

#Chek hyp  
  check.inputs(y=cbind(0,0,0),hyp.set=hyp,bf.fit=FALSE,prior=NULL,diel.setup=diel.setup,post.fit=FALSE,n.chains=1,
               n.mcmc=1,burnin=1,prints=FALSE,alt.optim=FALSE,delta=0)  

  
################################################
    
  
  #Find appropriate probabilities of hyp using inequalities in diel.setup
  prob.hyp=find.prob.hyp(hyp,diel.setup,fast)

if(return.probs==FALSE){    
  #Randomly select  probabilities- reps times
  prob.select=matrix(prob.hyp[sample(nrow(prob.hyp),reps),],nrow=reps,ncol=3)
  
  #add in normal distribution error on logit scale and backtransform
  prob.select.with.error=stats::plogis(stats::qlogis(prob.select)+matrix(stats::rnorm(length(prob.select),0,sd.error),ncol=3))
  
  #simulate n.sim datesets of n.sample size
  y=c(apply(prob.select.with.error,1,
        FUN=function(x){t(rmultinom(n.sim,size=n.sample,prob=x))}))
  #names(y)=paste(rep(c("p_crep","p_day","p_night"), times = reps), rep(1:(reps),each=3), sep = "_")
  
  y=matrix(y,ncol=3,byrow=TRUE)
  colnames(y)=c("y_crep","y_day","y_night")
  
  #return sim data and probability
  list(y=y,p=prob.select,p.error=prob.select.with.error,sd.error=sd.error)
}else{
  prob.hyp
}  
  
} #End Function
