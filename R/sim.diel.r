#' Simulate 
#'
#' Simulate diel data
#' @param n.sim The number of simulated datasets
#' @param reps The number of replicates of crepuscular, daytime, and nocturnal for each simulated dataset
#' @param n.sample The number of total samples for a given simulation
#' @param hyp The hypothesis to simulate data from
#' @param diel.setup Multinomial inequalities for hypotheses setup using function 'diel.ineq'.
#' @return A list of outputs
#' \item{y}{Matrix of simulated datasets}
#' \item{p}{Probabilities used to simulate the data}  
#' @importFrom stats rmultinom
#' @examples 
#' sim.diel(n.sim=1,reps=1,n.sample=100,hyp="D.th")
#' @export

sim.diel<- function(n.sim=1,reps=1,n.sample=100,hyp,diel.setup=NULL){
  #if diel.setup not provided use defaults
  if(is.null(diel.setup)){diel.setup=diel.ineq()}
  
  #Find appropriate probabilities of hyp using inequalities in diel.setup
  prob.hyp=find.prob.hyp(hyp,diel.setup)
  
  #Randomly select  probabilities- reps times
  prob.select=matrix(prob.hyp[sample(nrow(prob.hyp),reps),],nrow=reps,ncol=3)
  
  #simulate n.sim datesets of n.sample size
  y=c(apply(prob.select,1,
        FUN=function(x){t(rmultinom(n.sim,size=n.sample,prob=x))}))
  #names(y)=paste(rep(c("p_crep","p_day","p_night"), times = reps), rep(1:(reps),each=3), sep = "_")
  
  y=matrix(y,ncol=3,byrow=TRUE)
  colnames(y)=c("p_crep","p_day","p_night")
  
  #y=t(rmultinom(n.sim,size=n.sample,prob=prob.select))
  #return sim data and probability
  list(y=y,p=prob.select)
}
