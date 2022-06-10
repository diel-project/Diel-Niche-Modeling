#' Simulate 
#'
#' Simulate diel data
#' @param n.sim The number of simulated datasets
#' @param n.sample The number of total samples for a given simulation
#' @param hyp The hypothesis to simulate data from.
#' @param diel.setup Multinomial inequalities for hypotheses setup using function 'diel.ineq'.

#' @return A list of outputs
#' \item{y}{Matrix of simulated datasets}
#' \item{p}{Probabilities used to simulate the data}  
#' @examples 
#' sim.diel(1,100,"D.th")
#'                     
#' Required libraries:   stats
#' @importFrom stats rmultinom
#' @export

sim.diel<- function(n.sim,n.sample,hyp,
                    diel.setup = diel.ineq()){
  
  #Find appropriate probabilities of hyp using inequalities in diel.setup
  prob.hyp=find.prob.hyp(hyp,diel.setup)
  
  #Randomly select a single probability
  prob.select=prob.hyp[sample(nrow(prob.hyp),1),]
  
  #simulate n.sim datesets of n.sample size
  y=t(rmultinom(n.sim,size=n.sample,prob=prob.select))
  #return sim data and probability
  list(y=y,p=prob.select)
}
