#' Hypothesis Sets
#'
#' Call defined hypotheses sets
#' @param hyp.in, Hypothesis set code names
#' @return Names of hypotheses for the set. If NULL, the names of all hypotheses sets are returned.
#' @examples 
#' hyp.sets()
#' hyp.sets("hyp.th")
#' @export

# A function to simply call certain pre-defined hypotheses sets
# Each name here is defined in the Diel.Inequalities.r function where the inequality constraints
# are defined and named by hypothesis. 

#Need to create a call to provide a description of each hypothesis.

hyp.sets=function(hyp.in=NULL){

  #NEED TO INCLUDE C.th somewhere
  hyp.set=vector("list",9)
  hyp.set[[1]]=c("D.th","N.th","CR.th","C.th")     #General Hypotheses
  hyp.set[[2]]=c("D.max","N.max","CR.max")          #General Hypotheses
  hyp.set[[3]]=c("D.var","N.var","CR.var","EC.var") #General Hypotheses
  hyp.set[[4]]=c("Dn.th","Dcr.th","Nd.th","Ncr.th","CRd.th","CRn.th")          #Primary-secondary Hypotheses
  hyp.set[[5]]=c("Dn.max","Dcr.max","Nd.max","Ncr.max","CRd.max","CRn.max") #Primary-secondary Hypotheses
  hyp.set[[6]]=c("Dn.var","Dcr.var","Nd.var","Ncr.var","CRd.var","CRn.var",) #Primary-secondary Hypotheses
  hyp.set[[7]]=c(hyp.set[[1]],hyp.set[[4]]) #General & Primary-secondary Hypotheses
  hyp.set[[8]]=c(hyp.set[[2]],hyp.set[[5]]) #General & Primary-secondary Hypotheses
  hyp.set[[9]]=c(hyp.set[[4]],hyp.set[[6]]) #General & Primary-secondary Hypotheses
  hyp.set[[10]]=c(hyp.set[[1]],hyp.set[[2]],hyp.set[[3]],hyp.set[[4]],hyp.set[[5]],hyp.set[[6]]) #All hypotheses; used for testing

#name the list hyp.set
  names(hyp.set)=c("hyp.th","hyp.max","hyp.var",
                    "hyp.Ps.th","hyp.Ps.max","hyp.Ps.var",
                    "hyp.th.Ps.th","hyp.max.Ps.max","hyp.var.Ps.var",
                   "hyp.all")
if(!is.null(hyp.in)){ 
#match the model set called for and return it
  hyp.set[[match(hyp.in,names(hyp.set))]]
}else{
  cat("Names of Hypotheses Sets: \n",names(hyp.set),sep="  ")
}  
}
