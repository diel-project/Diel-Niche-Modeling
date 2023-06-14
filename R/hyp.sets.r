#' Hypothesis Sets
#'
#' @description Call defined hypotheses sets within \code{Diel.Niche}
#' @param hyp.in Hypothesis set code names, see details for additional information.
#' @details 
#'   To see all available hypothesis sets, set \code{hyp.in = NULL} to observe just the names of the available hypothesis sets or
#'    \code{hyp.in = "list"} to see the hypothesis sets as well as codes for the competing hypotheses within each set. 
#'   avaialable hypothesis sets include. Currently, the available hypothesis sets include Traditional, General, Threshold, Maximizing, Variation, and Selection.
#'   When inputted into \code{hyp.in}, they must be a scalar character object and the first letter must be capitalized. 
#'   
#' @return Names of hypotheses for the set. If NULL, the names of all hypotheses sets are returned. If "list", all hypotheses are printed.
#' @examples 
#' hyp.sets()
#' hyp.sets("Traditional")
#' @export

# A function to simply call certain pre-defined hypotheses sets
# Each name here is defined in the Diel.Inequalities.r function where the inequality constraints
# are defined and named by hypothesis. 


#Need to create a call to provide a description of each hypothesis.

hyp.sets=function(hyp.in=NULL){
  if(!is.null(hyp.in)){
    if(!is.character(hyp.in)){
      stop("If hyp.in is not null, it must be a character object.")
    }
  }

  #NEED TO INCLUDE C.th somewhere
  hyp.set=vector("list",6)
  
 #Hypothesis Sets
  hyp.set[[1]]=c("D","N","CR","C")
  hyp.set[[2]]=c("D","N","CR","C2","D.CR","D.N","CR.N")
  hyp.set[[3]]=c("D.th","N.th","CR.th","C.th")
  hyp.set[[4]]=c("D.max","N.max","CR.max")
  hyp.set[[5]]=c("D.var","N.var","CR.var","C.var")
  hyp.set[[6]]=c("D.avail","TW.avail","N.avail","EQ.avail","D.TW.avail","N.TW.avail","D.N.avail")
  
  names(hyp.set)=c("Traditional","General","Threshold","Maximizing","Variation","Selection")
  class(hyp.set) <- c("list",'diel')
if(is.null(hyp.in)){   
  cat("Names of Hypothesis Sets: \n",names(hyp.set),sep="  ")
}else{
  if(hyp.in=="list"){hyp.set}else{
  #match the model set called for and return it
    x=hyp.set[[match(hyp.in,names(hyp.set))]]
    if(is.null(x)){
      stop(
        "Unknown Input. Input one: NULL, list, Traditional, General, Threshold, Maximizing, Variation, or Selection \n")}else{x}
  }  
}

}#End Function
