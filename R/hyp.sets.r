#' Hypothesis Sets
#'
#' Call defined hypotheses sets
#' @param hyp.in Hypothesis set code names
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
  hyp.set=vector("list",22)
  
 #General Hypotheses
  hyp.set[[1]]=c("D.th","N.th","CR.th","EC.th")   
  hyp.set[[2]]=c("D.th","N.th","CR.th","C.th")    
  hyp.set[[3]]=c("D.max","N.max","CR.max")
  hyp.set[[4]]=c("D.max","N.max","CR.max","AC")
  hyp.set[[5]]=c("D.var","N.var","CR.var","EC.var") 
  hyp.set[[6]]=c("D.var","N.var","CR.var","AC.var")
  names(hyp.set)[1:6]=c("hyp.th","hyp.th2","hyp.max","hyp.max2",
                        "hyp.var","hyp.var2")
  
  
#Primary-secondary Hypotheses with strong constraints (3rd prob is zero)  
  hyp.set[[7]]=c("Dn.th","Dcr.th","Nd.th","Ncr.th","CRd.th","CRn.th")          
  hyp.set[[8]]=c("Dn.max","Dcr.max","Nd.max","Ncr.max","CRd.max","CRn.max") 
  hyp.set[[9]]=c("Dn.var","Dcr.var","Nd.var","Ncr.var","CRd.var","CRn.var") 
  names(hyp.set)[7:9]=c("hyp.Ps.th","hyp.Ps.max","hyp.Ps.var")
  
#Primary-secondary Hypotheses with weak constraints (3d prob is not zero)  
  hyp.set[[10]]=c("Dn.th.wk","Dcr.th.wk","Nd.th.wk","Ncr.th.wk","CRd.th.wk","CRn.th.wk")          
  hyp.set[[11]]=c("Dn.var.wk","Dcr.var.wk","Nd.var.wk","Ncr.var.wk","CRd.var.wk","CRn.var.wk") 
  names(hyp.set)[10:11]=c("hyp.Ps.th.wk","hyp.Ps.var.wk")

#General & Primary-secondary Hypotheses with strong constraints
  hyp.set[[12]]=c(hyp.set[[1]],hyp.set[[7]]) 
  hyp.set[[13]]=c(hyp.set[[3]],hyp.set[[8]]) 
  hyp.set[[14]]=c(hyp.set[[5]],hyp.set[[9]]) 
  names(hyp.set)[12:14]=c("hyp.th.Ps.th","hyp.max.Ps.max","hyp.var.Ps.var")
#General & Primary-secondary Hypotheses with strong constraints
  hyp.set[[15]]=c(hyp.set[[1]],hyp.set[[10]]) 
  hyp.set[[16]]=c(hyp.set[[3]],hyp.set[[11]]) 
  names(hyp.set)[15:16]=c("hyp.th.Ps.th.wk","hyp.var.Ps.var.wk")

#All hypotheses; used for testing    
  hyp.set[[17]]=c(hyp.set[[2]],hyp.set[[3]],hyp.set[[5]],
                  hyp.set[[7]],hyp.set[[8]],hyp.set[[9]]) 
  hyp.set[[18]]=unique(unlist(hyp.set[1:11]))
  names(hyp.set)[17:18]=c("hyp.all","hyp.all2")

  hyp.set[[19]]=c("D","N","C","CR")
  names(hyp.set)[19]=c("General")

  hyp.set[[20]]=c("D2","N2","C2","CR2")
  names(hyp.set)[20]=c("General2")
    
  hyp.set[[21]]=c("C.full", "CR.full",  "D.CR.full","N.CR.full", "CRc.full","CRd.full" ,
          "CRn.full", "D.full",  "D.N.full","Dc.full", "Dcr.full","Dn.full",  
          "N.full",  "Nc.full", "Ncr.full", "Nd.full" )
  
  names(hyp.set)[21]=c("Full")
  
  hyp.set[[22]]=c("D.Gen","CR.Gen","N.Gen","C.Gen","DN.Gen","CRN.Gen","CRD.Gen")
  names(hyp.set)[22]=c("General3")
  
if(is.null(hyp.in)){   
  cat("Names of Hypotheses Sets: \n",names(hyp.set),sep="  ")
}else{
if(hyp.in=="list"){hyp.set}else{
#match the model set called for and return it
  hyp.set[[match(hyp.in,names(hyp.set))]]
}  
}

}#end of function
