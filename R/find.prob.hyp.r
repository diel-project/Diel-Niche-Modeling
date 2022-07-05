#' Inequality Setup
#'
#' Function that inputs a given hypothesis and outputs as many possible
#' probability sets that match the diel hypothesis (i.e., satisfies the inequality constrints).
#' @param hyp Hypothesis name: D.th, D.max, D.var, Dn.th, Dn.max, Dn.var, Dc.th, Dc.max, Dc.var, Dcr.th, Dcr.max,Dcr.var, N.th, N.max, N.var, Nd.th, Nd.max, Nd.var, Nc.th, Nc.max, Nc.var, Ncr.th, Ncr.max, Ncr.var, CR.th, CR.max, CR.var,CRd.th,' CRd.max, CRd.var, CRn.th, CRn.max, CRn.var, CRc.th, CRc.max, CRc.var,'EC.th, EC.var, AC.var.
#' @param diel.setup A list of multinomial inequalities (Matrix A and vector b), representing diel hypotheses setup using the function 'diel.ineq'.
#' @return A matrix of probabilities that match hypothesis in variable hyp
#' @examples 
#' find.prob.hyp(hyp ="D.max")
#' @export


#start function
find.prob.hyp=function(hyp, diel.setup = NULL){
# If there is no diel setup then use defaults
  if(is.null(diel.setup)){diel.setup=diel.ineq()}

  index.models=match(hyp,names(diel.setup))

if(diel.setup[[index.models]]$func=="bf_multinom"){
  A=diel.setup[[index.models]][[2]]
  b=diel.setup[[index.models]][[3]]
  
  #Find all A %*% theta combinations
  p.ineq= round(apply(p.options2[1:2,],2,FUN=function(x){A%*%x}),digits=4)
  #find if that is <= b
  p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})  

  #Find where they are true
  index=which(p.ineq.logical)

 #These are the combinations of p's that match the constraints
  probs.out=t(p.options2[,index])
  

}#End if
  
if(diel.setup[[index.models]]$func=="bf_equality"){  
  A=diel.setup[[index.models]][[2]]
  b=diel.setup[[index.models]][[3]]
  C=diel.setup[[index.models]][[4]]
  d=diel.setup[[index.models]][[5]]
  
  #Find all A %*% theta combinations
  p.ineq= apply(p.options[1:2,],2,FUN=function(x){A%*%x})  
  #find if that is <= b
  p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})  
  
  #Find all C %*% theta combinations
  p.ineq2= apply(p.options[1:2,],2,FUN=function(x){C%*%x})  
  #find if abs(C*theta -d) < delta
  delta=0.001
  p.ineq.logical2= apply(p.ineq2,2,FUN=function(x){all(abs(x-d)<delta)})  
  
  p.ineq.logical=apply(data.frame(p.ineq.logical,p.ineq.logical2),1,FUN=function(x){all(x)})
  
   #Find where they are true
  index=which(p.ineq.logical)

 #These are the combinations of p's that match the constraints
  probs.out=t(p.options[,index])
  
}

if(diel.setup[[index.models]]$func=="bf_nonlinear"){  
probs.out=as.matrix(diel.setup[[index.models]]$data,ncol=2)
probs.out=cbind(probs.out,1-apply(probs.out,1,sum))
probs.out=matrix(probs.out,ncol=3)
  }
  
  

  
  #make sure there are not mistakes
  #index.remove=which(probs.out[,3]<0 | probs.out[,3]>1)
  #if(length(index.remove)>0){probs.out=probs.out[-index.remove,]}
  
  probs.out
}#end function
  