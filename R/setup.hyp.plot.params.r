#' Setup plotting hypotheses (Internal)
#'
#' Prepares data to plot hypotheses
#' @param diel.setup provided by user or used default as diel.setup=diel.ineq()
#' @param index.models vector of indices indicating which hypotheses to use from diel.setup
#' @param more.points which to use, p.options or p.options2
#' @param ... Other parameters
#' @return Internal list
#' @keywords internal
#' @noRd

setup.hyp.plot.params=function(diel.setup,index.models,more.points=FALSE,...){

#Loop through hypotheses and get sample points  
  plot.points=vector("list",length(index.models))
  if(isFALSE(more.points)){load.points=p.options}
  if(isTRUE(more.points)){load.points=p.options2}

  for(i in 1:length(index.models)){
  if(diel.setup[[index.models[i]]]$func=="bf_multinom"){
    A=round(diel.setup[[index.models[i]]][[2]],digits=5)
    b=round(diel.setup[[index.models[i]]][[3]],digits=5)
    
    #Find all A %*% theta combinations
    p.ineq= round(matrix(apply(load.points[1:2,],2,FUN=function(x){A%*%x}),nrow=nrow(A)),digits=6)
    #find if that is <= b
    p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})
    index=which(p.ineq.logical)
  
   #These are the combinations of p's that match the constraints
    p.plot=t(load.points[,index])
  
  }#end if statement
  
  if(diel.setup[[index.models[i]]]$func=="bf_equality"){  
    A=diel.setup[[index.models[i]]][[2]]
    b=diel.setup[[index.models[i]]][[3]]
    C=diel.setup[[index.models[i]]][[4]]
    d=diel.setup[[index.models[i]]][[5]]
    
    #Find all A %*% theta combinations
    p.ineq= round(matrix(apply(p.options2[1:2,],2,FUN=function(x){A%*%x}),nrow=nrow(A)),digits=6)

    #find if that is <= b
    p.ineq.logical= apply(p.ineq,2,FUN=function(x){all(x<=b)})  
    
    #Find all C %*% theta combinations
    p.ineq2= round(matrix(apply(p.options2[1:2,],2,FUN=function(x){C%*%x}),nrow=nrow(C)),digits=6)

    #find if abs(C*theta -d) < delta
    delta=0.005
    p.ineq.logical2= apply(p.ineq2,2,FUN=function(x){all(abs(x-d)<delta)})  
    
    p.ineq.logical=apply(data.frame(p.ineq.logical,p.ineq.logical2),1,FUN=function(x){all(x)})
    
   index=which(p.ineq.logical)
  
   #These are the combinations of p's that match the constraints
    p.plot=t(p.options2[,index])
  
  }
 
if(diel.setup[[index.models[i]]]$func=="bf_nonlinear"){  
  p.plot=as.matrix(diel.setup[[index.models[i]]]$data,ncol=2)
}  
 
  
  p.plot=cbind(p.plot,rep(diel.setup[[index.models[i]]]$Name,nrow(p.plot)))
  
  colnames(p.plot)=c("p.crep","p.day","p.night","hyp")

  #make sure there are not mistakes
  index.remove=which(p.plot[,3]<0 | p.plot[,3]>1)
  if(length(index.remove)>0){p.plot=p.plot[-index.remove,]}
  
  plot.points[[i]]=p.plot
  
}
  
  plot.points
  
} #end function
